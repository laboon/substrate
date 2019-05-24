// Copyright 2019 Parity Technologies (UK) Ltd.
// This file is part of Substrate.

// Substrate is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Substrate is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Substrate.  If not, see <http://www.gnu.org/licenses/>.

#![warn(missing_docs, rust_2018_idioms)]

//! Slashing interface
//!
//! That gives functionality to specialize slashing and misconduct for a given type
//! In order to customize severity level and misconduct fees.
//!
//! TODO(niklasad1): provide default impl?
//!
//! For example an use case could be to increase severity level exponentially on concurrent culprits

use parity_codec::Codec;
use primitives::traits::{SimpleArithmetic, MaybeSerializeDebug};
use srml_support::traits::Currency;

type BalanceOf<T> = <<T as OnSlashing>::Currency as Currency<<T as system::Trait>::AccountId>>::Balance;
type NegativeImbalanceOf<T> =
	<<T as OnSlashing>::Currency as Currency<<T as system::Trait>::AccountId>>::NegativeImbalance;

/// Estimates severity level based on misconduct
pub trait Misconduct {
	/// Severity
	type Severity: SimpleArithmetic + Codec + Copy + MaybeSerializeDebug + Default;
	/// Increase severity level on misconduct.
	fn on_misconduct(&self, severity: Self::Severity) -> Self::Severity;
	/// Decrease severity level after a certain point up to the implementor to determine when.
	fn on_signal(&self, severity: Self::Severity) -> Self::Severity;
}

// pub trait Slashing: OnSlashing {
//     /// ..
//     fn amount(free_balance: BalanceOf<Self>, severity: Self::Severity) -> BalanceOf<Self>;
// }

/// Wrappers sits between balances and `Slashing`
pub trait OnSlashing: system::Trait {

	/// Balance
	type Currency: Currency<Self::AccountId>;

	// /// Slashing
	// type Slashing: Slashing;

	/// Severity
	type Severity: SimpleArithmetic + Codec + Copy + MaybeSerializeDebug + Default;

	/// Misconduct
	type Misconduct: Misconduct;

	/// Slash `who`
	//
	// `free_balance` should be fetched by `Currency::free_balance` before calling this function
	// The reason why it is needed -> testability!!!
	fn slash(
		who: &Self::AccountId,
		free_balance: BalanceOf<Self>,
		severity: Self::Severity,
		misconduct: Self::Misconduct
	) -> (NegativeImbalanceOf<Self>, BalanceOf<Self>, Self::Severity);
}

#[cfg(test)]
mod test {

	use super::*;
	use runtime_io::with_externalities;
	use srml_support::{decl_module, impl_outer_origin, dispatch::Result};
	use system::{self, ensure_signed, GenesisConfig};
	use substrate_primitives::{Blake2Hasher, H256};
	use primitives::{
		BuildStorage,
		traits::{BlakeTwo256, IdentityLookup},
		testing::{Digest, DigestItem, Header}
	};

	impl_outer_origin! {
		pub enum Origin for Test {}
	}

	#[derive(Clone, Eq, PartialEq)]
	pub struct Test;

	impl system::Trait for Test {
		type Origin = Origin;
		type Index = u64;
		type BlockNumber = u64;
		type Hash = H256;
		type Hashing = BlakeTwo256;
		type Digest = Digest;
		type AccountId = u64;
		type Lookup = IdentityLookup<Self::AccountId>;
		type Header = Header;
		type Event = ();
		type Log = DigestItem;
	}

	impl balances::Trait for Test {
		type Balance = u64;
		type OnFreeBalanceZero = ();
		type OnNewAccount = ();
		type Event = ();
		type TransactionPayment = ();
		type TransferPayment = ();
		type DustRemoval = ();
	}

	impl OnSlashing for Test {
		type Currency = balances::Module<Self>;
		type Severity = u64;
		type Misconduct = Unresponsive;

		fn slash(
			who: &Self::AccountId,
			balance: BalanceOf<Self>,
			severity: Self::Severity,
			misconduct: Self::Misconduct
		) -> (NegativeImbalanceOf<Self>, BalanceOf<Self>, Self::Severity) {
			// TODO(niklasad1): move this to its own trait
			let slashed_amount = balance / severity;

			let (imbalance, remaining) = Self::Currency::slash(&who, slashed_amount);
			(imbalance, remaining, misconduct.on_misconduct(severity))
		}
	}

	pub struct Unresponsive;

	impl Misconduct for Unresponsive {
		type Severity = u64;

		fn on_misconduct(&self, severity: Self::Severity) -> Self::Severity {
			std::cmp::max(1, severity / 4)
		}
		fn on_signal(&self, severity: Self::Severity) -> Self::Severity {
			severity * 2
		}
	}

	decl_module! {
		pub struct Module<T: OnSlashing> for enum Call where origin: T::Origin {
			pub fn system_module_example(origin) -> Result {
				let _sender = ensure_signed(origin)?;
				let _random_seed = <system::Module<T>>::random_seed();
				let _extrinsic_count = <system::Module<T>>::extrinsic_count();
				Ok(())
			}
		}
	}

	#[derive(Default)]
	pub struct ExtBuilder {
		transaction_base_fee: u64,
		transaction_byte_fee: u64,
		existential_deposit: u64,
		transfer_fee: u64,
		creation_fee: u64,
		monied: bool,
		vesting: bool,
	}

	impl ExtBuilder {
		pub fn existential_deposit(mut self, existential_deposit: u64) -> Self {
			self.existential_deposit = existential_deposit;
			self
		}
		#[allow(dead_code)]
		pub fn transfer_fee(mut self, transfer_fee: u64) -> Self {
			self.transfer_fee = transfer_fee;
			self
		}
		pub fn creation_fee(mut self, creation_fee: u64) -> Self {
			self.creation_fee = creation_fee;
			self
		}
		pub fn transaction_fees(mut self, base_fee: u64, byte_fee: u64) -> Self {
			self.transaction_base_fee = base_fee;
			self.transaction_byte_fee = byte_fee;
			self
		}
		pub fn monied(mut self, monied: bool) -> Self {
			self.monied = monied;
			if self.existential_deposit == 0 {
				self.existential_deposit = 1;
			}
			self
		}
		pub fn vesting(mut self, vesting: bool) -> Self {
			self.vesting = vesting;
			self
		}
		pub fn build(self) -> runtime_io::TestExternalities<Blake2Hasher> {
			system::GenesisConfig::<Test>::default().build_storage().unwrap().0.into()
		}
	}

	#[test]
	fn it_works() {
		// Verifies initial conditions of mock
		with_externalities(&mut ExtBuilder::default()
			.build(),
		|| {
			let misconduct = Unresponsive;
			let who = 0;
			let balance = 1000;
			let severity = 1000;

			let (imba, rem, seve) = Test::slash(&who, balance, severity, misconduct);
			assert_eq!(seve, 250);

		});
	}
}
