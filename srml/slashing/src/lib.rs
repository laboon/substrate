#![deny(missing_docs, rust_2018_idioms)]

//! Abstract slashing interface
//!
//! That gives functionality to specialize slashing and misconduct for a given type
//! In order to customize severity level and misconduct fees.
//!
//! TODO(niklasad1): provide default impl?
//! TODO(niklasad1): move the `srml/support/src/traits.rs`?
//!
//! For example an use case could be to increase severity level exponentially on concurrent culprits

/// # Examples
///	```
///	use srml_slashing::{Slashing, Misconduct};
/// use parity_codec::Codec;
///	use sr_primitives::traits::{SimpleArithmetic, MaybeSerializeDebug};
///
///	// Dummy example that represent severity level as a simple counter that gets reset `on_signal`
///	// Slashes `balance / severity`
///	pub struct MyType;
///
/// impl Slashing for MyType {
///		type AccountId = u64;
///		type Balance = u32;
///		type Severity = u32;
///
///		fn on_slash(
///			severity: Self::Severity,
///			who: Self::AccountId,
///			balance: Self::Balance,
///			misconduct: impl Misconduct<Balance = Self::Balance>
///		) -> (Self::Balance, Self::Severity) {
///			let new_balance = misconduct.on_misconduct(balance, severity);
///			(new_balance, severity + 1)
///		}
///
///		fn on_signal(severity: Self::Severity) -> Self::Severity {
///			0
///		}
///	}
///
///	enum MyTypeMisconduct {
///		A,
///		B,
///		C,
///	}
///
///	impl Misconduct for MyTypeMisconduct {
///		type Balance = u32;
///
///		fn on_misconduct<B, S>(&self, balance: B, severity: S) -> Self::Balance
///		where
///			B: SimpleArithmetic + Codec + Copy + MaybeSerializeDebug + Default + Into<Self::Balance>,
///			S: SimpleArithmetic + Codec + Copy + MaybeSerializeDebug + Default + Into<Self::Balance>
///		{
///			let x: Self::Balance = balance.into() / severity.into();
///
///			match &self {
///				MyTypeMisconduct::A => x + 0,
///				MyTypeMisconduct::B => x + 1,
///				MyTypeMisconduct::C => x + 2,
///			}
///		}
///	}
/// ```

use parity_codec::Codec;
use sr_primitives::traits::{SimpleArithmetic, MaybeSerializeDebug};

/// Represents `generic` misconduct to be slashed
pub trait Misconduct {
	/// Account balance
	type Balance: SimpleArithmetic + Codec + Copy + MaybeSerializeDebug + Default;

	/// Calculate new balance based on the `misconduct` and `severity`
	fn on_misconduct<B, S>(&self, balance: B, severity: S) -> Self::Balance
	where
		B: SimpleArithmetic + Codec + Copy + MaybeSerializeDebug + Default + Into<Self::Balance>,
		S: SimpleArithmetic + Codec + Copy + MaybeSerializeDebug + Default + Into<Self::Balance>;
}

/// Slashing interface
// TODO(niklasad1): should imbalance be used here such as `trait::Currency`?!
pub trait Slashing {
	/// Account id
	type AccountId: Codec + Copy + MaybeSerializeDebug + Default;
	/// Account balance
	type Balance: SimpleArithmetic + Codec + Copy + MaybeSerializeDebug + Default;
	/// Severity, based on concurrent culprits
	type Severity: SimpleArithmetic + Codec + MaybeSerializeDebug + Default;

	/// Calculate `new balance` and `severity`
	// TODO(niklasad1): provide default impl?
	// TODO(niklasad1): make this `&mut self` and keep `severity` in the struct T instead of passing it explicitly?
	fn on_slash(
		severity: Self::Severity,
		who: Self::AccountId,
		balance: Self::Balance,
		misconduct: impl Misconduct<Balance = Self::Balance>
	) -> (Self::Balance, Self::Severity);

	/// Signal that a generic transition has occurred (e.g., time slot expired) and estimate new severity
	// TODO(niklasad1): should we have a default impl such as `exponential growth/decay` or just require each type
	// to impl this seperately?!
	fn on_signal(severity: Self::Severity) -> Self::Severity;
}
