use crate::types::SignedU256;
use primitive_types::U256;
pub mod pnl;
pub mod position;
pub mod rounding;

pub fn apply_signed_add(base: U256, delta: SignedU256) -> Result<U256, String> {
    if delta.mag.is_zero() {
        return Ok(base);
    }

    if delta.is_negative {
        base.checked_sub(delta.mag).ok_or("Underflow".into())
    } else {
        base.checked_add(delta.mag).ok_or("Overflow".into())
    }
}

/// base - delta  ==  base + (-delta)
pub fn apply_signed_sub(base: U256, delta: SignedU256) -> Result<U256, String> {
    apply_signed_add(base, delta.negated())
}

/// a + b
pub fn signed_add(a: SignedU256, b: SignedU256) -> SignedU256 {
    if a.is_zero() {
        return b;
    }
    if b.is_zero() {
        return a;
    }

    match (a.is_negative, b.is_negative) {
        (false, false) => SignedU256::pos(a.mag + b.mag),
        (true, true) => SignedU256::neg(a.mag + b.mag),
        (false, true) => {
            // a - |b|
            if a.mag >= b.mag {
                SignedU256::pos(a.mag - b.mag)
            } else {
                SignedU256::neg(b.mag - a.mag)
            }
        }
        (true, false) => {
            // -|a| + b = b - |a|
            if b.mag >= a.mag {
                SignedU256::pos(b.mag - a.mag)
            } else {
                SignedU256::neg(a.mag - b.mag)
            }
        }
    }
}

/// a - b
pub fn signed_sub(a: SignedU256, b: SignedU256) -> SignedU256 {
    signed_add(a, b.negated())
}

/// abs(a)
pub fn signed_abs(a: SignedU256) -> U256 {
    a.mag
}
