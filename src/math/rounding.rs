use primitive_types::U256;
pub fn div_ceil_u(a: i128, b: i128) -> Result<i128, String> {
    if a < 0 || b <= 0 {
        return Err("div_ceil_invalid".into());
    }
    let q = a / b;
    let r = a % b;
    Ok(if r == 0 { q } else { q + 1 })
}

pub fn div_floor_u(a: i128, b: i128) -> Result<i128, String> {
    if a < 0 || b <= 0 {
        return Err("div_floor_invalid".into());
    }
    Ok(a / b)
}

/// Ceil/floor helpers (нужен только floor тут, но оставляю стиль единым)
#[derive(Clone, Copy)]
pub enum Rounding {
    Down, // floor
    Up,   // ceil
}

pub fn div_round(n: U256, d: U256, rounding: Rounding) -> Result<U256, String> {
    if d.is_zero() {
        return Err("division_by_zero".into());
    }
    let q = n / d;
    let r = n % d;
    Ok(match rounding {
        Rounding::Down => q,
        Rounding::Up => {
            if r.is_zero() {
                q
            } else {
                q + U256::one()
            }
        }
    })
}
