use crate::math::rounding::{Rounding, div_round};
use crate::state::Position;
use crate::types::{Side, SignedU256, TokenAmount, Usd};
/// - full close => all tokens
/// - partial:
///   - long => ceil(pos.size_tokens * size_delta_usd / pos.size_usd)
///   - short => floor(...)
pub fn size_delta_in_tokens(
    pos: &Position,
    size_delta_usd: Usd,
    is_full_close: bool,
) -> Result<TokenAmount, String> {
    if is_full_close || size_delta_usd == pos.size_usd {
        return Ok(pos.size_tokens);
    }
    if pos.size_usd.is_zero() || pos.size_tokens.is_zero() || size_delta_usd.is_zero() {
        return Err("invalid_position_or_size_delta".into());
    }

    if size_delta_usd > pos.size_usd {
        return Err("size_delta_usd_exceeds_position_size".into());
    }

    let n = pos
        .size_tokens
        .checked_mul(size_delta_usd)
        .ok_or("size_delta_mul_overflow")?;
    let t = match pos.key.side {
        Side::Long => div_round(n, pos.size_usd, Rounding::Up)?,
        Side::Short => div_round(n, pos.size_usd, Rounding::Down)?,
    };
    Ok(t.min(pos.size_tokens))
}

/// Proportional pending impact tokens (MVP, toward-zero):
pub fn proportional_pending_impact_tokens(
    pos: &Position,
    size_delta_usd: Usd,
) -> Result<SignedU256, String> {
    if pos.size_usd.is_zero() || size_delta_usd.is_zero() {
        return Ok(SignedU256::zero());
    }

    if size_delta_usd > pos.size_usd {
        return Err("size_delta_usd_exceeds_position_size".into());
    }

    let pending = pos.pending_impact_tokens;
    if pending.mag.is_zero() {
        return Ok(SignedU256::zero());
    }
    // mag = floor(pending.mag * size_delta_usd / pos.size_usd)
    let prod = pending
        .mag
        .checked_mul(size_delta_usd)
        .ok_or("pending_impact_mul_overflow")?;

    let mag = prod / pos.size_usd; // floor

    if mag.is_zero() {
        return Ok(SignedU256::zero());
    }

    Ok(SignedU256 {
        is_negative: pending.is_negative,
        mag,
    })
}
