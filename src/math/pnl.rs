use crate::math::rounding::{Rounding, div_round};
use crate::state::Position;
use crate::types::{OraclePrices, Side, SignedU256, TokenAmount, Usd};
fn pick_price_for_pnl(side: Side, prices: &OraclePrices) -> Usd {
    let p = match side {
        Side::Long => prices.index_price_min,
        Side::Short => prices.index_price_max,
    };

    p
}

/// Total position PnL in USD (signed).
///
/// Assumptions:
/// - pos.size_tokens is in atoms
/// - prices.index_price_* is USD(1e30) per 1 atom (per-unit)
/// - pos.size_usd is USD(1e30)
pub fn total_position_pnl_usd(pos: &Position, prices: &OraclePrices) -> Result<SignedU256, String> {
    let px = pick_price_for_pnl(pos.key.side, prices);

    if px.is_zero() {
        return Err("invalid_index_price_for_pnl".into());
    }

    // value_usd = size_tokens * price_per_unit
    let value = pos
        .size_tokens
        .checked_mul(px)
        .ok_or("pnl_value_overflow")?;

    let entry = pos.size_usd;
    let pnl = match pos.key.side {
        Side::Long => {
            // pnl = value - entry
            if value >= entry {
                SignedU256::pos(value - entry)
            } else {
                SignedU256::neg(entry - value)
            }
        }
        Side::Short => {
            // pnl = entry - value
            if entry >= value {
                SignedU256::pos(entry - value)
            } else {
                SignedU256::neg(value - entry)
            }
        }
    };
    Ok(pnl)
}

/// Realized PnL for partial close
pub fn realized_pnl_usd(
    total_pnl_usd: SignedU256,
    size_delta_tokens: TokenAmount,
    pos_size_tokens: TokenAmount,
) -> Result<SignedU256, String> {
    if pos_size_tokens.is_zero() {
        return Err("invalid_pos_size_tokens".into());
    }
    if size_delta_tokens.is_zero() || total_pnl_usd.mag.is_zero() {
        return Ok(SignedU256::zero());
    }
    if size_delta_tokens > pos_size_tokens {
        return Err("size_delta_tokens_exceeds_position_size".into());
    }
    let prod = total_pnl_usd
        .mag
        .checked_mul(size_delta_tokens)
        .ok_or("realized_pnl_mul_overflow")?;

    let mag = prod / pos_size_tokens; // floor on magnitude

    if mag.is_zero() {
        return Ok(SignedU256::zero());
    }

    Ok(if total_pnl_usd.is_negative {
        SignedU256::neg(mag)
    } else {
        SignedU256::pos(mag)
    })
}

/// Convert +/- pnlUsd to collateral tokens:
/// +PnL: floor(pnlUsd / collateral_price_max) (min payout tokens)
/// -PnL: ceil(abs(pnlUsd) / collateral_price_min) (max cost tokens)
///
/// Assumptions:
/// - collateral_price_* is USD(1e30) per 1 collateral atom (per-unit)
pub fn pnl_usd_to_collateral_tokens(
    pnl_usd: SignedU256,
    prices: &OraclePrices,
) -> Result<SignedU256, String> {
    if pnl_usd.is_zero() {
        return Ok(SignedU256::zero());
    }

    if !pnl_usd.is_negative {
        let p = prices.collateral_price_max;

        let mag = div_round(pnl_usd.mag, p, Rounding::Down)?;
        Ok(SignedU256::pos(mag))
    } else {
        let p = prices.collateral_price_min;

        let mag = div_round(pnl_usd.mag, p, Rounding::Up)?;
        Ok(SignedU256::neg(mag))
    }
}
