use primitive_types::U256;

use crate::math;
use crate::state::{MarketState, Position};
use crate::types::{Side, SignedU256, Timestamp};
/// Funding index scale.
/// Index is stored as: (funding USD per 1 USD of position) * SCALE.
///
/// With SCALE = 1e18 we can represent very small per-second rates without quantization.
/// Example:
///   DAILY_RATE_BPS = 1  -> 0.01% per day at persistent imbalance
///   per_sec_rate ≈ 1e-4 / 86400 ≈ 1.157e-9
///   fp_per_sec   ≈ 1.157e-9 * 1e18 ≈ 1.157e9 (fits i128 easily)
fn funding_index_scale() -> U256 {
    U256::exp10(18) // 1e18
}

/// Desired funding rate (MVP): 1 bp/day = 0.01% per day when imbalance persists.
/// Increase to 5..20 bps/day if you want stronger incentives.
const DAILY_RATE_BPS: i128 = 1; // 0.01% / day

const SECONDS_PER_DAY: i128 = 86_400;
const BPS_DENOM: i128 = 10_000;

/// rate_fp_per_sec = SCALE * (DAILY_RATE_BPS / 10_000) / 86_400
fn rate_fp_per_sec() -> U256 {
    (funding_index_scale() / U256::from(SECONDS_PER_DAY)) * U256::from(DAILY_RATE_BPS)
        / U256::from(BPS_DENOM)
}
/// Result of funding settlement for a single position.
#[derive(Debug, Clone, Copy)]
pub struct FundingDelta {
    /// Positive value means "user pays", negative — "user receives".
    pub funding_fee_usd: SignedU256,
}

/// Funding service: responsible for
/// - evolving market funding indices over time;
/// - computing per-position funding deltas based on snapshots.
pub trait FundingService {
    /// Update market funding indices up to `now`, based on current OI imbalance.
    fn update_indices(&self, market: &mut MarketState, now: Timestamp);

    /// Compute funding delta for a given position (using market indices)
    /// and update the position snapshot to the latest index.
    ///
    /// Returns how much funding this position should pay (positive)
    /// or receive (negative) in USD.
    fn settle_position_funding(&self, market: &MarketState, pos: &mut Position) -> FundingDelta;
}

/// Basic implementation:
///
/// - Uses a very simple rule:
///     * If longs > shorts → longs pay a fixed rate to shorts.
///     * If shorts > longs → shorts pay a fixed rate to longs.
/// - Rate depends on imbalance **sign**, not magnitude (MVP).
#[derive(Default)]
pub struct BasicFundingService;

fn current_index_for_side(market: &MarketState, side: Side) -> SignedU256 {
    match side {
        Side::Long => market.funding.cumulative_index_long,
        Side::Short => market.funding.cumulative_index_short,
    }
}

impl FundingService for BasicFundingService {
    fn update_indices(&self, market: &mut MarketState, now: Timestamp) {
        let funding = &mut market.funding;

        // 1) First-time init or no time passed.
        if funding.last_updated_at == 0 {
            funding.last_updated_at = now;
            return;
        }
        if now <= funding.last_updated_at {
            return;
        }

        let dt: u64 = now - funding.last_updated_at;
        if dt == 0 {
            return;
        }

        // 2) Read current OI.
        let long_oi = market.oi_long_usd;
        let short_oi = market.oi_short_usd;
        let total_oi = long_oi + short_oi;

        // If there is no open interest at all, funding does not move.
        if total_oi.is_zero() {
            funding.last_updated_at = now;
            return;
        }

        // 3) Very simple rule for MVP:
        //
        //    - If market is long-heavy → longs pay shorts at a fixed rate.
        //    - If short-heavy → shorts pay longs.
        //
        // rate_abs_fp is "index units per second", in FUNDING_INDEX_SCALE.

        let delta_index_fp = rate_fp_per_sec().saturating_mul(U256::from(dt));
        if long_oi > short_oi {
            // Long-heavy → longs pay (their index increases), shorts receive (their index decreases)
            funding.cumulative_index_long = math::signed_add(
                funding.cumulative_index_long,
                SignedU256::pos(delta_index_fp),
            );
            funding.cumulative_index_short = math::signed_sub(
                funding.cumulative_index_short,
                SignedU256::pos(delta_index_fp),
            );
        } else {
            // Short-heavy → shorts pay, longs receive
            // Short-heavy: shorts pay
            funding.cumulative_index_long = math::signed_sub(
                funding.cumulative_index_long,
                SignedU256::pos(delta_index_fp),
            );
            funding.cumulative_index_short = math::signed_add(
                funding.cumulative_index_short,
                SignedU256::pos(delta_index_fp),
            );
        }

        funding.last_updated_at = now;
    }

    fn settle_position_funding(&self, market: &MarketState, pos: &mut Position) -> FundingDelta {
        // 1) Choose market index for position side (long/short).
        let current_idx = current_index_for_side(market, pos.key.side);
        let prev_idx = pos.funding_index;

        // delta_idx = current - prev (signed)
        let delta_idx = math::signed_sub(current_idx, prev_idx);
        pos.funding_index = current_idx;

        if delta_idx.is_zero() || pos.size_usd.is_zero() {
            return FundingDelta {
                funding_fee_usd: SignedU256::zero(),
            };
        }
        // 2) funding_fee_usd = sizeUsd * deltaIndex / SCALE
        //
        // Convention:
        //   - Positive funding_fee_usd → user pays.
        //   - Negative funding_fee_usd → user receives.
        //
        // Since we made payers' index go UP, receivers' index go DOWN,
        // the formula below automatically gives the right sign:
        // fee_mag = size_usd * abs(delta_idx) / SCALE
        let abs_idx = math::signed_abs(delta_idx);
        let scale = funding_index_scale();

        let fee_mag = match pos.size_usd.checked_mul(abs_idx) {
            Some(prod) => prod / scale, // floor
            None => U256::MAX,          // MVP fallback
        };

        // sign of fee == sign of delta_idx
        let fee = if delta_idx.is_negative {
            SignedU256::neg(fee_mag) // user receives
        } else {
            SignedU256::pos(fee_mag) // user pays
        };

        FundingDelta {
            funding_fee_usd: fee,
        }
    }
}

/// Preview funding fee for the position if we advanced indices to `now`.
/// Returns SignedU256:
///   + => user pays,
///   - => user receives.
pub fn preview_funding_fee_usd(
    market: &MarketState,
    pos: &Position,
    now: Timestamp,
) -> Result<SignedU256, String> {
    let last = market.funding.last_updated_at;
    if last == 0 || now <= last {
        return Ok(SignedU256::zero());
    }
    let dt: u64 = now - last;
    if dt == 0 {
        return Ok(SignedU256::zero());
    }

    let long_oi = market.oi_long_usd;
    let short_oi = market.oi_short_usd;
    if long_oi.is_zero() && short_oi.is_zero() {
        return Ok(SignedU256::zero());
    }

    let delta_index_fp = rate_fp_per_sec().saturating_mul(U256::from(dt));

    // Compute hypothetical indices after update (same rule as FundingService)
    let mut idx_long = market.funding.cumulative_index_long;
    let mut idx_short = market.funding.cumulative_index_short;

    if long_oi > short_oi {
        // long-heavy: longs pay (index up), shorts receive (index down)
        idx_long = math::signed_add(idx_long, SignedU256::pos(delta_index_fp));
        idx_short = math::signed_sub(idx_short, SignedU256::pos(delta_index_fp));
    } else if short_oi > long_oi {
        // short-heavy: shorts pay, longs receive
        idx_long = math::signed_sub(idx_long, SignedU256::pos(delta_index_fp));
        idx_short = math::signed_add(idx_short, SignedU256::pos(delta_index_fp));
    } else {
        // balanced: no move
        return Ok(SignedU256::zero());
    }

    let current_idx = match pos.key.side {
        Side::Long => idx_long,
        Side::Short => idx_short,
    };
    let prev_idx = pos.funding_index;
    let delta_idx = math::signed_sub(current_idx, prev_idx);
    if delta_idx.is_zero() || pos.size_usd.is_zero() {
        return Ok(SignedU256::zero());
    }

    let abs_idx = math::signed_abs(delta_idx);
    let fee_mag = pos.size_usd
        .checked_mul(abs_idx)
        .ok_or("funding_fee_mul_overflow")?
        / funding_index_scale();

    Ok(if fee_mag.is_zero() {
        SignedU256::zero()
    } else if delta_idx.is_negative {
        SignedU256::neg(fee_mag) // user receives
    } else {
        SignedU256::pos(fee_mag) // user pays
    })
}