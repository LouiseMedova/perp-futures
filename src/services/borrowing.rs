use primitive_types::{U256, U512};

use crate::state::{MarketState, PoolBalances, Position};
use crate::types::{AssetId, MarketId, Timestamp, TokenAmount, Usd};

/// Internal scale for borrowing index.
/// Same idea as with funding: factor per 1 USD of position * SCALE.
fn borrow_index_scale() -> U256 {
    U256::exp10(18)
}

const SECONDS_PER_DAY: u64 = 86_400;

/// 0.01% per day in basis points = 1 bps
const BASE_RATE_PER_DAY_BPS: u64 = 1;
/// 0.09% per day in basis points = 9 bps (so util=1 => 10 bps/day = 0.10%/day)
const SLOPE_PER_DAY_BPS: u64 = 9;

fn u512_to_u256_checked(x: U512) -> Result<U256, String> {
    let be = x.to_big_endian();
    if be[..32].iter().any(|&b| b != 0) {
        return Err("mul_div_overflow".into());
    }
    Ok(U256::from_big_endian(&be[32..]))
}

fn mul_div_u256(a: U256, b: U256, den: U256) -> Result<U256, String> {
    if den.is_zero() {
        return Err("mul_div_den_zero".into());
    }
    let q = (U512::from(a) * U512::from(b)) / U512::from(den);
    u512_to_u256_checked(q)
}

/// Convert "bps per day" into "fp per sec" in SCALE=1e18.
/// rate_fp_per_sec = (bps/10_000 per day) / 86400 * SCALE
fn bps_per_day_to_fp_per_sec(bps_per_day: u64) -> U256 {
    let scale = borrow_index_scale();
    let den = U256::from(10_000u64 * SECONDS_PER_DAY);
    // floor is fine for MVP
    mul_div_u256(U256::from(bps_per_day), scale, den).expect("bps_per_day_to_fp_per_sec overflow")
}

/// Result of borrowing settlement for a position.
#[derive(Debug, Clone, Copy)]
pub struct BorrowingDelta {
    /// Always >= 0 in a normal setup: borrowing is a pure cost.
    pub borrowing_fee_usd: Usd,
}

/// Service for borrowing logic:
/// - evolves market borrowing index over time;
/// - computes how much each position should pay.
pub trait BorrowingService {
    /// Update borrowing index for the market up to `now`,
    /// based on current utilization.
    fn update_index(&self, market: &mut MarketState, now: Timestamp);

    /// Compute borrowing fee for a position and update its snapshot.
    fn settle_position_borrowing(&self, market: &MarketState, pos: &mut Position)
    -> BorrowingDelta;
}

/// Basic implementation:
///
/// - utilization ≈ (oi_long + oi_short) / liquidity
/// - rate is a simple linear function of utilization:
///     rate_per_sec = base_rate + slope * utilization
#[derive(Default, Clone)]
pub struct BasicBorrowingService;

impl BasicBorrowingService {
    /// Compute utilization as a fixed-point in [0, 1] * BORROW_INDEX_SCALE.
    fn compute_utilization_fp(market: &MarketState) -> U256 {
        let borrowed = market.oi_long_usd + market.oi_short_usd;
        let liquidity = market.liquidity_usd;

        if liquidity == U256::zero() {
            return U256::zero();
        }

        let ratio_fp = borrowed.saturating_mul(borrow_index_scale()) / liquidity;

        // Cap at 1.0 in FP
        ratio_fp.min(borrow_index_scale())
    }
}

impl BorrowingService for BasicBorrowingService {
    fn update_index(&self, market: &mut MarketState, now: Timestamp) {
        if market.borrowing.last_updated_at == 0 {
            market.borrowing.last_updated_at = now;
            return;
        }
        if now <= market.borrowing.last_updated_at {
            return;
        }

        let dt: u64 = now - market.borrowing.last_updated_at;
        if dt == 0 {
            return;
        }

        // 1) Utilization in [0, 1] * SCALE
        let util_fp = Self::compute_utilization_fp(market);

        let borrowing = &mut market.borrowing;

        // 2) Simple linear rate:
        //
        //    rate_per_sec_fp = base_rate_fp + slope_fp * util
        //
        // Where:
        //   - base_rate_fp: minimal rate when utilization ~0.
        //   - slope_fp: how fast rate grows with utilization.
        //
        // Units: index units per second (same scale: BORROW_INDEX_SCALE).
        let base_rate_fp_per_sec = bps_per_day_to_fp_per_sec(BASE_RATE_PER_DAY_BPS); // ~1_157_407_407
        let slope_fp_per_sec = bps_per_day_to_fp_per_sec(SLOPE_PER_DAY_BPS); // ~10_416_666_667

        // rate_per_sec_fp = base + slope * util / SCALE
        let slope_term =
            mul_div_u256(slope_fp_per_sec, util_fp, borrow_index_scale()).unwrap_or(U256::zero());
        let rate_per_sec_fp = base_rate_fp_per_sec.saturating_add(slope_term);

        let delta_index_fp = rate_per_sec_fp.saturating_mul(U256::from(dt));

        borrowing.cumulative_factor = borrowing.cumulative_factor.saturating_add(delta_index_fp);
        borrowing.last_updated_at = now;
    }

    fn settle_position_borrowing(
        &self,
        market: &MarketState,
        pos: &mut Position,
    ) -> BorrowingDelta {
        let current_idx = market.borrowing.cumulative_factor;
        let prev_idx = pos.borrowing_index;

        let delta_idx = current_idx - prev_idx;
        if delta_idx <= U256::zero() || pos.size_usd == U256::zero() {
            pos.borrowing_index = current_idx;
            return BorrowingDelta {
                borrowing_fee_usd: U256::zero(),
            };
        }

        // borrowing_fee = sizeUsd * deltaIndex / SCALE
        let fee = pos.size_usd.saturating_mul(delta_idx) / borrow_index_scale();

        pos.borrowing_index = current_idx;

        BorrowingDelta {
            borrowing_fee_usd: fee,
        }
    }
}

/// Route borrowing fees (already converted to collateral tokens)
/// to the pool of (market, collateral_token).
pub fn apply_borrowing_fees_to_pool(
    pools: &mut PoolBalances,
    market_id: MarketId,
    collateral_token: AssetId,
    borrowing_tokens: TokenAmount,
) {
    if borrowing_tokens.is_zero() {
        return;
    }

    pools.add_fee_to_pool(market_id, collateral_token, borrowing_tokens);
}

fn utilization_fp(market: &MarketState) -> U256 {
    let borrowed = market.oi_long_usd.saturating_add(market.oi_short_usd);
    let liquidity = market.liquidity_usd;
    if liquidity.is_zero() {
        return U256::zero();
    }
    let fp = borrowed.saturating_mul(borrow_index_scale()) / liquidity;
    fp.min(borrow_index_scale())
}

/// Preview borrowing fee for the position if we advanced indices to `now`.
pub fn preview_borrowing_fee_usd(
    market: &MarketState,
    pos: &Position,
    now: Timestamp,
) -> Result<U256, String> {
    let last = market.borrowing.last_updated_at;
    if last == 0 || now <= last {
        return Ok(U256::zero());
    }
    let dt: u64 = now - last;
    if dt == 0 {
        return Ok(U256::zero());
    }
    let util = utilization_fp(market);
    let base = bps_per_day_to_fp_per_sec(BASE_RATE_PER_DAY_BPS);
    let slope = bps_per_day_to_fp_per_sec(SLOPE_PER_DAY_BPS);

    // rate_per_sec_fp = base + slope * util / SCALE
    let slope_term = mul_div_u256(slope, util, borrow_index_scale())?;
    let rate_per_sec_fp = base.saturating_add(slope_term);

    let delta_index_fp = rate_per_sec_fp.saturating_mul(U256::from(dt));
    let current_idx = market
        .borrowing
        .cumulative_factor
        .saturating_add(delta_index_fp);

    if current_idx <= pos.borrowing_index || pos.size_usd.is_zero() {
        return Ok(U256::zero());
    }

    let delta_idx = current_idx - pos.borrowing_index;
    // fee_usd = size_usd * delta_idx / SCALE
    Ok(pos.size_usd.saturating_mul(delta_idx) / borrow_index_scale())
}
