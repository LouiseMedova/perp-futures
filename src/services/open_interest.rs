use crate::types::{Side, Usd};

/// Snapshot of open interest (OI) in USD at a single point in time.
#[derive(Clone, Debug)]
pub struct OpenInterestSnapshot {
    pub long_usd: Usd,
    pub short_usd: Usd,
}

/// OI before and after an action (used for price impact calculations).
#[derive(Clone, Debug)]
pub struct OpenInterestParams {
    pub current: OpenInterestSnapshot,
    pub next: OpenInterestSnapshot,
}

/// High-level trait for building OpenInterestParams
/// for different actions (increase / decrease).
pub trait OpenInterestService {
    /// Build OI params for an **increase** of position size.
    ///
    /// Convention (without virtual liquidity):
    /// - if side == Long:
    ///       next.long = current.long + size_delta_usd
    ///       next.short = current.short
    /// - if side == Short:
    ///       next.short = current.short + size_delta_usd
    ///       next.long = current.long
    ///
    /// assume size_delta_usd >= 0.
    fn for_increase(
        &self,
        current_long_usd: Usd,
        current_short_usd: Usd,
        size_delta_usd: Usd,
        side: Side,
    ) -> OpenInterestParams;

    fn for_decrease(
        &self,
        current_long_usd: Usd,
        current_short_usd: Usd,
        size_delta_usd: Usd,
        side: Side,
    ) -> OpenInterestParams;
}

/// Minimal implementation that just follows the convention above.
#[derive(Default, Clone)]
pub struct BasicOpenInterestService;

impl OpenInterestService for BasicOpenInterestService {
    fn for_increase(
        &self,
        current_long_usd: Usd,
        current_short_usd: Usd,
        size_delta_usd: Usd,
        side: Side,
    ) -> OpenInterestParams {
        let current = OpenInterestSnapshot {
            long_usd: current_long_usd,
            short_usd: current_short_usd,
        };

        let (next_long, next_short) = match side {
            Side::Long => (current_long_usd + size_delta_usd, current_short_usd),
            Side::Short => (current_long_usd, current_short_usd + size_delta_usd),
        };

        let next = OpenInterestSnapshot {
            long_usd: next_long,
            short_usd: next_short,
        };

        OpenInterestParams { current, next }
    }

    fn for_decrease(
        &self,
        current_long_usd: Usd,
        current_short_usd: Usd,
        size_delta_usd: Usd,
        side: Side,
    ) -> OpenInterestParams {
        let current = OpenInterestSnapshot {
            long_usd: current_long_usd,
            short_usd: current_short_usd,
        };
        let (next_long, next_short) = match side {
            Side::Long => (
                current_long_usd
                    .checked_sub(size_delta_usd)
                    .expect("oi_long_underflow"),
                current_short_usd,
            ),
            Side::Short => (
                current_long_usd,
                current_short_usd
                    .checked_sub(size_delta_usd)
                    .expect("oi_short_underflow"),
            ),
        };

        let next = OpenInterestSnapshot {
            long_usd: next_long,
            short_usd: next_short,
        };

        OpenInterestParams { current, next }
    }
}
