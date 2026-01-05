// src/state/market_state.rs
use crate::types::*;

#[derive(Clone, Debug, Default)]
pub struct MarketFunding {
    pub cumulative_index_long: i128,
    pub cumulative_index_short: i128,
}

#[derive(Clone, Debug, Default)]
pub struct MarketBorrowing {
    pub cumulative_factor: i128,
}

#[derive(Clone, Debug, Default)]
pub struct ImpactPoolState {
    pub impact_tokens: TokenAmount,
    pub total_pending_impact_tokens: TokenAmount,
    pub last_bleed_at: Timestamp,
}

#[derive(Clone, Debug, Default)]
pub struct OpenInterest {
    pub long_usd: Usd,
    pub short_usd: Usd,
}

#[derive(Clone, Debug, Default)]
pub struct MarketConfig {
    pub min_collateral_factor_bps: i64,
    pub max_leverage_bps: i64,
    pub min_position_size_usd: Usd,
}

#[derive(Clone, Debug, Default)]
pub struct MarketState {
    /// Market identifier.
    pub id: MarketId,

    /// Index token for this market (e.g. ETH, BTC).
    pub index_token: AssetId,
    pub long_asset: AssetId,
    pub short_asset: AssetId,

    /// Open interest in USD for longs / shorts.
    pub oi_long_usd: Usd,
    pub oi_short_usd: Usd,

    /// Funding-related cumulative indices.
    pub funding: FundingState,

    /// Borrowing-related cumulative factor.
    pub borrowing: BorrowingState,

    /// State of the position impact pool.
    pub impact_pool: ImpactPoolState,
    pub liquidity_usd: Usd,
    // TODO:
    // pub impact_config: MarketImpactConfig,
    // pub limits: MarketLimits,
    // pub margin_config: MarginConfig,
}

#[derive(Clone, Debug, Default)]
pub struct FundingState {
    /// Cumulative funding index for longs.
    pub cumulative_index_long: SignedU256,
    /// Cumulative funding index for shorts.
    pub cumulative_index_short: SignedU256,
    /// Last time funding indices were updated.
    pub last_updated_at: Timestamp,
}

#[derive(Clone, Debug, Default)]
pub struct BorrowingState {
    /// Cumulative borrowing factor (в условных единицах Usd).
    pub cumulative_factor: Usd,
    /// Last time borrowing factor was updated.
    pub last_updated_at: Timestamp,
}
