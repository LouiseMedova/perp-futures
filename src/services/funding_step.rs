// src/services/funding_step.rs

use primitive_types::U256;

use crate::math;
use crate::services::FundingService;
use crate::state::{Claimables, MarketState, Position};
use crate::types::{OraclePrices, TokenAmount};
/// Result of applying funding for a single position on a single step.
#[derive(Debug, Clone, Copy)]
pub struct FundingStep {
    /// How much funding this position must pay in USD (payer side).
    /// Always >= 0.
    pub cost_usd: U256, // signed USD(1e30)
}

/// Apply funding for a single position:
///  - calls FundingService::settle_position_funding (updates pos.funding_index),
///  - if the position is on the payer side => returns positive cost_usd,
///  - if on receiver side => mints Claimables in collateral token and returns cost_usd = 0.
pub fn apply_funding_step<F: FundingService>(
    funding_svc: &F,
    market: &MarketState,
    pos: &mut Position,
    claimables: &mut Claimables,
    prices: &OraclePrices,
) -> Result<FundingStep, String> {
    let delta = funding_svc.settle_position_funding(market, pos);
    let fee_usd = delta.funding_fee_usd;

    if fee_usd.mag.is_zero() {
        return Ok(FundingStep {
            cost_usd: U256::zero(),
        });
    }

    if !fee_usd.is_negative {
        // Payer side: position pays funding in USD.
        return Ok(FundingStep {
            cost_usd: fee_usd.mag,
        });
    }

    // Receiver side: position earns funding.
    //
    // Convert reward USD -> collateral tokens (atoms).
    // Using collateral_price_max to minimize token payout (conservative).
    let price = prices.collateral_price_max;
    if price.is_zero() {
        return Err("invalid_collateral_price_max_for_funding".into());
    }

    let reward_usd: U256 = fee_usd.mag;

    // reward_tokens_atoms = reward_usd / price_per_unit
    // Round Down to avoid overpaying.
    let reward_tokens: TokenAmount =
        math::rounding::div_round(reward_usd, price, math::rounding::Rounding::Down)?;

    if !reward_tokens.is_zero() {
        claimables.add_funding(pos.key.account, pos.key.collateral_token, reward_tokens);
    }

    Ok(FundingStep {
        cost_usd: U256::zero(),
    })
}
