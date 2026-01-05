use primitive_types::U256;

use crate::services::BorrowingService;
use crate::services::FundingService;
use crate::services::borrowing_step::{BorrowingStep, apply_borrowing_step};
use crate::services::fees::{FeesService, StepFees};
use crate::services::funding_step::{FundingStep, apply_funding_step};
use crate::state::{Claimables, MarketState, Position};
use crate::types::{OraclePrices, Order, TokenAmount, Usd};
/// Full cost breakdown for a single "step" (one position update).
#[derive(Debug, Clone)]
pub struct StepCosts {
    /// Funding cost (payer side only), in USD.
    pub funding_usd: Usd,
    /// Borrowing cost in USD.
    pub borrowing_usd: Usd,
    /// Borrowing cost converted to collateral tokens (for pool yield).
    pub borrowing_tokens: TokenAmount,
    /// Trading cost (position + liquidation fees), in USD.
    pub trading_usd: Usd,
    /// Total step cost in USD (funding + borrowing + trading).
    pub total_usd: Usd,

    /// Detailed trading-related fees (position + liquidation).
    pub trading_fees: StepFees,
}

/// Compute all per-step costs: funding + borrowing + trading.
///
/// Side effects:
///  - updates funding snapshot in the position;
///  - adds funding rewards into Claimables (for receiver side);
///  - does NOT yet touch collateral or pool balances.
pub fn compute_step_costs<F, B, Fe>(
    funding_svc: &F,
    borrowing_svc: &B,
    fees_svc: &Fe,
    market: &MarketState,
    pos: &mut Position,
    claimables: &mut Claimables,
    prices: &OraclePrices,
    order: &Order,
    balance_was_improved: bool,
    size_delta_usd: Usd,
) -> Result<StepCosts, String>
where
    F: FundingService,
    B: BorrowingService,
    Fe: FeesService,
{
    // 1) Funding: updates pos.funding_index and claimables (for receiver side).
    let funding_step = apply_funding_step(funding_svc, market, pos, claimables, prices)?;

    // 2) Borrowing: cost in USD for this step.
    let borrowing_step = apply_borrowing_step(borrowing_svc, market, pos);

    // Convert borrowing from USD to collateral tokens (for pool yield).
    let borrowing_tokens: TokenAmount = if prices.collateral_price_min > U256::zero() {
        borrowing_step.cost_usd / prices.collateral_price_min
    } else {
        U256::zero()
    };

    // 3) Trading fees (position + liquidation).
    let trading_fees =
        fees_svc.compute_fees(pos, order, prices, balance_was_improved, size_delta_usd)?;

    let funding_usd = funding_step.cost_usd;
    let borrowing_usd = borrowing_step.cost_usd;
    let trading_usd = trading_fees.position_fee_usd + trading_fees.liquidation_fee_usd;

    let total_usd = funding_usd + borrowing_usd + trading_usd;

    Ok(StepCosts {
        funding_usd,
        borrowing_usd,
        borrowing_tokens,
        trading_usd,
        total_usd,
        trading_fees,
    })
}

/// Apply all step costs to position collateral.
///
/// total_usd = funding + borrowing + trading.
/// We convert total_usd → collateral tokens via collateral_price_min and
/// subtract from pos.collateral_amount, reverting on insufficient collateral.
pub fn apply_step_costs_to_position(
    pos: &mut Position,
    prices: &OraclePrices,
    step_costs: &StepCosts,
) -> Result<(), String> {
    if prices.collateral_price_min <= U256::zero() {
        return Err("invalid_collateral_price_min".into());
    }

    let total_tokens_cost: TokenAmount = step_costs.total_usd / prices.collateral_price_min;

    if total_tokens_cost > pos.collateral_amount {
        return Err("insufficient_collateral_for_step_costs".into());
    }
    pos.collateral_amount -= total_tokens_cost;
    Ok(())
}
