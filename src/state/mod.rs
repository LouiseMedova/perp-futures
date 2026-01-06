// src/state/mod.rs

mod claimables;
mod market_state;
mod order_store;
mod pool_balances;
mod position_store;

pub use claimables::*;
pub use market_state::*;
pub use order_store::*;
pub use pool_balances::*;
pub use position_store::*;

use crate::types::*;
use std::collections::HashMap;

#[derive(Default, Clone)]
pub struct State {
    pub positions: PositionStore,
    pub markets: HashMap<MarketId, MarketState>,
    pub pool_balances: PoolBalances,
    pub claimables: Claimables,
    pub orders: OrderStore,
}
