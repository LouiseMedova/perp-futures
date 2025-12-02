use std::{collections::HashMap, hash::Hash};

use crate::types::{AssetId, MarketId, TokenAmount};

/// Simple pool balances storage.
///
/// For each market you typically have two assets:
///  - long_token  (e.g. WETH, BTC, etc.)
///  - short_token (e.g. USDC, USDT, etc.)
#[derive(Debug, Default, Clone)]
pub struct PoolBalances {
    /// Total liquidity in tokens for each (market, asset).
    pub liquidity: HashMap<(MarketId, AssetId), TokenAmount>,
    /// Accumulated trading / borrowing fees for each (market, asset).
    pub fees: HashMap<(MarketId, AssetId), TokenAmount>,
}

impl PoolBalances {
    pub fn new() -> Self {
        Self {
            liquidity: HashMap::new(),
            fees: HashMap::new(),
        }
    }

    /// Add trading fees to the pool for a specific (market, asset).
    pub fn add_fee_to_pool(&mut self, market_id: MarketId, asset: AssetId, amount: TokenAmount) {
        if amount == 0 {
            return;
        }

        let entry = self.fees.entry((market_id, asset)).or_insert(0);
        *entry = entry.saturating_add(amount);
    }

    /// Add liquidity for a single asset (either long or short) to a market pool.
    ///
    /// In a real protocol we should also:
    ///  - mint LP shares,
    ///  - track the LP's ownership,
    ///  - enforce ratios between long/short side, etc.
    ///
    /// For MVP we just bump the raw pool balance.
    pub fn add_liquidity(&mut self, market_id: MarketId, asset: AssetId, amount: TokenAmount) {
        if amount == 0 {
            return;
        }

        let entry = self.liquidity.entry((market_id, asset)).or_insert(0);
        *entry = entry.saturating_add(amount);
    }

    /// Add liquidity for both sides of a 2-token pool (long + short) at once.
    pub fn add_liquidity_pair(
        &mut self,
        market_id: MarketId,
        long_asset: AssetId,
        long_amount: TokenAmount,
        short_asset: AssetId,
        short_amount: TokenAmount,
    ) {
        if long_amount > 0 {
            self.add_liquidity(market_id, long_asset, long_amount);
        }
        if short_amount > 0 {
            self.add_liquidity(market_id, short_asset, short_amount);
        }
    }

    /// Remove liquidity for a single asset (either long or short) from a market pool.
    pub fn remove_liquidity(
        &mut self,
        market_id: MarketId,
        asset: AssetId,
        amount: TokenAmount,
    ) -> Result<TokenAmount, String> {
        if amount == 0 {
            return Ok(0);
        }

        let key = (market_id, asset);
        let bal = self.liquidity.entry(key).or_insert(0);

        if *bal < amount {
            return Err("insufficient_pool_liquidity".into());
        }

        *bal -= amount;
        Ok(amount)
    }

    /// Convenience: remove liquidity for both long and short tokens at once.
    pub fn remove_liquidity_pair(
        &mut self,
        market_id: MarketId,
        long_asset: AssetId,
        long_amount: TokenAmount,
        short_asset: AssetId,
        short_amount: TokenAmount,
    ) -> Result<(TokenAmount, TokenAmount), String> {
        let taken_long = self.remove_liquidity(market_id, long_asset, long_amount)?;
        let taken_short = self.remove_liquidity(market_id, short_asset, short_amount)?;
        Ok((taken_long, taken_short))
    }

    /// Read current pool balance for (market, asset) without modifying it.
    pub fn get_balance(&self, market_id: MarketId, asset: AssetId) -> TokenAmount {
        self.liquidity.get(&(market_id, asset)).cloned().unwrap_or(0)
    }

    /// Get both sides of a 2-token pool for a given market.
    pub fn get_pair_balances(
        &self,
        market_id: MarketId,
        long_asset: AssetId,
        short_asset: AssetId,
    ) -> (TokenAmount, TokenAmount) {
        let long_bal = self.get_balance(market_id, long_asset);
        let short_bal = self.get_balance(market_id, short_asset);
        (long_bal, short_bal)
    }

     pub fn get_fee_for_pool(&self, market_id: MarketId, asset: AssetId) -> TokenAmount {
        *self.fees.get(&(market_id, asset)).unwrap_or(&0)
    }
}
