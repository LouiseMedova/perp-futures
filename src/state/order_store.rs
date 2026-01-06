use std::collections::HashMap;

use crate::types::{Order, OrderId};

#[derive(Default, Clone)]
pub struct OrderStore {
    orders: HashMap<OrderId, Order>,
    next_id: u64,
}

impl OrderStore {
    /// Создать новое хранилище ордеров.
    pub fn new() -> Self {
        Self {
            orders: HashMap::new(),
            next_id: 0,
        }
    }

    /// Положить ордер в pending, вернуть его OrderId.
    pub fn create(&mut self, order: Order) -> OrderId {
        let id = OrderId(self.next_id);
        self.next_id = self.next_id.checked_add(1).expect("order id overflow"); // на практике это невозможно
        self.orders.insert(id, order);
        id
    }

    /// Получить ордер по id (только чтение).
    pub fn get(&self, id: OrderId) -> Option<&Order> {
        self.orders.get(&id)
    }

    /// Получить ордер по id для изменения.
    /// В текущем флоу почти не нужно (мы обычно просто читаем и потом remove),
    /// но на будущее оставим.
    pub fn get_mut(&mut self, id: OrderId) -> Option<&mut Order> {
        self.orders.get_mut(&id)
    }

    /// Удалить ордер из pending по id, вернуть его, если был.
    pub fn remove(&mut self, id: OrderId) -> Option<Order> {
        self.orders.remove(&id)
    }

    /// Проверить, есть ли ордер с таким id.
    pub fn contains(&self, id: OrderId) -> bool {
        self.orders.contains_key(&id)
    }

    /// Количество активных pending-ордеров.
    pub fn len(&self) -> usize {
        self.orders.len()
    }

    pub fn is_empty(&self) -> bool {
        self.orders.is_empty()
    }

    /// Итератор по всем (OrderId, Order).
    pub fn iter(&self) -> impl Iterator<Item = (&OrderId, &Order)> {
        self.orders.iter()
    }
}
