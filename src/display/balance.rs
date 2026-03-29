use std::collections::BTreeMap;
use crate::data::amount::{WidthRecord, pad_start};
use crate::data::commodity_map::{
    CommodityMap, add_amount_to_map, subtract_amount_from_map,
    commodity_map_to_width_record, show_commodity_map_aligned,
};
use crate::data::ledger::Ledger;
use crate::data::transfer::Transfer;

pub enum BalanceFilter {
    OnlyOwner,
    All,
    Only(String),
}

type BalanceMap = BTreeMap<String, CommodityMap>;

fn add_transfer(map: &mut BalanceMap, transfer: &Transfer) {
    let qualify = |s: &str| {
        if s.contains(':') { s.to_string() } else { format!("{}:_default_", s) }
    };
    let sender = qualify(&transfer.from);
    let receiver = qualify(&transfer.to);
    subtract_amount_from_map(map.entry(sender).or_default(), &transfer.amount);
    add_amount_to_map(map.entry(receiver).or_default(), &transfer.amount);
}

fn normalize_acc_id(acc_id: &str) -> &str {
    acc_id.strip_suffix(":_default_").unwrap_or(acc_id)
}

fn show_account_aligned(width_rec: &WidthRecord, acc_id: &str, com_map: &CommodityMap) -> String {
    let account_width = width_rec.account.max(acc_id.chars().count());
    let commodity_str = show_commodity_map_aligned(
        width_rec.integer, width_rec.fraction, width_rec.commodity, com_map,
    );
    let indent = " ".repeat(account_width + 2);
    let indented = commodity_str.replace('\n', &format!("\n{}", indent));
    format!("{}  {}\n", pad_start(account_width, acc_id), indented)
}

pub fn show_balance(filter: &BalanceFilter, ledger: &Ledger) -> String {
    let mut balance_map: BalanceMap = BTreeMap::new();
    for transaction in &ledger.transactions {
        for transfer in transaction.to_transfers() {
            add_transfer(&mut balance_map, &transfer);
        }
    }

    let matches: Box<dyn Fn(&str) -> bool> = match filter {
        BalanceFilter::All => Box::new(|_| true),
        BalanceFilter::OnlyOwner => {
            let prefix = format!("{}:", ledger.owner.as_deref().unwrap_or(""));
            let owner = ledger.owner.clone().unwrap_or_default();
            Box::new(move |id: &str| id == owner || id.starts_with(&prefix))
        }
        BalanceFilter::Only(acc) => {
            let prefix = format!("{}:", acc);
            let acc = acc.clone();
            Box::new(move |id: &str| id == acc || id.starts_with(&prefix))
        }
    };

    let entries: Vec<(String, CommodityMap)> = balance_map
        .into_iter()
        .filter(|(id, _)| matches(id))
        .map(|(id, cm)| {
            let filtered: CommodityMap = cm.into_iter().filter(|(_, a)| !a.is_zero()).collect();
            (id, filtered)
        })
        .filter(|(_, cm)| !cm.is_empty())
        .collect();

    if entries.is_empty() {
        return String::new();
    }

    let width_rec = entries
        .iter()
        .map(|(id, cm)| {
            let norm = normalize_acc_id(id);
            let cw = commodity_map_to_width_record(cm);
            WidthRecord { account: norm.chars().count().max(cw.account), ..cw }
        })
        .fold(WidthRecord::zero(), |a, b| a.merge(&b));

    let display_rec = WidthRecord { account: width_rec.account + 2, ..width_rec };

    let output: String = entries
        .iter()
        .map(|(id, cm)| show_account_aligned(&display_rec, normalize_acc_id(id), cm))
        .collect();

    output + "\n\n"
}
