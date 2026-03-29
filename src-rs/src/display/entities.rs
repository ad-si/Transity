use crate::data::{Account, Entity, Ledger};
use crate::utils::{date_show_pretty_long, SortOrder};

const NO_ENTITIES_MSG: &str = "Journal does not contain any entities";

pub fn show_entities(sort: &SortOrder, ledger: &Ledger) -> String {
    let entities = match &ledger.entities {
        Some(e) if !e.is_empty() => e,
        _ => return NO_ENTITIES_MSG.to_string(),
    };

    let body: String = if matches!(sort, SortOrder::Alphabetically) {
        let mut ents = entities.clone();
        ents.sort_by(|a, b| a.id.to_lowercase().cmp(&b.id.to_lowercase()));
        ents.iter().map(show_entity).collect()
    } else {
        entities.iter().map(show_entity).collect()
    };

    format!("entities:\n{}", body)
}

fn show_entity(entity: &Entity) -> String {
    let mut s = format!("  - id: {}\n", entity.id);
    if let Some(name) = &entity.name {
        s += &format!("    name: {}\n", name);
    }
    if let Some(note) = &entity.note {
        s += &format!("    note: {}\n", note);
    }
    if let Some(utc) = &entity.utc {
        s += &format!("    utc: {}\n", date_show_pretty_long(utc));
    }
    if let Some(tags) = &entity.tags {
        s += &format!("    tags: {}\n", format_tags(tags));
    }
    if let Some(accounts) = &entity.accounts {
        s += &format!("    accounts: {}\n", format_accounts(accounts));
    }
    s += "\n";
    s
}

// PureScript `show tags` with `"` stripped: [tag1,tag2]
fn format_tags(tags: &[String]) -> String {
    format!("[{}]", tags.join(","))
}

fn format_accounts(accounts: &[Account]) -> String {
    accounts
        .iter()
        .map(|acc| format!("\n      - {}", acc.to_json_string()))
        .collect()
}
