use leptos::prelude::*;

use crate::trends::{TrendData, TrendSeries};

#[cfg(feature = "hydrate")]
mod echart;

#[server]
pub async fn get_trends() -> Result<TrendData, ServerFnError> {
  let loader = expect_context::<crate::server::LedgerLoader>();
  let ledger = loader
    .load()
    .map_err(|e| ServerFnError::new(format!("{:#}", e)))?;
  Ok(crate::trends::get_trend_data(&ledger))
}

#[component]
pub fn TrendsPage() -> impl IntoView {
  let trends = Resource::new(|| (), |_| get_trends());
  // Lives outside of the charts so that a reload keeps the selected view
  let converted_mode = RwSignal::new(false);
  let inflation_adjusted = RwSignal::new(false);
  let reloading = RwSignal::new(false);
  #[cfg(feature = "hydrate")]
  provide_context(echart::ChartViews::new());

  let reload_button = move || {
    view! {
      <button
        class="tx-toolbar-button trend-reload"
        on:click=move |_| trends.refetch()
        type="button"
        disabled=move || reloading.get()
        title="Reload the journal"
      >
        {move || if reloading.get() { "Reloading…" } else { "Reload" }}
      </button>
    }
    .into_any()
  };

  view! {
    <Transition
      fallback=move || view! { <p class="loading">"Loading..."</p> }
      set_pending=reloading.write_only()
    >
      {move || Suspend::new(async move {
        match trends.await {
          Ok(data) => view! {
            <Trends
              data
              converted_mode
              inflation_adjusted
              reload_button=reload_button()
            />
          }.into_any(),
          Err(e) => view! {
            <div class="tx-toolbar">{reload_button()}</div>
            <p class="error">{format!("Error: {e}")}</p>
          }.into_any(),
        }
      })}
    </Transition>
  }
}

#[component]
fn Trends(
  data: TrendData,
  converted_mode: RwSignal<bool>,
  inflation_adjusted: RwSignal<bool>,
  reload_button: AnyView,
) -> impl IntoView {
  if data.days.is_empty() {
    return view! {
      <div class="tx-toolbar">{reload_button}</div>
      <p class="loading">"No dated transactions to plot."</p>
    }
    .into_any();
  }

  let currency = data.currency.clone().unwrap_or_default();
  // The main currency might be gone after a reload
  if currency.is_empty() {
    converted_mode.set(false);
  }
  // The price index might be gone after a reload
  if data.inflation.is_none() {
    inflation_adjusted.set(false);
  }
  let button_class = move |active: bool| {
    if active {
      "tx-toolbar-button active"
    } else {
      "tx-toolbar-button"
    }
  };
  let days = data.days;
  let native = data.native;
  let converted = data.converted;
  let unconvertible = data.unconvertible;
  let inflation = data.inflation;
  let has_inflation = inflation.is_some();

  let charts = {
    let days = days.clone();
    let currency = currency.clone();
    move || {
      if converted_mode.get() {
        let note = (!unconvertible.is_empty()).then(|| {
          view! {
            <p class="trend-note">
              {format!(
                "No exchange rate to {} found for: {}. \
                 These holdings are not included.",
                currency,
                unconvertible.join(", "),
              )}
            </p>
          }
        });
        let mut series = converted.clone();
        let mut title = format!("Value in {currency}");
        let mut index_note = None;
        if let Some(adj) =
          inflation.as_ref().filter(|_| inflation_adjusted.get())
        {
          for s in &mut series {
            for (value, factor) in s.values.iter_mut().zip(&adj.factors) {
              *value = value.zip(*factor).map(|(v, f)| v * f);
            }
          }
          title = format!("{title} at prices of {}", format_day(adj.base_day));
          let first = adj.factors.iter().position(Option::is_some);
          index_note = (first != Some(0)).then(|| {
            let msg = match first {
              Some(i) => format!(
                "The price index of {} starts at {}. \
                 Earlier values are not shown.",
                currency,
                format_day(days[i]),
              ),
              None => format!("The price index of {currency} has no values."),
            };
            view! { <p class="trend-note">{msg}</p> }
          });
        }
        if series.len() > 1 {
          series.push(TrendSeries {
            label: "Total".to_string(),
            color_slot: None,
            values: (0..days.len())
              .map(|i| {
                series
                  .iter()
                  .filter_map(|s| s.values[i])
                  .reduce(|a, b| a + b)
              })
              .collect(),
          });
        }
        view! {
          {note}
          {index_note}
          <TrendChart
            key=format!("Value in {currency}")
            title
            unit=currency.clone()
            days=days.clone()
            series
            converted=true
          />
        }
        .into_any()
      } else {
        native
          .iter()
          .map(|trend| {
            view! {
              <TrendChart
                key=trend.commodity.clone()
                title=trend.commodity.clone()
                unit=trend.commodity.clone()
                days=days.clone()
                series=trend.series.clone()
                converted=false
              />
            }
          })
          .collect_view()
          .into_any()
      }
    }
  };

  view! {
    <div class="tx-toolbar" role="group" aria-label="Unit">
      <button
        class=move || button_class(!converted_mode.get())
        on:click=move |_| converted_mode.set(false)
        type="button"
        aria-pressed=move || (!converted_mode.get()).to_string()
      >
        "Commodities"
      </button>
      <button
        class=move || button_class(converted_mode.get())
        on:click=move |_| converted_mode.set(true)
        type="button"
        aria-pressed=move || converted_mode.get().to_string()
        disabled=currency.is_empty()
      >
        {format!("Value in {currency}")}
      </button>
      <button
        class=move || button_class(inflation_adjusted.get())
        on:click=move |_| inflation_adjusted.update(|on| *on = !*on)
        type="button"
        aria-pressed=move || inflation_adjusted.get().to_string()
        disabled=move || !has_inflation || !converted_mode.get()
        title=if has_inflation {
          "Adjust the values for inflation with the declared price index"
            .to_string()
        } else {
          format!("Declare `price-indices` for {currency} to enable this")
        }
      >
        "Inflation adjusted"
      </button>
      {reload_button}
    </div>
    <div class="trend-charts">{charts}</div>
  }
  .into_any()
}

/// ISO date of a `num_days_from_ce` day
fn format_day(day: i32) -> String {
  chrono::NaiveDate::from_num_days_from_ce_opt(day)
    .map(|d| d.to_string())
    .unwrap_or_default()
}

/// A chart rendered client-side by ECharts into an empty container
#[component]
fn TrendChart(
  /// Identifies the chart to keep its zoom window and legend selection
  key: String,
  title: String,
  unit: String,
  days: Vec<i32>,
  series: Vec<TrendSeries>,
  /// Values are converted into the main currency
  /// and the last series is the total of all others
  converted: bool,
) -> impl IntoView {
  #[cfg(feature = "hydrate")]
  let id = {
    let id = echart::next_chart_id();
    echart::mount_chart(id.clone(), key, unit, days, series, converted);
    id
  };
  #[cfg(not(feature = "hydrate"))]
  let id = {
    let _ = (key, unit, days, series, converted);
    String::new()
  };

  view! {
    <section class="trend-chart">
      <h2>{title}</h2>
      <div id=id class="trend-echart"></div>
    </section>
  }
}
