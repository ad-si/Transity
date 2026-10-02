//! Renders trend charts with Apache ECharts (via charming).
//! ECharts itself is vendored in `vendor/echarts/` and served by the server.

use charming::component::{
  Axis, DataZoom, DataZoomType, FilterMode, Grid, Legend, LegendType,
};
use charming::datatype::{CompositeValue, DataPoint};
use charming::element::{
  AxisLabel, AxisPointer, AxisPointerType, AxisType, Formatter, ItemStyle,
  JsFunction, LineStyle, SplitLine, Step, TextStyle, Tooltip, Trigger,
};
use charming::series::Line;
use charming::{Chart, Echarts, WasmRenderer};
use leptos::prelude::*;
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

use crate::trends::TrendSeries;

#[wasm_bindgen]
extern "C" {
  #[wasm_bindgen(js_namespace = echarts, js_name = dispose)]
  fn dispose_chart(chart: &JsValue);
}

/// Resolved value of a CSS custom property on the root element.
/// The canvas renderer of ECharts can't resolve `var(...)` itself.
fn css_var(name: &str) -> String {
  let value = (|| {
    let window = web_sys::window()?;
    let root = window.document()?.document_element()?;
    let style = window.get_computed_style(&root).ok()??;
    style.get_property_value(name).ok()
  })();
  value.unwrap_or_default().trim().to_string()
}

/// Incremented whenever the preferred color scheme changes,
/// so that charts can re-read their colors.
fn color_scheme_version() -> RwSignal<u32> {
  thread_local! {
    static VERSION: RwSignal<u32> = {
      let version = RwSignal::new(0);
      let query = web_sys::window()
        .and_then(|w| w.match_media("(prefers-color-scheme: dark)").ok())
        .flatten();
      if let Some(query) = query {
        let on_change = Closure::<dyn Fn()>::new(move || {
          version.update(|v| *v += 1);
        });
        let _ = query.add_event_listener_with_callback(
          "change",
          on_change.as_ref().unchecked_ref(),
        );
        // Lives as long as the page
        on_change.forget();
      }
      version
    };
  }
  VERSION.with(|v| *v)
}

fn series_color(series: &TrendSeries, is_total: bool) -> String {
  match (series.color_slot, is_total) {
    (_, true) => css_var("--color-text"),
    (Some(slot), _) => css_var(&format!("--series-{}", slot + 1)),
    (None, _) => css_var("--series-other"),
  }
}

/// Milliseconds since the Unix epoch for a `num_days_from_ce` day
fn day_to_millis(day: i32) -> f64 {
  // 0001-01-01 is day 1, 1970-01-01 is day 719_163
  (day as f64 - 719_163.0) * 86_400_000.0
}

fn tooltip_formatter(unit: &str, decimals: (u8, u8)) -> JsFunction {
  let unit = serde_json::to_string(unit).unwrap_or_else(|_| "\"\"".into());
  let (min, max) = decimals;
  JsFunction::new_with_args(
    "params",
    &format!(
      r#"
      if (!params.length) return '';
      const esc = s => String(s).replace(/[&<>"']/g,
        c => '&#' + c.charCodeAt(0) + ';');
      const fmt = v => v.toLocaleString('en-US', {{
        minimumFractionDigits: {min},
        maximumFractionDigits: {max},
      }});
      const date = new Date(params[0].value[0]).toISOString().slice(0, 10);
      const rows = params
        .filter(p => p.value[1] != null)
        .map(p =>
          '<li class="trend-tooltip-row">' +
          '<span class="trend-key" style="background:' + esc(p.color) +
          '"></span>' +
          '<strong class="trend-tooltip-value">' +
          esc(fmt(p.value[1]) + ' ' + {unit}) + '</strong>' +
          '<span class="trend-tooltip-label">' + esc(p.seriesName) +
          '</span></li>')
        .join('');
      return '<div class="trend-tooltip-date">' + date + '</div>' +
        '<ul>' + rows + '</ul>';
      "#
    ),
  )
}

fn compact_formatter() -> JsFunction {
  JsFunction::new_with_args(
    "v",
    r#"
    const a = Math.abs(v);
    if (a >= 1e9) return +(v / 1e9).toFixed(2) + 'B';
    if (a >= 1e6) return +(v / 1e6).toFixed(2) + 'M';
    if (a >= 1e4) return +(v / 1e3).toFixed(2) + 'k';
    return String(+v.toFixed(2));
    "#,
  )
}

fn build_chart(
  unit: &str,
  days: &[i32],
  series: &[TrendSeries],
  converted: bool,
) -> Chart {
  let muted = css_var("--color-muted");
  let border = css_var("--color-border");
  let border_table = css_var("--color-border-table");
  // Converted values are money, native ones can be fractional units
  let decimals = if converted { (2, 2) } else { (0, 8) };
  let total_index = (converted && series.len() > 1).then(|| series.len() - 1);

  let mut chart = Chart::new()
    .animation(false)
    .grid(
      Grid::new()
        .left(8)
        .right(16)
        .top(40)
        .bottom(64)
        .contain_label(true),
    )
    .legend(
      Legend::new()
        .type_(LegendType::Scroll)
        .left(0)
        .top(0)
        .text_style(TextStyle::new().color(muted.as_str()))
        .inactive_color(border_table.as_str())
        // Line keys instead of the default line-with-circle icons
        .item_width(14)
        .item_height(3)
        .data(
          series
            .iter()
            .map(|s| (s.label.clone(), "roundRect".to_string()))
            .collect::<Vec<_>>(),
        ),
    )
    .tooltip(
      Tooltip::new()
        .trigger(Trigger::Axis)
        .axis_pointer(
          AxisPointer::new()
            .type_(AxisPointerType::Line)
            .line_style(LineStyle::new().color(muted.as_str())),
        )
        .background_color("var(--bg-nav)")
        .border_color("var(--color-border)")
        .formatter(Formatter::Function(tooltip_formatter(unit, decimals))),
    )
    .x_axis(
      Axis::new()
        .type_(AxisType::Time)
        .axis_label(AxisLabel::new().color(muted.as_str()))
        .split_line(SplitLine::new().show(false)),
    )
    .y_axis(
      Axis::new()
        .type_(AxisType::Value)
        .axis_label(
          AxisLabel::new()
            .color(muted.as_str())
            .formatter(Formatter::Function(compact_formatter())),
        )
        .split_line(
          SplitLine::new().line_style(LineStyle::new().color(border.as_str())),
        ),
    )
    .data_zoom(
      DataZoom::new()
        .type_(DataZoomType::Inside)
        // Keep lines continuous when their points lie outside the window
        .filter_mode(FilterMode::None),
    )
    .data_zoom(
      DataZoom::new()
        .type_(DataZoomType::Slider)
        .filter_mode(FilterMode::None)
        .bottom(8)
        .border_color(border_table.as_str())
        .text_style(TextStyle::new().color(muted.as_str())),
    );

  for (i, s) in series.iter().enumerate() {
    let is_total = total_index == Some(i);
    let color = series_color(s, is_total);
    let data: Vec<DataPoint> = days
      .iter()
      .zip(&s.values)
      .map(|(day, value)| {
        DataPoint::from(CompositeValue::from(vec![
          CompositeValue::from(day_to_millis(*day)),
          CompositeValue::from(*value),
        ]))
      })
      .collect();
    chart = chart.series(
      Line::new()
        .name(s.label.as_str())
        .step(Step::End)
        .show_symbol(false)
        .z(if is_total { 3 } else { 2 })
        .line_style(LineStyle::new().color(color.as_str()).width(if is_total {
          2.5
        } else {
          2.0
        }))
        .item_style(ItemStyle::new().color(color.as_str()))
        .data(data),
    );
  }

  chart
}

static NEXT_ID: std::sync::atomic::AtomicUsize =
  std::sync::atomic::AtomicUsize::new(0);

pub fn next_chart_id() -> String {
  let id = NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
  format!("trend-chart-{id}")
}

/// Renders the chart into the element with the given id once it is
/// mounted, re-renders it when the color scheme changes,
/// keeps it sized to its container, and disposes it on unmount.
pub fn mount_chart(
  id: String,
  unit: String,
  days: Vec<i32>,
  series: Vec<TrendSeries>,
  converted: bool,
) {
  let state: Rc<RefCell<Option<Mounted>>> = Rc::new(RefCell::new(None));
  let scheme = color_scheme_version();

  Effect::new({
    let state = state.clone();
    move |_| {
      scheme.track();
      let chart = build_chart(&unit, &days, &series, converted);
      let mut current = state.borrow_mut();
      match current.as_ref() {
        Some(mounted) => WasmRenderer::update(&mounted.echarts, &chart),
        None => match Mounted::new(&id, &chart) {
          Ok(mounted) => *current = Some(mounted),
          Err(e) => leptos::logging::error!("Failed to render chart: {e}"),
        },
      }
    }
  });

  let state = send_wrapper::SendWrapper::new(state);
  on_cleanup(move || {
    if let Some(mounted) = state.borrow_mut().take() {
      mounted.observer.disconnect();
      dispose_chart(mounted.echarts.as_ref());
    }
  });
}

/// A rendered chart that follows the size of its container
struct Mounted {
  echarts: Echarts,
  observer: web_sys::ResizeObserver,
  // Must outlive the observer
  _on_resize: Closure<dyn Fn()>,
}

impl Mounted {
  fn new(id: &str, chart: &Chart) -> Result<Mounted, String> {
    let element = web_sys::window()
      .and_then(|w| w.document())
      .and_then(|d| d.get_element_by_id(id))
      .ok_or_else(|| format!("no element with id `{id}`"))?;
    let echarts = WasmRenderer::new_opt(None, None)
      .render(id, chart)
      .map_err(|e| format!("{e:?}"))?;
    let on_resize = Closure::<dyn Fn()>::new({
      // `Echarts` isn't `Clone`; clone the underlying JS reference
      let echarts: Echarts = JsValue::clone(&echarts).unchecked_into();
      move || echarts.resize(JsValue::UNDEFINED)
    });
    let observer =
      web_sys::ResizeObserver::new(on_resize.as_ref().unchecked_ref())
        .map_err(|e| format!("{e:?}"))?;
    observer.observe(&element);
    Ok(Mounted {
      echarts,
      observer,
      _on_resize: on_resize,
    })
  }
}
