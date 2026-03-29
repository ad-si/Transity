#[derive(Debug, Clone)]
pub struct GplotConfig {
    pub title: String,
    pub label_y: String,
    pub terminal_type: String,
    pub line_color: String,
    pub data: String,
    pub img_width: u32,
    pub img_height: u32,
}

impl Default for GplotConfig {
    fn default() -> Self {
        GplotConfig {
            title: "Made with Transity".to_string(),
            label_y: "Commodity".to_string(),
            terminal_type: "pngcairo".to_string(),
            line_color: "#808080".to_string(),
            data: String::new(),
            img_width: 840,
            img_height: 420,
        }
    }
}

fn gplot_table(data: &str) -> String {
    format!("$data << EOD\n{}\nEOD\n", data)
}

fn gplot_common_commands(config: &GplotConfig) -> Vec<String> {
    vec![
        format!(
            "set terminal {} size {}, {}",
            config.terminal_type, config.img_width, config.img_height
        ),
        format!("set title '{}'", config.title),
        "set key outside nobox".to_string(),
        format!(
            "set style line 12 lc rgb'{}' lt 0 lw 1",
            config.line_color
        ),
        "set grid back ls 12".to_string(),
        "set grid xtics ytics mxtics".to_string(),
    ]
}

fn gplot_time_axis_commands() -> Vec<String> {
    vec![
        "set xdata time".to_string(),
        "set timefmt '%Y-%m-%dT%H:%M:%S'".to_string(),
        "set format x '%Y-W%W'".to_string(),
        "set xlabel 'ISO Week'".to_string(),
        "set xtics rotate by 30 right".to_string(),
        "set zeroaxis".to_string(),
    ]
}

pub fn gplot_code(config: &GplotConfig) -> String {
    let mut parts = gplot_common_commands(config);
    parts.push("set style fill solid".to_string());
    parts.extend(gplot_time_axis_commands());
    parts.push(format!("set ylabel '{}'", config.label_y));
    parts.push(
        "plot for [i=0:*] $data index i using 1:3 with impulses title columnhead(1)".to_string(),
    );
    parts.push(String::new());
    gplot_table(&config.data) + &parts.join(";")
}

pub fn gplot_code_cumul(config: &GplotConfig) -> String {
    let mut parts = gplot_common_commands(config);
    parts.extend(gplot_time_axis_commands());
    parts.push("set yrange [*<0:0<*]".to_string());
    parts.push(format!("set ylabel '{}'", config.label_y));
    parts.push(
        "plot for [i=0:*] $data index i using 1:3 smooth cumulative with fillsteps title columnhead(1)".to_string(),
    );
    parts.push(String::new());
    gplot_table(&config.data) + &parts.join(";")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gplot_table_wraps_with_eod_markers() {
        let result = gplot_table("some data\nmore data");
        assert_eq!(result, "$data << EOD\nsome data\nmore data\nEOD\n");
    }

    #[test]
    fn test_gplot_table_empty_data() {
        let result = gplot_table("");
        assert_eq!(result, "$data << EOD\n\nEOD\n");
    }

    #[test]
    fn test_gplot_code_semicolon_separated() {
        let config = GplotConfig::default();
        let result = gplot_code(&config);
        let after_eod = result.split("\nEOD\n").last().unwrap();
        assert!(after_eod.contains(';'));
        assert!(after_eod.contains("set terminal pngcairo size 840, 420"));
        assert!(after_eod.contains("set title 'Made with Transity'"));
        assert!(after_eod.contains("set style fill solid"));
        assert!(after_eod.contains("with impulses"));
    }

    #[test]
    fn test_gplot_code_cumul_semicolon_separated() {
        let config = GplotConfig::default();
        let result = gplot_code_cumul(&config);
        let after_eod = result.split("\nEOD\n").last().unwrap();
        assert!(after_eod.contains(';'));
        assert!(after_eod.contains("set yrange [*<0:0<*]"));
        assert!(!after_eod.contains("set style fill solid"));
        assert!(after_eod.contains("smooth cumulative with fillsteps"));
    }

    #[test]
    fn test_gplot_code_snapshot() {
        let config = GplotConfig {
            data: "2024-01-01T00:00:00 account1 100 USD".to_string(),
            ..GplotConfig::default()
        };
        insta::assert_snapshot!(gplot_code(&config));
    }

    #[test]
    fn test_gplot_code_cumul_snapshot() {
        let config = GplotConfig::default();
        insta::assert_snapshot!(gplot_code_cumul(&config));
    }
}
