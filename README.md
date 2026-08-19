# cycling-fitness

A small R toolkit for analyzing indoor cycling workouts. It reads Garmin-style
`.tcx` files exported from a spin bike (power, heart rate, cadence), then builds
per-ride charts and long-term trend charts comparing your latest ride against
your whole history.

The headline metric is **efficiency = power / heart rate**, i.e. watt-minutes of
work per heartbeat — a rough proxy for aerobic fitness that should drift upward
as conditioning improves.

## What it produces

Running the analysis generates, from all `.tcx` files in the working directory:

- **`report.html`** — a single self-contained dashboard (charts embedded as
  data URIs, opens automatically in your browser) with stat cards, this-ride
  charts, a recent-rides table, and long-term trends.
- **`plots/*.png`** — the individual charts.
- **`daily-summary.csv`** — one row per ride (duration, median power, median HR,
  efficiency, energy).
- A short text summary printed to the console.

Each time-series chart overlays your latest ride on a "median ride" (the typical
value at each elapsed second across all history), plus reference lines for this
ride's median and your all-time median.

## Usage

Place your `.tcx` files in the repo directory, then:

```bash
Rscript analyze-ride.R
```

On macOS you can also double-click **`ride.command`** in Finder to run it.

### Requirements

- R (developed on 4.2)
- Packages: `tidyverse`, `lubridate`, `xml2`, `base64enc`

```r
install.packages(c("tidyverse", "lubridate", "xml2", "base64enc"))
```

`analyze-ride.R` is self-contained and parses TCX with `xml2` directly, so it
does **not** require the `trackeR` package.

## Files

| File | Purpose |
|------|---------|
| `analyze-ride.R` | Main script — TCX parsing, charts, HTML dashboard. Start here. |
| `ride.command` | Double-clickable macOS launcher for `analyze-ride.R`. |
| `cycling-post-wko.R` | Earlier post-workout report (requires `trackeR`). |
| `cycling-post-wko-lean.R` | Lighter variant that appends to an existing summary. |
| `cycling-explore.R` | Exploratory / scratch analysis (requires `trackeR`). |

## Data & privacy

Ride files (`.tcx`) and generated outputs (charts, `report.html`,
`daily-summary.csv`) are gitignored and are **not** part of this repository —
these workout files can contain personal health and location data. Bring your
own `.tcx` exports to run the analysis.
