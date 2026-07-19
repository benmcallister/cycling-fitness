## analyze-ride.R
## Self-contained cycling analysis that does NOT require trackeR.
## readTCX is replaced by a small xml2-based parser (read_tcx) so this runs
## on the R 4.2 library where trackeR isn't installed.
## Outputs: PNG charts in ./plots and an updated daily-summary.csv

suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(xml2)
  library(base64enc)
})

dir.create("plots", showWarnings = FALSE)

## ---- TCX reader (drop-in for trackeR::readTCX, columns used downstream) ----
read_tcx <- function(f) {
  x  <- read_xml(f)
  ns <- xml_ns(x)
  tps <- xml_find_all(x, ".//d1:Trackpoint", ns)
  if (length(tps) == 0) return(NULL)
  # xml_find_first is vectorized over a nodeset and returns NA-nodes for misses,
  # which keeps every field aligned to its trackpoint.
  txt <- function(path) xml_text(xml_find_first(tps, path, ns))
  tibble(
    time            = ymd_hms(txt(".//d1:Time")),
    heart_rate      = as.numeric(txt(".//d1:HeartRateBpm/d1:Value")),
    cadence_cycling = as.numeric(txt(".//d1:Cadence")),
    # Watts live in the Garmin ActivityExtension namespace; match by local name.
    power           = as.numeric(xml_text(xml_find_first(tps, ".//*[local-name()='Watts']")))
  )
}

## ---- Read every ride ----
files <- list.files(pattern = "\\.tcx$")
message("Reading ", length(files), " TCX files ...")
AllData <- files %>%
  map(function(f) { d <- tryCatch(read_tcx(f), error = function(e) NULL); d }) %>%
  compact() %>%
  bind_rows()

AllData$date <- as.Date(AllData$time)

## ---- Per-second data, one row per trackpoint ----
selectData <- AllData %>%
  select(time, date, heart_rate, cadence_cycling, power) %>%
  filter(!is.na(time))
selectData$efficiency <- selectData$power / selectData$heart_rate

selectData <- selectData %>%
  group_by(date) %>%
  mutate(
    elapsed_time    = difftime(time, min(time), units = "mins"),
    elapsed_seconds = difftime(time, min(time), units = "secs"),
    row_id          = row_number()
  ) %>%
  ungroup() %>%
  mutate(minutes = as.numeric(trunc(elapsed_time)))

## ---- "Median ride": typical value at each elapsed second across all rides ----
medianRide <- selectData %>%
  group_by(row_id) %>%
  summarize(
    median_power = median(power, na.rm = TRUE),
    median_HR    = median(heart_rate, na.rm = TRUE),
    n = n(), .groups = "drop"
  )
medianRide$efficiency <- medianRide$median_power / medianRide$median_HR

## ---- Most recent ride ----
last_wko_date <- max(selectData$date)
lastRide      <- selectData %>% filter(date == last_wko_date)
lastRideRows  <- max(lastRide$row_id)
lastRideLimits <- c(0, lastRideRows)

## ---- Reference medians ----
medPowerAll <- round(median(selectData$power, na.rm = TRUE))
medHRAll    <- round(median(selectData$heart_rate, na.rm = TRUE))
medEffAll   <- round(median(selectData$efficiency, na.rm = TRUE), 2)

medPowerLast <- round(median(lastRide$power, na.rm = TRUE))
medHRLast    <- round(median(lastRide$heart_rate, na.rm = TRUE))
medEffLast   <- round(median(lastRide$efficiency, na.rm = TRUE), 2)

subtitle <- paste(format(last_wko_date, "%A %b %d, %Y"), "vs. Median Ride (all history)")

## ---- Time-series: Power ----
p_power <- ggplot(lastRide, aes(row_id, power, color = "This Ride")) +
  geom_point(alpha = 0.2, shape = 3) +
  geom_smooth(data = medianRide, aes(y = median_power, color = "Median Ride"), se = FALSE) +
  geom_hline(yintercept = medPowerLast, color = "#F8766D") +
  geom_hline(yintercept = medPowerAll, linetype = "dashed", color = "#00BFC4") +
  xlim(lastRideLimits) +
  labs(title = "Power over the ride", subtitle = subtitle,
       x = "Ride progress (sample #)", y = "Power (Watts)", color = "") +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
ggsave("plots/01-power-timeseries.png", p_power, width = 9, height = 5, dpi = 120)

## ---- Time-series: Heart Rate ----
p_hr <- ggplot(lastRide, aes(row_id, heart_rate, color = "This Ride")) +
  geom_point(alpha = 0.15, shape = 4) +
  geom_smooth(data = medianRide, aes(y = median_HR, color = "Median Ride"), se = FALSE) +
  geom_hline(yintercept = medHRLast, color = "#F8766D") +
  geom_hline(yintercept = medHRAll, linetype = "dashed", color = "#00BFC4") +
  xlim(lastRideLimits) +
  labs(title = "Heart rate over the ride", subtitle = subtitle,
       x = "Ride progress (sample #)", y = "Heart Rate (bpm)", color = "") +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
ggsave("plots/02-hr-timeseries.png", p_hr, width = 9, height = 5, dpi = 120)

## ---- Time-series: Efficiency ----
p_eff <- ggplot(lastRide, aes(row_id, efficiency, color = "This Ride")) +
  geom_point(alpha = 0.3, shape = 1) +
  geom_smooth(data = medianRide, aes(y = efficiency, color = "Median Ride"), se = FALSE) +
  geom_hline(yintercept = medEffLast, color = "#F8766D") +
  geom_hline(yintercept = medEffAll, linetype = "dashed", color = "#00BFC4") +
  xlim(lastRideLimits) + ylim(0.4, 1.6) +
  labs(title = "Efficiency (watt-minutes per heartbeat) over the ride", subtitle = subtitle,
       x = "Ride progress (sample #)", y = "Watt-min / heartbeat", color = "") +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
ggsave("plots/03-efficiency-timeseries.png", p_eff, width = 9, height = 5, dpi = 120)

## ---- Daily / long-term summary ----
find_duration <- function(times) interval(min(times), max(times)) / dminutes(1)

dailyDuration <- selectData %>% group_by(date) %>%
  summarize(duration = find_duration(time), .groups = "drop")

dailyPowerHR <- selectData %>% group_by(date) %>%
  summarize(median_power = median(power, na.rm = TRUE),
            median_HR    = median(heart_rate, na.rm = TRUE), .groups = "drop")

dailySummary <- dailyDuration %>%
  left_join(dailyPowerHR, by = "date") %>%
  mutate(
    pHR         = median_power / median_HR,
    wattMinutes = median_power * duration,
    wattHours   = median_power * duration / 60,
    SessionNum  = row_number()
  )

lastRideSummary <- dailySummary %>% filter(date == last_wko_date)
trendSub <- paste("All rides,", format(min(dailySummary$date), "%b %Y"),
                  "-", format(max(dailySummary$date), "%b %Y"))

## helper to build a trend chart with the latest ride highlighted
trend_plot <- function(yvar, title, ylab, ymed = NULL) {
  g <- ggplot(dailySummary, aes(x = date, y = .data[[yvar]])) +
    geom_point(color = "grey55", alpha = 0.8) +
    geom_point(data = lastRideSummary, aes(x = date, y = .data[[yvar]]),
               color = "red", size = 3) +
    geom_label(data = lastRideSummary,
               aes(x = date, y = .data[[yvar]], label = round(.data[[yvar]], 2)),
               color = "red", nudge_y = 0, hjust = 1.1, size = 3.5) +
    labs(title = title, subtitle = trendSub, x = "", y = ylab) +
    theme(plot.title = element_text(face = "bold"))
  if (!is.null(ymed)) g <- g + geom_hline(yintercept = ymed, linetype = "dotted")
  g
}

ggsave("plots/04-trend-duration.png",
       trend_plot("duration", "Workout Duration", "Minutes", median(dailySummary$duration)),
       width = 9, height = 5, dpi = 120)
ggsave("plots/05-trend-median-power.png",
       trend_plot("median_power", "Median Power per Workout", "Watts", medPowerAll),
       width = 9, height = 5, dpi = 120)
ggsave("plots/06-trend-median-hr.png",
       trend_plot("median_HR", "Median Heart Rate per Workout", "bpm", medHRAll),
       width = 9, height = 5, dpi = 120)
ggsave("plots/07-trend-efficiency.png",
       trend_plot("pHR", "Efficiency: Watt-Minutes per Heartbeat", "Watt-min / heartbeat", medEffAll),
       width = 9, height = 5, dpi = 120)
ggsave("plots/08-trend-energy.png",
       trend_plot("wattMinutes", "Total Energy per Workout", "Watt-Minutes",
                  median(dailySummary$wattMinutes)),
       width = 9, height = 5, dpi = 120)

write.csv(dailySummary, "daily-summary.csv", row.names = FALSE)

## ---- Stats used by both the console report and the HTML dashboard ----
rank_of <- function(v, val) sum(v >= val, na.rm = TRUE)  # 1 = best/highest
n <- nrow(dailySummary)
days_off <- as.integer(last_wko_date - sort(dailySummary$date, decreasing = TRUE)[2])

## ---- Console report ----
cat("\n==================  RIDE REPORT  ==================\n")
cat(sprintf("Date:            %s\n", format(last_wko_date, "%A %b %d, %Y")))
cat(sprintf("Duration:        %.1f min\n", lastRideSummary$duration))
cat(sprintf("Median power:    %d W   (all-time median %d)   rank %d of %d\n",
            medPowerLast, medPowerAll, rank_of(dailySummary$median_power, lastRideSummary$median_power), n))
cat(sprintf("Median HR:       %d bpm (all-time median %d)\n", medHRLast, medHRAll))
cat(sprintf("Efficiency:      %.2f W-min/beat (all-time median %.2f)   rank %d of %d\n",
            medEffLast, medEffAll, rank_of(dailySummary$pHR, lastRideSummary$pHR), n))
cat(sprintf("Energy:          %.0f watt-min   rank %d of %d\n",
            lastRideSummary$wattMinutes, rank_of(dailySummary$wattMinutes, lastRideSummary$wattMinutes), n))
cat(sprintf("Days since prior ride: %d\n", days_off))

## ---- Single self-contained HTML dashboard (auto-opens in browser) ----
# Each PNG is embedded as a base64 data URI so report.html is one portable file.
img <- function(path) sprintf('<img src="%s">', dataURI(file = path, mime = "image/png"))

stat_card <- function(label, value, sub)
  sprintf('<div class="card"><div class="label">%s</div><div class="value">%s</div><div class="sub">%s</div></div>',
          label, value, sub)

cards <- paste(
  stat_card("Duration", sprintf("%.0f min", lastRideSummary$duration), "this session"),
  stat_card("Median Power", sprintf("%d W", medPowerLast),
            sprintf("median %d &middot; rank %d/%d", medPowerAll,
                    rank_of(dailySummary$median_power, lastRideSummary$median_power), n)),
  stat_card("Median HR", sprintf("%d bpm", medHRLast), sprintf("all-time median %d", medHRAll)),
  stat_card("Efficiency", sprintf("%.2f W&middot;min/beat", medEffLast),
            sprintf("median %.2f &middot; rank %d/%d", medEffAll,
                    rank_of(dailySummary$pHR, lastRideSummary$pHR), n)),
  stat_card("Energy", sprintf("%.0f W&middot;min", lastRideSummary$wattMinutes),
            sprintf("rank %d/%d", rank_of(dailySummary$wattMinutes, lastRideSummary$wattMinutes), n)),
  stat_card("Days Off", sprintf("%d", days_off), "since prior ride"),
  collapse = "\n")

section <- function(title, files)
  paste0("<h2>", title, "</h2>\n",
         paste(sprintf('<figure>%s</figure>', vapply(files, img, "")), collapse = "\n"))

## Recent-rides table (last 10 sessions, newest first, this ride highlighted)
recent <- dailySummary %>% arrange(desc(date)) %>% head(10)
recent_rows <- vapply(seq_len(nrow(recent)), function(i) {
  r <- recent[i, ]
  sprintf('<tr class="%s"><td>%s</td><td>%.0f</td><td>%d</td><td>%d</td><td>%.2f</td><td>%s</td></tr>',
          if (r$date == last_wko_date) "hl" else "",
          format(r$date, "%a %b %d, %Y"), r$duration, round(r$median_power),
          round(r$median_HR), r$pHR, format(round(r$wattMinutes), big.mark = ","))
}, character(1))
recent_tbl <- paste0(
  '<h2>Recent Rides</h2>\n<table><thead><tr>',
  '<th>Date</th><th>Min</th><th>Power (W)</th><th>HR</th><th>W&middot;min/beat</th><th>Energy (W&middot;min)</th>',
  '</tr></thead><tbody>\n', paste(recent_rows, collapse = "\n"), '\n</tbody></table>')

html <- sprintf('<!doctype html><html><head><meta charset="utf-8">
<title>Ride Report %s</title><style>
:root{color-scheme:light dark}
body{font-family:-apple-system,Segoe UI,Roboto,sans-serif;max-width:1000px;margin:0 auto;padding:24px;
     background:#fafafa;color:#1a1a1a}
h1{margin:0 0 2px} .when{color:#777;margin:0 0 20px}
.cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:12px;margin-bottom:28px}
.card{background:#fff;border:1px solid #e6e6e6;border-radius:10px;padding:14px 16px}
.card .label{font-size:12px;text-transform:uppercase;letter-spacing:.04em;color:#888}
.card .value{font-size:24px;font-weight:700;margin:4px 0}
.card .sub{font-size:12px;color:#999}
h2{margin:28px 0 10px;font-size:18px;border-bottom:2px solid #eee;padding-bottom:6px}
figure{margin:0 0 18px} img{width:100%%;border:1px solid #eee;border-radius:8px}
table{width:100%%;border-collapse:collapse;font-size:14px;background:#fff;border:1px solid #e6e6e6;border-radius:10px;overflow:hidden}
th,td{padding:8px 12px;text-align:right;border-bottom:1px solid #eee}
th:first-child,td:first-child{text-align:left}
thead th{background:#f3f3f3;color:#555;font-size:12px;text-transform:uppercase;letter-spacing:.03em}
tr.hl td{background:#ffecec;font-weight:700}
footer{color:#aaa;font-size:12px;margin-top:30px}
@media (prefers-color-scheme:dark){body{background:#161616;color:#eee}
 .card{background:#222;border-color:#333}.card .value{color:#fff} h2{border-color:#333} img{border-color:#333}
 table{background:#222;border-color:#333} th,td{border-color:#333} thead th{background:#2a2a2a;color:#aaa}
 tr.hl td{background:#3a2323}}
</style></head><body>
<h1>Cycling Ride Report</h1>
<p class="when">%s &middot; %d sessions on record</p>
<div class="cards">%s</div>
%s
%s
%s
<footer>Generated by analyze-ride.R</footer>
</body></html>',
  format(last_wko_date, "%Y-%m-%d"),
  format(last_wko_date, "%A %b %d, %Y"), n, cards,
  section("This Ride vs. Your Median Ride",
          c("plots/01-power-timeseries.png","plots/02-hr-timeseries.png","plots/03-efficiency-timeseries.png")),
  recent_tbl,
  section("Long-Term Trends (this ride in red)",
          c("plots/05-trend-median-power.png","plots/06-trend-median-hr.png","plots/07-trend-efficiency.png",
            "plots/04-trend-duration.png","plots/08-trend-energy.png")))

writeLines(html, "report.html")
cat("Dashboard written to report.html (opening in browser). daily-summary.csv updated.\n")
cat("===================================================\n")
try(browseURL(normalizePath("report.html")), silent = TRUE)
