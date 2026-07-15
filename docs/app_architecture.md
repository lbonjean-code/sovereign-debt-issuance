---
name: project-tracker-architecture
description: "Chart functions, GDP LOCF patterns, run-rate logic, ggplotly quirks, color constants, and helper functions for the sovereign bond tracker"
metadata: 
  node_type: memory
  type: project
  originSessionId: d8beb0c1-bf32-4d5d-b35e-e793d0cdbd57
---

## Color constants

```r
CLR_CP      <- "#90C987"  # short-term (CP) bars
CLR_LP      <- "#AAAAAA"  # long-term (LP) bars
CLR_TOTAL   <- "#E84855"  # total line / dots (red)
CLR_CURRENT <- "#E84855"  # current year run-rate line (red)
CLR_HIST    <- "#BBBBBB"  # historical run-rate lines
CLR_MED     <- "#555555"  # median line
CLR_PRE     <- "#4472C4"  # Pré (fixed rate) — blue
CLR_POS     <- "#FFC000"  # Pós (floating rate) — amber
# Debt charts (inline): Internal="#4472C4"  External="#ED7D31"
```

## Donut colors (DONUT_COLORS)

Shared semantic palette across all countries. Key mappings:
- CP/Cetes/Treasury Bill → `#90C987` (green = short-term)
- LP/BTP/Bono M/Fixed/Nominal/ZAR → `#AAAAAA` (grey = long-term / nominal)
- BTU/Udibono/Inflation-Linked → `#5B9BD5` (blue = inflation-linked)
- Bondes/FRN → `#FFC000` (amber = floating)
- COP → `#90C987`, UVR → `#5B9BD5`

## Chart functions reference

| Function | Purpose |
|---|---|
| `chart_ytd(lic, country, ccy_label)` | YTD issuance by FY, stacked CP+LP with total dot; same calendar window for all years |
| `chart_monthly(lic, country, ccy_label)` | Monthly issuance last 24 months, stacked with total line |
| `chart_monthly_pct_gdp(lic, gdp, country, ccy_label)` | Monthly issuance as % GDP, LOCF GDP |
| `chart_ytd_pct_gdp(lic, gdp, country, ccy_label)` | YTD issuance % GDP by FY, per-year LOCF GDP |
| `chart_pct_gdp(lic, gdp, country, ccy_label)` | Annual issuance % GDP |
| `build_runrate(lic, country, n_prev)` | Builds cumulative run-rate data frame |
| `plot_runrate(df, country, target, ccy_label)` | Renders run-rate as plotly line chart |
| `chart_runrate(lic, country, target, ccy_label)` | Wrapper: build + plot in absolute units |
| `chart_runrate_pct_gdp(lic, gdp, country, ccy_label)` | Run-rate divided by GDP, with LOCF |
| `chart_tsy_seasonal(tsy, country, ccy_label)` | Treasury cash: seasonal fan chart |
| `chart_tsy_ts(tsy, country, ccy_label)` | Treasury cash: time-series line |
| `chart_vs_target(lic, country, target, ccy_label)` | YTD issuance vs annual target bar |
| `chart_composition(lic, country, ccy_label, dimension)` | Dual donut: dimension="instrument" or "currency" |
| `chart_pre_pos(lic, country)` | 100% stacked Pré/Pós bar by FY |
| `chart_pre_pos_overview()` | 4-country side-by-side Pré/Pós YTD (plot_ly directly) |
| `chart_debt_pct_gdp(debt, gdp, country)` | Stacked Internal+External debt % GDP + total dot |
| `chart_debt_composition(debt, country)` | 100% stacked Internal/External debt share |
| `chart_holdings(holdings, country)` | Stacked % bar by investor type — SA (monthly Dec obs) and Mexico (weekly→monthly, Dec obs); uses `plot_ly` directly |
| `chart_colombia_holdings(holdings)` | Single horizontal bar by sector for Colombia (one snapshot date) |
| `chart_colombia_maturity(maturity)` | COP/UVR stacked bars by maturity year; deduplicates via slice_max |
| `chart_mexico_maturity(maturity)` | Multi-instrument stacked bars by maturity year; includes Total label row |
| `chart_sa_maturity(maturity)` | Single-color bars by SA fiscal year (2026/27→2057/58); ZAR trillones |
| `chart_mexico_avg_maturity(avg_maturity)` | Bar chart of weighted avg maturity (anos); Dec obs 2010+, current year latest month |
| `chart_sa_avg_maturity(avg_maturity)` | Bar chart of WAM by fiscal year (2013/14–2025/26); fixed-rate bonds only |
| `clean_legend(plt, hide_line_total)` | Strips ggplotly legend artifacts |
| `plotly_placeholder(msg)` | Empty plotly card with "Em breve" message |

## Critical ggplotly stacking reversal

**ggplotly reverses factor level order when stacking.** To achieve the visual order `[bottom → top]`, set factor levels as `[top → bottom]`.

Example: to display Pré at the bottom and Pós at the top:
```r
mutate(PrePos = factor(PrePos, levels = c("Pós", "Pré")))
# ggplot draws Pós at bottom, ggplotly reverses → Pré ends up at bottom ✓
```

Applied consistently in: `chart_vs_target`, `chart_pre_pos`, `chart_debt_pct_gdp` (Externa/Interna), `chart_debt_composition`.

**For debt charts:** to get Interna at bottom and Externa on top → set `levels = c("Externa", "Interna")`.

Also: use display names directly as factor levels (not `labels=` in scale_fill_manual), because ggplotly ignores scale labels and reads raw factor level names for the legend.

## build_runrate — cumulative run-rate with data-driven cutoff

```r
build_runrate <- function(lic, country, n_prev = 3) {
  cur_fis_month   <- fiscal_month(Sys.Date(), country)
  cur_months      <- lic |> filter(FY == cur_fy) |> pull(Mes_Fiscal)
  last_data_month <- if (length(cur_months) == 0) 0L else max(cur_months, na.rm = TRUE)
  cutoff_month    <- min(cur_fis_month, last_data_month)
  # complete() fills month gaps with 0; cumsum; current year filtered to cutoff_month
}
```

`cutoff_month = min(fiscal_month_today, last_month_with_data)` — prevents the current-year line from extending past months where auctions haven't been released yet (e.g. SA publishes July data with a lag, so line stops at June until July is released).

## plot_runrate — fiscal year labels and colors

```r
fy_fmts <- sapply(fy_levels, fmt_fy, country = country)
clrs    <- setNames(c("#BBBBBB","#78909C","#5B9BD5", CLR_CURRENT)[seq_along(fy_levels)], fy_fmts)
sizes   <- setNames(c(0.6, 0.7, 0.8, 1.4)[seq_along(fy_levels)], fy_fmts)
df <- df |> mutate(
  FY_label  = factor(sapply(FY, fmt_fy, country = country), levels = fy_fmts),
  Month_lbl = lbl[Mes_Fiscal]
)
```

Tooltips and target label use `fmt_fy()` so SA shows "2026/27 — Apr" not "2026 — Apr".
Current year x-axis label appended with `" (YTD)"`.
Target line label: `paste0("Meta ", fmt_fy(max(df$FY), country))`.

## GDP LOCF — two variants

**Variant 1 — per-month LOCF** (used in `chart_monthly_pct_gdp`, `chart_runrate_pct_gdp`):
For each calendar month date `d`, use the most recent GDP observation where `gdp$Periodo <= d`. The GDP denominator steps up only when a new quarterly release arrives.

**Variant 2 — per-FY LOCF for YTD charts** (`chart_ytd_pct_gdp`):
For each FY, find most recent GDP at the equivalent calendar position *within that year*:
```r
gdp_by_fy <- do.call(rbind, lapply(unique(ytd$FY), function(fy) {
  fy_today   <- as.Date(paste0(fy, fy_start_str)) + today_doy
  candidates <- gdp_pts$PIB_tri[gdp_pts$Periodo <= fy_today]
  if (length(candidates) == 0) return(NULL)
  data.frame(FY = fy, PIB_tri = tail(candidates, 1))
}))
```
This ensures historical years use the GDP that was available at that same point in their year (not the latest published GDP), enabling fair year-over-year comparison.

**Variant 3 — per-FY LOCF for run-rate % PIB** (`chart_runrate_pct_gdp`):
Same idea but using fiscal year end:
```r
fy_end     <- as.Date(paste0(fy + 1, fy_start_str)) - 1
candidates <- gdp_pts$PIB_tri[gdp_pts$Periodo <= fy_end]
```
Uses the last available GDP reading before or at fiscal year-end.

## GDP matching for debt/GDP chart (chart_debt_pct_gdp)

- **South Africa:** direct join on `FY_start == gdp$FY` (both are fiscal year start year, both annual).
- **Mexico / Colombia:** LOCF — for December of year Y: `gdp$Periodo <= as.Date(paste0(Y, "-12-31"))`. For Mexico this picks the October quarterly reading (most recent available by Dec). For Colombia, same pattern.

## clean_legend helper

Strips ggplotly artifacts like "(CP,1)" legend entries and removes group headers:
```r
clean_legend <- function(plt, hide_line_total = FALSE) {
  # gsub("\\((.+),\\d+\\)", "\\1", nm) → strips trailing ",1" etc.
  # Sets legendgroup = "" to remove group header rows
  # hide_line_total=TRUE: hides the line trace for "Total" (keeps dot marker)
  # Deduplicates repeated legend entries
}
```

## YTD window definition

For **all** countries, "YTD" means: from fiscal year start to the same calendar day-of-year as today. Implemented as:
```r
today_doy       <- as.numeric(today - today_fy_start)  # days elapsed in current FY
auction_doy     <- as.numeric(Fecha - as.Date(paste0(FY, fy_start_str)))
filter(auction_doy >= 0, auction_doy <= today_doy)
```
This gives a consistent "same window every year" comparison — not "Jan 1 to Dec 31" for past years.
