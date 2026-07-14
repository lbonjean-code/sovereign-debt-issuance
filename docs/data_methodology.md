---
name: project-data-methodology
description: "Per-country unit conversions (issuance, treasury, GDP, debt, holdings, maturity) and all classification rules (CP/LP, instrument, currency, Pré/Pós) for the sovereign bond tracker"
metadata: 
  node_type: memory
  type: project
  originSessionId: d8beb0c1-bf32-4d5d-b35e-e793d0cdbd57
---

## Unit conversions — Issuance (lic)

| Country | Source column | Raw unit | Transform | Stored unit |
|---|---|---|---|---|
| Chile | `Monto` | millions CLP | `/1e6` | trillones CLP |
| Mexico | `Monto` | millions MXN | `/1e6` | trillones MXN |
| South Africa | `Monto_Asignado` | raw ZAR | `/1e12` | trillones ZAR |
| Colombia | `Monto` | millions COP | `/1e6` | trillones COP |

SA is unusual: raw ZAR → trillones ZAR uses `/1e12`, not `/1e6` like the others.

## Unit conversions — Treasury cash (tsy)

| Country | Source column | Raw unit | Transform | Stored (Saldo) | Display label |
|---|---|---|---|---|---|
| Chile | `Total` | USD billions | kept as-is | USD billions | "USD bi" |
| Mexico | `Saldo` | millions MXN | `/1e3` | billions MXN | "MXN bi" |
| South Africa | `Saldo` | raw ZAR | `/1e9` | trillones ZAR | "ZAR tri" |
| Colombia | `Saldo` | millions COP | `/1e6` | trillones COP | "COP tri" |

SA treasury `Fecha` is constructed: `as.Date(paste0(Periodo, "-01"))` (Periodo = "YYYY-MM").

## Unit conversions — GDP (gdp)

| Country | CSV | Raw unit | Transform | Stored (PIB_tri) | Note |
|---|---|---|---|---|---|
| Chile | chile_gdp.csv | billions CLP quarterly | `rollsum(PIB,k=4,fill=NA,align="right")/1e3` | CLP trillones 12m rolling | Needs rollsum |
| Mexico | mexico_gdp.csv | millions MXN | `/1e6` | MXN trillones | **No rollsum** — already annualised |
| South Africa | south_africa_gdp.csv | R millions annual | `/1e6` | ZAR trillones | Annual, `Periodo=as.Date(paste0(FY,"-01-01"))` |
| Colombia | colombia_gdp.csv | miles de millones COP quarterly | `rollsum(PIB,k=4,fill=NA,align="right")/1e3` | COP trillones 12m rolling | Needs rollsum |

**Critical:** Mexico GDP values (~34–36T MXN) are already full-year rolling figures reported on quarterly dates — do NOT apply rollsum. Chile and Colombia are true quarterly and must be summed.

SA GDP: `FY = as.integer(Anio)` (the calendar year the April starts in).

## Unit conversions — Debt (debt)

| Country | CSV | Raw columns | Transform | Debt type | Year-end filter |
|---|---|---|---|---|---|
| Chile | chile_debt.csv | `Total`, `Interna`, `Externa` (millions USD) | `/1e6` → USD tri | **Gross** (Deuda Bruta USD) | `month(Periodo)==10` (Q4 = October) |
| Mexico | mexico_debt.csv | `Total`, `Interna`, `Externa` (miles de millones MXN) | `/1e3` | **Net** (Deuda Neta — Banxico) | `month(Periodo)==12` |
| South Africa | south_africa_debt.csv | `Gross`, `Domestic`, `Foreign` (R millions) | `/1e6` | **Gross** (Deuda Bruta) | Annual via `Ano_Fiscal` |
| Colombia | colombia_debt.csv | `Deuda_Total`, `Deuda_Interna`, `Deuda_Externa` (millions COP) | `/1e6` | **Gross** (Deuda Bruta) | `month(Periodo)==12` |

Chile is special: uses **USD** (not CLP). Requires a separate GDP in USD (`chile_gdp_usd.csv`, column `PIB` in USD, annual) → `/1e12` → USD trillones. Year-end observation is October (Q4), not December.

SA debt: `Ano_Fiscal` is "2024/25" string → `FY_start = as.integer(substr(Ano_Fiscal,1,4))`, `X_label = paste0(FY_start,"/",substr(FY_start+1,3,4))`.
All countries: filter `Year >= 2005` (or `FY_start >= 2005`).

**X-axis month suffix** in debt % PIB chart:
- South Africa: `" (Mar)"` (fiscal year-end)
- Chile: `" (Oct)"` (Q4 observation)
- Mexico / Colombia: `" (Dec)"`

**Debt label in legend:**
- Mexico: "Deuda Neta"
- Chile: "Deuda Bruta (USD)"
- SA / Colombia: "Deuda Bruta"

## Unit conversions — Holdings (investor breakdown)

### South Africa (`south_africa_holdings.csv`)
- Columns: `Periodo` (date), `Non_Residents`, `Local_Pension_Funds`, `Banks`, `Insurers`, `Other_Financial`, `Other` (all as decimals 0–1, i.e. share of total)
- Multiply by 100 to get % for display
- Frequency: monthly; use **December** for completed years, latest observation for current year
- Date range shown: 2020 onwards
- X-axis labels: `paste0(year, " (Dec)")` for completed years; `paste0(year, " (", month_name, ")")` for current year

### Mexico (`mexico_holdings.csv`)
- Columns: `Periodo` (date), `Total` (millions MXN), `Extranjeros`, `Siefores`, `Sociedades_Inversion`, `Sector_Bancario`, `Otros` (all in millions MXN)
- **Frequency: weekly** — aggregate to monthly by `floor_date(Periodo, "month")`, keep `slice_max(Periodo, n=1)` within each month
- Convert to % of Total for display
- Date range shown: 2020 onwards
- X-axis labels: same pattern as SA — `" (Dec)"` for completed years, month name for current year

### Colombia (`colombia_holdings.csv`)
- Columns: `Tenedor` (holder name text), `Total` (millions COP), `Fecha_Corte` (text "30-jun-2026" — do NOT convert with `as.Date()`)
- Single snapshot (one cut date): displayed as a **horizontal bar** (`orientation="h"` in plot_ly)
- Sector classification via `case_when(grepl(...))` on `Tenedor`:
  - "Pensiones|Cesantias" → "Fondos de Pensiones"
  - "Capital Extranjero|Extranjero" → "Extranjeros"
  - "Bancos Comerciales" → "Bancos"
  - "Seguros|Capitaliz" → "Seguros"
  - "Banco de la Rep" → "Banco de la República"
  - else → "Otros"

**CRITICAL:** `Fecha_Corte` in colombia_holdings.csv uses format "30-jun-2026" — `as.Date()` cannot parse this without a format spec. Since Fecha_Corte is not used in any chart, do NOT call `as.Date(Fecha_Corte)` in `load_colombia`. Doing so causes the entire Colombia data to return NULL from tryCatch.

## Unit conversions — Maturity profiles

### Mexico (`mexico_maturity.csv`)
- Columns: `Instrumento` (text), `Ano` (integer year), `Monto` (millions MXN)
- Includes a row where `Instrumento == "Total"` for each year (used for total labels)
- Transform: `Monto / 1e6` → MXN trillones
- Instruments shown (visual_order from bottom to top): Bonos Tasa Fija Bono M, Udibonos, Cetes, Bondes F, Bondes G, Bondes D
- ggplotly stacking: factor levels set as `rev(visual_order)` so bottom-to-top order is correct after ggplotly reversal

### Colombia (`colombia_maturity.csv`)
- Columns: `Ano_Vencimiento` (integer year), `TES_COP` (millions COP), `TES_UVR` (millions COP), `Total` (millions COP), `Fecha_Corte`
- May have **duplicate rows per year** (different cut dates) — deduplicate with `group_by(Ano_Vencimiento) |> slice_max(Total, n=1) |> ungroup()`
- Transform: `/1e6` → COP trillones
- Two segments: COP_tri (`#4472C4`) and UVR_tri (`#FFC000`)
- ggplotly stacking: `levels = c("UVR_tri", "COP_tri")` so COP at bottom, UVR on top after reversal

### South Africa (`south_africa_maturity.csv`)
- Columns: `Ano_Fiscal` (e.g. "2026/27"), `Start_Year` (integer, e.g. 2026), `Nominal_ZAR` (raw ZAR), `Fecha_Corte` (text "30 April 2026")
- 28 rows, one per fiscal year, from 2026/27 to 2057/58 (note: 2053/54 to 2056/57 are absent — jump from 2052/53 to 2057/58)
- Transform: `Nominal_ZAR / 1e12` → ZAR trillones
- **Single color** (no instrument breakdown) — `#4472C4` (blue)
- X-axis: `Ano_Fiscal` ordered by `Start_Year`, 45° rotation
- Labels on top: `round(ZAR_tri, 1)` (1 decimal)
- Subtitle: "Inclui títulos de taxa fixa, indexados à inflação e FRNs"

## CP/LP classification (Tipo column)

| Country | Rule |
|---|---|
| Chile | `Tenor == "CP"` → "CP", else "LP" |
| Mexico | `Plazo <= 365` → "CP", else "LP" |
| South Africa | `Tipo_Bono == "Treasury Bill"` → "CP", else "LP" |
| Colombia | `Tipo = Tenor` — column already contains "CP" or "LP" directly |

## Instrument classification (chart_composition dimension="instrument")

| Country | Groups | Source column / rule |
|---|---|---|
| Chile | CP / LP | `Tipo` |
| Mexico | Cetes / Bono M / Udibono / Bondes | `case_when` on `Instrumento` using grepl |
| South Africa | Fixed / Inflation-Linked / FRN / Treasury Bill | `Tipo_Bono` |
| Colombia | CP / LP | `Tipo` |

## Currency / index classification (chart_composition dimension="currency")

| Country | Groups | Rule |
|---|---|---|
| Chile | BTP (CLP nominal) / BTU (UF-linked) | `Moneda == "UF"` → "BTU", else "BTP" |
| Mexico | Nominal / Inflation-linked | `grepl("Udibono", Instrumento)` → "Inflation-linked", else "Nominal" |
| South Africa | ZAR / Inflation-Linked | `Tipo_Bono == "Inflation-Linked"` → "Inflation-Linked", else "ZAR" |
| Colombia | COP / UVR | `Moneda` column (already "COP" or "UVR") |

## Pré vs. Pós classification (classify_pre_pos)

Tags each auction row as "Pré" (fixed rate) or "Pós" (floating rate).

```r
classify_pre_pos <- function(lic, country) {
  if (country %in% c("chile", "colombia")) {
    lic |> mutate(PrePos = "Pré")                          # 100% fixed rate
  } else if (country == "mexico") {
    lic |> mutate(PrePos = case_when(
      Instrumento == "Cetes"                              ~ "Pré",
      grepl("Bono M",  Instrumento, ignore.case=TRUE)    ~ "Pré",
      grepl("Udibono", Instrumento, ignore.case=TRUE)    ~ "Pré",
      grepl("Bondes",  Instrumento, ignore.case=TRUE)    ~ "Pós",
      TRUE                                               ~ NA_character_
    ))
  } else { # south_africa
    lic |> mutate(PrePos = case_when(
      Tipo_Bono %in% c("Fixed","Inflation-Linked","Treasury Bill") ~ "Pré",
      Tipo_Bono == "FRN"                                           ~ "Pós",
      TRUE                                                         ~ NA_character_
    ))
  }
}
```

Chile/Colombia: all auctions are fixed rate (100% Pré).
Mexico: Bondes = floating; everything else = fixed.
SA: FRN = floating; all others (Fixed, IL, T-Bills) = fixed.
Inflation-linked bonds (Udibono, IL) are classified as **Pré** because their coupon is fixed (only principal is indexed).
