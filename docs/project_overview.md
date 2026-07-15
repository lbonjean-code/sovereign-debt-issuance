---
name: project-sovereign-tracker
description: "R Shiny dashboard for sovereign bond issuance tracking — Chile, Mexico, South Africa, Colombia — project overview, deployment, fiscal year logic, and UI structure"
metadata: 
  node_type: memory
  type: project
  originSessionId: d8beb0c1-bf32-4d5d-b35e-e793d0cdbd57
---

## Project overview

Dashboard lives at `C:\Users\lbonjean\Documents\sovereign_bond_tracker\app.R`.
Deploy with `rsconnect::deployApp("C:/Users/lbonjean/Documents/sovereign_bond_tracker")` (account: lbonjeanjgp, app: sovereign_bond_tracker).

**IMPORTANT:** Do NOT deploy automatically. User will explicitly say when to deploy.

**Why:** Tracks sovereign bond issuance, treasury cash, debt composition, and run-rates across Chile, Mexico, South Africa, and Colombia for fixed income analysis.

## Data paths

```r
DATA_DIR <- if (dir.exists("\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data"))
  "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data"
else "data"
```
Runs from the JGP file server locally; from `data/` subfolder on shinyapps.io. CSV files must be copied to `data/` before each redeploy.

**Data files (all CSVs):**

*Core (issuance, treasury, GDP):*
`chile_licitaciones.csv`, `chile_treasury.csv`, `chile_gdp.csv`,
`mexico_licitaciones.csv`, `mexico_treasury.csv`, `mexico_gdp.csv`,
`south_africa_licitaciones.csv`, `south_africa_treasury.csv`, `south_africa_gdp.csv`,
`colombia_licitaciones.csv`, `colombia_treasury.csv`, `colombia_gdp.csv`

*Debt:*
`chile_debt.csv`, `chile_gdp_usd.csv`,
`mexico_debt.csv`, `south_africa_debt.csv`, `colombia_debt.csv`

*Holdings (investor breakdown):*
`south_africa_holdings.csv`, `mexico_holdings.csv`, `colombia_holdings.csv`

*Maturity profiles:*
`mexico_maturity.csv`, `colombia_maturity.csv`, `south_africa_maturity.csv`

*Weighted average maturity:*
`mexico_avg_maturity.csv`, `south_africa_avg_maturity.csv`, `colombia_avg_maturity.csv`
(Chile avg maturity is a static hardcoded KPI card — no CSV)

## Fiscal year logic

| Country | FY definition | FY helper | Fiscal month 1 |
|---|---|---|---|
| Chile | Jan–Dec | `fy_chile(d) = year(d)` | January |
| Mexico | Jan–Dec | `fy_mexico(d) = year(d)` | January |
| Colombia | Jan–Dec | `year(d)` inline | January |
| South Africa | Apr–Mar | `fy_sa(d) = if_else(month(d)>=4, year(d), year(d)-1L)` | April |

SA FY label: `fmt_fy(fy, "south_africa")` → `paste0(fy, "/", substr(fy+1, 3, 4))` e.g. `"2026/27"`.
Other countries: `fmt_fy(fy, country)` → `as.character(fy)` e.g. `"2026"`.

`fiscal_month(d, country)`:
- SA: `((month(d) - 4L) %% 12L) + 1L` (Apr=1, Mar=12)
- Others: `month(d)`

`current_fy(country)`: returns `fy_sa(Sys.Date())` for SA, `year(Sys.Date())` for others.

## UI tab order and card order

**Tabs:** Chile → México → África do Sul → Colômbia → Visão Geral (last)

**Each country tab cards (in order):**
1. Emissões Mensais
2. Emissões Mensais % PIB
3. Caixa do Tesouro (Sazonal) + Caixa do Tesouro [side by side]
4. Emissões YTD
5. Emissões YTD em % do PIB
6. Run Rate — Emissão Acumulada
7. Run Rate — Emissão Acumulada % PIB
8. Emissões vs. Meta
9. Composição — Instrumento + Composição — Moeda [side by side]
10. Pré vs. Pós
11. Deuda % PIB + Composição Interna/Externa [side by side]
12. Country-specific tail cards (see below)

**Country-specific tail cards (after debt pair):**
- **Chile:** Two KPI cards side-by-side (col_widths = c(6,6)): "Amortizações de Bônus em 2026" (US$ 7.211 MM) + "Prazo Médio da Dívida" (10.4 anos) — both static, annual update
- **Mexico:** Holdings → Maturity Profile → Avg Maturity bar chart (SG231, Dec obs from 2010)
- **South Africa:** Holdings → Maturity Profile → Avg Maturity bar chart (Budget Review, 2013/14–2025/26)
- **Colombia:** Holdings + Maturity side-by-side (col_widths = c(5,7)) → Avg Maturity KPI card (Vida Media from IRC PDF)

**Visão Geral tab:** single card "Pré vs. Pós — Composição YTD por País" (`overview_pre_pos`)

## Known gaps / pending

None outstanding as of 2026-07-14.

**How to apply:** When adding new charts or countries, follow the existing load → chart → UI → server pattern. Copy new CSVs to both file server path and local `data/` before deploying.
