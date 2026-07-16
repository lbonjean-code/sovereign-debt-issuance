# Fixed Income Dashboard — Data Sources Summary

## Chile

### Licitaciones (Emissions)
- **File**: `chile_licitaciones.csv`
- **Source**: [Hacienda.cl — Resultados Última Licitación](https://www.hacienda.cl/areas-de-trabajo/finanzas-internacionales/oficina-de-la-deuda-publica/colocaciones-bcch-soma/resultados-ultima-licitacion)
- **Method**: Web scraping via `rvest`; historical base from `Chile Licitaciones Consolidated.xlsx` (sheet: "Debt")
- **Frequency**: Updated daily (Task Scheduler 8am + 6pm)
- **Columns**: `Moneda` (CLP/UF), `Madurez`, `Fecha_Licitacion`, `Monto` (millions CLP or millions UF), `Tasa`, `Tenor` (CP/LP)
- **Units**: Millions of CLP (CLP rows) / Millions of UF (UF rows)
- **Notes**: UF rows need conversion to CLP using current UF value for aggregation; CSV is the permanent record

### Treasury Cash Balance
- **File**: `chile_treasury.csv`
- **Source**: [BCCh BDE — Finanzas Públicas / Activos consolidados del tesoro](https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_FIN_PUB/MN_FIN_PUB_1/GOB_ACT_CONS_T1/act_cons_tesoro)
- **Method**: Web scraping via `rvest`; Spanish month mapping required
- **Frequency**: Updated daily
- **Columns**: `Periodo`, `Pesos`, `Dolar`, `Total`
- **Units**: Billions of USD (pre-converted from millions in script)
- **Notes**: BCCh publishes in millions USD; script divides by 1,000

### GDP
- **File**: `chile_gdp.csv`
- **Source**: [BCCh BDE — National Accounts](https://si3.bcentral.cl/Siete/EN/Siete/Cuadro/CAP_CCNN/MN_CCNN76/CCNN2018_P0_V2/637801082315858005)
- **Method**: Web scraping via `rvest`
- **Frequency**: Updated daily (quarterly releases)
- **Columns**: `Periodo` (quarterly date), `PIB`
- **Units**: Billions of CLP (miles de millones), current prices, reference 2018
- **Notes**: Raw quarterly values — sum 4 rolling quarters for 12m accumulated GDP in dashboard

### Debt (Internal vs External)
- **Status**: Pending — BCCh has quarterly debt by instrument in USD (Gobierno Central). Chart to be added later.

---

## Mexico

### Licitaciones (Emissions)
- **File**: `mexico_licitaciones.csv`
- **Source**: [Banxico SIE API](https://www.banxico.org.mx/SieAPIRest/service/v1/) — `/datos/oportuno` endpoint
- **Method**: Banxico REST API using `BANXICO_TOKEN` environment variable
- **Frequency**: Updated daily (8am + 6pm runs)
- **Instruments & Series**: Cetes (5 tenors), Bono M (6), Udibono (5), Bondes D (4), Bondes F (5)
- **Columns**: `Fecha`, `Instrumento`, `Tenor` (CP/LP), `Plazo`, `Monto` (millions MXN), `Tasa`, `Precio_Ponderado`
- **Units**: Millions of MXN (all instruments, including Udibonos — see UDI conversion below)
- **Notes**: Bondes use `Precio_Ponderado` instead of `Tasa`; afternoon auctions caught by 6pm run

### UDI/MXN Exchange Rate
- **File**: `mexico_udi.csv`
- **Source**: Banxico — Unidad de Inversión (UDI) daily value
- **Method**: Banxico REST API; updated alongside licitaciones in the same daily run
- **Frequency**: Updated daily (8am + 6pm runs)
- **Columns**: `Fecha`, `UDI_MXN` (MXN per UDI)
- **Purpose**: Udibono auction amounts in the raw source are in millions of UDI, not MXN. At load time, each Udibono row is multiplied by the UDI/MXN rate on its auction date (forward-filled for weekends/holidays) to convert to millions of MXN, making all instruments comparable

### Treasury Cash Balance
- **File**: `mexico_treasury.csv`
- **Source**: [Banxico SIE API — Series SF1575](https://www.banxico.org.mx/SieAPIRest/service/v1/series/SF1575)
- **Series**: SF1575 — "Crédito neto al Gobierno Federal" (Fuentes y usos de la base monetaria)
- **Method**: Banxico REST API
- **Frequency**: Updated daily
- **Columns**: `Periodo`, `Saldo`
- **Units**: Millions of MXN (converted from Miles de Pesos by dividing by 1,000)
- **Notes**: Values are negative in source (government is net creditor); multiplied by -1 in script

### GDP
- **File**: `mexico_gdp.csv`
- **Source**: [Banxico SIE API — Series SR17645](https://www.banxico.org.mx/SieAPIRest/service/v1/series/SR17645)
- **Series**: SR17645 — "Producto interno bruto, a precios de mercado" (precios corrientes)
- **Method**: Banxico REST API
- **Frequency**: Updated daily (quarterly releases)
- **Columns**: `Periodo` (quarterly date), `PIB`
- **Units**: Millions of MXN, current prices
- **Notes**: Already pre-accumulated rolling 12-month GDP — use directly as denominator, no summing needed

### Debt (Internal vs External)
- **File**: `mexico_debt.csv`
- **Source**: [Banxico SIE — Finanzas Públicas / Deuda Neta del Sector Público](https://www.banxico.org.mx/SieInternet/)
- **Series**: SG193 (Total), SG194 (Interna), SG195 (Externa) — Económica Amplia, Saldos al Final del Periodo
- **Method**: Banxico REST API
- **Frequency**: Updated daily (monthly releases)
- **Columns**: `Periodo`, `Total`, `Interna`, `Externa`
- **Units**: Miles de millones de pesos (billions MXN)
- **Notes**: This is NET debt (Deuda Neta), not gross — label accordingly in dashboard. Gross debt not available via automated source.

---

## South Africa

### Licitaciones (Emissions)
- **File**: `south_africa_licitaciones.csv`
- **Source**: [National Treasury Investor Relations](https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Historical%20Results/)
- **Method**: Downloads current fiscal year Excel files for Fixed-rate bonds, Inflation-linked bonds, Treasury bills, and FRNs
- **Frequency**: Updated daily
- **Columns**: `Fecha_Subasta`, `Fecha_Vencimiento`, `Bono`, `Tipo_Bono` (Fixed/Inflation-Linked/Treasury Bill/FRN), `Tenor_Anos`, `Plazo_Dias`, `Monto_Asignado` (raw ZAR), `Tasa_Corte`
- **Units**: Raw ZAR (divide by 1,000,000,000,000 for trillions)
- **Notes**: Historical base from `Painel - South Africa.xlsx` (sheet: Aux_SA); fiscal year April–March; FRN = Pós, all others = Pré

### Treasury Cash Balance
- **File**: `south_africa_treasury.csv`
- **Source**: [National Treasury Section 32 Monthly Reports — Table 4](https://www.treasury.gov.za/comm_media/press/monthly/)
- **Method**: PDF scraping via `pdftools`; extracts "Closing balance" from Table 4
- **URL pattern**: `https://www.treasury.gov.za/comm_media/press/monthly/{YY}{MM}/Table 4.pdf`
- **Fiscal month mapping**: Nov=01, Dec=02, Jan=03, Feb=04, Mar=05, Apr=06, May=07, Jun=08, Jul=09, Aug=10, Sep=11, Oct=12
- **Frequency**: Updated daily (new PDFs released monthly)
- **Columns**: `Periodo`, `Saldo`
- **Units**: Thousands of ZAR
- **Notes**: Historical base from `Painel - South Africa.xlsx` (sheet: Treasury)

### GDP
- **File**: `south_africa_gdp.csv`
- **Source**: [World Bank API — NY.GDP.MKTP.CN](https://api.worldbank.org/v2/country/ZA/indicator/NY.GDP.MKTP.CN)
- **Method**: World Bank REST API
- **Frequency**: Updated daily (annual releases)
- **Columns**: `Anio`, `PIB`
- **Units**: Millions of ZAR, current prices
- **Notes**: Annual only — no quarterly source available. Use most recent available year as denominator for all months in current year.

### Debt (Internal vs External)
- **File**: `south_africa_debt.csv`
- **Source**: [National Treasury Budget Time Series — Stats Table 10](https://www.treasury.gov.za/documents/National%20Budget/2026/TimeSeries/Excel/Stats_Table%2010%20-%20Timeseries%20and%20Snapshots.xlsx)
- **Method**: Direct download; URL dynamically finds latest budget year
- **Frequency**: Updated daily (annual budget releases)
- **Columns**: `Ano_Fiscal`, `Gross`, `Domestic`, `Foreign`
- **Units**: R million (gross loan debt)
- **Notes**: Fiscal year format e.g. "2024/25"; future year projections filtered out; URL updates annually with new budget

---

## Colombia

### Licitaciones (Emissions)
- **File**: `colombia_licitaciones.csv`
- **Source**: [IRC — Histórico Colocación TES](https://www.irc.gov.co/documents/d/guest/historico-colocacion-espanol-publicar-irc-1?download=true)
- **Method**: Direct download from IRC public document library
- **Frequency**: Updated daily
- **Columns**: `Fecha_Subasta`, `Fecha_Vencimiento`, `Moneda` (COP/UVR), `Tasa_Corte`, `Monto` (millions COP, Valor costo aprobado), `Tipo_Operacion`, `Tenor` (CP/LP), `Duracion`
- **Units**: Millions of COP
- **Notes**: Sheet "TES Total" covers both COP and UVR instruments; all issuance is Pré (no floating rate instruments)

### Treasury Cash Balance
- **File**: `colombia_treasury.csv`
- **Source**: [IRC — Depósitos y Saldos Remunerados](https://www.irc.gov.co/documents/d/guest/depositos-y-saldos-remunerados-3?download=true)
- **Method**: Direct download from IRC public document library
- **Frequency**: Updated daily
- **Columns**: `Periodo`, `Saldo`
- **Units**: Millions of COP
- **Notes**: Wide format in source (months as rows, years as columns); future months filtered out

### GDP
- **File**: `colombia_gdp.csv`
- **Source**: [DANE — PIB Producción Corriente](https://www.dane.gov.co/files/operaciones/PIB/anex-ProduccionCorriente-Itrim2026.xlsx)
- **Method**: Direct download; URL dynamically finds latest available quarter
- **Frequency**: Updated daily (quarterly releases)
- **Columns**: `Periodo` (quarterly date), `PIB`
- **Units**: Miles de millones de pesos (billions COP), current prices, base 2015
- **Notes**: Raw quarterly values — sum 4 rolling quarters for 12m accumulated GDP in dashboard; same methodology as Chile

### Debt (Internal vs External)
- **File**: `colombia_debt.csv`
- **Source**: [IRC — Histórico Total GNC Public Debt Profile](https://www.irc.gov.co/en/public-debt/gnc-public-debt-profile)
- **Method**: ⚠️ MANUAL DOWNLOAD — IRC authentication wall prevents automation. Download latest "Histórico Total" file monthly from IRC page and save to `\\jgprjfileserver\...\fixed income\Histórico Total {Mes}{Año}.xls`; update `file_path` in script
- **Frequency**: Manual monthly update
- **Columns**: `Periodo`, `Deuda_Interna`, `Deuda_Externa`, `Deuda_Total`
- **Units**: COP millones (gross debt)
- **Notes**: Sheet "Saldos"; most recent file: "Histórico Total Mayo2026"

---

## Pipeline Infrastructure

- **Master script**: `run_all.R` — runs all scripts sequentially with error handling and logging
- **Log file**: `\\jgprjfileserver\...\fixed income\data\run_log.txt`
- **Scheduling**: Windows Task Scheduler — 8:00 AM and 6:00 PM daily
- **R version**: 4.6.0 — `C:\Users\lbonjean\AppData\Local\Programs\R\R-4.6.0\bin\x64\Rscript.exe`
- **API tokens**: `BANXICO_TOKEN` stored in `.Renviron` via `usethis::edit_r_environ()`
- **File server**: `\\jgprjfileserver\Compartilhadas\Summer\lbonjean\fixed income\`

---

## Maturity Profiles

### Colombia
- **Files**: `colombia_maturity.csv`, `colombia_holdings.csv`
- **Source**: [IRC — TES Perfil Deuda](https://www.irc.gov.co/documents/d/guest/tes-perfil-deuda-jun-30?download=true)
- **Method**: PDF scraping via `pdftools`; page 3 = maturity by year, page 11 = holdings by sector
- **Script**: `colombia_maturity_holdings.R`
- **Frequency**: ⚠️ MANUAL MONTHLY UPDATE — update URL in script each month
- **URL pattern**: `tes-perfil-deuda-{mon}-{last_day}` (e.g. jul-31, ago-31, sep-30)
- **Maturity columns**: `Ano_Vencimiento`, `TES_COP`, `TES_UVR`, `Total` (millions COP), `Fecha_Corte`
- **Holdings columns**: `Tenedor`, `Total` (millions COP), `Fecha_Corte`
- **Current snapshot**: June 30, 2026

### Mexico
- **File**: `mexico_maturity.csv`
- **Source**: [SHCP Informe Trimestral de Deuda Pública](https://www.finanzaspublicas.hacienda.gob.mx/work/models/Finanzas_Publicas/docs/congreso/infotrim/)
- **Method**: PDF scraping via `pdftools`; dynamically finds latest quarterly report
- **Script**: `mexico_maturity.R`
- **URL pattern**: `itindc_{YYYY}{QQ}.PDF` where QQ = 01, 02, 03, 04
- **Frequency**: Updated daily (quarterly releases)
- **Columns**: `Instrumento`, `Ano` (year), `Monto` (millions MXN), `Fecha_Corte`
- **Instruments**: Total, Cetes, Bondes D, Bondes F, Bondes G, Bonos Tasa Fija Bono M, Udibonos
- **Coverage**: 6 years only (current year + 5)
- **Current snapshot**: March 31, 2026

### South Africa
- **File**: `south_africa_maturity.csv`
- **Source**: [National Treasury — Schedule of Domestic Government Bonds](https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Schedule%20of%20Domestic%20Debt/)
- **Method**: PDF scraping via `pdftools` using httr GET with browser headers
- **Script**: `south_africa_maturity.R`
- **Frequency**: ⚠️ MANUAL MONTHLY UPDATE — update URL in script each month
- **URL pattern**: `Schedule of domestic government bonds as at {DD} {Month} {YYYY}.pdf`
- **Columns**: `Ano_Fiscal` (e.g. "2026/27"), `Start_Year`, `Nominal_ZAR` (raw ZAR), `Fecha_Corte`
- **Coverage**: Full maturity schedule (2026/27 to 2057/58)
- **Current snapshot**: April 30, 2026
- **Note**: Fiscal year April–March; requires httr GET with User-Agent and Referer headers

### Chile
- **No maturity profile CSV** — static KPI card only
- **Value**: Amortizações de Bônus em 2026: US$ 7.211 MM
- **Source**: [Ministerio de Hacienda — Datos de la Deuda Pública](https://www.hacienda.cl/areas-de-trabajo/finanzas-internacionales/oficina-de-la-deuda-publica/datos-de-la-deuda-publica-de-chile)
- **Update frequency**: Annual (update each January with new financing plan)

---

## Holdings by Sector

### Mexico
- **File**: `mexico_holdings.csv`
- **Source**: Banxico SIE — Valores en Circulación / Por sector (GUBERNAMENTAL series)
- **Method**: Banxico REST API
- **Script**: `mexico_holdings.R`
- **Frequency**: Updated daily (weekly releases)
- **Series IDs**: SF65219 (Total), SF65211 (Bancario), SF65218 (Extranjero), SF65217 (Residentes País), SF65213 (Siefores), SF65214 (Soc. Inversión), SF65210 (Reportos Banxico), SF65212 (Garantías Banxico), SF65215 (Aseguradoras), SF65216 (Otros Residentes), SF235837 (Valores Banxico)
- **Units**: Millions of MXN
- **Note**: Values are absolute amounts — convert to % of Total for composition chart

### South Africa
- **File**: `south_africa_holdings.csv`
- **Source**: [National Treasury Investor Relations — Holdings of Domestic Bonds](https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Holdings%20of%20Domestic%20Bonds/)
- **Method**: Downloads latest monthly Excel file; dynamic URL finder with fallback
- **Script**: `south_africa_holdings.R`
- **Frequency**: Updated daily (monthly releases)
- **Columns**: `Periodo`, `Non_Residents`, `Banks`, `Insurers`, `Local_Pension_Funds`, `Other_Financial`, `Other`
- **Units**: Decimal (0-1) — multiply by 100 for %
- **Note**: February abbreviated as "Feb" in filenames

### Colombia
- **File**: `colombia_holdings.csv`
- **Source**: IRC — TES Perfil Deuda PDF (page 11)
- **Method**: PDF scraping via `pdftools`; extracted alongside maturity profile
- **Script**: `colombia_maturity_holdings.R`
- **Frequency**: ⚠️ MANUAL MONTHLY UPDATE — same as maturity profile
- **Columns**: `Tenedor`, `Total` (millions COP), `Fecha_Corte`
- **Current snapshot**: June 30, 2026
- **Note**: Point-in-time snapshot only — no historical series available

### Chile
- **Status**: SKIPPED — BCCh does not publish government-bond-specific sectoral holdings breakdown

---

## Prazo Médio da Dívida (Weighted Average Maturity)

### Chile
- **Type**: Static KPI card — no CSV
- **Value**: 10.4 anos (fechamento 2025)
- **Source**: [Ministerio de Hacienda — Datos de la Deuda Pública](https://www.hacienda.cl/areas-de-trabajo/finanzas-internacionales/oficina-de-la-deuda-publica/datos-de-la-deuda-publica-de-chile)
- **Update frequency**: Annual — update manually each January with new financing plan
- **Note**: Covers total government debt portfolio

### Mexico
- **File**: `mexico_avg_maturity.csv`
- **Source**: Banxico SIE — Finanzas Públicas / Otros indicadores de deuda pública
- **Series**: SG231 — Plazo promedio ponderado de vencimiento de valores gubernamentales
- **Method**: Banxico REST API; values in days, converted to years (/365)
- **Script**: `mexico_avg_maturity.R`
- **Frequency**: Updated daily (monthly releases)
- **Columns**: `Periodo`, `Dias`, `Anos`
- **Note**: Covers all government securities including T-bills; chart shows December of each year from 2010 onwards

### South Africa
- **File**: `south_africa_avg_maturity.csv`
- **Source**: National Treasury — Budget Review 2026, Chapter 7, Figure 7.2
- **Method**: Hardcoded from published Budget Review; update annually each February/March
- **Script**: `south_africa_avg_maturity.R`
- **Frequency**: Annual — update manually with each Budget Review
- **Columns**: `Fiscal_Year`, `WAM_Anos`
- **Coverage**: 2013/14 to 2025/26
- **Note**: Fixed-rate bonds (R-bonds) only — excludes T-bills, inflation-linked bonds (I-bonds), and floating rate notes (RN/RS)

### Colombia
- **File**: `colombia_avg_maturity.csv`
- **Source**: IRC — TES Perfil Deuda PDF (page 1, "Vida Media" — Total column)
- **Method**: Extracted from PDF alongside maturity profile in `colombia_maturity_holdings.R`
- **Frequency**: ⚠️ MANUAL MONTHLY UPDATE — same as maturity profile
- **Columns**: `Fecha_Corte`, `Vida_Media`
- **Type**: Point-in-time snapshot — KPI card only, no historical series
- **Note**: Covers total TES portfolio (COP + UVR combined)
