# ============================================================
# Sovereign Bond Issuance Tracker — Shiny Dashboard
# Countries: Chile, Mexico, South Africa, Colombia
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
library(readr)
library(zoo)

# ── Paths ────────────────────────────────────────────────────
DATA_DIR <- if (dir.exists("\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data")) {
  "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data"
} else {
  "data"
}

path <- function(file) file.path(DATA_DIR, file)

# ── Theme / palette ──────────────────────────────────────────
CLR_CP      <- "#90C987"   # light green
CLR_LP      <- "#AAAAAA"   # light grey
CLR_TOTAL   <- "#E84855"   # red (total line / dots)
CLR_CURRENT <- "#E84855"   # red (current year highlight)
CLR_HIST    <- "#BBBBBB"
CLR_MED     <- "#555555"
CLR_PRE     <- "#4472C4"   # fixed rate (Pré)
CLR_POS     <- "#FFC000"   # floating rate (Pós)

# Donut chart: shared semantic colors across countries
DONUT_COLORS <- c(
  "CP"               = "#90C987",  # Chile / short-term
  "LP"               = "#AAAAAA",  # Chile / long-term
  "BTP"              = "#AAAAAA",  # Chile nominal CLP
  "BTU"              = "#5B9BD5",  # Chile inflation-linked UF
  "Cetes"            = "#90C987",  # Mexico short-term (= CP)
  "Bono M"           = "#AAAAAA",  # Mexico nominal fixed
  "Udibono"          = "#5B9BD5",  # Mexico inflation-linked
  "Bondes"           = "#FFC000",  # Mexico floating rate
  "Nominal"          = "#AAAAAA",  # Mexico/SA currency grouping
  "Inflation-linked" = "#5B9BD5",  # Mexico currency grouping
  "Treasury Bill"    = "#90C987",  # SA short-term (= CP)
  "Fixed"            = "#AAAAAA",  # SA nominal fixed
  "Inflation-Linked" = "#5B9BD5",  # SA inflation-linked (= Udibono)
  "FRN"              = "#FFC000",  # SA floating rate (= Bondes)
  "ZAR"              = "#AAAAAA",  # SA nominal ZAR grouping
  "COP"              = "#90C987",  # Colombia nominal peso
  "UVR"              = "#5B9BD5"   # Colombia inflation-linked
)

plotly_placeholder <- function(msg = "Em breve") {
  plot_ly() |>
    layout(
      xaxis       = list(visible = FALSE),
      yaxis       = list(visible = FALSE),
      annotations = list(list(
        text = msg, x = 0.5, y = 0.5, showarrow = FALSE,
        xref = "paper", yref = "paper",
        font = list(size = 15, color = "grey60")
      ))
    )
}

PLOT_THEME <- theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    axis.title.x       = element_blank(),
    axis.title.y       = element_blank(),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 10)
  )

# ============================================================
# DATA LOADING & NORMALISATION
# ============================================================

fy_chile  <- function(d) year(d)
fy_mexico <- function(d) year(d)
fy_sa     <- function(d) if_else(month(d) >= 4, year(d), year(d) - 1L)

fiscal_month <- function(d, country) {
  if (country == "south_africa") ((month(d) - 4L) %% 12L) + 1L
  else month(d)
}

# ── Chile ────────────────────────────────────────────────────
load_chile <- function() {
  lic <- read_csv(path("chile_licitaciones.csv"), show_col_types = FALSE) |>
    filter(!is.na(Fecha_Licitacion), !is.na(Monto)) |>
    mutate(
      Fecha      = as.Date(Fecha_Licitacion),
      Monto      = Monto / 1e6,
      Tipo       = if_else(Tenor == "CP", "CP", "LP"),
      FY         = fy_chile(Fecha),
      Mes_Fiscal = month(Fecha)
    ) |>
    filter(Fecha >= as.Date("2000-01-01"), Fecha <= Sys.Date())

  tsy <- read_csv(path("chile_treasury.csv"), show_col_types = FALSE) |>
    mutate(
      Fecha      = as.Date(Periodo),
      Saldo      = Total,
      FY         = fy_chile(Fecha),
      Mes_Fiscal = month(Fecha)
    )

  gdp <- read_csv(path("chile_gdp.csv"), show_col_types = FALSE) |>
    mutate(Periodo = as.Date(Periodo)) |>
    arrange(Periodo) |>
    mutate(
      PIB_tri = rollsum(PIB, k = 4, fill = NA, align = "right") / 1e3,
      FY      = fy_chile(Periodo)
    )

  gdp_usd <- read_csv(path("chile_gdp_usd.csv"), show_col_types = FALSE) |>
    mutate(
      FY      = as.integer(Anio),
      Periodo = as.Date(paste0(FY, "-01-01")),
      PIB_tri = PIB / 1e12
    )

  debt <- read_csv(path("chile_debt.csv"), show_col_types = FALSE) |>
    mutate(Periodo = as.Date(Periodo)) |>
    filter(!is.na(Total), month(Periodo) == 10) |>   # Q4 = October observation
    mutate(Year     = year(Periodo),
           X_label  = as.character(Year),
           Debt_tri = Total   / 1e6,
           Int_tri  = Interna / 1e6,
           Ext_tri  = Externa / 1e6) |>
    filter(Year >= 2005) |>
    arrange(Year)

  list(lic = lic, tsy = tsy, gdp = gdp, gdp_usd = gdp_usd, debt = debt)
}

# ── Mexico ───────────────────────────────────────────────────
load_mexico <- function() {
  # UDI/MXN daily rate — forward-filled to cover weekends/holidays
  udi_raw <- read_csv(path("mexico_udi.csv"), show_col_types = FALSE) |>
    mutate(Fecha = as.Date(Fecha)) |>
    arrange(Fecha)
  udi_daily <- tibble(Fecha = seq(min(udi_raw$Fecha), Sys.Date(), by = "day")) |>
    left_join(udi_raw, by = "Fecha") |>
    fill(UDI_MXN, .direction = "down")

  lic <- read_csv(path("mexico_licitaciones.csv"), show_col_types = FALSE) |>
    filter(!is.na(Fecha), !is.na(Monto)) |>
    mutate(Fecha = as.Date(Fecha)) |>
    left_join(udi_daily, by = "Fecha") |>
    mutate(
      # Convert Udibono amounts from millions UDI → millions MXN before scaling
      Monto      = if_else(Instrumento == "Udibono", Monto * UDI_MXN, Monto),
      Monto      = Monto / 1e6,
      Tipo       = if_else(Plazo <= 365, "CP", "LP"),
      FY         = fy_mexico(Fecha),
      Mes_Fiscal = month(Fecha)
    ) |>
    select(-UDI_MXN) |>
    filter(Fecha >= as.Date("2000-01-01"), Fecha <= Sys.Date())

  tsy <- read_csv(path("mexico_treasury.csv"), show_col_types = FALSE) |>
    mutate(
      Fecha      = as.Date(Periodo),
      Saldo      = Saldo / 1e3,   # millions MXN → billions MXN
      FY         = fy_mexico(Fecha),
      Mes_Fiscal = month(Fecha)
    )

  gdp <- read_csv(path("mexico_gdp.csv"), show_col_types = FALSE) |>
    mutate(
      Periodo = as.Date(Periodo),
      PIB_tri = PIB / 1e6,
      FY      = fy_mexico(Periodo)
    )

  debt <- read_csv(path("mexico_debt.csv"), show_col_types = FALSE) |>
    mutate(Periodo = as.Date(Periodo)) |>
    filter(!is.na(Total), month(Periodo) == 12) |>
    mutate(Year     = year(Periodo),
           X_label  = as.character(Year),
           Debt_tri = Total   / 1e3,
           Int_tri  = Interna / 1e3,
           Ext_tri  = Externa / 1e3) |>
    filter(Year >= 2005) |>
    arrange(Year)

  holdings <- read_csv(path("mexico_holdings.csv"), show_col_types = FALSE) |>
    mutate(Periodo = as.Date(Periodo))

  maturity <- read_csv(path("mexico_maturity.csv"), show_col_types = FALSE)

  avg_maturity <- read_csv(path("mexico_avg_maturity.csv"), show_col_types = FALSE) |>
    mutate(Periodo = as.Date(Periodo))

  list(lic = lic, tsy = tsy, gdp = gdp, debt = debt,
       holdings = holdings, maturity = maturity, avg_maturity = avg_maturity)
}

# ── South Africa ─────────────────────────────────────────────
load_sa <- function() {
  lic <- read_csv(path("south_africa_licitaciones.csv"), show_col_types = FALSE) |>
    filter(!is.na(Fecha_Subasta), !is.na(Monto_Asignado)) |>
    mutate(
      Fecha      = as.Date(Fecha_Subasta),
      Monto      = Monto_Asignado / 1e12,
      Tipo       = if_else(Tipo_Bono == "Treasury Bill", "CP", "LP"),
      FY         = fy_sa(Fecha),
      Mes_Fiscal = fiscal_month(Fecha, "south_africa")
    ) |>
    filter(Fecha >= as.Date("2000-01-01"), Fecha <= Sys.Date())

  tsy <- read_csv(path("south_africa_treasury.csv"), show_col_types = FALSE) |>
    mutate(
      Fecha      = as.Date(paste0(Periodo, "-01")),
      Saldo      = Saldo / 1e9,
      FY         = fy_sa(Fecha),
      Mes_Fiscal = fiscal_month(Fecha, "south_africa")
    )

  gdp <- read_csv(path("south_africa_gdp.csv"), show_col_types = FALSE) |>
    mutate(
      FY      = as.integer(Anio),
      Periodo = as.Date(paste0(FY, "-01-01")),
      PIB_tri = PIB / 1e6
    )

  debt <- read_csv(path("south_africa_debt.csv"), show_col_types = FALSE) |>
    filter(!is.na(Gross)) |>
    mutate(FY_start = as.integer(substr(Ano_Fiscal, 1, 4)),
           X_label  = paste0(FY_start, "/", substr(as.character(FY_start + 1), 3, 4)),
           Debt_tri = Gross    / 1e6,
           Int_tri  = Domestic / 1e6,
           Ext_tri  = Foreign  / 1e6) |>
    filter(FY_start >= 2005) |>
    arrange(FY_start)

  holdings <- read_csv(path("south_africa_holdings.csv"), show_col_types = FALSE) |>
    mutate(Periodo = as.Date(Periodo))

  maturity <- read_csv(path("south_africa_maturity.csv"), show_col_types = FALSE)

  avg_maturity <- read_csv(path("south_africa_avg_maturity.csv"), show_col_types = FALSE)

  list(lic = lic, tsy = tsy, gdp = gdp, debt = debt, holdings = holdings,
       maturity = maturity, avg_maturity = avg_maturity)
}

load_colombia <- function() {
  lic <- read_csv(path("colombia_licitaciones.csv"), show_col_types = FALSE) |>
    filter(!is.na(Fecha_Subasta), !is.na(Monto)) |>
    mutate(
      Fecha      = as.Date(Fecha_Subasta),
      Monto      = Monto / 1e6,     # millions COP → trillions COP
      Tipo       = Tenor,            # already "CP" or "LP"
      FY         = year(Fecha),
      Mes_Fiscal = month(Fecha)
    ) |>
    filter(Fecha >= as.Date("2000-01-01"), Fecha <= Sys.Date())

  tsy <- read_csv(path("colombia_treasury.csv"), show_col_types = FALSE) |>
    mutate(
      Fecha      = as.Date(Periodo),
      Saldo      = Saldo / 1e6,     # millions COP → trillions COP
      FY         = year(Fecha),
      Mes_Fiscal = month(Fecha)
    )

  gdp <- read_csv(path("colombia_gdp.csv"), show_col_types = FALSE) |>
    mutate(Periodo = as.Date(Periodo)) |>
    arrange(Periodo) |>
    mutate(
      PIB_tri = rollsum(PIB, k = 4, fill = NA, align = "right") / 1e3,
      FY      = year(Periodo)
    )

  debt <- read_csv(path("colombia_debt.csv"), show_col_types = FALSE) |>
    mutate(Periodo = as.Date(Periodo)) |>
    filter(!is.na(Deuda_Total), month(Periodo) == 12) |>
    mutate(Year     = year(Periodo),
           X_label  = as.character(Year),
           Debt_tri = Deuda_Total  / 1e6,
           Int_tri  = Deuda_Interna / 1e6,
           Ext_tri  = Deuda_Externa / 1e6) |>
    filter(Year >= 2005) |>
    arrange(Year)

  holdings_co <- read_csv(path("colombia_holdings.csv"), show_col_types = FALSE)

  maturity <- read_csv(path("colombia_maturity.csv"), show_col_types = FALSE)

  avg_maturity <- read_csv(path("colombia_avg_maturity.csv"), show_col_types = FALSE)

  list(lic = lic, tsy = tsy, gdp = gdp, debt = debt,
       holdings = holdings_co, maturity = maturity, avg_maturity = avg_maturity)
}

# ── Annual targets (updated manually each year) ──────────────
TARGET_MEXICO <- data.frame(
  Categoria = c("Total", "Cetes", "Bondes", "Bonos", "Udibonos"),
  Target    = c(3.15, 1.63, 0.82, 0.23, 0.36),
  stringsAsFactors = FALSE
)

# Chile has two targets: original and extended
TARGET_CHILE <- data.frame(
  Categoria = c("Meta Original", "Meta Ampliada"),
  Target    = c(16.60, 22.32),
  stringsAsFactors = FALSE
)

TARGET_SA <- data.frame(
  Categoria = c("Total", "Treasury Bills", "Long-term"),
  Target    = c(1.853, 1.610, 0.243),
  stringsAsFactors = FALSE
)

# ============================================================
# CHART BUILDERS
# ============================================================

# Shared helper: cleans ggplotly legend artifacts
#   - "(CP,1)" → "CP"
#   - removes "colour" / "Tipo" group headers
#   - optionally hides the line entry for Total (keeps circle)
#   - deduplicates repeated entries
clean_legend <- function(plt, hide_line_total = FALSE) {
  seen <- character(0)
  for (i in seq_along(plt$x$data)) {
    nm   <- plt$x$data[[i]]$name
    mode <- plt$x$data[[i]]$mode
    nm_clean <- if (!is.null(nm)) gsub("\\((.+),\\d+\\)", "\\1", nm) else ""
    plt$x$data[[i]]$name        <- nm_clean
    plt$x$data[[i]]$legendgroup <- ""   # empty string = no group = no group header
    # Hide ggplotly legend-group header entries — they have no actual y data
    has_data <- !is.null(plt$x$data[[i]]$y) && length(plt$x$data[[i]]$y) > 0
    if (!has_data) {
      plt$x$data[[i]]$showlegend <- FALSE
    } else if (hide_line_total && nm_clean == "Total" && identical(mode, "lines")) {
      plt$x$data[[i]]$showlegend <- FALSE
    } else if (nm_clean != "" && nm_clean %in% seen) {
      plt$x$data[[i]]$showlegend <- FALSE
    } else if (nm_clean != "") {
      seen <- c(seen, nm_clean)
    }
  }
  plt
}

# Classify each auction as Pré (fixed) or Pós (floating) by country rules
classify_pre_pos <- function(lic, country) {
  if (country %in% c("chile", "colombia")) {
    lic |> mutate(PrePos = "Pré")
  } else if (country == "mexico") {
    lic |> mutate(PrePos = case_when(
      Instrumento == "Cetes"                              ~ "Pré",
      grepl("Bono M",  Instrumento, ignore.case = TRUE)  ~ "Pré",
      grepl("Udibono", Instrumento, ignore.case = TRUE)  ~ "Pré",
      grepl("Bondes",  Instrumento, ignore.case = TRUE)  ~ "Pós",
      TRUE                                               ~ NA_character_
    ))
  } else { # south_africa
    lic |> mutate(PrePos = case_when(
      Tipo_Bono %in% c("Fixed", "Inflation-Linked", "Treasury Bill") ~ "Pré",
      Tipo_Bono == "FRN"                                              ~ "Pós",
      TRUE                                                            ~ NA_character_
    ))
  }
}

current_fy <- function(country) {
  today <- Sys.Date()
  if (country == "south_africa") fy_sa(today) else year(today)
}

# Format a fiscal year number as a label (e.g. 2026 → "2026/27" for SA)
fmt_fy <- function(fy, country) {
  if (country == "south_africa") {
    paste0(fy, "/", substr(as.character(fy + 1), 3, 4))
  } else {
    as.character(fy)
  }
}

month_labels <- function(country) {
  if (country == "south_africa")
    c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
  else
    month.abb
}

# ── 1. YTD issuance by fiscal year ──────────────────────────
# For each year, sums all auctions from Jan 1 (or fiscal-year start) through
# the same month-day as today. E.g. if today = Jul 6: sums Jan 1 – Jul 6 for
# every year in the data. String comparison "%m-%d" avoids Feb-29 edge cases.
chart_ytd <- function(lic, country, ccy_label) {
  today      <- Sys.Date()
  cur_fy     <- current_fy(country)
  fy_start   <- if (country == "south_africa") "-04-01" else "-01-01"

  # Days elapsed since fiscal year start for today and each auction
  today_fy_start  <- as.Date(paste0(cur_fy, fy_start))
  today_doy       <- as.numeric(today - today_fy_start)

  ytd <- lic |>
    mutate(auction_doy = as.numeric(Fecha - as.Date(paste0(FY, fy_start)))) |>
    filter(auction_doy >= 0, auction_doy <= today_doy) |>
    group_by(FY, Tipo) |>
    summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop") |>
    mutate(FY_label = factor(FY))

  # Total per year for bar-top labels
  totals <- ytd |>
    group_by(FY, FY_label) |>
    summarise(Total = sum(Monto), .groups = "drop")

  if (nrow(ytd) == 0) return(plotly_empty(type = "bar"))

  p <- ggplot(ytd, aes(
    x    = FY_label,
    y    = Monto,
    fill = Tipo,
    text = paste0(
      FY, "  (", format(today_fy_start, "%d %b"), " – ", format(today, "%d %b"), ")",
      "\n", Tipo, ": ", round(Monto, 3), " ", ccy_label
    )
  )) +
    geom_col(position = "stack", width = 0.65) +
    geom_point(
      data        = totals,
      aes(x = FY_label, y = Total, colour = "Total",
          text = paste0(FY, "\nTotal: ", round(Total, 3), " ", ccy_label)),
      size = 4, inherit.aes = FALSE
    ) +
    scale_fill_manual(values   = c(CP = CLR_CP, LP = CLR_LP)) +
    scale_colour_manual(values = c(Total = CLR_TOTAL)) +
    scale_y_continuous(
      labels = label_number(suffix = paste0(" ", ccy_label), accuracy = 0.01),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      subtitle = paste0(
        format(today_fy_start, "%d %b"), " – ", format(today, "%d %b"),
        " (mesmo período todos os anos)"
      )
    ) +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.25),
           margin = list(b = 80))
}

# ── 2. Monthly issuance — rolling 24-month window ───────────
# Continuous timeline: one stacked bar per month, CP+LP, with a total line.
chart_monthly <- function(lic, country, ccy_label) {
  today      <- Sys.Date()
  start_date <- floor_date(today %m-% months(23), "month")  # 24 months incl. current

  monthly <- lic |>
    filter(Fecha >= start_date, Fecha <= today) |>
    mutate(Mes_Date = floor_date(Fecha, "month")) |>
    group_by(Mes_Date, Tipo) |>
    summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")

  totals <- monthly |>
    group_by(Mes_Date) |>
    summarise(Total = sum(Monto), .groups = "drop")

  if (nrow(monthly) == 0) return(plotly_empty(type = "bar"))

  p <- ggplot(monthly, aes(
    x    = Mes_Date,
    y    = Monto,
    fill = Tipo,
    text = paste0(format(Mes_Date, "%b %Y"), "\n", Tipo, ": ", round(Monto, 3), " ", ccy_label)
  )) +
    geom_col(position = "stack", width = 25) +
    geom_line(
      data      = totals,
      aes(x = Mes_Date, y = Total, colour = "Total",
          text = paste0(format(Mes_Date, "%b %Y"), "\nTotal: ", round(Total, 3), " ", ccy_label)),
      linewidth = 1, inherit.aes = FALSE
    ) +
    geom_point(
      data = totals,
      aes(x = Mes_Date, y = Total, colour = "Total",
          text = paste0(format(Mes_Date, "%b %Y"), "\nTotal: ", round(Total, 3), " ", ccy_label)),
      size = 2.2, inherit.aes = FALSE
    ) +
    scale_fill_manual(values   = c(CP = CLR_CP, LP = CLR_LP)) +
    scale_colour_manual(values = c(Total = CLR_TOTAL)) +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y",
      expand      = expansion(add = 15)
    ) +
    scale_y_continuous(
      labels = label_number(suffix = paste0(" ", ccy_label), accuracy = 0.01),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      subtitle = paste0(
        format(start_date, "%b %Y"), " — ", format(floor_date(today, "month"), "%b %Y")
      )
    ) +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  clean_legend(ggplotly(p, tooltip = "text"), hide_line_total = TRUE) |>
    layout(legend = list(orientation = "h", y = -0.25),
           margin = list(b = 80))
}

# ── 3. Monthly issuance as % of rolling 12m GDP ─────────────
# GDP denominator steps up only when a new quarterly release is available (LOCF).
chart_monthly_pct_gdp <- function(lic, gdp, country, ccy_label) {
  today      <- Sys.Date()
  start_date <- floor_date(today %m-% months(23), "month")

  monthly <- lic |>
    filter(Fecha >= start_date, Fecha <= today) |>
    mutate(Mes_Date = floor_date(Fecha, "month")) |>
    group_by(Mes_Date, Tipo) |>
    summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")

  # LOCF: for each month, use most recent quarterly GDP released at or before it
  gdp_pts <- gdp |>
    filter(!is.na(PIB_tri)) |>
    arrange(Periodo) |>
    select(Periodo, PIB_tri)

  gdp_for_month <- function(d) {
    idx <- which(gdp_pts$Periodo <= d)
    if (length(idx) == 0) NA_real_ else gdp_pts$PIB_tri[max(idx)]
  }

  month_gdp <- monthly |>
    distinct(Mes_Date) |>
    mutate(PIB_tri = sapply(Mes_Date, gdp_for_month))

  monthly <- monthly |>
    left_join(month_gdp, by = "Mes_Date") |>
    filter(!is.na(PIB_tri)) |>
    mutate(Pct = Monto / PIB_tri * 100)

  totals <- monthly |>
    group_by(Mes_Date) |>
    summarise(Total_Pct = sum(Pct), .groups = "drop")

  if (nrow(monthly) == 0) return(plotly_empty(type = "bar"))

  p <- ggplot(monthly, aes(
    x    = Mes_Date,
    y    = Pct,
    fill = Tipo,
    text = paste0(format(Mes_Date, "%b %Y"), "\n", Tipo, ": ",
                  round(Pct, 3), "% do PIB")
  )) +
    geom_col(position = "stack", width = 25) +
    geom_line(
      data = totals,
      aes(x = Mes_Date, y = Total_Pct, colour = "Total",
          text = paste0(format(Mes_Date, "%b %Y"), "\nTotal: ",
                        round(Total_Pct, 3), "% do PIB")),
      linewidth = 1, inherit.aes = FALSE
    ) +
    geom_point(
      data = totals,
      aes(x = Mes_Date, y = Total_Pct, colour = "Total",
          text = paste0(format(Mes_Date, "%b %Y"), "\nTotal: ",
                        round(Total_Pct, 3), "% do PIB")),
      size = 2.2, inherit.aes = FALSE
    ) +
    scale_fill_manual(values   = c(CP = CLR_CP, LP = CLR_LP)) +
    scale_colour_manual(values = c(Total = CLR_TOTAL)) +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b %Y",
      expand      = expansion(add = 15)
    ) +
    scale_y_continuous(
      labels = label_percent(scale = 1, accuracy = 0.01),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(subtitle = paste0(
      format(start_date, "%b %Y"), " — ",
      format(floor_date(today, "month"), "%b %Y"),
      " | PIB acumulado 12 meses"
    )) +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  clean_legend(ggplotly(p, tooltip = "text"), hide_line_total = TRUE) |>
    layout(legend = list(orientation = "h", y = -0.25), margin = list(b = 80))
}

# ── 4. YTD issuance as % of rolling 12m GDP, per fiscal year ─
# For each FY, cumulative issuance from fiscal year start to today's calendar
# position, divided by the most recent available rolling 12m GDP within that
# same window. Start year derived from data overlap, not hardcoded.
chart_ytd_pct_gdp <- function(lic, gdp, country, ccy_label) {
  today          <- Sys.Date()
  cur_fy         <- current_fy(country)
  fy_start_str   <- if (country == "south_africa") "-04-01" else "-01-01"
  today_fy_start <- as.Date(paste0(cur_fy, fy_start_str))
  today_doy      <- as.numeric(today - today_fy_start)

  ytd <- lic |>
    mutate(auction_doy = as.numeric(Fecha - as.Date(paste0(FY, fy_start_str)))) |>
    filter(auction_doy >= 0, auction_doy <= today_doy) |>
    group_by(FY, Tipo) |>
    summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")

  if (nrow(ytd) == 0) return(plotly_empty(type = "bar"))

  # GDP LOCF: for each FY X, use the most recent GDP Periodo that falls
  # within the equivalent YTD window of that year
  gdp_pts <- gdp |> filter(!is.na(PIB_tri)) |> arrange(Periodo)

  gdp_by_fy <- do.call(rbind, lapply(unique(ytd$FY), function(fy) {
    fy_today   <- as.Date(paste0(fy, fy_start_str)) + today_doy
    candidates <- gdp_pts$PIB_tri[gdp_pts$Periodo <= fy_today]
    if (length(candidates) == 0) return(NULL)
    data.frame(FY = fy, PIB_tri = tail(candidates, 1))
  }))

  if (is.null(gdp_by_fy) || nrow(gdp_by_fy) == 0) return(plotly_empty(type = "bar"))

  df <- ytd |>
    left_join(gdp_by_fy, by = "FY") |>
    filter(!is.na(PIB_tri)) |>
    mutate(Pct = Monto / PIB_tri * 100, FY_label = factor(FY))

  totals <- df |>
    group_by(FY, FY_label) |>
    summarise(Total_Pct = sum(Pct), .groups = "drop")

  p <- ggplot(df, aes(
    x    = FY_label,
    y    = Pct,
    fill = Tipo,
    text = paste0(FY, "\n", Tipo, ": ", round(Pct, 2), "% do PIB")
  )) +
    geom_col(position = "stack", width = 0.65) +
    geom_point(
      data = totals,
      aes(x = FY_label, y = Total_Pct, colour = "Total",
          text = paste0(FY, "\nTotal: ", round(Total_Pct, 2), "% do PIB")),
      size = 4, inherit.aes = FALSE
    ) +
    scale_fill_manual(values   = c(CP = CLR_CP, LP = CLR_LP)) +
    scale_colour_manual(values = c(Total = CLR_TOTAL)) +
    scale_y_continuous(
      labels = label_percent(scale = 1, accuracy = 0.01),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(subtitle = paste0(
      format(today_fy_start, "%d %b"), " – ", format(today, "%d %b"),
      " (mesmo período todos os anos) | PIB acumulado 12 meses"
    )) +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.25), margin = list(b = 80))
}

# ── 5. Annual issuance as % of GDP ──────────────────────────
# Stacked CP/LP bars (% of GDP) + red dot at total, matching Excel style.
# Past completed years: full year. Current year: YTD.
chart_pct_gdp <- function(lic, gdp, country) {
  today  <- Sys.Date()
  cur_fy <- current_fy(country)

  # Build issuance by FY + Tipo
  past <- lic |>
    filter(FY < cur_fy) |>
    group_by(FY, Tipo) |>
    summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop") |>
    mutate(Periodo = "Ano completo")

  curr <- lic |>
    filter(FY == cur_fy, Fecha <= today) |>
    group_by(FY, Tipo) |>
    summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop") |>
    mutate(Periodo = paste0("YTD ", format(today, "%d %b")))

  df_iss <- bind_rows(past, curr)

  # Most recent available quarterly GDP within each fiscal year
  gdp_fy <- gdp |>
    filter(!is.na(PIB_tri)) |>
    group_by(FY) |>
    slice_max(order_by = Periodo, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(FY, PIB_tri)

  df <- df_iss |>
    left_join(gdp_fy, by = "FY") |>
    mutate(Pct = Monto / PIB_tri * 100)

  # Totals per year for the dot and label
  totals <- df |>
    group_by(FY, Periodo) |>
    summarise(Total_Pct = sum(Pct), .groups = "drop")

  if (nrow(df) == 0) return(plotly_empty(type = "bar"))

  p <- ggplot(df, aes(
    x    = factor(FY),
    y    = Pct,
    fill = Tipo,
    text = paste0(FY, "\n", Tipo, ": ", round(Pct, 1), "% do PIB")
  )) +
    geom_col(width = 0.65, position = "stack") +
    geom_point(
      data = totals,
      aes(x = factor(FY), y = Total_Pct, colour = "Total",
          text = paste0(FY, " (", Periodo, ")\nTotal: ", round(Total_Pct, 1), "% do PIB")),
      size = 4, inherit.aes = FALSE
    ) +
    scale_fill_manual(values   = c(CP = CLR_CP, LP = CLR_LP)) +
    scale_colour_manual(values = c(Total = CLR_CURRENT)) +
    scale_y_continuous(
      labels = label_percent(scale = 1, accuracy = 0.1),
      expand = expansion(mult = c(0, 0.18))
    ) +
    labs(
      title    = "Emissões % do PIB anual",
      subtitle = paste0("Anos completos; ", cur_fy, " = YTD até ", format(today, "%d %b"))
    ) +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.25),
           margin = list(b = 80))
}

# ── 4. Treasury cash balance — seasonal ─────────────────────
chart_tsy_seasonal <- function(tsy, country, ccy_label, col = "Saldo") {
  tsy    <- tsy |> mutate(Saldo = .data[[col]])
  cur_fy <- current_fy(country)
  lbl    <- month_labels(country)
  hist   <- tsy |> filter(FY < cur_fy)
  cur    <- tsy |> filter(FY == cur_fy)

  band <- hist |>
    group_by(Mes_Fiscal) |>
    summarise(
      Med = median(Saldo, na.rm = TRUE),
      Lo  = quantile(Saldo, 0.25, na.rm = TRUE),
      Hi  = quantile(Saldo, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(MesLabel = factor(lbl[Mes_Fiscal], levels = lbl))

  cur <- cur |>
    mutate(MesLabel = factor(lbl[Mes_Fiscal], levels = lbl))

  p <- ggplot(band, aes(x = MesLabel)) +
    geom_ribbon(aes(ymin = Lo, ymax = Hi, group = 1, fill = "IQR Histórico"),
                alpha = 0.25) +
    geom_line(aes(y = Med, group = 1, colour = "Mediana"),
              linewidth = 1, linetype = "dashed") +
    geom_line(data = cur,
              aes(x = MesLabel, y = Saldo, group = 1, colour = as.character(cur_fy)),
              linewidth = 1.2, inherit.aes = FALSE) +
    geom_point(data = cur,
               aes(x = MesLabel, y = Saldo, colour = as.character(cur_fy)),
               size = 2.5, inherit.aes = FALSE) +
    scale_fill_manual(values   = c("IQR Histórico" = CLR_HIST)) +
    scale_colour_manual(values = setNames(
      c(CLR_MED, CLR_CURRENT),
      c("Mediana", as.character(cur_fy))
    )) +
    scale_y_continuous(
      labels = label_number(suffix = paste0(" ", ccy_label), accuracy = 0.01)
    ) +
    labs(subtitle = paste0("AF ", cur_fy, " vs. histórico — banda IQR 25–75% | Saldo ", col, " (", ccy_label, ")")) +
    PLOT_THEME

  clean_legend(ggplotly(p, tooltip = c("x", "y", "colour", "fill"))) |>
    layout(legend = list(orientation = "h", y = -0.2))
}

# ── 5. Treasury cash balance — time series ───────────────────
chart_tsy_ts <- function(tsy, ccy_label, col = "Saldo") {
  tsy <- tsy |> mutate(Saldo = .data[[col]])
  p <- ggplot(tsy, aes(x = Fecha, y = Saldo)) +
    geom_line(colour = CLR_CURRENT, linewidth = 1) +
    scale_y_continuous(
      labels = label_number(suffix = paste0(" ", ccy_label), accuracy = 0.01)
    ) +
    labs(subtitle = paste0("Série histórica | Saldo ", col, " (", ccy_label, ")")) +
    PLOT_THEME

  ggplotly(p, tooltip = c("x", "y"))
}

# ── 6. Issuance vs. annual target ────────────────────────────
# ── Cumulative run-rate charts ───────────────────────────────
# Shared helper: cumulative monthly issuance by fiscal year
# Returns a long data frame with FY, Mes_Fiscal, Cumulative columns.
build_runrate <- function(lic, country, n_prev = 3) {
  today         <- Sys.Date()
  cur_fy        <- current_fy(country)
  fy_years      <- (cur_fy - n_prev):cur_fy
  cur_fis_month <- fiscal_month(today, country)

  # End the current-year line at the last month with actual auction data,
  # not necessarily the current calendar month (data may lag by weeks).
  cur_months      <- lic |> filter(FY == cur_fy) |> pull(Mes_Fiscal)
  last_data_month <- if (length(cur_months) == 0) 0L else max(cur_months, na.rm = TRUE)
  cutoff_month    <- min(cur_fis_month, last_data_month)

  lic |>
    filter(FY %in% fy_years) |>
    group_by(FY, Mes_Fiscal) |>
    summarise(Monthly = sum(Monto, na.rm = TRUE), .groups = "drop") |>
    complete(FY = fy_years, Mes_Fiscal = 1:12, fill = list(Monthly = 0)) |>
    arrange(FY, Mes_Fiscal) |>
    group_by(FY) |>
    mutate(Cumulative = cumsum(Monthly)) |>
    ungroup() |>
    filter(FY < cur_fy | Mes_Fiscal <= cutoff_month)
}

# Shared ggplot builder (avoids duplication between absolute and % GDP versions)
plot_runrate <- function(df, country, y_label, subtitle_extra = "",
                         target_lines = NULL) {
  cur_fy    <- max(df$FY)
  lbl       <- month_labels(country)
  fy_levels <- sort(unique(df$FY))
  fy_fmts   <- sapply(fy_levels, fmt_fy, country = country)

  # Color + size by year (current = red/bold, others fade to grey)
  clrs  <- setNames(
    c("#BBBBBB", "#78909C", "#5B9BD5", CLR_CURRENT)[seq_along(fy_levels)],
    fy_fmts
  )
  sizes <- setNames(
    c(0.6, 0.7, 0.8, 1.4)[seq_along(fy_levels)],
    fy_fmts
  )

  df <- df |> mutate(
    FY_label  = factor(sapply(FY, fmt_fy, country = country), levels = fy_fmts),
    Month_lbl = lbl[Mes_Fiscal]
  )

  df_hist <- df |> filter(FY < cur_fy)
  df_curr <- df |> filter(FY == cur_fy)

  p <- ggplot() +
    geom_line(data = df_hist,
              aes(x = Mes_Fiscal, y = Cumulative, color = FY_label,
                  group = FY_label,
                  text = paste0(FY_label, " — ", Month_lbl, "\n",
                                round(Cumulative, 3), " ", y_label)),
              linewidth = 0.75) +
    geom_line(data = df_curr,
              aes(x = Mes_Fiscal, y = Cumulative, color = FY_label,
                  group = FY_label,
                  text = paste0(FY_label, " — ", Month_lbl, "\n",
                                round(Cumulative, 3), " ", y_label)),
              linewidth = 1.4)

  if (!is.null(target_lines)) {
    p <- p + geom_hline(data = target_lines,
                        aes(yintercept = Value, linetype = Label),
                        color = "grey30", linewidth = 0.55, alpha = 0.75) +
      scale_linetype_manual(values = setNames(
        rep("dashed", nrow(target_lines)), target_lines$Label))
  }

  p +
    scale_x_continuous(breaks = 1:12, labels = lbl) +
    scale_color_manual(values = clrs, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(subtitle = paste0(
      "Emissão acumulada desde o início do ano fiscal",
      if (nzchar(subtitle_extra)) paste0(" | ", subtitle_extra) else ""
    )) +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

# Absolute version (trillions of local currency)
chart_runrate <- function(lic, country, ccy_label, target_val = NULL) {
  df <- build_runrate(lic, country)
  if (nrow(df) == 0) return(plotly_empty(type = "scatter"))

  tgt <- if (!is.null(target_val)) {
    data.frame(Label = paste0("Meta ", fmt_fy(max(df$FY), country)), Value = target_val)
  } else NULL

  p <- plot_runrate(df, country, ccy_label, target_lines = tgt)
  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.25), margin = list(b = 80))
}

# % of rolling 12m GDP version
chart_runrate_pct_gdp <- function(lic, gdp, country) {
  df <- build_runrate(lic, country)
  if (nrow(df) == 0) return(plotly_empty(type = "scatter"))

  # LOCF: for each FY use the most recent GDP released within that fiscal year,
  # so the current year doesn't drop out just because its GDP isn't published yet.
  fy_start_str <- if (country == "south_africa") "-04-01" else "-01-01"
  gdp_pts      <- gdp |> filter(!is.na(PIB_tri)) |> arrange(Periodo)

  gdp_by_fy <- do.call(rbind, lapply(unique(df$FY), function(fy) {
    fy_end     <- as.Date(paste0(fy + 1, fy_start_str)) - 1
    candidates <- gdp_pts$PIB_tri[gdp_pts$Periodo <= fy_end]
    if (length(candidates) == 0) return(NULL)
    data.frame(FY = fy, PIB_tri = tail(candidates, 1))
  }))

  if (is.null(gdp_by_fy) || nrow(gdp_by_fy) == 0) return(plotly_empty(type = "scatter"))

  df <- df |>
    left_join(gdp_by_fy, by = "FY") |>
    filter(!is.na(PIB_tri)) |>
    mutate(Cumulative = Cumulative / PIB_tri * 100)

  p <- plot_runrate(df, country, "% PIB",
                    subtitle_extra = "PIB acumulado 12 meses") +
    scale_y_continuous(
      labels = label_percent(scale = 1, accuracy = 0.01),
      expand = expansion(mult = c(0, 0.1))
    )

  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.25), margin = list(b = 80))
}

# ── Debt composition donut (YTD, current fiscal year) ────────
# dimension: "instrument" = by bond type; "currency" = by currency/index
chart_composition <- function(lic, country, ccy_label, dimension = "instrument") {
  today    <- Sys.Date()
  cur_fy   <- current_fy(country)
  fy_start <- if (country == "south_africa") {
    as.Date(paste0(cur_fy, "-04-01"))
  } else {
    as.Date(paste0(cur_fy, "-01-01"))
  }

  ytd_lic <- lic |> filter(Fecha >= fy_start, Fecha <= today)

  df <- if (dimension == "instrument") {
    if (country == "chile") {
      ytd_lic |>
        group_by(Categoria = Tipo) |>
        summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")
    } else if (country == "mexico") {
      ytd_lic |>
        mutate(Categoria = case_when(
          Instrumento == "Cetes"                             ~ "Cetes",
          grepl("Bondes",  Instrumento, ignore.case = TRUE) ~ "Bondes",
          grepl("Udibono", Instrumento, ignore.case = TRUE) ~ "Udibono",
          grepl("Bono",    Instrumento, ignore.case = TRUE) ~ "Bono M",
          TRUE                                               ~ NA_character_
        )) |>
        filter(!is.na(Categoria)) |>
        group_by(Categoria) |>
        summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")
    } else if (country == "colombia") {
      ytd_lic |>
        group_by(Categoria = Tipo) |>
        summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")
    } else { # south_africa
      ytd_lic |>
        group_by(Categoria = Tipo_Bono) |>
        summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")
    }
  } else { # currency
    if (country == "chile") {
      ytd_lic |>
        filter(!is.na(Moneda)) |>
        mutate(Categoria = if_else(Moneda == "UF", "BTU", "BTP")) |>
        group_by(Categoria) |>
        summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")
    } else if (country == "mexico") {
      ytd_lic |>
        mutate(Categoria = if_else(
          grepl("Udibono", Instrumento, ignore.case = TRUE),
          "Inflation-linked", "Nominal"
        )) |>
        group_by(Categoria) |>
        summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")
    } else if (country == "colombia") {
      ytd_lic |>
        group_by(Categoria = Moneda) |>
        summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")
    } else { # south_africa
      ytd_lic |>
        mutate(Categoria = if_else(
          Tipo_Bono == "Inflation-Linked", "Inflation-Linked", "ZAR"
        )) |>
        group_by(Categoria) |>
        summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")
    }
  }

  if (nrow(df) == 0 || sum(df$Monto, na.rm = TRUE) == 0) {
    return(plotly_empty(type = "pie"))
  }

  df <- df |>
    mutate(Total = sum(Monto), Pct = Monto / Total * 100)

  clrs <- DONUT_COLORS[df$Categoria]
  clrs[is.na(clrs)] <- "#CCCCCC"

  plot_ly(
    data         = df,
    labels       = ~Categoria,
    values       = ~Monto,
    type         = "pie",
    hole         = 0.45,
    textinfo     = "label+percent",
    textposition = "auto",
    hovertext    = ~paste0(
      "<b>", Categoria, "</b><br>",
      round(Pct, 1), "%<br>",
      round(Monto, 3), " ", ccy_label
    ),
    hoverinfo = "text",
    marker    = list(
      colors = unname(clrs),
      line   = list(color = "white", width = 2)
    ),
    showlegend = TRUE
  ) |>
    layout(
      showlegend  = TRUE,
      legend      = list(orientation = "h", y = -0.1),
      margin      = list(t = 10, b = 50, l = 10, r = 10),
      annotations = list(list(
        text      = paste0("YTD<br>", format(today, "%d %b")),
        x = 0.5, y = 0.5, showarrow = FALSE,
        font = list(size = 11, color = "grey50")
      ))
    )
}

# ── Pré vs. Pós composition — 100% stacked by fiscal year ───
# Completed years = full year; current year = YTD to today.
chart_pre_pos <- function(lic, country) {
  today          <- Sys.Date()
  cur_fy         <- current_fy(country)
  fy_start       <- if (country == "south_africa") "-04-01" else "-01-01"
  today_fy_start <- as.Date(paste0(cur_fy, fy_start))
  today_doy      <- as.numeric(today - today_fy_start)

  df_raw <- classify_pre_pos(lic, country) |> filter(!is.na(PrePos))

  past <- df_raw |>
    filter(FY < cur_fy) |>
    group_by(FY, PrePos) |>
    summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")

  curr <- df_raw |>
    filter(FY == cur_fy) |>
    mutate(auction_doy = as.numeric(Fecha - today_fy_start)) |>
    filter(auction_doy >= 0, auction_doy <= today_doy) |>
    group_by(FY, PrePos) |>
    summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop")

  df <- bind_rows(past, curr) |>
    group_by(FY) |>
    mutate(Total = sum(Monto), Pct = Monto / Total * 100) |>
    ungroup() |>
    filter(!is.na(Total), Total > 0) |>
    # Reverse factor levels so ggplotly puts Pré at the bottom, Pós on top
    mutate(
      FY_label = factor(if_else(FY == cur_fy,
                                paste0(FY, " (YTD)"), as.character(FY))),
      PrePos   = factor(PrePos, levels = c("Pós", "Pré"))
    )

  if (nrow(df) == 0) return(plotly_empty(type = "bar"))

  p <- ggplot(df, aes(
    x    = FY_label,
    y    = Pct,
    fill = PrePos,
    text = paste0(FY, "\n", PrePos, ": ", round(Pct, 1), "%")
  )) +
    geom_col(position = "stack", width = 0.65) +
    scale_fill_manual(values = c("Pré" = CLR_PRE, "Pós" = CLR_POS)) +
    scale_y_continuous(
      labels = label_percent(scale = 1, accuracy = 1),
      limits = c(0, 101),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(subtitle = paste0(
      "Anos completos + ", cur_fy, " YTD até ", format(today, "%d %b")
    )) +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.25), margin = list(b = 80))
}

# ── Overview: Pré vs. Pós — all 4 countries, current YTD ────
chart_pre_pos_overview <- function(chile_lic, mexico_lic, sa_lic, colombia_lic) {
  today <- Sys.Date()

  get_ytd_prepos <- function(lic, country) {
    cur_fy         <- current_fy(country)
    fy_start       <- if (country == "south_africa") "-04-01" else "-01-01"
    today_fy_start <- as.Date(paste0(cur_fy, fy_start))
    today_doy      <- as.numeric(today - today_fy_start)

    classify_pre_pos(lic, country) |>
      filter(!is.na(PrePos), FY == cur_fy) |>
      mutate(auction_doy = as.numeric(Fecha - today_fy_start)) |>
      filter(auction_doy >= 0, auction_doy <= today_doy) |>
      group_by(PrePos) |>
      summarise(Monto = sum(Monto, na.rm = TRUE), .groups = "drop") |>
      mutate(Country = country)
  }

  df <- bind_rows(
    get_ytd_prepos(chile_lic,    "chile"),
    get_ytd_prepos(mexico_lic,   "mexico"),
    get_ytd_prepos(sa_lic,       "south_africa"),
    get_ytd_prepos(colombia_lic, "colombia")
  ) |>
    group_by(Country) |>
    mutate(Total = sum(Monto), Pct = Monto / Total * 100) |>
    ungroup() |>
    mutate(Country_label = factor(
      case_when(
        Country == "chile"        ~ "Chile",
        Country == "mexico"       ~ "México",
        Country == "south_africa" ~ "África do Sul",
        Country == "colombia"     ~ "Colômbia"
      ),
      levels = c("Chile", "México", "África do Sul", "Colômbia")
    ))

  if (nrow(df) == 0) return(plotly_empty(type = "bar"))

  all_ctry <- data.frame(Country_label = factor(
    c("Chile", "México", "África do Sul", "Colômbia"),
    levels = c("Chile", "México", "África do Sul", "Colômbia")
  ))

  pre_df <- left_join(all_ctry,
    df |> filter(PrePos == "Pré") |> select(Country_label, Pct),
    by = "Country_label") |>
    mutate(Pct = if_else(is.na(Pct), 0, Pct))

  pos_df <- left_join(all_ctry,
    df |> filter(PrePos == "Pós") |> select(Country_label, Pct),
    by = "Country_label") |>
    mutate(Pct = if_else(is.na(Pct), 0, Pct))

  plot_ly() |>
    add_bars(
      data            = pre_df,
      x               = ~Country_label,
      y               = ~Pct,
      name            = "Pré",
      marker          = list(color = CLR_PRE),
      text            = ~ifelse(Pct > 2, paste0(round(Pct, 1), "%"), ""),
      textposition    = "inside",
      insidetextanchor = "middle",
      hovertemplate   = ~paste0("<b>", Country_label, "</b><br>Pré: ",
                                round(Pct, 1), "%<extra></extra>")
    ) |>
    add_bars(
      data            = pos_df,
      x               = ~Country_label,
      y               = ~Pct,
      name            = "Pós",
      marker          = list(color = CLR_POS),
      text            = ~ifelse(Pct > 2, paste0(round(Pct, 1), "%"), ""),
      textposition    = "inside",
      insidetextanchor = "middle",
      hovertemplate   = ~paste0("<b>", Country_label, "</b><br>Pós: ",
                                round(Pct, 1), "%<extra></extra>")
    ) |>
    layout(
      barmode = "stack",
      yaxis   = list(title = "", tickformat = ".0f", ticksuffix = "%",
                     range = c(0, 102), showgrid = TRUE),
      xaxis   = list(title = ""),
      legend  = list(orientation = "h", y = -0.12),
      margin  = list(t = 40, b = 60, l = 50, r = 20),
      annotations = list(list(
        text      = paste0("YTD até ", format(today, "%d %b %Y")),
        x = 0.5, y = 1.06, xref = "paper", yref = "paper",
        showarrow = FALSE, xanchor = "center",
        font = list(size = 11, color = "grey50")
      ))
    )
}

# ── Debt as % of GDP — annual bar chart ─────────────────────
# debt: pre-normalised data frame from load_* (Debt_tri, X_label, optionally Year/FY_start)
# gdp:  country gdp data frame from load_* (PIB_tri, Periodo or FY)
chart_debt_pct_gdp <- function(debt, gdp, country) {
  debt_lbl <- if (country == "mexico") "Deuda Neta" else if (country == "chile") "Deuda Bruta (USD)" else "Deuda Bruta"

  df <- if (country == "south_africa") {
    debt |>
      left_join(gdp |> select(FY, PIB_tri), by = c("FY_start" = "FY")) |>
      filter(!is.na(PIB_tri)) |>
      mutate(Int_pct = Int_tri / PIB_tri * 100,
             Ext_pct = Ext_tri / PIB_tri * 100,
             Tot_pct = Debt_tri / PIB_tri * 100)
  } else if (country == "chile") {
    debt |>
      left_join(gdp |> select(FY, PIB_tri), by = c("Year" = "FY")) |>
      filter(!is.na(PIB_tri)) |>
      mutate(Int_pct = Int_tri / PIB_tri * 100,
             Ext_pct = Ext_tri / PIB_tri * 100,
             Tot_pct = Debt_tri / PIB_tri * 100)
  } else {
    gdp_pts <- gdp |> filter(!is.na(PIB_tri)) |> arrange(Periodo)
    debt |>
      mutate(PIB_tri = sapply(as.Date(paste0(Year, "-12-31")), function(d) {
        cands <- gdp_pts$PIB_tri[gdp_pts$Periodo <= d]
        if (length(cands) == 0) NA_real_ else tail(cands, 1)
      })) |>
      filter(!is.na(PIB_tri)) |>
      mutate(Int_pct = Int_tri / PIB_tri * 100,
             Ext_pct = Ext_tri / PIB_tri * 100,
             Tot_pct = Debt_tri / PIB_tri * 100)
  }

  if (nrow(df) == 0) return(plotly_empty(type = "bar"))

  month_suffix <- if (country == "south_africa") " (Mar)" else if (country == "chile") " (Oct)" else " (Dec)"
  df <- df |> mutate(X_label = paste0(X_label, month_suffix))

  x_levels <- unique(df$X_label)

  df_long <- df |>
    select(X_label, Int_pct, Ext_pct, Tot_pct) |>
    pivot_longer(cols = c(Int_pct, Ext_pct),
                 names_to = "Tipo", values_to = "Pct") |>
    mutate(
      X_label = factor(X_label, levels = x_levels),
      # Use display names as factor levels so clean_legend shows them directly
      # Reverse order so ggplotly puts Interna at bottom, Externa on top
      Tipo    = factor(
        if_else(Tipo == "Int_pct", "Interna", "Externa"),
        levels = c("Externa", "Interna")
      )
    )

  totals <- df |> mutate(X_label = factor(X_label, levels = x_levels))

  p <- ggplot(df_long, aes(
    x    = X_label,
    y    = Pct,
    fill = Tipo,
    text = paste0(X_label, "\n", Tipo, ": ", round(Pct, 1), "% do PIB")
  )) +
    geom_col(position = "stack", width = 0.7) +
    geom_point(
      data = totals,
      aes(x = X_label, y = Tot_pct, colour = "Total",
          text = paste0(X_label, "\nTotal: ", round(Tot_pct, 1), "% do PIB")),
      size = 3, inherit.aes = FALSE
    ) +
    scale_fill_manual(values = c("Interna" = "#4472C4", "Externa" = "#ED7D31")) +
    scale_colour_manual(values = c(Total = CLR_TOTAL)) +
    scale_y_continuous(
      labels = label_percent(scale = 1, accuracy = 1),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(subtitle = paste0(debt_lbl, " % PIB — série histórica")) +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.40), margin = list(b = 160))
}

# ── Debt composition — internal vs external 100% stacked ────
chart_debt_composition <- function(debt, country) {
  debt_lbl <- if (country == "mexico") "Deuda Neta" else if (country == "chile") "Deuda Bruta (USD)" else "Deuda Bruta"

  month_suffix <- if (country == "south_africa") " (Mar)" else if (country == "chile") " (Oct)" else " (Dec)"
  debt <- debt |> mutate(X_label = paste0(X_label, month_suffix))

  df_long <- debt |>
    mutate(
      Internal = Int_tri / Debt_tri * 100,
      External = Ext_tri / Debt_tri * 100
    ) |>
    select(X_label, Internal, External) |>
    pivot_longer(cols = c(Internal, External),
                 names_to = "Tipo", values_to = "Pct") |>
    mutate(
      X_label = factor(X_label, levels = unique(debt$X_label)),
      # Reverse factor for ggplotly: Internal ends up at bottom, External on top
      Tipo    = factor(Tipo, levels = c("External", "Internal"))
    )

  if (nrow(df_long) == 0) return(plotly_empty(type = "bar"))

  p <- ggplot(df_long, aes(
    x    = X_label,
    y    = Pct,
    fill = Tipo,
    text = paste0(X_label, "\n", Tipo, ": ", round(Pct, 1), "%")
  )) +
    geom_col(position = "stack", width = 0.7) +
    scale_fill_manual(
      values = c("Internal" = "#4472C4", "External" = "#ED7D31"),
      labels = c("Internal" = "Interna",  "External" = "Externa")
    ) +
    scale_y_continuous(
      labels = label_percent(scale = 1, accuracy = 1),
      limits = c(0, 101),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(subtitle = paste0(debt_lbl, " — Interna vs. Externa")) +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.40), margin = list(b = 160))
}

# ── Government bond holdings by investor type ────────────────
# SA:  monthly data (decimals 0-1); use Dec for completed years, latest for current
# MX:  weekly data (millions MXN); aggregate to monthly last obs, convert to % of Total
chart_holdings <- function(holdings, country) {
  cur_yr <- year(Sys.Date())

  if (country == "south_africa") {
    # bottom → top stack order
    plot_order <- c("Other", "Other_Financial", "Insurers", "Banks", "Local_Pension_Funds", "Non_Residents")
    seg_labels <- c(
      "Other"               = "Outros",
      "Other_Financial"     = "Outros Financeiros",
      "Insurers"            = "Seguradoras",
      "Banks"               = "Bancos",
      "Local_Pension_Funds" = "Pensões Locais",
      "Non_Residents"       = "Não Residentes"
    )
    seg_colors <- c(
      "Other"               = "#AAAAAA",
      "Other_Financial"     = "#5B9BD5",
      "Insurers"            = "#FFC000",
      "Banks"               = "#90C987",
      "Local_Pension_Funds" = "#4472C4",
      "Non_Residents"       = "#E84855"
    )

    completed <- holdings |>
      filter(year(Periodo) >= 2020, year(Periodo) < cur_yr, month(Periodo) == 12) |>
      mutate(label = paste0(year(Periodo), " (Dec)"))

    current_obs <- holdings |>
      filter(year(Periodo) == cur_yr) |>
      slice_max(Periodo, n = 1) |>
      mutate(label = paste0(cur_yr, " (", format(Periodo, "%B"), ")"))

    plot_data <- bind_rows(completed, current_obs) |>
      mutate(across(all_of(plot_order), ~ . * 100)) |>
      select(label, all_of(plot_order))

  } else { # mexico
    plot_order <- c("Otros", "Sector_Bancario", "Sociedades_Inversion", "Siefores", "Extranjeros")
    seg_labels <- c(
      "Otros"                = "Outros",
      "Sector_Bancario"      = "Bancos",
      "Sociedades_Inversion" = "Soc. de Inversión",
      "Siefores"             = "Siefores",
      "Extranjeros"          = "Estrangeiros"
    )
    seg_colors <- c(
      "Otros"                = "#AAAAAA",
      "Sector_Bancario"      = "#90C987",
      "Sociedades_Inversion" = "#5B9BD5",
      "Siefores"             = "#4472C4",
      "Extranjeros"          = "#E84855"
    )

    holdings_monthly <- holdings |>
      mutate(YearMonth = floor_date(Periodo, "month")) |>
      group_by(YearMonth) |>
      slice_max(Periodo, n = 1) |>
      ungroup() |>
      mutate(
        Extranjeros          = Residentes_Extranjero  / Total * 100,
        Siefores             = Siefores               / Total * 100,
        Sociedades_Inversion = Sociedades_Inversion   / Total * 100,
        Sector_Bancario      = Sector_Bancario        / Total * 100,
        Otros                = (Aseguradoras + Otros_Residentes_Pais +
                                Reportos_Banxico + Garantias_Banxico +
                                Valores_Adquiridos_Banxico) / Total * 100
      )

    completed <- holdings_monthly |>
      filter(year(YearMonth) >= 2020, year(YearMonth) < cur_yr, month(YearMonth) == 12) |>
      mutate(label = paste0(year(YearMonth), " (Dec)"))

    current_obs <- holdings_monthly |>
      filter(year(YearMonth) == cur_yr) |>
      slice_max(YearMonth, n = 1) |>
      mutate(label = paste0(cur_yr, " (", format(YearMonth, "%B"), ")"))

    plot_data <- bind_rows(completed, current_obs) |>
      select(label, all_of(plot_order))
  }

  if (nrow(plot_data) == 0) return(plotly_placeholder())

  x_labels <- unique(plot_data$label)

  p <- plot_ly(plot_data, x = ~factor(label, levels = x_labels))
  for (seg in plot_order) {
    pct_vals <- plot_data[[seg]]
    p <- add_bars(
      p,
      y                = pct_vals,
      name             = seg_labels[[seg]],
      marker           = list(color = seg_colors[[seg]]),
      text             = ifelse(pct_vals > 3, paste0(round(pct_vals, 0), "%"), ""),
      textposition     = "inside",
      insidetextanchor = "middle",
      hovertemplate    = paste0(seg_labels[[seg]], ": %{y:.1f}%<extra></extra>")
    )
  }

  p |>
    layout(
      barmode = "stack",
      xaxis   = list(title = "", categoryorder = "array", categoryarray = x_labels),
      yaxis   = list(title = "", ticksuffix = "%", range = c(0, 101),
                     tickformat = ".0f"),
      legend  = list(orientation = "h", y = -0.25),
      margin  = list(b = 80)
    )
}

# ── Colombia: TES holdings by sector (single-period snapshot) ─
chart_colombia_holdings <- function(holdings) {
  df <- holdings |>
    mutate(Sector = case_when(
      grepl("Pensiones|Cesantias",    Tenedor, ignore.case = TRUE) ~ "Fondos de Pensiones",
      grepl("Capital Extranjero|Extranjero", Tenedor, ignore.case = TRUE) ~ "Extranjeros",
      grepl("Bancos Comerciales",     Tenedor, ignore.case = TRUE) ~ "Bancos",
      grepl("Seguros|Capitaliz",      Tenedor, ignore.case = TRUE) ~ "Seguros",
      grepl("Banco de la Rep",        Tenedor, ignore.case = TRUE) ~ "Banco de la República",
      TRUE                                                         ~ "Otros"
    )) |>
    group_by(Sector) |>
    summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") |>
    mutate(Pct = Total / sum(Total) * 100) |>
    arrange(desc(Pct))

  if (nrow(df) == 0) return(plotly_placeholder())

  seg_colors <- c(
    "Fondos de Pensiones"  = "#4472C4",
    "Extranjeros"          = "#E84855",
    "Bancos"               = "#90C987",
    "Seguros"              = "#FFC000",
    "Banco de la República"= "#7030A0",
    "Otros"                = "#AAAAAA"
  )

  p <- plot_ly()
  for (i in seq_len(nrow(df))) {
    seg <- df$Sector[i]
    pct <- df$Pct[i]
    p <- add_bars(
      p,
      x                = pct,
      y                = list("Jun 2026"),
      orientation      = "h",
      name             = seg,
      marker           = list(color = seg_colors[seg]),
      text             = ifelse(pct > 3, paste0(seg, "\n", round(pct, 1), "%"), ""),
      textposition     = "inside",
      insidetextanchor = "middle",
      hovertemplate    = paste0(seg, ": %{x:.1f}%<extra></extra>")
    )
  }

  p |>
    layout(
      barmode  = "stack",
      title    = list(text = "Tenedores de TES — Jun 2026", font = list(size = 13)),
      xaxis    = list(title = "", ticksuffix = "%", range = c(0, 101), tickformat = ".0f"),
      yaxis    = list(title = "", showticklabels = FALSE),
      legend   = list(orientation = "h", y = -0.15),
      margin   = list(b = 80, t = 50)
    )
}

# ── Colombia: maturity profile of domestic TES (Clase B) ──────
chart_colombia_maturity <- function(maturity) {
  if (nrow(maturity) == 0) return(plotly_placeholder())

  df <- maturity |>
    group_by(Ano_Vencimiento) |>
    slice_max(Total, n = 1) |>
    ungroup() |>
    mutate(
      COP_tri   = TES_COP / 1e6,
      UVR_tri   = TES_UVR / 1e6,
      Total_tri = Total   / 1e6,
      Pct_lbl   = paste0(round(Total / sum(Total) * 100, 1), "%"),
      Year      = factor(Ano_Vencimiento)
    )

  # Reversed factor levels so ggplotly puts COP at bottom, UVR on top
  df_long <- df |>
    select(Year, Ano_Vencimiento, COP_tri, UVR_tri) |>
    pivot_longer(c(COP_tri, UVR_tri), names_to = "Tipo", values_to = "Valor") |>
    mutate(Tipo = factor(Tipo, levels = c("UVR_tri", "COP_tri")))

  p <- ggplot(df_long, aes(
    x    = Year,
    y    = Valor,
    fill = Tipo,
    text = paste0(Ano_Vencimiento, "\n", Tipo, ": ", round(Valor, 2), " COP tri")
  )) +
    geom_col(position = "stack", width = 0.7) +
    geom_text(
      data        = df,
      aes(x = Year, y = Total_tri, label = Pct_lbl),
      inherit.aes = FALSE,
      vjust       = -0.35,
      size        = 2.3,
      color       = "grey30"
    ) +
    scale_fill_manual(
      values = c("COP_tri" = "#4472C4", "UVR_tri" = "#FFC000"),
      labels = c("COP_tri" = "TES COP", "UVR_tri" = "TES UVR")
    ) +
    scale_y_continuous(
      labels = label_number(suffix = " COP tri", accuracy = 1),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(subtitle = "Perfil de Vencimento — TES Clase B (Jun 2026)") +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.25), margin = list(b = 100))
}

# ── Mexico: internal debt maturity profile ────────────────────
chart_mexico_maturity <- function(maturity) {
  if (nrow(maturity) == 0) return(plotly_placeholder())

  # Visual order bottom → top: fixed first, then short-term, then floating
  visual_order <- c("Bonos Tasa Fija Bono M", "Udibonos", "Cetes",
                    "Bondes F", "Bondes G", "Bondes D")

  inst_colors <- c(
    "Bonos Tasa Fija Bono M" = "#4472C4",
    "Udibonos"               = "#5B9BD5",
    "Cetes"                  = "#90C987",
    "Bondes F"               = "#FFC000",
    "Bondes G"               = "#ED7D31",
    "Bondes D"               = "#E84855"
  )

  totals <- maturity |>
    filter(Instrumento == "Total") |>
    mutate(Total_tri = Monto / 1e6,
           Ano       = factor(Ano))

  df <- maturity |>
    filter(Instrumento != "Total") |>
    mutate(
      Monto_tri   = Monto / 1e6,
      Ano         = factor(Ano),
      Instrumento = factor(Instrumento, levels = rev(visual_order))
    )

  p <- ggplot(df, aes(
    x    = Ano,
    y    = Monto_tri,
    fill = Instrumento,
    text = paste0(Ano, "\n", Instrumento, ": ", round(Monto_tri, 2), " MXN tri")
  )) +
    geom_col(position = "stack", width = 0.65) +
    geom_text(
      data        = totals,
      aes(x = Ano, y = Total_tri,
          label = paste0(round(Total_tri, 2), " tri")),
      inherit.aes = FALSE,
      vjust       = -0.35,
      size        = 3,
      color       = "grey30"
    ) +
    scale_fill_manual(values = inst_colors) +
    scale_y_continuous(
      labels = label_number(suffix = " MXN tri", accuracy = 0.1),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(subtitle = "Perfil de Vencimentos — Deuda Interna (Mar 2026)") +
    PLOT_THEME

  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.25), margin = list(b = 80))
}

# ── Mexico: weighted average maturity of internal debt ────────
chart_mexico_avg_maturity <- function(avg_maturity) {
  if (nrow(avg_maturity) == 0) return(plotly_placeholder())

  cur_yr <- year(Sys.Date())

  completed <- avg_maturity |>
    filter(year(Periodo) >= 2010, year(Periodo) < cur_yr, month(Periodo) == 12) |>
    mutate(label = as.character(year(Periodo)))

  current <- avg_maturity |>
    filter(year(Periodo) == cur_yr) |>
    slice_max(Periodo, n = 1) |>
    mutate(label = paste0(cur_yr, " (", format(Periodo, "%b"), ")"))

  df <- bind_rows(completed, current) |>
    mutate(label = factor(label, levels = label))

  p <- ggplot(df, aes(x = label, y = Anos,
                      text = paste0(label, ": ", round(Anos, 1), " anos"))) +
    geom_col(fill = "#4472C4", width = 0.7) +
    geom_text(aes(label = round(Anos, 1)), vjust = -0.5, size = 2.8, color = "#333333") +
    scale_y_continuous(
      labels = label_number(suffix = " anos", accuracy = 0.1),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      subtitle = "Valores Governamentais (anos ao fechamento do período)",
      caption  = "Fonte: Banxico (SG231)"
    ) +
    PLOT_THEME +
    theme(plot.caption = element_text(hjust = 0, size = 7, color = "#888888"))

  ggplotly(p, tooltip = "text") |>
    layout(margin = list(b = 60))
}

# ── South Africa: domestic government bond maturity profile ───
chart_sa_maturity <- function(maturity) {
  if (nrow(maturity) == 0) return(plotly_placeholder())

  df <- maturity |>
    arrange(Start_Year) |>
    mutate(
      ZAR_tri  = Nominal_ZAR / 1e12,
      Ano      = factor(Ano_Fiscal, levels = Ano_Fiscal),
      lbl      = paste0(round(ZAR_tri, 1))
    )

  p <- ggplot(df, aes(x = Ano, y = ZAR_tri,
                      text = paste0(Ano_Fiscal, ": ", round(ZAR_tri, 1), " ZAR tri"))) +
    geom_col(fill = "#4472C4", width = 0.7) +
    geom_text(aes(label = lbl), vjust = -0.5, size = 2.8, color = "#333333") +
    scale_y_continuous(
      labels = label_number(suffix = " ZAR tri", accuracy = 0.1),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(subtitle = "Inclui títulos de taxa fixa, indexados à inflação e FRNs") +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

  ggplotly(p, tooltip = "text") |>
    layout(margin = list(b = 80))
}

# ── South Africa: weighted average maturity of fixed-rate bonds ─
chart_sa_avg_maturity <- function(avg_maturity) {
  if (nrow(avg_maturity) == 0) return(plotly_placeholder())

  df <- avg_maturity |>
    mutate(Fiscal_Year = factor(Fiscal_Year, levels = Fiscal_Year))

  p <- ggplot(df, aes(x = Fiscal_Year, y = WAM_Anos,
                      text = paste0(Fiscal_Year, ": ", round(WAM_Anos, 1), " anos"))) +
    geom_col(fill = "#4472C4", width = 0.7) +
    geom_text(aes(label = round(WAM_Anos, 1)), vjust = -0.5, size = 2.8, color = "#333333") +
    scale_y_continuous(
      labels = label_number(suffix = " anos", accuracy = 0.1),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      subtitle = "Anos ao fechamento do período fiscal",
      caption  = "Fonte: National Treasury — Budget Review 2026, Capítulo 7, Figura 7.2"
    ) +
    PLOT_THEME +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      plot.caption = element_text(hjust = 0, size = 7, color = "#888888")
    )

  ggplotly(p, tooltip = "text") |>
    layout(margin = list(b = 80))
}

chart_vs_target <- function(lic, targets, country, ccy_label, year_label) {
  today        <- Sys.Date()
  cur_fy       <- current_fy(country)
  fy_start_str <- if (country == "south_africa") "-04-01" else "-01-01"
  fy_start     <- as.Date(paste0(cur_fy, fy_start_str))
  today_doy    <- as.numeric(today - fy_start)

  cur_lic <- lic |>
    mutate(auction_doy = as.numeric(Fecha - fy_start)) |>
    filter(auction_doy >= 0, auction_doy <= today_doy)

  if (country == "mexico") {
    cat_lic <- cur_lic |>
      mutate(Categoria = case_when(
        Instrumento == "Cetes"                            ~ "Cetes",
        grepl("Bondes",  Instrumento, ignore.case = TRUE) ~ "Bondes",
        grepl("Udibono", Instrumento, ignore.case = TRUE) ~ "Udibonos",
        grepl("Bono",    Instrumento, ignore.case = TRUE) ~ "Bonos",
        TRUE ~ NA_character_
      )) |>
      filter(!is.na(Categoria)) |>
      group_by(Categoria) |>
      summarise(Emitido = sum(Monto, na.rm = TRUE), .groups = "drop")
    emitted <- bind_rows(
      data.frame(Categoria = "Total", Emitido = sum(cur_lic$Monto, na.rm = TRUE)),
      cat_lic
    )

  } else if (country == "south_africa") {
    cat_lic <- cur_lic |>
      mutate(Categoria = if_else(Tipo_Bono == "Treasury Bill", "Treasury Bills", "Long-term")) |>
      group_by(Categoria) |>
      summarise(Emitido = sum(Monto, na.rm = TRUE), .groups = "drop")
    emitted <- bind_rows(
      data.frame(Categoria = "Total", Emitido = sum(cur_lic$Monto, na.rm = TRUE)),
      cat_lic
    )

  } else {
    # Chile: same emitted value shown against both targets
    total_emitted <- sum(cur_lic$Monto, na.rm = TRUE)
    emitted <- data.frame(
      Categoria = c("Meta Original", "Meta Ampliada"),
      Emitido   = c(total_emitted, total_emitted)
    )
  }

  df <- merge(targets, emitted, by = "Categoria", all.x = TRUE)
  df$Emitido  <- ifelse(is.na(df$Emitido), 0, df$Emitido)
  df$Restante <- pmax(df$Target - df$Emitido, 0)
  df$Categoria <- factor(df$Categoria, levels = targets$Categoria)

  rows_emit        <- df
  rows_emit$Status <- "Emitido"
  rows_emit$Valor  <- df$Emitido
  rows_emit$text   <- paste0(
    df$Categoria,
    "\nEmitido: ", round(df$Emitido, 3), " ", ccy_label,
    "\nMeta: ",    round(df$Target,  3), " ", ccy_label,
    " (", round(df$Emitido / df$Target * 100, 1), "%)"
  )

  rows_rest        <- df
  rows_rest$Status <- "Restante"
  rows_rest$Valor  <- df$Restante
  rows_rest$text   <- paste0(
    df$Categoria,
    "\nRestante: ", round(df$Restante, 3), " ", ccy_label,
    "\nMeta: ",     round(df$Target,   3), " ", ccy_label
  )

  plot_df        <- rbind(rows_emit, rows_rest)
  plot_df$Status <- factor(plot_df$Status, levels = c("Restante", "Emitido"))

  p <- ggplot(plot_df, aes(x = Categoria, y = Valor, fill = Status, text = text)) +
    geom_col(position = "stack", width = 0.6) +
    scale_fill_manual(
      values = c(Emitido = CLR_TOTAL, Restante = "#E0E0E0"),
      labels = c(Emitido = "Emitido", Restante = "Meta restante")
    ) +
    scale_y_continuous(
      labels = label_number(suffix = paste0(" ", ccy_label), accuracy = 0.01),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(subtitle = paste0("Meta anual ", year_label)) +
    PLOT_THEME +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  clean_legend(ggplotly(p, tooltip = "text")) |>
    layout(legend = list(orientation = "h", y = -0.25), margin = list(b = 80))
}

# ============================================================
# UI
# ============================================================

ui <- page_navbar(
  title   = "Sovereign Bond Issuance Tracker",
  theme   = bs_theme(
    bootswatch   = "flatly",
    primary      = "#2E4057",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  bg       = "#2E4057",
  inverse  = TRUE,
  fillable = FALSE,

  # ── Chile ──────────────────────────────────────────────────
  nav_panel(
    "Chile",
    card(
      card_header("Emissões Mensais"),
      plotlyOutput("cl_monthly", height = "320px")
    ),
    card(
      card_header("Emissões Mensais % PIB"),
      plotlyOutput("cl_monthly_pct", height = "320px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Caixa do Tesouro — Total (Sazonal)"),
           plotlyOutput("cl_tsy_seas",       height = "320px")),
      card(card_header("Caixa do Tesouro — Total"),
           plotlyOutput("cl_tsy_ts",         height = "320px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Caixa do Tesouro — Pesos (Sazonal)"),
           plotlyOutput("cl_tsy_seas_pesos", height = "320px")),
      card(card_header("Caixa do Tesouro — Pesos"),
           plotlyOutput("cl_tsy_ts_pesos",   height = "320px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Caixa do Tesouro — Dólares (Sazonal)"),
           plotlyOutput("cl_tsy_seas_dolar", height = "320px")),
      card(card_header("Caixa do Tesouro — Dólares"),
           plotlyOutput("cl_tsy_ts_dolar",   height = "320px"))
    ),
    card(
      card_header("Emissões YTD"),
      plotlyOutput("cl_ytd", height = "320px")
    ),
    card(
      card_header("Emissões YTD em % do PIB"),
      plotlyOutput("cl_ytd_pct_gdp", height = "340px")
    ),
    card(
      card_header("Run Rate — Emissão Acumulada"),
      plotlyOutput("cl_runrate", height = "340px")
    ),
    card(
      card_header("Run Rate — Emissão Acumulada % PIB"),
      plotlyOutput("cl_runrate_pct", height = "340px")
    ),
    card(
      card_header("Emissões vs. Meta 2026"),
      plotlyOutput("cl_vs_target", height = "320px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Composição — Instrumento"),
           plotlyOutput("cl_composition",     height = "360px")),
      card(card_header("Composição — Moeda"),
           plotlyOutput("cl_composition_ccy", height = "360px"))
    ),
    card(
      card_header("Pré vs. Pós"),
      plotlyOutput("cl_pre_pos", height = "340px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Deuda Bruta — Gobierno Central (USD) % do PIB"),
           plotlyOutput("cl_debt_pct",  height = "340px")),
      card(card_header("Composição — Interna / Externa (USD)"),
           plotlyOutput("cl_debt_comp", height = "340px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Amortizações de Bônus em 2026 (fechamento 2025)"),
        div(
          style = "padding: 20px; text-align: center;",
          div(style = "font-size: 2rem; font-weight: 700; color: #1e3a5f;", "US$ 7.211 MM"),
          div(style = "font-size: 0.8rem; color: #888; margin-top: 8px;",
              "Fonte: Ministerio de Hacienda de Chile")
        )
      ),
      card(
        card_header("Prazo Médio da Dívida"),
        div(
          style = "padding: 20px; text-align: center;",
          div(style = "font-size: 2rem; font-weight: 700; color: #1e3a5f;", "10.4 anos"),
          div(style = "font-size: 0.9rem; color: #555; margin-top: 6px;", "Fechamento 2025"),
          div(style = "font-size: 0.8rem; color: #888; margin-top: 8px;",
              "Fonte: Ministerio de Hacienda de Chile")
        )
      )
    )
  ),

  # ── Mexico ─────────────────────────────────────────────────
  nav_panel(
    "México",
    card(
      card_header("Emissões Mensais"),
      plotlyOutput("mx_monthly", height = "320px")
    ),
    card(
      card_header("Emissões Mensais % PIB"),
      plotlyOutput("mx_monthly_pct", height = "320px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Caixa do Tesouro (Sazonal)"),
           plotlyOutput("mx_tsy_seas", height = "320px")),
      card(card_header("Caixa do Tesouro"),
           plotlyOutput("mx_tsy_ts",   height = "320px"))
    ),
    card(
      card_header("Emissões YTD"),
      plotlyOutput("mx_ytd", height = "320px")
    ),
    card(
      card_header("Emissões YTD em % do PIB"),
      plotlyOutput("mx_ytd_pct_gdp", height = "340px")
    ),
    card(
      card_header("Run Rate — Emissão Acumulada"),
      plotlyOutput("mx_runrate", height = "340px")
    ),
    card(
      card_header("Run Rate — Emissão Acumulada % PIB"),
      plotlyOutput("mx_runrate_pct", height = "340px")
    ),
    card(
      card_header("Emissões vs. Meta 2026"),
      plotlyOutput("mx_vs_target", height = "320px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Composição — Instrumento"),
           plotlyOutput("mx_composition",     height = "360px")),
      card(card_header("Composição — Moeda"),
           plotlyOutput("mx_composition_ccy", height = "360px"))
    ),
    card(
      card_header("Pré vs. Pós"),
      plotlyOutput("mx_pre_pos", height = "340px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Deuda Neta % do PIB"),
           plotlyOutput("mx_debt_pct",  height = "340px")),
      card(card_header("Composição — Interna / Externa"),
           plotlyOutput("mx_debt_comp", height = "340px"))
    ),
    card(
      card_header("Detentores de Títulos do Governo"),
      plotlyOutput("mx_holdings", height = "380px")
    ),
    card(
      card_header("Perfil de Vencimentos — Deuda Interna (Mar 2026)"),
      plotlyOutput("mx_maturity", height = "380px")
    ),
    card(
      card_header("Prazo Médio Ponderado da Dívida Interna — Mexico"),
      plotlyOutput("mx_avg_maturity", height = "380px")
    )
  ),

  # ── South Africa ───────────────────────────────────────────
  nav_panel(
    "África do Sul",
    card(
      card_header("Emissões Mensais"),
      plotlyOutput("sa_monthly", height = "320px")
    ),
    card(
      card_header("Emissões Mensais % PIB"),
      plotlyOutput("sa_monthly_pct", height = "320px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Caixa do Tesouro (Sazonal)"),
           plotlyOutput("sa_tsy_seas", height = "320px")),
      card(card_header("Caixa do Tesouro"),
           plotlyOutput("sa_tsy_ts",   height = "320px"))
    ),
    card(
      card_header("Emissões YTD"),
      plotlyOutput("sa_ytd", height = "320px")
    ),
    card(
      card_header("Emissões YTD em % do PIB"),
      plotlyOutput("sa_ytd_pct_gdp", height = "340px")
    ),
    card(
      card_header("Run Rate — Emissão Acumulada"),
      plotlyOutput("sa_runrate", height = "340px")
    ),
    card(
      card_header("Run Rate — Emissão Acumulada % PIB"),
      plotlyOutput("sa_runrate_pct", height = "340px")
    ),
    card(
      card_header("Emissões vs. Meta 2026/27"),
      plotlyOutput("sa_vs_target", height = "320px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Composição — Instrumento"),
           plotlyOutput("sa_composition",     height = "360px")),
      card(card_header("Composição — Moeda"),
           plotlyOutput("sa_composition_ccy", height = "360px"))
    ),
    card(
      card_header("Pré vs. Pós"),
      plotlyOutput("sa_pre_pos", height = "340px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Deuda Bruta % do PIB"),
           plotlyOutput("sa_debt_pct",  height = "340px")),
      card(card_header("Composição — Domestic / Foreign"),
           plotlyOutput("sa_debt_comp", height = "340px"))
    ),
    card(
      card_header("Detentores de Títulos do Governo"),
      plotlyOutput("sa_holdings", height = "380px")
    ),
    card(
      card_header("Perfil de Vencimentos — Títulos Domésticos (Abr 2026)"),
      plotlyOutput("sa_maturity", height = "380px")
    ),
    card(
      card_header("Prazo Médio Ponderado de Títulos de Taxa Fixa"),
      plotlyOutput("sa_avg_maturity", height = "380px")
    )
  ),

  # ── Colombia ───────────────────────────────────────────────
  nav_panel(
    "Colômbia",
    card(
      card_header("Emissões Mensais"),
      plotlyOutput("co_monthly", height = "320px")
    ),
    card(
      card_header("Emissões Mensais % PIB"),
      plotlyOutput("co_monthly_pct", height = "320px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Caixa do Tesouro (Sazonal)"),
           plotlyOutput("co_tsy_seas", height = "320px")),
      card(card_header("Caixa do Tesouro"),
           plotlyOutput("co_tsy_ts",   height = "320px"))
    ),
    card(
      card_header("Emissões YTD"),
      plotlyOutput("co_ytd", height = "320px")
    ),
    card(
      card_header("Emissões YTD em % do PIB"),
      plotlyOutput("co_ytd_pct_gdp", height = "320px")
    ),
    card(
      card_header("Run Rate — Emissão Acumulada"),
      plotlyOutput("co_runrate", height = "340px")
    ),
    card(
      card_header("Run Rate — Emissão Acumulada % PIB"),
      plotlyOutput("co_runrate_pct", height = "340px")
    ),
    card(
      card_header("Emissões vs. Meta 2026"),
      plotlyOutput("co_vs_target", height = "320px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Composição — Instrumento"),
           plotlyOutput("co_composition",     height = "360px")),
      card(card_header("Composição — Moeda"),
           plotlyOutput("co_composition_ccy", height = "360px"))
    ),
    card(
      card_header("Pré vs. Pós"),
      plotlyOutput("co_pre_pos", height = "340px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Deuda Bruta % do PIB"),
           plotlyOutput("co_debt_pct",  height = "340px")),
      card(card_header("Composição — Interna / Externa"),
           plotlyOutput("co_debt_comp", height = "340px"))
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header("Tenedores de TES — Jun 2026"),
           plotlyOutput("co_holdings", height = "340px")),
      card(card_header("Perfil de Vencimento — TES Clase B (Jun 2026)"),
           plotlyOutput("co_maturity", height = "340px"))
    ),
    card(
      card_header("Prazo Médio da Dívida Interna"),
      htmlOutput("co_avg_maturity")
    )
  ),

  # ── Overview ───────────────────────────────────────────────
  nav_panel(
    "Visão Geral",
    card(
      card_header("Pré vs. Pós — Composição YTD por País"),
      plotlyOutput("overview_pre_pos", height = "420px")
    )
  ),

  nav_spacer(),
  nav_item(
    tags$small(
      class = "text-white-50 pe-2",
      paste("Actualizado:", format(Sys.time(), "%d %b %Y %H:%M"))
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  chile    <- tryCatch(load_chile(),    error = function(e) {
    showNotification(paste("Erro Chile:",         e$message), type = "error"); NULL })
  mexico   <- tryCatch(load_mexico(),   error = function(e) {
    showNotification(paste("Erro México:",        e$message), type = "error"); NULL })
  sa       <- tryCatch(load_sa(),       error = function(e) {
    showNotification(paste("Erro África do Sul:", e$message), type = "error"); NULL })
  colombia <- tryCatch(load_colombia(), error = function(e) {
    showNotification(paste("Erro Colômbia:",      e$message), type = "error"); NULL })

  # ── Chile ──────────────────────────────────────────────────
  output$cl_ytd       <- renderPlotly({ req(chile); chart_ytd(chile$lic, "chile", "CLP tri") })
  output$cl_pct_gdp     <- renderPlotly({ req(chile); chart_pct_gdp(chile$lic, chile$gdp, "chile") })
  output$cl_ytd_pct_gdp <- renderPlotly({ req(chile); chart_ytd_pct_gdp(chile$lic, chile$gdp, "chile", "CLP tri") })
  output$cl_runrate     <- renderPlotly({ req(chile); chart_runrate(chile$lic, "chile", "CLP tri", target_val = 16.60) })
  output$cl_runrate_pct <- renderPlotly({ req(chile); chart_runrate_pct_gdp(chile$lic, chile$gdp, "chile") })
  output$cl_monthly     <- renderPlotly({ req(chile); chart_monthly(chile$lic, "chile", "CLP tri") })
  output$cl_monthly_pct <- renderPlotly({ req(chile); chart_monthly_pct_gdp(chile$lic, chile$gdp, "chile", "CLP tri") })
  output$cl_vs_target   <- renderPlotly({ req(chile); chart_vs_target(chile$lic, TARGET_CHILE, "chile", "CLP tri", "2026") })
  output$cl_tsy_seas    <- renderPlotly({ req(chile); chart_tsy_seasonal(chile$tsy, "chile", "USD bi") })
  output$cl_tsy_ts      <- renderPlotly({ req(chile); chart_tsy_ts(chile$tsy, "USD bi") })
  output$cl_tsy_seas_pesos <- renderPlotly({ req(chile); chart_tsy_seasonal(chile$tsy, "chile", "USD bi", col = "Pesos") })
  output$cl_tsy_ts_pesos   <- renderPlotly({ req(chile); chart_tsy_ts(chile$tsy, "USD bi", col = "Pesos") })
  output$cl_tsy_seas_dolar <- renderPlotly({ req(chile); chart_tsy_seasonal(chile$tsy, "chile", "USD bi", col = "Dolar") })
  output$cl_tsy_ts_dolar   <- renderPlotly({ req(chile); chart_tsy_ts(chile$tsy, "USD bi", col = "Dolar") })
  output$cl_composition     <- renderPlotly({ req(chile); chart_composition(chile$lic, "chile", "CLP tri", "instrument") })
  output$cl_composition_ccy <- renderPlotly({ req(chile); chart_composition(chile$lic, "chile", "CLP tri", "currency") })
  output$cl_pre_pos         <- renderPlotly({ req(chile); chart_pre_pos(chile$lic, "chile") })
  output$cl_debt_pct        <- renderPlotly({ req(chile); chart_debt_pct_gdp(chile$debt, chile$gdp_usd, "chile") })
  output$cl_debt_comp       <- renderPlotly({ req(chile); chart_debt_composition(chile$debt, "chile") })

  # ── Mexico ─────────────────────────────────────────────────
  output$mx_ytd         <- renderPlotly({ req(mexico); chart_ytd(mexico$lic, "mexico", "MXN tri") })
  output$mx_pct_gdp     <- renderPlotly({ req(mexico); chart_pct_gdp(mexico$lic, mexico$gdp, "mexico") })
  output$mx_ytd_pct_gdp <- renderPlotly({ req(mexico); chart_ytd_pct_gdp(mexico$lic, mexico$gdp, "mexico", "MXN tri") })
  output$mx_runrate     <- renderPlotly({ req(mexico); chart_runrate(mexico$lic, "mexico", "MXN tri", target_val = 3.15) })
  output$mx_runrate_pct <- renderPlotly({ req(mexico); chart_runrate_pct_gdp(mexico$lic, mexico$gdp, "mexico") })
  output$mx_monthly     <- renderPlotly({ req(mexico); chart_monthly(mexico$lic, "mexico", "MXN tri") })
  output$mx_monthly_pct <- renderPlotly({ req(mexico); chart_monthly_pct_gdp(mexico$lic, mexico$gdp, "mexico", "MXN tri") })
  output$mx_vs_target   <- renderPlotly({ req(mexico); chart_vs_target(mexico$lic, TARGET_MEXICO, "mexico", "MXN tri", "2026") })
  output$mx_tsy_seas    <- renderPlotly({ req(mexico); chart_tsy_seasonal(mexico$tsy, "mexico", "MXN bi") })
  output$mx_tsy_ts      <- renderPlotly({ req(mexico); chart_tsy_ts(mexico$tsy, "MXN bi") })
  output$mx_composition     <- renderPlotly({ req(mexico); chart_composition(mexico$lic, "mexico", "MXN tri", "instrument") })
  output$mx_composition_ccy <- renderPlotly({ req(mexico); chart_composition(mexico$lic, "mexico", "MXN tri", "currency") })
  output$mx_pre_pos         <- renderPlotly({ req(mexico); chart_pre_pos(mexico$lic, "mexico") })
  output$mx_debt_pct        <- renderPlotly({ req(mexico); chart_debt_pct_gdp(mexico$debt, mexico$gdp, "mexico") })
  output$mx_debt_comp       <- renderPlotly({ req(mexico); chart_debt_composition(mexico$debt, "mexico") })
  output$mx_holdings        <- renderPlotly({ req(mexico); chart_holdings(mexico$holdings, "mexico") })
  output$mx_maturity        <- renderPlotly({ req(mexico); chart_mexico_maturity(mexico$maturity) })
  output$mx_avg_maturity    <- renderPlotly({ req(mexico); chart_mexico_avg_maturity(mexico$avg_maturity) })

  # ── South Africa ───────────────────────────────────────────
  output$sa_ytd         <- renderPlotly({ req(sa); chart_ytd(sa$lic, "south_africa", "ZAR tri") })
  output$sa_pct_gdp     <- renderPlotly({ req(sa); chart_pct_gdp(sa$lic, sa$gdp, "south_africa") })
  output$sa_ytd_pct_gdp <- renderPlotly({ req(sa); chart_ytd_pct_gdp(sa$lic, sa$gdp, "south_africa", "ZAR tri") })
  output$sa_runrate     <- renderPlotly({ req(sa); chart_runrate(sa$lic, "south_africa", "ZAR tri", target_val = 1.853) })
  output$sa_runrate_pct <- renderPlotly({ req(sa); chart_runrate_pct_gdp(sa$lic, sa$gdp, "south_africa") })
  output$sa_monthly     <- renderPlotly({ req(sa); chart_monthly(sa$lic, "south_africa", "ZAR tri") })
  output$sa_monthly_pct <- renderPlotly({ req(sa); chart_monthly_pct_gdp(sa$lic, sa$gdp, "south_africa", "ZAR tri") })
  output$sa_vs_target   <- renderPlotly({ req(sa); chart_vs_target(sa$lic, TARGET_SA, "south_africa", "ZAR tri", "2026/27") })
  output$sa_tsy_seas    <- renderPlotly({ req(sa); chart_tsy_seasonal(sa$tsy, "south_africa", "ZAR tri") })
  output$sa_tsy_ts      <- renderPlotly({ req(sa); chart_tsy_ts(sa$tsy, "ZAR tri") })
  output$sa_composition     <- renderPlotly({ req(sa); chart_composition(sa$lic, "south_africa", "ZAR tri", "instrument") })
  output$sa_composition_ccy <- renderPlotly({ req(sa); chart_composition(sa$lic, "south_africa", "ZAR tri", "currency") })
  output$sa_pre_pos         <- renderPlotly({ req(sa); chart_pre_pos(sa$lic, "south_africa") })
  output$sa_debt_pct        <- renderPlotly({ req(sa); chart_debt_pct_gdp(sa$debt, sa$gdp, "south_africa") })
  output$sa_debt_comp       <- renderPlotly({ req(sa); chart_debt_composition(sa$debt, "south_africa") })
  output$sa_holdings        <- renderPlotly({ req(sa); chart_holdings(sa$holdings, "south_africa") })
  output$sa_maturity        <- renderPlotly({ req(sa); chart_sa_maturity(sa$maturity) })
  output$sa_avg_maturity    <- renderPlotly({ req(sa); chart_sa_avg_maturity(sa$avg_maturity) })

  # ── Colombia ───────────────────────────────────────────────
  output$co_monthly      <- renderPlotly({ req(colombia); chart_monthly(colombia$lic, "colombia", "COP tri") })
  output$co_monthly_pct  <- renderPlotly({ req(colombia); chart_monthly_pct_gdp(colombia$lic, colombia$gdp, "colombia", "COP tri") })
  output$co_ytd          <- renderPlotly({ req(colombia); chart_ytd(colombia$lic, "colombia", "COP tri") })
  output$co_ytd_pct_gdp  <- renderPlotly({ req(colombia); chart_ytd_pct_gdp(colombia$lic, colombia$gdp, "colombia", "COP tri") })
  output$co_runrate      <- renderPlotly({ req(colombia); chart_runrate(colombia$lic, "colombia", "COP tri") })
  output$co_runrate_pct  <- renderPlotly({ req(colombia); chart_runrate_pct_gdp(colombia$lic, colombia$gdp, "colombia") })
  output$co_tsy_seas     <- renderPlotly({ req(colombia); chart_tsy_seasonal(colombia$tsy, "colombia", "COP tri") })
  output$co_tsy_ts       <- renderPlotly({ req(colombia); chart_tsy_ts(colombia$tsy, "COP tri") })
  output$co_vs_target    <- renderPlotly({ plotly_placeholder("Plano de financiamento 2026 em breve") })
  output$co_composition     <- renderPlotly({ req(colombia); chart_composition(colombia$lic, "colombia", "COP tri", "instrument") })
  output$co_composition_ccy <- renderPlotly({ req(colombia); chart_composition(colombia$lic, "colombia", "COP tri", "currency") })
  output$co_pre_pos         <- renderPlotly({ req(colombia); chart_pre_pos(colombia$lic, "colombia") })
  output$co_debt_pct        <- renderPlotly({ req(colombia); chart_debt_pct_gdp(colombia$debt, colombia$gdp, "colombia") })
  output$co_debt_comp       <- renderPlotly({ req(colombia); chart_debt_composition(colombia$debt, "colombia") })
  output$co_holdings        <- renderPlotly({ req(colombia); chart_colombia_holdings(colombia$holdings) })
  output$co_maturity        <- renderPlotly({ req(colombia); chart_colombia_maturity(colombia$maturity) })
  output$co_avg_maturity    <- renderUI({
    req(colombia)
    row    <- colombia$avg_maturity |> slice(1)
    valor  <- paste0(round(row$Vida_Media, 1), " anos")
    parts  <- strsplit(row$Fecha_Corte, "-")[[1]]
    mon    <- paste0(toupper(substr(parts[2], 1, 1)), substr(parts[2], 2, nchar(parts[2])))
    subtitle <- paste0(mon, " ", parts[3])
    div(style = "padding:20px; text-align:center;",
      div(style = "font-size:2rem; font-weight:700; color:#1e3a5f;", valor),
      div(style = "font-size:0.9rem; color:#555; margin-top:6px;", subtitle),
      div(style = "font-size:0.75rem; color:#888; margin-top:8px;",
          "Fonte: IRC — Perfil de Deuda TES Clase B")
    )
  })

  # ── Overview ───────────────────────────────────────────────
  output$overview_pre_pos <- renderPlotly({
    req(chile, mexico, sa, colombia)
    chart_pre_pos_overview(chile$lic, mexico$lic, sa$lic, colombia$lic)
  })
}

# ============================================================
shinyApp(ui, server)
