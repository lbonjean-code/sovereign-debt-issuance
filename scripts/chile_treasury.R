# Chile Treasury Cash Balance
# Scrapes "Otros Activos del Tesoro Público" - Pesos and Dolar rows
# Source: BCCh BDE - Finanzas Públicas / Activos consolidados del tesoro
# Note: BCCh reports in millions of USD, converted to billions on save

library(rvest)
library(dplyr)
library(tidyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_treasury.csv"

url <- "https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_FIN_PUB/MN_FIN_PUB_1/GOB_ACT_CONS_T1/act_cons_tesoro"

page <- read_html(url)
raw  <- html_table(page, fill = TRUE, convert = FALSE)[[1]]

# Helper: convert Spanish-formatted numbers (1.234,5 -> 1234.5)
parse_es_number <- function(x) {
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x)
  as.numeric(x)
}

# Helper: convert Spanish month abbreviations to dates
es_month_map <- c(
  "Ene" = "01", "Feb" = "02", "Mar" = "03", "Abr" = "04",
  "May" = "05", "Jun" = "06", "Jul" = "07", "Ago" = "08",
  "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dic" = "12"
)

parse_es_date <- function(x) {
  parts <- strsplit(x, "\\.")[[1]]
  mon   <- es_month_map[parts[1]]
  yr    <- parts[2]
  as.Date(paste0("01/", mon, "/", yr), format = "%d/%m/%Y")
}

treasury <- raw %>%
  filter(Serie %in% c("Pesos", "Dólar")) %>%
  pivot_longer(
    cols      = -Serie,
    names_to  = "Periodo",
    values_to = "Valor"
  ) %>%
  mutate(
    Valor   = parse_es_number(Valor),
    Periodo = as.Date(sapply(Periodo, parse_es_date))
  ) %>%
  filter(!is.na(Valor), !is.na(Periodo)) %>%
  pivot_wider(
    names_from  = Serie,
    values_from = Valor
  ) %>%
  rename(Dolar = `Dólar`) %>%
  arrange(Periodo) %>%
  # Convert from millions USD to billions USD
  mutate(
    Pesos = Pesos / 1000,
    Dolar = Dolar / 1000,
    Total = Pesos + Dolar
  )

# --- Save ---
write.csv(treasury, output_path, row.names = FALSE)
cat("Saved", nrow(treasury), "rows to", output_path, "\n")
print(tail(treasury, 6))