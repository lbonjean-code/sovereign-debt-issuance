# Chile Gross Debt - Internal & External
# Scrapes BCCh BDE - Stock de Deuda por Instrumento, Gobierno Central
# Source: https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_FIN_PUB/MN_FIN_PUB_1/GOB_CENTRAL_EST_3/gob_central_est_3
# Units: millions of USD, quarterly

library(rvest)
library(dplyr)
library(tidyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_debt.csv"

url <- "https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_FIN_PUB/MN_FIN_PUB_1/GOB_CENTRAL_EST_3/gob_central_est_3"

page <- read_html(url)
raw  <- html_table(page, fill = TRUE, convert = FALSE)[[1]]

# Helper: parse Spanish number format (1.234,5 -> 1234.5)
parse_es_number <- function(x) {
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x)
  as.numeric(x)
}

# Helper: parse Spanish quarter labels to dates (I.2022 -> 2022-01-01)
quarter_map <- c("I"=1, "II"=4, "III"=7, "IV"=10)

parse_quarter <- function(x) {
  parts <- strsplit(x, "\\.")[[1]]
  if (length(parts) != 2) return(NA_Date_)
  q   <- parts[1]
  yr  <- parts[2]
  mon <- quarter_map[q]
  if (is.na(mon)) return(NA_Date_)
  as.Date(paste0(yr, "-", sprintf("%02d", mon), "-01"))
}

# --- Extract rows ---
total_row   <- raw[raw$Serie == "Deuda total", ]
interna_row <- raw[raw$Serie == "Deuda Interna", ]
externa_row <- raw[raw$Serie == "Deuda Externa", ]

# Quarter columns (all except Sel. and Serie)
quarters <- names(raw)[!names(raw) %in% c("Sel.", "Serie")]

# Quarter column names (all except first)
quarters <- names(raw)[-1]

# Build dataframe
debt <- tibble(
  Periodo = as.Date(sapply(quarters, parse_quarter), origin = "1970-01-01"),  Total    = sapply(as.character(total_row[-1]),   parse_es_number),
  Interna  = sapply(as.character(interna_row[-1]),  parse_es_number),
  Externa  = sapply(as.character(externa_row[-1]),  parse_es_number)
) %>%
  filter(!is.na(Periodo), !is.na(Total)) %>%
  arrange(Periodo)

cat("Rows loaded:", nrow(debt), "\n")
print(tail(debt, 6))

# --- Save ---
write.csv(debt, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")