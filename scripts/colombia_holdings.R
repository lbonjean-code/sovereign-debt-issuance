# Colombia TES Holdings by Sector (monthly time series)
# Downloads the historical holders file from IRC (Ministerio de Hacienda).
# Unlike the old PDF-based snapshot, this file republishes FULL history each
# month in a wide format (one column per month since Jan 2010), so this
# script does a full rebuild each run rather than an incremental append.
# The sheet name changes every month (e.g. "Junio 2026"), so we just read
# whichever single sheet exists.
# Source: https://www.irc.gov.co/documents/d/guest/historico-tenedores-tes-2?download=true
# Units: Millions of COP

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

# --- Config ---
output_path <- "C:\\Users\\lbonjean\\Documents\\sovereign_bond_tracker\\data\\colombia_holdings.csv"

url <- "https://www.irc.gov.co/documents/d/guest/historico-tenedores-tes-2?download=true"

# --- Download ---
tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb", method = "wininet", quiet = TRUE)

sheet_name <- excel_sheets(tmp)[1]
cat("Reading sheet:", sheet_name, "\n")

raw <- read_excel(tmp, sheet = sheet_name, col_names = FALSE)

# --- Date header row (row 12), columns 2 onward ---
fecha_row <- as.numeric(raw[12, -1])
fechas <- as.Date(fecha_row, origin = "1899-12-30")

# --- Entity rows: row 14 through the row before "Total general" ---
total_row_idx <- which(raw[[1]] == "Total general")
entidad_rows <- raw[14:(total_row_idx - 1), ]

names(entidad_rows) <- c("Entidad", format(fechas, "%Y-%m-%d"))

# --- Reshape to long format (Periodo, Entidad, Valor) ---
holdings <- entidad_rows %>%
  pivot_longer(-Entidad, names_to = "Periodo", values_to = "Valor") %>%
  mutate(
    Periodo = as.Date(Periodo),
    Entidad = trimws(Entidad),
    Valor   = as.numeric(Valor)
  ) %>%
  filter(!is.na(Entidad), !is.na(Valor), Periodo <= Sys.Date()) %>%
  arrange(Periodo, Entidad)

cat("Rows loaded:", nrow(holdings), "\n")
cat("Sectors found:", length(unique(holdings$Entidad)), "\n")
cat("Date range:", format(min(holdings$Periodo)), "to", format(max(holdings$Periodo)), "\n")
print(tail(holdings, 6))

# --- Save (full rebuild each run — source file is self-contained history) ---
holdings_out <- holdings %>% mutate(Periodo = format(Periodo, "%Y-%m-%d"))
write.csv(holdings_out, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")