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

# --- Download (libcurl confirmed reliable; wininet was corrupting the file) ---
tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb", method = "libcurl", quiet = TRUE)

sheet_name <- excel_sheets(tmp)[1]
cat("Reading sheet:", sheet_name, "\n")

# --- Single read with col_types = "list" ---
# This preserves each cell's native type (number, date, text) with no
# per-column type-guessing conflicts, and - critically - everything below
# uses THIS data frame's own row numbering consistently. (readxl trims a
# purely decorative area at the top of this sheet and starts its own row 1
# a few rows below Excel's actual row 1; as long as we never mix in an
# absolute Excel row number - e.g. via a second read with range=cell_rows()
# - that offset doesn't matter.)
raw <- read_excel(tmp, sheet = sheet_name, col_names = FALSE, col_types = "list")

cell_chr <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_character_ else as.character(x)
cell_num <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else suppressWarnings(as.numeric(x))
cell_date <- function(x) {
  if (is.null(x) || length(x) == 0) return(as.Date(NA))
  if (inherits(x, "Date"))    return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  if (is.numeric(x))          return(as.Date(x, origin = "1899-12-30"))
  as.Date(NA)
}

col1 <- vapply(raw[[1]], cell_chr, character(1))
col1 <- trimws(col1)

# --- Locate structural markers by content, not fixed row numbers ---
entidad_header_row <- which(col1 == "ENTIDAD")[1]
stopifnot(!is.na(entidad_header_row))
date_row <- entidad_header_row + 1
cat("'ENTIDAD' header found at row:", entidad_header_row, "-> dates at row", date_row, "\n")

after_date <- (date_row + 1):nrow(raw)
first_data_row <- after_date[!is.na(col1[after_date]) & col1[after_date] != ""][1]
stopifnot(!is.na(first_data_row))
cat("Entity data starts at row:", first_data_row, "\n")

total_candidates <- which(col1 == "Total general")
total_row_idx <- total_candidates[total_candidates >= first_data_row][1]
stopifnot(!is.na(total_row_idx))
cat("'Total general' found at row:", total_row_idx, "\n")

# --- Dates: row `date_row`, columns 2 onward, from the SAME data frame ---
# (row-slicing keeps each column as a length-1 list, so unwrap with [[1]]
# before extracting the value)
fechas <- do.call(c, lapply(raw[date_row, -1], function(col) cell_date(col[[1]])))
cat("Dates found:", length(fechas), "- range:", format(min(fechas)), "to", format(max(fechas)), "\n")

# --- Entity values: rows first_data_row to total_row_idx-1 ---
entidad_names <- col1[first_data_row:(total_row_idx - 1)]
value_block   <- raw[first_data_row:(total_row_idx - 1), -1]
stopifnot(ncol(value_block) == length(fechas))

value_df <- as.data.frame(lapply(value_block, function(col) vapply(col, cell_num, numeric(1))))
names(value_df) <- format(fechas, "%Y-%m-%d")
value_df$Entidad <- entidad_names

# --- Reshape to long format (Periodo, Entidad, Valor) ---
holdings <- value_df %>%
  pivot_longer(-Entidad, names_to = "Periodo", values_to = "Valor") %>%
  mutate(Periodo = as.Date(Periodo)) %>%
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