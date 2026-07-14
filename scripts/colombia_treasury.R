# Colombia Treasury Cash Balance
# Downloads "Depósitos y Saldos Remunerados" from IRC (Ministerio de Hacienda)
# Source: https://www.irc.gov.co/documents/d/guest/depositos-y-saldos-remunerados-3?download=true
# Units: millions of COP

library(readxl)
library(dplyr)
library(tidyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\colombia_treasury.csv"

url <- "https://www.irc.gov.co/documents/d/guest/depositos-y-saldos-remunerados-3?download=true"

# --- Download ---
tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb", quiet = TRUE)

# --- Read ---
raw <- read_excel(tmp, sheet = 1, skip = 8, col_names = FALSE)

# --- Parse years from header row (row 1, columns 2 onwards) ---
years <- as.integer(trimws(gsub("[^0-9]", "", as.character(unlist(raw[1, -1])))))

# --- Extract only month rows (rows 2-13) ---
data_rows <- raw[2:13, ]

# --- Name columns directly: first col = Mes, rest = actual years ---
colnames(data_rows) <- c("Mes", as.character(years))

# --- Convert to character for safe pivoting ---
data_rows <- data_rows %>% mutate(across(everything(), as.character))

# --- Month name mapping ---
month_map <- c(
  "Enero"=1, "Febrero"=2, "Marzo"=3, "Abril"=4,
  "Mayo"=5, "Junio"=6, "Julio"=7, "Agosto"=8,
  "Septiembre"=9, "Octubre"=10, "Noviembre"=11, "Diciembre"=12
)

# --- Pivot to long format ---
treasury <- data_rows %>%
  pivot_longer(
    cols      = -Mes,
    names_to  = "Ano",
    values_to = "Saldo"
  ) %>%
  mutate(
    Ano     = as.integer(Ano),
    Mes_num = month_map[Mes],
    Saldo   = as.numeric(Saldo)
  ) %>%
  filter(!is.na(Saldo), !is.na(Ano), !is.na(Mes_num)) %>%
  mutate(Periodo = as.Date(paste0(Ano, "-", sprintf("%02d", Mes_num), "-01"))) %>%
  filter(Periodo <= Sys.Date()) %>%
  select(Periodo, Saldo) %>%
  arrange(Periodo)

cat("Rows loaded:", nrow(treasury), "\n")
print(tail(treasury, 6))

# --- Save ---
write.csv(treasury, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")