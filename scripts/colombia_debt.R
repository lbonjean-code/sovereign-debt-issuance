# Colombia Gross Debt - Internal & External
# Reads from manually downloaded "Histórico Total" file from IRC
# Source: https://www.irc.gov.co/en/public-debt/gnc-public-debt-profile
# Units: COP millones
# NOTE: Download latest "Histórico Total" file monthly from IRC and overwrite file_path

library(readxl)
library(dplyr)

# --- Config ---
# Update filename each month when new file is downloaded
file_path   <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\Histórico Total Mayo2026.xls"
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\colombia_debt.csv"

# --- Read Saldos sheet ---
raw <- read_excel(file_path, sheet = "Saldos", col_names = FALSE, skip = 20)

# --- Clean ---
debt <- raw %>%
  select(
    Periodo       = 1,
    Deuda_Interna = 2,
    Deuda_Externa = 3,
    Deuda_Total   = 4
  ) %>%
  mutate(
    Periodo = as.Date(as.numeric(Periodo), origin = "1899-12-30"),
    Deuda_Interna = as.numeric(Deuda_Interna),
    Deuda_Externa = as.numeric(Deuda_Externa),
    Deuda_Total   = as.numeric(Deuda_Total)
  ) %>%
  filter(!is.na(Periodo), !is.na(Deuda_Total)) %>%
  filter(Periodo <= Sys.Date()) %>%
  arrange(Periodo)

cat("Rows loaded:", nrow(debt), "\n")
print(tail(debt, 6))

# --- Save ---
write.csv(debt, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")