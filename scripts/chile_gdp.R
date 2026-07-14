# Chile GDP - Quarterly
# Scrapes "GDP, at current prices" quarterly series from BCCh BDE
# Source: National Accounts, Reference 2018, billions of pesos
# Note: raw quarterly values - sum 4 quarters for rolling 12-month GDP in dashboard

library(rvest)
library(dplyr)
library(tidyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_gdp.csv"

url <- "https://si3.bcentral.cl/Siete/EN/Siete/Cuadro/CAP_CCNN/MN_CCNN76/CCNN2018_P0_V2/637801082315858005"

page <- read_html(url)
raw  <- html_table(page, fill = TRUE, convert = FALSE)[[1]]

# --- Extract GDP at current prices row ---
gdp_wide <- raw %>%
  filter(trimws(Serie) == "GDP, at current prices")

if (nrow(gdp_wide) == 0) {
  stop("Could not find 'GDP, at current prices' row - check BCCh page structure")
}

# --- Pivot to long format ---
# Column names are like "I.2013", "II.2013", "III.2013", "IV.2013"
quarter_map <- c("I" = 1, "II" = 4, "III" = 7, "IV" = 10)

gdp <- gdp_wide %>%
  pivot_longer(
    cols      = -c(Sel., Serie),
    names_to  = "Trimestre",
    values_to = "PIB"
  ) %>%
  mutate(
    # Remove thousand separator dots and convert
    PIB = as.numeric(gsub(",", "", PIB)),
    # Parse "I.2013" -> date
    Quarter = sub("\\..*", "", Trimestre),
    Anio    = as.integer(sub(".*\\.", "", Trimestre)),
    Mes     = quarter_map[Quarter],
    Periodo = as.Date(paste0(Anio, "-", sprintf("%02d", Mes), "-01"))
  ) %>%
  filter(!is.na(PIB), !is.na(Periodo)) %>%
  select(Periodo, PIB) %>%
  arrange(Periodo)

# --- Save ---
write.csv(gdp, output_path, row.names = FALSE)
cat("Saved", nrow(gdp), "rows to", output_path, "\n")
print(tail(gdp, 8))