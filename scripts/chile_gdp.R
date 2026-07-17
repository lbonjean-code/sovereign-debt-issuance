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

# Column names are like "I.2013" -> quarter start month
quarter_map <- c("I" = 1, "II" = 4, "III" = 7, "IV" = 10)

# Scrape + build wrapped so a transient network/site failure keeps the existing CSV
gdp <- tryCatch({
  page <- read_html(url)
  raw  <- html_table(page, fill = TRUE, convert = FALSE)[[1]]

  gdp_wide <- raw %>%
    filter(trimws(Serie) == "GDP, at current prices")

  if (nrow(gdp_wide) == 0) {
    stop("Could not find 'GDP, at current prices' row - check BCCh page structure")
  }

  gdp_wide %>%
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
}, error = function(e) {
  message("chile_gdp scrape/parse failed (", conditionMessage(e), ") — keeping existing CSV.")
  NULL
})

# --- Save (only on success; preserve existing CSV otherwise) ---
if (!is.null(gdp) && nrow(gdp) > 0) {
  write.csv(gdp, output_path, row.names = FALSE)
  cat("Saved", nrow(gdp), "rows to", output_path, "\n")
  print(tail(gdp, 8))
} else {
  cat("chile_gdp: no data written; existing CSV preserved.\n")
}
