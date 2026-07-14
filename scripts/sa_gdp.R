# South Africa GDP - Annual
# Fetches GDP at current prices (LCU) from World Bank API
# Indicator: NY.GDP.MKTP.CN - GDP current LCU (ZAR)

library(httr)
library(jsonlite)
library(dplyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\south_africa_gdp.csv"

# --- Fetch from World Bank API ---
url <- "https://api.worldbank.org/v2/country/ZA/indicator/NY.GDP.MKTP.CN?format=json&per_page=100"

resp <- GET(url)

if (status_code(resp) != 200) {
  stop("Error fetching World Bank data: ", status_code(resp))
}

data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
obs  <- data[[2]]

gdp <- tibble(
  Anio = as.integer(obs$date),
  PIB  = as.numeric(obs$value) / 1000000  # convert to millions ZAR
) %>%
  filter(!is.na(PIB)) %>%
  arrange(Anio)

# --- Save ---
write.csv(gdp, output_path, row.names = FALSE)
cat("Saved", nrow(gdp), "rows to", output_path, "\n")
print(tail(gdp, 5))