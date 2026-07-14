# Chile GDP - Annual in current USD
# Fetches GDP from World Bank API
# Indicator: NY.GDP.MKTP.CD - GDP current USD
# Used as denominator for Chile debt % GDP chart

library(httr)
library(jsonlite)
library(dplyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_gdp_usd.csv"

# --- Fetch from World Bank API ---
url <- "https://api.worldbank.org/v2/country/CL/indicator/NY.GDP.MKTP.CD?format=json&per_page=100"

resp <- GET(url)

if (status_code(resp) != 200) {
  stop("Error fetching World Bank data: ", status_code(resp))
}

data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
obs  <- data[[2]]

gdp <- tibble(
  Anio = as.integer(obs$date),
  PIB  = as.numeric(obs$value)
) %>%
  filter(!is.na(PIB)) %>%
  arrange(Anio)

# --- Save ---
write.csv(gdp, output_path, row.names = FALSE)
cat("Saved", nrow(gdp), "rows to", output_path, "\n")
print(tail(gdp, 5))