# Mexico GDP - Quarterly
# Fetches PIB a precios de mercado (precios corrientes) from Banxico SIE API
# Series SR17645 - Quarterly, millions of pesos

library(httr)
library(jsonlite)
library(dplyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\mexico_gdp.csv"
token       <- Sys.getenv("BANXICO_TOKEN")

# --- Fetch from Banxico API ---
url <- paste0(
  "https://www.banxico.org.mx/SieAPIRest/service/v1/series/SR17645/datos/",
  "2000-01-01/", format(Sys.Date(), "%Y-%m-%d")
)

resp <- GET(url, add_headers("Bmx-Token" = token))

if (status_code(resp) != 200) {
  stop("Error fetching Banxico data: ", status_code(resp))
}

data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
obs  <- data$bmx$series$datos[[1]]

gdp <- tibble(
  Periodo = as.Date(obs$fecha, format = "%d/%m/%Y"),
  PIB     = as.numeric(gsub(",", "", obs$dato))
) %>%
  filter(!is.na(PIB), !is.na(Periodo)) %>%
  arrange(Periodo)

# --- Save ---
write.csv(gdp, output_path, row.names = FALSE)
cat("Saved", nrow(gdp), "rows to", output_path, "\n")
print(tail(gdp, 8))