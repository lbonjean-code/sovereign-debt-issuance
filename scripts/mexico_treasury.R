# Mexico Treasury Cash Balance
# Fetches "Crédito neto al Gobierno Federal" from Banxico SIE API
# Series SF1575 - Monthly, Miles de Pesos
# Note: values are negative (government is net creditor), multiply by -1 to get cash balance

library(httr)
library(jsonlite)
library(dplyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\mexico_treasury.csv"
token       <- Sys.getenv("BANXICO_TOKEN")

# --- Fetch from Banxico API ---
url <- paste0(
  "https://www.banxico.org.mx/SieAPIRest/service/v1/series/SF1575/datos/",
  "2010-01-01/", format(Sys.Date(), "%Y-%m-%d")
)

resp <- GET(url, add_headers("Bmx-Token" = token))

if (status_code(resp) != 200) {
  stop("Error fetching Banxico data: ", status_code(resp))
}

data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
obs  <- data$bmx$series$datos[[1]]

treasury <- tibble(
  Periodo = as.Date(obs$fecha, format = "%d/%m/%Y"),
  Saldo   = as.numeric(gsub(",", "", obs$dato))
) %>%
  filter(!is.na(Saldo), !is.na(Periodo)) %>%
  # Multiply by -1 to convert to positive cash balance
  # Divide by 1000 to convert from Miles de Pesos to Millones de Pesos
  mutate(Saldo = (Saldo * -1) / 1000) %>%
  arrange(Periodo)

# --- Save ---
write.csv(treasury, output_path, row.names = FALSE)
cat("Saved", nrow(treasury), "rows to", output_path, "\n")
print(tail(treasury, 6))