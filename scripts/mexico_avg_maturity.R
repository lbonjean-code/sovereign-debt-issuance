# Mexico Weighted Average Maturity of Government Securities
# Series SG231 - Plazo promedio ponderado de valores gubernamentales
# Source: Banxico SIE - Finanzas Publicas / Otros indicadores de deuda publica
# Units: Days (convert to years by dividing by 365)

library(httr)
library(jsonlite)
library(dplyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\mexico_avg_maturity.csv"
token <- Sys.getenv("BANXICO_TOKEN")

# --- Fetch series SG231 ---
url <- paste0(
  "https://www.banxico.org.mx/SieAPIRest/service/v1/series/SG231/datos/2000-01-01/",
  format(Sys.Date(), "%Y-%m-%d")
)

resp <- GET(url, add_headers("Bmx-Token" = token))
if (status_code(resp) != 200) stop("Error fetching SG231: ", status_code(resp))

data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
obs  <- data$bmx$series$datos[[1]]

avg_maturity <- tibble(
  Periodo = as.Date(obs$fecha, format = "%d/%m/%Y"),
  Dias    = suppressWarnings(as.numeric(gsub(",", "", obs$dato))),
  Anos    = round(Dias / 365, 2)
) %>%
  filter(!is.na(Dias)) %>%
  arrange(Periodo)

cat("Rows loaded:", nrow(avg_maturity), "\n")
print(tail(avg_maturity, 6))

# --- Save ---
write.csv(avg_maturity, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")