# Mexico UDI (Unidad de Inversion) daily value
# Series SP68257 - Valor de la UDI
# Source: Banxico SIE - Indices de Precios al Consumidor y UDIS
# Units: MXN per UDI
# Used to convert Udibono nominal amounts from UDI to MXN

library(httr)
library(jsonlite)
library(dplyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\mexico_udi.csv"
token <- Sys.getenv("BANXICO_TOKEN")

# --- Fetch series SP68257 ---
url <- paste0(
  "https://www.banxico.org.mx/SieAPIRest/service/v1/series/SP68257/datos/2000-01-01/",
  format(Sys.Date(), "%Y-%m-%d")
)

resp <- GET(url, add_headers("Bmx-Token" = token))
if (status_code(resp) != 200) stop("Error fetching SP68257: ", status_code(resp))

data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
obs  <- data$bmx$series$datos[[1]]

udi <- tibble(
  Fecha    = as.Date(obs$fecha, format = "%d/%m/%Y"),
  UDI_MXN  = suppressWarnings(as.numeric(gsub(",", "", obs$dato)))
) %>%
  filter(!is.na(UDI_MXN)) %>%
  arrange(Fecha)

cat("Rows loaded:", nrow(udi), "\n")
print(tail(udi, 5))

# --- Save ---
write.csv(udi, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")