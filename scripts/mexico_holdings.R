# Mexico Government Securities Holdings by Sector
# Fetches "Tenencia de Valores Gubernamentales" total (GUBERNAMENTAL) by sector
# Source: Banxico SIE - Valores en Circulación / Por sector
# Units: Millions of pesos, weekly

library(httr)
library(jsonlite)
library(dplyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\mexico_holdings.csv"
token       <- Sys.getenv("BANXICO_TOKEN")

# --- Series map ---
series_map <- c(
  "Total"                    = "SF65219",
  "Sector_Bancario"          = "SF65211",
  "Residentes_Extranjero"    = "SF65218",
  "Residentes_Pais"          = "SF65217",
  "Siefores"                 = "SF65213",
  "Sociedades_Inversion"     = "SF65214",
  "Reportos_Banxico"         = "SF65210",
  "Garantias_Banxico"        = "SF65212",
  "Aseguradoras"             = "SF65215",
  "Otros_Residentes_Pais"    = "SF65216",
  "Valores_Adquiridos_Banxico" = "SF235837"
)

# --- Function to fetch one series ---
fetch_serie <- function(id, nombre) {
  url  <- paste0(
    "https://www.banxico.org.mx/SieAPIRest/service/v1/series/", id,
    "/datos/2000-01-01/", format(Sys.Date(), "%Y-%m-%d")
  )
  resp <- GET(url, add_headers("Bmx-Token" = token))
  if (status_code(resp) != 200) {
    cat("Error fetching", nombre, "(", id, "):", status_code(resp), "\n")
    return(NULL)
  }
  data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  obs  <- data$bmx$series$datos[[1]]
  if (is.null(obs) || nrow(obs) == 0) return(NULL)
  tibble(
    Periodo = as.Date(obs$fecha, format = "%d/%m/%Y"),
    Valor   = suppressWarnings(as.numeric(gsub(",", "", obs$dato)))
  ) %>%
    filter(!is.na(Valor)) %>%
    rename(!!nombre := Valor)
}

# --- Fetch all series ---
cat("Fetching holdings by sector...\n")

result <- NULL
for (nombre in names(series_map)) {
  id  <- series_map[[nombre]]
  cat("  Fetching:", nombre, "(", id, ")\n")
  df  <- fetch_serie(id, nombre)
  if (!is.null(df)) {
    if (is.null(result)) {
      result <- df
    } else {
      result <- full_join(result, df, by = "Periodo")
    }
  }
}

holdings <- result %>% arrange(Periodo)

cat("Rows loaded:", nrow(holdings), "\n")
print(tail(holdings, 3))

# --- Save ---
write.csv(holdings, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")