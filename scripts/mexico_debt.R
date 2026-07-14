# Mexico Net Public Debt - Internal & External
# Fetches Deuda Neta del Sector Público, Económica Amplia, Saldos al Final del Periodo
# Series: SG193 (Total), SG194 (Interna), SG195 (Externa)
# Source: Banxico SIE - Finanzas Públicas
# Units: Miles de millones de pesos (billions of MXN)

library(httr)
library(jsonlite)
library(dplyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\mexico_debt.csv"
token       <- Sys.getenv("BANXICO_TOKEN")

# --- Function to fetch one series ---
fetch_serie <- function(id) {
  url  <- paste0("https://www.banxico.org.mx/SieAPIRest/service/v1/series/", id, "/datos/2000-01-01/", format(Sys.Date(), "%Y-%m-%d"))
  resp <- GET(url, add_headers("Bmx-Token" = token))
  if (status_code(resp) != 200) stop("Error fetching series ", id)
  data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  obs  <- data$bmx$series$datos[[1]]
  tibble(
    Periodo = as.Date(obs$fecha, format = "%d/%m/%Y"),
    Valor   = as.numeric(gsub(",", "", obs$dato))
  ) %>% filter(!is.na(Valor))
}

# --- Fetch all three series ---
cat("Fetching Total (SG193)...\n")
total    <- fetch_serie("SG193") %>% rename(Total = Valor)

cat("Fetching Interna (SG194)...\n")
interna  <- fetch_serie("SG194") %>% rename(Interna = Valor)

cat("Fetching Externa (SG195)...\n")
externa  <- fetch_serie("SG195") %>% rename(Externa = Valor)

# --- Combine ---
debt <- total %>%
  left_join(interna, by = "Periodo") %>%
  left_join(externa, by = "Periodo") %>%
  arrange(Periodo)

cat("Rows loaded:", nrow(debt), "\n")
print(tail(debt, 6))

# --- Save ---
write.csv(debt, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")