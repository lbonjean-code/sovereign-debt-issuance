# Colombia GDP - Quarterly
# Downloads PIB a precios corrientes from DANE
# Source: https://www.dane.gov.co/files/operaciones/PIB/anex-ProduccionCorriente-Itrim2026.xlsx
# Units: miles de millones de pesos (billions of COP)
# Note: URL contains the latest quarter - update when DANE publishes new data
# Format: anex-ProduccionCorriente-{Q}trim{YYYY}.xlsx where Q = I,II,III,IV

library(readxl)
library(dplyr)
library(tidyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\colombia_gdp.csv"

# --- Dynamically find latest available DANE file ---
find_latest_url <- function() {
  base <- "https://www.dane.gov.co/files/operaciones/PIB/anex-ProduccionCorriente-"
  quarters <- c("I", "II", "III", "IV")
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  for (yr in current_year:(current_year - 1)) {
    for (q in rev(seq_along(quarters))) {
      url <- paste0(base, quarters[q], "trim", yr, ".xlsx")
      resp <- tryCatch(HEAD(url), error = function(e) NULL)
      if (!is.null(resp) && httr::status_code(resp) == 200) {
        cat("Found:", url, "\n")
        return(url)
      }
    }
  }
  stop("Could not find latest DANE GDP file")
}

library(httr)
url <- find_latest_url()

# --- Download ---
tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb", quiet = TRUE)

# --- Read Cuadro 1 ---
raw <- read_excel(tmp, sheet = "Cuadro 1", col_names = FALSE)

# --- Find PIB row ---
pib_row <- which(apply(raw, 1, function(r) any(grepl("^Producto interno bruto$", trimws(r), ignore.case = TRUE))))[1]
cat("PIB row:", pib_row, "\n")

# --- Get year and quarter headers ---
year_row    <- as.character(unlist(raw[8, ]))
quarter_row <- as.character(unlist(raw[9, ]))

# --- Forward-fill years across 4 quarter columns ---
current_year_val <- NA
years_filled <- c()
for (y in year_row) {
  if (!is.na(y) && grepl("^[0-9]{4}", y)) current_year_val <- as.integer(substr(y, 1, 4))
  years_filled <- c(years_filled, current_year_val)
}

# --- Extract PIB values ---
pib_vals <- as.numeric(unlist(raw[pib_row, ]))

# --- Build quarterly dataframe ---
quarter_map <- c("I"=1, "II"=4, "III"=7, "IV"=10)

gdp <- tibble(
  Ano      = years_filled,
  Trimestre = quarter_row,
  PIB      = pib_vals
) %>%
  filter(!is.na(Ano), Trimestre %in% c("I","II","III","IV"), !is.na(PIB)) %>%
  mutate(
    Mes     = quarter_map[Trimestre],
    Periodo = as.Date(paste0(Ano, "-", sprintf("%02d", Mes), "-01"))
  ) %>%
  filter(Periodo <= Sys.Date()) %>%
  select(Periodo, PIB) %>%
  arrange(Periodo)

cat("Rows loaded:", nrow(gdp), "\n")
print(tail(gdp, 8))

# --- Save ---
write.csv(gdp, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")