# Chile Licitaciones - Data Load + Scrape
# First run: reads from historical Excel
# Subsequent runs: reads from CSV and appends new auctions from hacienda.cl

library(readxl)
library(dplyr)
library(rvest)

# --- Config ---
file_path   <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\chile\\Chile Licitaciones Consolidated.xlsx"
sheet_name  <- "Debt"
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_licitaciones.csv"

# --- Load base data: CSV if exists, otherwise Excel ---
if (file.exists(output_path)) {
  chile <- read.csv(output_path) %>%
    mutate(
      Fecha_Licitacion = as.Date(Fecha_Licitacion),
      Madurez          = as.Date(Madurez),
      Monto            = as.numeric(Monto),
      Tasa             = as.numeric(Tasa)
    )
  cat("Loaded", nrow(chile), "rows from existing CSV\n")
} else {
  raw <- read_excel(file_path, sheet = sheet_name, na = "n.d.")
  chile <- raw %>%
    select(
      Moneda,
      Madurez,
      Fecha_Licitacion = `Fecha de Licitación`,
      Monto = Total,
      Tasa = `Tasa de interés (base 365)`,
      Tenor
    ) %>%
    mutate(
      Fecha_Licitacion = as.Date(Fecha_Licitacion),
      Madurez          = as.Date(Madurez),
      Monto            = as.numeric(Monto),
      Tasa             = as.numeric(Tasa)
    )
  cat("First run: loaded", nrow(chile), "rows from Excel\n")
}

# --- Scrape ultima licitacion ---
url      <- "https://www.hacienda.cl/areas-de-trabajo/finanzas-internacionales/oficina-de-la-deuda-publica/colocaciones-bcch-soma/resultados-ultima-licitacion"
page     <- read_html(url)
raw_text <- page %>% html_node("table") %>% html_table(fill = TRUE, convert = FALSE)

tables <- list(raw_text)

if (length(tables) == 0) {
  message("No table found - page may be JavaScript-rendered.")
} else {
  raw_new <- tables[[1]]
  
  nueva <- raw_new %>%
    select(
      Fecha_Licitacion = `Fecha de Licitación`,
      Madurez          = `Fecha de Vencimiento`,
      Monto            = `Total Adjudicado`,
      Moneda           = Unidad,
      Tasa             = `Tasa de Interés`
    ) %>%
    mutate(
      Moneda = case_when(
        grepl("Pesos", Moneda, ignore.case = TRUE) ~ "CLP",
        grepl("UF",    Moneda, ignore.case = TRUE) ~ "UF",
        TRUE ~ Moneda
      ),
      Fecha_Licitacion = as.Date(Fecha_Licitacion, format = "%d-%b-%y"),
      Madurez          = as.Date(Madurez,          format = "%d-%b-%y"),
      Monto            = as.numeric(gsub("\\.", "", Monto)),
      Monto            = if_else(Moneda == "UF", Monto / 1000, Monto),
      Tasa             = as.numeric(gsub("%", "", gsub(",", ".", Tasa))),
      Tenor = if_else(
        as.numeric(Madurez - Fecha_Licitacion) / 365 < 1, "CP", "LP"
      )
    ) %>%
    select(Moneda, Madurez, Fecha_Licitacion, Monto, Tasa, Tenor)
  
  # Only append rows not already in CSV
  nuevos_clean <- anti_join(nueva, chile,
                            by = c("Fecha_Licitacion", "Moneda", "Madurez"))
  
  cat("Genuinely new rows:", nrow(nuevos_clean), "\n")
  
  chile <- bind_rows(chile, nuevos_clean) %>%
    arrange(Fecha_Licitacion)
}

# --- Save ---
write.csv(chile, output_path, row.names = FALSE)
cat("Saved", nrow(chile), "rows to", output_path, "\n")