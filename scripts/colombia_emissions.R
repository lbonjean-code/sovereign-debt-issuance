# Colombia Licitaciones - TES COP
# Downloads historical auction data from IRC (Ministerio de Hacienda)
# Source: https://www.irc.gov.co/documents/d/guest/historico-colocacion-espanol-publicar-irc-1?download=true

library(readxl)
library(dplyr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\colombia_licitaciones.csv"

url <- "https://www.irc.gov.co/documents/d/guest/historico-colocacion-espanol-publicar-irc-1?download=true"

# --- Download ---
tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb", method = "wininet", quiet = TRUE)

# --- Read TES COP sheet ---
raw <- read_excel(tmp, sheet = "TES Total", skip = 5, col_names = TRUE)

# --- Clean column names ---
names(raw) <- trimws(names(raw))

# --- Select and clean ---
colombia <- raw %>%
  select(
    Fecha_Subasta     = `Fecha de Cumplimiento`,
    Fecha_Vencimiento = `Fecha de Vencimiento`,
    Moneda,
    Tasa_Corte        = `Tasa de Corte`,
    Monto             = `Valor costo aprobado COP`,
    Tipo_Operacion    = `Tipo de operación`,
    Tenor             = `Tipo *`,
    Duracion          = `Duración`
  ) %>%
  mutate(
    Fecha_Subasta     = as.Date(Fecha_Subasta),
    Fecha_Vencimiento = as.Date(Fecha_Vencimiento),
    Monto             = as.numeric(Monto),
    Tasa_Corte        = as.numeric(Tasa_Corte),
    Duracion          = as.numeric(Duracion)
  ) %>%
  filter(!is.na(Fecha_Subasta), !is.na(Monto)) %>%
  arrange(Fecha_Subasta)

cat("Rows loaded:", nrow(colombia), "\n")
print(tail(colombia, 6))

# --- Save ---
write.csv(colombia, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")