# Colombia Licitaciones - TES COP
# Downloads historical auction data from IRC (Ministerio de Hacienda)
# Source: https://www.irc.gov.co/documents/d/guest/historico-colocacion-espanol-publicar-irc-{N}
# IRC bumps the friendlyURL suffix (-1, -2, -3, ...) each time it re-uploads the
# file, so we auto-discover the current one instead of hardcoding it.

library(readxl)
library(dplyr)
library(httr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\colombia_licitaciones.csv"

base_slug <- "https://www.irc.gov.co/documents/d/guest/historico-colocacion-espanol-publicar-irc"

# --- Find the current document suffix (highest -N that serves an xlsx) ---
valid_n <- integer(0)
for (n in 1:15) {
  r <- tryCatch(HEAD(paste0(base_slug, "-", n), timeout(20)),
                error = function(e) NULL)
  if (!is.null(r) && status_code(r) == 200) {
    ct <- headers(r)[["content-type"]]; if (is.null(ct)) ct <- ""
    if (grepl("spreadsheet|officedocument", ct, ignore.case = TRUE)) valid_n <- c(valid_n, n)
  }
}
if (length(valid_n) == 0) stop("No valid IRC colocacion URL found (tried -1..-15)")

url <- paste0(base_slug, "-", max(valid_n), "?download=true")
cat("Using source:", url, "\n")

# --- Download (libcurl; wininet is deprecated) ---
tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb", method = "libcurl", quiet = TRUE)

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