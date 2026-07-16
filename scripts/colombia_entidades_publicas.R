# Colombia Entidades Publicas - TES Colocaciones (monthly)
# Downloads the monthly "Como Vamos" file from IRC (Ministerio de Hacienda)
# and extracts the "Entidades Públicas" column from the "Colocaciones" sheet.
# The URL slug is predictable (publicar-como-vamos-{mes}-{dia}-{anio}), but the
# day varies by release, so we brute-force the days of the target month.
# Source pattern: https://www.irc.gov.co/documents/d/guest/publicar-como-vamos-{mes}-{dia}-{anio}_?download=true

library(readxl)
library(dplyr)
library(lubridate)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\colombia_entidades_publicas.csv"

mes_es <- c("enero","febrero","marzo","abril","mayo","junio",
            "julio","agosto","septiembre","octubre","noviembre","diciembre")

build_url <- function(anio, mes, dia) {
  sprintf("https://www.irc.gov.co/documents/d/guest/publicar-como-vamos-%s-%d-%d_?download=true",
          mes_es[mes], dia, anio)
}

# --- Find latest available file: try current month first, then previous ---
find_latest_url <- function(anio, mes) {
  ultimo_dia <- day(ceiling_date(make_date(anio, mes, 1), "month") - 1)
  for (d in ultimo_dia:1) {
    url <- build_url(anio, mes, d)
    tmp <- tempfile(fileext = ".xlsx")
    ok <- tryCatch({
      download.file(url, tmp, mode = "wb", method = "wininet", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (ok && file.exists(tmp) && file.info(tmp)$size > 0) return(list(url = url, tmp = tmp))
  }
  NULL
}

hoy <- Sys.Date()
resultado <- find_latest_url(year(hoy), month(hoy))
if (is.null(resultado)) {
  anterior <- hoy %m-% months(1)
  resultado <- find_latest_url(year(anterior), month(anterior))
}
stopifnot(!is.null(resultado))

cat("Using source file:", resultado$url, "\n")
tmp <- resultado$tmp

# --- Read Colocaciones sheet ---
raw <- read_excel(tmp, sheet = "Colocaciones", skip = 22, col_names = TRUE)
names(raw) <- trimws(names(raw))

# --- Select and clean ---
colombia_entidades <- raw %>%
  select(
    Fecha              = `Fecha`,
    Entidades_Publicas = `Entidades Públicas`
  ) %>%
  mutate(
    Fecha              = as.Date(Fecha),
    Entidades_Publicas = as.numeric(Entidades_Publicas)
  ) %>%
  filter(!is.na(Fecha), Fecha <= Sys.Date()) %>%
  arrange(Fecha)

cat("Rows loaded:", nrow(colombia_entidades), "\n")
print(tail(colombia_entidades, 6))

# --- Merge into permanent CSV record (CSV is the permanent record; never rebuild from source) ---
if (file.exists(output_path)) {
  existing <- read.csv(output_path, stringsAsFactors = FALSE) %>% mutate(Fecha = as.Date(Fecha))
  new_rows <- anti_join(colombia_entidades, existing, by = "Fecha")
  combined <- bind_rows(existing, new_rows) %>%
    filter(Fecha <= Sys.Date()) %>%
    arrange(Fecha)
  cat(nrow(new_rows), "new row(s) added\n")
} else {
  combined <- colombia_entidades %>% filter(Fecha <= Sys.Date())
  cat("No existing CSV found; creating new file with", nrow(combined), "rows\n")
}

# --- Save (dates as YYYY-MM-DD strings, per pipeline convention) ---
combined <- combined %>% mutate(Fecha = format(Fecha, "%Y-%m-%d"))
write.csv(combined, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")