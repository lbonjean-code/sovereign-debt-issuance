# Colombia TES Internal Debt Profile - Maturity Profile + Holdings by Sector
# Downloads monthly PDF from IRC and extracts:
# 1. Portafolio por Año de Vencimiento (page 3) - maturity profile
# 2. Saldo por Grupo de Tenedores (page 11) - holdings by sector
# Source: https://www.irc.gov.co/documents/d/guest/tes-perfil-deuda-{mon}-{day}?download=true
# Units: Millions of COP

library(pdftools)
library(dplyr)
library(stringr)
library(httr)

# --- Config ---
maturity_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\colombia_maturity.csv"
holdings_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\colombia_holdings.csv"

# --- Month mapping ---
months_map <- c(
  "1"="jan","2"="feb","3"="mar","4"="apr","5"="may","6"="jun",
  "7"="jul","8"="aug","9"="sep","10"="oct","11"="nov","12"="dec"
)
month_days <- c(31,28,31,30,31,30,31,31,30,31,30,31)

# --- Find latest available PDF ---

url <- "https://www.irc.gov.co/documents/d/guest/tes-perfil-deuda-jun-30?download=true"
# --- Download PDF ---
tmp <- tempfile(fileext = ".pdf")
download.file(url, tmp, mode = "wb", quiet = TRUE)
txt <- pdf_text(tmp)

# --- Extract cut-off date ---
fecha_corte <- str_extract(txt[[1]], "\\d{2}-[a-z]{3}-\\d{4}")
cat("Fecha de corte:", fecha_corte, "\n")

# --- Number parser ---
parse_number <- function(x) {
  x <- trimws(x)
  if (x == "-" || x == "") return(0)
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

# -------------------------------------------------------
# EXTRACT MATURITY PROFILE (page 3)
# -------------------------------------------------------
page3 <- txt[[3]]
lines <- strsplit(page3, "\n")[[1]]

# Find lines starting with a year (20xx) followed by spaces
maturity_lines <- lines[grepl("^\\s*20[0-9]{2}\\s", lines)]

maturity <- lapply(maturity_lines, function(line) {
  # Split on 2+ spaces to preserve column alignment
  parts <- str_split(trimws(line), "\\s{2,}")[[1]]
  parts <- parts[parts != ""]
  if (length(parts) < 6) return(NULL)
  year <- suppressWarnings(as.integer(parts[1]))
  if (is.na(year) || year < 2020 || year > 2100) return(NULL)
  # Columns: year, Descuento, Fija, Total_COP, TES_UVR, TOTAL, Participacion%
  tes_cop <- parse_number(parts[4])
  tes_uvr <- parse_number(parts[5])
  total   <- parse_number(parts[6])
  data.frame(Ano_Vencimiento = year, TES_COP = tes_cop, TES_UVR = tes_uvr, Total = total)
}) %>%
  bind_rows() %>%
  filter(!is.na(Ano_Vencimiento), !is.na(Total), Total > 0)

cat("Maturity profile rows:", nrow(maturity), "\n")
print(maturity)

# -------------------------------------------------------
# EXTRACT HOLDINGS BY SECTOR (page 11)
# -------------------------------------------------------
page11 <- txt[[11]]
lines11 <- strsplit(page11, "\n")[[1]]

holders_pattern <- paste(c(
  "Fondos de Pensiones",
  "Fondos de Capital Extranjero",
  "Bancos Comerciales",
  "Compa.*as de Seguros",
  "Banco de la Republica",
  "Fiducia Publica",
  "Carteras Colectivas",
  "Instituciones Oficiales",
  "Compa.*as de Financiamiento",
  "Corporaciones Financieras",
  "Comisionistas"
), collapse = "|")

holdings_lines <- lines11[grepl(holders_pattern, lines11, ignore.case = TRUE)]

holdings <- lapply(holdings_lines, function(line) {
  parts <- str_split(trimws(line), "\\s{2,}")[[1]]
  parts <- parts[parts != ""]
  if (length(parts) < 2) return(NULL)
  name  <- parts[1]
  nums  <- suppressWarnings(as.numeric(gsub("\\.", "", parts[-1])))
  nums  <- nums[!is.na(nums) & nums > 1000]
  if (length(nums) == 0) return(NULL)
  total <- nums[length(nums)]
  data.frame(Tenedor = trimws(name), Total = total)
}) %>%
  bind_rows() %>%
  filter(!is.na(Total))

cat("Holdings rows:", nrow(holdings), "\n")
print(holdings)

# --- Add fecha corte ---
maturity$Fecha_Corte <- fecha_corte
holdings$Fecha_Corte <- fecha_corte

# --- Save ---
write.csv(maturity, maturity_path, row.names = FALSE)
cat("Maturity profile saved to", maturity_path, "\n")

write.csv(holdings, holdings_path, row.names = FALSE)
cat("Holdings saved to", holdings_path, "\n")

# Add to colombia_maturity_holdings.R after the holdings extraction

# --- Extract Vida Media (Weighted Average Maturity) from page 1 ---
page1 <- txt[[1]]
lines1 <- strsplit(page1, "\n")[[1]]

# Find "Vida Media" line in the TOTAL GENERAL section
vida_media_line <- lines1[grepl("Vida Media", lines1)]

# The TOTAL column value is the last number on the first Vida Media line
vida_line <- vida_media_line[1]
nums <- str_extract_all(vida_line, "[0-9]+,[0-9]+")[[1]]
nums <- as.numeric(gsub(",", ".", nums))
vida_media_total <- nums[length(nums)]

cat("Vida Media (Total):", vida_media_total, "anos\n")

# Save as single-row CSV
vida_media_df <- data.frame(
  Fecha_Corte = fecha_corte,
  Vida_Media  = vida_media_total
)

vida_media_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\colombia_avg_maturity.csv"
write.csv(vida_media_df, vida_media_path, row.names = FALSE)
cat("Vida Media saved to", vida_media_path, "\n")