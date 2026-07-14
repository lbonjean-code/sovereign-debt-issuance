# Mexico Internal Debt Maturity Profile
# Extracts "Perfil de amortizaciones de la deuda interna del Gobierno Federal"
# Source: SHCP Informe Trimestral de Deuda Publica
# Units: Millions of pesos
# Note: URL pattern - update quarterly when new report is published
# itindc_{YYYY}{QQ}.PDF where QQ = 01, 02, 03, 04

library(pdftools)
library(dplyr)
library(stringr)
library(httr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\mexico_maturity.csv"

# --- Find latest available quarterly report ---
find_latest_url <- function() {
  base <- "https://www.finanzaspublicas.hacienda.gob.mx/work/models/Finanzas_Publicas/docs/congreso/infotrim/"
  current_year  <- as.integer(format(Sys.Date(), "%Y"))
  current_month <- as.integer(format(Sys.Date(), "%m"))
  current_q     <- ceiling(current_month / 3)
  
  for (yr in current_year:(current_year - 1)) {
    max_q <- if (yr == current_year) current_q else 4
    for (q in max_q:1) {
      quarter_folder <- c("it", "iit", "iiit", "ivt")[q]
      url <- paste0(base, yr, "/", quarter_folder, "/01inf/itindc_", yr, sprintf("%02d", q), ".PDF")
      resp <- tryCatch(HEAD(url), error = function(e) NULL)
      if (!is.null(resp) && status_code(resp) == 200) {
        cat("Found:", url, "\n")
        return(url)
      }
    }
  }
  stop("Could not find latest SHCP debt report")
}

url <- find_latest_url()

# --- Download PDF ---
tmp <- tempfile(fileext = ".pdf")
download.file(url, tmp, mode = "wb", quiet = TRUE)
txt <- pdf_text(tmp)

# --- Find page with maturity profile ---
maturity_page <- NULL
for (i in seq_along(txt)) {
  if (grepl("Perfil de amortizaciones de la deuda interna", txt[[i]], ignore.case = TRUE)) {
    maturity_page <- i
    break
  }
}

if (is.null(maturity_page)) stop("Could not find maturity profile page")
cat("Maturity profile found on page:", maturity_page, "\n")

# --- Parse the table ---
page_txt <- txt[[maturity_page]]
lines <- strsplit(page_txt, "\n")[[1]]

# Find the header line with years
header_line <- which(grepl("\\d{4}\\s+\\d{4}\\s+\\d{4}", lines))[1]
year_str <- lines[header_line]
years <- as.integer(str_extract_all(year_str, "20\\d{2}")[[1]])
cat("Years found:", paste(years, collapse=", "), "\n")

# Parse number helper
parse_number <- function(x) {
  x <- trimws(x)
  if (x == "" || x == "-" || x == "0.0") return(0)
  x <- gsub(",", "", x)
  suppressWarnings(as.numeric(x))
}

# Instruments to extract
instruments <- c("Total", "Cetes", "Bondes D", "Bondes F", "Bondes G",
                 "Bonos Tasa Fija Bono M", "Udibonos")

# Find and parse each instrument row
results <- list()
for (instr in instruments) {
  pattern <- paste0("^\\s*", gsub("\\.", "\\\\.", instr), "\\s")
  row <- lines[grepl(pattern, lines, ignore.case = FALSE)]
  if (length(row) == 0) next
  row <- row[1]
  # Extract numbers
  nums <- str_extract_all(row, "[0-9,]+\\.[0-9]+")[[1]]
  nums <- sapply(nums, parse_number)
  if (length(nums) >= length(years)) {
    nums <- nums[1:length(years)]
    df <- data.frame(
      Instrumento = instr,
      Ano = years,
      Monto = nums
    )
    results[[instr]] <- df
  }
}

maturity <- bind_rows(results)

# --- Extract cut-off date from PDF ---
fecha <- str_extract(page_txt, "al \\d{1,2} de [a-z]+ de \\d{4}")
cat("Fecha de corte:", fecha, "\n")
maturity$Fecha_Corte <- fecha

cat("Rows loaded:", nrow(maturity), "\n")
print(maturity)

# --- Save ---
write.csv(maturity, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")