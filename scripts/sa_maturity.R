# South Africa Domestic Government Bond Maturity Profile
# Downloads "Schedule of Domestic Government Bonds" from National Treasury
# Source: https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Schedule%20of%20Domestic%20Debt/
# Units: ZAR (nominal amount)

library(pdftools)
library(dplyr)
library(stringr)
library(httr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\south_africa_maturity.csv"

base <- "https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Schedule%20of%20Domestic%20Debt/"

# --- URL for latest available PDF ---
# NOTE: Update this URL each month when National Treasury publishes new schedule
# Pattern: Schedule of domestic government bonds as at {DD} {Month} {YYYY}.pdf
url <- paste0(base, "Schedule%20of%20domestic%20government%20bonds%20as%20at%2030%20April%202026.pdf")
cat("Using URL:", URLdecode(url), "\n")

# --- Download with proper headers ---
tmp <- tempfile(fileext = ".pdf")
resp <- GET(
  url,
  write_disk(tmp, overwrite = TRUE),
  add_headers(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    "Accept"     = "application/pdf,*/*",
    "Referer"    = "https://investor.treasury.gov.za/"
  )
)
if (status_code(resp) != 200) stop("Failed to download SA bond schedule: ", status_code(resp))
txt <- pdf_text(tmp)
page_txt <- paste(txt, collapse = "\n")
lines <- strsplit(page_txt, "\n")[[1]]

# --- Extract cut-off date from filename/title ---
fecha_corte <- str_extract(URLdecode(url), "\\d{2} [A-Za-z]+ \\d{4}")
cat("Fecha de corte:", fecha_corte, "\n")

# --- Parse "Total YYYY/YY" rows ---
# Lines look like: "Total 2026/27   87 049 919 187"
total_lines <- lines[grepl("^\\s*Total \\d{4}/\\d{2}", lines)]

maturity <- lapply(total_lines, function(line) {
  # Extract fiscal year label e.g. "2026/27"
  fy <- str_extract(line, "\\d{4}/\\d{2}")
  if (is.na(fy)) return(NULL)
  
  # Remove "Total YYYY/YY" prefix, then parse what remains as a single number
  remainder <- str_replace(line, "^\\s*Total\\s+\\d{4}/\\d{2}\\s*", "")
  remainder <- trimws(remainder)
  # Remove spaces used as thousands separators
  remainder <- gsub("\\s+", "", remainder)
  nominal <- suppressWarnings(as.numeric(remainder))
  
  if (is.na(nominal) || nominal < 1e9) return(NULL)
  
  start_yr <- as.integer(substr(fy, 1, 4))
  
  data.frame(
    Ano_Fiscal  = fy,
    Start_Year  = start_yr,
    Nominal_ZAR = nominal,
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows() %>%
  filter(!is.na(Nominal_ZAR)) %>%
  arrange(Start_Year)

maturity$Fecha_Corte <- fecha_corte

cat("Rows loaded:", nrow(maturity), "\n")
print(maturity)

# --- Save ---
write.csv(maturity, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")