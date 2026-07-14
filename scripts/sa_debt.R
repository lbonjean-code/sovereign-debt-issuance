# South Africa Gross Loan Debt - Domestic & Foreign
# Downloads Stats Table 10 from National Treasury Budget Time Series
# Source: treasury.gov.za National Budget 2026
# Units: R million
# Note: URL updates annually with new budget - script tries current year first

library(readxl)
library(dplyr)
library(tidyr)
library(httr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\south_africa_debt.csv"

# --- Dynamically find latest available budget year ---
find_latest_url <- function() {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  for (yr in current_year:2020) {
    url <- paste0(
      "https://www.treasury.gov.za/documents/National%20Budget/",
      yr,
      "/TimeSeries/Excel/Stats_Table%2010%20-%20Timeseries%20and%20Snapshots.xlsx"
    )
    resp <- tryCatch(HEAD(url), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      cat("Found budget year:", yr, "\n")
      return(url)
    }
  }
  stop("Could not find latest National Treasury Stats Table 10 file")
}

url <- find_latest_url()

# --- Download ---
tmp <- tempfile(fileext = ".xlsx")
download.file(url, tmp, mode = "wb", quiet = TRUE)

# --- Read Timeseries sheet ---
raw <- read_excel(tmp, sheet = "Timeseries", col_names = FALSE)

# --- Get fiscal year headers from row 2 ---
fy_headers <- as.character(unlist(raw[2, -1]))

# --- Extract rows ---
gross    <- as.numeric(unlist(raw[4, -1]))
domestic <- as.numeric(unlist(raw[5, -1]))
foreign  <- as.numeric(unlist(raw[9, -1]))

# --- Build dataframe ---
debt <- tibble(
  Ano_Fiscal = fy_headers,
  Gross      = gross,
  Domestic   = domestic,
  Foreign    = foreign
) %>%
  filter(!is.na(Ano_Fiscal), !is.na(Gross)) %>%
  arrange(Ano_Fiscal)

debt <- debt %>%
  mutate(Start_Year = as.integer(substr(Ano_Fiscal, 1, 4))) %>%
  filter(Start_Year < as.integer(format(Sys.Date(), "%Y")) - 
           ifelse(as.integer(format(Sys.Date(), "%m")) >= 4, 0, 1)) %>%
  select(-Start_Year)

cat("Rows loaded:", nrow(debt), "\n")
print(tail(debt, 6))

# --- Save ---
write.csv(debt, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")