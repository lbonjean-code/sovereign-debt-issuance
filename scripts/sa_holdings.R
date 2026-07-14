# South Africa Government Bond Holdings by Investor Type
# Downloads historical holdings file from National Treasury investor relations
# Source: https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Holdings%20of%20Domestic%20Bonds/
# Units: % of total (values between 0 and 1)

library(readxl)
library(dplyr)
library(tidyr)
library(httr)

# --- Config ---
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\south_africa_holdings.csv"

base <- "https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Holdings%20of%20Domestic%20Bonds/"

# Month name variants used in filenames
month_names <- list(
  "1"  = c("January"),
  "2"  = c("February", "Feb"),
  "3"  = c("March"),
  "4"  = c("April"),
  "5"  = c("May"),
  "6"  = c("June"),
  "7"  = c("July"),
  "8"  = c("August"),
  "9"  = c("September"),
  "10" = c("October"),
  "11" = c("November"),
  "12" = c("December")
)

all_months <- c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December")

# --- Find and download latest valid file ---
find_and_download <- function() {
  current_year  <- as.integer(format(Sys.Date(), "%Y"))
  current_month <- as.integer(format(Sys.Date(), "%m"))
  
  for (yr in current_year:(current_year - 1)) {
    max_m <- if (yr == current_year) current_month else 12
    for (m in max_m:1) {
      for (mon_name in month_names[[as.character(m)]]) {
        filename <- paste0("Historical%20government%20bond%20holdings%20", mon_name, "%20", yr, ".xlsx")
        url  <- paste0(base, filename)
        resp <- tryCatch(HEAD(url), error = function(e) NULL)
        if (!is.null(resp) && status_code(resp) == 200) {
          cat("Found:", URLdecode(filename), "\n")
          tmp <- tempfile(fileext = ".xlsx")
          download.file(url, tmp, mode = "wb", quiet = TRUE)
          # Validate file
          valid <- tryCatch({ excel_sheets(tmp); TRUE }, error = function(e) FALSE)
          if (valid) {
            cat("File is valid\n")
            return(tmp)
          } else {
            cat("File invalid, trying previous month...\n")
          }
        }
      }
    }
  }
  stop("Could not find a valid holdings file")
}

tmp <- find_and_download()

# --- Read Holdings sheet ---
raw <- read_excel(tmp, sheet = "Holdings", col_names = FALSE)

# --- Parse data ---
col_names <- c("Year", "Month", "Non_Residents", "Banks", "Insurers",
               "Local_Pension_Funds", "Other_Financial", "Other")

holdings <- raw %>%
  setNames(col_names) %>%
  slice(-1) %>%
  mutate(
    Year  = as.character(Year),
    Month = as.character(Month)
  ) %>%
  fill(Year, .direction = "down") %>%
  mutate(
    Year  = as.integer(gsub("[^0-9]", "", Year)),
    Month = trimws(Month)
  ) %>%
  filter(!is.na(Year), !is.na(Month), Month != "") %>%
  mutate(
    Mes_num = match(Month, all_months),
    Periodo = as.Date(paste0(Year, "-", sprintf("%02d", Mes_num), "-01")),
    across(Non_Residents:Other, as.numeric)
  ) %>%
  filter(!is.na(Periodo), !is.na(Non_Residents)) %>%
  filter(Periodo <= Sys.Date()) %>%
  select(Periodo, Non_Residents, Banks, Insurers,
         Local_Pension_Funds, Other_Financial, Other) %>%
  arrange(Periodo)

cat("Rows loaded:", nrow(holdings), "\n")
print(tail(holdings, 6))

# --- Save ---
write.csv(holdings, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")