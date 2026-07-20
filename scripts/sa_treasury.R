# South Africa Treasury Cash Balance
# Reads historical data from local Excel and appends new monthly PDFs
# Source: National Treasury Section 32 monthly releases - Table 4

library(readxl)
library(dplyr)
library(pdftools)
library(lubridate)

# --- Config ---
file_path   <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\Painel - South Africa.xlsx"
sheet_name  <- "Treasury"
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\south_africa_treasury.csv"

# --- Load historical ---
raw <- read_excel(file_path, sheet = sheet_name, skip = 2, col_names = FALSE)

historico <- raw %>%
  select(Periodo = 1, Saldo = 2) %>%
  filter(!is.na(Periodo), !is.na(Saldo)) %>%
  mutate(
    Periodo = as.Date(as.POSIXct(as.numeric(Periodo), origin = "1970-01-01")),
    Saldo   = as.numeric(Saldo)
  )

cat("Historical rows loaded:", nrow(historico), "\n")

# --- Fiscal month mapping ---
# Month number in URL: Nov=01, Dec=02, Jan=03, Feb=04, Mar=05,
#                      Apr=06, May=07, Jun=08, Jul=09, Aug=10, Sep=11, Oct=12
fiscal_month_num <- function(m) {
  c(Jan=3, Feb=4, Mar=5, Apr=6, May=7, Jun=8,
    Jul=9, Aug=10, Sep=11, Oct=12, Nov=1, Dec=2)[m]
}

# --- Function to extract closing balance from PDF ---
extract_closing_balance <- function(url) {
  tryCatch({
    tmp <- tempfile(fileext = ".pdf")
    download.file(url, tmp, mode = "wb", quiet = TRUE)
    txt <- pdf_text(tmp)
    
    # Find closing balance line
    lines <- unlist(strsplit(paste(txt, collapse = "\n"), "\n"))
    closing_line <- lines[grep("^Closing balance", lines)[1]]
    
    if (is.na(closing_line)) return(NA)
    
    # Extract number - remove spaces used as thousand separators
    # Split on 2+ spaces to separate numbers
    parts <- strsplit(trimws(closing_line), "\\s{2,}")[[1]]
    parts <- parts[parts != "Closing balance"]
    nums  <- as.numeric(gsub(" ", "", parts))
    nums  <- nums[!is.na(nums) & nums > 1000]

    # Column order in Table 4: current FY [Budget estimate | May | Year to date]
    # then prior FY [Preliminary | May | YTD]. We want the month's actual value,
    # which is the 2nd column (nums[2]) — NOT the Budget estimate (nums[1]).
    if (length(nums) < 2) return(NA)
    return(nums[2])
    
  }, error = function(e) {
    cat("Error fetching", url, ":", e$message, "\n")
    return(NA)
  })
}

# --- Determine which months to fetch ---
# Get last date in historical data
last_period <- tail(historico$Periodo, 1)
last_date  <- max(historico$Periodo, na.rm = TRUE)
next_date  <- last_date %m+% months(1)
today       <- Sys.Date()

cat("Last historical period:", last_period, "\n")
cat("Fetching from:", format(next_date, "%b-%Y"), "\n")

# --- Fetch new months ---
nuevos <- list()
current_date <- next_date

while (current_date <= today) {
  yr  <- substr(year(current_date), 3, 4)
  mon <- fiscal_month_num(format(current_date, "%b"))
  mon_str <- sprintf("%02d", mon)
  
  url     <- paste0("https://www.treasury.gov.za/comm_media/press/monthly/", yr, mon_str, "/Table%204.pdf")
  periodo <- format(current_date, "%b-%y")
  
  cat("Trying:", periodo, "->", url, "\n")
  
  saldo <- extract_closing_balance(url)
  
  if (!is.na(saldo)) {
    nuevos[[length(nuevos) + 1]] <- data.frame(Periodo = periodo, Saldo = saldo)
    cat("  Got:", saldo, "\n")
  } else {
    cat("  Not available yet\n")
  }
  
  current_date <- current_date %m+% months(1)
}

# --- Combine ---
if (length(nuevos) > 0) {
  nuevos_df   <- bind_rows(nuevos)
  sa_treasury <- bind_rows(historico, nuevos_df)
  cat("New rows added:", nrow(nuevos_df), "\n")
} else {
  sa_treasury <- historico
  cat("No new rows to add\n")
}

cat("Total rows:", nrow(sa_treasury), "\n")

# --- Save ---
write.csv(sa_treasury, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")