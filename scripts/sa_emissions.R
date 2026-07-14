# South Africa Auctions - Data Load + Update
# First run: reads from historical Excel
# Subsequent runs: reads from CSV and appends current fiscal year auctions

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# --- Config ---
file_path   <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\Painel - South Africa.xlsx"
sheet_name  <- "Aux_SA"
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\south_africa_licitaciones.csv"

# --- Dynamic fiscal year ---
today      <- Sys.Date()
start_year <- if (month(today) >= 4) year(today) else year(today) - 1
fy         <- paste0(start_year, "-", substr(start_year + 1, 3, 4))
cat("Current fiscal year:", fy, "\n")

# --- Base URLs ---
base <- "https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Historical%20Results"
urls <- list(
  Fixed     = paste0(base, "/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%20", fy, ".xlsx"),
  Inflation = paste0(base, "/Inflation-linked%20bonds/Inflation-linked%20bonds%20auctions%20-%20", fy, ".xlsx"),
  TBill     = paste0(base, "/Treasury%20bills/Treasury%20bill%20auctions%20-%20", fy, ".xlsx"),
  FRN       = paste0(base, "/Floating-rate%20note%20auctions/Floating-rate%20note%20auctions%20-%20", fy, ".xlsx")
)

# --- Helper: convert Excel serial date ---
from_xl_date <- function(x) as.Date(as.numeric(x), origin = "1899-12-30")

# --- Load base data: CSV if exists, otherwise Excel ---
if (file.exists(output_path)) {
  historico <- read.csv(output_path) %>%
    mutate(
      Fecha_Subasta     = as.Date(Fecha_Subasta),
      Fecha_Vencimiento = as.Date(Fecha_Vencimiento),
      Monto_Asignado    = as.numeric(Monto_Asignado),
      Tasa_Corte        = as.numeric(Tasa_Corte),
      Tenor_Anos        = as.numeric(Tenor_Anos),
      Plazo_Dias        = as.numeric(Plazo_Dias)
    )
  cat("Loaded", nrow(historico), "rows from existing CSV\n")
} else {
  raw <- read_excel(file_path, sheet = sheet_name)
  historico <- raw %>%
    select(
      Fecha_Subasta     = Fecha_Subasta,
      Fecha_Vencimiento = Fecha_Vencimiento,
      Bono,
      Tipo_Bono,
      Tenor_Anos,
      Plazo_Dias,
      Monto_Asignado,
      Tasa_Corte
    ) %>%
    mutate(
      Fecha_Subasta     = as.Date(Fecha_Subasta),
      Fecha_Vencimiento = as.Date(Fecha_Vencimiento),
      Monto_Asignado    = as.numeric(Monto_Asignado),
      Tasa_Corte        = as.numeric(Tasa_Corte),
      Tenor_Anos        = as.numeric(Tenor_Anos),
      Plazo_Dias        = as.numeric(Plazo_Dias),
      Tipo_Bono         = if_else(Tipo_Bono == "Floating-Rate", "FRN", Tipo_Bono)
    )
  cat("First run: loaded", nrow(historico), "rows from Excel\n")
}

# -------------------------------------------------------
# PARSE FIXED-RATE BONDS
# -------------------------------------------------------
parse_fixed <- function(url) {
  tmp <- tempfile(fileext = ".xlsx")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  sheets <- excel_sheets(tmp)
  
  map_dfr(sheets, function(sh) {
    raw <- read_excel(tmp, sheet = sh, col_names = FALSE)
    auction_row   <- which(raw[[1]] == "Auction date")
    bonds_row     <- which(raw[[1]] == "Bonds auctioned")
    allocated_row <- which(raw[[1]] == "Total amount allocated (R)")
    yield_row     <- which(raw[[1]] == "Clearing yield")
    if (length(auction_row) == 0 || length(bonds_row) == 0) return(NULL)
    data_cols <- 2:ncol(raw)
    
    # Forward-fill auction dates across merged cells
    auction_vals <- as.numeric(unlist(raw[auction_row, data_cols]))
    for (i in seq_along(auction_vals)) {
      if (is.na(auction_vals[i]) && i > 1) auction_vals[i] <- auction_vals[i-1]
    }
    
    tibble(
      Fecha_Subasta  = from_xl_date(auction_vals),
      Bono           = as.character(unlist(raw[bonds_row, data_cols])),
      Monto_Asignado = as.numeric(unlist(raw[allocated_row, data_cols])),
      Tasa_Corte     = as.numeric(unlist(raw[yield_row, data_cols]))
    ) %>%
      filter(!is.na(Fecha_Subasta), !is.na(Bono), Bono != "NA") %>%
      mutate(
        Tipo_Bono         = "Fixed",
        Fecha_Vencimiento = NA_Date_,
        # Derive tenor from bond name - e.g. R2033 matures in 2033
        Tenor_Anos        = as.numeric(substr(Bono, 2, 5)) - as.numeric(format(Fecha_Subasta, "%Y")),
        Plazo_Dias        = NA_real_
      )
  })
}

# -------------------------------------------------------
# PARSE INFLATION-LINKED BONDS
# -------------------------------------------------------
parse_inflation <- function(url) {
  tmp <- tempfile(fileext = ".xlsx")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  sheets <- excel_sheets(tmp)
  
  map_dfr(sheets, function(sh) {
    raw <- read_excel(tmp, sheet = sh, col_names = FALSE)
    auction_row   <- which(raw[[1]] == "Auction date")
    bonds_row     <- which(raw[[1]] == "Bonds auctioned")
    allocated_row <- which(raw[[1]] == "Total amount allocated (R)")
    yield_row     <- which(raw[[1]] == "Clearing yield")
    if (length(auction_row) == 0 || length(bonds_row) == 0) return(NULL)
    data_cols <- 2:ncol(raw)
    
    # Forward-fill auction dates across merged cells
    auction_vals <- as.numeric(unlist(raw[auction_row, data_cols]))
    for (i in seq_along(auction_vals)) {
      if (is.na(auction_vals[i]) && i > 1) auction_vals[i] <- auction_vals[i-1]
    }
    
    tibble(
      Fecha_Subasta  = from_xl_date(auction_vals),
      Bono           = as.character(unlist(raw[bonds_row, data_cols])),
      Monto_Asignado = as.numeric(unlist(raw[allocated_row, data_cols])),
      Tasa_Corte     = as.numeric(unlist(raw[yield_row, data_cols]))
    ) %>%
      filter(!is.na(Fecha_Subasta), !is.na(Bono), Bono != "NA") %>%
      mutate(
        Tipo_Bono         = "Inflation-Linked",
        Fecha_Vencimiento = NA_Date_,
        # Derive tenor from bond name - e.g. I2038 matures in 2038
        Tenor_Anos        = as.numeric(substr(Bono, 2, 5)) - as.numeric(format(Fecha_Subasta, "%Y")),
        Plazo_Dias        = NA_real_
      )
  })
}

# -------------------------------------------------------
# PARSE TREASURY BILLS
# -------------------------------------------------------
parse_tbill <- function(url) {
  tmp <- tempfile(fileext = ".xlsx")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  sheets <- excel_sheets(tmp)
  
  map_dfr(sheets, function(sh) {
    raw <- read_excel(tmp, sheet = sh, col_names = FALSE)
    auction_row   <- which(apply(raw, 1, function(r) any(grepl("Auction date", r, ignore.case = TRUE))))
    maturity_row  <- which(apply(raw, 1, function(r) any(grepl("Maturity date", r, ignore.case = TRUE))))
    allocated_row <- which(apply(raw, 1, function(r) any(grepl("Total amount allocated", r, ignore.case = TRUE))))
    yield_row     <- which(apply(raw, 1, function(r) any(grepl("Weighted average effective yield", r, ignore.case = TRUE))))
    if (length(auction_row) == 0 || length(yield_row) == 0) return(NULL)
    auction_vals  <- unlist(raw[auction_row, ])
    maturity_vals <- unlist(raw[maturity_row, ])
    yield_vals    <- unlist(raw[yield_row, ])
    alloc_vals    <- unlist(raw[allocated_row, ])
    date_cols <- which(!is.na(as.numeric(auction_vals)))
    if (length(date_cols) == 0) return(NULL)
    n_auctions <- length(date_cols) / 4
    tenors <- rep(c(91, 182, 273, 364), each = n_auctions)
    tibble(
      Fecha_Subasta     = from_xl_date(as.numeric(auction_vals[date_cols])),
      Fecha_Vencimiento = from_xl_date(as.numeric(maturity_vals[date_cols])),
      Monto_Asignado    = as.numeric(alloc_vals[date_cols]) * 1000000,
      Tasa_Corte        = as.numeric(yield_vals[date_cols]),
      Plazo_Dias        = tenors,
      Tenor_Anos        = round(tenors / 365, 2)
    ) %>%
      filter(!is.na(Fecha_Subasta), !is.na(Tasa_Corte), Tasa_Corte > 0) %>%
      mutate(Bono = "T-Bill", Tipo_Bono = "Treasury Bill")
  })
}

# -------------------------------------------------------
# PARSE FRN
# -------------------------------------------------------
parse_frn <- function(url) {
  tmp <- tempfile(fileext = ".xlsx")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  sheets <- excel_sheets(tmp)
  
  map_dfr(sheets, function(sh) {
    raw <- read_excel(tmp, sheet = sh, col_names = FALSE)
    find_val <- function(label) {
      row <- which(apply(raw, 1, function(r) any(grepl(label, r, ignore.case = TRUE))))
      if (length(row) == 0) return(NA)
      as.character(raw[row[1], 2])
    }
    auction_date <- from_xl_date(as.numeric(find_val("Auction Date")))
    bond         <- gsub("\\s*\\(.*", "", find_val("Bond"))
    allocated    <- as.numeric(gsub(",", "", find_val("Total Amount Allocated"))) * 1000000
    margin       <- as.numeric(find_val("Weighted Average \\(Margin\\)"))
    if (is.na(auction_date)) return(NULL)
    tibble(
      Fecha_Subasta     = auction_date,
      Bono              = bond,
      Monto_Asignado    = allocated,
      Tasa_Corte        = margin,
      Tipo_Bono         = "FRN",
      Fecha_Vencimiento = NA_Date_,
      Tenor_Anos        = NA_real_,
      Plazo_Dias        = NA_real_
    )
  })
}

# -------------------------------------------------------
# FETCH AND COMBINE
# -------------------------------------------------------
cat("Downloading current fiscal year files...\n")

nuevos <- bind_rows(
  tryCatch(parse_fixed(urls$Fixed),         error = function(e) { cat("Fixed error:", e$message, "\n"); NULL }),
  tryCatch(parse_inflation(urls$Inflation),  error = function(e) { cat("Inflation error:", e$message, "\n"); NULL }),
  tryCatch(parse_tbill(urls$TBill),          error = function(e) { cat("TBill error:", e$message, "\n"); NULL }),
  tryCatch(parse_frn(urls$FRN),              error = function(e) { cat("FRN error:", e$message, "\n"); NULL })
) %>%
  select(Fecha_Subasta, Fecha_Vencimiento, Bono, Tipo_Bono,
         Tenor_Anos, Plazo_Dias, Monto_Asignado, Tasa_Corte)

cat("New rows fetched:", nrow(nuevos), "\n")

# --- Only append genuinely new rows ---
# For Fixed and Inflation-Linked, Fecha_Vencimiento is NA so use Bono instead
# For T-bills, use Fecha_Vencimiento since Bono is always "T-Bill"
# For FRN, use Bono
nuevos_fixed_infl <- nuevos %>% filter(Tipo_Bono %in% c("Fixed", "Inflation-Linked", "FRN"))
nuevos_tbill      <- nuevos %>% filter(Tipo_Bono == "Treasury Bill")

new_fixed_infl <- anti_join(nuevos_fixed_infl, historico,
                            by = c("Fecha_Subasta", "Tipo_Bono", "Bono"))
new_tbill      <- anti_join(nuevos_tbill, historico,
                            by = c("Fecha_Subasta", "Tipo_Bono", "Fecha_Vencimiento"))

nuevos_clean <- bind_rows(new_fixed_infl, new_tbill)

cat("Genuinely new rows:", nrow(nuevos_clean), "\n")

sa_updated <- bind_rows(historico, nuevos_clean) %>%
  arrange(Fecha_Subasta, Tipo_Bono)

cat("Total rows after update:", nrow(sa_updated), "\n")

# --- Save ---
write.csv(sa_updated, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")