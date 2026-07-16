# South Africa Fixed-Rate Bond Auction Details
# Scrapes bid/cover, yield and new issue premium data from SA Treasury historical files
# First run: pulls all available fiscal years (2010-11 to current)
# Subsequent runs: only refreshes current fiscal year

library(readxl)
library(dplyr)
library(purrr)
library(lubridate)

output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\sa_auction_details.csv"
base_url    <- paste0(
  "https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/",
  "Historical%20Results/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%20"
)

from_xl_date <- function(x) as.Date(as.numeric(x), origin = "1899-12-30")

# --- Parse one fiscal year file, returning per-bond-per-auction rows ---
parse_btc_file <- function(start_year) {
  fy <- paste0(start_year, "-", substr(start_year + 1, 3, 4))
  cat("Trying FY:", fy, "\n")

  tmp <- NULL
  for (ext in c(".xlsx", ".xls")) {
    t  <- tempfile(fileext = ext)
    ok <- tryCatch({
      download.file(paste0(base_url, fy, ext), t, mode = "wb", quiet = TRUE)
      TRUE
    }, error   = function(e) FALSE,
       warning = function(w) FALSE)
    if (ok && file.exists(t) && file.size(t) > 5000) { tmp <- t; break }
  }
  if (is.null(tmp)) { cat("  Not found:", fy, "\n"); return(NULL) }

  sheets <- tryCatch(excel_sheets(tmp), error = function(e) NULL)
  if (is.null(sheets)) return(NULL)

  map_dfr(sheets, function(sh) {
    raw <- tryCatch(
      read_excel(tmp, sheet = sh, col_names = FALSE, .name_repair = "minimal"),
      error = function(e) NULL
    )
    if (is.null(raw) || nrow(raw) < 5 || ncol(raw) < 2) return(NULL)

    col1 <- trimws(as.character(unlist(raw[[1]])))

    find_row <- function(...) {
      patterns <- c(...)
      for (pat in patterns) {
        r <- which(grepl(pat, col1, ignore.case = TRUE))
        if (length(r) > 0) return(r[1])
      }
      NA_integer_
    }

    r_auction  <- find_row("^Auction date")
    r_bonds    <- find_row("^Bonds auctioned")
    r_alloc    <- find_row("^Total amount allocated")
    r_num_bids <- find_row("^Total number of bids")
    r_tot_bids <- find_row("^Total amount of (all )?bids")
    r_clear    <- find_row("^Clearing yield")
    r_best     <- find_row("^Best bid")
    r_worst    <- find_row("^Worst bid")
    r_btc      <- find_row("^Bid to cover ratio")

    if (any(is.na(c(r_auction, r_bonds, r_alloc, r_btc)))) return(NULL)

    data_cols <- 2:ncol(raw)

    # Forward-fill auction dates across merged/blank cells
    auction_vals <- as.numeric(unlist(raw[r_auction, data_cols]))
    for (i in seq_along(auction_vals)) {
      if (is.na(auction_vals[i]) && i > 1) auction_vals[i] <- auction_vals[i - 1]
    }

    get_row <- function(r) {
      if (is.na(r)) rep(NA_real_, length(data_cols))
      else suppressWarnings(as.numeric(unlist(raw[r, data_cols])))
    }

    tibble(
      Fecha_Subasta  = from_xl_date(auction_vals),
      Bono           = as.character(unlist(raw[r_bonds, data_cols])),
      Monto_Asignado = get_row(r_alloc),
      Num_Bids       = get_row(r_num_bids),
      Total_Bids     = get_row(r_tot_bids),
      Tasa_Corte     = get_row(r_clear),
      Mejor_Oferta   = get_row(r_best),
      Peor_Oferta    = get_row(r_worst),
      BTC            = get_row(r_btc),
      FY             = fy
    ) |>
      filter(
        !is.na(Fecha_Subasta),
        !is.na(Bono), Bono != "NA", nchar(Bono) >= 3,
        !is.na(Tasa_Corte), Tasa_Corte > 0,
        !is.na(BTC), BTC > 0
      )
  })
}

# --- Determine which years to pull ---
today     <- Sys.Date()
cur_start <- if (month(today) >= 4) year(today) else year(today) - 1

if (file.exists(output_path)) {
  hist <- read.csv(output_path) |>
    mutate(Fecha_Subasta = as.Date(Fecha_Subasta))
  cat("Loaded", nrow(hist), "rows from existing CSV\n")
  nuevos <- parse_btc_file(cur_start)
} else {
  hist   <- NULL
  cat("First run: pulling all fiscal years 2010-11 to", paste0(cur_start, "-", substr(cur_start + 1, 3, 4)), "\n")
  nuevos <- map_dfr(2010:cur_start, parse_btc_file)
}

cat("Rows fetched:", nrow(nuevos), "\n")

if (!is.null(hist) && nrow(nuevos) > 0) {
  nuevos_clean <- anti_join(nuevos, hist, by = c("Fecha_Subasta", "Bono"))
} else {
  nuevos_clean <- nuevos
}

cat("Genuinely new rows:", nrow(nuevos_clean), "\n")

result <- bind_rows(hist, nuevos_clean) |>
  arrange(Fecha_Subasta, Bono)

cat("Total rows after update:", nrow(result), "\n")
write.csv(result, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")
