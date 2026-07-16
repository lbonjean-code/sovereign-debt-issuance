# South Africa Auctions — Full Rebuild from SA Treasury Website
# Pulls all historical data: Fixed, Inflation-Linked, T-Bills, FRN (no Switches)
# First run (no CSV): downloads everything; subsequent runs: current FY only

library(readxl)
library(dplyr)
library(purrr)
library(lubridate)

output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\south_africa_licitaciones.csv"

BASE <- "https://investor.treasury.gov.za/Debt%20Operations%20and%20Data/Historical%20Results"

# ── Hardcoded file lists (exact names, accounting for SA Treasury naming chaos) ─

FIXED_FILES <- list(
  list(fy="2010-13", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202010-13.xls"),  fmt="long"),
  list(fy="2012-13", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202012-13.xls"),  fmt="sheet"),
  list(fy="2013-14", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202013-14.xls"),  fmt="sheet"),
  list(fy="2014-15", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202014-15.xls"),  fmt="sheet"),
  list(fy="2015-16", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202015-16.xls"),  fmt="sheet"),
  list(fy="2016-17", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202016-17.xls"),  fmt="sheet"),
  list(fy="2017-18", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202017-18.xls"),  fmt="sheet"),
  list(fy="2018-19", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202018-19.xls"),  fmt="sheet"),
  list(fy="2019-20", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202019-20.xls"),  fmt="sheet"),
  list(fy="2020-21", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202020-21.xlsx"), fmt="sheet"),
  list(fy="2021-22", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202021-22.xlsx"), fmt="sheet"),
  list(fy="2022-23", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202022-23.xlsx"), fmt="sheet"),
  list(fy="2023-24", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202023-24.xlsx"), fmt="sheet"),
  list(fy="2024-25", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202024-25.xlsx"), fmt="sheet"),
  list(fy="2025-26", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202025-26.xlsx"), fmt="sheet"),
  list(fy="2026-27", url=paste0(BASE,"/Fixed-rate%20bonds/Fixed-rate%20bond%20auctions%20-%202026-27.xlsx"), fmt="sheet")
)

IL_FILES <- list(
  # Note: 2010-13 and 2012-13 have a double space in the filename
  list(fy="2010-13", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20%20bond%20auctions%20-%202010-13.xlsx"), fmt="sheet"),
  list(fy="2012-13", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20%20bond%20auctions%20-%202012-13.xls"),  fmt="sheet"),
  list(fy="2013-14", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20bond%20auctions%20-%202013-14.xls"),     fmt="sheet"),
  list(fy="2014-15", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20bond%20auctions%20-%202014-15.xls"),     fmt="sheet"),
  list(fy="2015-16", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20bond%20auctions%20-%202015-16.xls"),     fmt="sheet"),
  list(fy="2016-17", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20bond%20auctions%20-%202016-17.xls"),     fmt="sheet"),
  # 2017-18 onwards uses capital L in "Linked"
  list(fy="2017-18", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-Linked%20bond%20auctions%20-%202017-18.xls"),     fmt="sheet"),
  list(fy="2018-19", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-Linked%20bond%20auctions%20-%202018-19.xls"),     fmt="sheet"),
  list(fy="2019-20", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-Linked%20bond%20auctions%20-%202019-20.xls"),     fmt="sheet"),
  list(fy="2020-21", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-Linked%20bond%20auctions%20-%202020-21.xlsx"),    fmt="sheet"),
  list(fy="2021-22", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20bond%20auctions%20-%202021-22.xlsx"),    fmt="sheet"),
  list(fy="2022-23", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20bond%20auctions%20-%202022-23.xlsx"),    fmt="sheet"),
  list(fy="2023-24", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20bond%20auctions%20-%202023-24.xlsx"),    fmt="sheet"),
  list(fy="2024-25", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20bond%20auctions%20-%202024-25.xlsx"),    fmt="sheet"),
  # 2025-26 and 2026-27 use plural "bonds auctions"
  list(fy="2025-26", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20bonds%20auctions%20-%202025-26.xlsx"),   fmt="sheet"),
  list(fy="2026-27", url=paste0(BASE,"/Inflation-linked%20bonds/Inflation-linked%20bonds%20auctions%20-%202026-27.xlsx"),   fmt="sheet")
)

TBILL_FILES <- list(
  list(fy="2010-11", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202010-11.xls")),
  list(fy="2011-12", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202011-12.xls")),
  list(fy="2012-13", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202012-13.xls")),
  list(fy="2013-14", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202013-14.xls")),
  list(fy="2014-15", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202014-15.xls")),
  list(fy="2015-16", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202015-16.xls")),
  list(fy="2016-17", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202016-17.xls")),
  list(fy="2017-18", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202017-18.xls")),
  list(fy="2018-19", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202018-19.xls")),
  list(fy="2019-20", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202019-20.xls")),
  list(fy="2020-21", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202020-21.xls")),
  list(fy="2021-22", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202021-22.xls")),
  list(fy="2022-23", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202022-23.xls")),
  list(fy="2023-24", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202023-24.xlsx")),
  list(fy="2024-25", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202024-25.xlsx")),
  list(fy="2025-26", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202025-26.xlsx")),
  list(fy="2026-27", url=paste0(BASE,"/Treasury%20bills/Treasury%20bill%20auctions%20-%202026-27.xlsx"))
)

FRN_FILES <- list(
  list(fy="2022-23", url=paste0(BASE,"/Floating-rate%20note%20auctions/Floating-rate%20note%20auctions%20-%202022-23.xls")),
  list(fy="2023-24", url=paste0(BASE,"/Floating-rate%20note%20auctions/Floating-rate%20note%20auctions%20-%202023-24.xls")),
  list(fy="2024-25", url=paste0(BASE,"/Floating-rate%20note%20auctions/Floating-rate%20note%20auctions%20-%202024-25.xls")),
  list(fy="2025-26", url=paste0(BASE,"/Floating-rate%20note%20auctions/Floating-rate%20note%20auctions%20-%202025-26.xlsx")),
  list(fy="2026-27", url=paste0(BASE,"/Floating-rate%20note%20auctions/Floating-rate%20note%20auctions%20-%202026-27.xlsx"))
)

# ── Helpers ──────────────────────────────────────────────────────────────────

from_xl_date <- function(x) {
  if (is.na(x) || x == "NA") return(NA_Date_)
  num <- suppressWarnings(as.numeric(x))
  if (!is.na(num) && num > 30000 && num < 60000)
    return(as.Date(num, origin = "1899-12-30"))
  s <- trimws(as.character(x))
  for (fmt in c("%d %B %Y", "%d %b %Y", "%d-%b-%y", "%B %d, %Y", "%d/%m/%Y")) {
    d <- suppressWarnings(as.Date(s, format = fmt))
    if (!is.na(d)) return(d)
  }
  NA_Date_
}

fix_yield <- function(y) {
  # Yields stored as decimals (0.085) in older files → convert to %
  ifelse(!is.na(y) & y < 1 & y > 0, y * 100, y)
}

safe_download <- function(url, ext) {
  tmp <- tempfile(fileext = ext)
  ok  <- tryCatch(
    { download.file(url, tmp, mode = "wb", quiet = TRUE); TRUE },
    error   = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (!ok || !file.exists(tmp) || file.size(tmp) < 2000) return(NULL)
  tmp
}

# ── Parser: 2010-13 Fixed-rate (long format, one sheet) ─────────────────────

parse_fixed_long <- function(url) {
  cat("  [long format]\n")
  tmp <- safe_download(url, ".xls")
  if (is.null(tmp)) return(NULL)

  df <- tryCatch(
    read_excel(tmp, col_names = FALSE, col_types = "text"),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) < 2) return(NULL)

  # Row 1 = header; columns: Bono, Coupon, Venc, Fecha, Settle, Oferta, Asignado,
  #   Yield, Bids, BTC, MktRate, Spread, NonComp
  df <- df[-1, ]

  bono     <- trimws(as.character(unlist(df[[1]])))
  venc_raw <- as.character(unlist(df[[3]]))
  fecha_raw <- as.character(unlist(df[[4]]))
  asignado <- suppressWarnings(as.numeric(unlist(df[[7]])))
  yield    <- fix_yield(suppressWarnings(as.numeric(unlist(df[[8]]))))

  fecha    <- map(fecha_raw, from_xl_date) |> unlist() |> as.Date(origin = "1970-01-01")
  venc     <- as.Date(suppressWarnings(as.numeric(venc_raw)), origin = "1899-12-30")

  # Normalise bond codes: "207" → "R207"; already "R2023" stays
  bono <- ifelse(grepl("^R\\d", bono), bono, paste0("R", bono))

  tibble(
    Fecha_Subasta     = fecha,
    Fecha_Vencimiento = venc,
    Bono              = bono,
    Tipo_Bono         = "Fixed",
    Monto_Asignado    = asignado * 1e6,   # R'm → Rands
    Tasa_Corte        = yield,
    Tenor_Anos        = suppressWarnings(as.numeric(format(venc, "%Y")) - as.numeric(format(fecha, "%Y"))),
    Plazo_Dias        = suppressWarnings(as.numeric(venc - fecha))
  ) |>
    filter(
      !is.na(Fecha_Subasta), !is.na(Bono), nchar(Bono) >= 3,
      Bono != "R0", !is.na(Monto_Asignado), Monto_Asignado > 0
    )
}

# ── Parser: Fixed-rate and IL monthly sheets (2012-13 onwards) ──────────────

parse_bond_sheets <- function(url, ext, tipo) {
  tmp <- safe_download(url, ext)
  if (is.null(tmp)) return(NULL)

  sheets <- tryCatch(excel_sheets(tmp), error = function(e) NULL)
  if (is.null(sheets)) return(NULL)

  map_dfr(sheets, function(sh) {
    raw <- tryCatch(
      read_excel(tmp, sheet = sh, col_names = FALSE, col_types = "text"),
      error = function(e) NULL
    )
    if (is.null(raw) || nrow(raw) < 5 || ncol(raw) < 2) return(NULL)

    col1 <- trimws(as.character(unlist(raw[[1]])))

    r_auction <- which(grepl("^Auction date", col1, ignore.case = TRUE))[1]
    r_bonds   <- which(grepl("^Bonds auctioned", col1, ignore.case = TRUE))[1]
    # Use "Total amount allocated" for both Fixed and IL (not "Amount on offer")
    r_alloc   <- which(grepl("^Total amount allocated", col1, ignore.case = TRUE))[1]
    r_yield   <- which(grepl("^Clearing yield", col1, ignore.case = TRUE))[1]

    if (any(is.na(c(r_auction, r_bonds, r_alloc)))) return(NULL)

    data_cols <- 2:ncol(raw)

    # Auction dates — forward-fill merged cells
    date_raw <- as.character(unlist(raw[r_auction, data_cols]))
    for (i in seq_along(date_raw)) {
      if ((is.na(date_raw[i]) || date_raw[i] == "NA") && i > 1)
        date_raw[i] <- date_raw[i - 1]
    }
    dates <- map(date_raw, from_xl_date) |> unlist() |> as.Date(origin = "1970-01-01")

    # Amounts — detect if header says "million" (older files)
    amount_label <- col1[r_alloc]
    is_millions  <- grepl("million", amount_label, ignore.case = TRUE) |
                    grepl("R'm", amount_label, fixed = TRUE)
    amounts <- suppressWarnings(as.numeric(as.character(unlist(raw[r_alloc, data_cols]))))
    if (is_millions) {
      pos_vals <- amounts[!is.na(amounts) & amounts > 0]
      # If any value >= 1e6 the file mislabels "R million" but stores raw Rands
      if (length(pos_vals) > 0 && max(pos_vals) < 1e6) amounts <- amounts * 1e6
    }

    yields <- if (!is.na(r_yield)) {
      fix_yield(suppressWarnings(as.numeric(as.character(unlist(raw[r_yield, data_cols])))))
    } else rep(NA_real_, length(data_cols))

    bonds <- trimws(as.character(unlist(raw[r_bonds, data_cols])))

    tibble(
      Fecha_Subasta  = dates,
      Bono           = bonds,
      Monto_Asignado = amounts,
      Tasa_Corte     = yields
    ) |>
      filter(
        !is.na(Fecha_Subasta), !is.na(Bono), Bono != "NA", nchar(Bono) >= 3,
        !is.na(Monto_Asignado), Monto_Asignado > 0
      ) |>
      mutate(
        Tipo_Bono         = tipo,
        Fecha_Vencimiento = NA_Date_,
        Tenor_Anos        = suppressWarnings(as.numeric(substr(Bono, 2, 5)) - year(Fecha_Subasta)),
        Plazo_Dias        = NA_real_
      )
  })
}

# ── Parser: Treasury Bills (all years) ───────────────────────────────────────

parse_tbill <- function(url, ext) {
  tmp <- safe_download(url, ext)
  if (is.null(tmp)) return(NULL)

  sheets <- tryCatch(excel_sheets(tmp), error = function(e) NULL)
  if (is.null(sheets)) return(NULL)

  map_dfr(sheets, function(sh) {
    raw <- tryCatch(
      read_excel(tmp, sheet = sh, col_names = FALSE, col_types = "text"),
      error = function(e) NULL
    )
    if (is.null(raw) || nrow(raw) < 5) return(NULL)

    col1 <- trimws(as.character(unlist(raw[[1]])))
    all_vals <- as.character(unlist(raw))

    r_auction  <- which(grepl("^Auction date", col1, ignore.case = TRUE))[1]
    r_maturity <- which(grepl("^Maturity date", col1, ignore.case = TRUE))[1]
    r_alloc    <- which(grepl("^Total amount allocated", col1, ignore.case = TRUE))[1]
    r_yield    <- which(grepl("^Weighted average effective yield", col1, ignore.case = TRUE))[1]

    if (any(is.na(c(r_auction, r_alloc, r_yield)))) return(NULL)

    # Each column group = one tenor; determine tenor from header rows
    # Row 2 typically has "91-day", "182-day", etc. spread across columns
    header_row <- as.character(unlist(raw[2, ]))
    tenor_vals  <- suppressWarnings(as.integer(gsub("[^0-9]", "", header_row)))

    all_dates    <- as.character(unlist(raw[r_auction, ]))
    date_cols    <- which(!is.na(suppressWarnings(as.numeric(all_dates))) |
                          grepl("\\d{1,2} [A-Za-z]+ \\d{4}", all_dates))
    date_cols    <- date_cols[date_cols > 1]
    if (length(date_cols) == 0) return(NULL)

    # Amounts in R million → × 1e6
    amount_label <- col1[r_alloc]
    mult <- if (grepl("million", amount_label, ignore.case = TRUE)) 1e6 else 1

    map_dfr(date_cols, function(dc) {
      dt       <- from_xl_date(as.character(raw[r_auction, dc]))
      mat_dt   <- if (!is.na(r_maturity)) from_xl_date(as.character(raw[r_maturity, dc])) else NA_Date_
      alloc    <- suppressWarnings(as.numeric(as.character(raw[r_alloc, dc]))) * mult
      yld      <- fix_yield(suppressWarnings(as.numeric(as.character(raw[r_yield, dc]))))
      # Tenor from header; fallback from maturity - auction
      ten_days <- if (!is.na(mat_dt) && !is.na(dt)) as.numeric(mat_dt - dt) else NA_real_
      # Round to nearest standard tenor
      ten_std  <- tenor_vals[dc]
      if (is.na(ten_std) || ten_std == 0) ten_std <- suppressWarnings(round(ten_days / 91) * 91)

      tibble(
        Fecha_Subasta     = dt,
        Fecha_Vencimiento = mat_dt,
        Bono              = "T-Bill",
        Tipo_Bono         = "Treasury Bill",
        Monto_Asignado    = alloc,
        Tasa_Corte        = yld,
        Tenor_Anos        = round(ten_days / 365, 2),
        Plazo_Dias        = ten_days
      )
    }) |>
      filter(!is.na(Fecha_Subasta), !is.na(Monto_Asignado), Monto_Asignado > 0,
             !is.na(Tasa_Corte), Tasa_Corte > 0)
  })
}

# ── Parser: Floating-rate notes (all years) ───────────────────────────────────

parse_frn <- function(url, ext) {
  tmp <- safe_download(url, ext)
  if (is.null(tmp)) return(NULL)

  sheets <- tryCatch(excel_sheets(tmp), error = function(e) NULL)
  if (is.null(sheets)) return(NULL)

  map_dfr(sheets, function(sh) {
    raw <- tryCatch(
      read_excel(tmp, sheet = sh, col_names = FALSE, col_types = "text"),
      error = function(e) NULL
    )
    if (is.null(raw) || nrow(raw) < 4 || ncol(raw) < 2) return(NULL)

    find_val <- function(pattern) {
      r <- which(grepl(pattern, trimws(as.character(unlist(raw[[1]]))), ignore.case = TRUE))
      if (length(r) == 0) return(NA_character_)
      as.character(raw[r[1], 2])
    }

    dt       <- from_xl_date(find_val("Auction [Dd]ate"))
    bond_raw <- find_val("^Bond")
    bond     <- trimws(gsub("\\s*\\(.*", "", bond_raw))
    alloc    <- suppressWarnings(as.numeric(gsub(",", "", find_val("Total Amount Allocated")))) * 1e6
    margin   <- suppressWarnings(as.numeric(find_val("Weighted Average.*Margin")))

    if (is.na(dt) || is.na(bond) || bond == "" || is.na(alloc)) return(NULL)

    tibble(
      Fecha_Subasta     = dt,
      Fecha_Vencimiento = NA_Date_,
      Bono              = bond,
      Tipo_Bono         = "FRN",
      Monto_Asignado    = alloc,
      Tasa_Corte        = margin,
      Tenor_Anos        = NA_real_,
      Plazo_Dias        = NA_real_
    )
  })
}

# ── Helper: run a list of file specs through a parser ────────────────────────

run_files <- function(file_list, parser_fn, ...) {
  map_dfr(file_list, function(spec) {
    cat("Fetching", spec$fy, "...\n")
    ext <- if (grepl("\\.xlsx$", spec$url)) ".xlsx" else ".xls"
    tryCatch(
      parser_fn(spec$url, ext, ...),
      error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
    )
  })
}

# ── Current FY detection ──────────────────────────────────────────────────────

today     <- Sys.Date()
cur_start <- if (month(today) >= 4) year(today) else year(today) - 1
cur_fy    <- paste0(cur_start, "-", substr(cur_start + 1, 3, 4))
cat("Current fiscal year:", cur_fy, "\n")

# ── Main logic ────────────────────────────────────────────────────────────────

select_cols <- function(df) {
  df |> select(Fecha_Subasta, Fecha_Vencimiento, Bono, Tipo_Bono,
               Tenor_Anos, Plazo_Dias, Monto_Asignado, Tasa_Corte)
}

if (file.exists(output_path)) {
  # ── Incremental update: only current FY ────────────────────────────────────
  cat("Existing CSV found — refreshing current FY only:", cur_fy, "\n")
  historico <- read.csv(output_path) |>
    mutate(
      Fecha_Subasta     = as.Date(Fecha_Subasta),
      Fecha_Vencimiento = as.Date(Fecha_Vencimiento),
      Monto_Asignado    = as.numeric(Monto_Asignado),
      Tasa_Corte        = as.numeric(Tasa_Corte),
      Tenor_Anos        = as.numeric(Tenor_Anos),
      Plazo_Dias        = as.numeric(Plazo_Dias)
    )
  cat("Loaded", nrow(historico), "rows from CSV\n")

  is_cur <- function(lst) lst$fy == cur_fy

  nuevos <- bind_rows(
    run_files(Filter(is_cur, FIXED_FILES),
              function(url, ext, ...) {
                spec <- Filter(is_cur, FIXED_FILES)[[1]]
                if (spec$fmt == "long") parse_fixed_long(url)
                else parse_bond_sheets(url, ext, "Fixed")
              }),
    run_files(Filter(is_cur, IL_FILES),
              function(url, ext, ...) parse_bond_sheets(url, ext, "Inflation-Linked")),
    run_files(Filter(is_cur, TBILL_FILES),
              function(url, ext, ...) parse_tbill(url, ext)),
    run_files(Filter(is_cur, FRN_FILES),
              function(url, ext, ...) parse_frn(url, ext))
  ) |> select_cols()

  cat("New rows fetched:", nrow(nuevos), "\n")

  # Fixed/IL/FRN: dedup on date + bono + tipo; T-bills: also maturity date
  new_bond  <- nuevos |> filter(Tipo_Bono != "Treasury Bill") |>
    anti_join(historico, by = c("Fecha_Subasta", "Tipo_Bono", "Bono"))
  new_tbill <- nuevos |> filter(Tipo_Bono == "Treasury Bill") |>
    anti_join(historico, by = c("Fecha_Subasta", "Tipo_Bono", "Fecha_Vencimiento"))
  nuevos_clean <- bind_rows(new_bond, new_tbill)

  cat("Genuinely new rows:", nrow(nuevos_clean), "\n")
  result <- bind_rows(historico, nuevos_clean) |> arrange(Fecha_Subasta, Tipo_Bono)

} else {
  # ── First run: download everything ─────────────────────────────────────────
  cat("No CSV found — performing full historical download\n")

  cat("\n--- Fixed-rate bonds ---\n")
  fixed_long  <- parse_fixed_long(FIXED_FILES[[1]]$url)
  fixed_sheet <- run_files(
    FIXED_FILES[-1],
    function(url, ext, ...) parse_bond_sheets(url, ext, "Fixed")
  )
  fixed_all <- bind_rows(fixed_long, fixed_sheet) |> select_cols()

  cat("\n--- Inflation-linked bonds ---\n")
  il_all <- run_files(
    IL_FILES,
    function(url, ext, ...) parse_bond_sheets(url, ext, "Inflation-Linked")
  ) |> select_cols()

  cat("\n--- Treasury bills ---\n")
  tbill_all <- run_files(
    TBILL_FILES,
    function(url, ext, ...) parse_tbill(url, ext)
  ) |> select_cols()

  cat("\n--- Floating-rate notes ---\n")
  frn_all <- run_files(
    FRN_FILES,
    function(url, ext, ...) parse_frn(url, ext)
  ) |> select_cols()

  result <- bind_rows(fixed_all, il_all, tbill_all, frn_all) |>
    filter(!is.na(Fecha_Subasta), !is.na(Monto_Asignado), Monto_Asignado > 0) |>
    # Remove duplicates (2010-13 and 2012-13 overlap for Fixed)
    distinct(Fecha_Subasta, Bono, Tipo_Bono, Plazo_Dias, .keep_all = TRUE) |>
    arrange(Fecha_Subasta, Tipo_Bono)

  cat("\nTotal rows:", nrow(result), "\n")
  cat("By Tipo_Bono:\n")
  print(table(result$Tipo_Bono))
}

write.csv(result, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")
