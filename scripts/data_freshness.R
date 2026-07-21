# Data freshness monitor
# Reports the latest DATA date inside each pipeline CSV (not the file mtime —
# the hardened scripts keep the file "fresh" even when they fall back to old
# data). Flags any series whose latest data point is older than its expected
# cadence allows, i.e. a source that has silently stopped updating.
# Appends a summary to run_log.txt and writes data_freshness.csv.
#
# To tune: adjust max_age (days) per series in the `cfg` table below.

suppressMessages({library(dplyr); library(readr); library(tibble)})

data_dir <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\"
log_path <- paste0(data_dir, "run_log.txt")
out_path <- paste0(data_dir, "data_freshness.csv")

log_line <- function(msg) { cat(msg, "\n"); write(msg, log_path, append = TRUE) }

# file | date column | cadence label | max acceptable age (days) before WARN
cfg <- tribble(
  ~file,                             ~date_col,          ~cadence,       ~max_age,
  "chile_uf.csv",                    "Fecha",            "daily",           5,
  "mexico_udi.csv",                  "Fecha",            "daily",           5,
  "chile_licitaciones.csv",          "Fecha_Licitacion", "weekly",         16,
  "mexico_licitaciones.csv",         "Fecha",            "weekly",         16,
  "south_africa_licitaciones.csv",   "Fecha_Subasta",    "weekly",         28,
  "colombia_licitaciones.csv",       "Fecha_Subasta",    "weekly",         16,
  "sa_auction_details.csv",          "Fecha_Subasta",    "weekly",         30,
  "mexico_holdings.csv",             "Periodo",          "weekly",         16,
  "chile_treasury.csv",              "Periodo",          "monthly",        90,
  "mexico_treasury.csv",             "Periodo",          "monthly",        60,
  "south_africa_treasury.csv",       "Periodo",          "monthly-lag",    95,
  "colombia_treasury.csv",           "Periodo",          "monthly",        60,
  "mexico_debt.csv",                 "Periodo",          "monthly",       100,
  "colombia_debt.csv",               "Periodo",          "monthly",        80,
  "south_africa_holdings.csv",       "Periodo",          "monthly",        80,
  "colombia_holdings.csv",           "Periodo",          "monthly",        80,
  "mexico_avg_maturity.csv",         "Periodo",          "monthly",        95,
  "colombia_entidades_publicas.csv", "Fecha",            "monthly",        80,
  "chile_gdp.csv",                   "Periodo",          "quarterly",     250,
  "mexico_gdp.csv",                  "Periodo",          "quarterly",     250,
  "colombia_gdp.csv",                "Periodo",          "quarterly",     250,
  "chile_debt.csv",                  "Periodo",          "quarterly",     250,
  "chile_holdings.csv",              "Periodo",          "quarterly",     180,
  "south_africa_gdp.csv",            "Anio",             "annual",        430,
  "south_africa_debt.csv",           "Ano_Fiscal",       "annual",        430,
  "chile_gdp_usd.csv",               "Anio",             "annual",        500
)

today <- Sys.Date()
cur_yr <- as.integer(format(today, "%Y"))

# Latest data date from a column, handling ISO dates, plain years, and "YYYY/YY".
# Explicit format => as.Date returns NA (not an error) for non-date strings.
# Future-dated rows (data-entry errors) are ignored so they don't distort "latest".
parse_latest <- function(vals) {
  s <- trimws(as.character(vals))
  s <- s[!is.na(s) & s != "" & s != "NA"]
  if (length(s) == 0) return(as.Date(NA))
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  d <- d[!is.na(d) & d <= today]
  if (length(d) > 0) return(max(d))
  yr <- suppressWarnings(as.integer(substr(s, 1, 4)))
  yr <- yr[!is.na(yr) & yr >= 2000 & yr <= cur_yr]
  if (length(yr) > 0) return(as.Date(paste0(max(yr), "-12-31")))
  as.Date(NA)
}

res <- bind_rows(lapply(seq_len(nrow(cfg)), function(i) {
  r <- cfg[i, ]
  p <- paste0(data_dir, r$file)
  latest <- if (!file.exists(p)) as.Date(NA) else tryCatch({
    df <- suppressWarnings(read_csv(p, show_col_types = FALSE, progress = FALSE))
    if (!(r$date_col %in% names(df))) as.Date(NA) else parse_latest(df[[r$date_col]])
  }, error = function(e) as.Date(NA))
  age    <- if (is.na(latest)) NA_integer_ else as.integer(today - latest)
  status <- if (!file.exists(p)) "MISSING"
            else if (is.na(latest)) "NODATE"
            else if (age > r$max_age) "WARN"
            else "OK"
  data.frame(file = r$file, latest = latest, age_days = age,
             max_age = r$max_age, cadence = r$cadence, status = status,
             stringsAsFactors = FALSE)
}))

# --- Append human-readable summary to run_log (WARN/problems first) ---
ord <- res |> arrange(match(status, c("WARN", "MISSING", "NODATE", "OK")),
                       dplyr::desc(age_days))
log_line(paste0("--- Data freshness (", format(today), ") ---"))
for (i in seq_len(nrow(ord))) {
  x <- ord[i, ]
  log_line(sprintf("  %-7s %-32s latest %-11s %5s / %dd  [%s]",
                   x$status, x$file,
                   if (is.na(x$latest)) "NA" else format(x$latest),
                   if (is.na(x$age_days)) "?" else paste0(x$age_days, "d"),
                   x$max_age, x$cadence))
}
n_bad <- sum(res$status != "OK")
log_line(sprintf("--- Freshness: %d OK, %d need attention ---",
                 sum(res$status == "OK"), n_bad))

# --- Machine-readable output ---
res$checked_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
write.csv(res, out_path, row.names = FALSE)
cat("Saved", out_path, "\n")
