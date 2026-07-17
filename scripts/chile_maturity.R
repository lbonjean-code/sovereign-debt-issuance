# Chile Debt Maturity Profile — CLP and USD amortization schedules
# Source: "Amort Divida CLP Chile.xlsx" / "Amort Divida USD Chile.xlsx"
# These are manual exports (no stable public URL found) — to refresh, replace
# the files in the "chile" folder with an updated export using the same names.
# Each file: monthly rows (Period = "Mon-YYYY"), columns Total / Governments /
# Interest. "Governments" = principal (amortization); Total = Governments + Interest.
# We aggregate principal by calendar year — this is a redemption/maturity
# profile (principal only), consistent with the other countries' maturity charts.

library(readxl)
library(dplyr)

base <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\chile\\"

clp_path <- paste0(base, "Amort Divida CLP Chile.xlsx")
usd_path <- paste0(base, "Amort Divida USD Chile.xlsx")

out_clp <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_maturity_clp.csv"
out_usd <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_maturity_usd.csv"

parse_amort <- function(path) {
  raw <- read_excel(path, sheet = 1, col_names = TRUE, col_types = "text") |>
    filter(Period != "Total")

  raw |>
    mutate(
      Year        = as.integer(substr(Period, nchar(Period) - 3, nchar(Period))),
      Principal   = suppressWarnings(as.numeric(Governments)),
      Interest    = suppressWarnings(as.numeric(Interest))
    ) |>
    filter(!is.na(Year)) |>
    group_by(Year) |>
    summarise(
      Principal = sum(Principal, na.rm = TRUE),
      Interest  = sum(Interest,  na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(Year)
}

clp <- parse_amort(clp_path)
usd <- parse_amort(usd_path)

cat("CLP: ", nrow(clp), "years,", min(clp$Year), "-", max(clp$Year), "\n")
print(tail(clp, 5))
cat("\nUSD: ", nrow(usd), "years,", min(usd$Year), "-", max(usd$Year), "\n")
print(tail(usd, 5))

write.csv(clp, out_clp, row.names = FALSE)
write.csv(usd, out_usd, row.names = FALSE)
cat("\nSaved to", out_clp, "\n")
cat("Saved to", out_usd, "\n")
