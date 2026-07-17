# Chile Holdings — Non-Resident participation in central government debt
# Source: Ministerio de Hacienda (IRO) — "Non-Resident holdings of total debt"
# The page URL serves the .xlsx directly (redirects to a signed CDN link).
# Quarterly; we take "Total Debt % Non-Resident" (row 18) → Non_Residents share.

library(httr)
library(readxl)
library(dplyr)

output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_holdings.csv"
url <- "https://www.hacienda.cl/english/investor-relations-office/economics-statistics-information/debt-statistics/non-resident-holdings-of-total-debt"

tmp <- tempfile(fileext = ".xlsx")
GET(url, write_disk(tmp, overwrite = TRUE), user_agent("Mozilla/5.0"))
if (file.size(tmp) < 5000) stop("Download failed or file too small")

raw <- suppressMessages(read_excel(tmp, sheet = "Trimestral",
                                   col_names = FALSE, col_types = "text"))

data_cols <- 2:ncol(raw)

# Row 5 = years (merged, forward-fill); Row 6 = quarters (I/II/III/IV)
years <- as.character(unlist(raw[5, data_cols]))
for (i in seq_along(years)) if ((is.na(years[i]) || years[i] == "NA") && i > 1) years[i] <- years[i - 1]
years <- suppressWarnings(as.integer(years))

quarters <- trimws(as.character(unlist(raw[6, data_cols])))

# Row 18 = "Total Debt — % Non-Resident" (fraction 0-1)
nr_share <- suppressWarnings(as.numeric(as.character(unlist(raw[18, data_cols]))))

q_month <- c("I" = 3L, "II" = 6L, "III" = 9L, "IV" = 12L)

result <- tibble(
  Year    = years,
  Quarter = quarters,
  Non_Residents = nr_share
) |>
  filter(!is.na(Year), Quarter %in% names(q_month),
         !is.na(Non_Residents), is.finite(Non_Residents)) |>
  mutate(
    Mes     = q_month[Quarter],
    Periodo = as.Date(sprintf("%d-%02d-01", Year, Mes)),
    # move to last day of the quarter-end month so month() is correct
    Periodo = as.Date(format(Periodo + 40, "%Y-%m-01")) - 1,
    Residents = 1 - Non_Residents
  ) |>
  select(Periodo, Non_Residents, Residents) |>
  arrange(Periodo)

cat("Total rows:", nrow(result), "\n")
print(tail(result, 6))

write.csv(result, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")
