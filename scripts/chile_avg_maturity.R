# Chile Weighted Average Maturity — Deuda Bruta del Gobierno Central
# Source: manual reconstruction "madurez_trimestral_chile_2018_2025.xlsx",
# sheet "Anclas", column "Madurez exacta (años)" — year-end (Dic) anchor values
# computed bond-by-bond from DIPRES quarterly gross-debt reports. Not an official
# ODP series (ODP publishes only the current figure, 10.4y); Dic-2025 reconstructed
# 10.41 vs 10.4 official. MANUAL source — to refresh, replace the workbook in the
# "chile" folder with an updated version and re-run. NOT in run_all.R.

library(readxl)
library(dplyr)
library(stringr)

xl  <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\chile\\madurez_trimestral_chile_2018_2025.xlsx"
out <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_avg_maturity.csv"

raw <- suppressMessages(read_excel(xl, sheet = "Anclas", skip = 4,
                                   col_names = FALSE, col_types = "text"))

df <- raw |>
  transmute(Ancla = `...1`,
            Madurez_Anos = suppressWarnings(as.numeric(`...2`))) |>
  filter(str_detect(Ancla, "^Dic"), !is.na(Madurez_Anos)) |>
  mutate(Ano = as.integer(str_extract(Ancla, "[0-9]{4}"))) |>
  select(Ano, Madurez_Anos) |>
  arrange(Ano)

cat("Rows:", nrow(df), "\n")
print(as.data.frame(df))

write.csv(df, out, row.names = FALSE)
cat("Saved to", out, "\n")
