# Chile Auction Details - BTC + Yield data
# Reads yearly Excel files from Hacienda Chile (Licitaciones YYYY.xlsx)
# 2021 format: header row 4, data from row 7, Tasa at col 12, Fecha at col 6 (has extra Fecha Emisión)
# 2022+ format: header row 5, data from row 8, Tasa at col 11, Fecha at col 5

library(readxl)
library(dplyr)

base        <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\chile\\"
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_auction_details.csv"

parse_year <- function(yr) {
  path <- paste0(base, "Licitaciones ", yr, ".xlsx")
  if (!file.exists(path)) return(NULL)

  is_old  <- (yr <= 2022)  # 2021+2022 both have extra Fecha Emisión at col 4
  skip_n  <- 6             # both formats: data starts at row 7
  # Old (2021-2022) cols: 1=Bono,2=ISIN,3=Moneda,4=FechaEmision,5=FechaVenc,6=FechaLic,7=Cupo,8=Demandado,9=Asignado,12=Tasa
  # New (2023+)    cols: 1=Bono,2=ISIN,3=Moneda,4=FechaVenc,5=FechaLic,6=Cupo,7=Demandado,8=Asignado,11=Tasa

  raw <- read_excel(path, sheet = "Licitaciones Bonos", skip = skip_n,
                    col_names = FALSE, col_types = "text")

  if (is_old) {
    raw <- raw |> select(
      Bono              = `...1`,
      Moneda            = `...3`,
      Fecha_Vencimiento = `...5`,
      Fecha_Licitacion  = `...6`,
      Cupo              = `...7`,
      Monto_Demandado   = `...8`,
      Monto_Asignado    = `...9`,
      Tasa_Corte        = `...12`
    ) |>
    mutate(Moneda = case_when(
      grepl("peso", Moneda, ignore.case = TRUE) ~ "CLP",
      grepl("UF",   Moneda, ignore.case = TRUE) ~ "UF",
      TRUE ~ Moneda
    ))
  } else {
    raw <- raw |> select(
      Bono              = `...1`,
      Moneda            = `...3`,
      Fecha_Vencimiento = `...4`,
      Fecha_Licitacion  = `...5`,
      Cupo              = `...6`,
      Monto_Demandado   = `...7`,
      Monto_Asignado    = `...8`,
      Tasa_Corte        = `...11`
    )
  }

  from_xl_date <- function(x) {
    n <- suppressWarnings(as.numeric(x))
    as.Date(ifelse(!is.na(n), as.integer(n), NA_integer_), origin = "1899-12-30")
  }

  raw |>
    filter(!is.na(Bono), nchar(trimws(Bono)) > 0) |>
    mutate(
      Fecha_Licitacion  = from_xl_date(Fecha_Licitacion),
      Fecha_Vencimiento = from_xl_date(Fecha_Vencimiento),
      across(c(Cupo, Monto_Demandado, Monto_Asignado, Tasa_Corte),
             ~suppressWarnings(as.numeric(.x))),
      BTC = ifelse(!is.na(Monto_Demandado) & !is.na(Cupo) & Cupo > 0,
                   Monto_Demandado / Cupo, NA_real_)
    ) |>
    filter(!is.na(Fecha_Licitacion), !is.na(Monto_Asignado), Monto_Asignado > 0,
           Fecha_Licitacion <= Sys.Date(), Fecha_Vencimiento > Fecha_Licitacion)
}

all_data <- lapply(2021:2026, parse_year)
result   <- bind_rows(all_data) |> arrange(Fecha_Licitacion)

cat("Total rows:", nrow(result), "\n")
print(tail(result, 5))

write.csv(result, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")
