# Chile UF (Unidad de Fomento) daily values in CLP
# Source: Banco Central de Chile BDE REST API
# Series: F073.UFF.PRE.Z.D (UF diario)

library(httr)
library(jsonlite)
library(dplyr)

output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\chile_uf.csv"

user <- Sys.getenv("BCENTRAL_USER")
pass <- Sys.getenv("BCENTRAL_PASS")

fetch_uf <- function(from, to) {
  url <- paste0(
    "https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx",
    "?user=", URLencode(user, reserved = TRUE),
    "&pass=", URLencode(pass, reserved = TRUE),
    "&firstdate=", format(from, "%Y-%m-%d"),
    "&lastdate=",  format(to,   "%Y-%m-%d"),
    "&timeseries=F073.UFF.PRE.Z.D",
    "&function=GetSeries"
  )
  resp <- GET(url)
  if (status_code(resp) != 200) stop("BCCh API error: ", status_code(resp))
  data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  obs <- data$Series$Obs
  if (is.null(obs) || length(obs) == 0) return(NULL)
  tibble(
    Fecha  = as.Date(obs$indexDateString, format = "%d-%m-%Y"),
    UF_CLP = suppressWarnings(as.numeric(obs$value))
  ) |> filter(!is.na(UF_CLP), is.finite(UF_CLP))
}

# Load existing CSV if present, otherwise fetch from 2020-01-01
if (file.exists(output_path)) {
  existing <- read.csv(output_path) |> mutate(Fecha = as.Date(Fecha))
  last_date <- max(existing$Fecha)
  cat("Existing data through", format(last_date), "\n")
  if (last_date >= Sys.Date() - 1) {
    cat("Already up to date.\n")
    quit(save = "no")
  }
  new_data <- fetch_uf(last_date + 1, Sys.Date())
  result <- bind_rows(existing, new_data) |> arrange(Fecha) |> distinct(Fecha, .keep_all = TRUE)
} else {
  cat("First run: fetching from 2020-01-01\n")
  result <- fetch_uf(as.Date("2020-01-01"), Sys.Date())
}

cat("Total rows:", nrow(result), "\n")
print(tail(result, 5))

write.csv(result, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")
