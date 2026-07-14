# Mexico Licitaciones - Data Load + API Update
# First run: reads from historical Excel
# Subsequent runs: reads from CSV and appends new auctions from Banxico API

library(readxl)
library(dplyr)
library(httr)
library(jsonlite)

# --- Config ---
file_path   <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\Painel - Mexico.xlsx"
sheet_name  <- "Aux_MX_Correct"
output_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\mexico_licitaciones.csv"
token       <- Sys.getenv("BANXICO_TOKEN")

# --- Load base data: CSV if exists, otherwise Excel ---
if (file.exists(output_path)) {
  mexico <- read.csv(output_path) %>%
    mutate(
      Fecha            = as.Date(Fecha),
      Monto            = as.numeric(Monto),
      Tasa             = as.numeric(Tasa),
      Precio_Ponderado = as.numeric(Precio_Ponderado)
    )
  cat("Loaded", nrow(mexico), "rows from existing CSV\n")
} else {
  raw <- read_excel(file_path, sheet = sheet_name)
  mexico <- raw %>%
    select(
      Fecha,
      Instrumento,
      Tenor,
      Plazo,
      Monto,
      Tasa,
      Precio_Ponderado = `Precio Ponderado`
    ) %>%
    mutate(
      Fecha            = as.Date(Fecha),
      Monto            = as.numeric(Monto),
      Tasa             = as.numeric(Tasa),
      Precio_Ponderado = as.numeric(Precio_Ponderado)
    )
  cat("First run: loaded", nrow(mexico), "rows from Excel\n")
}

# --- Banxico API: series IDs by instrument ---
series_map <- tribble(
  ~Instrumento,  ~Plazo_id,    ~Monto_id,    ~Tasa_id,     ~es_precio,
  "Cetes",       "SF43935",    "SF43937",    "SF43936",    FALSE,
  "Cetes",       "SF43938",    "SF43940",    "SF43939",    FALSE,
  "Cetes",       "SF43941",    "SF43943",    "SF43942",    FALSE,
  "Cetes",       "SF43944",    "SF43946",    "SF43945",    FALSE,
  "Cetes",       "SF349778",   "SF349780",   "SF349785",   FALSE,
  "Bono M",      "SF43882",    "SF43884",    "SF43883",    FALSE,
  "Bono M",      "SF43885",    "SF43887",    "SF43886",    FALSE,
  "Bono M",      "SF44945",    "SF44947",    "SF44946",    FALSE,
  "Bono M",      "SF44070",    "SF44072",    "SF44071",    FALSE,
  "Bono M",      "SF45383",    "SF45385",    "SF45384",    FALSE,
  "Bono M",      "SF60689",    "SF60690",    "SF60696",    FALSE,
  "Udibono",     "SF61593",    "SF61594",    "SF61592",    FALSE,
  "Udibono",     "SF43926",    "SF43928",    "SF43927",    FALSE,
  "Udibono",     "SF43923",    "SF43925",    "SF43924",    FALSE,
  "Udibono",     "SF46957",    "SF46959",    "SF46958",    FALSE,
  "Udibono",     "SF46960",    "SF46962",    "SF46961",    FALSE,
  "Bondes D",    "SF60714",    "SF60715",    "SF60650",    TRUE,
  "Bondes D",    "SF339743",   "SF339744",   "SF339745",   TRUE,
  "Bondes D",    "SF60668",    "SF60669",    "SF60651",    TRUE,
  "Bondes D",    "SF60673",    "SF60674",    "SF60652",    TRUE,
  "Bondes F",    "SF341518",   "SF341522",   "SF341526",   TRUE,
  "Bondes F",    "SF341519",   "SF341523",   "SF341527",   TRUE,
  "Bondes F",    "SF341520",   "SF341524",   "SF341528",   TRUE,
  "Bondes F",    "SF345517",   "SF345521",   "SF345525",   TRUE,
  "Bondes F",    "SF341521",   "SF341525",   "SF341529",   TRUE,
)

# --- Function to fetch one series ---
fetch_serie <- function(id, token) {
  url  <- paste0("https://www.banxico.org.mx/SieAPIRest/service/v1/series/", id, "/datos/oportuno")
  resp <- GET(url, add_headers("Bmx-Token" = token))
  if (status_code(resp) != 200) return(NULL)
  data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  obs  <- data$bmx$series$datos[[1]]
  if (is.null(obs) || nrow(obs) == 0) return(NULL)
  tibble(
    fecha = as.Date(obs$fecha, format = "%d/%m/%Y"),
    valor = suppressWarnings(as.numeric(obs$dato))
  )
}

# --- Fetch latest data ---
cat("Fetching latest data from Banxico API...\n")

nuevos <- lapply(1:nrow(series_map), function(i) {
  row <- series_map[i, ]
  plazo <- fetch_serie(row$Plazo_id, token)
  monto <- fetch_serie(row$Monto_id, token)
  tasa  <- fetch_serie(row$Tasa_id,  token)
  
  if (is.null(plazo) || is.null(monto)) return(NULL)
  
  result <- plazo %>%
    rename(Plazo = valor) %>%
    left_join(monto %>% rename(Monto = valor), by = "fecha") %>%
    filter(!is.na(Monto), !is.na(Plazo))
  
  if (!is.null(tasa)) {
    result <- result %>% left_join(tasa %>% rename(Tasa_raw = valor), by = "fecha")
  } else {
    result <- result %>% mutate(Tasa_raw = NA_real_)
  }
  
  if (nrow(result) == 0) return(NULL)
  if (!"Tasa_raw" %in% names(result)) result$Tasa_raw <- NA_real_
  result$Tasa_raw <- as.numeric(result$Tasa_raw)
  
  es_precio <- as.logical(row$es_precio)
  
  result %>%
    mutate(
      Instrumento      = row$Instrumento,
      Tenor            = if_else(Plazo < 365, "CP", "LP"),
      Tasa             = if_else(es_precio, NA_real_, Tasa_raw),
      Precio_Ponderado = if_else(es_precio, Tasa_raw, NA_real_),
      Fecha            = fecha
    ) %>%
    select(Fecha, Instrumento, Tenor, Plazo, Monto, Tasa, Precio_Ponderado)
}) %>% bind_rows()

cat("New rows fetched:", nrow(nuevos), "\n")

# --- Only append genuinely new rows ---
nuevos_clean <- anti_join(nuevos, mexico,
                          by = c("Fecha", "Instrumento", "Plazo"))

cat("Genuinely new rows:", nrow(nuevos_clean), "\n")

mexico <- bind_rows(mexico, nuevos_clean) %>%
  arrange(Fecha, Instrumento)

cat("Total rows:", nrow(mexico), "\n")

# --- Save ---
write.csv(mexico, output_path, row.names = FALSE)
cat("Saved to", output_path, "\n")