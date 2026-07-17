# run_all.R
# Master script to run all data pipeline scripts sequentially
# Logs results to run_log.txt on the file server

log_path <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\run_log.txt"

log <- function(msg) {
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", msg)
  cat(line, "\n")
  write(line, log_path, append = TRUE)
}

run_script <- function(path) {
  script_name <- basename(path)
  tryCatch({
    local(source(path, local = TRUE))
    log(paste("OK:", script_name))
  }, error = function(e) {
    log(paste("ERROR:", script_name, "-", e$message))
  })
}

log("--- Run started ---")

base <- "\\\\jgprjfileserver\\Compartilhadas\\Summer\\lbonjean\\fixed income\\data\\"

run_script(paste0(base, "chile_uf.R"))
run_script(paste0(base, "chile_emissions.R"))
run_script(paste0(base, "chile_treasury.R"))
run_script(paste0(base, "chile_gdp.R"))
run_script(paste0(base, "mexico_emissions.R"))
run_script(paste0(base, "mexico_treasury.R"))
run_script(paste0(base, "mexico_gdp.R"))
run_script(paste0(base, "sa_emissions.R"))
run_script(paste0(base, "sa_auction_details.R"))
run_script(paste0(base, "sa_treasury.R"))
run_script(paste0(base, "sa_gdp.R"))
run_script(paste0(base, "colombia_licitaciones.R"))
run_script(paste0(base, "colombia_treasury.R"))
run_script(paste0(base, "colombia_gdp.R"))
run_script(paste0(base, "chile_gdp_usd.R"))
run_script(paste0(base, "chile_debt.R"))
run_script(paste0(base, "chile_holdings.R"))
run_script(paste0(base, "mexico_debt.R"))
run_script(paste0(base, "mexico_holdings.R"))
run_script(paste0(base, "mexico_maturity.R"))
run_script(paste0(base, "south_africa_debt.R"))
run_script(paste0(base, "south_africa_holdings.R"))
run_script(paste0(base, "colombia_debt.R"))


log("--- Run finished ---")