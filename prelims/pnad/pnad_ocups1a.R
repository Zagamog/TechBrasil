# R/download_pnadc.R

# — install/load
if (!requireNamespace("PNADcIBGE", quietly = TRUE)) {
  install.packages("PNADcIBGE")
}
library(PNADcIBGE)
library(purrr)

# extend timeout so big downloads don’t abort
options(timeout = 3600)

# the variables you want from PNAD-C Q2
vars <- c(
  "V2009","VD3004","VD4001","VD4002","VD4003","VD4004A","VD4009",
  "VD4011","VD4012","VD4013","VD4019","VD4020","VD4032",
  "V3019A","V3020B","V3020C","V3021A","V3022C","V3022D","V3022E",
  "V3023A","V4019","V4010","V3026","V3026A","V3029","V3029A","V3032"
)

# helper to fetch & cache one year’s Q2 into your working folder
download_trimester <- function(year,
                               out_dir = "D:/Country/Brazil/TechBrazil/working/pnad") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_file <- file.path(out_dir, paste0("pnadc_q2_", year, ".rds"))
  if (!file.exists(out_file)) {
    message("Downloading PNAD-C Q2 ", year, " …")
    df <- get_pnadc(
      year     = year,
      topic    = 2,
      design   = FALSE,
      vars     = vars,
      deflator = FALSE,
      labels   = TRUE
    )
    saveRDS(df, out_file)
  } else {
    message("Cached file already exists: ", out_file)
  }
  invisible(out_file)
}

# public wrapper: download a vector of years
# e.g. download_pnadc_trim2(2016:2019)
download_pnadc_trim2 <- function(years,
                                 out_dir = "D:/Country/Brazil/TechBrazil/rawdata/pnad") {
  map_chr(years,
          ~ download_trimester(.x, out_dir = out_dir))
}

# Example usage:
# source("R/download_pnadc.R")
# download_pnadc_trim2(2016:2019)  # download 2016–19 Q2
# download_pnadc_trim2(2022:2024)  # download 2022–24 Q2


# If you source this file, you can call:
 download_pnadc_trim2(2016:2019)   # for 2016–19
# download_pnadc_trim2(2022:2024)   # for 2022–24
