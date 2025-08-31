# rais_apo1a.R


library(dplyr)
library(data.table)

# Geo keys (same names you already use elsewhere)
load("working/ibge/df_codes_ibge.rda")
dft_geo_keys <- as.data.table(df_codes_ibge)[
  , .(CO_MUN6, SG_UF, NM_UF, CO_UF,
      CO_RGIMED, NM_RGIMED, CO_RGINTM, NM_RGIINTM)
]
dft_geo_keys <- unique(dft_geo_keys, by = "CO_MUN6")

# Load CBO 4-digit lookup
load("working/qbq/qbq_ocup_cmento1.rda")
cbo_4dig_lookup <- qbq_ocup_cmento1 %>%
  select(cbo_4dig, cbo_familia) %>%
  distinct() %>%
  filter(!is.na(cbo_4dig) & !is.na(cbo_familia)) %>%
  as.data.table()


# Load and preprocess both years
load("working/rais/2023/rais2023.rda")


# Convert lookups to data.table
setDT(dft_geo_keys)
setDT(cbo_4dig_lookup)

# Process each year separately - never combine raw data
process_rais_year <- function(rais_data, year) {
  # Convert to DT and filter in one step
  dt <- as.data.table(rais_data)[
    get("Vínculo Ativo 31/12") == 1 & 
      !is.na(get("CBO Ocupação 2002")) & 
      get("CBO Ocupação 2002") != "" &
      get("Escolaridade após 2005") %in% c(1:8, -1)  # Include up to superior incompleto
  ]
  
  # Add derived columns
  dt[, `:=`(
    CodCBO = as.character(get("CBO Ocupação 2002")),
    year = year
  )]
  dt[, cbo_4dig := substr(CodCBO, 1, 4)]
  
  # Join geography (data.table style)
  dt <- dft_geo_keys[dt, on = "CO_MUN6"]
  dt <- cbo_4dig_lookup[dt, on = "cbo_4dig"]
  
  # Filter and keep only needed columns
  dt <- dt[!is.na(cbo_familia) & !is.na(CO_UF)][
    , .(year, cbo_4dig, cbo_familia, CO_MUN6, CO_UF, SG_UF, NM_UF, 
        CO_RGIMED, NM_RGIMED, CO_RGINTM, NM_RGIINTM)
  ]
  
  return(dt)
}


# Process 2023 - 
dt_2023 <- process_rais_year(rais2023, 2023)

# Create ALL cubes for 2023 before clearing memory
# Municipal level
employment_2023 <- dt_2023[, .N, by = .(year, cbo_4dig, CO_MUN6)]
mun_totals_2023 <- dt_2023[, .N, by = .(year, CO_MUN6)]

# Regional level - Imediata  
employment_rgimed_2023 <- dt_2023[, .N, by = .(year, cbo_4dig, CO_RGIMED)]
rgimed_totals_2023 <- dt_2023[, .N, by = .(year, CO_RGIMED)]

# Regional level - Intermediária
employment_rgintm_2023 <- dt_2023[, .N, by = .(year, cbo_4dig, CO_RGINTM)]
rgintm_totals_2023 <- dt_2023[, .N, by = .(year, CO_RGINTM)]

# UF level
employment_uf_2023 <- dt_2023[, .N, by = .(year, cbo_4dig, SG_UF)]
uf_totals_2023 <- dt_2023[, .N, by = .(year, SG_UF)]

# National level
br_cbo_2023 <- dt_2023[, .N, by = .(year, cbo_4dig)]
br_total_2023 <- dt_2023[, .N, by = year]

# Save ALL 2023 cubes
save(employment_2023, mun_totals_2023, 
     employment_rgimed_2023, rgimed_totals_2023,
     employment_rgintm_2023, rgintm_totals_2023,
     employment_uf_2023, uf_totals_2023,
     br_cbo_2023, br_total_2023, 
     file = "working/rais/2023/cubes_2023.rda")

rm(dt_2023, rais2023)  # Clear memory
gc()




# Process 2024

load("working/rais/2024/rais2024.rda")
dt_2024 <- process_rais_year(rais2024, 2024)

# Create ALL cubes for 2024
employment_2024 <- dt_2024[, .N, by = .(year, cbo_4dig, CO_MUN6)]
mun_totals_2024 <- dt_2024[, .N, by = .(year, CO_MUN6)]
employment_rgimed_2024 <- dt_2024[, .N, by = .(year, cbo_4dig, CO_RGIMED)]
rgimed_totals_2024 <- dt_2024[, .N, by = .(year, CO_RGIMED)]
employment_rgintm_2024 <- dt_2024[, .N, by = .(year, cbo_4dig, CO_RGINTM)]
rgintm_totals_2024 <- dt_2024[, .N, by = .(year, CO_RGINTM)]
employment_uf_2024 <- dt_2024[, .N, by = .(year, cbo_4dig, SG_UF)]
uf_totals_2024 <- dt_2024[, .N, by = .(year, SG_UF)]
br_cbo_2024 <- dt_2024[, .N, by = .(year, cbo_4dig)]
br_total_2024 <- dt_2024[, .N, by = year]

save(employment_2024, mun_totals_2024,
     employment_rgimed_2024, rgimed_totals_2024,
     employment_rgintm_2024, rgintm_totals_2024,
     employment_uf_2024, uf_totals_2024,
     br_cbo_2024, br_total_2024,
     file = "working/rais/2024/cubes_2024.rda")

rm(dt_2024, rais2024)
gc()