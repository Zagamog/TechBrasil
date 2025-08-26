# rais_apo2RGIMED.R
library(data.table)

# Load base municipal index
load("working/rais/indices/dft_apl_MUN_2324.rda")

# Load geo lookup if not already available
dft_geo_keys <- unique(as.data.table(df_codes_ibge)[, .(CO_MUN6, SG_UF, NM_UF, CO_RGIMED, NM_RGIMED)], by = "CO_MUN6")

# Merge with geo keys to get CO_RGIMED
dft_apl_RGIMED <- merge(dft_apl_MUN_2324, dft_geo_keys, by = "CO_MUN6", all.x = TRUE)

# --------------------------
# Aggregate to Região Imediata level
# --------------------------
dft_apl_RGIMED <- dft_apl_RGIMED[, .(
  E_rgimed_cbo = sum(E_mun_cbo, na.rm = TRUE),
  E_rgimed     = sum(E_mun, na.rm = TRUE),
  E_br_cbo = unique(E_br_cbo),
  E_br     = unique(E_br)
), by = .(year, CO_RGIMED, NM_RGIMED, SG_UF, cbo_4dig)]

# Compute metrics
dft_apl_RGIMED[, p_rgimed := E_rgimed_cbo / E_rgimed]
dft_apl_RGIMED[, p_br := E_br_cbo / E_br]
dft_apl_RGIMED[, LQ := p_rgimed / p_br]
dft_apl_RGIMED[, RCA1 := as.integer(LQ >= 1)]

# --------------------------
# Filter: Min. Employment ≥ 300 AND LQ ≥ 1.25 for BOTH years
# --------------------------
dft_apl_RGIMED[, eligible := (E_rgimed_cbo >= 300 & LQ >= 1.25)]

# Check persistence (appears in BOTH 2023 and 2024)
dft_apl_RGIMED_persistent <- dft_apl_RGIMED[eligible == TRUE,
                                            .(persist = .N), by = .(CO_RGIMED, cbo_4dig)][persist == 2]

# Final APL table with 2024 metrics
dft_apl_RGIMED_final <- merge(
  dft_apl_RGIMED_persistent, 
  dft_apl_RGIMED[year == 2024, .(CO_RGIMED, NM_RGIMED, SG_UF, cbo_4dig, E_rgimed_cbo, LQ)], 
  by = c("CO_RGIMED", "cbo_4dig"),
  all.x = TRUE
)

# Add occupation family labels
load("working/qbq/qbq_ocup_cmento1.rda")
cbo_lookup <- unique(qbq_ocup_cmento1[, c("cbo_4dig", "cbo_familia")])
dft_apl_RGIMED_final <- merge(dft_apl_RGIMED_final, cbo_lookup, by = "cbo_4dig", all.x = TRUE)

# Sort by state, then região imediata, then employment
setorder(dft_apl_RGIMED_final, SG_UF, NM_RGIMED, -E_rgimed_cbo)

# Save result
save(dft_apl_RGIMED_final, file = "working/rais/indices/dft_apl_RGIMED_final.rda")