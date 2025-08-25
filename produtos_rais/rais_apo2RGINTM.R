# rais_apo2RGINTM.R
library(data.table)

# Load base municipal index
load("working/rais/indices/dft_apl_MUN_2324.rda")

# Load geo lookup if not already available
if (!exists("dft_geo_keys")) {
  load("working/ibge/df_codes_ibge.rda")
  dft_geo_keys <- unique(as.data.table(df_codes_ibge)[, .(CO_MUN6, SG_UF, NM_UF, CO_RGINTM, NM_RGIINTM)], by = "CO_MUN6")
}

# Merge with geo keys to get CO_RGINTM
dft_apl_RGINTM <- merge(dft_apl_MUN_2324, dft_geo_keys, by = "CO_MUN6", all.x = TRUE)

# --------------------------
# Aggregate to Região Intermediária level
# --------------------------
dft_apl_RGINTM <- dft_apl_RGINTM[, .(
  E_rgintm_cbo = sum(E_mun_cbo, na.rm = TRUE),
  E_rgintm     = sum(E_mun, na.rm = TRUE),
  E_br_cbo = unique(E_br_cbo),
  E_br     = unique(E_br)
), by = .(year, CO_RGINTM, NM_RGIINTM, SG_UF, cbo_4dig)]

# Compute metrics
dft_apl_RGINTM[, p_rgintm := E_rgintm_cbo / E_rgintm]
dft_apl_RGINTM[, p_br := E_br_cbo / E_br]
dft_apl_RGINTM[, LQ := p_rgintm / p_br]
dft_apl_RGINTM[, RCA1 := as.integer(LQ >= 1)]

# --------------------------
# Filter: Min. Employment ≥ 400 AND LQ ≥ 1.25 for BOTH years
# --------------------------
dft_apl_RGINTM[, eligible := (E_rgintm_cbo >= 400 & LQ >= 1.25)]

# Check persistence (appears in BOTH 2023 and 2024)
dft_apl_RGINTM_persistent <- dft_apl_RGINTM[eligible == TRUE,
                                            .(persist = .N), by = .(CO_RGINTM, cbo_4dig)][persist == 2]

# Final APL table with 2024 metrics
dft_apl_RGINTM_final <- merge(
  dft_apl_RGINTM_persistent, 
  dft_apl_RGINTM[year == 2024, .(CO_RGINTM, NM_RGIINTM, SG_UF, cbo_4dig, E_rgintm_cbo, LQ)], 
  by = c("CO_RGINTM", "cbo_4dig"),
  all.x = TRUE
)

# Add occupation family labels
load("working/qbq/qbq_ocup_cmento1.rda")
cbo_lookup <- unique(qbq_ocup_cmento1[, c("cbo_4dig", "cbo_familia")])
dft_apl_RGINTM_final <- merge(dft_apl_RGINTM_final, cbo_lookup, by = "cbo_4dig", all.x = TRUE)

# Sort by state, then região intermediária, then employment
setorder(dft_apl_RGINTM_final, SG_UF, NM_RGIINTM, -E_rgintm_cbo)

# Save result
save(dft_apl_RGINTM_final, file = "working/rais/indices/dft_apl_RGINTM_final.rda")