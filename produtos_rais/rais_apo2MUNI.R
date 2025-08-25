# rais_apo2MUNI.R
library(data.table)

# Load the base municipal indices from rais_apo1b.R
load("working/rais/indices/dft_apl_MUN_2324.rda")

# Load geo lookup for labels
if (!exists("dft_geo_keys")) {
  load("working/ibge/df_codes_ibge.rda")
  dft_geo_keys <- unique(as.data.table(df_codes_ibge)[, .(CO_MUN6, SG_UF, NM_UF, NM_RGIMED, NM_RGIINTM)], by = "CO_MUN6")
}

# --------------------------
# Filter: Min. Employment ≥ 100 AND LQ ≥ 1.25 for persistence check
# --------------------------
dft_apl_MUN_2324[, eligible := (E_mun_cbo >= 100 & LQ >= 1.25)]

# Check persistence (appears in BOTH 2023 and 2024)
dft_apl_MUN_persistent <- dft_apl_MUN_2324[eligible == TRUE,
                                           .(persist = .N), by = .(CO_MUN6, cbo_4dig)][persist == 2]

# Final APL table with 2024 metrics
dft_apl_MUN_final <- merge(
  dft_apl_MUN_persistent, 
  dft_apl_MUN_2324[year == 2024, .(CO_MUN6, cbo_4dig, E_mun_cbo, LQ)], 
  by = c("CO_MUN6", "cbo_4dig"),
  all.x = TRUE
)

# Add geographic and occupation labels
dft_apl_MUN_final <- merge(dft_apl_MUN_final, dft_geo_keys, by = "CO_MUN6", all.x = TRUE)

# Add occupation family labels
load("working/qbq/qbq_ocup_cmento1.rda")
cbo_lookup <- unique(qbq_ocup_cmento1[, c("cbo_4dig", "cbo_familia")])
dft_apl_MUN_final <- merge(dft_apl_MUN_final, cbo_lookup, by = "cbo_4dig", all.x = TRUE)

# Sort by state, then municipality, then employment
setorder(dft_apl_MUN_final, SG_UF, NM_UF, -E_mun_cbo)

# Save result
save(dft_apl_MUN_final, file = "working/rais/indices/dft_apl_MUN_final.rda")