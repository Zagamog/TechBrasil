# rais_apo2UF.R

library(data.table)


# Load base index
load("D:/Country/Brazil/TechBrazil/working/rais/indices/dft_ubiquity_CBO_2324.rda")
load("D:/Country/Brazil/TechBrazil/working/rais/indices/dft_diversity_MUN_2324.rda")
load("D:/Country/Brazil/TechBrazil/working/rais/indices/dft_occ_complexity_2324.rda")
load("D:/Country/Brazil/TechBrazil/working/rais/indices/dft_mun_complexity_2324.rda")
load("D:/Country/Brazil/TechBrazil/working/rais/indices/dft_apl_MUN_2324.rda")

# Load geo lookup if not already available
if (!exists("dft_geo_keys")) {
  load("working/ibge/df_codes_ibge.rda")
  dft_geo_keys <- unique(as.data.table(df_codes_ibge)[, .(CO_MUN6, SG_UF, NM_UF)], by = "CO_MUN6")
}

# Merge SG_UF for aggregation
dft_apl_UF <- merge(dft_apl_MUN_2324, dft_geo_keys, by = "CO_MUN6", all.x = TRUE)

# --------------------------
# Aggregate to UF level
# --------------------------
dft_apl_UF <- dft_apl_UF[, .(
  E_uf_cbo = sum(E_mun_cbo, na.rm = TRUE),
  E_uf     = sum(E_mun, na.rm = TRUE),
  E_br_cbo = unique(E_br_cbo),
  E_br     = unique(E_br)
), by = .(year, SG_UF, cbo_4dig)]

# Compute metrics
dft_apl_UF[, p_uf := E_uf_cbo / E_uf]
dft_apl_UF[, p_br := E_br_cbo / E_br]
dft_apl_UF[, LQ := p_uf / p_br]
dft_apl_UF[, RCA1 := as.integer(LQ >= 1)]

# --------------------------
# Filter: Min. Employment ≥ 500 AND RCA ≥ 1 for BOTH years
# --------------------------
# Mark eligible per year
dft_apl_UF[, eligible := (E_uf_cbo >= 500 & LQ >= 1)]

# Check persistence (appears in BOTH 2023 and 2024)
dft_apl_UF_persistent <- dft_apl_UF[eligible == TRUE,
                                    .(persist = .N), by = .(SG_UF, cbo_4dig)][persist == 2]

# Final APL table (add readable label if available)
dft_apl_UF_final <- merge(
  dft_apl_UF_persistent, 
  dft_apl_UF[year == 2024, .(SG_UF, cbo_4dig, E_uf_cbo, LQ)], 
  by = c("SG_UF", "cbo_4dig"),
  all.x = TRUE
)

# Optional: add occupation label
load("working/qbq/qbq_ocup_cmento1.rda")  # gives 'qbq_ocup_cmento1' with cbo_4dig, cbo_familia
cbo_lookup <- unique(qbq_ocup_cmento1[, c("cbo_4dig", "cbo_familia")])
dft_apl_UF_final <- merge(dft_apl_UF_final, cbo_lookup, by = "cbo_4dig", all.x = TRUE) %>% arrange(SG_UF)

# Save result
save(dft_apl_UF_final, file = "working/rais/indices/dft_apl_UF_final.rda")



