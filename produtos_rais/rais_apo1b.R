# rais_apo1b.R

library(data.table)

# ---- Load cubes (both years) ----
load("working/rais/2023/cubes_2023.rda")  # employment_2023, mun_totals_2023, br_cbo_2023, br_total_2023
load("working/rais/2024/cubes_2024.rda")  # employment_2024, mun_totals_2024, br_cbo_2024, br_total_2024

# ---- Stack years ----
employment <- rbindlist(list(employment_2023, employment_2024), use.names=TRUE)
mun_totals <- rbindlist(list(mun_totals_2023, mun_totals_2024), use.names=TRUE)
br_cbo     <- rbindlist(list(br_cbo_2023, br_cbo_2024), use.names=TRUE)
br_total   <- rbindlist(list(br_total_2023, br_total_2024), use.names=TRUE)

# Clean
rm(employment_2023, employment_2024, mun_totals_2023, mun_totals_2024,
   br_cbo_2023, br_cbo_2024, br_total_2023, br_total_2024)
gc()

# Set keys
setkey(employment, year, CO_MUN6, cbo_4dig)
setkey(mun_totals, year, CO_MUN6)
setkey(br_cbo,     year, cbo_4dig)
setkey(br_total,   year)

# --- Safer joins ---

# Step 1: Join employment with municipal total
X1 <- merge(employment, mun_totals, by = c("year", "CO_MUN6"), suffixes = c("", ".mun"))

# Step 2: Join with Brazil-level total per CBO
X2 <- merge(X1, br_cbo, by = c("year", "cbo_4dig"), suffixes = c("", ".brcbo"))

# Step 3: Join with Brazil overall total
X3 <- merge(X2, br_total, by = "year", suffixes = c("", ".brtot"))

# --- Rename columns explicitly ---
setnames(X3, c("N", "N.mun", "N.brcbo", "N.brtot"),
         c("E_mun_cbo", "E_mun", "E_br_cbo", "E_br"))

# Assign to final object
dft_apl_MUN_2324 <- X3


# ---- Compute metrics ----
dft_apl_MUN_2324[, p_mun := E_mun_cbo / E_mun]
dft_apl_MUN_2324[, p_br  := E_br_cbo  / E_br]
dft_apl_MUN_2324[, LQ := p_mun / p_br]
dft_apl_MUN_2324[, RCA_squash := (LQ - 1) / (LQ + 1)]
dft_apl_MUN_2324[, logRCA := log(LQ)]
dft_apl_MUN_2324[, RCA1 := as.integer(LQ >= 1)]

# ---- Diversity per municipality & year ----
dft_diversity_MUN_2324 <- dft_apl_MUN_2324[, .(
  HHI     = sum((E_mun_cbo / E_mun)^2),
  Shannon = -sum(fifelse(p_mun > 0, p_mun * log(p_mun), 0))
), by = .(year, CO_MUN6)]

# ---- Ubiquity per occupation & year ----
dft_ubiquity_CBO_2324 <- dft_apl_MUN_2324[, .(
  ubiquity = sum(RCA1, na.rm = TRUE)
), by = .(year, cbo_4dig)]

# ---- Complexity proxies ----
dft_occ_complexity_2324 <- dft_apl_MUN_2324[RCA1 == 1, .(
  avg_mun_div = mean(
    dft_diversity_MUN_2324[.SD, on = .(year, CO_MUN6), x.HHI],
    na.rm = TRUE
  )
), by = .(year, cbo_4dig), .SDcols = c("year", "CO_MUN6")]


dft_mun_complexity_2324 <- dft_apl_MUN_2324[RCA1 == 1, .(
  ubi_mean = mean(
    dft_ubiquity_CBO_2324[.SD, on = .(year, cbo_4dig), ubiquity],
    na.rm = TRUE
  )
), by = .(year, CO_MUN6), .SDcols = c("year", "cbo_4dig")]


dir.create("working/rais/indices", recursive = TRUE, showWarnings = FALSE)

# Save each artifact separately
save(dft_apl_MUN_2324,        file = "working/rais/indices/dft_apl_MUN_2324.rda")
save(dft_diversity_MUN_2324, file = "working/rais/indices/dft_diversity_MUN_2324.rda")
save(dft_ubiquity_CBO_2324,  file = "working/rais/indices/dft_ubiquity_CBO_2324.rda")
save(dft_occ_complexity_2324, file = "working/rais/indices/dft_occ_complexity_2324.rda")
save(dft_mun_complexity_2324, file = "working/rais/indices/dft_mun_complexity_2324.rda")





