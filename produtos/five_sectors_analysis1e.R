# five_sectors_analysis1e.R
#
# Extend Employment Analysis to Informal Sector using PNAD-C expansion
#
# Method: 
#   - Load df_cbocod_mun23/24.rda (contains formal + total employment)
#   - Calculate TOTAL employment (all sectors) first
#   - Then filter for 5 priority sectors as subset
#   - Show: "XM in 5 sectors out of YM total"
#
# Outputs:
#   - total_employment_brasil.rda (all sectors summary)
#   - total_employment_by_uf.rda (all sectors by UF)
#   - informal_five_sectors_uf.rda (5 sectors detail)
#   - brasil_informal_summary.rda (5 sectors summary)

library(dplyr)
library(tidyr)
library(data.table)

cat("=============================================================================\n")
cat("  EMPLOYMENT ANALYSIS: ALL SECTORS + FIVE PRIORITY SECTORS (PNAD-C Method)  \n")
cat("=============================================================================\n\n")

# =============================================================================
# STEP 1: LOAD INFORMAL/FORMAL EMPLOYMENT DATA
# =============================================================================

cat("=== Loading informal/formal employment data ===\n")

load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/df_cbocod_mun23.rda")
load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/df_cbocod_mun24.rda")

cat(sprintf("df_cbocod_mun23 rows: %s\n", format(nrow(df_cbocod_mun23), big.mark = ",")))
cat(sprintf("df_cbocod_mun24 rows: %s\n", format(nrow(df_cbocod_mun24), big.mark = ",")))

# Columns: NM_UF, cbo_4dig, CO_MUN6, year, SG_UF, NM_MUN, vinculos_formais, vinculos_total

# =============================================================================
# PART A: TOTAL EMPLOYMENT - ALL SECTORS
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  PART A: TOTAL EMPLOYMENT (ALL SECTORS)                                    \n")
cat("=============================================================================\n")

# Convert to data.table
dt23 <- as.data.table(df_cbocod_mun23)
dt24 <- as.data.table(df_cbocod_mun24)

# --- A1: Brasil Total (All Sectors) ---
cat("\n=== A1: Brasil Total (All Sectors) ===\n")

brasil_total_23 <- dt23[, .(
  formal = sum(vinculos_formais, na.rm = TRUE),
  total = sum(vinculos_total, na.rm = TRUE)
)]
brasil_total_23[, informal := total - formal]

brasil_total_24 <- dt24[, .(
  formal = sum(vinculos_formais, na.rm = TRUE),
  total = sum(vinculos_total, na.rm = TRUE)
)]
brasil_total_24[, informal := total - formal]

# Average 2023-2024
total_employment_brasil <- data.table(
  Formal = (brasil_total_23$formal + brasil_total_24$formal) / 2,
  Informal = (brasil_total_23$informal + brasil_total_24$informal) / 2,
  Total = (brasil_total_23$total + brasil_total_24$total) / 2
)
total_employment_brasil[, Taxa_Formalidade := Formal / Total]
total_employment_brasil[, Taxa_Formalidade_Pct := round(Taxa_Formalidade * 100, 1)]

cat("\nBRASIL TOTAL EMPLOYMENT (All Sectors):\n")
cat(sprintf("  Formal:   %s (%.1f M)\n", format(round(total_employment_brasil$Formal), big.mark = ","), total_employment_brasil$Formal / 1e6))
cat(sprintf("  Informal: %s (%.1f M)\n", format(round(total_employment_brasil$Informal), big.mark = ","), total_employment_brasil$Informal / 1e6))
cat(sprintf("  Total:    %s (%.1f M)\n", format(round(total_employment_brasil$Total), big.mark = ","), total_employment_brasil$Total / 1e6))
cat(sprintf("  Formality Rate: %.1f%%\n", total_employment_brasil$Taxa_Formalidade_Pct))

# --- A2: By UF (All Sectors) ---
cat("\n=== A2: By UF (All Sectors) ===\n")

uf_total_23 <- dt23[, .(
  formal_2023 = sum(vinculos_formais, na.rm = TRUE),
  total_2023 = sum(vinculos_total, na.rm = TRUE)
), by = NM_UF]

uf_total_24 <- dt24[, .(
  formal_2024 = sum(vinculos_formais, na.rm = TRUE),
  total_2024 = sum(vinculos_total, na.rm = TRUE)
), by = NM_UF]

total_employment_by_uf <- merge(uf_total_23, uf_total_24, by = "NM_UF", all = TRUE)
total_employment_by_uf[, `:=`(
  Formal = (formal_2023 + formal_2024) / 2,
  Total = (total_2023 + total_2024) / 2
)]
total_employment_by_uf[, Informal := Total - Formal]
total_employment_by_uf[, Taxa_Formalidade := Formal / Total]
total_employment_by_uf[, Taxa_Formalidade_Pct := round(Taxa_Formalidade * 100, 1)]

# Keep only summary columns
total_employment_by_uf <- total_employment_by_uf[, .(NM_UF, Formal, Informal, Total, Taxa_Formalidade, Taxa_Formalidade_Pct)]

# Add Brasil row
brasil_row <- data.table(
  NM_UF = "BRASIL",
  Formal = total_employment_brasil$Formal,
  Informal = total_employment_brasil$Informal,
  Total = total_employment_brasil$Total,
  Taxa_Formalidade = total_employment_brasil$Taxa_Formalidade,
  Taxa_Formalidade_Pct = total_employment_brasil$Taxa_Formalidade_Pct
)
total_employment_by_uf <- rbind(total_employment_by_uf, brasil_row)

cat("\nTop 10 UFs by Total Employment (All Sectors):\n")
print(total_employment_by_uf[NM_UF != "BRASIL"][order(-Total)][1:10, .(
  NM_UF,
  `Formal (M)` = round(Formal / 1e6, 2),
  `Informal (M)` = round(Informal / 1e6, 2),
  `Total (M)` = round(Total / 1e6, 2),
  `Formality (%)` = Taxa_Formalidade_Pct
)])

# =============================================================================
# PART B: FIVE PRIORITY SECTORS (SUBSET)
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  PART B: FIVE PRIORITY SECTORS (SUBSET)                                    \n")
cat("=============================================================================\n")

cat("\n=== B1: Loading CBO → Sector mapping ===\n")

load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/cnct_qbq_matches2.rda")

# Get best match per CBO
cbo_area <- cnct_qbq_matches2 %>%
  group_by(CodCBO) %>%
  slice_max(final_score, n = 1) %>%
  ungroup() %>%
  select(CodCBO, `Área Tecnológica`) %>%
  distinct()

# Map Área Tecnológica → 5 Sectors
sector_mapping <- data.frame(
  Area_Tecnologica = c(
    "Gestão e Promoção da Saúde e Bem-Estar",
    "Acolhimento e Hospedagem", "Serviços de Gastronomia", "Atividades Turísticas", 
    "Recreação e Sociabilidade", "Apoio Técnico a Eventos",
    "Produção Agrícola e Pecuária", "Silvicultura", "Pesca e Aquicultura", "Produção Alimentícia",
    "Sistemas de Energia", "Construção de Obras", "Mensuração Espacial e Volumétrica", 
    "Operações de Transporte", "Infraestrutura de Informação e Comunicação",
    "Manufatura", "Materiais", "Química", "Têxtil e Vestuário", 
    "Metalmecânica", "Eletrônica e Automação", "Manutenção e Operação"
  ),
  Sector = c(
    "Health Care",
    rep("Tourism", 5),
    rep("Agribusiness", 4),
    rep("Energy & Infrastructure", 5),
    rep("Manufacturing", 7)
  ),
  stringsAsFactors = FALSE
)

cbo_sector <- cbo_area %>%
  left_join(sector_mapping, by = c("Área Tecnológica" = "Area_Tecnologica")) %>%
  filter(!is.na(Sector))

cat(sprintf("CBOs mapped to 5 sectors: %d\n", nrow(cbo_sector)))

# =============================================================================
# STEP 3: CHECK DATA STRUCTURE
# =============================================================================

cat("\n=== Data structure ===\n")
cat("Columns: NM_UF, cbo_4dig, CO_MUN6, year, SG_UF, NM_MUN, vinculos_formais, vinculos_total\n")

# =============================================================================
# STEP 4: PREPARE CBO MAPPING (4-digit)
# =============================================================================

cat("\n=== Preparing CBO mapping ===\n")

# Extract 4-digit CBO from mapping (CodCBO is 6 digits)
cbo_sector$cbo_4dig <- substr(cbo_sector$CodCBO, 1, 4)

# Keep unique 4-digit mappings
cbo_sector_4dig <- cbo_sector %>%
  select(cbo_4dig, Sector) %>%
  distinct()

cat(sprintf("Unique 4-digit CBOs mapped: %d\n", nrow(cbo_sector_4dig)))

# =============================================================================
# STEP 5: JOIN WITH SECTOR MAPPING AND AGGREGATE
# =============================================================================

cat("\n=== Joining with sector mapping ===\n")

# Convert to data.table
dt23 <- as.data.table(df_cbocod_mun23)
dt24 <- as.data.table(df_cbocod_mun24)

# Join 2023 data with sector mapping
dt23_sector <- merge(
  dt23,
  cbo_sector_4dig,
  by = "cbo_4dig",
  all.x = FALSE
)
cat(sprintf("2023 rows after sector join: %s\n", format(nrow(dt23_sector), big.mark = ",")))

# Join 2024 data with sector mapping
dt24_sector <- merge(
  dt24,
  cbo_sector_4dig,
  by = "cbo_4dig",
  all.x = FALSE
)
cat(sprintf("2024 rows after sector join: %s\n", format(nrow(dt24_sector), big.mark = ",")))

# Aggregate by UF × Sector for 2023
result_23 <- dt23_sector[, .(
  formal_2023 = sum(vinculos_formais, na.rm = TRUE),
  total_2023 = sum(vinculos_total, na.rm = TRUE)
), by = .(NM_UF, Sector)]
result_23[, informal_2023 := total_2023 - formal_2023]

# Aggregate by UF × Sector for 2024
result_24 <- dt24_sector[, .(
  formal_2024 = sum(vinculos_formais, na.rm = TRUE),
  total_2024 = sum(vinculos_total, na.rm = TRUE)
), by = .(NM_UF, Sector)]
result_24[, informal_2024 := total_2024 - formal_2024]

# =============================================================================
# STEP 6: COMBINE YEARS AND CREATE FINAL OUTPUT
# =============================================================================

cat("\n=== Combining years ===\n")

# Merge 2023 and 2024 results
informal_five_sectors_uf <- merge(result_23, result_24, by = c("NM_UF", "Sector"), all = TRUE)

# Calculate averages
informal_five_sectors_uf[, `:=`(
  formal_avg = (formal_2023 + formal_2024) / 2,
  total_avg = (total_2023 + total_2024) / 2,
  informal_avg = (informal_2023 + informal_2024) / 2
)]
informal_five_sectors_uf[, taxa_formalidade := formal_avg / total_avg]

# =============================================================================
# STEP 7: CREATE SUMMARY TABLES
# =============================================================================

cat("\n=== Creating summary tables ===\n")

# Table 1: UF-level summary (same structure as brasil_informal_summary but by UF)
informal_summary_by_uf <- informal_five_sectors_uf[, .(
  Formal = sum(formal_avg, na.rm = TRUE),
  Informal = sum(informal_avg, na.rm = TRUE),
  Total = sum(total_avg, na.rm = TRUE)
), by = NM_UF]
informal_summary_by_uf[, Taxa_Formalidade := Formal / Total]
informal_summary_by_uf[, Taxa_Formalidade_Pct := round(Taxa_Formalidade * 100, 1)]

# Add Brasil totals row
brasil_total_row <- data.table(
  NM_UF = "BRASIL",
  Formal = sum(informal_summary_by_uf$Formal),
  Informal = sum(informal_summary_by_uf$Informal),
  Total = sum(informal_summary_by_uf$Total),
  Taxa_Formalidade = sum(informal_summary_by_uf$Formal) / sum(informal_summary_by_uf$Total),
  Taxa_Formalidade_Pct = round(sum(informal_summary_by_uf$Formal) / sum(informal_summary_by_uf$Total) * 100, 1)
)
informal_summary_by_uf <- rbind(informal_summary_by_uf, brasil_total_row)

# Table 2: By UF × Sector (wide format for total employment)
total_emp_uf_sector <- dcast(informal_five_sectors_uf, NM_UF ~ Sector, value.var = "total_avg", fill = 0)
total_emp_uf_sector[, TOTAL := rowSums(.SD, na.rm = TRUE), .SDcols = setdiff(names(total_emp_uf_sector), "NM_UF")]

# Add Brasil totals row
brasil_totals <- total_emp_uf_sector[, lapply(.SD, sum, na.rm = TRUE), .SDcols = setdiff(names(total_emp_uf_sector), "NM_UF")]
brasil_totals[, NM_UF := "BRASIL"]
total_emp_uf_sector <- rbind(total_emp_uf_sector, brasil_totals, fill = TRUE)

# Table 3: Formality rate by UF × Sector
formality_wide <- dcast(informal_five_sectors_uf, NM_UF ~ Sector, value.var = "taxa_formalidade", fill = NA)

# Calculate Brasil average formality (weighted by employment)
brasil_form <- informal_five_sectors_uf[, .(
  taxa_formalidade = sum(formal_avg, na.rm = TRUE) / sum(total_avg, na.rm = TRUE)
), by = Sector]
brasil_form_wide <- dcast(brasil_form, . ~ Sector, value.var = "taxa_formalidade")
brasil_form_wide[, NM_UF := "BRASIL"]
brasil_form_wide[, `.` := NULL]
formality_wide <- rbind(formality_wide, brasil_form_wide, fill = TRUE)

# Convert to percentage for display
formality_rate_uf_sector <- copy(formality_wide)
cols_to_pct <- setdiff(names(formality_rate_uf_sector), "NM_UF")
formality_rate_uf_sector[, (cols_to_pct) := lapply(.SD, function(x) round(x * 100, 1)), .SDcols = cols_to_pct]

# Table 4: Brasil summary by sector
brasil_informal_summary <- informal_five_sectors_uf[, .(
  Formal = sum(formal_avg, na.rm = TRUE),
  Informal = sum(informal_avg, na.rm = TRUE),
  Total = sum(total_avg, na.rm = TRUE),
  Taxa_Formalidade = sum(formal_avg, na.rm = TRUE) / sum(total_avg, na.rm = TRUE)
), by = Sector]
brasil_informal_summary[, Taxa_Formalidade_Pct := round(Taxa_Formalidade * 100, 1)]

# Add totals row
total_row <- data.table(
  Sector = "TOTAL 5 SECTORS",
  Formal = sum(brasil_informal_summary$Formal),
  Informal = sum(brasil_informal_summary$Informal),
  Total = sum(brasil_informal_summary$Total),
  Taxa_Formalidade = sum(brasil_informal_summary$Formal) / sum(brasil_informal_summary$Total),
  Taxa_Formalidade_Pct = round(sum(brasil_informal_summary$Formal) / sum(brasil_informal_summary$Total) * 100, 1)
)
brasil_informal_summary <- rbind(brasil_informal_summary, total_row)

# =============================================================================
# STEP 8: PRINT RESULTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("       BRASIL SUMMARY: FORMAL vs INFORMAL EMPLOYMENT IN 5 SECTORS              \n")
cat("================================================================================\n\n")

print(brasil_informal_summary[, .(
  Sector,
  `Formal (M)` = round(Formal / 1e6, 2),
  `Informal (M)` = round(Informal / 1e6, 2),
  `Total (M)` = round(Total / 1e6, 2),
  `Formality Rate (%)` = Taxa_Formalidade_Pct
)])

cat("\n")
cat("================================================================================\n")
cat("       UF SUMMARY: FORMAL vs INFORMAL IN 5 SECTORS (Top 10 UFs)                \n")
cat("================================================================================\n\n")

# Show top 10 UFs by total employment
top_ufs_summary <- informal_summary_by_uf[NM_UF != "BRASIL"][order(-Total)][1:10]
print(top_ufs_summary[, .(
  NM_UF,
  `Formal (M)` = round(Formal / 1e6, 2),
  `Informal (M)` = round(Informal / 1e6, 2),
  `Total (M)` = round(Total / 1e6, 2),
  `Formality Rate (%)` = Taxa_Formalidade_Pct
)])

cat("\n")
cat("================================================================================\n")
cat("       TOTAL EMPLOYMENT BY UF × SECTOR (Top 10 UFs)                            \n")
cat("================================================================================\n\n")

# Show top 10 UFs by total employment
top_ufs <- total_emp_uf_sector[NM_UF != "BRASIL"][order(-TOTAL)][1:10]
print(top_ufs)

cat("\n")
cat("================================================================================\n")
cat("       FORMALITY RATE (%) BY UF × SECTOR (Sample)                              \n")
cat("================================================================================\n\n")

print(head(formality_rate_uf_sector, 10))

# =============================================================================
# STEP 9: SAVE RESULTS
# =============================================================================

cat("\n=== Saving results ===\n")

# --- PART A outputs (All Sectors) ---
save(total_employment_brasil, 
     file = "D:/Country/Brazil/TechBrazil/working/rais/total_employment_brasil.rda")
cat("✅ Saved: total_employment_brasil.rda (All sectors - Brasil total)\n")

save(total_employment_by_uf, 
     file = "D:/Country/Brazil/TechBrazil/working/rais/total_employment_by_uf.rda")
cat("✅ Saved: total_employment_by_uf.rda (All sectors - by UF)\n")

# --- PART B outputs (Five Sectors) ---
save(informal_five_sectors_uf, 
     file = "D:/Country/Brazil/TechBrazil/working/rais/informal_five_sectors_uf.rda")
cat("✅ Saved: informal_five_sectors_uf.rda (5 sectors - UF × Sector detail)\n")

save(informal_summary_by_uf, 
     file = "D:/Country/Brazil/TechBrazil/working/rais/informal_summary_by_uf.rda")
cat("✅ Saved: informal_summary_by_uf.rda (5 sectors - by UF)\n")

save(total_emp_uf_sector, 
     file = "D:/Country/Brazil/TechBrazil/working/rais/total_emp_uf_sector.rda")
cat("✅ Saved: total_emp_uf_sector.rda (5 sectors - UF × Sector wide)\n")

save(formality_rate_uf_sector, 
     file = "D:/Country/Brazil/TechBrazil/working/rais/formality_rate_uf_sector.rda")
cat("✅ Saved: formality_rate_uf_sector.rda (5 sectors - formality % wide)\n")

save(brasil_informal_summary, 
     file = "D:/Country/Brazil/TechBrazil/working/rais/brasil_informal_summary.rda")
cat("✅ Saved: brasil_informal_summary.rda (5 sectors - Brasil by sector)\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("       FINAL SUMMARY: ALL SECTORS vs FIVE PRIORITY SECTORS                     \n")
cat("================================================================================\n\n")

five_sectors_total <- sum(brasil_informal_summary$Total[brasil_informal_summary$Sector != "TOTAL 5 SECTORS"])
all_sectors_total <- total_employment_brasil$Total
pct_coverage <- round(five_sectors_total / all_sectors_total * 100, 1)

cat(sprintf("ALL SECTORS (Brasil Total):\n"))
cat(sprintf("  Total Employment: %.1f M\n", all_sectors_total / 1e6))
cat(sprintf("  Formal: %.1f M | Informal: %.1f M\n", 
            total_employment_brasil$Formal / 1e6, 
            total_employment_brasil$Informal / 1e6))
cat(sprintf("  Formality Rate: %.1f%%\n\n", total_employment_brasil$Taxa_Formalidade_Pct))

cat(sprintf("FIVE PRIORITY SECTORS:\n"))
cat(sprintf("  Total Employment: %.1f M (%.1f%% of all sectors)\n", five_sectors_total / 1e6, pct_coverage))

cat("\n=== Done ===\n")
cat("\nNote: Scarcity index remains from formal sector analysis (scarcity_index_uf_sector.rda)\n")