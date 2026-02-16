# five_sectors_analysis1a.R (REVISED)
#
# Complete analysis of 5 priority sectors for PROPAG
# Outputs: Total employment and Técnico Nível Médio by sector for 2023 and 2024
#
# REVISION: Use CBO Grande Grupo 3 (occupation codes starting with "3") 
#           instead of escolaridade <= 8 to identify technical workers

library(dplyr)
library(tidyr)

# =============================================================================
# STEP 1: LOAD DATA
# =============================================================================

cat("=== Loading data ===\n")

# RAIS data
load("D:/Country/Brazil/TechBrazil/working/rais/2023/rais2023.rda")
load("D:/Country/Brazil/TechBrazil/working/rais/2024/rais2024.rda")

# CBO → Área Tecnológica mapping
load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/cnct_qbq_matches2.rda")

cat(sprintf("RAIS 2023 rows: %s\n", format(nrow(rais2023), big.mark = ",")))
cat(sprintf("RAIS 2024 rows: %s\n", format(nrow(rais2024), big.mark = ",")))

# =============================================================================
# STEP 2: CREATE CBO → SECTOR MAPPING
# =============================================================================

cat("\n=== Creating CBO → Sector mapping ===\n")

# Get unique CBO → Área mapping (use highest score match per CBO)
cbo_area <- cnct_qbq_matches2 %>%
  group_by(CodCBO) %>%
  slice_max(final_score, n = 1) %>%
  ungroup() %>%
  select(CodCBO, `Área Tecnológica`) %>%
  distinct()

# Define 5 sectors mapping
sector_mapping <- data.frame(
  Area_Tecnologica = c(
    # Health Care (1 área)
    "Gestão e Promoção da Saúde e Bem-Estar",
    # Tourism (5 áreas)
    "Acolhimento e Hospedagem", "Serviços de Gastronomia", "Atividades Turísticas", 
    "Recreação e Sociabilidade", "Apoio técnico a eventos",
    # Agribusiness (4 áreas)
    "Produção Agrícola e Pecuária", "Silvicultura", "Pesca e Aquicultura", "Produção Alimentícia",
    # Energy & Infrastructure (5 áreas)
    "Sistemas de Energia", "Construção de Obras", "Mensuração Espacial e Volumétrica", 
    "Operações de Transporte", "Infraestrutura de Informação e Comunicação",
    # Manufacturing (7 áreas)
    "Manufatura", "Materiais", "Química", "Têxtil e Vestuário", 
    "Metalmecânica", "Eletrônica e Automação", "Manutenção e Operação"
  ),
  Sector = c(
    "Health Care",
    rep("Tourism", 5),
    rep("Agribusiness", 4),
    rep("Energy & Infrastructure", 5),
    rep("Manufacturing", 7)
  )
)

# Add sector to CBO mapping
cbo_sector <- cbo_area %>%
  left_join(sector_mapping, by = c("Área Tecnológica" = "Area_Tecnologica")) %>%
  filter(!is.na(Sector))

cat("CBOs per sector:\n")
print(table(cbo_sector$Sector))

# =============================================================================
# STEP 3: STANDARDIZE COLUMN NAMES
# =============================================================================

cat("\n=== Standardizing column names ===\n")

# Check column names
cat("RAIS 2023 columns:\n")
print(names(rais2023))
cat("\nRAIS 2024 columns:\n")
print(names(rais2024))

# RAIS 2023 uses old column names, RAIS 2024 uses new column names
# Standardize both to common names

# For 2023
if ("CBO Ocupação 2002" %in% names(rais2023)) {
  rais2023 <- rais2023 %>% rename(
    CodCBO = `CBO Ocupação 2002`,
    vinculos = `Vínculo Ativo 31/12`
  )
} else if ("CBO 2002 Ocupação - Código" %in% names(rais2023)) {
  rais2023 <- rais2023 %>% rename(
    CodCBO = `CBO 2002 Ocupação - Código`,
    vinculos = `Ind Vínculo Ativo 31/12 - Código`
  )
}

# For 2024
if ("CBO 2002 Ocupação - Código" %in% names(rais2024)) {
  rais2024 <- rais2024 %>% rename(
    CodCBO = `CBO 2002 Ocupação - Código`,
    vinculos = `Ind Vínculo Ativo 31/12 - Código`
  )
} else if ("CBO Ocupação 2002" %in% names(rais2024)) {
  rais2024 <- rais2024 %>% rename(
    CodCBO = `CBO Ocupação 2002`,
    vinculos = `Vínculo Ativo 31/12`
  )
}

# Convert CBO to character for joining
rais2023$CodCBO <- as.character(rais2023$CodCBO)
rais2024$CodCBO <- as.character(rais2024$CodCBO)

# =============================================================================
# STEP 3B: CREATE CBO GRANDE GRUPO 3 FLAG (Técnicos de Nível Médio)
# =============================================================================

cat("\n=== Creating CBO Grande Grupo 3 flag ===\n")

# CBO Grande Grupo 3 = Técnicos de Nível Médio
# These are occupation codes that start with "3"
rais2023 <- rais2023 %>%
  mutate(is_tecnico = substr(CodCBO, 1, 1) == "3")

rais2024 <- rais2024 %>%
  mutate(is_tecnico = substr(CodCBO, 1, 1) == "3")

# Check counts
cat(sprintf("RAIS 2023 - CBO Grande Grupo 3 (Técnicos): %s\n", 
            format(sum(rais2023$is_tecnico & rais2023$vinculos == 1, na.rm = TRUE), big.mark = ",")))
cat(sprintf("RAIS 2024 - CBO Grande Grupo 3 (Técnicos): %s\n", 
            format(sum(rais2024$is_tecnico & rais2024$vinculos == 1, na.rm = TRUE), big.mark = ",")))

# =============================================================================
# STEP 4: CALCULATE TOTALS FOR 2023
# =============================================================================

cat("\n=== Calculating 2023 totals ===\n")

# Total vínculos ativos 2023
total_2023 <- sum(rais2023$vinculos == 1, na.rm = TRUE)
cat(sprintf("Total vínculos ativos 2023: %s\n", format(total_2023, big.mark = ",")))

# Join with sector mapping
rais2023_sector <- rais2023 %>%
  inner_join(cbo_sector, by = "CodCBO") %>%
  filter(vinculos == 1)

cat(sprintf("Matched to 5 sectors: %s\n", format(nrow(rais2023_sector), big.mark = ",")))

# Total by sector (all education)
total_by_sector_2023 <- rais2023_sector %>%
  group_by(Sector) %>%
  summarise(Total_2023 = n(), .groups = 'drop')

# Técnico Nível Médio by sector (CBO Grande Grupo 3)
tecnico_2023 <- rais2023_sector %>%
  filter(is_tecnico == TRUE) %>%
  group_by(Sector) %>%
  summarise(Tecnico_2023 = n(), .groups = 'drop')

# Combine
result_2023 <- left_join(total_by_sector_2023, tecnico_2023, by = "Sector")

# =============================================================================
# STEP 5: CALCULATE TOTALS FOR 2024
# =============================================================================

cat("\n=== Calculating 2024 totals ===\n")

# Total vínculos ativos 2024
total_2024 <- sum(rais2024$vinculos == 1, na.rm = TRUE)
cat(sprintf("Total vínculos ativos 2024: %s\n", format(total_2024, big.mark = ",")))

# Join with sector mapping
rais2024_sector <- rais2024 %>%
  inner_join(cbo_sector, by = "CodCBO") %>%
  filter(vinculos == 1)

cat(sprintf("Matched to 5 sectors: %s\n", format(nrow(rais2024_sector), big.mark = ",")))

# Total by sector (all education)
total_by_sector_2024 <- rais2024_sector %>%
  group_by(Sector) %>%
  summarise(Total_2024 = n(), .groups = 'drop')

# Técnico Nível Médio by sector (CBO Grande Grupo 3)
tecnico_2024 <- rais2024_sector %>%
  filter(is_tecnico == TRUE) %>%
  group_by(Sector) %>%
  summarise(Tecnico_2024 = n(), .groups = 'drop')

# Combine
result_2024 <- left_join(total_by_sector_2024, tecnico_2024, by = "Sector")

# =============================================================================
# STEP 6: COMBINE RESULTS
# =============================================================================

cat("\n=== Final Results ===\n")

# Merge 2023 and 2024
final_result <- left_join(result_2023, result_2024, by = "Sector") %>%
  mutate(
    Pct_Tecnico_2023 = round(Tecnico_2023 / Total_2023 * 100, 1),
    Pct_Tecnico_2024 = round(Tecnico_2024 / Total_2024 * 100, 1),
    Growth_Total = round((Total_2024 - Total_2023) / Total_2023 * 100, 1),
    Growth_Tecnico = round((Tecnico_2024 - Tecnico_2023) / Tecnico_2023 * 100, 1)
  ) %>%
  arrange(desc(Total_2024))

# Add totals row
totals_row <- data.frame(
  Sector = "TOTAL 5 SECTORS",
  Total_2023 = sum(final_result$Total_2023),
  Tecnico_2023 = sum(final_result$Tecnico_2023),
  Total_2024 = sum(final_result$Total_2024),
  Tecnico_2024 = sum(final_result$Tecnico_2024)
) %>%
  mutate(
    Pct_Tecnico_2023 = round(Tecnico_2023 / Total_2023 * 100, 1),
    Pct_Tecnico_2024 = round(Tecnico_2024 / Total_2024 * 100, 1),
    Growth_Total = round((Total_2024 - Total_2023) / Total_2023 * 100, 1),
    Growth_Tecnico = round((Tecnico_2024 - Tecnico_2023) / Tecnico_2023 * 100, 1)
  )

final_result <- rbind(final_result, totals_row)

# Print results
cat("\n========================================\n")
cat("5 PRIORITY SECTORS - EMPLOYMENT SUMMARY\n")
cat("========================================\n\n")

cat("TOTAL EMPLOYMENT (All Occupations in Sector):\n")
cat("----------------------------------------------\n")
print(final_result %>% select(Sector, Total_2023, Total_2024, Growth_Total))

cat("\n\nTÉCNICO NÍVEL MÉDIO (CBO Grande Grupo 3):\n")
cat("------------------------------------------\n")
print(final_result %>% select(Sector, Tecnico_2023, Tecnico_2024, Growth_Tecnico, Pct_Tecnico_2024))


# =============================================================================
# STEP 7: CALCULATE GRAND TOTALS
# =============================================================================

# Grand totals (all vínculos ativos)
grand_total_2023 <- sum(rais2023$vinculos == 1, na.rm = TRUE)
grand_total_2024 <- sum(rais2024$vinculos == 1, na.rm = TRUE)

# Técnico totals (CBO Grande Grupo 3)
tecnico_total_2023 <- sum(rais2023$is_tecnico & rais2023$vinculos == 1, na.rm = TRUE)
tecnico_total_2024 <- sum(rais2024$is_tecnico & rais2024$vinculos == 1, na.rm = TRUE)

# 5 sectors totals
sectors_total_2023 <- sum(final_result$Total_2023[final_result$Sector != "TOTAL 5 SECTORS"])
sectors_total_2024 <- sum(final_result$Total_2024[final_result$Sector != "TOTAL 5 SECTORS"])
sectors_tecnico_2023 <- sum(final_result$Tecnico_2023[final_result$Sector != "TOTAL 5 SECTORS"])
sectors_tecnico_2024 <- sum(final_result$Tecnico_2024[final_result$Sector != "TOTAL 5 SECTORS"])

cat(sprintf("\nGrand Total 2023: %s\n", format(grand_total_2023, big.mark = ",")))
cat(sprintf("Grand Total 2024: %s\n", format(grand_total_2024, big.mark = ",")))
cat(sprintf("\nTécnico (CBO GG3) Total 2023: %s\n", format(tecnico_total_2023, big.mark = ",")))
cat(sprintf("Técnico (CBO GG3) Total 2024: %s\n", format(tecnico_total_2024, big.mark = ",")))

cat(sprintf("\n5 Sectors as %% of Total Employment 2023: %.1f%%\n", sectors_total_2023/grand_total_2023*100))
cat(sprintf("5 Sectors as %% of Total Employment 2024: %.1f%%\n", sectors_total_2024/grand_total_2024*100))
cat(sprintf("\n5 Sectors Técnico as %% of All Técnico 2023: %.1f%%\n", sectors_tecnico_2023/tecnico_total_2023*100))
cat(sprintf("5 Sectors Técnico as %% of All Técnico 2024: %.1f%%\n", sectors_tecnico_2024/tecnico_total_2024*100))


# =============================================================================
# STEP 8: PRINT FORMATTED TABLE
# =============================================================================

cat("\n\n")
cat("================================================================================\n")
cat("                    FINAL TABLE - 5 PRIORITY SECTORS                           \n")
cat("        (Técnico = CBO Grande Grupo 3 - Técnicos de Nível Médio)               \n")
cat("================================================================================\n")
cat(sprintf("%-25s %15s %15s %15s %15s\n", 
            "Sector", "Total 2023", "Total 2024", "Tecnico 2023", "Tecnico 2024"))
cat("--------------------------------------------------------------------------------\n")

for (i in 1:nrow(final_result)) {
  cat(sprintf("%-25s %15s %15s %15s %15s\n",
              final_result$Sector[i],
              format(final_result$Total_2023[i], big.mark = ","),
              format(final_result$Total_2024[i], big.mark = ","),
              format(final_result$Tecnico_2023[i], big.mark = ","),
              format(final_result$Tecnico_2024[i], big.mark = ",")))
}

cat("--------------------------------------------------------------------------------\n")
cat(sprintf("%-25s %15s %15s %15s %15s\n",
            "GRAND TOTAL (All Economy)",
            format(grand_total_2023, big.mark = ","),
            format(grand_total_2024, big.mark = ","),
            format(tecnico_total_2023, big.mark = ","),
            format(tecnico_total_2024, big.mark = ",")))
cat("================================================================================\n")
cat(sprintf("5 Sectors as %% of Total:    %13.1f%% %15.1f%% %14.1f%% %15.1f%%\n",
            sectors_total_2023/grand_total_2023*100,
            sectors_total_2024/grand_total_2024*100,
            sectors_tecnico_2023/tecnico_total_2023*100,
            sectors_tecnico_2024/tecnico_total_2024*100))
cat("================================================================================\n")


# =============================================================================
# STEP 9: CREATE FINAL RESULT WITH GRAND TOTALS
# =============================================================================

# Create full result with grand totals
grand_total_row <- data.frame(
  Sector = "GRAND TOTAL (All Economy)",
  Total_2023 = grand_total_2023,
  Tecnico_2023 = tecnico_total_2023,
  Total_2024 = grand_total_2024,
  Tecnico_2024 = tecnico_total_2024,
  Pct_Tecnico_2023 = round(tecnico_total_2023 / grand_total_2023 * 100, 1),
  Pct_Tecnico_2024 = round(tecnico_total_2024 / grand_total_2024 * 100, 1),
  Growth_Total = round((grand_total_2024 - grand_total_2023) / grand_total_2023 * 100, 1),
  Growth_Tecnico = round((tecnico_total_2024 - tecnico_total_2023) / tecnico_total_2023 * 100, 1)
)

pct_row <- data.frame(
  Sector = "5 Sectors as % of Total",
  Total_2023 = round(sectors_total_2023 / grand_total_2023 * 100, 1),
  Tecnico_2023 = round(sectors_tecnico_2023 / tecnico_total_2023 * 100, 1),
  Total_2024 = round(sectors_total_2024 / grand_total_2024 * 100, 1),
  Tecnico_2024 = round(sectors_tecnico_2024 / tecnico_total_2024 * 100, 1),
  Pct_Tecnico_2023 = NA, Pct_Tecnico_2024 = NA, Growth_Total = NA, Growth_Tecnico = NA
)

final_result_full <- rbind(final_result, grand_total_row, pct_row)

# =============================================================================
# STEP 10: SAVE RESULTS
# =============================================================================

# Save CSV
write.csv(final_result_full, "D:/Country/Brazil/TechBrazil/working/rais/five_sectors_summary.csv", row.names = FALSE)
cat("\n✅ Saved: five_sectors_summary.csv\n")

# Save RDA (one object per file, filename = object name)
save(final_result_full, file = "D:/Country/Brazil/TechBrazil/working/rais/final_result_full.rda")
cat("✅ Saved: final_result_full.rda\n")

cat("\n=== Done ===\n")