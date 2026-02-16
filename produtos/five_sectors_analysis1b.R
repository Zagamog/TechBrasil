# five_sectors_analysis1b.R
#
# Scarcity index analysis for 5 priority sectors for PROPAG
# Uses CAGED/RAIS scarcity data mapped to sectors via CNCT catalogue
# Outputs: Scarcity indicators by sector for 2023 and 2024

library(dplyr)
library(tidyr)
library(stringr)

# =============================================================================
# STEP 1: LOAD DATA
# =============================================================================

cat("=== Loading data ===\n")

# CAGED/RAIS scarcity data (by Curso, monthly 2020-2024)
load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/caged_rais_cnct_2020_2024_shiny.rda")

# CNCT catalogue (Curso → Área Tecnológica mapping)
load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/df_exarcu.rda")

cat(sprintf("CAGED/RAIS scarcity data rows: %s\n", format(nrow(caged_rais_curso), big.mark = ",")))
cat(sprintf("CNCT catalogue rows: %s\n", format(nrow(df_exarcu), big.mark = ",")))

# Check column names in df_exarcu
cat("\ndf_exarcu columns:\n")
print(names(df_exarcu))

# Check column names in caged_rais_curso
cat("\ncaged_rais_curso columns:\n")
print(names(caged_rais_curso))

# =============================================================================
# STEP 2: DEFINE ÁREA TECNOLÓGICA → 5 SECTORS MAPPING
# =============================================================================

cat("\n=== Creating Área Tecnológica → Sector mapping ===\n")

# Same mapping as in five_sectors_analysis1a.R
sector_mapping <- data.frame(
  Area_Tecnologica = c(
    # Health Care (1 área)
    "Gestão e Promoção da Saúde e Bem-Estar",
    # Tourism (5 áreas)
    "Acolhimento e Hospedagem", "Serviços de Gastronomia", "Atividades Turísticas", 
    "Recreação e Sociabilidade", "Apoio Técnico a Eventos",
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
  ),
  stringsAsFactors = FALSE
)

cat("Áreas per sector:\n")
print(table(sector_mapping$Sector))

# =============================================================================
# STEP 3: CREATE CURSO → SECTOR MAPPING (WITH NAME CLEANING)
# =============================================================================

cat("\n=== Creating Curso → Sector mapping ===\n")

# Check the Area column name in df_exarcu (might be "Area" or "Área" or other)
# Adjust based on actual column names
area_col <- names(df_exarcu)[grepl("rea", names(df_exarcu), ignore.case = TRUE)]
curso_col <- names(df_exarcu)[grepl("Denomina", names(df_exarcu), ignore.case = TRUE)]

cat(sprintf("Area column found: %s\n", paste(area_col, collapse = ", ")))
cat(sprintf("Curso column found: %s\n", paste(curso_col, collapse = ", ")))

# Clean CNCT catalogue curso names:
# - Remove "Técnico em " prefix
# - Convert to lowercase
# - Trim whitespace
classification_clean <- df_exarcu %>%
  select(Curso_CNCT_orig = all_of(curso_col[1]), Area_Tecnologica = all_of(area_col[1])) %>%
  distinct() %>%
  mutate(
    curso_clean = str_remove(Curso_CNCT_orig, "^Técnico em\\s+"),
    curso_clean = str_trim(tolower(curso_clean))
  ) %>%
  inner_join(sector_mapping, by = "Area_Tecnologica") %>%
  filter(!is.na(Sector))

cat(sprintf("\nCNTC cursos mapped to 5 sectors: %d\n", nrow(classification_clean)))
cat("\nCursos per sector:\n")
print(table(classification_clean$Sector))

# =============================================================================
# STEP 4: CLEAN CAGED CURSO NAMES AND CHECK MATCHING
# =============================================================================

cat("\n=== Cleaning CAGED curso names and checking matches ===\n")

# Clean CAGED/RAIS curso names (same logic - lowercase and trim)
caged_rais_clean <- caged_rais_curso %>%
  mutate(
    curso_clean = str_trim(tolower(Curso))
  )

# Get unique cleaned Cursos from each source
cursos_caged_clean <- unique(caged_rais_clean$curso_clean)
cursos_cnct_clean <- unique(classification_clean$curso_clean)

cat(sprintf("Unique Cursos in CAGED/RAIS data: %d\n", length(cursos_caged_clean)))
cat(sprintf("Unique Cursos in CNCT catalogue (5 sectors): %d\n", length(cursos_cnct_clean)))

# Check overlap
matched_cursos <- intersect(cursos_caged_clean, cursos_cnct_clean)
cat(sprintf("Cursos with exact match: %d\n", length(matched_cursos)))

# Show some matched for verification
if (length(matched_cursos) > 0) {
  cat(sprintf("\nSample matched Cursos (first 10):\n"))
  print(head(matched_cursos, 10))
}

# Show some unmatched for debugging
unmatched_caged <- setdiff(cursos_caged_clean, cursos_cnct_clean)
if (length(unmatched_caged) > 0) {
  cat(sprintf("\nSample unmatched Cursos from CAGED (first 10):\n"))
  print(head(unmatched_caged, 10))
}

# Create the curso_sector mapping table for joining
curso_sector <- classification_clean %>%
  select(curso_clean, Sector, Area_Tecnologica) %>%
  distinct()

# =============================================================================
# STEP 5: JOIN SCARCITY DATA TO SECTORS
# =============================================================================

cat("\n=== Joining scarcity data to sectors ===\n")

# Join using cleaned Curso names
scarcity_with_sector <- caged_rais_clean %>%
  inner_join(curso_sector, by = "curso_clean")

cat(sprintf("Rows after joining to sectors: %s\n", format(nrow(scarcity_with_sector), big.mark = ",")))
cat(sprintf("Percentage of original data matched: %.1f%%\n", 
            nrow(scarcity_with_sector) / nrow(caged_rais_curso) * 100))

# =============================================================================
# STEP 6: AGGREGATE SCARCITY BY SECTOR - BRASIL LEVEL
# =============================================================================

cat("\n=== Aggregating scarcity by sector (Brasil) ===\n")

# Filter to Brasil only and aggregate by Sector, Year, Month
scarcity_sector_monthly <- scarcity_with_sector %>%
  filter(NM_UF == "Brasil") %>%
  group_by(ANO, MES, Sector) %>%
  summarise(
    # Weighted mean of salary differential (weighted by employment stock)
    dif_sal_weighted = weighted.mean(dif_sal_adm_des_pc_m12, w = estoque_liquido, na.rm = TRUE),
    # Weighted mean of turnover rate
    tx_rotatividade_weighted = weighted.mean(tx_rotatividade_m12, w = estoque_liquido, na.rm = TRUE),
    # Total employment stock
    estoque_total = sum(estoque_liquido, na.rm = TRUE),
    # Number of courses in sector
    n_cursos = n_distinct(Curso),
    .groups = 'drop'
  )

# =============================================================================
# STEP 7: GET LATEST MONTH FOR 2023 AND 2024
# =============================================================================

cat("\n=== Extracting latest month for 2023 and 2024 ===\n")

# Find latest available month for each year
latest_months <- scarcity_sector_monthly %>%
  group_by(ANO) %>%
  summarise(max_mes = max(MES), .groups = 'drop') %>%
  filter(ANO %in% c(2023, 2024))

cat("Latest available months:\n")
print(latest_months)

# Get 2023 data (latest month)
latest_2023 <- latest_months %>% filter(ANO == 2023) %>% pull(max_mes)
scarcity_2023 <- scarcity_sector_monthly %>%
  filter(ANO == 2023, MES == latest_2023) %>%
  select(Sector, 
         Dif_Sal_2023 = dif_sal_weighted, 
         Rotatividade_2023 = tx_rotatividade_weighted,
         Estoque_2023 = estoque_total,
         N_Cursos_2023 = n_cursos)

# Get 2024 data (latest month)
latest_2024 <- latest_months %>% filter(ANO == 2024) %>% pull(max_mes)
scarcity_2024 <- scarcity_sector_monthly %>%
  filter(ANO == 2024, MES == latest_2024) %>%
  select(Sector, 
         Dif_Sal_2024 = dif_sal_weighted, 
         Rotatividade_2024 = tx_rotatividade_weighted,
         Estoque_2024 = estoque_total,
         N_Cursos_2024 = n_cursos)

# =============================================================================
# STEP 8: COMBINE RESULTS
# =============================================================================

cat("\n=== Combining 2023 and 2024 results ===\n")

scarcity_result <- scarcity_2023 %>%
  full_join(scarcity_2024, by = "Sector") %>%
  mutate(
    # Calculate change in scarcity index
    Dif_Sal_Change = Dif_Sal_2024 - Dif_Sal_2023,
    Rotatividade_Change = Rotatividade_2024 - Rotatividade_2023,
    Estoque_Growth_Pct = (Estoque_2024 - Estoque_2023) / Estoque_2023 * 100
  ) %>%
  arrange(desc(Dif_Sal_2024))

# =============================================================================
# STEP 9: PRINT RESULTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("        SCARCITY INDEX BY SECTOR - 5 PRIORITY SECTORS (BRASIL)                 \n")
cat(sprintf("        2023 (Month %d) vs 2024 (Month %d)                                      \n", latest_2023, latest_2024))
cat("================================================================================\n\n")

cat("SALARY DIFFERENTIAL (Admitidos vs Desligados, %) - Higher = More Scarcity:\n")
cat("--------------------------------------------------------------------------------\n")
cat(sprintf("%-25s %15s %15s %15s\n", "Sector", "2023", "2024", "Change"))
cat("--------------------------------------------------------------------------------\n")
for (i in 1:nrow(scarcity_result)) {
  cat(sprintf("%-25s %14.1f%% %14.1f%% %+14.1f pp\n",
              scarcity_result$Sector[i],
              scarcity_result$Dif_Sal_2023[i],
              scarcity_result$Dif_Sal_2024[i],
              scarcity_result$Dif_Sal_Change[i]))
}

cat("\n\nTURNOVER RATE (Taxa de Rotatividade) - Higher = More Labor Market Churn:\n")
cat("--------------------------------------------------------------------------------\n")
cat(sprintf("%-25s %15s %15s %15s\n", "Sector", "2023", "2024", "Change"))
cat("--------------------------------------------------------------------------------\n")
for (i in 1:nrow(scarcity_result)) {
  cat(sprintf("%-25s %14.2f %14.2f %+14.2f\n",
              scarcity_result$Sector[i],
              scarcity_result$Rotatividade_2023[i],
              scarcity_result$Rotatividade_2024[i],
              scarcity_result$Rotatividade_Change[i]))
}

cat("\n\nEMPLOYMENT STOCK (Estoque Líquido):\n")
cat("--------------------------------------------------------------------------------\n")
cat(sprintf("%-25s %15s %15s %15s\n", "Sector", "2023", "2024", "Growth %"))
cat("--------------------------------------------------------------------------------\n")
for (i in 1:nrow(scarcity_result)) {
  cat(sprintf("%-25s %15s %15s %14.1f%%\n",
              scarcity_result$Sector[i],
              format(round(scarcity_result$Estoque_2023[i]), big.mark = ","),
              format(round(scarcity_result$Estoque_2024[i]), big.mark = ","),
              scarcity_result$Estoque_Growth_Pct[i]))
}

cat("\n================================================================================\n")

# =============================================================================
# STEP 10: SAVE RESULTS
# =============================================================================

cat("\n=== Saving results ===\n")

# Save detailed results
write.csv(scarcity_result, 
          "D:/Country/Brazil/TechBrazil/working/rais/five_sectors_scarcity.csv", 
          row.names = FALSE)
cat("✅ Saved: five_sectors_scarcity.csv\n")

# Save monthly time series for potential plotting
write.csv(scarcity_sector_monthly, 
          "D:/Country/Brazil/TechBrazil/working/rais/five_sectors_scarcity_monthly.csv", 
          row.names = FALSE)
cat("✅ Saved: five_sectors_scarcity_monthly.csv\n")

# Also save as .rda for easy loading (one object per file, filename = object name)
save(scarcity_result, 
     file = "D:/Country/Brazil/TechBrazil/working/rais/scarcity_result.rda")
cat("✅ Saved: scarcity_result.rda\n")

save(scarcity_sector_monthly, 
     file = "D:/Country/Brazil/TechBrazil/working/rais/scarcity_sector_monthly.rda")
cat("✅ Saved: scarcity_sector_monthly.rda\n")

save(curso_sector, 
     file = "D:/Country/Brazil/TechBrazil/working/rais/curso_sector.rda")
cat("✅ Saved: curso_sector.rda\n")

cat("\n=== Done ===\n")