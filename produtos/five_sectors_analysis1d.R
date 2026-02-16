# five_sectors_analysis1d.R
#
# Calculate by UF x Sector:
# 1. Tech Employment Potential (absolute numbers)
# 2. Scarcity Index (normalized to All Técnicos Brasil = 100)
#
# Outputs two matrices for heatmap and choropleth visualization

library(dplyr)
library(tidyr)
library(stringr)

# =============================================================================
# PART A: TECH EMPLOYMENT POTENTIAL BY UF x SECTOR
# =============================================================================

cat("=== PART A: Tech Employment Potential by UF x Sector ===\n")

# -----------------------------------------------------------------------------
# STEP A1: LOAD RAIS DATA
# -----------------------------------------------------------------------------

cat("\n=== Loading RAIS data ===\n")

load("D:/Country/Brazil/TechBrazil/working/rais/2023/rais2023.rda")
load("D:/Country/Brazil/TechBrazil/working/rais/2024/rais2024.rda")
load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/cnct_qbq_matches2.rda")

cat(sprintf("RAIS 2023 rows: %s\n", format(nrow(rais2023), big.mark = ",")))
cat(sprintf("RAIS 2024 rows: %s\n", format(nrow(rais2024), big.mark = ",")))

# -----------------------------------------------------------------------------
# STEP A2: CREATE CBO → SECTOR MAPPING
# -----------------------------------------------------------------------------

cat("\n=== Creating CBO → Sector mapping ===\n")

cbo_area <- cnct_qbq_matches2 %>%
  group_by(CodCBO) %>%
  slice_max(final_score, n = 1) %>%
  ungroup() %>%
  select(CodCBO, `Área Tecnológica`) %>%
  distinct()

sector_mapping <- data.frame(
  Area_Tecnologica = c(
    "Gestão e Promoção da Saúde e Bem-Estar",
    "Acolhimento e Hospedagem", "Serviços de Gastronomia", "Atividades Turísticas", 
    "Recreação e Sociabilidade", "Apoio técnico a eventos",
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
  )
)

cbo_sector <- cbo_area %>%
  left_join(sector_mapping, by = c("Área Tecnológica" = "Area_Tecnologica")) %>%
  filter(!is.na(Sector))

# -----------------------------------------------------------------------------
# STEP A3: STANDARDIZE COLUMN NAMES
# -----------------------------------------------------------------------------

cat("\n=== Standardizing column names ===\n")

# For 2023
if ("CBO Ocupação 2002" %in% names(rais2023)) {
  rais2023 <- rais2023 %>% rename(
    CodCBO = `CBO Ocupação 2002`,
    vinculos = `Vínculo Ativo 31/12`,
    escolaridade = `Escolaridade após 2005`
  )
} else if ("CBO 2002 Ocupação - Código" %in% names(rais2023)) {
  rais2023 <- rais2023 %>% rename(
    CodCBO = `CBO 2002 Ocupação - Código`,
    vinculos = `Ind Vínculo Ativo 31/12 - Código`,
    escolaridade = `Escolaridade Após 2005 - Código`
  )
}

# For 2024
if ("CBO 2002 Ocupação - Código" %in% names(rais2024)) {
  rais2024 <- rais2024 %>% rename(
    CodCBO = `CBO 2002 Ocupação - Código`,
    vinculos = `Ind Vínculo Ativo 31/12 - Código`,
    escolaridade = `Escolaridade Após 2005 - Código`
  )
} else if ("CBO Ocupação 2002" %in% names(rais2024)) {
  rais2024 <- rais2024 %>% rename(
    CodCBO = `CBO Ocupação 2002`,
    vinculos = `Vínculo Ativo 31/12`,
    escolaridade = `Escolaridade após 2005`
  )
}

rais2023$CodCBO <- as.character(rais2023$CodCBO)
rais2024$CodCBO <- as.character(rais2024$CodCBO)

# No UF code mapping needed - SG_UF already has state abbreviations (RN, PA, BA, SP, etc.)

# -----------------------------------------------------------------------------
# STEP A5: CALCULATE TECH POTENTIAL BY UF x SECTOR
# -----------------------------------------------------------------------------

cat("\n=== Calculating Tech Potential by UF x Sector ===\n")

# Function to calculate tech potential for one year
calc_tech_potential <- function(rais_data, cbo_sector_map) {
  rais_data %>%
    inner_join(cbo_sector_map, by = "CodCBO") %>%
    filter(vinculos == 1) %>%
    mutate(GG = substr(CodCBO, 1, 1)) %>%
    filter(!is.na(NM_UF) & NM_UF != "") %>%
    group_by(NM_UF, Sector) %>%
    summarise(
      less_than_em = sum(escolaridade <= 6),
      em_gg3 = sum(escolaridade == 7 & GG == "3"),
      em_non_gg3 = sum(escolaridade == 7 & GG != "3"),
      superior_plus = sum(escolaridade >= 8),
      total = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      tech_potential = em_gg3 + 0.5 * (less_than_em + em_non_gg3)
    )
}

# Calculate for 2023 and 2024
tech_2023 <- calc_tech_potential(rais2023, cbo_sector) %>%
  select(NM_UF, Sector, tech_potential_2023 = tech_potential)

tech_2024 <- calc_tech_potential(rais2024, cbo_sector) %>%
  select(NM_UF, Sector, tech_potential_2024 = tech_potential)

# Combine and average
tech_potential_long <- tech_2023 %>%
  full_join(tech_2024, by = c("NM_UF", "Sector")) %>%
  mutate(
    tech_potential_2023 = ifelse(is.na(tech_potential_2023), 0, tech_potential_2023),
    tech_potential_2024 = ifelse(is.na(tech_potential_2024), 0, tech_potential_2024),
    tech_potential_avg = (tech_potential_2023 + tech_potential_2024) / 2
  )

# Pivot to wide format (UF rows, Sector columns)
tech_potential_uf_sector <- tech_potential_long %>%
  select(NM_UF, Sector, tech_potential_avg) %>%
  pivot_wider(names_from = Sector, values_from = tech_potential_avg, values_fill = 0) %>%
  arrange(NM_UF)

# Add row totals
tech_potential_uf_sector <- tech_potential_uf_sector %>%
  mutate(TOTAL = rowSums(select(., -NM_UF)))

# Add column totals row
col_totals <- tech_potential_uf_sector %>%
  summarise(across(-NM_UF, sum)) %>%
  mutate(NM_UF = "TOTAL")

tech_potential_uf_sector <- bind_rows(tech_potential_uf_sector, col_totals)

cat("\nTech Employment Potential by UF x Sector (sample):\n")
print(head(tech_potential_uf_sector, 10))

# =============================================================================
# PART B: SCARCITY INDEX BY UF x SECTOR
# =============================================================================

cat("\n\n=== PART B: Scarcity Index by UF x Sector ===\n")

# -----------------------------------------------------------------------------
# STEP B1: LOAD SCARCITY DATA
# -----------------------------------------------------------------------------

cat("\n=== Loading Scarcity data ===\n")

load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/caged_rais_cnct_2020_2024_shiny.rda")
load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/df_exarcu.rda")

# Get column names
area_col <- names(df_exarcu)[grepl("rea", names(df_exarcu), ignore.case = TRUE)][1]
curso_col <- names(df_exarcu)[grepl("Denomina", names(df_exarcu), ignore.case = TRUE)][1]

# Clean curso names in CNCT catalogue
classification_clean <- df_exarcu %>%
  select(Curso_CNCT_orig = all_of(curso_col), Area_Tecnologica = all_of(area_col)) %>%
  distinct() %>%
  mutate(
    curso_clean = str_remove(Curso_CNCT_orig, "^Técnico em\\s+"),
    curso_clean = str_trim(tolower(curso_clean))
  )

# Clean CAGED curso names
caged_rais_clean <- caged_rais_curso %>%
  mutate(curso_clean = str_trim(tolower(Curso)))

# -----------------------------------------------------------------------------
# STEP B2: CALCULATE BRASIL BASELINE (All Técnicos = 100)
# -----------------------------------------------------------------------------

cat("\n=== Calculating Brasil baseline ===\n")

# Use MEDIAN across all 2023-2024 data for baseline (consistent with UF calculation)
baseline_scarcity <- caged_rais_clean %>%
  filter(NM_UF == "Brasil") %>%
  filter(ANO %in% c(2023, 2024)) %>%
  summarise(
    dif_sal_baseline = median(dif_sal_adm_des_pc_m12, na.rm = TRUE),
    .groups = 'drop'
  )

baseline_dif_sal <- baseline_scarcity$dif_sal_baseline
cat(sprintf("Brasil All Técnicos baseline (median): %.2f%%\n", baseline_dif_sal))

# -----------------------------------------------------------------------------
# STEP B3: CREATE CURSO → SECTOR MAPPING FOR SCARCITY
# -----------------------------------------------------------------------------

sector_mapping_scarcity <- data.frame(
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

curso_sector_scarcity <- classification_clean %>%
  inner_join(sector_mapping_scarcity, by = "Area_Tecnologica") %>%
  select(curso_clean, Sector) %>%
  distinct()

# -----------------------------------------------------------------------------
# STEP B4: CALCULATE SCARCITY BY UF x SECTOR
# -----------------------------------------------------------------------------

cat("\n=== Calculating Scarcity by UF x Sector ===\n")

# Join scarcity data to sectors
scarcity_with_sector <- caged_rais_clean %>%
  inner_join(curso_sector_scarcity, by = "curso_clean")

# Calculate by UF x Sector 
# - Pool ALL months from 2023-2024 (not just latest)
# - Use MEDIAN to reduce outlier impact
# - Filter cells with estoque_liquido >= 100

scarcity_uf_sector_long <- scarcity_with_sector %>%
  filter(NM_UF != "Brasil") %>%
  filter(ANO %in% c(2023, 2024)) %>%
  group_by(NM_UF, Sector) %>%
  summarise(
    dif_sal_median = median(dif_sal_adm_des_pc_m12, na.rm = TRUE),
    estoque_total = sum(estoque_liquido, na.rm = TRUE),
    n_obs = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    # Only calculate scarcity index for cells with sufficient data
    # Floor negative values to 0 (no scarcity)
    scarcity_index = ifelse(
      estoque_total >= 100,
      pmax(0, round(dif_sal_median / baseline_dif_sal * 100)),
      NA_real_
    )
  )

cat(sprintf("Cells with sufficient data (estoque >= 100): %d of %d\n",
            sum(!is.na(scarcity_uf_sector_long$scarcity_index)),
            nrow(scarcity_uf_sector_long)))

# Pivot to wide format
scarcity_index_uf_sector <- scarcity_uf_sector_long %>%
  select(NM_UF, Sector, scarcity_index) %>%
  pivot_wider(names_from = Sector, values_from = scarcity_index, values_fill = 0) %>%
  arrange(NM_UF)

# Add row averages (only for non-NA cells)
scarcity_index_uf_sector <- scarcity_index_uf_sector %>%
  rowwise() %>%
  mutate(AVG = {
    vals <- c_across(-NM_UF)
    vals <- vals[!is.na(vals)]
    if(length(vals) > 0) round(mean(vals)) else NA_real_
  }) %>%
  ungroup()

# Add BRASIL row (calculated from full Brasil data, always has sufficient volume)
brasil_scarcity <- scarcity_with_sector %>%
  filter(NM_UF == "Brasil") %>%
  filter(ANO %in% c(2023, 2024)) %>%
  group_by(Sector) %>%
  summarise(
    dif_sal_median = median(dif_sal_adm_des_pc_m12, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(scarcity_index = pmax(0, round(dif_sal_median / baseline_dif_sal * 100))) %>%
  select(Sector, scarcity_index) %>%
  pivot_wider(names_from = Sector, values_from = scarcity_index) %>%
  mutate(NM_UF = "BRASIL", AVG = NA)

scarcity_index_uf_sector <- bind_rows(scarcity_index_uf_sector, brasil_scarcity)

cat("\nScarcity Index by UF x Sector (sample):\n")
print(head(scarcity_index_uf_sector, 10))

# =============================================================================
# STEP C: PRINT SUMMARY
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("       SUMMARY: UF x SECTOR MATRICES CREATED                                   \n")
cat("================================================================================\n\n")

cat("1. tech_potential_uf_sector:\n")
cat(sprintf("   - Dimensions: %d UFs x %d Sectors + TOTAL\n", 
            nrow(tech_potential_uf_sector) - 1, ncol(tech_potential_uf_sector) - 2))
cat(sprintf("   - Total Tech Employment Potential: %.2f M\n", 
            tech_potential_uf_sector$TOTAL[tech_potential_uf_sector$NM_UF == "TOTAL"] / 1e6))

cat("\n2. scarcity_index_uf_sector:\n")
cat(sprintf("   - Dimensions: %d UFs x %d Sectors + AVG\n", 
            nrow(scarcity_index_uf_sector) - 1, ncol(scarcity_index_uf_sector) - 2))
cat(sprintf("   - Baseline (All Técnicos Brasil): 100 (= %.2f%% salary differential)\n", baseline_dif_sal))

# =============================================================================
# STEP D: SAVE RESULTS
# =============================================================================

cat("\n=== Saving results ===\n")

save(tech_potential_uf_sector, file = "D:/Country/Brazil/TechBrazil/working/rais/tech_potential_uf_sector.rda")
cat("✅ Saved: tech_potential_uf_sector.rda\n")

save(scarcity_index_uf_sector, file = "D:/Country/Brazil/TechBrazil/working/rais/scarcity_index_uf_sector.rda")
cat("✅ Saved: scarcity_index_uf_sector.rda\n")

cat("\n=== Done ===\n")