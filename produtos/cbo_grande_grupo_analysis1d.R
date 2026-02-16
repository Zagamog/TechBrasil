# cbo_grande_grupo_analysis1d.R
#
# Cross-tabulation of Education Level x CBO Grande Grupo for 5 priority sectors
# Used to calculate PROPAG target population with imputation for EM Técnico vs EM Regular

library(dplyr)
library(tidyr)

# =============================================================================
# STEP 1: LOAD DATA
# =============================================================================

cat("=== Loading data ===\n")

load("D:/Country/Brazil/TechBrazil/working/rais/2023/rais2023.rda")
load("D:/Country/Brazil/TechBrazil/working/rais/2024/rais2024.rda")
load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/cnct_qbq_matches2.rda")

cat(sprintf("RAIS 2023 rows: %s\n", format(nrow(rais2023), big.mark = ",")))
cat(sprintf("RAIS 2024 rows: %s\n", format(nrow(rais2024), big.mark = ",")))

# =============================================================================
# STEP 2: CREATE CBO → SECTOR MAPPING
# =============================================================================

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

# =============================================================================
# STEP 3: STANDARDIZE COLUMN NAMES
# =============================================================================

cat("\n=== Standardizing column names ===\n")

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

# =============================================================================
# STEP 4: FILTER TO 5 SECTORS AND CREATE CROSS-TABULATION
# =============================================================================

cat("\n=== Creating Education x Grande Grupo cross-tabulation for 5 sectors ===\n")

# 2023 - filter to 5 sectors
rais2023_5sec <- rais2023 %>%
  inner_join(cbo_sector, by = "CodCBO") %>%
  filter(vinculos == 1) %>%
  mutate(GG = substr(CodCBO, 1, 1))

crosstab_2023 <- rais2023_5sec %>%
  group_by(escolaridade, GG) %>%
  summarise(Total_2023 = n(), .groups = 'drop')

# 2024 - filter to 5 sectors
rais2024_5sec <- rais2024 %>%
  inner_join(cbo_sector, by = "CodCBO") %>%
  filter(vinculos == 1) %>%
  mutate(GG = substr(CodCBO, 1, 1))

crosstab_2024 <- rais2024_5sec %>%
  group_by(escolaridade, GG) %>%
  summarise(Total_2024 = n(), .groups = 'drop')

# =============================================================================
# STEP 5: COMBINE AND FORMAT RESULTS
# =============================================================================

edu_gg_five_sectors <- crosstab_2023 %>%
  full_join(crosstab_2024, by = c("escolaridade", "GG")) %>%
  mutate(
    Total_2023 = ifelse(is.na(Total_2023), 0, Total_2023),
    Total_2024 = ifelse(is.na(Total_2024), 0, Total_2024)
  ) %>%
  arrange(escolaridade, GG)

# =============================================================================
# STEP 6: CALCULATE PROPAG TARGET POPULATION
# =============================================================================

cat("\n=== Calculating PROPAG Target Population ===\n")

# A. Less than EM Completo (Escolaridade 1-6)
less_than_em_2023 <- sum(edu_gg_five_sectors$Total_2023[edu_gg_five_sectors$escolaridade <= 6])
less_than_em_2024 <- sum(edu_gg_five_sectors$Total_2024[edu_gg_five_sectors$escolaridade <= 6])

# B. EM Completo (Escolaridade 7) split by GG3 vs non-GG3
em_gg3_2023 <- sum(edu_gg_five_sectors$Total_2023[edu_gg_five_sectors$escolaridade == 7 & edu_gg_five_sectors$GG == "3"])
em_gg3_2024 <- sum(edu_gg_five_sectors$Total_2024[edu_gg_five_sectors$escolaridade == 7 & edu_gg_five_sectors$GG == "3"])

em_non_gg3_2023 <- sum(edu_gg_five_sectors$Total_2023[edu_gg_five_sectors$escolaridade == 7 & edu_gg_five_sectors$GG != "3"])
em_non_gg3_2024 <- sum(edu_gg_five_sectors$Total_2024[edu_gg_five_sectors$escolaridade == 7 & edu_gg_five_sectors$GG != "3"])

# C. Superior+ (Escolaridade 8-11)
superior_plus_2023 <- sum(edu_gg_five_sectors$Total_2023[edu_gg_five_sectors$escolaridade >= 8])
superior_plus_2024 <- sum(edu_gg_five_sectors$Total_2024[edu_gg_five_sectors$escolaridade >= 8])

# Total 5 sectors
total_5sec_2023 <- sum(edu_gg_five_sectors$Total_2023)
total_5sec_2024 <- sum(edu_gg_five_sectors$Total_2024)

# PROPAG Target = 50% of (Less than EM) + 50% of (EM Regular)
target_2023 <- 0.5 * less_than_em_2023 + 0.5 * em_non_gg3_2023
target_2024 <- 0.5 * less_than_em_2024 + 0.5 * em_non_gg3_2024

# =============================================================================
# STEP 7: CREATE SUMMARY TABLE
# =============================================================================

propag_target_summary <- data.frame(
  Category = c(
    "A. Less than EM Completo (Esc 1-6)",
    "B. EM Técnico (imputed: Esc 7 + GG3)",
    "C. EM Regular (Esc 7 + GG ≠ 3)",
    "D. Superior+ (Esc 8-11)",
    "TOTAL 5 SECTORS",
    "",
    "PROPAG TARGET: 50% of A + 50% of C"
  ),
  Total_2023 = c(
    less_than_em_2023,
    em_gg3_2023,
    em_non_gg3_2023,
    superior_plus_2023,
    total_5sec_2023,
    NA,
    target_2023
  ),
  Total_2024 = c(
    less_than_em_2024,
    em_gg3_2024,
    em_non_gg3_2024,
    superior_plus_2024,
    total_5sec_2024,
    NA,
    target_2024
  )
)

# =============================================================================
# STEP 8: PRINT RESULTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("       5 SECTORS: EDUCATION x GRANDE GRUPO CROSS-TABULATION (2023 vs 2024)     \n")
cat("================================================================================\n\n")

cat("PROPAG TARGET POPULATION SUMMARY:\n")
cat("--------------------------------------------------------------------------------\n")
cat(sprintf("%-45s %15s %15s\n", "Category", "2023", "2024"))
cat("--------------------------------------------------------------------------------\n")

for (i in 1:nrow(propag_target_summary)) {
  if (is.na(propag_target_summary$Total_2023[i])) {
    cat("--------------------------------------------------------------------------------\n")
  } else {
    cat(sprintf("%-45s %15s %15s\n",
                propag_target_summary$Category[i],
                format(round(propag_target_summary$Total_2023[i]), big.mark = ","),
                format(round(propag_target_summary$Total_2024[i]), big.mark = ",")))
  }
}
cat("================================================================================\n")

# =============================================================================
# STEP 9: SAVE RESULTS
# =============================================================================

cat("\n=== Saving results ===\n")

save(edu_gg_five_sectors, file = "D:/Country/Brazil/TechBrazil/working/rais/edu_gg_five_sectors.rda")
cat("✅ Saved: edu_gg_five_sectors.rda\n")

save(propag_target_summary, file = "D:/Country/Brazil/TechBrazil/working/rais/propag_target_summary.rda")
cat("✅ Saved: propag_target_summary.rda\n")

cat("\n=== Done ===\n")