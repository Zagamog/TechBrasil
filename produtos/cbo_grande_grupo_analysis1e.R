# cbo_grande_grupo_analysis1e.R
#
# Calculate Technical Employment Potential by Sector
# Combines with scarcity index for final PROPAG prioritization table

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
# STEP 4: FILTER TO 5 SECTORS AND CALCULATE BY SECTOR
# =============================================================================

cat("\n=== Calculating Education x GG by Sector ===\n")

# 2023
rais2023_5sec <- rais2023 %>%
  inner_join(cbo_sector, by = "CodCBO") %>%
  filter(vinculos == 1) %>%
  mutate(GG = substr(CodCBO, 1, 1))

by_sector_2023 <- rais2023_5sec %>%
  group_by(Sector) %>%
  summarise(
    less_than_em = sum(escolaridade <= 6),
    em_gg3 = sum(escolaridade == 7 & GG == "3"),
    em_non_gg3 = sum(escolaridade == 7 & GG != "3"),
    superior_plus = sum(escolaridade >= 8),
    total = n(),
    .groups = 'drop'
  ) %>%
  mutate(year = 2023)

# 2024
rais2024_5sec <- rais2024 %>%
  inner_join(cbo_sector, by = "CodCBO") %>%
  filter(vinculos == 1) %>%
  mutate(GG = substr(CodCBO, 1, 1))

by_sector_2024 <- rais2024_5sec %>%
  group_by(Sector) %>%
  summarise(
    less_than_em = sum(escolaridade <= 6),
    em_gg3 = sum(escolaridade == 7 & GG == "3"),
    em_non_gg3 = sum(escolaridade == 7 & GG != "3"),
    superior_plus = sum(escolaridade >= 8),
    total = n(),
    .groups = 'drop'
  ) %>%
  mutate(year = 2024)

# =============================================================================
# STEP 5: CALCULATE AVERAGES AND TECH EMPLOYMENT POTENTIAL
# =============================================================================

cat("\n=== Calculating Technical Employment Potential by Sector ===\n")

# Combine and average
by_sector_avg <- by_sector_2023 %>%
  select(Sector, less_than_em_2023 = less_than_em, em_gg3_2023 = em_gg3, 
         em_non_gg3_2023 = em_non_gg3, superior_plus_2023 = superior_plus, total_2023 = total) %>%
  left_join(
    by_sector_2024 %>%
      select(Sector, less_than_em_2024 = less_than_em, em_gg3_2024 = em_gg3,
             em_non_gg3_2024 = em_non_gg3, superior_plus_2024 = superior_plus, total_2024 = total),
    by = "Sector"
  ) %>%
  mutate(
    less_than_em_avg = (less_than_em_2023 + less_than_em_2024) / 2,
    em_gg3_avg = (em_gg3_2023 + em_gg3_2024) / 2,
    em_non_gg3_avg = (em_non_gg3_2023 + em_non_gg3_2024) / 2,
    superior_plus_avg = (superior_plus_2023 + superior_plus_2024) / 2,
    total_avg = (total_2023 + total_2024) / 2
  )

# Calculate Tech Employment Potential
# = EM Técnico (already) + 50% of (Less than EM + EM Regular)
tech_potential_by_sector <- by_sector_avg %>%
  mutate(
    tech_employment_potential = em_gg3_avg + 0.5 * (less_than_em_avg + em_non_gg3_avg)
  ) %>%
  select(Sector, tech_employment_potential) %>%
  arrange(desc(tech_employment_potential))

# Add total row
total_row <- data.frame(
  Sector = "TOTAL 5 SECTORS",
  tech_employment_potential = sum(tech_potential_by_sector$tech_employment_potential)
)

tech_potential_by_sector <- rbind(tech_potential_by_sector, total_row)

# =============================================================================
# STEP 6: PRINT RESULTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("       TECHNICAL EMPLOYMENT POTENTIAL BY SECTOR (Average 2023-2024)            \n")
cat("================================================================================\n\n")

cat(sprintf("%-30s %25s\n", "Sector", "Tech Employment Potential"))
cat("--------------------------------------------------------------------------------\n")

for (i in 1:nrow(tech_potential_by_sector)) {
  cat(sprintf("%-30s %20s\n",
              tech_potential_by_sector$Sector[i],
              format(round(tech_potential_by_sector$tech_employment_potential[i]), big.mark = ",")))
}
cat("================================================================================\n")

# =============================================================================
# STEP 7: SAVE RESULTS
# =============================================================================

cat("\n=== Saving results ===\n")

save(tech_potential_by_sector, file = "D:/Country/Brazil/TechBrazil/working/rais/tech_potential_by_sector.rda")
cat("✅ Saved: tech_potential_by_sector.rda\n")

cat("\n=== Done ===\n")