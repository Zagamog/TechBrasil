# cbo_grande_grupo_analysis1c.R
#
# Analysis of 5 priority sectors employment by Education Level (Escolaridade)
# Shows education composition within the 5 sectors for PROPAG target population

library(dplyr)

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
# STEP 4: FILTER TO 5 SECTORS AND CALCULATE BY EDUCATION LEVEL
# =============================================================================

cat("\n=== Calculating totals by Education Level for 5 sectors ===\n")

# 2023 - filter to 5 sectors
rais2023_5sec <- rais2023 %>%
  inner_join(cbo_sector, by = "CodCBO") %>%
  filter(vinculos == 1)

edu_5sec_2023 <- rais2023_5sec %>%
  group_by(escolaridade) %>%
  summarise(Total_2023 = n(), .groups = 'drop')

# 2024 - filter to 5 sectors
rais2024_5sec <- rais2024 %>%
  inner_join(cbo_sector, by = "CodCBO") %>%
  filter(vinculos == 1)

edu_5sec_2024 <- rais2024_5sec %>%
  group_by(escolaridade) %>%
  summarise(Total_2024 = n(), .groups = 'drop')

# =============================================================================
# STEP 5: COMBINE RESULTS
# =============================================================================

edu_descriptions <- data.frame(
  escolaridade = 1:11,
  Descricao = c(
    "1 - Analfabeto",
    "2 - Até 5ª Incompleto",
    "3 - 5ª Completo Fundamental",
    "4 - 6ª a 9ª Fundamental",
    "5 - Fundamental Completo",
    "6 - Médio Incompleto",
    "7 - Médio Completo",
    "8 - Superior Incompleto",
    "9 - Superior Completo",
    "10 - Mestrado",
    "11 - Doutorado"
  ),
  stringsAsFactors = FALSE
)

edu_five_sectors <- edu_descriptions %>%
  left_join(edu_5sec_2023, by = "escolaridade") %>%
  left_join(edu_5sec_2024, by = "escolaridade") %>%
  mutate(
    Total_2023 = ifelse(is.na(Total_2023), 0, Total_2023),
    Total_2024 = ifelse(is.na(Total_2024), 0, Total_2024)
  )

# Add subtotal rows
subtotal_less_medio <- data.frame(
  escolaridade = NA,
  Descricao = "SUBTOTAL: Less than Médio Completo (1-6)",
  Total_2023 = sum(edu_five_sectors$Total_2023[edu_five_sectors$escolaridade <= 6]),
  Total_2024 = sum(edu_five_sectors$Total_2024[edu_five_sectors$escolaridade <= 6])
)

subtotal_less_superior <- data.frame(
  escolaridade = NA,
  Descricao = "SUBTOTAL: Less than Superior Completo (1-8)",
  Total_2023 = sum(edu_five_sectors$Total_2023[edu_five_sectors$escolaridade <= 8]),
  Total_2024 = sum(edu_five_sectors$Total_2024[edu_five_sectors$escolaridade <= 8])
)

subtotal_superior_plus <- data.frame(
  escolaridade = NA,
  Descricao = "SUBTOTAL: Superior Completo+ (9-11)",
  Total_2023 = sum(edu_five_sectors$Total_2023[edu_five_sectors$escolaridade >= 9]),
  Total_2024 = sum(edu_five_sectors$Total_2024[edu_five_sectors$escolaridade >= 9])
)

total_row <- data.frame(
  escolaridade = NA,
  Descricao = "TOTAL 5 SECTORS",
  Total_2023 = sum(edu_five_sectors$Total_2023),
  Total_2024 = sum(edu_five_sectors$Total_2024)
)

edu_five_sectors <- rbind(edu_five_sectors, subtotal_less_medio, subtotal_less_superior, 
                          subtotal_superior_plus, total_row)

# =============================================================================
# STEP 6: PRINT RESULTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("       5 PRIORITY SECTORS - EMPLOYMENT BY EDUCATION LEVEL (2023 vs 2024)       \n")
cat("================================================================================\n\n")

cat(sprintf("%-45s %15s %15s\n", "Education Level", "2023", "2024"))
cat("--------------------------------------------------------------------------------\n")

for (i in 1:nrow(edu_five_sectors)) {
  cat(sprintf("%-45s %15s %15s\n",
              edu_five_sectors$Descricao[i],
              format(edu_five_sectors$Total_2023[i], big.mark = ","),
              format(edu_five_sectors$Total_2024[i], big.mark = ",")))
}
cat("================================================================================\n")

# =============================================================================
# STEP 7: SAVE RESULTS
# =============================================================================

cat("\n=== Saving results ===\n")

save(edu_five_sectors, file = "D:/Country/Brazil/TechBrazil/working/rais/edu_five_sectors.rda")
cat("✅ Saved: edu_five_sectors.rda\n")

cat("\n=== Done ===\n")