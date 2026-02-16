# cbo_grande_grupo_analysis1a.R
#
# Analysis of 5 priority sectors employment by CBO Grande Grupo
# Shows occupational composition within the 5 sectors

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
    vinculos = `Vínculo Ativo 31/12`
  )
} else if ("CBO 2002 Ocupação - Código" %in% names(rais2023)) {
  rais2023 <- rais2023 %>% rename(
    CodCBO = `CBO 2002 Ocupação - Código`,
    vinculos = `Ind Vínculo Ativo 31/12 - Código`
  )
}

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

rais2023$CodCBO <- as.character(rais2023$CodCBO)
rais2024$CodCBO <- as.character(rais2024$CodCBO)

# =============================================================================
# STEP 4: FILTER TO 5 SECTORS AND CALCULATE BY GRANDE GRUPO
# =============================================================================

cat("\n=== Calculating totals by Grande Grupo for 5 sectors ===\n")

# 2023 - filter to 5 sectors
rais2023_5sec <- rais2023 %>%
  inner_join(cbo_sector, by = "CodCBO") %>%
  filter(vinculos == 1)

gg_5sec_2023 <- rais2023_5sec %>%
  mutate(GG = substr(CodCBO, 1, 1)) %>%
  group_by(GG) %>%
  summarise(Total_2023 = n(), .groups = 'drop')

# 2024 - filter to 5 sectors
rais2024_5sec <- rais2024 %>%
  inner_join(cbo_sector, by = "CodCBO") %>%
  filter(vinculos == 1)

gg_5sec_2024 <- rais2024_5sec %>%
  mutate(GG = substr(CodCBO, 1, 1)) %>%
  group_by(GG) %>%
  summarise(Total_2024 = n(), .groups = 'drop')

# =============================================================================
# STEP 5: COMBINE RESULTS
# =============================================================================

gg_descriptions <- data.frame(
  GG = as.character(0:9),
  Descricao = c(
    "0 - Forças Armadas, Policiais e Bombeiros",
    "1 - Dirigentes e Gerentes",
    "2 - Profissionais das Ciências e das Artes",
    "3 - Técnicos de Nível Médio",
    "4 - Trabalhadores de Serviços Administrativos",
    "5 - Trabalhadores dos Serviços, Vendedores",
    "6 - Trabalhadores Agropecuários, Florestais",
    "7 - Trabalhadores Produção Bens Industriais I",
    "8 - Trabalhadores Produção Bens Industriais II",
    "9 - Trabalhadores Manutenção e Reparação"
  ),
  stringsAsFactors = FALSE
)

cbo_gg_five_sectors <- gg_descriptions %>%
  left_join(gg_5sec_2023, by = "GG") %>%
  left_join(gg_5sec_2024, by = "GG") %>%
  mutate(
    Total_2023 = ifelse(is.na(Total_2023), 0, Total_2023),
    Total_2024 = ifelse(is.na(Total_2024), 0, Total_2024)
  )

# Add totals row
total_row <- data.frame(
  GG = "",
  Descricao = "TOTAL 5 SECTORS",
  Total_2023 = sum(cbo_gg_five_sectors$Total_2023),
  Total_2024 = sum(cbo_gg_five_sectors$Total_2024)
)

cbo_gg_five_sectors <- rbind(cbo_gg_five_sectors, total_row)

# =============================================================================
# STEP 6: PRINT RESULTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("       5 PRIORITY SECTORS - EMPLOYMENT BY CBO GRANDE GRUPO (2023 vs 2024)      \n")
cat("================================================================================\n\n")

cat(sprintf("%-50s %15s %15s\n", "Grande Grupo", "2023", "2024"))
cat("--------------------------------------------------------------------------------\n")

for (i in 1:nrow(cbo_gg_five_sectors)) {
  cat(sprintf("%-50s %15s %15s\n",
              cbo_gg_five_sectors$Descricao[i],
              format(cbo_gg_five_sectors$Total_2023[i], big.mark = ","),
              format(cbo_gg_five_sectors$Total_2024[i], big.mark = ",")))
}
cat("================================================================================\n")

# =============================================================================
# STEP 7: SAVE RESULTS
# =============================================================================

cat("\n=== Saving results ===\n")

save(cbo_gg_five_sectors, file = "D:/Country/Brazil/TechBrazil/working/rais/cbo_gg_five_sectors.rda")
cat("✅ Saved: cbo_gg_five_sectors.rda\n")

cat("\n=== Done ===\n")