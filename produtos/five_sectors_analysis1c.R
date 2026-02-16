# five_sectors_analysis1c.R
#
# Final PROPAG prioritization table combining:
# 1. Technical Employment Potential by sector
# 2. Scarcity Index normalized to All Técnicos = 100
#
# Requires running first:
# - cbo_grande_grupo_analysis1e.R (tech_potential_by_sector.rda)
# - five_sectors_analysis1b.R (scarcity data)

library(dplyr)
library(tidyr)
library(stringr)

# =============================================================================
# STEP 1: LOAD TECH POTENTIAL DATA
# =============================================================================

cat("=== Loading Tech Potential data ===\n")

load("D:/Country/Brazil/TechBrazil/working/rais/tech_potential_by_sector.rda")

cat("Tech Potential by Sector:\n")
print(tech_potential_by_sector)

# =============================================================================
# STEP 2: LOAD SCARCITY DATA AND CALCULATE ALL TÉCNICOS BASELINE
# =============================================================================

cat("\n=== Loading Scarcity data ===\n")

# Load CAGED/RAIS scarcity data
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

# =============================================================================
# STEP 3: CALCULATE ALL TÉCNICOS SCARCITY (BASELINE)
# =============================================================================

cat("\n=== Calculating All Técnicos baseline scarcity ===\n")

# Get latest data for Brasil - ALL cursos (not just 5 sectors)
all_tecnicos_scarcity <- caged_rais_clean %>%
  filter(NM_UF == "Brasil") %>%
  filter(ANO %in% c(2023, 2024)) %>%
  group_by(ANO) %>%
  filter(MES == max(MES)) %>%
  ungroup() %>%
  summarise(
    dif_sal_all = weighted.mean(dif_sal_adm_des_pc_m12, w = estoque_liquido, na.rm = TRUE),
    estoque_all = sum(estoque_liquido, na.rm = TRUE),
    .groups = 'drop'
  )

baseline_dif_sal <- all_tecnicos_scarcity$dif_sal_all
cat(sprintf("All Técnicos baseline salary differential: %.2f%%\n", baseline_dif_sal))

# =============================================================================
# STEP 4: CALCULATE 5 SECTORS SCARCITY WITH NORMALIZATION
# =============================================================================

cat("\n=== Calculating 5 Sectors scarcity (normalized) ===\n")

# Define sector mapping
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

# Create curso → sector mapping
curso_sector <- classification_clean %>%
  inner_join(sector_mapping, by = "Area_Tecnologica") %>%
  select(curso_clean, Sector) %>%
  distinct()

# Join scarcity data to sectors
scarcity_with_sector <- caged_rais_clean %>%
  inner_join(curso_sector, by = "curso_clean")

# Calculate scarcity by sector (average of 2023-2024)
scarcity_by_sector <- scarcity_with_sector %>%
  filter(NM_UF == "Brasil") %>%
  filter(ANO %in% c(2023, 2024)) %>%
  group_by(ANO, Sector) %>%
  filter(MES == max(MES)) %>%
  summarise(
    dif_sal = weighted.mean(dif_sal_adm_des_pc_m12, w = estoque_liquido, na.rm = TRUE),
    estoque = sum(estoque_liquido, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(Sector) %>%
  summarise(
    dif_sal_avg = mean(dif_sal),
    estoque_avg = mean(estoque),
    .groups = 'drop'
  ) %>%
  mutate(
    # Normalize to All Técnicos = 100
    scarcity_index_normalized = round(dif_sal_avg / baseline_dif_sal * 100)
  )

cat("\nScarcity by Sector:\n")
print(scarcity_by_sector)

# =============================================================================
# STEP 5: COMBINE TECH POTENTIAL AND SCARCITY INDEX
# =============================================================================

cat("\n=== Creating final PROPAG prioritization table ===\n")

# Remove total row from tech_potential for joining
tech_potential_sectors <- tech_potential_by_sector %>%
  filter(Sector != "TOTAL 5 SECTORS")

# Combine
propag_sector_priorities <- tech_potential_sectors %>%
  left_join(scarcity_by_sector %>% select(Sector, scarcity_index_normalized), by = "Sector") %>%
  mutate(
    tech_employment_potential_millions = round(tech_employment_potential / 1e6, 2)
  ) %>%
  select(
    Sector,
    Tech_Employment_Potential_M = tech_employment_potential_millions,
    Scarcity_Index = scarcity_index_normalized
  ) %>%
  arrange(desc(Scarcity_Index))

# Add total row
total_row <- data.frame(
  Sector = "TOTAL 5 SECTORS",
  Tech_Employment_Potential_M = sum(propag_sector_priorities$Tech_Employment_Potential_M),
  Scarcity_Index = NA
)

propag_sector_priorities <- rbind(propag_sector_priorities, total_row)

# =============================================================================
# STEP 6: PRINT RESULTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("       PROPAG SECTOR PRIORITIES - TECHNICAL EMPLOYMENT & SCARCITY              \n")
cat("       (Scarcity Index: All Técnicos = 100)                                    \n")
cat("================================================================================\n\n")

cat(sprintf("%-30s %25s %20s\n", "Sector", "Tech Employment Potential", "Scarcity Index"))
cat("--------------------------------------------------------------------------------\n")

for (i in 1:nrow(propag_sector_priorities)) {
  scarcity_str <- ifelse(is.na(propag_sector_priorities$Scarcity_Index[i]), 
                         "-", 
                         as.character(propag_sector_priorities$Scarcity_Index[i]))
  cat(sprintf("%-30s %20.2f M %20s\n",
              propag_sector_priorities$Sector[i],
              propag_sector_priorities$Tech_Employment_Potential_M[i],
              scarcity_str))
}
cat("================================================================================\n")

cat(sprintf("\nBaseline: All Técnicos salary differential = %.2f%%\n", baseline_dif_sal))
cat("Scarcity Index > 100 means higher scarcity than average technical occupation\n")

# =============================================================================
# STEP 7: SAVE RESULTS
# =============================================================================

cat("\n=== Saving results ===\n")

save(propag_sector_priorities, file = "D:/Country/Brazil/TechBrazil/working/rais/propag_sector_priorities.rda")
cat("✅ Saved: propag_sector_priorities.rda\n")

cat("\n=== Done ===\n")