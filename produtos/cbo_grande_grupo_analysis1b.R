# cbo_grande_grupo_analysis1b.R
#
# Analysis of all Brazil formal employment by CBO Grande Grupo
# Shows occupational composition for entire economy

library(dplyr)

# =============================================================================
# STEP 1: LOAD DATA
# =============================================================================

cat("=== Loading data ===\n")

load("D:/Country/Brazil/TechBrazil/working/rais/2023/rais2023.rda")
load("D:/Country/Brazil/TechBrazil/working/rais/2024/rais2024.rda")

cat(sprintf("RAIS 2023 rows: %s\n", format(nrow(rais2023), big.mark = ",")))
cat(sprintf("RAIS 2024 rows: %s\n", format(nrow(rais2024), big.mark = ",")))

# =============================================================================
# STEP 2: STANDARDIZE COLUMN NAMES
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
# STEP 3: CALCULATE BY GRANDE GRUPO
# =============================================================================

cat("\n=== Calculating totals by Grande Grupo ===\n")

# 2023
gg_2023 <- rais2023 %>%
  filter(vinculos == 1) %>%
  mutate(GG = substr(CodCBO, 1, 1)) %>%
  group_by(GG) %>%
  summarise(Total_2023 = n(), .groups = 'drop')

# 2024
gg_2024 <- rais2024 %>%
  filter(vinculos == 1) %>%
  mutate(GG = substr(CodCBO, 1, 1)) %>%
  group_by(GG) %>%
  summarise(Total_2024 = n(), .groups = 'drop')

# =============================================================================
# STEP 4: COMBINE RESULTS
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

cbo_gg_all_economy <- gg_descriptions %>%
  left_join(gg_2023, by = "GG") %>%
  left_join(gg_2024, by = "GG") %>%
  mutate(
    Total_2023 = ifelse(is.na(Total_2023), 0, Total_2023),
    Total_2024 = ifelse(is.na(Total_2024), 0, Total_2024)
  )

# Add totals row
total_row <- data.frame(
  GG = "",
  Descricao = "TOTAL ALL ECONOMY",
  Total_2023 = sum(cbo_gg_all_economy$Total_2023),
  Total_2024 = sum(cbo_gg_all_economy$Total_2024)
)

cbo_gg_all_economy <- rbind(cbo_gg_all_economy, total_row)

# =============================================================================
# STEP 5: PRINT RESULTS
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("       ALL ECONOMY - EMPLOYMENT BY CBO GRANDE GRUPO (2023 vs 2024)             \n")
cat("================================================================================\n\n")

cat(sprintf("%-50s %15s %15s\n", "Grande Grupo", "2023", "2024"))
cat("--------------------------------------------------------------------------------\n")

for (i in 1:nrow(cbo_gg_all_economy)) {
  cat(sprintf("%-50s %15s %15s\n",
              cbo_gg_all_economy$Descricao[i],
              format(cbo_gg_all_economy$Total_2023[i], big.mark = ","),
              format(cbo_gg_all_economy$Total_2024[i], big.mark = ",")))
}
cat("================================================================================\n")

# =============================================================================
# STEP 6: SAVE RESULTS
# =============================================================================

cat("\n=== Saving results ===\n")

save(cbo_gg_all_economy, file = "D:/Country/Brazil/TechBrazil/working/rais/cbo_gg_all_economy.rda")
cat("✅ Saved: cbo_gg_all_economy.rda\n")

cat("\n=== Done ===\n")