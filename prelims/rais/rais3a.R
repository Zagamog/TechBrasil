
# rais3a.R
load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/caged_rais_cnct_2020_2024_shiny.rda")


# Get unique CBOs by Área Tecnológica
cbo_area <- inner_join(eixo_area, cnct_cbo, by = "Cod_Curso") %>%
  select(Area_Tecnologica, Co_Ocupacao) %>%
  distinct()

# Define 5 sectors
sector_areas <- list(
  "Health Care" = c("Gestão e Promoção da Saúde e Bem-Estar"),
  "Tourism" = c("Acolhimento e Hospedagem", "Serviços de Gastronomia", "Atividades Turísticas", 
                "Recreação e Sociabilidade", "Apoio Técnico a Eventos"),
  "Agribusiness" = c("Produção Agrícola e Pecuária", "Silvicultura", "Pesca e Aquicultura", 
                     "Produção Alimentícia"),
  "Energy & Infrastructure" = c("Sistemas de Energia", "Construção de Obras", 
                                "Mensuração Espacial e Volumétrica", "Operações de Transporte",
                                "Infraestrutura de Informação e Comunicação"),
  "Manufacturing" = c("Manufatura", "Materiais", "Química", "Têxtil e Vestuário", 
                      "Metalmecânica", "Eletrônica e Automação", "Manutenção e Operação")
)

# Add sector to CBO mapping
cbo_area$Sector <- NA
for (s in names(sector_areas)) {
  cbo_area$Sector[cbo_area$Area_Tecnologica %in% sector_areas[[s]]] <- s
}

# Keep only CBOs in our 5 sectors
cbo_sector <- cbo_area %>% filter(!is.na(Sector)) %>% select(Co_Ocupacao, Sector) %>% distinct()

# Now join to rais2024
rais2024$CBO6 <- as.character(rais2024$`CBO 2002 Ocupação - Código`)

# Calculate totals
rais_sector <- rais2024 %>%
  inner_join(cbo_sector, by = c("CBO6" = "Co_Ocupacao")) %>%
  filter(`Ind Vínculo Ativo 31/12 - Código` == 1)

# TOTAL (all education levels)
total_by_sector <- rais_sector %>%
  group_by(Sector) %>%
  summarise(Total_All = n())

# TECH MEDIO (Escolaridade <= 8)
tech_medio_by_sector <- rais_sector %>%
  filter(`Escolaridade Após 2005 - Código` <= 8) %>%
  group_by(Sector) %>%
  summarise(Tech_Medio = n())

# Combine
result <- left_join(total_by_sector, tech_medio_by_sector, by = "Sector")
result$Pct_Tech_Medio <- round(result$Tech_Medio / result$Total_All * 100, 1)
print(result)
