# fundeb1a.R
#remotes::install_github("mellohenrique/simulador.fundeb2")
library(simulador.fundeb)
library(dplyr)

load("D:/Country/Brazil/TechBrazil/rawdata/fundeb/pesos.rda")
load("D:/Country/Brazil/TechBrazil/rawdata/fundeb/matriculas.rda")
load("D:/Country/Brazil/TechBrazil/rawdata/fundeb/complementar.rda")

data("matriculas")
data("complementar")
data("pesos")


# save pesos as csv
write.csv(pesos, "D:/Country/Brazil/TechBrazil/rawdata/fundeb/pesos.csv", row.names = FALSE)

df_teste = simulador.fundeb::simula_fundeb(
  dados_matriculas = matriculas,
  dados_complementar = complementar,
  dados_peso = pesos,
  complementacao_vaaf = 4e5,
  complementacao_vaat = 1e5,
  complementacao_vaar = 1e5)


matriculas_uf <- subset(matriculas, nchar(ibge) == 2)

# Step 2: Define full candidate column lists
all_cols_ept <- c(
  "ensino_medio_integrado_a_educacao_profissional_rede_publica",
  "itinerario_de_formacao_tecnica_e_profissional_rede_publica",
  "educacao_profissional_concomitante_ao_ensino_medio_rede_publica",
  
  "ens_medio_integrado_a_ed_profisional_rede_conveniada_de_formacao_por_alternancia",
  "eja_integrada_a_ed_profissional_de_nivel_medio_rede_conveniada_de_formacao_por_alternancia",
  "itinerario_de_formacao_tecnica_e_profissional_rede_conveniada_de_formacao_por_alternancia",
  "educacao_profissional_concomitante_ao_ensino_medio_rede_conveniada_de_formacao_por_alternancia",
  
  "ens_medio_integrado_a_ed_profissional_rede_conveniada_instituicoes_de_ed_profissional",
  "eja_integrada_a_ed_profissional_de_nivel_medio_rede_conveniada_instituicoes_de_ed_profissional",
  "itinerario_de_formacao_tecnica_e_profissional_rede_conveniada_instituicoes_de_ed_profissional",
  "educacao_profissional_concomitante_ao_ensino_medio_rede_conveniada_instituicoes_de_ed_profissional"
)

all_cols_ensino_medio <- c(
  "ensino_medio_urbano_rede_publica",
  "ensino_medio_rural_rede_publica",
  "ensino_medio_integral_rede_publica",
  "ensino_medio_integrado_a_educacao_profissional_rede_publica"
)

# Step 3: Keep only existing columns
cols_ept <- all_cols_ept[all_cols_ept %in% names(matriculas_uf)]
cols_ensino_medio <- all_cols_ensino_medio[all_cols_ensino_medio %in% names(matriculas_uf)]

# Step 4: Compute totals
matriculas_uf$total_ept_matriculas <- rowSums(matriculas_uf[, cols_ept, drop = FALSE], na.rm = TRUE)
matriculas_uf$total_ensino_medio <- rowSums(matriculas_uf[, cols_ensino_medio, drop = FALSE], na.rm = TRUE)

# Step 5: Compute percentage
matriculas_uf$porcentual_ept <- ifelse(
  matriculas_uf$total_ensino_medio > 0,
  100 * matriculas_uf$total_ept_matriculas / matriculas_uf$total_ensino_medio,
  NA_real_
)

# Step 6: Final result
df_ept_summary <- matriculas_uf[, c("ibge", "total_ept_matriculas", "total_ensino_medio", "porcentual_ept")]

uf_codes <- df_teste %>% select(uf,ibge,nome)

df_ept_summary <- df_ept_summary %>%
  left_join(uf_codes, by = "ibge") %>%
  select(ibge, uf, nome, total_ept_matriculas, total_ensino_medio, porcentual_ept)
