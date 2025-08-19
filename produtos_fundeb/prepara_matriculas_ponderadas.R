rm(list = ls())
gc()

# fundeb1a.R
options(scipen = 999)

library(simulador.fundeb)
library(tidyverse)
library(readxl)
library(here)

input_folder <- here("rawdata", "mec_inep") #preencher com o caminho no pro arquivo no seu computador
output_folder <- here("working", "mec_inep") #preencher com o caminho no pro arquivo no seu computador

#Importa base de matriculas agregadas no nível de município importadas do servidor da Base dos Dados
matriculas <- read_csv(here(input_folder, "matriculas_por_etapa_mun_20162024.csv")) 

#Conta de matrículas por tipos relevantes de etapa no nível ano-uf
matriculas %>%
  mutate(
    tipo_localizacao = if_else(tipo_localizacao == 1 , "urban", "rural")
  ) %>%
  select(-contains("integral"), - contains("tecnico")) %>% #Usando as categorias agregadas para facilitar (ou seja, sem discriminar os fatores de ponderação)
  group_by(ano, sigla_uf, id_municipio, tipo_localizacao) %>%
  summarise(across(.cols = contains("soma"),
                   .fns = ~ sum(.x, na.rm = TRUE))) %>%
  pivot_wider(id_cols = c(ano, sigla_uf, id_municipio),
              names_from = tipo_localizacao,
              values_from = contains("soma")) %>%
  ungroup -> matriculas_w

#Adiciona pesos de ponderação (pesos VAAF publicados pelo FNDE em 2024)
matriculas_w %>% #pesos VAAF 2024 divulgados pelo FNDE
  mutate(
    w_soma_creche_rural = soma_creche_rural*1.25,
    w_soma_creche_urbana = soma_creche_urban*1.25,
#    w_soma_creche_rural_integral = soma_creche_integral_rural*1.5,
#    w_soma_creche_urbana_integral = soma_creche_integral_urban*1.5,
    
    
    w_soma_pre_escola_rural = soma_pre_escola_rural*1.15,
#    w_soma_pre_escola_integral_rural = soma_creche_integral_rural*1.25,
    w_soma_pre_escola_urban = soma_pre_escola_rural*1.15,
#    w_soma_pre_escola_integral_urban = soma_creche_integral_rural*1.25,
    
    
    w_soma_ef_iniciais_rural = soma_ef_iniciais_rural*1.15,
    w_soma_ef_iniciais_urban = soma_ef_iniciais_urban,
    w_soma_ef_finais_rural = soma_ef_iniciais_rural*1.2,
    w_soma_ef_finais_urban = soma_ef_iniciais_urban*1.1,
#    w_soma_ef_integral_rural = soma_ef_integral_rural*1.4,
#    w_soma_ef_integral_urban = soma_ef_integral_urban*1.4,
    
    w_soma_em_rural = soma_em_rural*1.3,
    w_soma_em_urban = soma_em_urban*1.25,
#    w_soma_em_integral_rural = soma_em_integral_rural*1.4,
#    w_soma_em_integral_urban = soma_em_integral_urban*1.4,
    
#    w_soma_em_tecnico_rural = soma_em_tecnico_rural*1.3,
#    w_soma_em_tecnico_urban = soma_em_tecnico_urban*1.3,
    
    w_soma_profissional_rural = soma_profissional_rural*1.3,
    w_soma_profissional_urban = soma_profissional_urban*1.3,
    
    w_soma_eja_rural = soma_eja_rural*1.2,
    w_soma_eja_urban = soma_eja_urban*1.2,
    
    w_soma_especial_rural = soma_especial_rural*1.4,
    w_soma_especial_urban = soma_especial_urban*1.4
  ) %>%
  group_by(ano, sigla_uf, id_municipio) %>%
  summarise(
    across(.cols = contains("soma"),
           .fns = ~ sum(.x, na.rm = TRUE))
  ) %>%
  ungroup() -> matriculas_mun_ano

#Agrega entre as colunas das diferentes etapas para grupos de EPT e EM
matriculas_mun_ano %>%
  mutate(across(starts_with("soma"), ~replace_na(., 0))) %>%
  group_by(ano, sigla_uf, id_municipio) %>%
  transmute(
    matriculas_total = rowSums(across(starts_with("soma_creche"))) + rowSums(across(starts_with("soma_pre_escola"))) 
    + rowSums(across(starts_with("soma_ef"))) + rowSums(across(starts_with("soma_em"))) + rowSums(across(starts_with("soma_profissional"))) +
      rowSums(across(starts_with("soma_eja"))),
    
    matriculas_total_pond = rowSums(across(starts_with("w_soma_creche"))) + rowSums(across(starts_with("w_soma_pre_escola"))) 
    + rowSums(across(starts_with("w_soma_ef"))) + rowSums(across(starts_with("w_soma_em"))) + rowSums(across(starts_with("w_soma_profissional"))) +
      rowSums(across(starts_with("w_soma_eja"))),
    
    matriculas_total_tecprof = rowSums(across(matches("^soma_profissional_"))),
    matriculas_total_tecprof_pond = rowSums(across(matches("^w_soma_profissional_"))),
    
    matriculas_total_em = rowSums(across(matches("^soma_em"))),
    matriculas_total_em_pond = rowSums(across(matches("^w_soma_em"))),
    
    share_matriculas_ept = matriculas_total_tecprof/matriculas_total,
    share_matriculas_em = matriculas_total_em/matriculas_total,
    
    share_matriculas_ept_pond = matriculas_total_tecprof_pond/matriculas_total_pond,
    share_matriculas_em_pond = matriculas_total_em_pond/matriculas_total_pond,
  ) %>%
  ungroup %>%
  arrange(id_municipio) -> matriculas_final

#Agrega no nível de UF e gera CSV para construir as estimativas de fluxo do FUNDEB
matriculas_final %>%
  group_by(ano, sigla_uf) %>%
  summarise(
    across(.cols = contains("matriculas"),
           .fns = ~ sum(.x, na.rm = TRUE))
  ) -> matriculas_final_uf

write_csv(matriculas_final_uf
          , here(ouput_folder, "matriculas_total_em_ept_uf_20162024.csv"))

#Merge with IBGE intermediate and imediate regions
regioes_ibge <- read_xlsx(here("rawdata", "ibge", "regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx")) %>%
  rename(id_municipio = CD_GEOCODI) %>%
  select(id_municipio, nome_rgi, nome_rgint) %>%
  mutate(id_municipio = as.numeric(id_municipio))

left_join(matriculas_final, regioes_ibge, by = "id_municipio") %>% 
  relocate(nome_rgi, nome_rgint, .before = id_municipio) -> matriculas_final

write_csv(matriculas_final, here(output_folder,"matriculas_total_em_ept_mun_20162024.csv"))
