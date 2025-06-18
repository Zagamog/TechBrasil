rm(list = ls())
gc()
options(scipen = 999)

library(tidyverse) #load tidyverse packages for data manipulation and visualization
library(readxl) # read xlsx files
library(here) #package to simplify directory definition and make the code more easily reproduceble.
library(zoo) #to compute moving averages
library(scales)
library(dlookr)

input_folder <- here("rawdata") 

####STATE-LEVEL ANALYSIS#######
#Importar regioes gegráficas IBGE
regioes_ibge <- read_xls(here(input_folder, "regioes_geograficas_composicao_ibge.xls")) %>%
  select(id_municipio, cod_rgint, nome_rgint) %>%
  mutate(id_municipio = as.numeric(id_municipio))

#Importa CAGED
caged <- read_csv(here(input_folder, "caged_cbo_mun_2023_2024.csv")) %>%
  left_join(regioes_ibge, by = "id_municipio") %>%
  relocate(cod_rgint, nome_rgint, .before = id_municipio) %>%
  mutate(micro_regiao = str_sub(id_municipio, 1, 5),
         
         cbo1 = str_sub(cbo_2002, 1, 1),
         cbo2 = str_sub(cbo_2002, 1, 2),
         cbo3 = str_sub(cbo_2002, 1, 3)) %>%
  filter(cbo1 == 3) %>%
  group_by(ano, mes, sigla_uf, nome_rgint, cbo2, saldo_movimentacao) %>%
  summarise(
    media_salario = weighted.mean(media_salario_ponderada, w = n_movimentacoes, na.rm = TRUE), # Usa variável ajustada pelas horas semanais de trabalho do contrato
    n_movimentacoes = sum(n_movimentacoes_ponderada, na.rm = TRUE)
  ) %>%
  ungroup 
gc()

#Importa RAIS
rais <- read_csv(here(input_folder, "rais_cbo_mun_2022_2023.csv")) %>%
  left_join(regioes_ibge, by = "id_municipio") %>%
  relocate(cod_rgint, nome_rgint, .before = id_municipio) %>%
  mutate(ano = ano + 1, # ajusta variável de ano porque vamos usar o estoque em dezembro do ano anterior como denominador para a taxa de rotatividade no ano seguinte
         micro_regiao = str_sub(id_municipio, 1, 5),
         
         cbo1 = str_sub(cbo_2002, 1, 1),
         cbo2 = str_sub(cbo_2002, 1, 2),
         cbo3 = str_sub(cbo_2002, 1, 3)) %>%
  filter(cbo1 == 3) %>%
  group_by(ano, sigla_uf, nome_rgint, cbo2) %>%
  summarise(
    total_vinculo_ativo_3112 = sum(total_vinculo_ativo_3112_ponderado, na.rm = TRUE), # Usa variável ajustada pelas horas semanais de trabalho do contrato
  ) %>%
  ungroup %>%
  relocate(cbo2, .before = total_vinculo_ativo_3112)


#Agrega CAGED no nível de CBO3 com colunas para separadas para admitidos e desligados
caged %>% #compute year-month means
  mutate(adm = if_else(saldo_movimentacao == 1, 1, NA_real_),
         des = if_else(saldo_movimentacao == - 1, 1, NA_real_)) %>%
  group_by(ano, mes, sigla_uf, nome_rgint, cbo2) %>%
  summarise(
    across(
      .cols = c(adm, des),
      .fns = list(media_sal = ~ weighted.mean(media_salario*.x, w = n_movimentacoes, na.rm = TRUE),
                  soma_mov = ~ sum(.x*n_movimentacoes, na.rm = TRUE)),
      .names = "{.fn}_{.col}"),
    
    dif_sal_adm_des = media_sal_adm - media_sal_des,
    dif_sal_adm_des_pc = (media_sal_adm - media_sal_des)*100/media_sal_des,
    rotatividade = soma_mov_adm - soma_mov_des
  ) -> caged

#Junta CAGED, RAIS
window_mean <- function(vec, lag = 6, lead = 5) {
  n <- length(vec)
  sapply(seq_along(vec), function(i) {
    window <- vec[max(1, i - lag):min(n, i + lead)]
    mean(window, na.rm = TRUE)
  })
}

caged_rais <- left_join(caged, rais, by = c("ano", "sigla_uf", "nome_rgint", "cbo2")) %>%
  mutate(tx_rotatividade = rotatividade/total_vinculo_ativo_3112) %>%
  arrange(ano, mes) %>%
  group_by(nome_rgint, cbo2) %>% 
  mutate(
    across(.cols = c(dif_sal_adm_des, dif_sal_adm_des_pc, tx_rotatividade),
           .fns = ~ if_else(.x == Inf, NA, .x)),
    
    across(.cols = c(dif_sal_adm_des, dif_sal_adm_des_pc, tx_rotatividade),
           
           .fns = ~ window_mean(.x),
           
           #.fns = ~ (lag(.x, 6) + lag(.x, 5) + lag(.x, 4) + lag(.x, 3) + lag(.x, 2)
           #+ lag(.x, 1) + .x + lead(.x, 1) + lead(.x, 2) + lead(.x, 3) + lead(.x, 4) + lead(.x, 5)),
           
    #       .fns =  ~ rollapply(data = .x, width = 12,
    #                           FUN = function(x) mean(x, na.rm = TRUE),
    #                           fill = NA, align = "center"),
           .names = "{.col}_m12")
  ) %>%
  ungroup

diagnostic_cagedrais <- dlookr::diagnose(caged_rais)
descstats_cegedrais <- dlookr::describe(caged_rais)

write_csv(caged_rais, here("working", "rais_caged_cbo2_rgint_2023_2024.csv"))


