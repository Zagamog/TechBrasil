# Install and load required packages
if (!requireNamespace("PNADcIBGE", quietly = TRUE)) {
  install.packages("PNADcIBGE")
}
library(PNADcIBGE)
library(tidyverse)
library(here)

# Increase timeout to 30 minutes
options(timeout = 3600)

vars <- c("V2009", #variáveis básicas
          "VD3004", "VD4001", "VD4002", "VD4003", "VD4004A", "VD4009", "VD4012","VD4020", "VD4032", #variáveis derivadas educaçao e situacao mercado de trabalho
          "V3019A", "V3020B", "V3020C", "V3021A", "V3022C", "V3022D", "V3022E","V3023A",
          "V3026", "V3026A", "V3029", "V3029A", "V3032")  # variáveis de ensino técnico e profissional do suplemento educaçao (concentradas no segundo trimestre do ano)

dummy <- function(var){ # função para criar variáveis dummies
  case_when(var == 1 ~ 1,
            var == 2 ~ 0,
            TRUE ~ NA_real_)
}
# Function to download and load PNADC microdata for a given year and quarter from IBGE API
get_pnadc_trimester <- function(year) {
  get_pnadc(
    year = year,
    topic = 2,
    design = FALSE, #no standard error computation for now
    vars = vars,       # NULL for all variables, or specify c("UF", "VD4001", etc.)
    deflator = TRUE,
    labels = TRUE
  ) %>%
    select(Ano, UF, V1028, V2009:VD4032, Efetivo,ID_DOMICILIO, - S12001A) %>%
    rename(
      ano = Ano,
      id_uf = UF,
      peso = V1028,
      
      idade = V2009,
      
      frequenta_curso_tecnico_medio = V3019A,
      tipo_curso_tecnico_frequenta = V3020B,
      tipo_instituicao_curso_tecnico_frequenta = V3020C,
      
      frequentou_curso_tecnico_medio = V3021A,
      tipo_curso_tecnico_frequentou = V3022C, 
      tipo_instituicao_curso_tecnico_frequentou = V3022D,
      ano_iniciou_curso_tecnico_frequentou = V3022E,
      concluiu_curso_tecnico = V3023A,
      
      frequenta_curso_prof = V3026,
      tipo_instituicao_curso_prof_frequenta = V3026A,
      
      frequentou_curso_prof = V3029,
      ano_iniciou_curso_prof_frequentou = V3029A,
      concluiu_curso_prof = V3032,
      
      
      nivel_edu = VD3004,
      forca_trab = VD4001,
      ocupado = VD4002,
      forca_trab_potencial = VD4003,
      subocupado = VD4004A,
      tipo_emprego = VD4009,
      contribui_previdencia = VD4012,
      renda_efetiva = VD4020,
      horas_trab_efetiva = VD4032) %>%
    mutate(
      across(.cols = c(frequenta_curso_tecnico_medio, frequenta_curso_prof ,
                       frequentou_curso_tecnico_medio, frequentou_curso_prof,
                       concluiu_curso_tecnico, concluiu_curso_prof,
                       
                       forca_trab, forca_trab_potencial, ocupado),
             .fns = ~ dummy(as.integer(.x))),
      
      concluiu_curso_tec_prof = case_when(concluiu_curso_tecnico == 1 | concluiu_curso_prof == 1 ~ 1,
                                          is.na(concluiu_curso_tecnico) & is.na(concluiu_curso_prof) ~ 0,
                                          TRUE ~ 0),
      
#      empregado_formal = case_when(tipo_emprego == "Empregado no setor privado com carteira de trabalho assinada" ~ 1,
#                                   tipo_emprego == "Trabalhador doméstico com carteira de trabalho assinada" ~ 1,
#                                   tipo_emprego == "Empregado no setor público com carteira de trabalho assinada" ~ 1,
#                                   tipo_emprego == "Militar e servidor estatutário" ~ 1,
#                                   TRUE ~ 0),
      #poderia criar uma categória de formal incluindo também empregador e conta propria que contribui para previdência
#      empregador_ou_contapropria = case_when(tipo_emprego == "Empregador" ~ 1,
#                                             tipo_emprego == "Conta-própria" ~ 1
#                                             TRUE ~ 0)
    ) %>%
    group_by(ano, id_uf, idade, concluiu_curso_tec_prof) %>%
    summarise(
      Total = sum(peso, na.rm = TRUE),
      PEA = sum(forca_trab*peso, na.rm = TRUE),
      Ocupado = sum(ocupado*peso, na.rm = TRUE)
    ) %>%
    ungroup
    
}


# Download and load 2nd trimester data for 2023 and 2024 (separate to avoid errors)
pnadc_1 <- map_dfr(.x = c(2016:2019), get_pnadc_trimester)
pnadc_2 <- map_dfr(.x = c(2022:2024), get_pnadc_trimester)

pnadc <- bind_rows(pnadc_1, pnadc_2) %>%
  mutate(trimestre = "abril-maio-junho") %>%
  relocate(trimestre, .after = ano)

write_csv(pnadc, here("working", "ibge","pnadc_20162024_uf_idade_ept_trim2.csv"))