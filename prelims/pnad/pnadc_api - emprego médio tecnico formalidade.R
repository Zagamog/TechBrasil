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
          "VD3004", "VD4001", "VD4002", "VD4003", "VD4004A", "VD4009", "VD4011", "VD4012", "VD4013", "VD4019","VD4020", "VD4032", #variáveis derivadas educaçao e situacao mercado de trabalho
          "V3019A", "V3020B", "V3020C", "V3021A", "V3022C", "V3022D", "V3022E","V3023A", "V4019", "V4010",
          "V3026", "V3026A", "V3029", "V3029A", "V3032")  # variáveis de ensino técnico e profissional do suplemento educaçao (concentradas no segundo trimestre do ano)


labelled <- function(x, label) {
  attr(x, "label") <- label
  x
}

# Function to download and load PNADC microdata for a given year and quarter from IBGE API
get_pnadc_trimester <- function(year) {
  get_pnadc(
    year = year,
    topic = 2,
    design = FALSE,
    vars = vars,
    deflator = FALSE,
    labels = TRUE
  ) %>%
    mutate(
      Ano    = labelled(Ano, "Ano da entrevista"),
      UF     = labelled(UF, "Código da Unidade da Federação"),
      V1028  = labelled(V1028, "Peso da pessoa"),
      V2009  = labelled(V2009, "Idade"),
      V3023A = labelled(V3023A, "Concluiu curso técnico"),
      V3032  = labelled(V3032, "Concluiu curso profissional"),
      
      VD3004 = labelled(VD3004, "Nível de instrução"),
      VD4001 = labelled(VD4001, "Na força de trabalho"),
      VD4002 = labelled(VD4002, "Ocupado"),
      VD4009 = labelled(VD4009, "Tipo de vínculo"),
      VD4011 = labelled(VD4011, "CBO 1 dígito"),
      VD4013 = labelled(VD4013, "Contribui para previdência"),
      V4019  = labelled(V4019, "Empregador com CNPJ"),
      
      emprego_formal = case_when(
        VD4009 %in% c("Empregado no setor privado com carteira de trabalho assinada",  # Empregado com carteira assinada, público ou doméstico; militares e estatutários
                      "Trabalhador doméstico com carteira de trabalho assinada", 
                      "Empregado no setor público com carteira de trabalho assinada", 
                      "Militar e servidor estatutário") ~ 1,                      
        VD4009 == "Empregador" & V4019 == 1 ~ 1,  # Empregador com CNPJ                               
        VD4009 == "Conta-própria" & VD4013 == 1 ~ 1, # Conta própria que contribui para previdência                               
        TRUE ~ 0
      ),
      
      VD4001 = if_else(VD4001 == "Pessoas na força de trabalho", 1, 0),
      VD4002 = if_else(VD4002 == "Pessoas ocupadas", 1, 0),
      
      V3023A = case_when(V3023A == "Sim" ~ 1, 
                         V3023A == "Não" ~ 0,
                         TRUE ~ NA_real_),
      v3032 = case_when(V3032 == "Sim" ~ 1,
                      V3023A == "Não" ~ 0,
                      TRUE ~ NA_real_),
      
      concluiu_curso_tec_prof = case_when(
        V3023A == 1 | V3032 == 1 ~ 1,
        is.na(V3023A) & is.na(V3032) ~ 0,
        TRUE ~ 0
      )
    ) %>%
    filter(VD3004 != "Superior completo", concluiu_curso_tec_prof == 1) %>% #exclui indivíduos com ensino superior completo e mantem apenas que concluiram curso EPT
   group_by(Ano, UF, V4010) %>%
    summarise(
 #     Total = sum(V1028, na.rm = TRUE),
#      PEA = sum(VD4001*V1028, na.rm = TRUE),
      Ocupado = sum(VD4002 * V1028, na.rm = TRUE),
      `Ocupado Formal` = sum(VD4002 * emprego_formal * V1028, na.rm = TRUE),
      `Ocupado Informal` = Ocupado - `Ocupado Formal`,
      `Taxa Formalidade` = `Ocupado Formal`/Ocupado
    ) %>%
    ungroup()
}


# Download and load 2nd trimester data for 2023 and 2024 (separate to avoid errors)
pnadc_1 <- map_dfr(.x = c(2016:2019), get_pnadc_trimester)
pnadc_2 <- map_dfr(.x = c(2022:2024), get_pnadc_trimester)

pnadc <- bind_rows(pnadc_1, pnadc_2) %>%
  mutate(trimestre = "abril-maio-junho") %>%
  filter(!is.na(V4010)) %>%
  relocate(trimestre, .after = Ano) %>%
  rename(ANO = Ano,
         NM_UF = UF)

write_csv(pnadc, here("working", "ibge","pnadc_20162024_uf_formalidade_ocup_ept_trim2.csv"))
