# pnad_ocups1b.R

library(tidyverse)


# — 0) CBO lookup —
cbo4 <- read_csv(
  "D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result4.csv",
  col_types = cols(
    COD_4       = col_character(),
    COD_NOME    = col_character(),
    cbo_4dig    = col_character(),
    cbo_familia = col_character()
  )
)

# — 1) Load raw PNAD-C RDSs into one data frame —
load_pnadc_raw <- function(
    raw_dir = "D:/Country/Brazil/TechBrazil/rawdata/pnad"
) {
  files <- list.files(raw_dir, pattern = "\\.rds$", full.names = TRUE)
  map_dfr(files, readRDS)
}


raw  <- load_pnadc_raw("D:/Country/Brazil/TechBrazil/rawdata/pnad")

# 2–6) pipeline
raw2 <- raw  %>% mutate(V1028 = as.numeric(V1028), VD4002 = as.numeric(VD4002))


raw3 <- raw2 %>%
  mutate(V4010 = as.character(V4010)) %>%   # make sure it’s character
  left_join(cbo4, by = c("V4010" = "COD_4")) %>%
  filter(!is.na(cbo_4dig))


raw4 <- raw3 %>% mutate(
  concluiu_curso_tec_prof = case_when(V3023A == "Sim" | V3032 == "Sim" ~ 1, TRUE ~ 0),
  emprego_formal          = case_when(
    VD4009 %in% c("Empregado no setor privado com carteira de trabalho assinada",
                  "Trabalhador doméstico com carteira de trabalho assinada",
                  "Empregado no setor público com carteira de trabalho assinada",
                  "Militar e servidor estatutário") ~ 1,
    VD4009 == "Empregador" & V4019 == 1                   ~ 1,
    VD4009 == "Conta-própria" & VD4013 == 1               ~ 1,
    TRUE                                                  ~ 0
  )
)

raw5 <- raw4 %>% filter(VD3004 != "Superior completo", concluiu_curso_tec_prof == 1)

df_pnadc_occ <- raw5 %>% 
  group_by(Ano, UF, cbo_4dig) %>%
  summarise(
    Ocupado          = sum(VD4002 * V1028, na.rm = TRUE),
    Ocupado_Formal   = sum(VD4002 * emprego_formal * V1028, na.rm = TRUE),
    Ocupado_Informal = Ocupado - Ocupado_Formal,
    Taxa_Formalidade = if_else(Ocupado>0, Ocupado_Formal / Ocupado, NA_real_),
    .groups = "drop"
  ) %>%
  rename(ANO = Ano, NM_UF = UF, CBO4 = cbo_4dig)

df_pnadc_occ <- df_pnadc_occ %>%
  mutate(across(c(Ocupado, Ocupado_Formal, Ocupado_Informal), round),
         Taxa_Formalidade = round(Taxa_Formalidade, 2))

df_pnadc_occ <- df_pnadc_occ %>%
  mutate(
    Projection_Rate = case_when(
      Ocupado_Informal == 0            ~ 0,            # no informal → rate = 0
      Ocupado_Formal    == 0            ~ NA_real_,     # no formal → undefined
      TRUE                              ~ Ocupado_Informal / Ocupado_Formal
    ),
    Projection_Rate = round(Projection_Rate, 2)         # round to 2 decimals
  )




save(df_pnadc_occ, file="D:/Country/Brazil/TechBrazil/working/pnad/df_pnadc_occ.rda")

