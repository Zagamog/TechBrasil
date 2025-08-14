# cod_cbo1a.R
# Match COD to CBO

library(dplyr)
library(reticulate)

cod <- openxlsx::read.xlsx("D:/Country/Brazil/TechBrazil/rawdata/ibge/cod.xlsx")


cod$COD_ALL <- as.character(cod$COD_ALL)

# Create substrings of length 1 to 4, but only when string is long enough
cod$COD_1 <- substr(cod$COD_ALL, 1, 1)
cod$COD_2 <- ifelse(nchar(cod$COD_ALL) >= 2, substr(cod$COD_ALL, 1, 2), NA)
cod$COD_3 <- ifelse(nchar(cod$COD_ALL) >= 3, substr(cod$COD_ALL, 1, 3), NA)
cod$COD_4 <- ifelse(nchar(cod$COD_ALL) >= 4, substr(cod$COD_ALL, 1, 4), NA)

df_cod1 <- cod[nchar(cod$COD_ALL) == 1, c("COD_1", "COD_NOME")]
df_cod2 <- cod[nchar(cod$COD_ALL) == 2, c("COD_2", "COD_NOME")]
df_cod3 <- cod[nchar(cod$COD_ALL) == 3, c("COD_3", "COD_NOME")]
df_cod4 <- cod[nchar(cod$COD_ALL) == 4, c("COD_4", "COD_NOME")]



load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.rda")
names(qbq_ocup_cmento1) 

# [1] "CodCBO"            "Ocupação"          "Síntese"           "PerfilOcupacional"
# [5] "NivelOcupacao"     "cbo_nome"          "cbo_4dig"          "cbo_familia"      
# [9] "cbo_3dig"          "cbo_subgru"        "cbo_2dig"          "cbo_prigru"       
# [13] "cbo_1dig"          "cbo_gragru"       


# Create qbq1 with capitalization and filter for NAs

df_qbq1 <- unique(qbq_ocup_cmento1[, c("cbo_1dig", "cbo_gragru")]) %>% 
  filter(!is.na(cbo_1dig)) %>%
  mutate(cbo_gragru = toupper(cbo_gragru))

# My invented gragru based on Informaçoes gerais CBO 6.0.6 Page 15 at mtecbo 
# df_qbq1$cbo_gragru[df_qbq1$cbo_1dig == "8"] <- "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS CONTINUOS"
# No need anymore as I amended the base df in qbq3a.R


df_qbq2 <- unique(qbq_ocup_cmento1[, c("cbo_2dig", "cbo_prigru")]) %>% 
  filter(!is.na(cbo_2dig)) %>%
  mutate(cbo_prigru = toupper(cbo_prigru))

df_qbq3 <- unique(qbq_ocup_cmento1[, c("cbo_3dig", "cbo_subgru")]) %>% 
  filter(!is.na(cbo_3dig)) %>%
  mutate(cbo_subgru = toupper(cbo_subgru))

df_qbq4 <- unique(qbq_ocup_cmento1[, c("cbo_4dig", "cbo_familia")]) %>% 
  filter(!is.na(cbo_4dig)) %>%
  mutate(cbo_familia = toupper(cbo_familia))




# Save as pickle file
pd <- import("pandas")

# Convert R dataframes to Pandas
py$df_qbq1 <- r_to_py(df_qbq1)
py$df_cod1 <- r_to_py(df_cod1)

py$df_qbq2 <- r_to_py(df_qbq2)
py$df_cod2 <- r_to_py(df_cod2)

py$df_qbq3 <- r_to_py(df_qbq3)
py$df_cod3 <- r_to_py(df_cod3)

py$df_qbq4 <- r_to_py(df_qbq4)
py$df_cod4 <- r_to_py(df_cod4)


# Save as pickle
py_save_object(df_qbq1,"D:/Country/Brazil/TechBrazil/working/ibge/df_qbq1.pkl")
py_save_object(df_cod1, "D:/Country/Brazil/TechBrazil/working/ibge/df_cod1.pkl")

py_save_object(df_qbq2,"D:/Country/Brazil/TechBrazil/working/ibge/df_qbq2.pkl")
py_save_object(df_cod2, "D:/Country/Brazil/TechBrazil/working/ibge/df_cod2.pkl")

py_save_object(df_qbq3,"D:/Country/Brazil/TechBrazil/working/ibge/df_qbq3.pkl")
py_save_object(df_cod3, "D:/Country/Brazil/TechBrazil/working/ibge/df_cod3.pkl")

py_save_object(df_qbq4,"D:/Country/Brazil/TechBrazil/working/ibge/df_qbq4.pkl")
py_save_object(df_cod4, "D:/Country/Brazil/TechBrazil/working/ibge/df_cod4.pkl")








