# explore_censo2.R
# Download and load Censo Escolar main microdata 2024

library(here)
library(tidyverse)
library(janitor)
library(aws.s3)
library(dotenv)
library(openxlsx)

# Comparing compositition of QT_MAT_MED - does it include EJA_MED?

junk <- df_censo_UF %>% select(SG_UF,NM_UF, ANO,QT_MAT_MED,
                               QT_MAT_MED_CT,QT_MAT_MED_NM,QT_MAT_MED_PROP,QT_MAT_EJA_MED) %>%
        filter(!is.na(QT_MAT_MED_CT))

junk$sum1diff <- junk$QT_MAT_MED -(junk$QT_MAT_MED_CT + junk$QT_MAT_MED_NM + junk$QT_MAT_MED_PROP)
junk$sum2diff <- junk$QT_MAT_MED -(junk$QT_MAT_MED_CT + junk$QT_MAT_MED_NM + junk$QT_MAT_MED_PROP + junk$QT_MAT_EJA_MED)


meta11a_opcao2 <- df_censo_UF %>% filter(AGREG=="UF_TUDO") %>% 
  select(ANO, NM_UF, SG_UF, QT_MAT_MED) %>%
  left_join(Tecnico_FormaR_Garabed1, by = c("ANO", "SG_UF")) %>%
  select(ANO, NM_UF, SG_UF, QT_MAT_MED, QT_MAT_CURSO_TEC_CT, QT_MAT_CURSO_TEC_CONC) %>%
  mutate(
    QT_MAT_TEC_NUM2 = ifelse(is.na(QT_MAT_CURSO_TEC_CT), 0, QT_MAT_CURSO_TEC_CT) +
      ifelse(is.na(QT_MAT_CURSO_TEC_CONC), 0, QT_MAT_CURSO_TEC_CONC),
    Meta11a_opcao2 = ifelse(QT_MAT_MED > 0, 
                            QT_MAT_TEC_NUM2 / QT_MAT_MED, 
                            NA)
  )

# Definiç