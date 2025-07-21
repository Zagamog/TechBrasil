# rais_download_finaljul25a.R

library(dplyr)
library(data.table)



load("D:/Country/Brazil/TechBrazil/working/rais/df_NICOMJNE.rda")
load("D:/Country/Brazil/TechBrazil/working/rais/df_NOSPSU.rda")


# I get a dataframe from df_NICOMJNE and df_NOSPSU at level of UF

df_NICOMJNE_UF <- df_NICOMJNE %>%
  group_by(CO_UF,`CBO Ocupação 2002`) %>%
  summarise(Vinculos_Ativos =sum(`Vínculo Ativo 31/12`, na.rm = TRUE),
            Salario_Medio = mean(`Vl Remun Média Nom`, na.rm = TRUE),
            .groups = "drop") %>%
    ungroup()





names(df_NICOMJNE)
