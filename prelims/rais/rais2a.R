# rais2a.R

load("D:/Country/Brazil/TechBrazil/rawdata/rais/working/rais/2024/rais2024.rda")
load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")
load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.rda")


library(dplyr)


# load("D:/Country/Brazil/TechBrazil/working/mintraemp/rais_caged_cbo4_mun_2023_2024.rda")
# dplyr::glimpse(rais_caged_cbo4_mun_2023_2024)
# 
# dfr23 <- rais_caged_cbo4_mun_2023_2024 %>% filter(ANO==2023)
# table(dfr23$MES)
# 
# junk <- dfr23 %>% filter(SG_UF=="TO") %>% select(MES, cbo1, total_vinculo_ativo_3112)
# junk2_1 <- junk %>% group_by(MES,cbo1) %>% 
#   summarise(total_vinculo_ativo_3112 = sum(total_vinculo_ativo_3112, na.rm = TRUE)) %>%
#   arrange(MES, desc(total_vinculo_ativo_3112)) 
# junk2_1
names(rais2024)

rais_cbo_uf24_ <- rais2024 %>% rename(CodCBO=`CBO Ocupação 2002`, vinculos =`Vínculo Ativo 31/12`, CO_MUN6=`Mun Trab`) %>%
  mutate(CodCBO=as.character(CodCBO)) 

temp_uf <- df_codes_ibge %>% select(CO_MUN6,CO_UF,SG_UF,NM_UF) %>% unique()
temp_cbo <- qbq_ocup_cmento1 %>% select(CodCBO, `Ocupação`,cbo_1dig,cbo_gragru,cbo_2dig, cbo_prigru,cbo_4dig,cbo_familia) %>% 
  distinct() 

rais_cbo6_uf24 <- rais_cbo_uf24_ %>% 
  left_join(temp_cbo, by = "CodCBO", relationship = "many-to-many") %>% 
  left_join(temp_uf, by = "CO_MUN6", relationship = "many-to-many") %>%
  select(CodCBO, vinculos, CO_UF, SG_UF, NM_UF,
         cbo_1dig, cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia, `Ocupação`) %>%
  filter(!is.na(CodCBO)) %>%
  group_by(CodCBO, CO_UF, SG_UF, NM_UF,cbo_1dig, cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia, `Ocupação`) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = 'drop') %>%
  arrange(CO_UF, desc(vinculos))
sum(rais_cbo6_uf24$vinculos, na.rm = TRUE)




# Step 1: Create the 4-digit CBO code
rais_cbo4_uf24_ <- rais_cbo_uf24_ 
rais_cbo4_uf24_$cbo_4dig <- substr(rais_cbo4_uf24_$CodCBO, 1, 4)
# Now we keep the feasible columns and compute the sum of vinculos by 4 digits without any joins
rais_cbo4_uf24_z <- rais_cbo4_uf24_ %>%
  select(cbo_4dig,vinculos,CO_MUN6) %>% 
  group_by(cbo_4dig, CO_MUN6) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = 'drop')
rais_cbo4_uf24_z <- rais_cbo4_uf24_z %>%
  left_join(temp_uf, by = "CO_MUN6", relationship = "many-to-many") %>%
  select(vinculos, CO_UF, SG_UF, NM_UF, cbo_4dig) %>%
# sum(is.na(rais_cbo4_uf24_z$cbo_4dig)) is 0
  group_by(CO_UF, SG_UF, NM_UF,cbo_4dig) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = 'drop') %>%
  arrange(CO_UF, desc(vinculos)) # 40554596 good
# sum(rais_cbo4_uf24_z$vinculos)

rais_cbo4_uf24 <- rais_cbo4_uf24_z %>% 
  left_join(temp_cbo, by = "cbo_4dig", relationship = "many-to-many") %>%
  select(vinculos, CO_UF, SG_UF, NM_UF,
         cbo_1dig, cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia) %>% unique()
sum(rais_cbo4_uf24$vinculos) # 40554596 good


# Lets save working/rais
save(rais_cbo4_uf24, file = "D:/Country/Brazil/TechBrazil/working/rais/rais_cbo4_uf24.rda")
save(rais_cbo6_uf24, file = "D:/Country/Brazil/TechBrazil/working/rais/rais_cbo6_uf24.rda")





