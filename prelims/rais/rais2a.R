# rais2a.R

load("D:/Country/Brazil/TechBrazil/working/rais/2024/rais2024.rda")
load("D:/Country/Brazil/TechBrazil/working/rais/2023/rais2023.rda")
load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")
load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.rda")

library(tidyverse)
library(dplyr)
library(janitor)


# rais2024 <- rais2024 %>% rename(CO_MUN6=`Mun Trab`)
# save(rais2024, file = "D:/Country/Brazil/TechBrazil/working/rais/2024/rais2024.rda")

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

rais_cbo_uf24_ <- rais2024 %>% rename(CodCBO=`CBO Ocupação 2002`, vinculos =`Vínculo Ativo 31/12`) %>%
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
# 40,554,596



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
  arrange(CO_UF, desc(vinculos)) 
sum(rais_cbo4_uf24_z$vinculos) # 40,554,596 good

rais_cbo4_uf24 <- rais_cbo4_uf24_z %>% 
  left_join(temp_cbo, by = "cbo_4dig", relationship = "many-to-many") %>%
  select(vinculos, CO_UF, SG_UF, NM_UF,
         cbo_1dig, cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia) %>% unique()
sum(rais_cbo4_uf24$vinculos) # 40554596 good

load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup.rda")
# Lets try to fill in Ocupação missings
sum(is.na(rais_cbo6_uf24$`Ocupação`)) # 14,526
## Try to fill missing ocupação codes
##################################################
##################################################
##################################################

# Bring in some vars from cbo now
# Caminho base dos arquivos
base <- "D:/Country/Brazil/TechBrazil/rawdata/cbo/"

# --- Carregar arquivos CSV ---

# Grande Grupo (1 dígito)
df_gg <- read_delim(paste0(base, "CBO2002 - Grande Grupo.csv"), delim = ";", locale = locale(encoding = "LATIN1")) %>%
  clean_names() %>%
  rename(cbo_1dig = codigo, cbo_gragru = titulo) %>%
  mutate(cbo_1dig = str_pad(as.character(cbo_1dig), 1, pad = "0"))
# My invented gragru based on Informaçoes gerais CBO 6.0.6 Page 15 at mtecbo 
df_gg$cbo_gragru[df_gg$cbo_1dig == "8"] <- "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS CONTINUOS"
df_gg %>% select(cbo_gragru) %>% n_distinct() # 10 distinct values with addition from SP


# Subgrupo Principal (2 dígitos)
df_sgp <- read_delim(paste0(base, "CBO2002 - SubGrupo Principal.csv"), delim = ";", locale = locale(encoding = "LATIN1")) %>%
  clean_names() %>%
  rename(cbo_2dig = codigo, cbo_prigru = titulo) %>%
  mutate(cbo_2dig = str_pad(as.character(cbo_2dig), 2, pad = "0"))

# Subgrupo (3 dígitos)
df_sg <- read_delim(paste0(base, "CBO2002 - SubGrupo.csv"), delim = ";", locale = locale(encoding = "LATIN1")) %>%
  clean_names() %>%
  rename(cbo_3dig = codigo, cbo_subgru = titulo) %>%
  mutate(cbo_3dig = str_pad(as.character(cbo_3dig), 3, pad = "0"))

# Família Ocupacional (4 dígitos)
df_fam <- read_delim(paste0(base, "CBO2002 - Familia.csv"), delim = ";", locale = locale(encoding = "LATIN1")) %>%
  clean_names() %>%
  rename(cbo_4dig = codigo, cbo_familia = titulo) %>%
  mutate(cbo_4dig = str_pad(as.character(cbo_4dig), 4, pad = "0"))

# Ocupações (6 dígitos finais)
df_ocup <- read_delim(paste0(base, "CBO2002 - Ocupacao.csv"), delim = ";", locale = locale(encoding = "LATIN1")) %>%
  clean_names() %>%
  rename(cbo_6dig = codigo, cbo_nome = titulo) %>%
  mutate(
    cbo_1dig = str_sub(cbo_6dig, 1, 1),
    cbo_2dig = str_sub(cbo_6dig, 1, 2),
    cbo_3dig = str_sub(cbo_6dig, 1, 3),
    cbo_4dig = str_sub(cbo_6dig, 1, 4)
  )
df_ocup$cbo_nome[df_ocup$cbo_nome == "Operador de pá carregadeira" & df_ocup$cbo_3dig =="715"] <- "Operador de pá carregadeira715" 
df_ocup %>% select(cbo_nome) %>% n_distinct() 

# --- Unir tudo em um só dataframe ---
df_cbo_hier <- df_ocup %>%
  left_join(df_fam, by = "cbo_4dig") %>%
  left_join(df_sg, by = "cbo_3dig") %>%
  left_join(df_sgp, by = "cbo_2dig") %>%
  left_join(df_gg, by = "cbo_1dig") %>%
  relocate(cbo_6dig, cbo_nome, cbo_4dig, cbo_familia, cbo_3dig, cbo_subgru,
           cbo_2dig, cbo_prigru, cbo_1dig, cbo_gragru) %>%
  rename(CodCBO= cbo_6dig, `Ocupação` = cbo_nome) 

##################################################
##################################################
##################################################
temp_ocup <- df_cbo_hier %>% select(CodCBO,`Ocupação`) %>% unique()
tofix_CBOs <- rais_cbo6_uf24 %>% filter(is.na(`Ocupação`)) %>% select(CodCBO) %>% unique()
blix <- left_join(tofix_CBOs, temp_ocup, by = "CodCBO", relationship = "many-to-many")
sum(is.na(blix$`Ocupação`))
# 38
rais_cbo6_uf24 <- rais_cbo6_uf24 %>%
  left_join(temp_ocup, by = "CodCBO", relationship = "many-to-many") %>%
  mutate(`Ocupação` = coalesce(`Ocupação.x`, `Ocupação.y`)) %>%
  select(-`Ocupação.x`, -`Ocupação.y`)

# fix cbo_gragru
temp_ocup <- df_cbo_hier %>% select(CodCBO,`cbo_gragru`) %>% unique()
tofix_CBOs <- rais_cbo6_uf24 %>% filter(is.na(`cbo_gragru`)) %>% select(CodCBO) %>% unique()
blix <- left_join(tofix_CBOs, temp_ocup, by = "CodCBO", relationship = "many-to-many")
sum(is.na(blix$`cbo_gragru`))
rais_cbo6_uf24 <- rais_cbo6_uf24 %>%
  left_join(temp_ocup, by = "CodCBO", relationship = "many-to-many") %>%
  mutate(`cbo_gragru` = coalesce(`cbo_gragru.x`, `cbo_gragru.y`)) %>%
  select(-`cbo_gragru.x`, -`cbo_gragru.y`)

# fix cbo_familia
temp_ocup <- df_cbo_hier %>% select(CodCBO,`cbo_familia`) %>% unique()
tofix_CBOs <- rais_cbo6_uf24 %>% filter(is.na(`cbo_familia`)) %>% select(CodCBO) %>% unique()
blix <- left_join(tofix_CBOs, temp_ocup, by = "CodCBO", relationship = "many-to-many")
sum(is.na(blix$`cbo_familia`))
rais_cbo6_uf24 <- rais_cbo6_uf24 %>%
  left_join(temp_ocup, by = "CodCBO", relationship = "many-to-many") %>%
  mutate(`cbo_familia` = coalesce(`cbo_familia.x`, `cbo_familia.y`)) %>%
  select(-`cbo_familia.x`, -`cbo_familia.y`)

rais_cbo6_uf24 <- rais_cbo6_uf24 %>%
  mutate(
    cbo_1dig = if_else(is.na(cbo_1dig), substr(CodCBO, 1, 1), cbo_1dig),
    cbo_2dig = if_else(is.na(cbo_2dig), substr(CodCBO, 1, 2), cbo_2dig),
    cbo_4dig = if_else(is.na(cbo_4dig), substr(CodCBO, 1, 4), cbo_4dig))
  

# Fix missings 

sum(is.na(rais_cbo4_uf24$cbo_gragru)) # 3299
sum(is.na(rais_cbo4_uf24$vinculos)) # 0
sum(is.na(rais_cbo4_uf24$cbo_1dig)) # 3299
sum(is.na(rais_cbo4_uf24$cbo_2dig)) # 3299
sum(is.na(rais_cbo4_uf24$cbo_4dig)) #  0
sum(is.na(rais_cbo4_uf24$`cbo_gragru`)) # 3299
sum(is.na(rais_cbo4_uf24$`cbo_familia`)) # 3299

fixed424 <- rais_cbo4_uf24 %>% filter(!is.na(cbo_gragru))
tofix424_ <- rais_cbo4_uf24 %>% filter(is.na(cbo_gragru)) %>% select(-cbo_1dig,-cbo_gragru,-cbo_2dig,-cbo_prigru, -cbo_familia)
getcols424 <- df_cbo_hier %>% select(cbo_1dig,cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia)
tofix424 <- left_join(tofix424_,getcols424,by="cbo_4dig", relationship = "many-to-many") %>% unique() %>% arrange(cbo_4dig) %>% filter(!is.na(cbo_gragru))

rais_cbo4_uf24 <- rbind(fixed424,tofix424) # from 15,715 to 15,573 
sum(is.na(rais_cbo4_uf24$cbo_gragru)) # 0





sum(is.na(rais_cbo6_uf24$`Ocupação`)) # 248
sum(is.na(rais_cbo6_uf24$cbo_gragru)) # 352
sum(is.na(rais_cbo6_uf24$vinculos)) # 0
sum(is.na(rais_cbo6_uf24$cbo_1dig)) # 0
sum(is.na(rais_cbo6_uf24$cbo_2dig)) # 0
sum(is.na(rais_cbo6_uf24$cbo_4dig)) # 0
sum(is.na(rais_cbo6_uf24$`cbo_gragru`)) # 352
sum(is.na(rais_cbo6_uf24$`cbo_familia`)) # 352


fixed624 <- rais_cbo6_uf24 %>% filter(!is.na(cbo_gragru))
# tofix624_ <- rais_cbo6_uf24 %>% filter(is.na(cbo_gragru)) %>% filter(nchar(CodCBO)==5| CodCBO == "999999") # 147 obs I don't know what to do with 5 digit CodCBOs
# nor 28 with 999999 so 175 out
tofix624_ <- rais_cbo6_uf24 %>% filter(is.na(cbo_gragru) & nchar(CodCBO)==6 & CodCBO != "999999") %>% 
         select(-cbo_1dig,-cbo_gragru,-cbo_2dig,-cbo_prigru, -cbo_familia, -cbo_4dig, -`Ocupação`)
# 177 potential fixes
getcols624 <- df_cbo_hier %>% select(cbo_1dig,cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia, `Ocupação`,CodCBO)
tofix624 <- left_join(tofix624_,getcols624,by="CodCBO", relationship = "many-to-many") %>% unique() %>% arrange(CodCBO) 
# Cannot fix - the CodCBOs are not present in the df_cbo_hier my source of truth for CBO



# Lets save working/rais
save(rais_cbo4_uf24, file = "D:/Country/Brazil/TechBrazil/working/rais/rais_cbo4_uf24.rda")
save(rais_cbo6_uf24, file = "D:/Country/Brazil/TechBrazil/working/rais/rais_cbo6_uf24.rda")






names(rais2023)

rais_cbo_uf23_ <- rais2023 %>% rename(CodCBO=`CBO Ocupação 2002`, vinculos =`Vínculo Ativo 31/12`) %>%
  mutate(CodCBO=as.character(CodCBO)) 

temp_uf <- df_codes_ibge %>% select(CO_MUN6,CO_UF,SG_UF,NM_UF) %>% unique()
temp_cbo <- qbq_ocup_cmento1 %>% select(CodCBO, `Ocupação`,cbo_1dig,cbo_gragru,cbo_2dig, cbo_prigru,cbo_4dig,cbo_familia) %>% 
  distinct()  %>% filter(!is.na(cbo_gragru))

temp_cbo %>% select(cbo_gragru) %>% n_distinct() # 9

#df_cbo_hier %>% select(cbo_1dig,cbo_gragru) %>% distinct() # actually is 9 as there is also NA 0 military is missing ok


rais_cbo6_uf23 <- rais_cbo_uf23_ %>%  
  left_join(temp_cbo, by = "CodCBO", relationship = "many-to-many") %>% 
    select(CodCBO, vinculos, CO_UF, SG_UF, NM_UF,
         cbo_1dig, cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia, `Ocupação`) %>%
  filter(!is.na(CodCBO)) %>%
  group_by(CodCBO, CO_UF, SG_UF, NM_UF,cbo_1dig, cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia, `Ocupação`) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = 'drop') %>%
  arrange(CO_UF, desc(vinculos))

sum(rais_cbo6_uf23$vinculos, na.rm = TRUE)
# 54,613,569
rais_cbo4_uf23 %>% select(cbo_gragru) %>% distinct()



# Step 1: Create the 4-digit CBO code
rais_cbo4_uf23_ <- rais_cbo_uf23_ 
rais_cbo4_uf23_$cbo_4dig <- substr(rais_cbo4_uf23_$CodCBO, 1, 4)
# Now we keep the feasible columns and compute the sum of vinculos by 4 digits without any joins
rais_cbo4_uf23_z <- rais_cbo4_uf23_ %>%
  select(cbo_4dig,vinculos,CO_MUN6, CO_UF, SG_UF, NM_UF) %>% 
  group_by(cbo_4dig, CO_MUN6,CO_UF, SG_UF, NM_UF) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = 'drop')

  # sum(is.na(rais_cbo4_uf23_z$cbo_4dig)) is 0
rais_cbo4_uf23_z <- rais_cbo4_uf23_z %>%
  group_by(CO_UF, SG_UF, NM_UF,cbo_4dig) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = 'drop') %>%
  arrange(CO_UF, desc(vinculos)) # 54613569 good
sum(rais_cbo4_uf23_z$vinculos)

rais_cbo4_uf23 <- rais_cbo4_uf23_z %>% 
  left_join(temp_cbo, by = "cbo_4dig", relationship = "many-to-many") %>%
  select(vinculos, CO_UF, SG_UF, NM_UF,
         cbo_1dig, cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia) %>% unique()
sum(rais_cbo4_uf23$vinculos) # 54613569 good

sum(is.na(rais_cbo6_uf23$`Ocupação`)) # 17566


# fix cbo_gragru
temp_ocup <- df_cbo_hier %>% select(CodCBO,`cbo_gragru`) %>% unique()
tofix_CBOs <- rais_cbo6_uf23 %>% filter(is.na(`cbo_gragru`)) %>% select(CodCBO) %>% unique()
blix <- left_join(tofix_CBOs, temp_ocup, by = "CodCBO", relationship = "many-to-many")
sum(is.na(blix$`cbo_gragru`)) # 48
rais_cbo6_uf23 <- rais_cbo6_uf23 %>%
  left_join(temp_ocup, by = "CodCBO", relationship = "many-to-many") %>%
  mutate(`cbo_gragru` = coalesce(`cbo_gragru.x`, `cbo_gragru.y`)) %>%
  select(-`cbo_gragru.x`, -`cbo_gragru.y`)

# fix cbo_familia
temp_ocup <- df_cbo_hier %>% select(CodCBO,`cbo_familia`) %>% unique()
tofix_CBOs <- rais_cbo6_uf23 %>% filter(is.na(`cbo_familia`)) %>% select(CodCBO) %>% unique()
blix <- left_join(tofix_CBOs, temp_ocup, by = "CodCBO", relationship = "many-to-many")
sum(is.na(blix$`cbo_familia`)) # 48
rais_cbo6_uf23 <- rais_cbo6_uf23 %>%
  left_join(temp_ocup, by = "CodCBO", relationship = "many-to-many") %>%
  mutate(`cbo_familia` = coalesce(`cbo_familia.x`, `cbo_familia.y`)) %>%
  select(-`cbo_familia.x`, -`cbo_familia.y`)

rais_cbo6_uf23 <- rais_cbo6_uf23 %>%
  mutate(
    cbo_1dig = if_else(is.na(cbo_1dig), substr(CodCBO, 1, 1), cbo_1dig),
    cbo_2dig = if_else(is.na(cbo_2dig), substr(CodCBO, 1, 2), cbo_2dig),
    cbo_4dig = if_else(is.na(cbo_4dig), substr(CodCBO, 1, 4), cbo_4dig))



sum(is.na(rais_cbo4_uf23$cbo_gragru)) # 3683
sum(is.na(rais_cbo4_uf23$vinculos)) # 0
sum(is.na(rais_cbo4_uf23$cbo_1dig)) # 3683
sum(is.na(rais_cbo4_uf23$cbo_2dig)) # 3683
sum(is.na(rais_cbo4_uf23$cbo_4dig)) #  0
sum(is.na(rais_cbo4_uf23$`cbo_gragru`)) # 3683
sum(is.na(rais_cbo4_uf23$`cbo_familia`)) # 3683


fixed423 <- rais_cbo4_uf23 %>% filter(!is.na(cbo_gragru))
tofix423_ <- rais_cbo4_uf23 %>% filter(is.na(cbo_gragru)) %>% select(-cbo_1dig,-cbo_gragru,-cbo_2dig,-cbo_prigru, -cbo_familia)
getcols423 <- df_cbo_hier %>% select(cbo_1dig,cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia)
tofix423 <- left_join(tofix423_,getcols423,by="cbo_4dig", relationship = "many-to-many") %>% unique() %>% arrange(cbo_4dig) %>% filter(!is.na(cbo_gragru))

rais_cbo4_uf23 <- rbind(fixed423,tofix423) # from 16,185 to 15,794
sum(is.na(rais_cbo4_uf23$cbo_gragru)) # 0



sum(is.na(rais_cbo6_uf23$`Ocupação`)) # 17,566
sum(!is.na(rais_cbo6_uf23$`Ocupação`)) # 41,055
sum(is.na(rais_cbo6_uf23$cbo_1dig)) # 0
sum(is.na(rais_cbo6_uf23$cbo_2dig)) # 0
sum(is.na(rais_cbo6_uf23$cbo_4dig)) # 0
sum(is.na(rais_cbo6_uf23$`cbo_gragru`)) # 910
sum(is.na(rais_cbo6_uf23$`cbo_familia`)) # 910


fixed623 <- rais_cbo6_uf23 %>% filter(!is.na(cbo_gragru))
# tofix624_ <- rais_cbo6_uf24 %>% filter(is.na(cbo_gragru)) %>% filter(nchar(CodCBO)==5| CodCBO == "999999") # 147 obs I don't know what to do with 5 digit CodCBOs
# nor 28 with 999999 so 175 out
tofix623_ <- rais_cbo6_uf23 %>% filter(is.na(cbo_gragru) & nchar(CodCBO)==6 & CodCBO != "999999") %>% 
  select(-cbo_1dig,-cbo_gragru,-cbo_2dig,-cbo_prigru, -cbo_familia, -cbo_4dig, -`Ocupação`)
# 272 potential fixes
getcols623 <- df_cbo_hier %>% select(cbo_1dig,cbo_gragru,cbo_2dig,cbo_prigru,cbo_4dig,cbo_familia, `Ocupação`,CodCBO)
tofix623 <- left_join(tofix623_,getcols623,by="CodCBO", relationship = "many-to-many") %>% unique() %>% arrange(CodCBO) 
# Cannot fix - the CodCBOs are not present in the df_cbo_hier my source of truth for CBO


sum(is.na(rais_cbo6_uf23$`Ocupação`)) # 17,566

# What about `Ocupação` 
tofix_CBOs <- rais_cbo6_uf23 %>% filter(is.na(`Ocupação`)) %>% select(CodCBO) %>% unique()
temp_ocup <- df_cbo_hier %>% select(CodCBO,`Ocupação`) %>% unique()
blix <- left_join(tofix_CBOs, temp_ocup, by = "CodCBO", relationship = "many-to-many")
sum(is.na(blix$`Ocupação`))
# 48

rais_cbo6_uf23 <- rais_cbo6_uf23 %>%
  left_join(temp_ocup, by = "CodCBO", relationship = "many-to-many") %>%
  mutate(`Ocupação` = coalesce(`Ocupação.x`, `Ocupação.y`)) %>%
  select(-`Ocupação.x`, -`Ocupação.y`)

sum(is.na(rais_cbo6_uf23$`Ocupação`)) # 910


# Lets save working/rais
save(rais_cbo4_uf23, file = "D:/Country/Brazil/TechBrazil/working/rais/rais_cbo4_uf23.rda")
save(rais_cbo6_uf23, file = "D:/Country/Brazil/TechBrazil/working/rais/rais_cbo6_uf23.rda")






