# WB_J_ST_ind.R 
library(dplyr)


# From memory as of 01/10/26 we have df_rais_all

df_rais_all %>% select(cbo_gragru) %>% distinct() %>% arrange(cbo_gragru)

# From course side we may get 
load("D:/Country/Brazil/TechBrazil/RShiny/Produtos2/caged_rais_cnct_2020_2024_shiny.rda")

caged_rais_curso %>% select(Eixo_Tecnologico) %>% distinct() %>% arrange(Eixo_Tecnologico)

# Area_Tecnologica

blix <- caged_rais_curso %>% select(Eixo_Tecnologico,Area_Tecnologica) %>% distinct() 

  
blix <- caged_rais_curso %>% filter(Area_Tecnologica=="Mineração e Extração") %>% 
  select(Area_Tecnologica,Curso) %>% distinct()
