# explore_cnct.R

library(dplyr)

names(df_cnct2025a)

# [1] "course_id"                        "Eixo Tecnológico"                
# [3] "Área Tecnológica"                 "Denominação do Curso"            
# [5] "Perfil Profissional de Conclusão" "Carga Horária Mínima"            
# [7] "Descrição Carga Horária Mínima"   "Pré-Requisitos para Ingresso"    
# [9] "Itinerários Formativos"           "Campo de Atuação"                
# [11] "Ocupações CBO Associadas"         "Infraestrutura Mínima"           
# [13] "Legislação Profissional"          "eixo_code"                       
# [15] "area_code"                        "curso_code"  

df_cnct2025a %>% select(course_id) %>% n_distinct() # 218

df_cnct2025a %>% select(`Eixo Tecnológico`) %>% n_distinct()
df_cnct2025a %>% select(`Eixo Tecnológico`) %>% unique() %>% arrange()

df_cnct2025a%>% select(`Área Tecnológica`) %>% n_distinct()

names(df_censo_supl_tec24)

# [1] "CO_MUN"                     "CO_ENTIDADE"               
# [3] "NO_ENTIDADE"                "ANO"                       
# [5] "TP_DEPENDENCIA"             "NO_AREA_CURSO_PROFISSIONAL"
# [7] "NO_CURSO_EDUC_PROFISSIONAL" "QT_CURSO_TEC"              
# [9] "QT_MAT_CURSO_TEC"           "QT_CURSO_TEC_CT"           
# [11] "QT_MAT_CURSO_TEC_CT"        "QT_CURSO_TEC_NM"           
# [13] "QT_MAT_CURSO_TEC_NM"        "QT_CURSO_TEC_CONC"         
# [15] "QT_MAT_CURSO_TEC_CONC"      "QT_CURSO_TEC_SUBS"         
# [17] "QT_MAT_TEC_SUBS"            "QT_CURSO_TEC_EJA"          
# [19] "QT_MAT_TEC_EJA"      

df_censo_supl_tec24 %>% select(NO_AREA_CURSO_PROFISSIONAL) %>%   n_distinct()  # 14
df_censo_supl_tec24 %>% select(NO_AREA_CURSO_PROFISSIONAL) %>% unique() %>% arrange()

df_censo_supl_tec24 %>% select(NO_CURSO_EDUC_PROFISSIONAL) %>%   n_distinct()  # 199

df_censo_supl_tec %>% select(ID_AREA_CURSO_PROFISSIONAL) %>% unique() %>% arrange()

# 1 to 13 and 999

df_censo_supl_tec24 %>% select(ID_AREA_CURSO_PROFISSIONAL, CO_CURSO_EDUC_PROFISSIONAL) %>% unique() %>%
  mutate(temp=as.character(CO_CURSO_EDUC_PROFISSIONAL)) %>% 
  unique() %>% arrange(temp) %>% select(-CO_CURSO_EDUC_PROFISSIONAL) %>% print(n=199)


df_cu_censos <- df_censo_supl_tec %>%
  mutate(
    curso_str = as.character(CO_CURSO_EDUC_PROFISSIONAL),
    curso_len = nchar(curso_str),
    eixo_code = case_when(
      curso_len == 4 ~ substr(curso_str, 1, 1),
      curso_len == 5 ~ substr(curso_str, 1, 2),
      TRUE ~ NA_character_
    ),
    eixo_code = sprintf("%02d", as.integer(eixo_code)),  # pad with leading zero
    curso_cursocode = case_when(
      curso_len == 4 ~ substr(curso_str, 2, 4),
      curso_len == 5 ~ substr(curso_str, 3, 5),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(eixo_code != "99") %>%
  select(CO_CURSO_EDUC_PROFISSIONAL, eixo_code, curso_cursocode, 
         NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL) %>%
  arrange(CO_CURSO_EDUC_PROFISSIONAL)


df_cu_censos %>% select(eixo_code,NO_AREA_CURSO_PROFISSIONAL) %>% unique() %>% arrange(eixo_code)
df_cnct_ %>% select(eixo_code,`Eixo Tecnológico`) %>% unique() %>% arrange(eixo_code)



df_cnct_ %>% select(`Denominação do Curso`) %>%  n_distinct()  # 215
df_cnct_ %>% select(`Eixo Tecnológico`, `Denominação do Curso`) %>% unique() %>% arrange()

df_cu_censos %>% select(NO_AREA_CURSO_PROFISSIONAL,NO_CURSO_EDUC_PROFISSIONAL) %>%  n_distinct()  # 202
df_cu_censos %>% select(NO_AREA_CURSO_PROFISSIONAL,NO_CURSO_EDUC_PROFISSIONAL) %>% unique() %>% arrange() %>% print(n=202)

junk <- df_censo_supl_tec %>% filter( NO_CURSO_EDUC_PROFISSIONAL == "Outros - Eixo Ambiente e Saúde") 
junk <- df_cnct_ %>% filter(`Denominação do Curso` == "Técnico em Dependência Química")

qbq_ocup_cmento1 %>% select(desArea) %>% unique() %>% arrange()
