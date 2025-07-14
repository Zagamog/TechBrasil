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



