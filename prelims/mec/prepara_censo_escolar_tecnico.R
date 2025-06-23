library(here)
library(tidyverse)
library(janitor)


input_folder <- here("rawdata", "mec_inep")

importa_censo_tec <- function(ano){
  read_csv2(here(input_folder, paste0("suplemento_cursos_tecnicos_", ano, ".csv")), #arquivo input consta nos microdados do Censo Escolar, baixado no site do INEP
                   locale = locale(encoding = "ISO-8859-1")) %>%
  clean_names %>%
    rename(ano = nu_ano_censo,
           sigla_uf = sg_uf,
           id_uf = co_uf,
           id_municipio = co_municipio) %>%
    filter(tp_dependencia == 2) %>%
    select(ano, sigla_uf, id_uf, id_municipio, no_entidade, tp_dependencia, 
           no_area_curso_profissional, no_curso_educ_profissional, qt_mat_curso_tec)
}

censo_tec <- map_dfr(.x = c(2023, 2024), .f = importa_censo_tec)


###Confere totais de matrículas no suplemento de edu tecnica e no questionário básico
censo_tec %>%
  group_by(sigla_uf, ano) %>%
  summarise(matriculas_tec = sum(qt_mat_curso_tec, na.rm = TRUE)) -> confere

matriculas_uf <- read_csv(here("working", "mec_inep","matriculas_total_em_ept_uf_20162024.csv")) 
inner_join(select(matriculas_uf, ano, sigla_uf, matriculas_total_tecprof),
           confere, by = c("ano", "sigla_uf")) -> compara
#Alagoas e Goias estão com números divergentes de matrículas EPT
#Por enquanto vou ignorar e seguir

censo_tec %>%
  group_by(id_municipio, ano, no_area_curso_profissional, no_curso_educ_profissional) %>%
  summarise(matriculas_tec = sum(qt_mat_curso_tec, na.rm = TRUE)) %>%
  ungroup()-> matriculas_tec_ano_mun_area_curso

#Junta com códigos IBGE de regiões imediatas e intermediárias
regioes_ibge <- read_xlsx(here("rawdata", "ibge", "regioes_geograficas_composicao_por_municipios_2017_20180911.xlsx")) %>%
  rename(id_municipio = CD_GEOCODI) %>%
  select(id_municipio, nome_rgi, nome_rgint) %>%
  mutate(id_municipio = as.numeric(id_municipio))

left_join(matriculas_tec_ano_mun_area_curso, regioes_ibge, by = "id_municipio") %>% 
  relocate(ano, nome_rgi, nome_rgint, .before = id_municipio) -> matriculas_tec_ano_mun_area_curso

write_csv(matriculas_tec_ano_uf_area_curso, here("working", "mec_inep", "matriculas_tec_ano_mun_area_curso.csv"))


