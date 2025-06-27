# bahia_conect1a.R

library(dplyr)
library(ggplot2)
library(sf)
library(openxlsx)
library(scales) # for pretty_breaks
library(RColorBrewer)
library(tidyr)

# Load the downloaded connectivity data from June 23, 2025

# Data https://conectividadenaeducacao.nic.br/#home 

# divulgacao_ensino_medio_municipios_2023.xlsx
conect_esc24 <- read.xlsx("D:/Country/Brazil/TechBrazil/rawdata/simet/base-escolas-simet-2025-06-23.xlsx",
                        sheet="base_dados",colNames = TRUE) 

conect_esc24$escolar_qt_desktop_aluno[conect_esc24$esolar_qt_desktop_aluno==88888] <- 0
conect_esc24$escolar_qt_desktop_aluno[conect_esc24$escolar_co_entidade==29043409] <- 0
conect_esc24$escolar_qt_comp_portatil_aluno[conect_esc24$escolar_qt_comp_portatil_aluno==88888] <- 0
conect_esc24$escolar_qt_tablet_aluno[conect_esc24$escolar_qt_tablet_aluno==88888] <- 0

# Select and  Harmonize variable names
conect_esc24b <- conect_esc24 %>% select(1,5,6,8:17,19:23,28:31,35,36,43,57:59) %>%
  rename(CO_MUN=escolar_co_municipio,CO_UF=escolar_co_uf)

load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")

ibge_codes <- df_codes_ibge %>% select(CO_MUN,NM_MUN,SG_UF,CO_UF,NM_UF) %>% unique()

conect_esc24c <- left_join(conect_esc24b,ibge_codes,by=c("CO_MUN","CO_UF"),relationship = "many-to-many") 
  
conect_esc24c$simet_mean_tcp_down_mbps[conect_esc24c$simet_mean_tcp_down_mbps=="sem medidor"] <- NA
# replace commas with dots in the column simet_mean_tcp_down_mbps
conect_esc24c$simet_mean_tcp_down_mbps <- gsub(",",".",conect_esc24c$simet_mean_tcp_down_mbps)

# convert to numeric
conect_esc24c$simet_mean_tcp_down_mbps <- as.numeric(conect_esc24c$simet_mean_tcp_down_mbps)

mean(conect_esc24c$simet_mean_tcp_down_mbps,na.rm=TRUE)

conect_esc24c$summer <- 1

conect_esc24c %>% group_by(NM_UF) %>%
  summarise(mean_speed=round(mean(simet_mean_tcp_down_mbps,na.rm=TRUE))) %>% print(n=27)

conect_esc24c %>% filter(!is.na(simet_mean_tcp_down_mbps)) %>% filter(escolar_tp_dependencia=="Estadual") %>%  group_by(NM_UF) %>%
  summarise(n_scls_con=sum(summer)) %>% print(n=27)

ba_esc24c <- conect_esc24c %>% filter(SG_UF=="BA" & escolar_tp_dependencia=="Estadual") 

ba_esc24c %>% filter(!is.na(simet_mean_tcp_down_mbps)) %>% filter(escolar_tp_dependencia=="Estadual") %>%  group_by(NM_MUN) %>%
  summarise(mean_speed=round(mean(simet_mean_tcp_down_mbps,na.rm=TRUE)),
            mean_ideal=round(mean(`Vel..download.necessaria.(Mbit/s)`,na.rm=TRUE))) %>%
  mutate(need_index=mean_ideal/mean_speed) %>% arrange(desc(mean_speed)) %>% print(n=220)


###
# escolar_in_laboratorip_informatica
# convet não sim to 1 0 then count
conect_esc24c %>% group_by(NM_UF) %>% filter(escolar_tp_dependencia=="Estadual") %>% 
  mutate(escolar_in_laboratorio_informatica=ifelse(escolar_in_laboratorio_informatica=="Sim",1,0)) %>%
  summarise(n_scls_lab=sum(escolar_in_laboratorio_informatica), n_scls=sum(summer)) %>%
  mutate(prop_lab_inf=(n_scls_lab/n_scls)*100) %>% arrange(desc(prop_lab_inf)) %>% print(n=27)
  

# Summarize proportion of schools with IT labs
summary_lab <- conect_esc24c %>%
  group_by(NM_UF) %>%
  filter(escolar_tp_dependencia == "Estadual") %>%
  mutate(escolar_in_laboratorio_informatica = ifelse(escolar_in_laboratorio_informatica == "Sim", 1, 0)) %>%
  summarise(
    n_scls_lab = sum(escolar_in_laboratorio_informatica, na.rm = TRUE),
    n_scls = sum(summer),
    prop_lab_inf = round((n_scls_lab / n_scls) * 100, 1)
  ) %>%
  arrange(desc(prop_lab_inf))

saveRDS(summary_lab, "working/Bahia/summary_lab_prop.rds")




summary_net1 <- conect_esc24c %>%
  group_by(NM_UF) %>%
  filter(escolar_tp_dependencia == "Estadual") %>%
  mutate(escolar_in_internet = ifelse(escolar_in_internet == "Sim", 1, 0)) %>%
  summarise(
    n_scls_net = sum(escolar_in_internet, na.rm = TRUE),
    n_scls = sum(summer),
    prop_net1= round((n_scls_net / n_scls) * 100, 1)
  ) %>%
  arrange(desc(prop_net1))

saveRDS(summary_net1, "working/Bahia/summary_net1_prop.rds")



summary_net2 <- conect_esc24c %>%
  group_by(NM_UF) %>%
  filter(escolar_tp_dependencia == "Estadual") %>%
  mutate(escolar_in_internet_aprendizagem = ifelse(escolar_in_internet_aprendizagem == "Sim", 1, 0)) %>%
  summarise(
    n_scls_net = sum(escolar_in_internet_aprendizagem, na.rm = TRUE),
    n_scls = sum(summer),
    prop_net2= round((n_scls_net / n_scls) * 100, 1)
  ) %>%
  arrange(desc(prop_net2))

saveRDS(summary_net2, "working/Bahia/summary_net2_prop.rds")


summary_net2_sat <- conect_esc24c %>%
  group_by(NM_UF) %>%
  filter(escolar_tp_dependencia == "Estadual") %>%
  summarise(
    n_scls_sat = sum(satelite_mec == TRUE, na.rm = TRUE),
    n_scls = sum(summer),
    prop_sat = round((n_scls_sat / n_scls) * 100, 1)
  ) %>%
  arrange(desc(prop_sat))

saveRDS(summary_net2_sat, "working/Bahia/summary_net2_satprop.rds")


school_net_wrat <- conect_esc24c %>%
  mutate(
    simet_clean = as.numeric(simet_mean_tcp_down_mbps),
    enroll = as.numeric(escolar_qtematriculas_maior_turno),
    actual_per_student = ifelse(simet_clean > 0 & enroll > 0,
                                round(simet_clean / enroll, 2), NA_real_)
  ) %>%
  filter(
    escolar_tp_dependencia == "Estadual",
    !is.na(latitude), !is.na(longitude)
  ) %>%
  group_by(CO_MUN) %>%
  mutate(
    N_ESTMUN = sum(enroll, na.rm = TRUE),
    muni_weighted_actual = if (all(is.na(actual_per_student))) NA_real_
    else round(sum(actual_per_student * enroll, na.rm = TRUE) / sum(enroll[!is.na(actual_per_student)], na.rm = TRUE), 2)
  ) %>%
  ungroup() %>%
  group_by(NM_UF) %>%
  mutate(
    N_ESTEST = sum(enroll, na.rm = TRUE),
    state_weighted_actual = if (all(is.na(actual_per_student))) NA_real_
    else round(sum(actual_per_student * enroll, na.rm = TRUE) / sum(enroll[!is.na(actual_per_student)], na.rm = TRUE), 2)
  ) %>%
  ungroup() %>%
  select(
    escolar_co_entidade,
    latitude,
    longitude,
    CO_MUN,
    NM_MUN,
    NM_UF,
    enroll,
    N_ESTMUN,
    N_ESTEST,
    simet_clean,
    simet_mean_tcp_down_mbps,
    actual_per_student,
    muni_weighted_actual,
    state_weighted_actual
  )

saveRDS(school_net_wrat, "working/Bahia/school_net_wrat.rds")




