options(scipen = 999)

library(tidyverse)
library(here)
library(deflateBR)

input_folder <- here("rawdata") 

importa_fundeb <- function(file, year){
read_xls(here(input_folder, paste0("fundeb/", file)), #caminhos das tabelas importadas do Tesouro Transparente
         sheet = "E_TOTAL",
         skip = 7) %>%
  filter(!is.na(UF)) %>%
  mutate(ano = year) %>%
  select(ano, UF, TOTAL) %>%
  rename(sigla_uf = UF,
         fundeb = TOTAL) 
}

file_names <- c("pge_fundeb_2016.xls","pge_fundeb_2017.xls", "Fundeb 2018.xls", "Fundeb 2019.xls", "Fundeb 2020.xls", 
                "Fundeb 2021.xls", "Fundeb 2022.xls", "Fundeb 2023.xls", "Fundeb 2024.xls")
anos <- c(2016:2024)

fundeb <- map2_df(.x = file_names, .y = anos, .f = importa_fundeb)

#Puxa base de matrículas para estimar fluxos FUNDEB
matriculas_uf <- read_csv(here("working", "mec_inep","matriculas_total_em_ept_uf_20162024.csv")) 


#Junta as duas bases
fundeb_ept <- left_join(fundeb, matriculas_uf, by = c("ano", "sigla_uf")) %>%
  mutate(
    date = as.Date(paste0(ano, "-07-01")), # using mid-year as approximation
    
    fundeb_ept_pond = fundeb*share_matriculas_ept_pond,
    fundeb_em_pond = fundeb*share_matriculas_em_pond,
    
    across(.cols = contains("fundeb"),
           .fns = ~ deflate(
             nominal_values = .x,
             index = "inpc",
             nominal_date = date,
             real_date = "12/2024"),
           .names = "{.col}_real")
  ) %>%
  select(- date) %>%
  relocate(fundeb_ept_pond, fundeb_em_pond, fundeb_real, fundeb_ept_pond_real,
           fundeb_em_pond_real, .after = fundeb)

write_csv(fundeb,here("working", "fundeb", "fundeb_matriculas_uf_ept_em.csv"))

