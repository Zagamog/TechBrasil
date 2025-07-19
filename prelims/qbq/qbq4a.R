# qbq4a.R

# Matching courses and occupations


# From AWS S3 bucket

library(aws.s3)
library(dotenv)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(janitor)
library(stringr)
library(stringi)
library(purrr)
library(tidyr)
library(reticulate)


# Load credentials
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

# Helper function to check/download if missing
update_data_from_s3 <- function(local_path, s3_key, bucket) {
  if (!file.exists(local_path)) {
    tryCatch({
      message("☁️ Downloading missing file from S3: ", s3_key)
      save_object(object = s3_key, bucket = bucket, file = local_path)
    }, error = function(e) {
      stop("❌ Failed to download from S3: ", s3_key, " — ", e$message)
    })
  } else {
    message("✅ Local version found: ", basename(local_path))
  }
}

# Load the data

paths <- list(
  df_censo_supl_tec_4qbq = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_censo_supl_tec_4qbq.rda",
  qbq_ocup = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup.rda",
  qbq_conhecimento1 = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_conhecimento1.rda"
)

s3_keys <- list(
  df_censo_supl_tec_4qbq = "working/qbq/df_censo_supl_tec_4qbq.rda",
  qbq_conhecimento1 = "working/qbq/qbq_conhecimento1.rda",
  qbq_ocup = "working/qbq/qbq_ocup.rda"
)


# use purrr::walk2 to check and download if missing

walk2(paths, s3_keys, ~update_data_from_s3(.x, .y, bucket_name))

# Load the data into R
load("D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbq.rda")
load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup.rda")
load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_conhecimento1.rda")



qbq_conhecimento1 %>% select(desArea) %>% unique() %>% n_distinct() # 22
qbq_conhecimento1 %>% select(desArea) %>% unique() 
# desArea
# 1                                                                           CIÊNCIAS EXATAS E INFORMÁTICA
# 6                                                                                     CIÊNCIAS BIOLÓGICAS
# 7                                                                              CIÊNCIAS SOCIAIS APLICADAS
# 19                                                                                       CIÊNCIAS HUMANAS
# 28                                                                            LINGUÍSTICA, LETRAS E ARTES
# 31                                   OUTROS CONHECIMENTOS DOS DOMÍNIOS DE FORMAÇÃO GERAL E/OU TRANSVERSAL
# 32                                                                       ADMINISTRAÇÃO, GESTÃO E NEGÓCIOS
# 51                                                                                  PROCESSOS DE COMÉRCIO
# 54                                             TURISMO, HOSPEDAGEM, ALIMENTAÇÃO, EVENTOS, ESPORTE E LAZER
# 260                                                                         SERVIÇOS DA SAÚDE E BEM-ESTAR
# 284                                                                                     CIÊNCIAS AGRÁRIAS
# 322                                                                      PROCESSOS DE PRODUÇÃO INDUSTRIAL
# 525                                                       SERVIÇOS DE TRANSPORTES, ARMAZENAGEM E CORREIOS
# 556                                                                 PRODUÇÃO CULTURAL, DESIGN E DECORAÇÃO
# 621                                                                          SERVIÇOS FINANCEIROS E AFINS
# 769                                  SERVIÇOS EDUCACIONAIS, DE DESENVOLVIMENTO COMUNITÁRIO E DEFESA CIVIL
# 1168                        SERVIÇOS DE UTILIDADE PÚBLICA (TELECOMUNICAÇÕES, SANEAMENTO BÁSICO E ENERGIA)
# 1944              PROCESSOS DE PRODUÇÃO EM AGRICULTURA, PECUÁRIA, PRODUÇÃO FLORESTAL, PESCA E AQUICULTURA
# 2609                                                                                    CIÊNCIAS DA SAÚDE
# 3856                                                  MODA, ESTÉTICA, EMBELEZAMENTO E SERVIÇOS ÀS PESSOAS
# 15384 LIMPEZA, CONSERVAÇÃO, PORTARIA, VIGILÂNCIA, ZELADORIA E MANUTENÇÃO DE EDIFÍCIOS E DE ÁREAS PÚBLICAS
# 23969                                                SAÚDE, CUIDADOS E ADESTRAMENTO DE ANIMAIS DOMÉSTICOS

qbq_conhecimento1 %>% select(desCampo) %>% unique() %>% n_distinct() # 211
qbq_conhecimento1 %>% select(desCampo) %>% unique() %>% arrange(desCampo)

# First 15 of 211
# desCampo
# 1                                                                                                               PRODUÇÃO EM PESCA E AQUICULTURA
# 2                                                                                                            ADESTRAMENTO DE ANIMAIS DOMÉSTICOS
# 3                                                                                                                        ADMINISTRAÇÃO E GESTÃO
# 4                                                                                                       APOIO AO ALUNO DE SERVIÇOS EDUCACIONAIS
# 5                                                                                                           ARQUITETURA E ORGANIZAÇÃO DO ESPAÇO
# 6                                                                                                                                         ARTES
# 7                                                                                                                               ARTES CIRCENSES
# 8                                                                                                                                 ARTES CÊNICAS
# 9                                                                                                                                ARTES MUSICAIS
# 10                                                                                                                              ARTES PLÁSTICAS
# 11                                                                                                                       ATENDIMENTO AO CLIENTE
# 12                                                                                                        AUTOMAÇÃO EM AGROPECUÁRIA E FLORESTAL
# 13                                                                                                   AUTOMAÇÃO EM SERVIÇOS DE UTILIDADE PÚBLICA
# 14                                                            AUTOMAÇÃO EM SERVIÇOS EDUCACIONAIS, DE DESENVOLVIMENTO COMUNITÁRIO E DEFESA CIVIL

qbq_conhecimento1 %>% select(desConhecimento) %>% unique() %>% n_distinct() # 21,118
qbq_conhecimento1 %>% select(desConhecimento) %>% unique() %>% print(max=50)


# Merge 
qbq_ocup_cmento1_ <- qbq_ocup %>%  select(CodCBO,`Ocupação`, `Síntese`, PerfilOcupacional, NivelOcupacao) %>% 
  filter(!is.na(PerfilOcupacional) & !is.null(PerfilOcupacional) & PerfilOcupacional!="NULL") %>%
  left_join(qbq_conhecimento1, by = "CodCBO") %>%
  filter(!is.na(desArea) & !is.null(desArea) & desArea!="NULL") %>%
  select(CodCBO, `Ocupação`, `Síntese`, PerfilOcupacional, NivelOcupacao, desArea, desCampo, desConhecimento) %>% 
  filter(!is.na(PerfilOcupacional)) %>% unique() %>% arrange(CodCBO)
# from 87,083 to 86,310 obs



qbq_ocup_cmento1 <- qbq_ocup %>% 
  select(CodCBO, `Ocupação`, `Síntese`, PerfilOcupacional, NivelOcupacao) %>%
  filter(!is.na(PerfilOcupacional) & !is.null(PerfilOcupacional) & PerfilOcupacional!="NULL") %>%
  left_join(qbq_conhecimento1, by = "CodCBO") %>%
  filter(!is.na(desArea) & !is.null(desArea) & desArea!="NULL") %>%
  distinct() %>%
  group_by(CodCBO, `Ocupação`, `Síntese`, PerfilOcupacional, NivelOcupacao) %>%
  summarise(
    desArea = list(unique(na.omit(desArea))),
    desCampo = list(unique(na.omit(desCampo))),
    desConhecimento = list(unique(na.omit(desConhecimento))),
    .groups = "drop"
  )
#1899 occupations


# save
save(qbq_ocup_cmento1, file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.rda")
# In AWS
put_object(file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.rda",
           object = "working/qbq/qbq_ocup_cmento1.rda",
           bucket = bucket_name)

# Convert to data.frame first if necessary
# reticulate::conda_list()
pd <- import("pandas")
df_py <- r_to_py(qbq_ocup_cmento1)
# Save as pickle (requires reticulate config + pandas setup)
py_save_object(qbq_ocup_cmento1, "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.pkl")

# Save to .pkl file
put_object(file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.pkl",
           object = "working/qbq/qbq_ocup_cmento1.pkl",
           bucket = bucket_name)


# The cnct file in pickle too
df_py <- r_to_py(df_censo_supl_tec_4qbq)
# Save as pickle (requires reticulate config + pandas setup)
py_save_object(df_censo_supl_tec_4qbq, "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbq.pkl")

# Save to .pkl file
put_object(file = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbq.pkl",
           object = "working/qbq/df_censo_supl_tec_4qbq.pkl",
           bucket = bucket_name)








