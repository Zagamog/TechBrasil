# qbq2a.R

# Matching qbq occupations with courses


# https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/quadro-brasileiro-de-qualificacoes-qbq
# Downloaded Sunday, July 13, 2025

# From AWS S3 bucket

library(aws.s3)
library(dotenv)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(janitor)

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
  df_cnct2025a = "D:/Country/Brazil/TechBrazil/working/mec_outros/df_cnct2025a.rds",
  df_censo_supl_tec23 = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_censo_supl_tec23.rda",
  df_censo_supl_tec24 = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_censo_supl_tec24.rda",
  qbq_ocup = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup.rda"
)

s3_keys <- list(
  df_cnct2025a = "working/mec_outros/df_cnct2025a.rds",
  df_censo_supl_tec23 = "working/mec_inep/df_censo_supl_tec23.rda",
  df_censo_supl_tec24 = "working/mec_inep/df_censo_supl_tec24.rda",
  qbq_ocup = "working/qbq/qbq_ocup.rda"
)


# use purrr::walk2 to check and download if missing

walk2(paths, s3_keys, ~update_data_from_s3(.x, .y, bucket_name))

# Load the data into R
df_cnct2025a <- readRDS(paths$df_cnct2025a)

load(paths$df_censo_supl_tec23, envir = .GlobalEnv)  # Should load object(s) inside .rda
load(paths$df_censo_supl_tec24, envir = .GlobalEnv)
load(paths$qbq_ocup, envir = .GlobalEnv)


# lets combine the 23 and 24 data with a simple rbing
df_censo_supl_tec <- bind_rows(df_censo_supl_tec23, df_censo_supl_tec24)


# get some variables from cnct
df_cnct_ <- df_cnct2025a %>% select(course_id,`Eixo Tecnológico`,`Área Tecnológica`,`Denominação do Curso`,`Perfil Profissional de Conclusão`,
                                    `Campo de Atuação`, `Ocupações CBO Associadas`,`Infraestrutura Mínima`,eixo_code,area_code,curso_code) %>%
        relocate(eixo_code, area_code, curso_code, .after = course_id)



                                    
   



