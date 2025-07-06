# prepara_censo_escolar_tecnico.R
# Processa suplemento de cursos técnicos (2023–2024) com base no padrão INEP

library(here)
library(tidyverse)
library(readr)
library(aws.s3)
library(dotenv)
library(digest)

# --- Step 1: Load AWS credentials ---
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

# --- Step 2: Utilities ---
update_data_from_s3 <- function(local_path, s3_path, bucket) {
  if (!file.exists(local_path)) {
    tryCatch({
      save_object(object = s3_path, bucket = bucket, file = local_path)
      message("✅ Downloaded from S3: ", local_path)
    }, error = function(e) {
      message("❌ Failed to download from S3: ", s3_path, " — ", e$message)
    })
  } else {
    message("✅ Using local version: ", local_path)
  }
}

upload_if_different <- function(local_path, s3_path, bucket) {
  tmpfile <- tempfile()
  tryCatch({
    save_object(object = s3_path, bucket = bucket, file = tmpfile)
    local_hash <- digest::digest(file = local_path, algo = "md5")
    remote_hash <- digest::digest(file = tmpfile, algo = "md5")
    if (local_hash != remote_hash) {
      put_object(file = local_path, object = s3_path, bucket = bucket)
      message("✅ Updated on S3: ", s3_path)
    } else {
      message("ℹ️ No update needed. Same content on S3: ", s3_path)
    }
  }, error = function(e) {
    put_object(file = local_path, object = s3_path, bucket = bucket)
    message("✅ Uploaded new file to S3: ", s3_path)
  }, finally = {
    unlink(tmpfile)
  })
}

# --- Step 3: File paths ---
input_folder <- here("rawdata", "mec_inep")
output_folder <- here("working", "mec_inep")
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# --- Step 4: Process one year of supplement and save ---
processar_suplemento_ano <- function(ano) {
  message("📥 Processando suplemento técnico: ", ano)
  
  local_path <- file.path(input_folder, paste0("suplemento_cursos_tecnicos_", ano, ".csv"))
  s3_path <- paste0("rawdata/mec_inep/suplemento_cursos_tecnicos_", ano, ".csv")
  update_data_from_s3(local_path, s3_path, bucket_name)
  
  if (!file.exists(local_path)) stop("❌ Arquivo não encontrado: ", local_path)
  
  df_proc <- read_csv2(local_path, locale = locale(encoding = "ISO-8859-1")) %>%
    rename(CO_MUN = CO_MUNICIPIO, ANO = NU_ANO_CENSO) %>%
    select(CO_MUN, CO_ENTIDADE, NO_ENTIDADE, ANO,TP_DEPENDENCIA,
           NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL,
           starts_with("QT_")) %>%
    mutate(across(starts_with("QT_"), ~replace_na(., 0)))
  
  obj_name <- paste0("df_censo_supl_tec", substr(ano, 3, 4))
  assign(obj_name, df_proc, envir = .GlobalEnv)
  
  rda_path <- file.path(output_folder, paste0(obj_name, ".rda"))
  save(list = obj_name, file = rda_path)
  
  s3_key <- paste0("working/mec_inep/", obj_name, ".rda")
  upload_if_different(rda_path, s3_key, bucket_name)
}

# --- Step 5: Processar 2023 e 2024 separadamente ---
processar_suplemento_ano(2023)
processar_suplemento_ano(2024)
