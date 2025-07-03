# prepara_censo_escolar.R
# Processa microdados do Censo Escolar 2007–2024 a partir de CSVs locais

library(tidyverse)
library(here)
library(aws.s3)
library(dotenv)
library(digest)

# --- Load AWS credentials ---
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

# --- S3 upload helper ---
upload_if_missing_or_changed <- function(local_path, s3_key, bucket) {
  if (!file.exists(local_path)) stop("❌ Local file not found: ", local_path)
  temp_s3 <- tempfile(fileext = ".rds")
  s3_exists <- tryCatch({
    save_object(object = s3_key, bucket = bucket, file = temp_s3)
    TRUE
  }, error = function(e) FALSE)
  
  if (!s3_exists) {
    message("☁️ Not found on S3 — uploading: ", s3_key)
    put_object(file = local_path, object = s3_key, bucket = bucket)
    return(TRUE)
  }
  
  local_hash <- digest(local_path, algo = "md5")
  s3_hash <- digest(temp_s3, algo = "md5")
  if (local_hash != s3_hash) {
    message("🔁 File changed — uploading: ", s3_key)
    put_object(file = local_path, object = s3_key, bucket = bucket)
  } else {
    message("✅ S3 version is up to date: ", s3_key)
  }
}


# --- Main processor ---
processar_csv_local <- function(anos, base_dir = here::here("rawdata", "mec_inep")) {
  anos <- as.integer(anos)
  failed_years <- c()
  success_years <- c()
  
  for (ano in anos) {
    message("📂 Processing year: ", ano)
    
    file_name <- paste0("microdados_ed_basica_", ano, ".csv")
    csv_path <- file.path(base_dir, file_name)
    
    if (!file.exists(csv_path)) {
      warning("❌ CSV not found: ", csv_path)
      failed_years <- c(failed_years, ano)
      next
    }
    
    # --- Read CSV ---
    df <- tryCatch({
      readr::read_csv2(csv_path, locale = locale(encoding = "ISO-8859-1"))
    }, error = function(e) {
      warning("❌ Failed to read: ", csv_path, " — ", e$message)
      failed_years <- c(failed_years, ano)
      return(NULL)
    })
    if (is.null(df)) next
    
    # --- Define base columns to keep ---
    cols_keep <- c(
      "NU_ANO_CENSO", "NO_UF", "SG_UF", "CO_UF", "NO_MUNICIPIO", "CO_MUNICIPIO",
      "NO_ENTIDADE", "CO_ENTIDADE", "TP_LOCALIZACAO", "TP_LOCALIZACAO_DIFERENCIADA",
      "TP_DEPENDENCIA", "IN_PODER_PUBLICO_PARCERIA", "IN_LABORATORIO_INFORMATICA",
      "IN_LABORATORIO_EDUC_PROF", "IN_SALA_OFICINAS_EDUC_PROF", "QT_DOC_BAS", "QT_DOC_MED",
      "QT_DOC_PROF", "QT_DOC_PROF_TEC", "IN_PROF_TEC", "QT_MAT_BAS", "QT_MAT_EJA",
      "QT_MAT_ESP", "QT_MAT_FUND", "QT_MAT_INF", "QT_MAT_MED", "QT_MAT_PROF_TEC",
      "QT_MAT_MED_NM", "QT_MAT_PROF_TEC_SUBS", "QT_MAT_EJA_MED_TEC",
      "QT_MAT_EJA_FUND_FIC", "QT_MAT_EJA_MED_FIC", "QT_MAT_PROF"
    )
    
    # --- Subset to available columns (defensive)
    df_proc <- df %>%
      select(any_of(cols_keep))
    
    # --- Create fallback for QT_MAT_MED_NM if missing
    if (!"QT_MAT_MED_NM" %in% names(df_proc)) {
      if (all(c("QT_MAT_PROF", "QT_MAT_PROF_TEC") %in% names(df_proc))) {
        df_proc$QT_MAT_MED_NM <- df_proc$QT_MAT_PROF - df_proc$QT_MAT_PROF_TEC
      } else {
        df_proc$QT_MAT_MED_NM <- 0
      }
    }
    
    # --- Replace NA with 0 for numeric columns (excluding IDs)
    df_proc <- df_proc %>%
      mutate(across(-c(NO_UF, NO_MUNICIPIO, NO_ENTIDADE), ~ tidyr::replace_na(., 0)))
    
    # --- Add derived variable: QT_MAT_PROF_TEC_PROPAG
    df_proc <- df_proc %>%
      mutate(QT_MAT_PROF_TEC_PROPAG = QT_MAT_PROF_TEC - QT_MAT_MED_NM)
    
    # --- Add new derived variable only for 2023 and 2024
    if (ano %in% c(2023, 2024)) {
      if (all(c("QT_MAT_EJA_FUND_FIC", "QT_MAT_EJA_MED_FIC", "QT_MAT_EJA_MED_TEC") %in% names(df_proc))) {
        df_proc <- df_proc %>%
          mutate(QT_MAT_EJA_ARTIC_EPT = QT_MAT_EJA_FUND_FIC + QT_MAT_EJA_MED_FIC + QT_MAT_EJA_MED_TEC)
      }
    }
    
    # --- Save as .rda with appropriate object name
    short_ano <- substr(as.character(ano), 3, 4)
    obj_name <- paste0("df_censo", short_ano)
    assign(obj_name, df_proc)
    
    local_rda <- here::here("working", "mec_inep", paste0(obj_name, ".rda"))
    dir.create(dirname(local_rda), showWarnings = FALSE, recursive = TRUE)
    save(list = obj_name, file = local_rda)
    
    # --- Upload to S3 if changed
    s3_key <- paste0("working/mec_inep/censo_escolar_", ano, ".rda")
    tryCatch({
      upload_if_missing_or_changed(local_rda, s3_key, bucket_name)
      success_years <- c(success_years, ano)
    }, error = function(e) {
      warning("❌ Upload failed for ", ano, ": ", e$message)
      failed_years <- c(failed_years, ano)
    })
  }
  
  list(success = success_years, failed = failed_years)
}




processar_csv_local(2007)           # Single year
processar_csv_local(2008:2023)     # All years
processar_csv_local(2024)  

