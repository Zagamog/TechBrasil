library(tidyverse)
library(readxl)
library(here)
library(deflateBR)
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

# --- Step 3: File paths ---
input_fundeb <- here("rawdata", "fundeb")
input_mec <- here("working", "mec_inep")
output_folder <- here("working", "fundeb")
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

file_names <- c("pge_fundeb_2016.xls","pge_fundeb_2017.xls", "Fundeb 2018.xls", "Fundeb 2019.xls", "Fundeb 2020.xls", 
                "Fundeb 2021.xls", "Fundeb 2022.xls", "Fundeb 2023.xls", "Fundeb 2024.xls")

# --- Step 4: Process one year of supplement and save ---
processar_fundeb_censo_ano <- function(ano) {
  message("📥 Processando FUNDEB: ", ano)

#Import original FUNDEB files from tesouro transparente    
  local_path <- file.path(input_fundeb, paste0(if_else(ano < 2018, "pge_fundeb_", "Fundeb "), ano, ".xls"))
  s3_path <- paste0("rawdata/mec_inep/", if_else(ano < 2018, "pge_fundeb_", "Fundeb "), ano, ".xls")
  update_data_from_s3(local_path, s3_path, bucket_name)
  
  if (!file.exists(local_path)) stop("❌ Arquivo não encontrado: ", local_path)
  
  df_proc_fundeb <- read_xls(local_path, #caminhos das tabelas xls importadas do Tesouro Transparente
                            sheet = "E_TOTAL",
                            skip = 7) %>%
    filter(!is.na(UF)) %>%
    rename(NM_UF = ESTADOS,
           SG_UF = UF,
           FUNDEB_TOTAL_EST_UF = TOTAL) %>%
    mutate(ANO = ano) 

#Import and process censo escolar matriculas prof and med
  message("📥 Processando Censo Escolar: ", ano)
local_path <- file.path(input_mec, paste0("censo_escolar_", ano, ".rda"))
s3_path <- paste0("working/mec_inep/censo_escolar_", ano, ".rda")
update_data_from_s3(local_path, s3_path, bucket_name)

if (!file.exists(local_path)) stop("❌ Arquivo não encontrado: ", local_path)
#Prepare censo escolar dataset to merge it with FUNDEB
# Build last two digits of year
yy <- substr(as.character(ano), 3, 4)

# Load .rda file
load(here::here("working", "mec_inep", paste0("censo_escolar_", ano, ".rda"))) 

# Get the object by name
df_name <- paste0("df_censo", yy)
df <- get(df_name)

# Process
df_proc_mec <- df %>%
  # --- filter only state managed schools  (TP_DEPENDENCIA == 2)
  filter(TP_DEPENDENCIA == 2) %>%
  #--- Aggregate at year UF level #We are not considering fatores de ponderação for simplicity
    group_by(NU_ANO_CENSO, NO_UF, SG_UF, CO_UF) %>%
    summarise(
      across(.cols = c(QT_MAT_PROF, QT_MAT_MED, QT_MAT_BAS),
             .fns = ~ sum(.x, na.rm = TRUE))
    ) %>%
    # --- Add derived variable: CREATE MATRICULAS SHARE
    mutate(
      QT_MAT_PROF_SHARE = QT_MAT_PROF/QT_MAT_BAS,
      QT_MAT_MED_SHARE = QT_MAT_MED/QT_MAT_BAS
    ) %>%
    ungroup()
  
#Join both datasets
df_proc_fundeb_censo <- left_join(df_proc_fundeb, df_proc_mec,
                                  by = c("ANO" = "NU_ANO_CENSO", 
                                         "NM_UF" = "NO_UF",
                                         "SG_UF")) %>%
  # --- Add derived variable: FUNDEB_PROF (FUNDEB_MED) = TOTAL FUNDEB * SHARE OF EPT (EM) ENROLLMENT as estimate of FUNDEB flow for that type of education
  mutate(
    FUNDEB_PROF_EST_UF = FUNDEB_TOTAL_EST_UF*QT_MAT_PROF_SHARE,
    FUNDEB_MED_EST_UF = FUNDEB_TOTAL_EST_UF*QT_MAT_MED_SHARE
  ) %>%
  # --- Deflate monetary values to december 2024 using INPC
  mutate(
    date = as.Date(paste0(ANO, "-12-01")), # considering december as the nominal month for 2024 value not to be changed,
    
    across(.cols = contains("fundeb"),
           .fns = ~ deflate(
             nominal_values = .x,
             index = "inpc",
             nominal_date = date,
             real_date = "11/2024"), #deflating to real november 2024 values, which keep 2024 financial values as nominal
           .names = "{.col}_real")
  ) %>%
  # --- Select variables of interest
  select(ANO, NM_UF, SG_UF, CO_UF, FUNDEB_TOTAL_EST_UF, FUNDEB_PROF_EST_UF, FUNDEB_MED_EST_UF)
  
  # --- Save as .rda with appropriate object name
  obj_name <- paste0("fundeb_fluxos_uf_", ano)
  assign(obj_name, df_proc_fundeb_censo)

  local_rda <- here::here("working", "fundeb", paste0(obj_name, ".rda"))
  dir.create(dirname(local_rda), showWarnings = FALSE, recursive = TRUE)
  save(list = obj_name, file = local_rda)

  # --- Upload to S3 if changed
  s3_key <- paste0("working/fundeb/fundeb_fluxos_uf_", ano, ".rda")
  tryCatch({
  upload_if_missing_or_changed(local_rda, s3_key, bucket_name)
}, error = function(e) {
  warning("❌ Upload failed for ", ano, ": ", e$message)
})
 # list(success = success_years, failed = failed_years)
}

purrr::walk(.x = c(2016:2024),
            .f = processar_fundeb_censo_ano)


