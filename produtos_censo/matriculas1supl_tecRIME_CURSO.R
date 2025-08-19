# matriculas1supl_tecRGIMED_CURSO.R
# Agrega matrículas por área e curso para Regiões Imediatas (RGIMED) – Suplemento Técnico

library(tidyverse)
library(here)
library(aws.s3)
library(dotenv)
library(digest)

dotenv::load_dot_env()
bucket_name <- "techbrazildata"

upload_if_missing_or_changed <- function(local_path, s3_key, bucket) {
  if (!file.exists(local_path)) stop("❌ Local file not found: ", local_path)
  temp_s3 <- tempfile(fileext = ".rda")
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

# --- Load IBGE metadata and suplemento técnico datasets ---
load(here("working", "ibge", "df_codes_ibge.rda"))
load(here("working", "mec_inep", "df_censo_supl_tec23.rda"))
load(here("working", "mec_inep", "df_censo_supl_tec24.rda"))

df_supl <- bind_rows(df_censo_supl_tec23, df_censo_supl_tec24) %>%
  left_join(df_codes_ibge %>%
              select(CO_MUN, SG_UF, NM_UF, CO_RGIMED, NM_RGIMED),
            by = "CO_MUN")

# --- Aggregate by area (RGIMED level) ---
df_supltec_rimed_area <- df_supl %>%
  group_by(ANO, SG_UF, NM_UF, CO_RGIMED, NM_RGIMED, NO_AREA_CURSO_PROFISSIONAL) %>%
  summarise(QT_MAT_CURSO_TEC = sum(QT_MAT_CURSO_TEC, na.rm = TRUE), .groups = "drop") %>%
  arrange(ANO, SG_UF, NM_RGIMED, NO_AREA_CURSO_PROFISSIONAL)

# --- Aggregate by course (RGIMED level) ---
df_supltec_rimed_curso <- df_supl %>%
  group_by(ANO, SG_UF, NM_UF, CO_RGIMED, NM_RGIMED, NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL) %>%
  summarise(QT_MAT_CURSO_TEC = sum(QT_MAT_CURSO_TEC, na.rm = TRUE), .groups = "drop") %>%
  arrange(ANO, SG_UF, NM_RGIMED, NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL)

# --- Save and upload each output separately ---
path_area <- here("working", "mec_inep", "df_supltec_rimed_area.rda")
path_curso <- here("working", "mec_inep", "df_supltec_rimed_curso.rda")

save(df_supltec_rimed_area, file = path_area)
save(df_supltec_rimed_curso, file = path_curso)

upload_if_missing_or_changed(path_area, "working/mec_inep/df_supltec_rimed_area.rda", bucket_name)
upload_if_missing_or_changed(path_curso, "working/mec_inep/df_supltec_rimed_curso.rda", bucket_name)
