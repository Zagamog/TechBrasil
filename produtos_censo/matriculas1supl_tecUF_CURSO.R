# matriculas1supl_tecUF_CURSO.R

library(tidyverse)
library(here)
library(aws.s3)
library(dotenv)
library(digest)

# --- Setup ---
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

# --- Load data and IBGE codes ---
load(here("working", "mec_inep", "df_censo_supl_tec23.rda"))
load(here("working", "mec_inep", "df_censo_supl_tec24.rda"))
load(here("working", "ibge", "df_codes_ibge.rda"))

df_supl <- bind_rows(df_censo_supl_tec23, df_censo_supl_tec24)

df_geo <- df_codes_ibge %>%
  select(CO_MUN, SG_UF, NM_UF, CO_RGINTM, NM_RGIINTM) %>%
  distinct()

df_supl <- df_supl %>%
  left_join(df_geo, by = "CO_MUN")

# --- Aggregate by area (UF level) ---
df_supltec_uf_area <- df_supl %>%
  group_by(ANO, SG_UF, NM_UF, NO_AREA_CURSO_PROFISSIONAL) %>%
  summarise(QT_MAT_CURSO_TEC = sum(QT_MAT_CURSO_TEC, na.rm = TRUE), .groups = "drop") %>%
  arrange(ANO, SG_UF, NO_AREA_CURSO_PROFISSIONAL)

# --- Aggregate by course (UF level) ---
df_supltec_uf_curso <- df_supl %>%
  group_by(ANO, SG_UF, NM_UF, NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL) %>%
  summarise(QT_MAT_CURSO_TEC = sum(QT_MAT_CURSO_TEC, na.rm = TRUE), .groups = "drop") %>%
  arrange(ANO, SG_UF, NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL)

# --- Save each to a separate .rda ---
save(df_supltec_uf_area, file = "working/mec_inep/df_supltec_uf_area.rda")
save(df_supltec_uf_curso, file = "working/mec_inep/df_supltec_uf_curso.rda")

# --- Upload to S3 ---
upload_if_missing_or_changed("working/mec_inep/df_supltec_uf_area.rda", "working/mec_inep/df_supltec_uf_area.rda", bucket_name)
upload_if_missing_or_changed("working/mec_inep/df_supltec_uf_curso.rda", "working/mec_inep/df_supltec_uf_curso.rda", bucket_name)


