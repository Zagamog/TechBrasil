# br_censo_tec.R
# Process Censo Escolar EPT technical course data (suplemento), handle AWS fallback, and upload outputs.

library(here)
library(tidyverse)
library(janitor)
library(readxl)
library(aws.s3)
library(dotenv)

# --- Step 1: Load AWS credentials ---
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

# --- Step 2: Utility: Download from S3 if local missing ---
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

# --- Step 3: Utility: Upload if file not already present or has changed ---
upload_if_not_exists <- function(local_path, s3_path, bucket) {
  if (!object_exists(object = s3_path, bucket = bucket)) {
    put_object(file = local_path, object = s3_path, bucket = bucket)
    message("✅ Uploaded to S3: ", s3_path)
  } else {
    message("ℹ️ File already exists on S3: ", s3_path)
  }
}

# --- Step 4: Paths ---
input_folder <- here("rawdata", "mec_inep")
output_folder <- here("working", "mec_inep")

# --- Step 5: Load df_codes_ibge ---
df_ibge_path <- here("working", "ibge", "df_codes_ibge.rda")
update_data_from_s3(df_ibge_path, "working/ibge/df_codes_ibge.rda", bucket_name)
load(df_ibge_path)  # loads df_codes_ibge

# --- Step 6: Import function with fallback ---
importa_censo_tec <- function(ano) {
  local_path <- here(input_folder, paste0("suplemento_cursos_tecnicos_", ano, ".csv"))
  s3_path <- paste0("rawdata/mec_inep/suplemento_cursos_tecnicos_", ano, ".csv")
  
  update_data_from_s3(local_path, s3_path, bucket_name)
  
  read_csv2(local_path, locale = locale(encoding = "ISO-8859-1")) %>%
    clean_names() %>%
    rename(
      ano = nu_ano_censo,
      sigla_uf = sg_uf,         # Keep only for early aggregation
      CO_MUN = co_municipio     # Final key used in merging
    ) %>%
    filter(tp_dependencia == 2) %>%
    select(ano, sigla_uf, CO_MUN,
           no_entidade, tp_dependencia,
           no_area_curso_profissional,
           no_curso_educ_profissional,
           qt_mat_curso_tec)
}

# --- Step 7: Load 2023/2024 datasets ---
censo_tec <- map_dfr(c(2023, 2024), importa_censo_tec)

# --- Step 8: Validation: compare with totals from another file ---
totals_path <- here("working", "mec_inep", "matriculas_total_em_ept_uf_20162024.csv")
update_data_from_s3(totals_path, "working/mec_inep/matriculas_total_em_ept_uf_20162024.csv", bucket_name)
matriculas_uf <- read_csv(totals_path)

censo_tec %>%
  group_by(sigla_uf, ano) %>%
  summarise(matriculas_tec = sum(qt_mat_curso_tec, na.rm = TRUE), .groups = "drop") -> confere

inner_join(
  select(matriculas_uf, ano, sigla_uf, matriculas_total_tecprof),
  confere,
  by = c("ano", "sigla_uf")
) -> compara

# --- Step 9: Aggregated table by município, área, curso ---
censo_tec %>%
  group_by(CO_MUN, ano, no_area_curso_profissional, no_curso_educ_profissional) %>%
  summarise(matriculas_tec = sum(qt_mat_curso_tec, na.rm = TRUE), .groups = "drop") -> matriculas_tec_ano_mun_area_curso

# --- Step 10: Merge with df_codes_ibge to get regional names ---
df_mergeable <- df_codes_ibge %>%
  select(CO_MUN, CO_UF, SG_UF, NM_UF, NM_MUN, NM_RGIMED, NM_RGIINTM)

matriculas_tec_ano_mun_area_curso <- left_join(
  matriculas_tec_ano_mun_area_curso,
  df_mergeable,
  by = "CO_MUN"
) %>%
  relocate(ano, CO_UF, SG_UF, NM_UF, NM_MUN, NM_RGIMED, NM_RGIINTM, .before = no_area_curso_profissional)

# --- Step 11: Save outputs (CSV and RDA) ---
csv_output <- file.path(output_folder, "matriculas_tec_ano_mun_area_curso.csv")
rda_output <- file.path(output_folder, "matriculas_tec_ano_mun_area_curso.rda")

write_csv(matriculas_tec_ano_mun_area_curso, csv_output)
save(matriculas_tec_ano_mun_area_curso, file = rda_output)

upload_if_not_exists(csv_output, "working/mec_inep/matriculas_tec_ano_mun_area_curso.csv", bucket_name)
upload_if_not_exists(rda_output, "working/mec_inep/matriculas_tec_ano_mun_area_curso.rda", bucket_name)

message("✅ Script completed and outputs saved/uploaded.")
