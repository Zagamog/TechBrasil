# prepara_censo_escolar_tecnico.R
# Process Censo Escolar EPT technical course data, validate totals, enrich with geo metadata, and upload results to S3.

# I tried to fix this code but realized it needs to be replaced - Suhas


library(here)
library(tidyverse)
library(janitor)
library(readxl)
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
df_ibge_path <- here("working", "ibge", "df_codes_ibge.rda")
totals_path <- here("working", "mec_inep", "matriculas_total_em_ept_uf_20162024.csv")

# --- Step 4: Load IBGE geo data and UF lookup ---
load(df_ibge_path)

uf_lookup <- df_codes_ibge %>%
  distinct(CO_UF, SG_UF)

# --- Step 5: Import EPT suplemento data with fallback ---
importa_censo_tec <- function(ano) {
  local_path <- here(input_folder, paste0("suplemento_cursos_tecnicos_", ano, ".csv"))
  s3_path <- paste0("rawdata/mec_inep/suplemento_cursos_tecnicos_", ano, ".csv")
  update_data_from_s3(local_path, s3_path, bucket_name)
  
  read_csv2(local_path, locale = locale(encoding = "ISO-8859-1")) %>%
    rename(
      ANO = NU_ANO_CENSO,
      CO_UF = CO_UF,
      CO_MUN = CO_MUNICIPIO
    ) %>%
    filter(TP_DEPENDENCIA == 2) %>%
    mutate(across(starts_with("QT_"), ~replace_na(., 0)))  # fill NA in all `qt_` vars with 0
}

# --- Step 6: Load and combine data for 2023 and 2024 ---
censo_tec <- map_dfr(c(2023, 2024), importa_censo_tec)


matriculas_uf <- read_csv(totals_path) %>%
  rename(SG_UF = sigla_uf) %>%
  left_join(uf_lookup, by = "SG_UF")

# --- Step 8: Validate totals by CO_UF and year ---
censo_tec %>%
  group_by(CO_UF, ANO) %>%
  summarise(matriculas_tec = sum(QT_MAT_CURSO_TEC, na.rm = TRUE), .groups = "drop") -> confere

inner_join(
  select(matriculas_uf, ano, CO_UF, matriculas_total_tecprof),
  confere,
  by = c("ano" = "ANO", "CO_UF" = "CO_UF")
) -> compara

# Optional: flag mismatches
mismatches <- compara %>%
  filter(MATRICULAS_TOTAL_TECPROF != MATRICULAS_TEC)

if (nrow(mismatches) > 0) {
  message("⚠️ Discrepancies found in total validation:")
  print(mismatches)
} else {
  message("✅ Totals match between processed data and official file.")
}


# Exemplo de agregação total por curso
matriculas_tec_ano_mun_area_curso <- censo_tec %>%
  group_by(CO_MUN, ANO, NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL) %>%
  summarise(across(starts_with("QT_"), sum, na.rm = TRUE), .groups = "drop")


# --- Step 10: Enrich with geo metadata from df_codes_ibge ---





# --- Step 11: Save and upload outputs ---
csv_output <- here("working", "mec_inep", "matriculas_tec_ano_mun_area_curso.csv")
rda_output <- here("working", "mec_inep", "matriculas_tec_ano_mun_area_curso.rda")

write_csv(matriculas_tec_ano_mun_area_curso, csv_output)
save(matriculas_tec_ano_mun_area_curso, file = rda_output)

upload_if_different(csv_output, "working/mec_inep/matriculas_tec_ano_mun_area_curso.csv", bucket_name)
upload_if_different(rda_output, "working/mec_inep/matriculas_tec_ano_mun_area_curso.rda", bucket_name)


