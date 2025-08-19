# matriculas1supl_tecUF.R

library(dplyr)
library(purrr)
library(tidyr)
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

# --- Load data ---
load("working/ibge/df_codes_ibge.rda")
geo_vars <- df_codes_ibge %>% select(CO_MUN, SG_UF, NM_UF) %>% distinct()

# --- Load 2023 and 2024 suplemento data ---
load("working/mec_inep/df_censo_supl_tec23.rda")
load("working/mec_inep/df_censo_supl_tec24.rda")

supltec_dfs <- list(df_censo_supl_tec23, df_censo_supl_tec24)
names(supltec_dfs) <- c("2023", "2024")

qt_vars_all <- names(df_censo_supl_tec24) %>%
  keep(~ startsWith(.x, "QT_"))

# --- TUDO aggregation ---
df_tudo_supltec <- map2_dfr(supltec_dfs, names(supltec_dfs), function(df, ano) {
  df %>%
    left_join(geo_vars, by = "CO_MUN") %>%
    group_by(SG_UF, NM_UF) %>%
    summarise(across(all_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(ANO = as.integer(ano), AGREG = "UF_TUDO")
})

# --- REDE aggregation ---
tp_dependencias <- list(
  "UF_REDEST" = 1,
  "UF_REDMUN" = 2,
  "UF_REDFED" = 3,
  "UF_REDPRI" = 4
)

df_rede_supltec <- map_dfr(names(tp_dependencias), function(rede) {
  tp_val <- tp_dependencias[[rede]]
  map2_dfr(supltec_dfs, names(supltec_dfs), function(df, ano) {
    df %>%
      filter(TP_DEPENDENCIA == tp_val) %>%
      left_join(geo_vars, by = "CO_MUN") %>%
      group_by(SG_UF, NM_UF) %>%
      summarise(across(all_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(ANO = as.integer(ano), AGREG = rede)
  })
})

# --- PÚBLICA aggregation (sum of FED + EST + MUN) ---
df_pub_supltec <- df_rede_supltec %>%
  filter(AGREG %in% c("UF_REDFED", "UF_REDEST", "UF_REDMUN")) %>%
  group_by(SG_UF, NM_UF, ANO) %>%
  summarise(across(all_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(AGREG = "UF_PUB")

# --- Combine all ---
df_supltec_UF <- bind_rows(df_tudo_supltec, df_rede_supltec, df_pub_supltec)

# --- Save and upload ---
save(df_supltec_UF, file = "working/mec_inep/df_supltec_UF.rda")

tryCatch({
  upload_if_missing_or_changed("working/mec_inep/df_supltec_UF.rda", "working/mec_inep/df_supltec_UF.rda", bucket_name)
}, error = function(e) {
  warning("❌ Upload to S3 failed: ", e$message)
})

