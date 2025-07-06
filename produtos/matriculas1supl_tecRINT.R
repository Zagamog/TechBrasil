# matriculas1supl_tecRINT.R

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

# --- Load geo data and input suplemento ---
load("working/ibge/df_codes_ibge.rda")
geo_vars <- df_codes_ibge %>% 
  select(CO_MUN, SG_UF, NM_UF, CO_RGINTM, NM_RGIINTM) %>% distinct()

load("working/mec_inep/df_censo_supl_tec23.rda")
load("working/mec_inep/df_censo_supl_tec24.rda")

supltec_dfs <- list(df_censo_supl_tec23, df_censo_supl_tec24)
names(supltec_dfs) <- c("2023", "2024")

qt_vars_all <- names(df_censo_supl_tec24) %>%
  keep(~ startsWith(.x, "QT_"))

# --- TUDO ---
df_tudo_rgintm <- map2_dfr(supltec_dfs, names(supltec_dfs), function(df, ano) {
  df %>%
    left_join(geo_vars, by = "CO_MUN") %>%
    group_by(SG_UF, NM_UF, CO_RGINTM, NM_RGIINTM) %>%
    summarise(across(all_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(ANO = as.integer(ano), AGREG = "RGINTM_TUDO")
})

# --- REDE ---
tp_dependencias <- list(
  "RGINTM_REDEST" = 1,
  "RGINTM_REDMUN" = 2,
  "RGINTM_REDFED" = 3,
  "RGINTM_REDPRI" = 4
)

df_rede_rgintm <- map_dfr(names(tp_dependencias), function(rede) {
  tp_val <- tp_dependencias[[rede]]
  map2_dfr(supltec_dfs, names(supltec_dfs), function(df, ano) {
    df %>%
      filter(TP_DEPENDENCIA == tp_val) %>%
      left_join(geo_vars, by = "CO_MUN") %>%
      group_by(SG_UF, NM_UF, CO_RGINTM, NM_RGIINTM) %>%
      summarise(across(all_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(ANO = as.integer(ano), AGREG = rede)
  })
})

# --- PÚBLICA (FED + EST + MUN) ---
df_pub_rgintm <- df_rede_rgintm %>%
  filter(AGREG %in% c("RGINTM_REDFED", "RGINTM_REDEST", "RGINTM_REDMUN")) %>%
  group_by(SG_UF, NM_UF, CO_RGINTM, NM_RGIINTM, ANO) %>%
  summarise(across(all_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(AGREG = "RGINTM_PUB")

# --- Combina todos ---
df_supltec_RGINTM <- bind_rows(df_tudo_rgintm, df_rede_rgintm, df_pub_rgintm)

# --- Replace zero-only columns by year ---
replace_zero_cols_with_na_by_year <- function(df, vars_to_check) {
  map_dfr(unique(df$ANO), function(yr) {
    df_year <- df %>% filter(ANO == yr)
    cols_all_zero <- df_year %>%
      select(any_of(vars_to_check)) %>%
      summarise(across(everything(), ~ all(. == 0))) %>%
      pivot_longer(everything(), names_to = "var", values_to = "all_zero") %>%
      filter(all_zero) %>%
      pull(var)
    df_year %>% mutate(across(all_of(cols_all_zero), ~ na_if(., 0)))
  })
}

df_supltec_RGINTM <- replace_zero_cols_with_na_by_year(df_supltec_RGINTM, qt_vars_all)

# --- Save + upload ---
save(df_supltec_RGINTM, file = "working/mec_inep/df_supltec_RGINTM.rda")

tryCatch({
  upload_if_missing_or_changed("working/mec_inep/df_supltec_RGINTM.rda", "working/mec_inep/df_supltec_RGINTM.rda", bucket_name)
}, error = function(e) {
  warning("❌ Upload to S3 failed: ", e$message)
})
