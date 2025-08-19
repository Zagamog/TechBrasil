# matriculas1supl_tecRIME.R
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
geo_vars <- df_codes_ibge %>% select(CO_MUN, SG_UF, NM_UF, CO_RGIMED, NM_RGIMED) %>% distinct()

load("working/mec_inep/df_censo_supl_tec23.rda")
load("working/mec_inep/df_censo_supl_tec24.rda")

supltec_dfs <- list(df_censo_supl_tec23, df_censo_supl_tec24)
names(supltec_dfs) <- c("2023", "2024")

qt_vars_all <- names(df_censo_supl_tec24) %>%
  keep(~ startsWith(.x, "QT_"))

# --- TUDO aggregation ---
df_tudo_supltec_RIME <- map2_dfr(supltec_dfs, names(supltec_dfs), function(df, ano) {
  df %>%
    left_join(geo_vars, by = "CO_MUN") %>%
    group_by(SG_UF, NM_UF, CO_RGIMED, NM_RGIMED) %>%
    summarise(across(all_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(ANO = as.integer(ano), AGREG = "RIME_TUDO")
})

# --- REDE aggregation ---
tp_dependencias <- list(
  "RIME_REDEST" = 1,
  "RIME_REDMUN" = 2,
  "RIME_REDFED" = 3,
  "RIME_REDPRI" = 4
)

df_rede_supltec_RIME <- map_dfr(names(tp_dependencias), function(rede) {
  tp_val <- tp_dependencias[[rede]]
  map2_dfr(supltec_dfs, names(supltec_dfs), function(df, ano) {
    df %>%
      filter(TP_DEPENDENCIA == tp_val) %>%
      left_join(geo_vars, by = "CO_MUN") %>%
      group_by(SG_UF, NM_UF, CO_RGIMED, NM_RGIMED) %>%
      summarise(across(all_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(ANO = as.integer(ano), AGREG = rede)
  })
})

# --- PÚBLICA aggregation ---
df_pub_supltec_RIME <- df_rede_supltec_RIME %>%
  filter(AGREG %in% c("RIME_REDFED", "RIME_REDEST", "RIME_REDMUN")) %>%
  group_by(SG_UF, NM_UF, CO_RGIMED, NM_RGIMED, ANO) %>%
  summarise(across(all_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(AGREG = "RIME_PUB")

# --- Replace zero-only columns with NA ---
replace_zero_cols_with_na_by_year <- function(df, vars_to_check) {
  map_dfr(unique(df$ANO), function(yr) {
    df_year <- df %>% filter(ANO == yr)
    
    cols_all_zero <- df_year %>%
      select(any_of(vars_to_check)) %>%
      summarise(across(everything(), ~ all(. == 0))) %>%
      pivot_longer(everything(), names_to = "var", values_to = "all_zero") %>%
      filter(all_zero) %>%
      pull(var)
    
    df_year %>%
      mutate(across(all_of(cols_all_zero), ~ na_if(., 0)))
  })
}

df_tudo_supltec_RIME <- replace_zero_cols_with_na_by_year(df_tudo_supltec_RIME, qt_vars_all)
df_rede_supltec_RIME <- replace_zero_cols_with_na_by_year(df_rede_supltec_RIME, qt_vars_all)
df_pub_supltec_RIME  <- replace_zero_cols_with_na_by_year(df_pub_supltec_RIME, qt_vars_all)

# --- Combine and save ---
df_supltec_RIME <- bind_rows(df_tudo_supltec_RIME, df_rede_supltec_RIME, df_pub_supltec_RIME)

save(df_supltec_RIME, file = "working/mec_inep/df_supltec_RIME.rda")

tryCatch({
  upload_if_missing_or_changed("working/mec_inep/df_supltec_RIME.rda", "working/mec_inep/df_supltec_RIME.rda", bucket_name)
}, error = function(e) {
  warning("❌ Upload to S3 failed: ", e$message)
})
