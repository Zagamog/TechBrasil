# matriculas1RINM.R

library(dplyr)
library(purrr)
library(tidyr)

load("working/ibge/df_codes_ibge.rda")

# Geographic info for merge
geo_vars <- df_codes_ibge %>%
  select(CO_MUN, CO_UF, SG_UF, NM_UF, CO_RGIMED, NM_RGIMED) %>%
  distinct()

anos_validos <- 2007:2024

qt_vars_base <- c("QT_MAT_PROF_TEC", "QT_MAT_PROF", "QT_MAT_MED", "QT_MAT_BAS", "QT_MAT_FUND", 
                  "QT_MAT_EJA", "QT_MAT_INF", "QT_MAT_ESP")

qt_vars_new <- c("QT_MAT_PROF_TEC_SUBS", "QT_MAT_EJA_MED_TEC", 
                 "QT_MAT_EJA_FUND_FIC", "QT_MAT_EJA_MED_FIC",
                 "QT_MAT_PROF_TEC_MED","QT_MAT_MED_CT")

qt_vars_extra <- c("QT_MAT_PROF_TEC_PROPAG", "QT_MAT_EJA_ARTIC_EPT", "QT_MAT_MED_NM")

qt_vars_all <- unique(c(qt_vars_base, qt_vars_new, qt_vars_extra))

# ---- Process each year and aggregate by UF ----
df_tudos_aggRIME <- map_dfr(anos_validos, function(ano) {
  short <- substr(as.character(ano), 3, 4)
  file <- file.path("working/mec_inep", paste0("df_censo", short, ".rda"))
  if (!file.exists(file)) return(NULL)
  
  load(file)
  df <- get(paste0("df_censo", short))
  
  df <- df %>%
    select(-c(SG_UF, CO_UF, NO_UF, NO_MUNICIPIO)) %>%
    rename(CO_MUN = CO_MUNICIPIO) %>%
    mutate(ANO = ano) %>%
    left_join(geo_vars, by = "CO_MUN")
  
  # Ensure all expected vars exist — add missing as NA
  missing_vars <- setdiff(qt_vars_all, names(df))
  df[missing_vars] <- NA
  
  # Fallback for QT_MAT_MED_NM
  if (!"QT_MAT_MED_NM" %in% names(df) || all(is.na(df$QT_MAT_MED_NM))) {
    if (all(c("QT_MAT_PROF", "QT_MAT_PROF_TEC") %in% names(df))) {
      df$QT_MAT_MED_NM <- df$QT_MAT_PROF - df$QT_MAT_PROF_TEC
    } else {
      df$QT_MAT_MED_NM <- NA
    }
  }
  
  # Constructed vars (2023+ only, NA otherwise)
  if (all(c("QT_MAT_PROF_TEC", "QT_MAT_MED_NM") %in% names(df))) {
    df$QT_MAT_PROF_TEC_PROPAG <- df$QT_MAT_PROF_TEC - df$QT_MAT_MED_NM
  }
  
  if (all(c("QT_MAT_EJA_FUND_FIC", "QT_MAT_EJA_MED_FIC") %in% names(df))) {
    df$QT_MAT_EJA_ARTIC_EPT <- df$QT_MAT_EJA_FUND_FIC + df$QT_MAT_EJA_MED_FIC
  }
  
  # Fill only base vars with 0, not newer ones
  df <- df %>%
    mutate(across(any_of(qt_vars_base), ~replace_na(., 0)))
  
  df %>%
    mutate(ANO = ano) %>%
    group_by(SG_UF, NM_UF,CO_RGIMED, NM_RGIMED,ANO) %>%
    summarise(across(any_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(ANO = ano, AGREG = "RIME_TUDO")
})

#Identify columns with only zeros
cols_all_zero <- df_tudos_aggRIME %>%
  select(any_of(qt_vars_all)) %>%
  summarise(across(everything(), ~ all(. == 0))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "all_zero") %>%
  filter(all_zero) %>%
  pull(var)

# Replace 0s with NA in those columns
df_tudos_aggRIME <- df_tudos_aggRIME %>%
  mutate(across(all_of(cols_all_zero), ~ na_if(., 0)))





tp_dependencias <- list(
  "UF_REDEST" = 1,
  "UF_REDMUN" = 2,
  "UF_REDFED" = 3,
  "UF_REDPRI" = 4
)

df_red_aggRIME <- map_dfr(names(tp_dependencias), function(red_name) {
  tp_val <- tp_dependencias[[red_name]]
  
  map_dfr(anos_validos, function(ano) {
    short <- substr(as.character(ano), 3, 4)
    file <- file.path("working/mec_inep", paste0("df_censo", short, ".rda"))
    if (!file.exists(file)) return(NULL)
    
    load(file)
    df <- get(paste0("df_censo", short))
    
    df <- df %>%
      select(-c(SG_UF, CO_UF, NO_UF, NO_MUNICIPIO)) %>%
      rename(CO_MUN = CO_MUNICIPIO) %>%
      mutate(ANO = ano) %>%
      left_join(geo_vars, by = "CO_MUN")
    
    # Skip if TP_DEPENDENCIA not present
    if (!"TP_DEPENDENCIA" %in% names(df)) return(NULL)
    
    df <- df %>% filter(TP_DEPENDENCIA == tp_val)
    
    # Add missing vars as NA
    missing_vars <- setdiff(qt_vars_all, names(df))
    df[missing_vars] <- NA
    
    # Constructed fields
    if (!"QT_MAT_MED_NM" %in% names(df) || all(is.na(df$QT_MAT_MED_NM))) {
      if (all(c("QT_MAT_PROF", "QT_MAT_PROF_TEC") %in% names(df))) {
        df$QT_MAT_MED_NM <- df$QT_MAT_PROF - df$QT_MAT_PROF_TEC
      } else {
        df$QT_MAT_MED_NM <- NA
      }
    }
    if (all(c("QT_MAT_PROF_TEC", "QT_MAT_MED_NM") %in% names(df))) {
      df$QT_MAT_PROF_TEC_PROPAG <- df$QT_MAT_PROF_TEC - df$QT_MAT_MED_NM
    }
    if (all(c("QT_MAT_EJA_FUND_FIC", "QT_MAT_EJA_MED_FIC") %in% names(df))) {
      df$QT_MAT_EJA_ARTIC_EPT <- df$QT_MAT_EJA_FUND_FIC + df$QT_MAT_EJA_MED_FIC
    }
    
    # Fill only base vars with 0
    df <- df %>% mutate(across(any_of(qt_vars_base), ~replace_na(., 0)))
    
    df %>%
      group_by(SG_UF, NM_UF,CO_RGIMED, NM_RGIMED) %>%
      summarise(across(any_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(ANO = ano, AGREG = red_name)
  })
})



# Step 1: Aggregate UF-level PUBLIC data
df_pub_aggRIME <- df_red_aggRIME %>%
  filter(AGREG %in% c("UF_REDFED", "UF_REDMUN", "UF_REDEST")) %>%
  group_by(SG_UF, NM_UF,CO_RGIMED, NM_RGIMED,ANO) %>%
  summarise(across(any_of(qt_vars_all), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(AGREG = "RIME_PUB")

# Step 2: Identify columns that are entirely zero (across all rows)
cols_all_zero <- df_pub_aggRIME %>%
  select(any_of(qt_vars_all)) %>%
  summarise(across(everything(), ~ all(. == 0))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "all_zero") %>%
  filter(all_zero) %>%
  pull(var)

# Step 3: Replace 0 with NA only in those columns
df_pub_aggRIME <- df_pub_aggRIME %>%
  mutate(across(all_of(cols_all_zero), ~ na_if(., 0)))



# Fix 0s to NAs for new variables


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

df_tudos_aggRIME <- replace_zero_cols_with_na_by_year(df_tudos_aggRIME, qt_vars_all)
df_red_aggRIME     <- replace_zero_cols_with_na_by_year(df_red_aggRIME, qt_vars_all)
df_pub_aggRIME      <- replace_zero_cols_with_na_by_year(df_pub_aggRIME, qt_vars_all)


# Combine all aggregated data frames into one
df_censo_RIME <- bind_rows(df_tudos_aggRIME, df_red_aggRIME, df_pub_aggRIME)

save(df_censo_RIME, file = "working/mec_inep/df_censo_RIME.rda")



