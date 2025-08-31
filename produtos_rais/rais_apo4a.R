# rais_apo4a.R

# Says APO but actually has nothing to do with apo
# Extarct dfs to show municipal jobs by cbo in Shiny App

library(dplyr)
library(data.table)

load("working/rais/2023/cubes_2023.rda")
load("working/rais/2024/cubes_2024.rda")
load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")
load("D:/Country/Brazil/TechBrazil/working/pnad/df_pnadc_occ.rda")


# Dedicated geo keys for informality analysis  
dft_informality_geo_codes <- as.data.table(df_codes_ibge)[
  , .(CO_MUN6, CO_MUN, SG_UF, NM_UF, CO_UF, NM_MUN,
      CO_RGIMED, NM_RGIMED, CO_RGINTM, NM_RGIINTM)
]
dft_informality_geo_codes <- unique(dft_informality_geo_codes, by = "CO_MUN6")

create_cbocod_mun <- function(target_year) {
  
  # Step 1: Create formality lookup for target year
  formality_lookup <- df_pnadc_occ %>%
    filter(ANO == target_year) %>%
    select(NM_UF, CBO4, Taxa_Formalidade) %>%
    rename(cbo_4dig = CBO4)
  
  # Step 2: Calculate PNAD state shares for target year
  pnad_state_shares <- df_pnadc_occ %>%
    filter(ANO == target_year) %>%
    group_by(NM_UF) %>%
    mutate(cbo_share_pnad = Ocupado / sum(Ocupado)) %>%
    select(NM_UF, CBO4, cbo_share_pnad) %>%
    rename(cbo_4dig = CBO4)
  
  # Step 3: Calculate RAIS state shares (select appropriate dataset by year)
  employment_uf_data <- if(target_year == 2023) employment_uf_2023 else employment_uf_2024
  employment_mun_data <- if(target_year == 2023) employment_2023 else employment_2024
  
  rais_state_shares <- employment_uf_data %>%
    merge(dft_informality_geo_codes[, .(SG_UF, NM_UF)] %>% unique(), by = "SG_UF") %>%
    group_by(NM_UF) %>%
    mutate(cbo_share_rais = N / sum(N)) %>%
    select(NM_UF, cbo_4dig, cbo_share_rais)
  
  # Step 4: Municipal calculation
  df_result <- employment_mun_data %>%
    merge(dft_informality_geo_codes[, .(CO_MUN6, NM_UF, SG_UF, NM_MUN)], by = "CO_MUN6") %>%
    merge(formality_lookup, by = c("NM_UF", "cbo_4dig"), all.x = TRUE) %>%
    merge(pnad_state_shares, by = c("NM_UF", "cbo_4dig"), all.x = TRUE) %>%
    merge(rais_state_shares, by = c("NM_UF", "cbo_4dig"), all.x = TRUE) %>%
    mutate(
      year = target_year,
      vinculos_formais = N,
      vinculos_total = round(case_when(
        Taxa_Formalidade > 0 ~ N / Taxa_Formalidade,
        Taxa_Formalidade == 0 & !is.na(cbo_share_pnad) & !is.na(cbo_share_rais) ~ 
          N * (cbo_share_pnad / cbo_share_rais),
        TRUE ~ N
      )),
      vinculos_informais = vinculos_total - vinculos_formais
    )
  
  # Print summary
  cat(target_year, "Municipal data rows:", nrow(df_result), "\n")
  cat("Zero formality cases handled:", sum(df_result$Taxa_Formalidade == 0, na.rm = TRUE), "\n")
  
  return(df_result)
}

# Apply to both years
df_cbocod_mun23 <- create_cbocod_mun(2023)
df_cbocod_mun24 <- create_cbocod_mun(2024)

df_cbocod_mun23 <- df_cbocod_mun23 %>% select(-c(5,8,9,10,13))
df_cbocod_mun24 <- df_cbocod_mun24 %>% select(-c(5,8,9,10,13))


save(df_cbocod_mun23, file = "working/rais/df_cbocod_mun23.rda")
save(df_cbocod_mun24, file = "working/rais/df_cbocod_mun24.rda")



we can drop 5 8 9 10 13


