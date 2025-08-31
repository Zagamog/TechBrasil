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


sum(df_cbocod_mun23$vinculos_total)
sum(df_cbocod_mun24$vinculos_formais)
sum(df_cbocod_mun23$vinculos_informais)
