##############################################################################################################
# EPT FORMALITY RATE DATA LOADING
##############################################################################################################



# Step 1: Create formality lookup for 2023
formality_lookup23 <- df_pnadc_occ %>%
  filter(ANO == 2023) %>%
  select(NM_UF, CBO4, Taxa_Formalidade) %>%
  rename(cbo_4dig = CBO4)

# Step 2: Calculate state shares for 2023
pnad_state_shares23 <- df_pnadc_occ %>%
  filter(ANO == 2023) %>%
  group_by(NM_UF) %>%
  mutate(cbo_share_pnad = Ocupado / sum(Ocupado)) %>%
  select(NM_UF, CBO4, cbo_share_pnad) %>%
  rename(cbo_4dig = CBO4)

rais_state_shares23 <- employment_uf_2023 %>%
  merge(dft_informality_geo_codes[, .(SG_UF, NM_UF)] %>% unique(), by = "SG_UF") %>%
  group_by(NM_UF) %>%
  mutate(cbo_share_rais = N / sum(N)) %>%
  select(NM_UF, cbo_4dig, cbo_share_rais)

# Step 3: Municipal calculation
df_cbocod_mun23 <- employment_2023 %>%
  merge(dft_informality_geo_codes[, .(CO_MUN6, NM_UF, SG_UF, NM_MUN)], by = "CO_MUN6") %>%
  merge(formality_lookup23, by = c("NM_UF", "cbo_4dig"), all.x = TRUE) %>%
  merge(pnad_state_shares23, by = c("NM_UF", "cbo_4dig"), all.x = TRUE) %>%
  merge(rais_state_shares23, by = c("NM_UF", "cbo_4dig"), all.x = TRUE) %>%
  mutate(
    year = 2023,
    vinculos_formais = N,  # Already whole numbers from RAIS
    vinculos_total = round(case_when(
      Taxa_Formalidade > 0 ~ N / Taxa_Formalidade,
      Taxa_Formalidade == 0 & !is.na(cbo_share_pnad) & !is.na(cbo_share_rais) ~ 
        N * (cbo_share_pnad / cbo_share_rais),
      TRUE ~ N
    )),
    vinculos_informais = vinculos_total - vinculos_formais  # Will be whole numbers after rounding
  )
# Check results
cat("2023 Municipal data rows:", nrow(df_cbocod_mun23), "\n")
cat("Zero formality cases handled:", sum(df_cbocod_mun23$Taxa_Formalidade == 0, na.rm = TRUE), "\n")


sum(df_cbocod_mun23$vinculos_total)
sum(df_cbocod_mun23$vinculos_formais)
sum(df_cbocod_mun23$vinculos_informais)

sum(df_pnadc_occ$Ocupado)
sum(df_pnadc_occ$Ocupado_Formal)
sum(df_pnadc_occ$Ocupado_Informal)





