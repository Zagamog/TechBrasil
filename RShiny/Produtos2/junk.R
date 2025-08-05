
# Simulate inputs
selected_uf     <- "Brasil"     # e.g., "SP", "MG", "Brasil", etc.
selected_cbo1dig <- "6"         # <- From cbo_1dig selection (e.g., "1")
score_thresh    <- 0.02         # <- Slider input

# STEP 1: Filter RAIS codes from selected UF + 1-digit CBO
valid_cbo_codes <- df_raisCodCBO_wide %>%
  mutate(
    CodCBO   = as.character(CodCBO),
    cbo_1dig = substr(CodCBO, 1, 1)
  ) %>%
  filter(
    NM_UF == selected_uf,
    cbo_1dig == selected_cbo1dig
  ) %>%
  pull(CodCBO) %>%
  unique()

valid_cbo_codes

# STEP 2: Filter match table for above CodCBOs and high scores
matched <- qbq_cnct_matches %>%
  filter(final_score >= score_thresh) %>%
  mutate(CodCBO = as.character(CodCBO)) %>%
  filter(CodCBO %in% valid_cbo_codes) %>%
  mutate(
    eixo_code   = substr(IDX_EIXARECUR, 1, 2),
    curso_code  = substr(IDX_EIXARECUR, 5, 6),
    IDX_EIXCUR  = paste0(eixo_code, sprintf("%03d", as.integer(curso_code)))
  )


# STEP 3: Join with ocupação metadata and filters

qbq_ocup_cmento1 <- qbq_ocup_cmento1 %>%
  mutate(
    NivelOcupacao = as.integer(NivelOcupacao)
  )

matched <- matched %>%
  inner_join(qbq_ocup_cmento1 %>% select(-cbo_1dig), by = "CodCBO") %>%
  filter(NivelOcupacao %in% c(1, 2, 3, 4, 5))  # Adjust if needed

# STEP 4: Join with Eixo/Área metadata (based on IDX_EIXCUR)
matched <- matched %>%
  inner_join(df_exarcu %>% select(IDX_EIXCUR, `Área Tecnológica`, `Eixo Tecnológico`, `Denominação do Curso`),
             by = "IDX_EIXCUR")

# STEP 5: Join with course enrollment for that UF
matched <- matched %>%
  left_join(
    df_mat_curso_wide %>%
      filter(NM_UF == selected_uf) %>%
      select(IDX_EIXCUR, `Matrículas 2023`, `Matrículas 2024`),
    by = "IDX_EIXCUR"
  )

# STEP 6: Deduplicate CodCBO + Course combinations
matched_deduped <- matched %>%
  distinct(CodCBO, IDX_EIXCUR, .keep_all = TRUE)

# STEP 7: Showsult
matched_deduped %>% select(`Denominação do Curso`,`Matrículas 2023`, final_score) %>% unique() %>% 
  arrange(desc(final_score))



