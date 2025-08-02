library(dplyr)

# Step 1: Filter matches and map to 5-digit IDX_EIXCUR
df <- cnct_qbq_matches %>%
  filter(final_score >= 0.2) %>%
  mutate(
    CodCBO      = as.character(CodCBO),
    eixo_code   = substr(IDX_EIXARECUR,1,2),
    curso_code  = substr(IDX_EIXARECUR,5,6),
    IDX_EIXCUR = paste0(eixo_code, sprintf("%03d", as.integer(curso_code)))
  ) %>%
  inner_join(qbq_ocup_cmento1, by = "CodCBO") %>%
  filter(NivelOcupacao %in% 3) %>%
  mutate(cbo_1dig = substr(CodCBO, 1, 1)) %>%
  inner_join(df_exarcu %>% select(IDX_EIXCUR, `Eixo Tecnológico`), by = "IDX_EIXCUR")

# Step 2: Bring in matriculas for a specific UF
df_mat_curso_filtered <- df_mat_curso_wide %>%
  filter(NM_UF == "São Paulo") %>%
  select(IDX_EIXCUR, `Matrículas 2023`, `Matrículas 2024`)

# Step 3: Join and aggregate
df_summary <- df %>%
  inner_join(df_mat_curso_filtered, by = "IDX_EIXCUR") %>%
  group_by(`Eixo Tecnológico`, cbo_1dig) %>%
  summarise(
    n_matches         = n(),
    avg_score         = mean(final_score),
    `Matrículas 2023` = sum(`Matrículas 2023`, na.rm = TRUE),
    `Matrículas 2024` = sum(`Matrículas 2024`, na.rm = TRUE),
    .groups           = "drop"
  ) %>%
  arrange(desc(`Matrículas 2024`))

View(df_summary)
library(dplyr)


# Step 1: Filter matches and map to 5-digit IDX_EIXCUR
df <- cnct_qbq_matches %>%
  filter(final_score >= 0.2) %>%
  mutate(
    CodCBO      = as.character(CodCBO),
    eixo_code   = substr(IDX_EIXARECUR,1,2),
    curso_code  = substr(IDX_EIXARECUR,5,6),
    IDX_EIXCUR  = paste0(eixo_code, sprintf("%03d", as.integer(curso_code)))
  ) %>%
  inner_join(qbq_ocup_cmento1, by = "CodCBO") %>%
  filter(NivelOcupacao %in% 3) %>%
  inner_join(df_exarcu %>% select(IDX_EIXCUR, `Eixo Tecnológico`), by = "IDX_EIXCUR") %>%
  mutate(cbo_1dig = substr(CodCBO, 1, 1))

# Step 2: Filter RAIS vínculos for a UF
df_rais_filtered <- rais_cbo6_uf24 %>%
  filter(CO_UF == "35") %>%  # São Paulo
  select(CodCBO, vinc_2023 = vinculos)  # Assuming only 2023 is available

# Step 3: Join and aggregate
df_summary_vinc <- df %>%
  inner_join(df_rais_filtered, by = "CodCBO") %>%
  group_by(`Eixo Tecnológico`, cbo_1dig) %>%
  summarise(
    n_matches  = n(),
    avg_score  = mean(final_score),
    vinc_2023  = sum(vinc_2023, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(desc(vinc_2023))

View(df_summary_vinc)



