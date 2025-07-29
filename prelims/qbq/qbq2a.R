# qbq2a.R

# Matching qbq occupations with courses


# https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/quadro-brasileiro-de-qualificacoes-qbq
# Downloaded Sunday, July 13, 2025

# From AWS S3 bucket

library(aws.s3)
library(dotenv)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(janitor)
library(stringr)
library(stringi)
library(purrr)

# Load credentials
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

# Helper function to check/download if missing
update_data_from_s3 <- function(local_path, s3_key, bucket) {
  if (!file.exists(local_path)) {
    tryCatch({
      message("☁️ Downloading missing file from S3: ", s3_key)
      save_object(object = s3_key, bucket = bucket, file = local_path)
    }, error = function(e) {
      stop("❌ Failed to download from S3: ", s3_key, " — ", e$message)
    })
  } else {
    message("✅ Local version found: ", basename(local_path))
  }
}

# Load the data

paths <- list(
  df_cnct2025a = "D:/Country/Brazil/TechBrazil/working/mec_outros/df_cnct2025a.rds",
  df_censo_supl_tec23 = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_censo_supl_tec23.rda",
  df_censo_supl_tec24 = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_censo_supl_tec24.rda",
  qbq_ocup = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup.rda"
)

s3_keys <- list(
  df_cnct2025a = "working/mec_outros/df_cnct2025a.rds",
  df_censo_supl_tec23 = "working/mec_inep/df_censo_supl_tec23.rda",
  df_censo_supl_tec24 = "working/mec_inep/df_censo_supl_tec24.rda",
  qbq_ocup = "working/qbq/qbq_ocup.rda"
)


# use purrr::walk2 to check and download if missing

walk2(paths, s3_keys, ~update_data_from_s3(.x, .y, bucket_name))

# Load the data into R
df_cnct2025a <- readRDS(paths$df_cnct2025a)

load(paths$df_censo_supl_tec23, envir = .GlobalEnv)  # Should load object(s) inside .rda
load(paths$df_censo_supl_tec24, envir = .GlobalEnv)
load(paths$qbq_ocup, envir = .GlobalEnv)


# lets combine the 23 and 24 data with a simple rbind ;a nd drop the Curso Normal that is not EPT but by mitake is in data
df_censo_supl_tec <- bind_rows(df_censo_supl_tec23, df_censo_supl_tec24) %>%
  filter(NO_CURSO_EDUC_PROFISSIONAL != "Ensino Médio - Curso Normal/Magistério")


# get some variables from cnct
df_cnct_ <- df_cnct2025a %>% select(IDX_EIXARECUR,`Eixo Tecnológico`,`Área Tecnológica`,`Denominação do Curso`,`Perfil Profissional de Conclusão`,
                                    `Campo de Atuação`, `Ocupações CBO Associadas`,`Infraestrutura Mínima`,eixo_code,area_code,curso_code) %>%
        relocate(eixo_code, area_code, curso_code, .after = IDX_EIXARECUR) 


### MERGING

normalize_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_squish() %>%
    stri_trans_general("Latin-ASCII")
}



# --- Step 1: Clean and prepare CENSO ---
df_censo_cleaned <- df_censo_supl_tec %>%
  distinct(NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL) %>%
  mutate(
    eixo_cleaned = normalize_text(NO_AREA_CURSO_PROFISSIONAL),
    curso_cleaned = normalize_text(NO_CURSO_EDUC_PROFISSIONAL)
  )

# --- Step 2: Clean and prepare CNCT ---
df_cnct_cleaned <- df_cnct_ %>%
  distinct(`Eixo Tecnológico`, `Denominação do Curso`) %>%
  mutate(
    eixo_cleaned = normalize_text(`Eixo Tecnológico`),
    curso_cleaned = `Denominação do Curso` %>%
      str_remove(regex("^t[eé]cnico em\\s+", ignore_case = TRUE)) %>%
      normalize_text()
  )

# --- Step 3: Create unified lookup table with consistent IDs ---
df_lookup <- bind_rows(
  df_censo_cleaned %>% select(eixo_cleaned, curso_cleaned),
  df_cnct_cleaned %>% select(eixo_cleaned, curso_cleaned)
) %>%
  distinct() %>%
  arrange(eixo_cleaned, curso_cleaned) %>%
  group_by(eixo_cleaned) %>%
  mutate(
    IDX_EIX = sprintf("%02d", cur_group_id()),  # índice por eixo
    IDX_CUR = sprintf("%03d", row_number())     # índice dentro do eixo
  ) %>%
  ungroup()


df_lookup <- df_lookup %>%
  mutate(IDX_EIXCUR = paste0(IDX_EIX, IDX_CUR))


# --- Step 4: Join index into both datasets ---
df_censo_cleaned <- df_censo_cleaned %>%
  left_join(df_lookup, by = c("eixo_cleaned", "curso_cleaned")) %>% arrange(IDX_EIXCUR)

df_cnct_cleaned <- df_cnct_cleaned %>%
  left_join(df_lookup, by = c("eixo_cleaned", "curso_cleaned")) 

# --- Step 5: Perform exact match on cleaned keys ---
df_matched <- df_censo_cleaned %>%
  inner_join(df_cnct_cleaned, by = c("eixo_cleaned", "curso_cleaned", "IDX_EIX", "IDX_CUR", "IDX_EIXCUR")) %>% arrange(IDX_EIXCUR)

# --- Step 7: Unmatched records ---

df_unmatched_censo <- df_censo_cleaned %>%
  anti_join(df_matched, by = c("eixo_cleaned", "curso_cleaned", "IDX_EIX", "IDX_CUR", "IDX_EIXCUR")) %>%
  arrange(IDX_EIXCUR)

df_unmatched_cnct <- df_cnct_cleaned %>%
  anti_join(df_matched, by = c("eixo_cleaned", "curso_cleaned", "IDX_EIX", "IDX_CUR", "IDX_EIXCUR")) %>%
  arrange(IDX_EIXCUR)

df_censo_touse <- bind_rows(df_matched,df_unmatched_censo) %>% arrange(IDX_EIXCUR)

fix_df_censo <- df_censo_touse %>% filter(is.na(`Eixo Tecnológico`))


patch_censo_area_initial <- function(df, area_name, patch_info, tag_map) {
  df <- df %>%
    left_join(patch_info, by = "IDX_EIXCUR") %>%
    left_join(tag_map, by = "IDX_EIXCUR") %>%
    mutate(
      `Eixo Tecnológico` = case_when(
        !is.na(denom) ~ area_name,
        TRUE ~ `Eixo Tecnológico`
      ),
      `Denominação do Curso` = coalesce(denom, `Denominação do Curso`),
      tag_cnct = pmap(list(tag_cnct, IDX_EIXCUR, `Eixo Tecnológico`, `Denominação do Curso`),
                      function(x, y, eixo, curso) {
                        if (!is.null(x)) {
                          x
                        } else if (is.na(eixo) && is.na(curso)) {
                          NULL
                        } else {
                          list(y)
                        }
                      })
    ) %>%
    select(-denom)
  
  return(df)
}



# Area 01 : Ambiente e Saude

# 1. Define manual patch_info (Denominação and Eixo fix):

patch_info_amb <- tibble(
  IDX_EIXCUR = c("01011", "01024"),
  denom = c("Técnico em Gerência em Saúde", "Técnico em Outros - Eixo Ambiente e Saúde")
)

# 2. Define tag_map (list-column for CNCT match): 
tag_map_amb <- tibble(
  IDX_EIXCUR = c("01011", "01024"),
  tag_cnct = list("01012", c("01006", "01012"))
)

# 3. Apply the patch for this area:
df_censo_touse <- patch_censo_area_initial(
  df = df_censo_touse,
  area_name = "Ambiente e Saúde",
  patch_info = patch_info_amb,
  tag_map = tag_map_amb
)
# For further areas


patch_censo_area2 <- function(df, area_name, patch_info, tag_map) {
  # Ensure all keys are character
  patch_info <- patch_info %>% mutate(IDX_EIXCUR = as.character(IDX_EIXCUR))
  tag_map <- tag_map %>% mutate(IDX_EIXCUR = as.character(IDX_EIXCUR))
  df <- df %>% mutate(IDX_EIXCUR = as.character(IDX_EIXCUR))
  
  # Rename to avoid .x/.y suffixes
  tag_map <- tag_map %>% rename(tag_cnct_new = tag_cnct)
  
  # Separate rows to update
  df_patch <- df %>%
    filter(IDX_EIXCUR %in% patch_info$IDX_EIXCUR) %>%
    left_join(patch_info, by = "IDX_EIXCUR") %>%
    left_join(tag_map, by = "IDX_EIXCUR") %>%
    mutate(
      `Eixo Tecnológico` = area_name,
      `Denominação do Curso` = ifelse(!is.na(denom), denom, `Denominação do Curso`),
      tag_cnct = ifelse(
        map_lgl(tag_cnct, is.null),  # Only replace if NULL
        tag_cnct_new,
        tag_cnct
      )
    ) %>%
    select(-denom, -tag_cnct_new)
  
  # Keep untouched rows
  df_rest <- df %>%
    filter(!IDX_EIXCUR %in% patch_info$IDX_EIXCUR)
  
  bind_rows(df_rest, df_patch) %>% arrange(IDX_EIXCUR)
}


  


# 2 	Controle e processos industriais
# 1. Define manual patch_info (Denominação and Eixo fix):

patch_info_ctrl <- tibble(
  IDX_EIXCUR = c("02023"),
  denom = c("Técnico em Outros - Eixo Controle e Processos Industriais")
)

# 2. Define tag_map (list-column for CNCT match): 
tag_map_ctrl <- tibble(
  IDX_EIXCUR = c("02023"),
  tag_cnct = list("02007")  # Técnico em Ferramentaria
)

# 3. Apply the patch for this area:
df_censo_touse <- patch_censo_area2(
  df = df_censo_touse,
  area_name = "Controle e Processos Industriais",
  patch_info = patch_info_ctrl,
  tag_map = tag_map_ctrl
)

# 3 Desenvolvimento educacional e social
# 1. Manual patch info: fixing Eixo + Denominação
patch_info_dev <- tibble(
  IDX_EIXCUR = c("03010", "03009"),
  denom = c(
    "Técnico em Produção de Materiais Didáticos Bilíngues em Libras/Língua Portuguesa",
    "Técnico em Treinamento e Instrução de Cães-Guia"
  )
)

# 2. Mapping tag_cnct values from CNCT
tag_map_dev <- tibble(
  IDX_EIXCUR = c("03010", "03009"),
  tag_cnct = list("03011", "03014")
)

# 3. Apply second-round patch
df_censo_touse <- patch_censo_area2(
  df = df_censo_touse,
  area_name = "Desenvolvimento Educacional e Social",
  patch_info = patch_info_dev,
  tag_map = tag_map_dev
)

# 4 Gestao e negócios nothing available to patch ! will have tag_cnct as NULL
patch_info_gestao <- tibble(
  IDX_EIXCUR = c("04010"),
  denom = c("Técnico em Outros - Eixo Gestão e Negócios")
)

tag_map_gestao <- tibble(
  IDX_EIXCUR = c("04010"),
  tag_cnct = list(NULL)  # no match
)

df_censo_touse <- patch_censo_area2(
  df = df_censo_touse,
  area_name = "Gestão e Negócios",
  patch_info = patch_info_gestao,
  tag_map = tag_map_gestao
)

# 5 Informática e comunicação
patch_info_info <- tibble(
  IDX_EIXCUR = c("05006"),
  denom = c("Técnico em Outros - Eixo Informação e Comunicação")
)

tag_map_info <- tibble(
  IDX_EIXCUR = c("05006"),
  tag_cnct = list(NULL)  # no CNCT match
)

df_censo_touse <- patch_censo_area2(
  df = df_censo_touse,
  area_name = "Informação e Comunicação",
  patch_info = patch_info_info,
  tag_map = tag_map_info
)

# 6 Infraestrutura e transporte
# 1. Define manual patch_info (Denominação and Eixo fix):
patch_info_infra <- tibble(
  IDX_EIXCUR = c("06001", "06010"),
  denom = c("Técnico em Aeroportuário", "Técnico em Outros - Eixo Infraestrutura")
)

# 2. Define tag_map (list-column for CNCT match): 
tag_map_infra <- tibble(
  IDX_EIXCUR = c("06001", "06010"),
  tag_cnct = list("06013", c("06003", "06009"))  # Aeroportuário, then Carpintaria & Hidrologia
)

# 3. Apply the patch:
df_censo_touse <- patch_censo_area2(
  df = df_censo_touse,
  area_name = "Infraestrutura",
  patch_info = patch_info_infra,
  tag_map = tag_map_infra
)

# 7 Miltar there are 22 ilitar courses in CNCT without mapping in censo
# Patch for Informação e Comunicação (no CNCT match) - we dont do anything with them.

patch_info_info <- tibble(
  IDX_EIXCUR = "05006",
  denom = "Técnico em Outros - Eixo Informação e Comunicação"
)

tag_map_info <- tibble(
  IDX_EIXCUR = "05006",
  tag_cnct = list(NULL)  # Explicitly no match
)

df_censo_touse <- patch_censo_area2(
  df = df_censo_touse,
  area_name = "Informação e Comunicação",
  patch_info = patch_info_info,
  tag_map = tag_map_info
)

 
# 8 Patch for Produção Cultural e Design

patch_info_pcd <- tibble(
  IDX_EIXCUR = c("09021", "09025", "09027"),
  denom = c(
    "Técnico em Instrumento Musical",
    "Técnico em Outros - Eixo Produção Cultural e Design",
    "Técnico em Processos Fonográficos"
  )
)

tag_map_pcd <- tibble(
  IDX_EIXCUR = c("09021", "09025", "09027"),
  tag_cnct = list("09020", NULL, NULL)  # Only Instrumento Musical has a CNCT match
)

df_censo_touse <- patch_censo_area2(
  df = df_censo_touse,
  area_name = "Produção Cultural e Design",
  patch_info = patch_info_pcd,
  tag_map = tag_map_pcd
)

# Patch for Segurança

patch_info_seg <- tibble(
  IDX_EIXCUR = c("12002", "12004"),
  denom = c(
    "Técnico em Outros - Eixo Segurança",
    "Técnico em Prevenção e Combate a Incêndios"
  )
)

tag_map_seg <- tibble(
  IDX_EIXCUR = c("12002", "12004"),
  tag_cnct = list(NULL, "12003")  # Only 12004 gets mapped
)

df_censo_touse <- patch_censo_area2(
  df = df_censo_touse,
  area_name = "Segurança",
  patch_info = patch_info_seg,
  tag_map = tag_map_seg
)

# Other patches
patch_info_other <- tibble(
  IDX_EIXCUR = c("10012", "11012", "11014", "11015", "13007"),
  denom = c(
    "Técnico em Outros - Eixo Produção Industrial",
    "Técnico em Outros - Eixo Recursos Naturais",
    "Técnico em Pós-Colheita",
    "Técnico em Recursos Minerais",
    "Técnico em Outros - Eixo Turismo, hospitalidade e lazer"
  )
)

tag_map_other <- tibble(
  IDX_EIXCUR = patch_info_other$IDX_EIXCUR,
  tag_cnct = replicate(nrow(patch_info_other), list(NULL))
)

# Apply patches
df_censo_touse <- patch_censo_area2(
  df = df_censo_touse,
  area_name = NA,  # Keep existing area
  patch_info = patch_info_other,
  tag_map = tag_map_other
)

# Final refinements
junk <- df_censo_touse %>%
  filter(map_lgl(tag_cnct, ~ length(.) == 1 && !is.null(.))) %>%
  mutate(tag_cnct = map_chr(tag_cnct, 1)) %>%
  arrange(IDX_EIXCUR)

# 186 rows with data (some lost because there are censo courses not in CNCT)


# # Recover other columns from df_cnct_ 
df_cnct_full <- df_cnct_ %>%
  mutate(
    eixo_cleaned = normalize_text(`Eixo Tecnológico`),
    curso_cleaned = `Denominação do Curso` %>%
      str_remove(regex("^t[eé]cnico em\\s+", ignore_case = TRUE)) %>%
      normalize_text()
  ) %>%
  left_join(df_cnct_cleaned %>% select(eixo_cleaned, curso_cleaned, IDX_EIXCUR),
            by = c("eixo_cleaned", "curso_cleaned"))

junk2 <- df_censo_touse %>%
  filter(map_lgl(tag_cnct, ~ length(.) == 1 && !is.null(.))) %>%
  mutate(tag_cnct = map_chr(tag_cnct, 1)) %>%
  left_join(
    df_cnct_full %>% 
      select(IDX_EIXCUR, , `Área Tecnológica`, `Perfil Profissional de Conclusão`,
             `Campo de Atuação`, `Ocupações CBO Associadas`, `Infraestrutura Mínima`),
    by = c("tag_cnct" = "IDX_EIXCUR")
  ) %>%
  arrange(IDX_EIXCUR) 



df_censo_supl_tec2 <- df_censo_supl_tec %>%
  left_join(
    df_censo_cleaned %>%
      select(NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL, IDX_EIX, IDX_CUR, IDX_EIXCUR),
    by = c("NO_AREA_CURSO_PROFISSIONAL", "NO_CURSO_EDUC_PROFISSIONAL")
  )


df_censo_supl_tec3 <- df_censo_supl_tec2 %>%
  left_join(
    junk2 %>%
      select(IDX_EIXCUR, 
             Eixo_Tecnologico_CNCT = `Eixo Tecnológico`, 
             Denominacao_Curso_CNCT = `Denominação do Curso`,
             Area_Tecnologica_CNCT = `Área Tecnológica`, 
             Perfil_CNCT = `Perfil Profissional de Conclusão`, 
             Campo_CNCT = `Campo de Atuação`, 
             Ocupacoes_CNCT = `Ocupações CBO Associadas`, 
             Infra_CNCT = `Infraestrutura Mínima`
      ),
    by = "IDX_EIXCUR"
  )

# Unmatched rows: NA in Eixo Tecnológico (after join)
df_censo_notin_cnct <- df_censo_supl_tec3 %>%
  filter(is.na(Eixo_Tecnologico_CNCT))

sum(df_censo_notin_cnct$QT_MAT_CURSO_TEC)  # 55,707 individuals
save(df_censo_notin_cnct, file = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_censo_notin_cnct.rda")






# Matched rows: keep only rows where we found a CNCT match

df_censo_supl_tec_4qbqALL <- df_censo_supl_tec3 %>%
  filter(!is.na(Eixo_Tecnologico_CNCT))

sum(df_censo_supl_tec_4qbqALL$QT_MAT_CURSO_TEC) # 4,526,525 

# so 55707/4562525 = 1.2% of the data is unmatched in CNCT

save(df_censo_supl_tec_4qbqALL, file = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbqALL.rda")


# Get censo data from Shiny App
load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")
temp_UFs <- df_codes_ibge %>%
  select(CO_MUN, NM_UF, SG_UF, CO_UF) %>%
  distinct() %>%
  arrange(SG_UF)

## Enrollment by courses

df_censo_cnct <- left_join(df_censo_supl_tec_4qbqALL, temp_UFs, by ="CO_MUN") %>% filter(Eixo_Tecnologico_CNCT != "Militar")


# I need to create aggregates of QT_MAT_CURSO_TEC by Eixo_Tecnologico_CNCT, Area_Tecnologica_CNCT, IDX_EIXCUR,
names(df_cnct2025a)

# Aggrgated by UF

df_mat_uf <- df_censo_cnct %>%
  group_by(CO_UF, NM_UF, SG_UF, ANO) %>%
  summarise(QT_MAT_CURSO_TEC_UF = sum(QT_MAT_CURSO_TEC, na.rm = TRUE)) %>%
  arrange(SG_UF)

# Create total row for each ANO (e.g. 2023 and 2024)
total_rows <- df_mat_uf %>%
  group_by(ANO) %>%
  summarise(
    QT_MAT_CURSO_TEC_UF = sum(QT_MAT_CURSO_TEC_UF, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CO_UF = 0L,
    NM_UF = "Brasil",
    SG_UF = "BR"
  ) %>%
  dplyr::relocate(CO_UF, NM_UF, SG_UF, ANO, QT_MAT_CURSO_TEC_UF) 


df_mat_uf <- bind_rows(df_mat_uf, total_rows) %>%
  arrange(ANO, desc(SG_UF))  



# Aggregates by Eixo
# Base aggregation (already done)
df_mat_eixo <- df_censo_cnct %>%
  group_by(CO_UF, NM_UF, SG_UF, ANO, Eixo_Tecnologico_CNCT) %>%
  summarise(QT_MAT_CURSO_TEC_EIX = sum(QT_MAT_CURSO_TEC, na.rm = TRUE)) %>%
  rename(`Eixo Tecnológico` = Eixo_Tecnologico_CNCT)

# Now: aggregate total Brasil by Eixo and ANO
total_rows_eixo <- df_mat_eixo %>%
  group_by(`Eixo Tecnológico`, ANO) %>%
  summarise(
    QT_MAT_CURSO_TEC_EIX = sum(QT_MAT_CURSO_TEC_EIX, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CO_UF = 0L,
    NM_UF = "Brasil",
    SG_UF = "BR"
  ) %>%
  dplyr::relocate(CO_UF, NM_UF, SG_UF, ANO, QT_MAT_CURSO_TEC_EIX) 



# Bind to main table
df_mat_eixo <- bind_rows(df_mat_eixo, total_rows_eixo) %>%
  arrange(ANO, `Eixo Tecnológico`, desc(SG_UF))  # optional sorting



# Aggregates by Área
df_mat_area <- df_censo_cnct %>%
  group_by(CO_UF, NM_UF, SG_UF, ANO, Area_Tecnologica_CNCT) %>%
  summarise(QT_MAT_CURSO_TEC_ARE = sum(QT_MAT_CURSO_TEC, na.rm = TRUE)) %>%
  rename(`Área Tecnológica` = Area_Tecnologica_CNCT)

# Add BR totals
total_rows_area <- df_mat_area %>%
  group_by(`Área Tecnológica`, ANO) %>%
  summarise(QT_MAT_CURSO_TEC_ARE = sum(QT_MAT_CURSO_TEC_ARE, na.rm = TRUE)) %>%
  mutate(
    CO_UF = 0L,
    NM_UF = "Brasil",
    SG_UF = "BR"
  ) %>%
  dplyr::relocate(CO_UF, NM_UF, SG_UF, ANO, QT_MAT_CURSO_TEC_ARE) 


df_mat_area <- bind_rows(df_mat_area, total_rows_area) %>%
  arrange(ANO, `Área Tecnológica`, desc(SG_UF))

# Aggregates by Curso (IDX_EIXCUR)
df_mat_curso <- df_censo_cnct %>%
  group_by(CO_UF, NM_UF, SG_UF, ANO, IDX_EIXCUR, Denominacao_Curso_CNCT) %>%
  summarise(QT_MAT_CURSO_TEC_CUR = sum(QT_MAT_CURSO_TEC, na.rm = TRUE))  %>%
  rename(`Denominação do Curso` = Denominacao_Curso_CNCT)

# Add BR totals
total_rows_curso <- df_mat_curso %>%
  group_by(`Denominação do Curso`, IDX_EIXCUR, ANO) %>%
  summarise(QT_MAT_CURSO_TEC_CUR = sum(QT_MAT_CURSO_TEC_CUR, na.rm = TRUE)) %>%
  mutate(
    CO_UF = 0L,
    NM_UF = "Brasil",
    SG_UF = "BR"
  ) %>%
  select(CO_UF, NM_UF, SG_UF, ANO, `Denominação do Curso`, IDX_EIXCUR, QT_MAT_CURSO_TEC_CUR)

df_mat_curso <- bind_rows(df_mat_curso, total_rows_curso) %>%
  arrange(ANO, `Denominação do Curso`, desc(SG_UF))


# Add BR totals
df_mat_curso <- df_mat_curso %>% ungroup()
df_mat_area  <- df_mat_area  %>% ungroup()
df_mat_eixo  <- df_mat_eixo  %>% ungroup()
df_mat_uf    <- df_mat_uf    %>% ungroup()


save(df_mat_uf, file = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_mat_uf.rda")
save(df_mat_eixo, file = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_mat_eixo.rda")
save(df_mat_area, file = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_mat_area.rda")
save(df_mat_curso, file = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_mat_curso.rda")



# To match with qbq data I dont need the entire data;

df_censo_supl_tec_4qbq <- df_censo_supl_tec_4qbqALL %>%
  select(IDX_EIXCUR, Eixo_Tecnologico_CNCT, 
         Denominacao_Curso_CNCT, Area_Tecnologica_CNCT, Perfil_CNCT,
         Campo_CNCT, Ocupacoes_CNCT, Infra_CNCT) %>% unique() %>% arrange(IDX_EIXCUR)


# #stringr trim then filter 
# temp <- df_censo_supl_tec_4qbq %>% select(Ocupacoes_CNCT) %>% 
#    mutate(cupacoes_CNCT = str_trim(Ocupacoes_CNCT)) %>%
#   filter(Ocupacoes_CNCT!="Ocupação ainda não classificada" & Ocupacoes_CNCT!="Ocupação ainda não classificada.") 
#   
# 
# temp <- temp %>% 
#   mutate(
#     cbo_list = str_extract_all(Ocupacoes_CNCT, "\\d{4}-\\d{2}"),
#     cbo_list = map(cbo_list, ~ unique(.x))  # Deduplicate within each list
#   ) %>% arrange(Ocupacoes_CNCT)


# Define manual expansions
expand_manual <- list(
  "3171" = c("317105", "317110", "317115", "317120"),
  "3172" = c("317205", "317210"),
  "3513" = c("351305", "351310", "351315"),
  "3515" = c("351505", "351510", "351515"),
  "3742" = c("374205", "374210", "374215")
)


df_censo_supl_tec_4qbq <- df_censo_supl_tec_4qbq %>%
  mutate(
    Ocupacoes_CNCT = str_trim(Ocupacoes_CNCT),
    Ocupacoes_CNCT = if_else(
      Ocupacoes_CNCT %in% c("Ocupação ainda não classificada", "Ocupação ainda não classificada."),
      "Sem CBO colocado no CNCT",
      Ocupacoes_CNCT
    ),
    cbo_list = str_extract_all(Ocupacoes_CNCT, "\\d{6}|\\d{4}-\\d{2}|\\d{4}"),
    cbo_list = map(cbo_list, function(x) {
      x_clean <- str_replace_all(x, "-", "")
      expanded <- unlist(map(x_clean, function(code) {
        if (code %in% names(expand_manual)) {
          expand_manual[[code]]
        } else {
          code
        }
      }))
      unique(expanded)
    })
  ) %>% 
  arrange(Ocupacoes_CNCT)

leftover_4digit <- df_censo_supl_tec_4qbq %>%
  mutate(flat_cbo = map(cbo_list, ~ .x[str_length(.x) == 4])) %>%
  filter(map_lgl(flat_cbo, ~ length(.x) > 0))

# None 





save(df_censo_supl_tec_4qbq, file = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbq.rda")
openxlsx::write.xlsx(df_censo_supl_tec_4qbq, "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbq.xlsx", rowNames = FALSE)





# ✅ Upload to S3
put_object(file = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_censo_notin_cnct.rda",
           object = "working/mec_inep/df_censo_notin_cnct.rda",
           bucket = bucket_name)

put_object(file = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbqALL.rda",
           object = "working/qbq/df_censo_supl_tec_4qbqALL.rda",
           bucket = bucket_name)

put_object(file = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbq.rda",
           object = "working/qbq/df_censo_supl_tec_4qbq.rda",
           bucket = bucket_name)

put_object(file = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbq.xlsx",
           object = "working/qbq/df_censo_supl_tec_4qbq.xlsx",
           bucket = bucket_name)






