# qbq4a.R — Extract CNCT CBOs with updated official IDX_EIXARECUR

library(aws.s3)
library(dotenv)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(janitor)
library(stringr)
library(stringi)
library(purrr)
library(tidyr)
library(reticulate)


# Load .env and credentials
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

# Normalize function for Eixo and Curso
normalize_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_squish() %>%
    stri_trans_general("Latin-ASCII")
}


# Utility to download from S3 if not found locally
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

# File paths
paths <- list(
  df_cnct2025a = "D:/Country/Brazil/TechBrazil/working/mec_outros/df_cnct2025a.rds"
)
s3_keys <- list(
  df_cnct2025a = "working/mec_outros/df_cnct2025a.rds"
)

walk2(paths, s3_keys, ~update_data_from_s3(.x, .y, bucket_name))
df_cnct2025a <- readRDS(paths$df_cnct2025a)

# Manual expansions for 4-digit CBOs
expand_manual <- list(
  "3171" = c("317105", "317110", "317115", "317120"),
  "3172" = c("317205", "317210"),
  "3513" = c("351305", "351310", "351315"),
  "3515" = c("351505", "351510", "351515"),
  "3742" = c("374205", "374210", "374215")
)

# Extract CBOs and expand
df_cnct_cbo <- df_cnct2025a %>%
  select(IDX_EIXARECUR, `Ocupações CBO Associadas`) %>%
  mutate(
    Ocupacoes_CNCT = str_trim(`Ocupações CBO Associadas`),
    Ocupacoes_CNCT = if_else(
      Ocupacoes_CNCT %in% c("Ocupação ainda não classificada", "Ocupação ainda não classificada."),
      "Sem CBO colocado no CNCT",
      Ocupacoes_CNCT
    ),
    cbo_list = str_extract_all(Ocupacoes_CNCT, "\\d{6}|\\d{4}-\\d{2}|\\d{4}"),
    cbo_list = map(cbo_list, function(x) {
      x_clean <- str_replace_all(x, "-", "")
      expanded <- unlist(map(x_clean, function(code) {
        if (code %in% names(expand_manual)) expand_manual[[code]] else code
      }))
      unique(expanded)
    })
  ) %>%
  select(IDX_EIXARECUR, cbo_list) %>%
  unnest_longer(cbo_list, values_to = "CNCT_CBO") %>%
  filter(!is.na(CNCT_CBO))

# Salvar .rda localmente
save(df_cnct_cbo, file = "D:/Country/Brazil/TechBrazil/working/qbq/df_cnct_cbo.rda")

# Upload para S3
put_object(
  file = "D:/Country/Brazil/TechBrazil/working/qbq/df_cnct_cbo.rda",
  object = "working/qbq/df_cnct_cbo.rda",
  bucket = bucket_name
)





df_cnct2025b <- df_cnct2025a %>% rename(Eixo_Tecnologico_CNCT=`Eixo Tecnológico`,
                                        Area_Tecnologica_CNCT=`Área Tecnológica`,
                                        Denominacao_Curso_CNCT=`Denominação do Curso`,
                                        Perfil_Profissional_CNCT=`Perfil Profissional de Conclusão`,
                                        Campo_de_Atuacao_CNCT=`Campo de Atuação`) %>%
                                   mutate(
    Eixo_Tecnologico_CNCT_cleaned = normalize_text(Eixo_Tecnologico_CNCT),
    Area_Tecnologica_CNCT_cleaned = normalize_text(Area_Tecnologica_CNCT)) %>%
  select(1,17,18,4,5,10)

save(df_cnct2025b, file = "D:/Country/Brazil/TechBrazil/working/mec_outros/df_cnct2025b.rda")

# Convert to data.frame first if necessary
# reticulate::conda_list()
pd <- import("pandas")
df_py <- r_to_py(df_cnct2025b)
# Save as pickle (requires reticulate config + pandas setup)
py_save_object(df_cnct2025b, "D:/Country/Brazil/TechBrazil/working/mec_outros/df_cnct2025b.pkl")

# Save to .pkl file
put_object(file = "D:/Country/Brazil/TechBrazil/working/mec_outros/df_cnct2025b.pkl",
           object = "working/mec_outros/df_cnct2025b.pkl",
           bucket = bucket_name)


df_cnct2025b %>% select(Eixo_Tecnologico_CNCT_cleaned) %>% unique() %>% arrange() # 13
df_cnct2025b %>% select(Area_Tecnologica_CNCT_cleaned) %>% unique() %>% arrange() # 36







