# Censo_UF_garabed1b.R
options(scipen = 999) # Disable scientific notation for clarity

library(tidyverse)
library(here)
library(aws.s3)
library(dotenv)
library(digest)
library(openxlsx)
library(ggplot2)

# --- Load AWS credentials ---
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

# --- Helper: check and download from S3 if missing ---
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

# --- Define local paths and corresponding S3 keys ---
paths <- list(
  df_codes_ibge = "D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda",
  E_Medio_Garabed1 = "D:/Country/Brazil/TechBrazil/working/mec_inep/E_Medio_Garabed1.rda",
  Tecnico_FormaR_Garabed1 = "D:/Country/Brazil/TechBrazil/working/mec_inep/Tecnico_FormaR_Garabed1.rda",
  df_censo_UF = "D:/Country/Brazil/TechBrazil/working/mec_inep/df_censo_UF.rda"
)

s3_keys <- list(
  df_codes_ibge = "working/ibge/df_codes_ibge.rda",
  E_Medio_Garabed1 = "working/mec_inep/E_Medio_Garabed1.rda",
  Tecnico_FormaR_Garabed1 = "working/mec_inep/Tecnico_FormaR_Garabed1.rda",
  df_censo_UF = "working/mec_inep/df_censo_UF.rda"
)

# --- Check and download if missing ---
walk2(paths, s3_keys, ~update_data_from_s3(.x, .y, bucket_name))

# --- Load the data files ---
# --- Load the data files into the global environment ---
walk(paths, ~load(.x, envir = .GlobalEnv))




# Definition 1 of Meta 11a 
# Numerator and Denominator from df_censo_UF.rda
# Numerator is QT_MAT_PROF_TEC_PROPAG;
# Denominator is QT_MAT_MED

meta11a_opcao1 <- df_censo_UF %>% filter(AGREG=="UF_TUDO") %>% 
    select(ANO, NM_UF, SG_UF, QT_MAT_PROF_TEC_PROPAG, QT_MAT_MED) %>%
  
  mutate(
    Meta11a_opcao1 = ifelse(QT_MAT_MED > 0, 
                            QT_MAT_PROF_TEC_PROPAG / QT_MAT_MED, 
                            NA)
  ) 



# Definição 2
# Denominator is the same;
#Numerator is from Tecnico_FormaR_Garabed1.rda QT_MAT_CURSO_TEC+CT + QT_MAT_CURSO_TEC_CONC

# Resumo do dataset Técnico com agrupamento
Tecnico_FormaR_Garabed1_sum <- Tecnico_FormaR_Garabed1 %>%
  group_by(ANO, SG_UF) %>%
  summarise(
    QT_MAT_CURSO_TEC_CT = sum(QT_MAT_CURSO_TEC_CT, na.rm = TRUE),
    QT_MAT_CURSO_TEC_CONC = sum(QT_MAT_CURSO_TEC_CONC, na.rm = TRUE),
    .groups = "drop"
  )

# Cálculo da Meta 11a (opção 2)
meta11a_opcao2 <- df_censo_UF %>%
  filter(AGREG == "UF_TUDO") %>%
  select(ANO, NM_UF, SG_UF, QT_MAT_MED) %>%
  left_join(Tecnico_FormaR_Garabed1_sum, by = c("ANO", "SG_UF")) %>%
  mutate(
    QT_MAT_TEC_NUM2 = coalesce(QT_MAT_CURSO_TEC_CT, 0) + coalesce(QT_MAT_CURSO_TEC_CONC, 0),
    Meta11a_opcao2 = ifelse(QT_MAT_MED > 0, QT_MAT_TEC_NUM2 / QT_MAT_MED, NA)
  )


# Definição 3
# Denominator is the same;
#Numerator is from Tecnico_FormaR_Garabed1.rda QT_MAT_CURSO_TEC+CT 

meta11a_opcao3 <- df_censo_UF %>% filter(AGREG=="UF_TUDO") %>% 
  select(ANO, NM_UF, SG_UF, QT_MAT_MED) %>%
  left_join(Tecnico_FormaR_Garabed1_sum, by = c("ANO", "SG_UF")) %>%
  select(ANO, NM_UF, SG_UF, QT_MAT_MED, QT_MAT_CURSO_TEC_CT) %>%
  mutate(
    QT_MAT_TEC_NUM3 = ifelse(is.na(QT_MAT_CURSO_TEC_CT), 0, QT_MAT_CURSO_TEC_CT), 
    Meta11a_opcao3 = ifelse(QT_MAT_MED > 0, 
                            QT_MAT_TEC_NUM3 / QT_MAT_MED, 
                            NA)
  )




# Combine os dados usando uma coluna comum "Meta11a"
df_plot11a <- bind_rows(
  meta11a_opcao1 %>% 
    mutate(Definicao = "Meta 11a - Opção 1", Meta11a = Meta11a_opcao1) %>% 
    select(ANO, NM_UF, SG_UF, Meta11a, Definicao),
  
  meta11a_opcao2 %>% 
    mutate(Definicao = "Meta 11a - Opção 2", Meta11a = Meta11a_opcao2) %>% 
    select(ANO, NM_UF, SG_UF, Meta11a, Definicao),
  
  meta11a_opcao3 %>% 
    mutate(Definicao = "Meta 11a - Opção 3", Meta11a = Meta11a_opcao3) %>% 
    select(ANO, NM_UF, SG_UF, Meta11a, Definicao)
)




# Nova versão da função ggplot
ggplot_meta11a <- function(data, selected_SG_UF, title) {
  data %>%
    filter(SG_UF == selected_SG_UF) %>%
    ggplot(aes(x = ANO, y =Meta11a, color = Definicao)) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values = c("Meta 11a - Opção 1" = "blue", "Meta 11a - Opção 2" = "red",
                                  "Meta 11a - Opção 3" = "green")) +
    scale_x_continuous(breaks = seq(2007, 2024, by = 1)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 0.6)) +
    labs(
      title = title,
      x = "Ano",
      y = "Proporção",
      color = "Definição"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


uf <- "RJ"
ggplot_meta11a(df_plot11a, selected_SG_UF = uf, paste("Meta 11a Comparação - UF:", uf))


# Create combined dataframe for migration to Shiny:
meta11a_opcoes <- meta11a_opcao1 %>%
  left_join(meta11a_opcao2, by = c("ANO", "SG_UF", "NM_UF"), suffix = c("", "_DROP")) %>%
  left_join(meta11a_opcao3, by = c("ANO", "SG_UF", "NM_UF"), suffix = c("", "_DROP2")) %>%
  select(-ends_with("_DROP"), -ends_with("_DROP2"))

save(meta11a_opcoes,file="D:/Country/Brazil/TechBrazil/working/mec_inep/meta11a_opcoes.rda")


# Upload to AWS 

# Upload para S3
put_object(
  file = "D:/Country/Brazil/TechBrazil/working/mec_inep/meta11a_opcoes.rda",
  object = "working/mec_inep/meta11a_opcoes.rda",
  bucket = bucket_name
)


# I need to download the data from S3

# Download the data from S3

download_meta11a_opcoes <- function(local_path, s3_key, bucket) {
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