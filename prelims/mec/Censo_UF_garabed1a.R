# Censo_UF_garabed1a.R


library(tidyverse)
library(here)
library(aws.s3)
library(dotenv)
library(digest)
library(openxlsx)

load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")

# --- Load AWS credentials ---
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

# --- S3 upload helper ---
upload_if_missing_or_changed <- function(local_path, s3_key, bucket) {
  if (!file.exists(local_path)) stop("❌ Local file not found: ", local_path)
  temp_s3 <- tempfile(fileext = ".rds")
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

# --- Step 2: Utility to download from S3 if needed ---
update_data_from_s3 <- function(local_path, s3_path, bucket) {
  if (!file.exists(local_path)) {
    tryCatch({
      save_object(object = s3_path, bucket = bucket, file = local_path)
      message("✅ Downloaded from S3: ", local_path)
    }, error = function(e) {
      message("❌ Failed to download from S3: ", s3_path, " — ", e$message)
    })
  } else {
    message("✅ Using local version: ", local_path)
  }
}

# --- Step 3: Define path and filename ---
input_folder <- here("rawdata", "mec_inep")
file_name <- "Garabed_MTjuly25.xlsx"
local_path <- file.path(input_folder, file_name)
s3_path <- paste0("rawdata/mec_inep/", file_name)

# --- Step 4: Download from S3 if missing ---
update_data_from_s3(local_path, s3_path, bucket_name)

# --- Step 5: Ler a planilha "E_Medio_Prop_Prof" ---
if (file.exists(local_path)) {
  df <- openxlsx::read.xlsx(local_path, sheet = "E_Medio_Prop_Prof")
  message("✅ Planilha 'aaa' carregada com sucesso.")
} else {
  stop("❌ Arquivo não encontrado localmente nem no S3: ", local_path)
}

# Import from xlsx file 
E_Medio_main <- openxlsx::read.xlsx(local_path, sheet = "E_Medio_Prop_Prof") %>% arrange(Ano,UF)

table(E_Medio_main$`Ensino.Médio`)

#Integrado FIC Integrado Técnico Normal/Magistério      Profissional      Propedeutico 
#167               378               283               378               378 

# Mat for  Profissional is QT_MAT_PROF


# Padronizar nomes das colunas
E_Medio_mainR <- E_Medio_main %>%
  rename(ANO = Ano, SG_UF = UF, CO_UF = Ufcod)

# Criar nova variável com nomes padronizados para as modalidades
E_Medio_mainR <- E_Medio_mainR %>%
  mutate(tipo_matricula = case_when(
    `Ensino.Médio` == "Profissional" ~ "QT_MAT_PROF",
    `Ensino.Médio` == "Propedeutico" ~ "QT_MAT_MED_PROP",
    `Ensino.Médio` == "Normal/Magistério" ~ "QT_MAT_MED_NM",
    `Ensino.Médio` == "Integrado Técnico" ~ "QT_MAT_MED_CT",
    `Ensino.Médio` == "Integrado FIC" ~ "QT_MAT_EJA_MED_FIC",
    TRUE ~ NA_character_
  ))

# Pivot para formato largo com nomes padronizados
E_Medio_mainR <- E_Medio_mainR %>%
  select(ANO, SG_UF, CO_UF, tipo_matricula, Mat) %>%
  pivot_wider(
    names_from = tipo_matricula,
    values_from = Mat,
    values_fill = 0  # Preenche com zero onde não houver valor
  )

# Please note QT_MAT_PROF=QT_MAT_MED_CT+QT_MAT_MED_NM+QT_MAT_EJA_MED_FIC
E_Medio_mainR$AGREG ="UF_TUDO_GARABED"

# Bring in UF Name
df_ <- df_codes_ibge %>% select(CO_UF,NM_UF) %>% unique()

E_Medio_Garabed1 <- E_Medio_mainR %>%
  left_join(df_, by = "CO_UF") %>%
  relocate(NM_UF, .before = SG_UF)



# --- Step 7: Ler a planilha "E_Medio_Prop_ProfDepAdm" ---

if (file.exists(local_path)) {
  df <- openxlsx::read.xlsx(local_path, sheet = "E_Medio_Prop_Prof_DepAdm")
  message("✅ Planilha 'aaa' carregada com sucesso.")
} else {
  stop("❌ Arquivo não encontrado localmente nem no S3: ", local_path)
}
# Import from xlsx file

E_Medio_DepAdm <- openxlsx::read.xlsx(local_path, sheet = "E_Medio_Prop_Prof_DepAdm") %>% arrange(Ano,UF)
E_Medio_DepAdm %>% select(Forma.de.Oferta) %>% unique()
E_Medio_DepAdm %>% select(`Dependência.Adm.`) %>% unique()
# Could not figure out usefulness of the aggregates here

# --- Step 8: Ler a planilha "E_Medio_Prop_ProfDepAdm" ---

if (file.exists(local_path)) {
  df <- openxlsx::read.xlsx(local_path, sheet = "Tecnico_FormaOferta_DA")
  message("✅ Planilha 'aaa' carregada com sucesso.")
} else {
  stop("❌ Arquivo não encontrado localmente nem no S3: ", local_path)
}

Tecnico_Forma <- openxlsx::read.xlsx(local_path, sheet = "Tecnico_FormaOferta_DA") %>% arrange(Ano,UF)

table(Tecnico_Forma$`Forma.de.Oferta`)


Tecnico_FormaR <- Tecnico_Forma %>%
  rename(
    ANO = Ano,
    SG_UF = UF,
    TP_DEPENDENCIA = `Dependência.Administrativa`,
    NOME_CURSO = `Nome.do.curso`
  ) %>%
  mutate(
    NOME_CURSO = if_else(is.na(NOME_CURSO), "Curso_desconocido", NOME_CURSO),
    
    # Variáveis para agregados internos do PROPAG
    tipo_agregado = case_when(
      `Forma.de.Oferta` == "Normal/Magistério" ~ "QT_MAT_PROF",
      `Forma.de.Oferta` %in% c(
        "Técnico Concomitante",
        "Técnico Integrado",
        "Técnico Integrado EJA",
        "Técnico Subsequente"
      ) ~ "QT_MAT_PROF_TEC",
      TRUE ~ NA_character_
    ),
    tipo_especifico = case_when(
      `Forma.de.Oferta` %in% c("Técnico Concomitante", "Técnico Integrado") ~ "QT_MAT_PROF_TEC_MED",
      TRUE ~ NA_character_
    ),
    
    # Variáveis padronizadas pelo INEP
    tipo_matricula = case_when(
      `Forma.de.Oferta` == "Técnico Integrado" ~ "QT_MAT_CURSO_TEC_CT",
      `Forma.de.Oferta` == "Técnico Concomitante" ~ "QT_MAT_CURSO_TEC_CONC",
      `Forma.de.Oferta` == "Técnico Subsequente" ~ "QT_MAT_CURSO_TEC_SUBS",
      `Forma.de.Oferta` == "Técnico Integrado EJA" ~ "QT_MAT_CURSO_TEC_EJA",
      `Forma.de.Oferta` == "Normal/Magistério" ~ "QT_MAT_CURSO_TEC_NM",
      TRUE ~ NA_character_
    )
  )


# Agregações padronizadas
Tecnico_FormaR <- Tecnico_FormaR %>%
  group_by(ANO, SG_UF, TP_DEPENDENCIA) %>%
  summarise(
    QT_MAT_PROF = sum(Matrículas, na.rm = TRUE),
    QT_MAT_PROF_TEC = sum(if_else(!`Forma.de.Oferta` %in% "Normal/Magistério", Matrículas, 0), na.rm = TRUE),
    QT_MAT_PROF_TEC_MED = sum(if_else(`Forma.de.Oferta` %in% c("Técnico Concomitante", "Técnico Integrado"), Matrículas, 0), na.rm = TRUE),
    
    QT_MAT_CURSO_TEC_CT = sum(if_else(tipo_matricula == "QT_MAT_CURSO_TEC_CT", Matrículas, 0), na.rm = TRUE),
    QT_MAT_CURSO_TEC_CONC = sum(if_else(tipo_matricula == "QT_MAT_CURSO_TEC_CONC", Matrículas, 0), na.rm = TRUE),
    QT_MAT_CURSO_TEC_SUBS = sum(if_else(tipo_matricula == "QT_MAT_CURSO_TEC_SUBS", Matrículas, 0), na.rm = TRUE),
    QT_MAT_CURSO_TEC_EJA = sum(if_else(tipo_matricula == "QT_MAT_CURSO_TEC_EJA", Matrículas, 0), na.rm = TRUE),
    QT_MAT_CURSO_TEC_NM = sum(if_else(tipo_matricula == "QT_MAT_CURSO_TEC_NM", Matrículas, 0), na.rm = TRUE),
    QT_MAT_CURSO_TEC = QT_MAT_CURSO_TEC_CT + QT_MAT_CURSO_TEC_CONC + QT_MAT_CURSO_TEC_SUBS + QT_MAT_CURSO_TEC_EJA + QT_MAT_CURSO_TEC_NM,
    
    .groups = "drop"
  )

# Visualizar resultado
glimpse(Tecnico_FormaR)

## By curso

Tecnico_Forma_Cursos <- Tecnico_Forma %>%
  rename(
    ANO = Ano,
    SG_UF = UF,
    TP_DEPENDENCIA = `Dependência.Administrativa`,
    NOME_CURSO = `Nome.do.curso`
  ) %>%
  mutate(
    NOME_CURSO = if_else(is.na(NOME_CURSO), "Curso_desconocido", NOME_CURSO)
  ) %>%
  group_by(ANO, SG_UF, TP_DEPENDENCIA, NOME_CURSO) %>%
  summarise(QT_MAT_CURSO_TEC = sum(Matrículas, na.rm = TRUE), .groups = "drop")


# QT_MAT_CURSO_TEC is the total of CT + CONC + SUBS + EJA
# validation
total_cursos_check <- Tecnico_Forma_Cursos %>%
  group_by(ANO, SG_UF, TP_DEPENDENCIA) %>%
  summarise(SOMA_CURSOS = sum(QT_MAT_CURSO_TEC, na.rm = TRUE), .groups = "drop")

comparacao <- Tecnico_FormaR %>%
  left_join(total_cursos_check, by = c("ANO", "SG_UF", "TP_DEPENDENCIA")) %>%
  mutate(DIF = QT_MAT_CURSO_TEC - SOMA_CURSOS)

comparacao %>% filter(abs(DIF) > 0)


