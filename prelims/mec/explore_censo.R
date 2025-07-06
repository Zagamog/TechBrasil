# explore_censo.R
# Download and load Censo Escolar main microdata 2024

library(here)
library(tidyverse)
library(janitor)
library(aws.s3)
library(dotenv)
library(openxlsx)

# --- Step 1: Load AWS credentials ---
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

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
file_name <- "microdados_ed_basica_2024.csv"
local_path <- file.path(input_folder, file_name)
s3_path <- paste0("rawdata/mec_inep/", file_name)

# --- Step 4: Ensure local copy available ---
update_data_from_s3(local_path, s3_path, bucket_name)

# --- Step 5: Load and clean into a tibble ---
df_ed_basica_2024 <- read_csv2(local_path, locale = locale(encoding = "ISO-8859-1")) 

# Optional: preview structure
glimpse(df_ed_basica_2024)

# Lets get data for Sergip
# select STaRTS WITH qt_mat in dplyr

df_sergipe <- df_ed_basica_2024 %>%
  filter(sg_uf == "SE") %>%
  select(co_entidade,no_entidade,no_municipio,tp_dependencia,in_poder_publico_parceria,tp_poder_publico_parceria,
         starts_with("qt_mat")) %>% filter(tp_dependencia == 2) 


openxlsx::write.xlsx(df_sergipe, "D:/Temp/sergipe_est.xlsx")


# choose from df_sergipe co_entidade==28018117 and flip the columns to row just to look at the data

esc_sergipe_ <- df_sergipe %>%
  filter(co_entidade == 28026730) %>%
  pivot_longer(cols = starts_with("qt_mat"), names_to = "VarNom", values_to = "value") %>%
  arrange(VarNom) %>%
  mutate(VarNom=toupper(VarNom)) %>%
  select(VarNom, value)


#
dict_path <- "D:/Temp2/microdados_censo_escolar_2024/microdados_censo_escolar_2024/Anexos/ANEXO I - Dicionário de Dados/dicionário_dados_educação_básica.xlsx"


# Read only rows 7–485 (skip 8 and 9), columns A:X
dict_raw <- read.xlsx(dict_path,
                      sheet = 1,
                      rows = c(7, 10:485),
                      cols = 1:24,
                      colNames = FALSE)


# rename the columns from X1 to X24 to the vector
colnames(dict_raw) <- c("VarNum", "VarNom", "VarDesc", "VarTip", "VarVals","VarValText",
                         paste0("Y", sprintf("%02d", 7:24)))
dict_raw <- dict_raw[-1,]



# Join esc_sergipe with the dictionary by matching `type` to `VarNum`
esc_sergipe <- esc_sergipe_ %>%
  left_join(dict_raw, by = "VarNom")

# Optional: select and reorder relevant columns
esc_sergipe <- esc_sergipe %>%
  select(VarNum, VarNom, VarDesc, value)


openxlsx::write.xlsx(esc_sergipe, "D:/Temp/es_sergipe_ept.xlsx")


junk <- read.csv("D:/Country/Brazil/TechBrazil/rawdata/mec_inep/microdados_ed_basica_2007.csv",
                 encoding = "ISO-8859-1", sep = ";") 

junk2 <- junk %>% filter (CO_UF==29) %>% select(
"NU_ANO_CENSO", "NO_UF", "SG_UF", "CO_UF", "NO_MUNICIPIO", "CO_MUNICIPIO",
"NO_ENTIDADE", "CO_ENTIDADE", "TP_LOCALIZACAO", "TP_LOCALIZACAO_DIFERENCIADA",
"TP_DEPENDENCIA", "IN_PODER_PUBLICO_PARCERIA", "IN_LABORATORIO_INFORMATICA",
"IN_LABORATORIO_EDUC_PROF", "IN_SALA_OFICINAS_EDUC_PROF", "QT_DOC_BAS", "QT_DOC_MED",
"QT_DOC_PROF", "QT_DOC_PROF_TEC", "IN_PROF_TEC", "QT_MAT_BAS", "QT_MAT_EJA",
"QT_MAT_ESP", "QT_MAT_FUND", "QT_MAT_INF", "QT_MAT_MED", "QT_MAT_PROF_TEC",
"QT_MAT_MED_NM", "QT_MAT_PROF_TEC_SUBS", "QT_MAT_EJA_MED_TEC")

glimpse(junk2)


junk <- read.xlsx("D:/Country/Brazil/TechBrazil/rawdata/mec_inep/Garabed_MTjuly25.xlsx", sheet="E_Medio_Prop_Prof")
library(tidyverse)

# Primeiro, filtra apenas os casos relevantes e soma por combinação chave
parcial_regular_check <- E_Medio_DepAdm %>%
  filter(Forma.de.Oferta %in% c("Médio_Parcial", "Médio_Regular", "Médio_Total")) %>%
  group_by(Ano, UF, `Dependência.Adm.`, Forma.de.Oferta) %>%
  summarise(total = sum(Matrículas, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Forma.de.Oferta,
    values_from = total,
    values_fill = 0
  ) %>%
  mutate(
    Soma_Parcial_Regular = Médio_Parcial + Médio_Regular,
    Diferenca = Médio_Total - Soma_Parcial_Regular
  )

# Exibir inconsistências
parcial_regular_check %>%
  filter(abs(Diferenca) > 0)

parcial_integral_check <- E_Medio_DepAdm %>%
  filter(Forma.de.Oferta %in% c("Médio_Parcial", "Médio_Integral", "Médio_Total")) %>%
  group_by(Ano, UF, `Dependência.Adm.`, Forma.de.Oferta) %>%
  summarise(total = sum(Matrículas, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Forma.de.Oferta,
    values_from = total,
    values_fill = 0
  ) %>%
  mutate(
    Soma_Parcial_Integral = Médio_Parcial + Médio_Integral,
    Diferenca = Médio_Total - Soma_Parcial_Integral
  )

# Ver inconsistências
parcial_integral_check %>%
  filter(abs(Diferenca) == 0)

eja_check <- E_Medio_DepAdm %>%
  filter(Forma.de.Oferta %in% c("Médio_EJA", "Técnico Integrado EJA", "Médio_EJA_Integrado_FIC")) %>%
  group_by(Ano, UF, `Dependência.Adm.`, Forma.de.Oferta) %>%
  summarise(total = sum(Matrículas, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Forma.de.Oferta,
    values_from = total,
    values_fill = 0
  ) %>%
  mutate(
    Soma_EJA_TEC = `Técnico Integrado EJA` + `Médio_EJA_Integrado_FIC`,
    Diferenca = `Médio_EJA` - Soma_EJA_TEC
  )

# Verificar se são iguais
eja_check %>% filter(abs(Diferenca) != 0)

prof_check <- E_Medio_DepAdm %>%
  filter(Forma.de.Oferta %in% c(
    "Médio_Profissional",
    "Técnico Integrado",
    "Técnico Integrado EJA",
    "Normal/Magistério",
    "Médio_EJA_Integrado_FIC"
  )) %>%
  group_by(Ano, UF, `Dependência.Adm.`, Forma.de.Oferta) %>%
  summarise(total = sum(Matrículas, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Forma.de.Oferta,
    values_from = total,
    values_fill = 0
  ) %>%
  mutate(
    Soma_Subtipos = `Técnico Integrado` + `Técnico Integrado EJA` +
      `Normal/Magistério` + `Médio_EJA_Integrado_FIC`,
    Diferenca = `Médio_Profissional` - Soma_Subtipos
  )

# Verificar inconsistências
prof_check %>%
  filter(abs(Diferenca) > 0)



total_vs_componentes <- E_Medio_DepAdm %>%
  filter(Forma.de.Oferta %in% c("Médio_Total", "Médio_Regular", "Médio_Profissional", "Médio_EJA")) %>%
  group_by(Ano, UF, `Dependência.Adm.`, Forma.de.Oferta) %>%
  summarise(total = sum(Matrículas, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Forma.de.Oferta,
    values_from = total,
    values_fill = 0
  ) %>%
  mutate(
    Soma_Componentes = `Médio_Regular` + `Médio_Profissional` + `Médio_EJA`,
    Diferenca = `Médio_Total` - Soma_Componentes
  )

# Mostrar inconsistências
total_vs_componentes %>%
  filter(abs(Diferenca) != 0)


E_Medio_DepAdm %>%
  filter(Forma.de.Oferta %in% c("Médio_Regular", "Médio_Profissional")) %>%
  group_by(Ano, UF, `Dependência.Adm.`, Forma.de.Oferta) %>%
  summarise(total = sum(Matrículas, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Forma.de.Oferta,
    values_from = total,
    values_fill = 0
  ) %>%
  mutate(Prof_maior_que_Regular = `Médio_Profissional` > `Médio_Regular`) %>%
  filter(Prof_maior_que_Regular == TRUE)












