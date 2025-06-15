library(openxlsx)
library(dplyr)
library(stringr)
library(aws.s3)
library(dotenv)

# --- Step 1: Load AWS credentials ---
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

# --- Step 2: Helper to download from S3 if not found ---
download_if_missing <- function(local_path, s3_key, bucket) {
  if (!file.exists(local_path)) {
    tryCatch({
      save_object(object = s3_key, bucket = bucket, file = local_path)
      message("✅ Downloaded from S3: ", s3_key)
    }, error = function(e) {
      stop("❌ Failed to download ", s3_key, ": ", e$message)
    })
  } else {
    message("✅ Using local file: ", local_path)
  }
}

# --- Step 3: Define all paths and download from S3 if needed ---

years <- c("2019", "2023", "2024")
local_base <- "D:/Country/Brazil/TechBrazil/rawdata/mec_inep/"
for (yr in years) {
  download_if_missing(
    file.path(local_base, paste0("EMR_", yr, ".xlsx")),
    paste0("rawdata/mec_inep/EMR_", yr, ".xlsx"),
    bucket_name
  )
  download_if_missing(
    file.path(local_base, paste0("EPT_", yr, ".xlsx")),
    paste0("rawdata/mec_inep/EPT_", yr, ".xlsx"),
    bucket_name
  )
}

# --- Step 4: Function to read and clean EMR file ---
read_clean_emr <- function(file_path) {
  df <- read.xlsx(file_path, startRow = 14, colNames = FALSE)
  colnames(df)[1:25] <- c(
    "regiao", "uf", "municipio", "codigo_municipio",
    "total_EMR",
    paste0(rep(c("1serie_", "2serie_", "3serie_", "ns_"), each = 5),
           rep(c("total", "federal", "estadual", "municipal", "privada"), times = 4),
           "_EMR")
  )
  df %>%
    filter(!is.na(municipio), str_trim(municipio) != "") %>%
    select(1:25) %>%
    mutate(uf = str_trim(uf))
}

# --- Step 5: Function to read and clean EPT file ---
read_clean_ept <- function(file_path) {
  df <- read.xlsx(file_path, startRow = 14, colNames = FALSE)
  colnames(df)[1:45] <- c(
    "regiao", "uf", "municipio", "codigo_municipio", "total_EPTsnormal",
    paste0(rep(c(
      "tecnico_integrado_", "medio_normal_", "tecnico_concomitante_",
      "tecnico_subsequente_", "tecnico_eja_integrado_",
      "fic_concomitante_", "fic_eja_fundamental_", "fic_eja_medio_"), each = 5),
      rep(c("total", "federal", "estadual", "municipal", "privada"), times = 8))
  )
  df %>%
    filter(!is.na(municipio), str_trim(municipio) != "") %>%
    mutate(uf = str_trim(uf))
}

# --- Step 6: Read data ---
df_EM2019 <- read_clean_emr(file.path(local_base, "EMR_2019.xlsx"))
df_EPT2019 <- read_clean_ept(file.path(local_base, "EPT_2019.xlsx"))
df_EM2023 <- read_clean_emr(file.path(local_base, "EMR_2023.xlsx"))
df_EPT2023 <- read_clean_ept(file.path(local_base, "EPT_2023.xlsx"))
df_EM2024 <- read_clean_emr(file.path(local_base, "EMR_2024.xlsx"))
df_EPT2024 <- read_clean_ept(file.path(local_base, "EPT_2024.xlsx"))

# --- Step 7: Aggregate summaries by UF ---
summarize_ept <- function(df_em, df_ept, year) {
  emr_col <- paste0("total_EMR", year)
  ept_col <- paste0("total_EPT", year)
  subc_col <- paste0("total_subconeja", year)
  denom_col <- paste0("EMR_denom", year)
  
  df_em %>%
    group_by(uf) %>%
    summarise(!!emr_col := sum(total_EMR, na.rm = TRUE), .groups = "drop") %>%
    left_join(
      df_ept %>%
        group_by(uf) %>%
        summarise(
          !!ept_col := sum(tecnico_integrado_total + tecnico_concomitante_total + tecnico_subsequente_total, na.rm = TRUE),
          !!subc_col := sum(tecnico_concomitante_total + tecnico_subsequente_total +
                              tecnico_eja_integrado_total + fic_concomitante_total +
                              fic_eja_fundamental_total, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "uf"
    ) %>%
    mutate(!!denom_col := !!sym(emr_col) - !!sym(subc_col)) %>%
    select(uf, !!ept_col, !!denom_col)
}

df_ept19 <- summarize_ept(df_EM2019, df_EPT2019, "19")
df_ept23 <- summarize_ept(df_EM2023, df_EPT2023, "23")
df_ept24 <- summarize_ept(df_EM2024, df_EPT2024, "24")

# --- Step 8: Compute FUNEMR per UF ---
compute_emrfun_by_uf <- function(df_em) {
  df_em %>%
    filter(!is.na(municipio), str_trim(municipio) != "") %>%
    mutate(total_EMRFUN = rowSums(select(., 
                                         `1serie_estadual_EMR`, `1serie_municipal_EMR`,
                                         `2serie_estadual_EMR`, `2serie_municipal_EMR`,
                                         `3serie_estadual_EMR`, `3serie_municipal_EMR`,
                                         `ns_estadual_EMR`,     `ns_municipal_EMR`), na.rm = TRUE)) %>%
    group_by(uf) %>%
    summarise(total_EMRFUN = sum(total_EMRFUN, na.rm = TRUE), .groups = "drop")
}

df_emrfun19 <- compute_emrfun_by_uf(df_EM2019) %>% rename(total_EMRFUN19 = total_EMRFUN)
df_emrfun23 <- compute_emrfun_by_uf(df_EM2023) %>% rename(total_EMRFUN23 = total_EMRFUN)
df_emrfun24 <- compute_emrfun_by_uf(df_EM2024) %>% rename(total_EMRFUN24 = total_EMRFUN)

df_emrfun_all <- df_emrfun19 %>%
  full_join(df_emrfun23, by = "uf") %>%
  full_join(df_emrfun24, by = "uf")

# --- Step 9: Merge and enrich ---
df_eptcenso <- df_ept23 %>%
  left_join(df_ept24, by = "uf") %>%
  left_join(df_ept19, by = "uf") %>%
  left_join(df_emrfun_all, by = "uf")

# Use df_teste from fundeb1a.R
df_estados_teste <- df_teste %>% filter(nchar(ibge) == 2)
df_valor_aluno_estado <- df_estados_teste %>%
  mutate(
    valor_vaaf = recursos_vaaf_final / matriculas_vaaf,
    valor_vaat = recursos_vaat_final / matriculas_vaat,
    valor_fundeb_por_matricula = valor_vaaf + valor_vaat
  ) %>%
  select(uf, valor_fundeb_por_matricula, recursos_fundeb)

ufs_lookup <- tibble::tibble(
  uf_code = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT",
              "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  uf_name = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará", "Distrito Federal",
              "Espírito Santo", "Goiás", "Maranhão", "Minas Gerais", "Mato Grosso do Sul", "Mato Grosso",
              "Pará", "Paraíba", "Pernambuco", "Piauí", "Paraná", "Rio de Janeiro", "Rio Grande do Norte",
              "Rondônia", "Roraima", "Rio Grande do Sul", "Santa Catarina", "Sergipe", "São Paulo", "Tocantins")
)

df_valor_aluno_estado <- df_valor_aluno_estado %>%
  left_join(ufs_lookup, by = c("uf" = "uf_code"))

df_eptcenso <- df_eptcenso %>%
  left_join(df_valor_aluno_estado %>% select(uf_name, valor_fundeb_por_matricula, recursos_fundeb),
            by = c("uf" = "uf_name")) %>%
  mutate(
    recursosFUNEMR = total_EMRFUN24 * 1.25 * valor_fundeb_por_matricula,
    recursosFUNEPT = total_EPT24 * 1.30 * valor_fundeb_por_matricula,
    cr2324 = pmax(total_EPT24 - total_EPT23, 0),
    vez_cr2324 = ifelse(EMR_denom24 > 0, floor((0.5 * EMR_denom24) / cr2324), NA),
    cr1924 = pmax(total_EPT24 - total_EPT19, 0),
    vez_cr1924 = ifelse(EMR_denom24 > 0, floor((0.5 * EMR_denom24) / cr1924), NA)
  )

# --- Step 10: Save final object ---
saveRDS(df_eptcenso, "D:/Country/Brazil/TechBrazil/working/fundeb/df_eptcenso.rds")
