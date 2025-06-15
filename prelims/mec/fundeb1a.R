# fundeb1a.R
#remotes::install_github("mellohenrique/simulador.fundeb2")

# Loading datat provided from fundeb simulador app


library(simulador.fundeb)
library(dplyr)
library(aws.s3)
library(dotenv)

# --- Step 1: Load AWS credentials ---
dotenv::load_dot_env()

bucket_name <- "techbrazildata"

# --- Step 2: Define local + S3 paths ---
local_base <- "D:/Country/Brazil/TechBrazil/rawdata/fundeb/"
file_list <- c("pesos.rda", "matriculas.rda", "complementar.rda")
s3_paths <- paste0("rawdata/fundeb/", file_list)
local_paths <- paste0(local_base, file_list)

# --- Step 3: Download any missing files from S3 ---
download_if_missing <- function(local_path, s3_path, bucket) {
  if (!file.exists(local_path)) {
    tryCatch({
      save_object(object = s3_path, bucket = bucket, file = local_path)
      message("✅ Downloaded from S3: ", s3_path)
    }, error = function(e) {
      message("❌ Failed to download ", s3_path, ": ", e$message)
    })
  } else {
    message("✅ Using local file: ", local_path)
  }
}

for (i in seq_along(file_list)) {
  download_if_missing(local_paths[i], s3_paths[i], bucket_name)
}

# --- Step 4: Load the .rda files ---
load(local_paths[1])  # pesos.rda
load(local_paths[2])  # matriculas.rda
load(local_paths[3])  # complementar.rda

# Redundant but safe for function fallback
data("matriculas")
data("complementar")
data("pesos")

# --- Step 5: Optional save pesos as CSV for inspection ---
write.csv(pesos, file.path(local_base, "pesos.csv"), row.names = FALSE)

# --- Step 6: Run simulation ---
df_teste <- simula_fundeb(
  dados_matriculas = matriculas,
  dados_complementar = complementar,
  dados_peso = pesos,
  complementacao_vaaf = 4e5,
  complementacao_vaat = 1e5,
  complementacao_vaar = 1e5
)

# --- Step 7: Filter only UF-level data ---
matriculas_uf <- subset(matriculas, nchar(ibge) == 2)

# --- Step 8: Define columns ---
all_cols_ept <- c(
  "ensino_medio_integrado_a_educacao_profissional_rede_publica",
  "itinerario_de_formacao_tecnica_e_profissional_rede_publica",
  "educacao_profissional_concomitante_ao_ensino_medio_rede_publica",
  "ens_medio_integrado_a_ed_profisional_rede_conveniada_de_formacao_por_alternancia",
  "eja_integrada_a_ed_profissional_de_nivel_medio_rede_conveniada_de_formacao_por_alternancia",
  "itinerario_de_formacao_tecnica_e_profissional_rede_conveniada_de_formacao_por_alternancia",
  "educacao_profissional_concomitante_ao_ensino_medio_rede_conveniada_de_formacao_por_alternancia",
  "ens_medio_integrado_a_ed_profissional_rede_conveniada_instituicoes_de_ed_profissional",
  "eja_integrada_a_ed_profissional_de_nivel_medio_rede_conveniada_instituicoes_de_ed_profissional",
  "itinerario_de_formacao_tecnica_e_profissional_rede_conveniada_instituicoes_de_ed_profissional",
  "educacao_profissional_concomitante_ao_ensino_medio_rede_conveniada_instituicoes_de_ed_profissional"
)

all_cols_ensino_medio <- c(
  "ensino_medio_urbano_rede_publica",
  "ensino_medio_rural_rede_publica",
  "ensino_medio_integral_rede_publica",
  "ensino_medio_integrado_a_educacao_profissional_rede_publica"
)

cols_ept <- all_cols_ept[all_cols_ept %in% names(matriculas_uf)]
cols_ensino_medio <- all_cols_ensino_medio[all_cols_ensino_medio %in% names(matriculas_uf)]

# --- Step 9: Compute totals ---
matriculas_uf$total_ept_matriculas <- rowSums(matriculas_uf[, cols_ept, drop = FALSE], na.rm = TRUE)
matriculas_uf$total_ensino_medio <- rowSums(matriculas_uf[, cols_ensino_medio, drop = FALSE], na.rm = TRUE)

# --- Step 10: Compute percentage ---
matriculas_uf$porcentual_ept <- ifelse(
  matriculas_uf$total_ensino_medio > 0,
  100 * matriculas_uf$total_ept_matriculas / matriculas_uf$total_ensino_medio,
  NA_real_
)

# --- Step 11: Final summary result ---
df_ept_summary <- matriculas_uf[, c("ibge", "total_ept_matriculas", "total_ensino_medio", "porcentual_ept")]

uf_codes <- df_teste %>% select(uf, ibge, nome)
df_ept_summary <- df_ept_summary %>%
  left_join(uf_codes, by = "ibge") %>%
  select(ibge, uf, nome, total_ept_matriculas, total_ensino_medio, porcentual_ept)

# Optional save output
save(df_ept_summary, file = "D:/Country/Brazil/TechBrazil/working/fundeb/df_ept_summary.rda")

print("✅ Computation complete. Summary preview:")
