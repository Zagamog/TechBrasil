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

## Running this function leads to the files matriculas, complementar, and pesos being loaded into the environment. 
