# qbq1a.R

# Get qbq data

# https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/quadro-brasileiro-de-qualificacoes-qbq
# Downloaded Sunday, July 13, 2025

# From AWS S3 bucket

library(aws.s3)
library(dotenv)
library(dplyr)
library(openxlsx)

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

qbq_excel_path <- "D:/Country/Brazil/TechBrazil/rawdata/qbq/qbq_2025.xlsx"
qbq_s3_key <- "rawdata/qbq/qbq_2025.xlsx"

# If local file does not exist, download from S3
update_data_from_s3(qbq_excel_path, qbq_s3_key, bucket_name)

# Load the Excel file


qbq_ocup <- read.xlsx(qbq_excel_path, sheet = "Ocupação")
qbq_conhecimento1 <- read.xlsx(qbq_excel_path, sheet = "Conhecimento I")
qbq_conhecimento2 <- read.xlsx(qbq_excel_path, sheet = "Conhecimento II")
qbq_habilidade <- read.xlsx(qbq_excel_path, sheet = "Habilidade")
qbq_atitude <- read.xlsx(qbq_excel_path, sheet = "Atitude")
qbq_ocnaoc <- read.xlsx(qbq_excel_path, sheet = "Ocupações não classificada")


# save the dataframes to .rda files and to AWS
save(qbq_ocup, file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup.rda")
save(qbq_conhecimento1, file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_conhecimento1.rda")
save(qbq_conhecimento2, file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_conhecimento2.rda")
save(qbq_habilidade, file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_habilidade.rda")
save(qbq_atitude, file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_atitude.rda")
save(qbq_ocnaoc, file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocnaoc.rda")


# Upload to S3
put_object( file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup.rda", object = "working/qbq/qbq_ocup.rda",bucket = bucket_name)
put_object( file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_conhecimento1.rda", object = "working/qbq/qbq_conhecimento1.rda",bucket = bucket_name)
put_object( file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_conhecimento2.rda", object = "working/qbq/qbq_conhecimento2.rda",bucket = bucket_name)
put_object( file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_habilidade.rda", object = "working/qbq/qbq_habilidade.rda",bucket = bucket_name)
put_object( file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_atitude.rda", object = "working/qbq/qbq_atitude.rda",bucket = bucket_name)
put_object( file = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocnaoc.rda", object = "working/qbq/qbq_ocnaoc.rda",bucket = bucket_name)



