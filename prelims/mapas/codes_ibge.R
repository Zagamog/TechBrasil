# codes_ibge.R
# Process Censo Escolar EPT technical course data (suplemento), handle AWS fallback, and upload outputs.

library(here)
library(tidyverse)
library(janitor)
library(readxl)
library(aws.s3)
library(dotenv)

# --- Step 1: Load AWS credentials ---
dotenv::load_dot_env()
bucket_name <- "techbrazildata"

# --- Step 2: Utility: Download from S3 if local missing ---
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

# --- Step 3: Utility: Upload if file not already present or has changed ---
upload_if_not_exists <- function(local_path, s3_path, bucket) {
  if (!object_exists(object = s3_path, bucket = bucket)) {
    put_object(file = local_path, object = s3_path, bucket = bucket)
    message("✅ Uploaded to S3: ", s3_path)
  } else {
    message("ℹ️ File already exists on S3: ", s3_path)
  }
}

# --- Step 4: Paths ---
input_folder <- here("rawdata", "mec_inep")
output_folder <- here("working", "mec_inep")

# --- Step 5: Load df_codes_ibge ---
df_ibge_path <- here("working", "ibge", "df_codes_ibge.rda")
update_data_from_s3(df_ibge_path, "working/ibge/df_codes_ibge.rda", bucket_name)
load(df_ibge_path)  # loads df_codes_ibge


# --- Step 5a: Add CO_MUN6 to df_codes_ibge and re-save ---
df_codes_ibge <- df_codes_ibge %>%
  mutate(CO_MUN6 = as.numeric(substr(CO_MUN, 1, 6)))

df_codes_ibge <- df_codes_ibge %>% relocate(CO_MUN6, .after = CO_MUN)


# Save updated df_codes_ibge locally
save(df_codes_ibge, file = df_ibge_path)

# Upload to S3 if changed or missing
upload_if_not_exists(df_ibge_path, "working/ibge/df_codes_ibge.rda", bucket_name)

message("✅ CO_MUN6 added to df_codes_ibge and saved locally and to S3.")









