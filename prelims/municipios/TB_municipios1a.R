# TB_municipios1a.R

# This script reads an SQLite database for Brazilian municipalities data (PIB),
# prioritizing a local copy and downloading from AWS S3 if not found.

library(DBI)
library(RSQLite) # Make sure RSQLite is loaded for dbConnect
library(aws.s3) # Added for S3 interaction
library(dotenv) # Added for secure AWS credentials
library(dplyr) # For data manipulation
library(here)
# --- S3 Configuration and Utility Function ---
# Ensure your .env file with AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY is in the project directory
dotenv::load_dot_env()


# I download the xlsx file from
# https://ftp.ibge.gov.br/Pib_Municipios/2021/base/ 
# base_de_dados_2002_2009_xls.zip
# base_de_dados_2010_2021_xlsx.zip 
# Created sqlite database with dbeaver: TB_municipios.sqlite; table PIB_dos_Municipios for 2002-2021


bucket_name <- "techbrazildata"
s3_db_path <- "working/sqlite/TB_municipios.sqlite" # S3 path for your SQLite DB

# Function to download file from S3 if not present locally
# (Copied from br_pop1a.R or techbrasil_awsui.R)
update_data_from_s3 <- function(local_path, s3_path, bucket) {
  if (!file.exists(local_path)) {
    tryCatch({
      save_object(object = s3_path, bucket = bucket, file = local_path)
      message("✅ File downloaded from S3: ", local_path)
    }, error = function(e) {
      message("❌ Error downloading from S3: ", e$message)
    })
  } else {
    message("✅ Using local version: ", local_path)
  }
}

# Define local path to your SQLite database
db_path <- "D:/Country/Brazil/TechBrazil/working/sqlite/TB_municipios.sqlite"

# Ensure the local directory exists before attempting to download
db_dir <- dirname(db_path)
if (!dir.exists(db_dir)) {
  dir.create(db_dir, recursive = TRUE)
  message("Created local directory: ", db_dir)
}

# --- Step 1: Ensure SQLite DB is available locally (download from S3 if necessary) ---
update_data_from_s3(local_path = db_path, s3_path = s3_db_path, bucket = bucket_name)

# --- Step 2: Connect to the database ---
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# --- Step 3: Perform database operations ---
# List all tables in the database
message("\nTables in the database:")
print(dbListTables(con))

# Preview the first few rows of your PIB table
message("\nPreview of PIB_dos_Municipios:")
print(dbGetQuery(con, "SELECT * FROM PIB_dos_Municipios LIMIT 10"))

# Quick summary of columns
message("\nFields in PIB_dos_Municipios:")
print(dbListFields(con, "PIB_dos_Municipios"))

# Read the entire table into a dataframe
df_pibmunis <- dbReadTable(con, "PIB_dos_Municipios")

# Optional: Check year range
message("\nYear range in PIB_dos_Municipios:")
print(dbGetQuery(con, "SELECT MIN(Ano) AS min_year, MAX(Ano) AS max_year FROM PIB_dos_Municipios"))

# Example: Table of 'Nome.da.Grande.Região'
message("\nTable of 'Nome.da.Grande.Região':")
print(table(df_pibmunis$`Nome.da.Grande.Região`))

# Convert alphanumeric to numeric


cols_to_convert_indices <- c(1, 2, 4, 7, 10, 12, 14, 17, 20, 23, 27, 33:40)
for (col_idx in cols_to_convert_indices) {
  df_pibmunis[[col_idx]] <- as.numeric(gsub(",", "", df_pibmunis[[col_idx]]))
}


glimpse(df_pibmunis)

# --- Step 4: Always disconnect when done ---
dbDisconnect(con)
message("\nDisconnected from the database.")
# --- Step 5: Upload processed data back to S3 ---
# Save the processed data to a local file
save(df_pibmunis, file = "working/ibge/df_pibmunis.rda")
# Upload the processed file back to S3

tryCatch({
  put_object(file = "working/ibge/df_pibmunis.rda", object = "working/ibge/df_pibmunis.rda",
             bucket = bucket_name)
  message("✅ Uploaded to S3: working/ibge/df_pibmunis.rda")
}, error = function(e) {
  message("❌ Error uploading to S3: ", e$message)
})




