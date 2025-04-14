# br_pop1a.R
# This script downloads IBGE population projections for Brazil, 
# creates Amazon Legal and related aggregates, and saves the results locally and to S3.

library(dplyr)
library(openxlsx)
library(aws.s3)
library(dotenv)

# --- Step 1: Load AWS Credentials Securely ---
# Ensure each team member has their own `.env` file in the project directory
dotenv::load_dot_env()

# Define AWS bucket and file paths
bucket_name <- "techbrazildata"
s3_object <- "rawdata/ibge/projecoes_2024_tab3_grupos_etarios_especificos.xlsx"
local_file <- "rawdata/ibge/projecoes_2024.xlsx"  # Local working file
processed_file <- "working/ibge/pop01_70b.rda"
s3_output <- "working/ibge/pop01_70b.rda"

# --- Step 2: Define Functions for S3 Handling ---
# Function to download file from S3 if not present locally
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

# Function to upload processed file back to S3
upload_processed_data <- function(local_path, s3_path, bucket) {
  tryCatch({
    put_object(file = local_path, object = s3_path, bucket = bucket)
    message("✅ Uploaded to S3: ", s3_path)
  }, error = function(e) {
    message("❌ Error uploading to S3: ", e$message)
  })
}

# --- Step 3: Fetch Data ---
update_data_from_s3(local_file, s3_object, bucket_name)

# Load data from downloaded file
pop01_70a <- openxlsx::read.xlsx(local_file, sheet = 1, startRow = 7, colNames = TRUE)

# --- Step 4: Process Data ---
# Select relevant columns and rename
pop01_70b <- pop01_70a %>%
  select(1:4, POP_T, `0-14_T`, `15-17_T`, `18-21_T`, `15-59_T`, `60+_T`) %>%
  mutate(CODEFED = as.character(`CÓD.`)) %>%
  select(-`CÓD.`) %>%
  relocate(CODEFED, .after = "SIGLA")

# Define lists for aggregate regions
amazonia_legal <- c("Acre", "Amapá", "Amazonas", "Maranhão", "Mato Grosso",
                    "Pará", "Rondônia", "Roraima", "Tocantins")

nordeste_r <- c("Alagoas", "Bahia", "Ceará", "Paraíba", 
                "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe")

centro_oeste_r <- c("Distrito Federal", "Goiás", "Mato Grosso do Sul")

variables_to_sum <- c("POP_T", "0-14_T", "15-17_T", "18-21_T", "15-59_T", "60+_T")

# Aggregate regions
aggregate_region <- function(region_list, region_name, code, sigla) {
  pop01_70a %>%
    filter(LOCAL %in% region_list) %>%
    group_by(ANO) %>%
    summarise(
      LOCAL = region_name,
      CODEFED = code,
      SIGLA = sigla,
      across(all_of(variables_to_sum), \(x) sum(x, na.rm = TRUE))
    )
}

amazon_aggregate <- aggregate_region(amazonia_legal, "Amazonia_Legal", "99", "AML")
nordeste_aggregate <- aggregate_region(nordeste_r, "Nordeste_r", "2b", "ND_")
centro_oeste_aggregate <- aggregate_region(centro_oeste_r, "Centro-Oeste_r", "5b", "CO_")

# Combine aggregates with the main dataframe
pop01_70b <- bind_rows(pop01_70b, amazon_aggregate, nordeste_aggregate, centro_oeste_aggregate)

# Calculate proportions
pop01_70b <- pop01_70b %>%
  mutate(
    P_0_14_T = `0-14_T` / POP_T,
    P_15_17_T = `15-17_T` / POP_T,
    P_18_21_T = `18-21_T` / POP_T,
    P_15_59_T = `15-59_T` / POP_T,
    P_60_plus_T = `60+_T` / POP_T
  )

# Determine the crossover year and add crossover values
pop01_70b <- pop01_70b %>%
  group_by(LOCAL) %>%
  mutate(
    Crossover_Flag = if_else(
      lag(`0-14_T`) > lag(`60+_T`) & `0-14_T` < `60+_T`,
      1,
      0
    ),
    Crossover_Value_Num = if_else(Crossover_Flag == 1, `0-14_T`, NA_real_),
    Crossover_Value_Prop = if_else(Crossover_Flag == 1, P_0_14_T, NA_real_)
  ) %>%
  ungroup()

# --- Step 5: Save Locally & Upload to S3 ---
save(pop01_70b, file = processed_file)

upload_processed_data(processed_file, s3_output, bucket_name)
