# export_shiny_data.R
# Standalone script to create CSV exports of key datasets for Shiny app
# Run this to generate clean CSV files without large spatial data

library(dplyr)
library(data.table)

# Set working directory (adjust as needed)
# setwd("D:/Country/Brazil/TechBrazil")

cat("=== SHINY DATA EXPORT SCRIPT ===\n")
cat("Creating CSV exports of key datasets...\n\n")

# Create output directory if it doesn't exist
if (!dir.exists("shiny_data_exports")) {
  dir.create("shiny_data_exports")
}

# ===== 1. GEOGRAPHIC KEYS =====
cat("1. Exporting geographic hierarchy data...\n")

if (file.exists("df_codes_ibge.rda")) {
  load("df_codes_ibge.rda")
  
  # Create clean geographic keys
  dft_geo_keys <- as.data.table(df_codes_ibge)[
    , .(CO_MUN6, CO_MUN, SG_UF, NM_UF, CO_UF, NM_MUN,
        CO_RGIMED, NM_RGIMED, CO_RGINTM, NM_RGIINTM)
  ]
  dft_geo_keys <- unique(dft_geo_keys, by = "CO_MUN6")
  
  write.csv(dft_geo_keys, "shiny_data_exports/dft_geo_keys.csv", row.names = FALSE)
  cat("   ✓ dft_geo_keys.csv created\n")
} else {
  cat("   ✗ df_codes_ibge.rda not found\n")
}

# ===== 2. DYNAMISM DATA =====
cat("2. Exporting economic dynamism data...\n")

if (file.exists("working/ibge/MUN_dyna02_21.rda")) {
  load("working/ibge/MUN_dyna02_21.rda")
  write.csv(MUN_dyna02_21, "shiny_data_exports/MUN_dyna02_21.csv", row.names = FALSE)
  cat("   ✓ MUN_dyna02_21.csv created\n")
} else {
  cat("   ✗ MUN_dyna02_21.rda not found\n")
}

# ===== 3. PIB CLEAN SUBSET =====
cat("3. Exporting PIB data subset...\n")

if (file.exists("working/ibge/pib_clean_subset.rda")) {
  load("working/ibge/pib_clean_subset.rda")
  write.csv(pib_subset, "shiny_data_exports/pib_clean_subset.csv", row.names = FALSE)
  cat("   ✓ pib_clean_subset.csv created\n")
} else if (file.exists("working/ibge/df_pibmunis.rda")) {
  # Create it if it doesn't exist
  cat("   Creating PIB subset from raw data...\n")
  load("working/ibge/df_pibmunis.rda")
  
  pib_subset <- df_pibmunis %>%
    mutate(
      population = ifelse(
        is.na(df_pibmunis[[40]]) | df_pibmunis[[40]] == 0,
        NA_real_,
        (df_pibmunis[[39]] * 1000) / df_pibmunis[[40]]
      )
    ) %>%
    select(
      year = 1, mun_code = 7, mun_name = 8, uf_name = 6,
      pib_total = 39, pib_per_capita = 40, population,
      agro_va = 33, industry_va = 34, services_va = 35, admin_va = 36, total_va = 37,
      main_activity = 41, second_activity = 42, third_activity = 43
    )
  
  write.csv(pib_subset, "shiny_data_exports/pib_clean_subset.csv", row.names = FALSE)
  cat("   ✓ pib_clean_subset.csv created\n")
} else {
  cat("   ✗ PIB data not found\n")
}

# ===== 4. APL MATRICULATION DATA =====
cat("4. Exporting APL matriculation data...\n")

# UF level
if (file.exists("apl_matri_UF.rda")) {
  load("apl_matri_UF.rda")
  write.csv(apl_matri_UF, "shiny_data_exports/apl_matri_UF.csv", row.names = FALSE)
  cat("   ✓ apl_matri_UF.csv created\n")
} else {
  cat("   ✗ apl_matri_UF.rda not found\n")
}

# Municipality level (if available)
if (file.exists("apl_matri_MUN.rda")) {
  load("apl_matri_MUN.rda")
  write.csv(apl_matri_MUN, "shiny_data_exports/apl_matri_MUN.csv", row.names = FALSE)
  cat("   ✓ apl_matri_MUN.csv created\n")
}

# ===== 5. RAIS/CBO DATA (if available) =====
cat("5. Checking for RAIS/CBO data...\n")

if (file.exists("working/rais/dft_apl_MUN_2324.rda")) {
  load("working/rais/dft_apl_MUN_2324.rda")
  write.csv(dft_apl_MUN_2324, "shiny_data_exports/dft_apl_MUN_2324.csv", row.names = FALSE)
  cat("   ✓ dft_apl_MUN_2324.csv created\n")
} else {
  cat("   ✗ RAIS data not found\n")
}

# ===== 6. DEMOGRAPHIC TRANSITION DATA (if available) =====
cat("6. Checking for demographic data...\n")

# Look for demographic transition files
demo_files <- list.files(".", pattern = "transicao_demog|demographic", full.names = TRUE, recursive = TRUE)
if (length(demo_files) > 0) {
  cat("   Found demographic files, but skipping for now\n")
} else {
  cat("   ✗ No demographic data found\n")
}

# ===== 7. SUMMARY =====
cat("\n=== EXPORT SUMMARY ===\n")
exported_files <- list.files("shiny_data_exports", pattern = "\\.csv$")
cat(sprintf("Total files exported: %d\n", length(exported_files)))
cat("Files created:\n")
for (file in exported_files) {
  file_size <- file.size(file.path("shiny_data_exports", file))
  size_mb <- round(file_size / 1024 / 1024, 2)
  cat(sprintf("  - %s (%s MB)\n", file, size_mb))
}

# ===== 8. CREATE METADATA =====
cat("\nCreating metadata file...\n")

metadata <- data.frame(
  file_name = exported_files,
  description = c(
    "Geographic hierarchy (municipalities, regions, states)",
    "Economic dynamism index by municipality (2002-2021)",
    "PIB data subset with simplified column names",
    "APL matriculation data by UF",
    "APL matriculation data by municipality (if available)",
    "RAIS employment data by municipality and CBO (if available)"
  )[1:length(exported_files)],
  created_date = Sys.Date(),
  stringsAsFactors = FALSE
)

write.csv(metadata, "shiny_data_exports/data_metadata.csv", row.names = FALSE)
cat("   ✓ data_metadata.csv created\n")

cat(sprintf("\n✓ Export complete! All files saved in 'shiny_data_exports/' directory\n"))
cat("These CSV files can be uploaded to Shiny apps or shared without spatial data.\n")