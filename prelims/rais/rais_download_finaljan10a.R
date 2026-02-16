# rais_download_finaljan10a_2024.R
#
# Modified from rais_download_finaljul25a.R for 2024 data
# Changes: 2023 → 2024, sep = ";" → sep = ","

library(dplyr)
library(data.table)

# Load IBGE codes (needed for geography join)
load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")

# Dowload vinculos by muni

extract_rais_txt_files <- function(ano = 2024) {
  # Base folders
  base_dir <- "D:/Country/Brazil/TechBrazil/rawdata/rais/mintraemp_download"
  z7_folder <- file.path(base_dir, "raw_7z_files", as.character(ano))
  out_folder <- file.path(base_dir, "txt_extracted", as.character(ano))
  dir.create(out_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Path to existing 7za.exe (assumed already downloaded)
  tf7z <- file.path(base_dir, "7za.exe")
  if (!file.exists(tf7z)) {
    stop("❌ 7za.exe not found in base directory. Please place it in:\n", tf7z)
  }
  
  # List all .7z files to extract
  zips <- list.files(z7_folder, pattern = "\\.7z$", full.names = TRUE)
  
  for (zip_path in zips) {
    zip_name <- basename(zip_path)
    txt_name <- sub("\\.7z$", ".txt", zip_name)
    out_txt <- file.path(out_folder, txt_name)
    
    if (file.exists(out_txt)) {
      message(sprintf("✅ Skipping (already extracted): %s", txt_name))
      next
    }
    
    message(sprintf("📂 Extracting: %s → %s", zip_name, out_folder))
    cmd <- paste0(shQuote(tf7z), " e ", shQuote(zip_path), " -o", shQuote(out_folder), " -y")
    system(cmd, intern = FALSE)
  }
  
  message("✅ Extraction complete.")
}

# Skip extraction - already done manually
# extract_rais_txt_files(ano = 2024)

# 1. Define the list of .txt file paths
caminho_base <- "D:/Country/Brazil/TechBrazil/rawdata/rais/mintraemp_download/txt_extracted/2024/"
lista_arquivos <- list.files(path = caminho_base, pattern = "\\.txt$", full.names = TRUE)

cat("Files found:\n")
print(basename(lista_arquivos))

# 2. Define the function to read and select columns
# CHANGED: sep = "," instead of sep = ";" and column numbers too 
ler_rais_com_colunas <- function(arquivo, colunas = c(8,12,18,20,26,35,38)) {
  cat(sprintf("Reading: %s\n", basename(arquivo)))
  dt <- fread(arquivo, sep = ",", encoding = "Latin-1", showProgress = TRUE)
  cat(sprintf("  Columns: %d, Rows: %s\n", ncol(dt), format(nrow(dt), big.mark = ".")))
  dt_selecionado <- dt[, ..colunas]
  return(dt_selecionado)
}

# Check file order first
cat("\nFile order:\n")
for (i in seq_along(lista_arquivos)) {
  cat(sprintf("  [%d] %s\n", i, basename(lista_arquivos[i])))
}

# Read files based on alphabetical order:
# [1] RAIS_VINC_PUB_CENTRO_OESTE.txt
# [2] RAIS_VINC_PUB_MG_ES_RJ.txt
# [3] RAIS_VINC_PUB_NI.txt
# [4] RAIS_VINC_PUB_NORDESTE.txt
# [5] RAIS_VINC_PUB_NORTE.txt
# [6] RAIS_VINC_PUB_SP.txt
# [7] RAIS_VINC_PUB_SUL.txt

# Skip NI file (index 3) - it's only 4KB
cat("\n=== Reading regional files ===\n")

df_CO <- ler_rais_com_colunas(lista_arquivos[1])   # CENTRO_OESTE
df_MJ <- ler_rais_com_colunas(lista_arquivos[2])   # MG_ES_RJ
df_NI <- ler_rais_com_colunas(lista_arquivos[3])   # NI 
df_NE <- ler_rais_com_colunas(lista_arquivos[4])   # NORDESTE
df_NO <- ler_rais_com_colunas(lista_arquivos[5])   # NORTE
df_SP <- ler_rais_com_colunas(lista_arquivos[6])   # SP
df_SU <- ler_rais_com_colunas(lista_arquivos[7])   # SUL

# Combine in two batches 
df_COMJNE <- rbind(df_CO, df_MJ, df_NI, df_NE)
df_COMJNE <- df_COMJNE %>% rename(CO_MUN6 = `Município - Código`)
sum(df_COMJNE$CO_MUN6 == 999999)

df_ <- df_codes_ibge %>% select(CO_MUN6, NM_MUN, SG_UF, CO_UF, NM_UF) %>% unique()

df_COMJNE <- left_join(df_COMJNE, df_, by = "CO_MUN6", relationship = "many-to-many") %>% 
  filter(CO_MUN6 != 999999) 

df_NOSPSU <- rbind(df_NO, df_SP, df_SU)
df_NOSPSU <- df_NOSPSU %>% rename(CO_MUN6 = `Município - Código`)

sum(df_NOSPSU$CO_MUN6 == 999999)

df_NOSPSU <- left_join(df_NOSPSU, df_, by = "CO_MUN6", relationship = "many-to-many") %>% 
  filter(CO_MUN6 != 999999) 

# Save intermediate files
save(df_COMJNE, file = "D:/Country/Brazil/TechBrazil/working/rais/2024/df_COMJNE.rda")
save(df_NOSPSU, file = "D:/Country/Brazil/TechBrazil/working/rais/2024/df_NOSPSU.rda")

# Combine all
rais2024 <- rbind(df_COMJNE, df_NOSPSU)
save(rais2024, file = "D:/Country/Brazil/TechBrazil/working/rais/2024/rais2024.rda")

# Validation
cat("\n=== Validation ===\n")
cat(sprintf("Total rows: %s\n", format(nrow(rais2024), big.mark = ".")))
cat(sprintf("Total vínculos ativos: %s\n", format(sum(rais2024$`Ind Vínculo Ativo 31/12 - Código` == 1, na.rm = TRUE), big.mark = ".")))
cat("Official RAIS 2024: 57,132,156\n")

cat("\n✅ Saved: D:/Country/Brazil/TechBrazil/working/rais/2024/rais2024.rda\n")
cat("\nNext step: Run rais2a.R to create rais_cbo6_uf24.rda\n")