# rais_download_finaljul25a.R

library(dplyr)
library(data.table)

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

# Example usage
extract_rais_txt_files(ano = 2024)

df_NI <- fread("D:/Country/Brazil/TechBrazil/rawdata/rais/mintraemp_download/txt_extracted/2024/RAIS_VINC_PUB_NI.txt",
            sep = ";", encoding = "Latin-1", showProgress = TRUE)

df_NI <- df_NI[,c(8,12,13,18,20,25,35)]

sum(is.na(df_NI$`Mun Trab`))



library(data.table)

# 1. Define the list of .txt file paths
caminho_base <- "D:/Country/Brazil/TechBrazil/rawdata/rais/mintraemp_download/txt_extracted/2024/"
lista_arquivos <- list.files(path = caminho_base, pattern = "\\.txt$", full.names = TRUE)

# 2. Define the function to read and select columns
ler_rais_com_colunas <- function(arquivo, colunas = c(8,12,13,18,20,25,35)) {
  dt <- fread(arquivo, sep = ";", encoding = "Latin-1", showProgress = TRUE)
  dt_selecionado <- dt[, ..colunas]
  return(dt_selecionado)
}
df_NI <- ler_rais_com_colunas(lista_arquivos[3])
df_CO <- ler_rais_com_colunas(lista_arquivos[1])
df_MJ <- ler_rais_com_colunas(lista_arquivos[2])
df_NE <- ler_rais_com_colunas(lista_arquivos[4])

df_NICOMJNE <- rbind(df_NI, df_CO, df_MJ, df_NE)
df_NICOMJNE <- df_NICOMJNE %>% rename(CO_MUN6 = `Mun Trab`)

sum(df_NICOMJNE$CO_MUN6 == 999999) # 52,153 out of 24,646,504

df_ <- df_codes_ibge %>% select(CO_MUN6, NM_MUN, SG_UF, CO_UF, NM_UF) %>% unique()


df_NICOMJNE <- left_join(df_NICOMJNE, df_, by = "CO_MUN6", relationship = "many-to-many") %>% 
  filter(CO_MUN6!= 999999) 


df_NO <- ler_rais_com_colunas(lista_arquivos[5])
df_SP <- ler_rais_com_colunas(lista_arquivos[6])
df_SU <- ler_rais_com_colunas(lista_arquivos[7])
df_NOSPSU <- rbind(df_NO, df_SP, df_SU)

df_NOSPSU <- df_NOSPSU %>% rename(CO_MUN6 = `Mun Trab`)
sum(df_NOSPSU$CO_MUN6 == 999999) # 82,421 out of 39,897,002

df_NOSPSU <- left_join(df_NOSPSU, df_, by = "CO_MUN6", relationship = "many-to-many") %>% 
      filter(CO_MUN6!= 999999) 



save(df_NICOMJNE, file = "D:/Country/Brazil/TechBrazil/working/rais/df_NICOMJNE.rda")
save(df_NOSPSU, file = "D:/Country/Brazil/TechBrazil/working/rais/df_NOSPSU.rda")








