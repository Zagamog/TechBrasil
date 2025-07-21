# rais_download_finaljul25.R

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

df <- fread("D:/Country/Brazil/TechBrazil/rawdata/rais/mintraemp_download/txt_extracted/2024/RAIS_VINC_PUB_NI.txt",
            sep = ";", encoding = "Latin-1", showProgress = TRUE)

df_ <- df[,c(8,12,13,18,20,25,35)]

sum(!is.na(df_$`Mun Trab`))





