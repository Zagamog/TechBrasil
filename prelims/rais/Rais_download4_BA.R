################################################################################
## Automated download of RAIS microdata (vínculos) for BAHIA (1985–2016)      ##
## Retries each file up to 3 times, logs failures, and continues             ##
## Uses your installed WinRAR to extract .7z (no external 7za/fart)          ##
################################################################################

# Required packages -----------------------------------------------------------
if (!require(data.table)) {
  install.packages('data.table')
  library(data.table)
}

# 1) Define working directories -----------------------------------------------
# Adjust as needed:
diret <- "D:/Country/Brazil/Datos_RAIS/RAIS_BA"  
dir.create(diret, recursive = TRUE, showWarnings = FALSE)
setwd(diret)

# Subfolder for raw .7z files
z7_folder <- file.path(diret, "raw_7z_files")
dir.create(z7_folder, showWarnings = FALSE)

# Subfolder for extracted .txt
txt_folder <- file.path(diret, "txt_extracted")
dir.create(txt_folder, showWarnings = FALSE)

# 2) Path to your WinRAR executable ------------------------------------------
# Adjust to the actual path, if needed
winrar_cmd <- "C:/Program Files/WinRAR/WinRAR.exe"

# 3) Years, state, and variables ---------------------------------------------
resyr <- 1985:2016
resuf <- "BA"  # Only Bahia

# Full set of variables
vardisp <- c('Bairros SP','Bairros Fortaleza (1996-16)','Bairros RJ (1996-16)',
             'Causa Afastamento 1 (2002-16)','Causa Afastamento 2 (2002-16)',
             'IBGE Subatividade (1985-1993)','IBGE Subsetor (1985-1993)',
             'IBGE Subsetor (2015-16)','Causa Afastamento 3 (2002-16)',
             'Motivo Desligamento','CBO Ocupação (1985-1993)',
             'CBO 94 Ocupação (1994-02)','CBO Ocupação 2002 (2003-16)',
             'CNAE 2.0 Classe (2004-16)','CNAE 95 Classe (1994-16)',
             'Distritos SP','Vínculo Ativo 31/12','Faixa Etária',
             'Faixa Hora Contrat (1994-16)','Faixa Remun Dezem {SM}',
             'Faixa Remun Média {SM}','Faixa Tempo Emprego',
             'Grau Instrução 2005-1985 (1985-05)',
             'Escolaridade após 2005 (2006-16)','Qtd Hora Contr (1994-16)',
             'Idade (1994-16)','Ind CEI Vinculado (1999-16)','Ind Simples (2001-16)',
             'Mês Admissão','Mês Desligamento','Mun Trab (2002-16)','Município',
             'Nacionalidade','Natureza Jurídica (1994-16)','Ind Portador Defic (2007-16)',
             'Qtd Dias Afastamento (2002-16)','Raça Cor (2006-16)',
             'Regiões Adm DF (1996-16)','Vl Remun Dezembro Nom (1999-16)',
             'Vl Remun Dezembro {SM}','Vl Remun Média Nom (1999-16)',
             'Vl Remun Média {SM}','CNAE 2.0 Subclasse (2004-16)',
             'Sexo Trabalhador','Tamanho Estabelecimento','Tempo Emprego',
             'Tipo Admissão (1994-16)','Tipo Estab1','Tipo Estab2','Tipo Defic (2007-16)',
             'Tipo Vínculo','Vl Rem Janeiro CC (2015-16)','Vl Rem Fevereiro CC (2015-16)',
             'Vl Rem Março CC (2015-16)','Vl Rem Abril CC (2015-16)',
             'Vl Rem Maio CC (2015-16)','Vl Rem Junho CC (2015-16)',
             'Vl Rem Julho CC (2015-16)','Vl Rem Agosto CC (2015-16)',
             'Vl Rem Setembro CC (2015-16)','Vl Rem Outubro CC (2015-16)',
             'Vl Rem Novembro CC (2015-16)','Ano Chegada Brasil (2016)')

# Clean variable names (remove parentheses, braces, etc.)
res <- gsub('\\s*\\([^\\)]+\\)', '', vardisp)  
res <- gsub('\\{','(', res, fixed=TRUE)
res <- gsub('\\}',')', res, fixed=TRUE)
res <- unique(res)

# We'll keep ALL municipalities
resmun <- NULL

# 4) Build list of <UF><YEAR> combos -----------------------------------------
filest <- sprintf('%s%s', resuf, resyr)  # e.g. BA1985, BA1986, ...
filest <- sort(filest)

# 5) Data frame to log any failures ------------------------------------------
failed_files <- data.frame(
  file      = character(),
  stage     = character(),  # "download", "extract", "read"
  error_msg = character(),
  stringsAsFactors = FALSE
)

# 6) Loop over each <UF><YEAR> -----------------------------------------------
for (file in filest) {
  year <- gsub('[A-Z]', '', file)  # numeric part
  uf   <- gsub('[0-9]', '', file)  # "BA"
  
  files_7z  <- paste0(file, '.7z')   # e.g. BA1985.7z
  files_txt <- paste0(file, '.txt')  # e.g. BA1985.txt
  ftp.path  <- paste0('ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/', year, '/', files_7z)
  
  # Where to store the .7z locally
  local_7z_path <- file.path(z7_folder, files_7z)
  
  ##############
  ## 1) DOWNLOAD (3 attempts)
  ##############
  download_success <- FALSE
  for (attempt in 1:3) {
    cat(sprintf("\nDownloading %s (attempt %d of 3)\n", files_7z, attempt))
    try_dl <- try(
      download.file(ftp.path, destfile=local_7z_path, mode='wb', method='libcurl'),
      silent=TRUE
    )
    if (!inherits(try_dl, 'try-error')) {
      download_success <- TRUE
      break
    } else {
      message(sprintf("Download attempt %d failed, retrying in 30s...", attempt))
      Sys.sleep(30)
    }
  }
  
  if (!download_success) {
    failed_files <- rbind(
      failed_files,
      data.frame(file=file, stage="download",
                 error_msg="All 3 download attempts failed",
                 stringsAsFactors=FALSE)
    )
    next  # Skip to next file
  }
  
  ################
  ## 2) EXTRACT ##
  ################
  # year-specific folder for extracted .txt
  year_txt_folder <- file.path(txt_folder, year)
  dir.create(year_txt_folder, showWarnings = FALSE)
  
  cat(sprintf("Extracting %s -> %s\n", files_7z, year_txt_folder))
  
  # Prepare WinRAR arguments:
  #   x   = extract with full path
  #   -y  = assume yes on prompts
  # Then the .7z file, then the destination folder (with trailing slash if needed)
  rar_args <- c(
    "x",
    "-y",
    shQuote(local_7z_path),
    shQuote(year_txt_folder)
  )
  
  try_extract <- try({
    out <- system2(
      command = shQuote(winrar_cmd),
      args    = rar_args,
      stdout  = TRUE,
      stderr  = TRUE
    )
    exit_code <- attr(out, "status")
    if (is.null(exit_code)) exit_code <- 0
    if (exit_code != 0) {
      stop(paste("WinRAR extraction error code:", exit_code, "\n", paste(out, collapse="\n")))
    }
  }, silent=TRUE)
  
  if (inherits(try_extract, "try-error")) {
    failed_files <- rbind(
      failed_files,
      data.frame(file=file, stage="extract",
                 error_msg=conditionMessage(attr(try_extract, "condition")),
                 stringsAsFactors=FALSE)
    )
    next
  }
  
  # Check if .txt is present
  path.file <- file.path(year_txt_folder, files_txt)
  if (!file.exists(path.file)) {
    failed_files <- rbind(
      failed_files,
      data.frame(file=file, stage="extract",
                 error_msg="Extracted .txt not found",
                 stringsAsFactors=FALSE)
    )
    next
  }
  
  ##############
  ## 3) READING
  ##############
  cat(sprintf("\nReading %s into R...\n", files_txt))
  try_read <- try({
    df <- suppressWarnings(
      fread(path.file, sep=';', dec=',', select=res,
            header=TRUE, encoding='Latin-1')
    )
    
    # (Optional) municipality filter
    # if (!is.null(resmun) && length(resmun) > 0) {
    #   df <- df[df$`Município` %in% resmun, ]
    # }
    
    # Save as .Rda
    dfname <- file
    assign(dfname, df)
    save(list=dfname, file=file.path(diret, paste0(file, '.Rda')))
    rm(list=dfname)
    gc()
  }, silent=TRUE)
  
  if (inherits(try_read, "try-error")) {
    failed_files <- rbind(
      failed_files,
      data.frame(file=file, stage="read",
                 error_msg=conditionMessage(attr(try_read, "condition")),
                 stringsAsFactors=FALSE)
    )
    next
  }
}

# 7) Write out any failures ---------------------------------------------------
if (nrow(failed_files) > 0) {
  write.csv(failed_files, "failed_files_log.csv", row.names=FALSE)
  cat("\nSome files failed. See 'failed_files_log.csv' for details.\n")
} else {
  cat("\nAll BA files processed successfully!\n")
}
