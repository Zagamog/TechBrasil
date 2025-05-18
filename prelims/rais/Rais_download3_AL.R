################################################################################
## Automated download of RAIS microdata (vínculos) for ALAGOAS (1985–2016)    ##
## Retries each file up to 3 times, logs failures, and continues             ##
## Stores .7z and .txt locally                                              ##
################################################################################

# Required packages -----------------------------------------------------------
if (!require(data.table)) {
  install.packages('data.table')
  library(data.table)
}

# Define working directory (change as needed) ---------------------------------
diret <- "D:/Country/Brazil/Datos_RAIS/RAIS_AL"  # <-- main folder for outputs
dir.create(diret, recursive = TRUE, showWarnings = FALSE)
setwd(diret)

# Subfolder to keep the raw .7z files
z7_folder <- file.path(diret, "raw_7z_files")
dir.create(z7_folder, showWarnings = FALSE)

# Subfolder for extracted .txt files
txt_folder <- file.path(diret, "txt_extracted")
dir.create(txt_folder, showWarnings = FALSE)

# Temporary folder only for storing 7za/fart executables ----------------------
td   <- tempdir()
tf7z <- file.path(td, "7za.exe")
fart <- file.path(td, "fart.exe")

# Define years and single state (AL) ------------------------------------------
resyr <- 1985:2016
resuf <- "AL"  # Only Alagoas

# Full set of variables (from original 'vardisp') -----------------------------
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

# Clean variable names (remove parentheses, braces, etc.) ---------------------
res <- gsub('\\s*\\([^\\)]+\\)', '', vardisp)  # remove text in (...)
res <- gsub('\\{','(', res, fixed=TRUE)
res <- gsub('\\}',')', res, fixed=TRUE)
res <- unique(res)

# We'll keep ALL municipalities
resmun <- NULL

# Create vector of AL + each year ---------------------------------------------
filest <- sprintf('%s%s', resuf, resyr)  # e.g. AL1985, AL1986, ...
filest <- sort(filest)

# Download 7-Zip and FART executables to temp ---------------------------------
download.file('http://cemin.wikidot.com/local--files/raisrm/7za.exe',
              tf7z, mode='wb')
download.file('http://cemin.wikidot.com/local--files/raisr/fart.exe',
              fart, mode='wb')

# Data frame to log any failures ----------------------------------------------
failed_files <- data.frame(
  file      = character(),
  stage     = character(),  # e.g., "download", "extract", "read"
  error_msg = character(),
  stringsAsFactors = FALSE
)

# Loop through each AL + year -------------------------------------------------
for (file in filest) {
  year <- gsub('[A-Z]', '', file)  # numeric part
  uf   <- gsub('[0-9]', '', file)  # "AL"
  
  files_7z  <- paste0(file, '.7z')   # e.g. AL1985.7z
  files_txt <- paste0(file, '.txt')  # e.g. AL1985.txt
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
  try_extract <- try({
    system(paste0(
      shQuote(tf7z), ' e ', shQuote(local_7z_path),
      ' -o', shQuote(year_txt_folder), ' -y'
    ), intern=FALSE)
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
  
  # .txt check
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
  
  #################################################
  ## 3) TEXT REPLACEMENT (Tipo Estab) if requested
  #################################################
  if (any(c('Tipo Estab1','Tipo Estab2') %in% res)) {
    try_fart <- try({
      system(
        paste0(
          shQuote(fart), ' -c ', shQuote(path.file),
          ' "Tipo Estab;Tipo Estab" "Tipo Estab1;Tipo Estab2"'
        ),
        intern=FALSE
      )
    }, silent=TRUE)
    
    if (inherits(try_fart, "try-error")) {
      failed_files <- rbind(
        failed_files,
        data.frame(file=file, stage="fart",
                   error_msg=conditionMessage(attr(try_fart, "condition")),
                   stringsAsFactors=FALSE)
      )
      # Still try to read
    }
  }
  
  ##############
  ## 4) READING
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

# Optionally remove the temp folder storing 7za/fart
unlink(td, recursive=TRUE)

# Finally, write out any failures ---------------------------------------------
if (nrow(failed_files) > 0) {
  write.csv(failed_files, "failed_files_log.csv", row.names=FALSE)
  cat("\nSome files failed. See 'failed_files_log.csv' for details.\n")
} else {
  cat("\nAll AL files processed successfully!\n")
}
