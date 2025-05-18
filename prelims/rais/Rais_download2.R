################################################################################
## Automated download of RAIS microdata (vínculos) for ALL states and years   ##
## Saves each file as .Rda with ALL variables                                 ##
## Adapted from script by Guilherme Cemin de Paula - http://cemin.wikidot.com ##
################################################################################

## Install/load required packages ---------------------------------------------
if (!require(data.table)) {
  install.packages('data.table')
  library(data.table)
}

## Define your working directory ---------------------------------------------
## Change "C:/MyData" to the folder where you'd like to store .Rda files.
diret <- "D:/Country/Brazil/Datos_RAIS"
dir.create(diret, recursive = TRUE, showWarnings = FALSE)
setwd(diret)

## We will NOT use previously downloaded 7z files:
prevdir <- NULL
prevres <- character(0)

## Temp files/folders --------------------------------------------------------
tfzip <- tempfile(fileext='.zip')
td    <- tempdir()
tf7z  <- tempfile(pattern='7za',  tmpdir=td, fileext='.exe')
fart  <- tempfile(pattern='fart', tmpdir=td, fileext='.exe')

## Define range of years, states (units), and variables -----------------------
resyr <- 1985:2016
resuf <- c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG',
           'MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR',
           'RS','SC','SE','SP','TO')

## Full set of variables (same as 'vardisp' in original script)
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

## Clean variable names (remove parentheses, braces, etc.) -------------------
res <- gsub('\\s*\\([^\\)]+\\)','', vardisp)        # remove text in ( )
res <- gsub('\\{','(',        res, fixed=TRUE)
res <- gsub('\\}',')',        res, fixed=TRUE)
res <- unique(res)

## We want ALL municipalities, so set resmun to NULL
resmun <- NULL

## Create a vector of all <UF><YEAR> combos -----------------------------------
filest <- sprintf('%s%s',
                  expand.grid(resuf, resyr)[,1],
                  expand.grid(resuf, resyr)[,2])
filest <- sort(filest)

## Download 7-Zip and FART executables (for Windows text replace) ------------
download.file('http://cemin.wikidot.com/local--files/raisrm/7za.exe',
              tf7z, mode='wb')
download.file('http://cemin.wikidot.com/local--files/raisr/fart.exe',
              fart, mode='wb')

## Loop through each file <UF><YEAR> ------------------------------------------
for (file in filest) {
  year <- gsub('[A-Z]', '', file)
  uf   <- gsub('[0-9]', '', file)
  
  files_7z <- paste0(file, '.7z')   # e.g., SP2016.7z
  files_txt <- paste0(file, '.txt') # e.g., SP2016.txt
  
  ## Construct FTP path to the MTE server ------------------------------------
  ftp.path <- paste0('ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/',
                     year,'/', files_7z)
  
  ## Attempt to download (up to 10 retries) ----------------------------------
  counter <- 0
  while(counter < 10) {
    counter <- counter + 1
    try1 <- try(
      download.file(ftp.path, destfile=tfzip, mode='wb', method='libcurl'),
      silent=FALSE
    )
    if (!inherits(try1, 'try-error')) {
      # Succeeded, break out of the retry loop
      break
    } else {
      # Wait 60 seconds before next attempt
      message(sprintf("Download attempt %d of 10 failed. Retrying in 60s...", counter))
      Sys.sleep(60)
    }
  }
  ## Create a folder to extract .txt -----------------------------------------
  dirtxt <- file.path(td, paste0('txt', year))
  dir.create(dirtxt, showWarnings = FALSE)
  
  ## Extract the downloaded .7z into that folder -----------------------------
  cat(sprintf("\nExtracting %s. Please wait...\n", files_7z))
  system(paste0(tf7z, ' e ', shQuote(tfzip),
                ' -o', shQuote(dirtxt), ' -y'))
  
  ## If "Tipo Estab1" or "Tipo Estab2" was requested, do text replacement ----
  path.file <- file.path(dirtxt, files_txt)
  if (any(c('Tipo Estab1','Tipo Estab2') %in% res)) {
    system(
      paste0(
        shQuote(fart),' -c ', shQuote(path.file),
        ' "Tipo Estab;Tipo Estab" "Tipo Estab1;Tipo Estab2"'
      ),
      intern=FALSE
    )
  }
  
  ## Read data with only the requested variables -----------------------------
  cat(sprintf("\nReading %s into R...\n", files_txt))
  df <- suppressWarnings(
    fread(path.file, sep=';', dec=',', select=res,
          header=TRUE, encoding='Latin-1')
  )
  
  ## Filter municipalities if needed (here, we keep all -> resmun=NULL) ------
  if (!is.null(resmun) && length(resmun) > 0) {
    df <- df[df$`Município` %in% resmun, ]
  }
  
  ## Save as .Rda ------------------------------------------------------------
  dfname <- file
  assign(dfname, df)
  save(list=dfname, file=file.path(getwd(), paste0(file, '.Rda')))
  rm(list=dfname)
  gc()
}

## Clean up temporary folder and objects --------------------------------------
unlink(td, recursive=TRUE)
rm(list=ls())
cat("\nAll done! RDA files are in:", diret, "\n")
