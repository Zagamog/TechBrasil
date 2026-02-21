###############################################################################
# avila_vidal_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Simulações Financeiras PROPAG (Cenários Avila-Vidal / FGV)
#
# USO NAS ABAS: B1 (Visão Geral PROPAG), B2 (Cenários por UF),
#               B3 (Comparação de Cenários)
#
# OBJETIVO:
#   Ler a planilha de simulações financeiras do PROPAG preparada pela
#   equipe FGV (Vidal), com 9 cenários de combinação de abatimento,
#   FEF, investimento direto e juros. Padronizar nomes de colunas,
#   normalizar nomes de UFs e gerar tabela de cenários válidos (dfcen_val)
#   com mapeamento para as opções II-A/B/C, III-A/B/C, IV-A/B e ND.
#
# DADOS BRUTOS (S3):
#   rawdata/fgv/fgv_fin2b.xlsx  (9 planilhas, dados revisados julho 2025)
#   Origem: Equipe FGV-DGPE, simulações financeiras de cenários PROPAG
#   preparadas por Bruno Vidal.
#
# DADOS PROCESSADOS (S3 e local):
#   working/fgv/df_2a.rda, df_2b.rda, df_2c.rda  (Juros 0%)
#   working/fgv/df_3a.rda, df_3b.rda, df_3c.rda  (Juros 1%)
#   working/fgv/df_4a.rda, df_4b.rda              (Juros 2%)
#   working/fgv/df_nd.rda                          (Não Adere, 4%)
#   working/fgv/dfcen_val.rda                      (tabela de cenários)
#
# SINCRONIZAÇÃO S3:
#   - Primeira execução: baixa automaticamente do S3
#   - Execuções seguintes: compara data de modificação local vs S3;
#     baixa somente se a versão no S3 for mais recente
#   - Upload ao S3 ocorre somente se o conteúdo do arquivo processado
#     difere do que está no S3 (comparação MD5 vs ETag)
#
# CREDENCIAIS AWS:
#   Manter arquivo .env na raiz do projeto com as variáveis:
#     AWS_ACCESS_KEY_ID=...
#     AWS_SECRET_ACCESS_KEY=...
#     AWS_DEFAULT_REGION=us-east-1
#   IMPORTANTE: nunca sincronizar .env ao repositório (incluir em .gitignore)
#
# DEPENDÊNCIAS: dplyr, tibble, openxlsx, stringr, stringi, aws.s3, dotenv
# SAÍDA: df_2a–4b.rda, df_nd.rda, dfcen_val.rda (11 arquivos)
#   Carregados diretamente por BM_FGV_Propag2.R.
#   Abas B1–B3: simulações financeiras por cenário e UF.
#
# CENÁRIOS:
#   II-A/B/C  = Juros 0% (três combinações de abatimento/FEF/investimento)
#   III-A/B/C = Juros 1%
#   IV-A/B    = Juros 2%
#   ND        = Não Adere (4%)
#
# COLUNAS PADRONIZADAS (124 colunas por dataframe):
#   NM_UF, DIVIDA, Distr_FEF, Valor_Abat,
#   Saldo2025–Saldo2054, ApoFEF2025–ApoFEF2054,
#   InvDir2025–InvDir2054, JurPag2025–JurPag2054
###############################################################################

library(dplyr)
library(tibble)
library(openxlsx)
library(stringr)
library(stringi)
library(aws.s3)
library(dotenv)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

# Carregar credenciais AWS do arquivo .env na raiz do projeto
dotenv::load_dot_env()

# Bucket S3 do projeto
S3_BUCKET <- "techbrazildata"

# Caminhos S3
S3_BRUTO <- "rawdata/fgv/fgv_fin2b.xlsx"

# Caminhos locais (relativos à raiz do projeto)
LOCAL_BRUTO <- "rawdata/fgv/fgv_fin2b.xlsx"
DIR_PROCESSADO <- "working/fgv"

# Nomes dos dataframes (correspondem às 9 planilhas do xlsx)
NOMES_DF <- c("df_2a", "df_2b", "df_2c", "df_3a", "df_3b", "df_3c",
              "df_4a", "df_4b", "df_nd")

# Nomes padronizados das colunas (124 colunas por dataframe)
NOMES_COLUNAS <- c(
  "NM_UF", "DIVIDA", "Distr_FEF", "Valor_Abat",
  sprintf("Saldo%04d",  2025:2054),
  sprintf("ApoFEF%04d", 2025:2054),
  sprintf("InvDir%04d", 2025:2054),
  sprintf("JurPag%04d", 2025:2054)
)

###############################################################################
# FUNÇÕES DE SINCRONIZAÇÃO S3
###############################################################################

# Obter data de última modificação de um objeto no S3
# Retorna POSIXct ou NULL se o objeto não existir
s3_ultima_modificacao <- function(s3_key, bucket) {
  resultado <- tryCatch({
    info <- suppressMessages(head_object(object = s3_key, bucket = bucket))
    as.POSIXct(attr(info, "last-modified"), format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  }, error = function(e) {
    return(NULL)
  })
  return(resultado)
}

sincronizar_s3 <- function(caminho_local, s3_key, bucket) {
  dir.create(dirname(caminho_local), recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(caminho_local)) {
    message(">> Arquivo local não encontrado: ", caminho_local)
    message(">> Baixando do S3: ", s3_key)
    tryCatch({
      save_object(object = s3_key, bucket = bucket, file = caminho_local)
      message("   Download concluído.")
      return(TRUE)
    }, error = function(e) {
      stop("Erro ao baixar do S3: ", e$message)
    })
  }
  
  data_local <- file.mtime(caminho_local)
  data_s3 <- s3_ultima_modificacao(s3_key, bucket)
  
  if (is.null(data_s3)) {
    message(">> Objeto S3 não encontrado: ", s3_key)
    message(">> Usando versão local: ", caminho_local)
    return(FALSE)
  }
  
  if (data_s3 > data_local) {
    message(">> Versão S3 mais recente (S3: ", data_s3, " | Local: ", data_local, ")")
    message(">> Baixando atualização do S3: ", s3_key)
    tryCatch({
      save_object(object = s3_key, bucket = bucket, file = caminho_local)
      message("   Download concluído.")
      return(TRUE)
    }, error = function(e) {
      message("   Erro ao baixar — usando versão local: ", e$message)
      return(FALSE)
    })
  }
  
  message(">> Versão local atualizada: ", caminho_local)
  return(FALSE)
}

conteudo_identico_s3 <- function(caminho_local, s3_key, bucket) {
  tryCatch({
    md5_local <- tools::md5sum(caminho_local)
    info <- head_object(object = s3_key, bucket = bucket)
    etag_s3 <- gsub('"', '', attr(info, "etag"))
    identico <- unname(md5_local) == etag_s3
    if (identico) {
      message(">> Conteúdo idêntico ao S3 (MD5: ", etag_s3, ")")
    } else {
      message(">> Conteúdo diferente do S3 (local: ", md5_local, " | S3: ", etag_s3, ")")
    }
    return(identico)
  }, error = function(e) {
    message(">> Não foi possível comparar com S3: ", e$message)
    return(FALSE)
  })
}

upload_s3_se_diferente <- function(caminho_local, s3_key, bucket) {
  if (conteudo_identico_s3(caminho_local, s3_key, bucket)) {
    message(">> Upload dispensado — arquivo S3 já possui conteúdo idêntico")
    return(invisible(FALSE))
  }
  tryCatch({
    put_object(file = caminho_local, object = s3_key, bucket = bucket)
    message(">> Upload ao S3 concluído: ", s3_key)
    return(invisible(TRUE))
  }, error = function(e) {
    message(">> Erro no upload ao S3: ", e$message)
    return(invisible(FALSE))
  })
}

###############################################################################
# PASSO 1: VERIFICAR SE TODOS OS PROCESSADOS JÁ EXISTEM
###############################################################################

# Verificar se todos os .rda já existem localmente
todos_locais <- all(sapply(NOMES_DF, function(nome) {
  file.exists(file.path(DIR_PROCESSADO, paste0(nome, ".rda")))
}))
dfcen_local <- file.exists(file.path(DIR_PROCESSADO, "dfcen_val.rda"))

if (todos_locais && dfcen_local) {
  message("=== Todos os arquivos processados já existem localmente ===")
  
  # Verificar se S3 tem versão mais recente de algum arquivo
  # Verificar se S3 tem versão mais recente de algum arquivo
  algum_s3_mais_recente <- FALSE
  for (nome in c(NOMES_DF, "dfcen_val")) {
    local_path <- file.path(DIR_PROCESSADO, paste0(nome, ".rda"))
    s3_key <- paste0("working/fgv/", nome, ".rda")
    s3_existe <- tryCatch(
      suppressMessages(object_exists(object = s3_key, bucket = S3_BUCKET)),
      error = function(e) FALSE
    )
    if (isTRUE(s3_existe)) {
      data_s3 <- s3_ultima_modificacao(s3_key, S3_BUCKET)
      data_local <- file.mtime(local_path)
      if (!is.null(data_s3) && !is.na(data_local) && data_s3 > data_local) {
        message(">> S3 mais recente: ", nome, ".rda — baixando")
        sincronizar_s3(local_path, s3_key, S3_BUCKET)
        algum_s3_mais_recente <- TRUE
      }
    }
  }
  
  # Carregar todos os .rda
  for (nome in NOMES_DF) {
    load(file.path(DIR_PROCESSADO, paste0(nome, ".rda")), envir = .GlobalEnv)
  }
  load(file.path(DIR_PROCESSADO, "dfcen_val.rda"), envir = .GlobalEnv)
  
  # Upload ao S3 se ainda não existem (semeadura inicial)
  for (nome in c(NOMES_DF, "dfcen_val")) {
    local_path <- file.path(DIR_PROCESSADO, paste0(nome, ".rda"))
    s3_key <- paste0("working/fgv/", nome, ".rda")
    s3_existe <- tryCatch(
      suppressMessages(object_exists(object = s3_key, bucket = S3_BUCKET)),
      error = function(e) FALSE
    )
    if (!isTRUE(s3_existe)) {
      message(">> Não encontrado no S3 — fazendo upload: ", s3_key)
      tryCatch({
        put_object(file = local_path, object = s3_key, bucket = S3_BUCKET)
        message("   Upload concluído.")
      }, error = function(e) {
        message("   Erro no upload: ", e$message)
      })
    }
  }
  
  message("=== Arquivos carregados ===")
  
} else {
  
  ###########################################################################
  # PASSO 2: REPROCESSAR
  ###########################################################################
  
  message("=== Processamento necessário ===")
  
  # Sincronizar xlsx bruto
  sincronizar_s3(LOCAL_BRUTO, S3_BRUTO, S3_BUCKET)
  
  # -------------------------------------------------------------------------
  # Ler as 9 planilhas e atribuir nomes padronizados
  # -------------------------------------------------------------------------
  message(">> Lendo planilhas de fgv_fin2b.xlsx...")
  sheet_names <- getSheetNames(LOCAL_BRUTO)
  
  for (i in seq_along(NOMES_DF)) {
    df <- read.xlsx(LOCAL_BRUTO, sheet = sheet_names[i])
    
    # Verificar compatibilidade de colunas
    if (ncol(df) == length(NOMES_COLUNAS)) {
      names(df) <- NOMES_COLUNAS
    } else {
      warning("Planilha ", NOMES_DF[i], ": esperadas ", length(NOMES_COLUNAS),
              " colunas, encontradas ", ncol(df))
    }
    
    assign(NOMES_DF[i], df, envir = .GlobalEnv)
  }
  
  # -------------------------------------------------------------------------
  # Normalizar nomes de UFs
  # Title case com correções para preposições portuguesas
  # -------------------------------------------------------------------------
  message(">> Normalizando nomes de UFs...")
  
  normalizar_nomes_uf <- function(df) {
    df %>%
      mutate(
        NM_UF = NM_UF %>%
          stringr::str_squish() %>%
          stringi::stri_trans_totitle(locale = "pt") %>%
          stringr::str_replace_all(c(
            "Mato Grosso Do Sul"    = "Mato Grosso do Sul",
            "Rio Grande Do Norte"   = "Rio Grande do Norte",
            "Rio Grande Do Sul"     = "Rio Grande do Sul",
            "Rio De Janeiro"        = "Rio de Janeiro"
          ))
      )
  }
  
  for (nome in NOMES_DF) {
    df <- get(nome)
    if ("NM_UF" %in% names(df)) {
      df <- normalizar_nomes_uf(df)
      assign(nome, df, envir = .GlobalEnv)
    }
  }
  
  # -------------------------------------------------------------------------
  # Salvar dataframes de cenários
  # -------------------------------------------------------------------------
  dir.create(DIR_PROCESSADO, recursive = TRUE, showWarnings = FALSE)
  
  for (nome in NOMES_DF) {
    local_path <- file.path(DIR_PROCESSADO, paste0(nome, ".rda"))
    save(list = nome, file = local_path)
    message(">> Salvo: ", local_path)
  }
  
  # =========================================================================
  # TABELA DE CENÁRIOS (dfcen_val)
  # =========================================================================
  
  message(">> Gerando tabela de cenários...")
  
  # Mapas código ↔ rótulo (usados no UI do Shiny)
  # A = abatimento, G = FEF, I = investimento direto, J = juros
  # A.map: A1=Sem abatimento, A2=10%, A3=20%
  # G.map: G1=1%, G2=1.5%, G3=2%
  # I.map: I1=0%, I2=0.5%, I3=1%, I4=1.5%, I5=2%
  # J.map: J1=0%, J2=1%, J3=2%, J4=4% (Não Adere)
  
  # Todas as combinações normais (J1–J3)
  df_all <- expand.grid(
    A = c("A1", "A2", "A3"),
    G = c("G1", "G2", "G3"),
    I = c("I1", "I2", "I3", "I4", "I5"),
    J = c("J1", "J2", "J3"),
    stringsAsFactors = FALSE
  )
  
  # Linha especial: Não Adere (J4)
  nd_row <- data.frame(A = "ND1", G = "ND1", I = "ND1", J = "J4",
                       stringsAsFactors = FALSE)
  
  dfcen_val <- rbind(df_all, nd_row)
  
  # -------------------------------------------------------------------------
  # Marcar as 9 combinações válidas (8 cenários + Não Adere)
  # -------------------------------------------------------------------------
  valid_tbl <- tribble(
    ~A,    ~G,    ~I,    ~J,
    # Juros 2% (IV)
    "A2",  "G2",  "I2",  "J3",   # IV-A: 10% abat | 0.5% ID | 1.5% FEF
    "A1",  "G2",  "I2",  "J3",   # IV-B: 0% abat  | 0.5% ID | 1.5% FEF
    # Juros 1% (III)
    "A2",  "G2",  "I2",  "J2",   # III-A: 10% abat | 0.5% ID | 1.5% FEF
    "A3",  "G1",  "I1",  "J2",   # III-B: 20% abat | 0%   ID | 1%   FEF
    "A1",  "G3",  "I3",  "J2",   # III-C: 0% abat  | 1%   ID | 2%   FEF
    # Juros 0% (II)
    "A2",  "G2",  "I4",  "J1",   # II-A: 10% abat | 1.5% ID | 1.5% FEF
    "A3",  "G1",  "I3",  "J1",   # II-B: 20% abat | 1%   ID | 1%   FEF
    "A1",  "G3",  "I5",  "J1",   # II-C: 0% abat  | 2%   ID | 2%   FEF
    # Não Adere
    "ND1", "ND1", "ND1", "J4"
  )
  
  dfcen_val$valid <- with(dfcen_val,
                          paste(A, G, I, J) %in% paste(valid_tbl$A, valid_tbl$G, valid_tbl$I, valid_tbl$J)
  )
  
  dfcen_val <- dfcen_val %>% arrange(desc(valid))
  
  # -------------------------------------------------------------------------
  # Reordenar linhas válidas para corresponder à ordem das planilhas
  # (ajustes manuais da planilha original para manter correspondência
  # entre posição na tabela e sheet do xlsx)
  # -------------------------------------------------------------------------
  dfcen_val[7, ] <- NA
  dfcen_val[95, ] <- NA
  dfcen_val[8, ] <- NA
  dfcen_val[106, ] <- NA
  
  dfcen_val[7, ] <- data.frame(
    A = "A2", G = "G1", I = "I1", J = "J3", valid = TRUE,
    stringsAsFactors = FALSE)
  dfcen_val[8, ] <- data.frame(
    A = "A1", G = "G2", I = "I2", J = "J3", valid = TRUE,
    stringsAsFactors = FALSE)
  dfcen_val[95, ] <- data.frame(
    A = "A2", G = "G2", I = "I2", J = "J3", valid = FALSE,
    stringsAsFactors = FALSE)
  dfcen_val[106, ] <- data.frame(
    A = "A1", G = "G1", I = "I3", J = "J3", valid = FALSE,
    stringsAsFactors = FALSE)
  
  # -------------------------------------------------------------------------
  # Atribuir rótulos de opção (II-A, II-B, ..., ND)
  # -------------------------------------------------------------------------
  opcao <- c("II-A", "II-B", "II-C", "III-A", "III-B", "III-C",
             "IV-A", "IV-B", "ND", rep("NA", nrow(dfcen_val) - 9))
  dfcen_val$opcao <- opcao
  
  dfcen_val <- dfcen_val %>% arrange(desc(valid))
  
  # -------------------------------------------------------------------------
  # Salvar dfcen_val
  # -------------------------------------------------------------------------
  local_dfcen <- file.path(DIR_PROCESSADO, "dfcen_val.rda")
  save(dfcen_val, file = local_dfcen)
  message(">> Salvo: ", local_dfcen)
  
  # =========================================================================
  # UPLOAD CONDICIONAL DE TODOS OS ARQUIVOS
  # =========================================================================
  
  for (nome in c(NOMES_DF, "dfcen_val")) {
    local_path <- file.path(DIR_PROCESSADO, paste0(nome, ".rda"))
    s3_key <- paste0("working/fgv/", nome, ".rda")
    upload_s3_se_diferente(local_path, s3_key, S3_BUCKET)
  }
}

###############################################################################
# VALIDAÇÃO
###############################################################################
message(">> df_2a: ", nrow(df_2a), " UFs, ", ncol(df_2a), " colunas")
message(">> dfcen_val: ", sum(dfcen_val$valid, na.rm = TRUE), " cenários válidos de ",
        nrow(dfcen_val), " combinações")
message("=== avila_vidal_01a.R concluído ===")