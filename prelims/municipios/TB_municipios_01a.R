###############################################################################
# TB_municipios_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# PIB Municipal IBGE (2002–2021)
#
# USO NAS ABAS: C4 (Modelo Econométrico), D1 (Dinamismo Econômico)
#               Também insumo para codes_ibge_01a.R → A2, D1, D2, D3
#
# OBJETIVO:
#   Ler as bases de dados de PIB dos Municípios do IBGE (2002-2009 e
#   2010-2021), padronizar nomes de colunas, combinar em um único
#   dataframe e salvar como df_pibmunis.rda.
#
# DADOS BRUTOS (S3):
#   rawdata/ibge/PIB dos Municípios - base de dados 2002-2009.xls
#   rawdata/ibge/PIB dos Municípios - base de dados 2010-2021.xlsx
#   Origem: IBGE — Sistema de Contas Nacionais
#   https://ftp.ibge.gov.br/Pib_Municipios/2021/base/
#   Arquivos originais: base_de_dados_2002_2009_xls.zip e
#                       base_de_dados_2010_2021_xlsx.zip
#
# DADOS PROCESSADOS (S3 e local):
#   working/ibge/df_pibmunis.rda
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
# DEPENDÊNCIAS: dplyr, readxl, aws.s3, dotenv
# SAÍDA: df_pibmunis.rda
#   Carregado por codes_ibge_01a.R para gerar df_codes_ibge.rda.
#   Carregado diretamente por BM_FGV_Propag2.R para Abas C4 e D1
#   (PIB per capita, crescimento econômico, modelo econométrico).
###############################################################################

library(dplyr)
library(readxl)
library(aws.s3)
library(dotenv)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

# Carregar credenciais AWS do arquivo .env na raiz do projeto
dotenv::load_dot_env()

# Bucket S3 do projeto
S3_BUCKET <- "techbrazildata"

# Caminhos S3 — dados brutos
S3_PIB_2002_2009 <- "rawdata/ibge/PIB dos Municípios - base de dados 2002-2009.xls"
S3_PIB_2010_2021 <- "rawdata/ibge/PIB dos Municípios - base de dados 2010-2021.xlsx"

# Caminhos S3 — saída processada
S3_PROCESSADO <- "working/ibge/df_pibmunis.rda"

# Caminhos locais (relativos à raiz do projeto)
LOCAL_PIB_2002_2009 <- "rawdata/ibge/PIB dos Municípios - base de dados 2002-2009.xls"
LOCAL_PIB_2010_2021 <- "rawdata/ibge/PIB dos Municípios - base de dados 2010-2021.xlsx"
LOCAL_PROCESSADO    <- "working/ibge/df_pibmunis.rda"

###############################################################################
# FUNÇÕES DE SINCRONIZAÇÃO S3
###############################################################################

# Obter data de última modificação de um objeto no S3
s3_ultima_modificacao <- function(s3_key, bucket) {
  tryCatch({
    info <- head_object(object = s3_key, bucket = bucket)
    as.POSIXct(attr(info, "last-modified"), format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  }, error = function(e) {
    NULL
  })
}

# Sincronizar arquivo do S3 para local
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

# Comparar MD5 do arquivo local com ETag do objeto S3
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

# Upload ao S3 — somente se conteúdo local difere do que está no S3
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
# PASSO 1: SINCRONIZAR DADOS PROCESSADOS
###############################################################################

processado_atualizado <- FALSE

if (file.exists(LOCAL_PROCESSADO)) {
  data_processado_local <- file.mtime(LOCAL_PROCESSADO)
  data_processado_s3 <- s3_ultima_modificacao(S3_PROCESSADO, S3_BUCKET)
  
  if (!is.null(data_processado_s3) && data_processado_s3 > data_processado_local) {
    message("=== Arquivo processado no S3 mais recente — baixando ===")
    sincronizar_s3(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
    processado_atualizado <- TRUE
  } else {
    message("=== Arquivo processado local está atualizado ===")
    processado_atualizado <- TRUE
  }
} else {
  data_processado_s3 <- s3_ultima_modificacao(S3_PROCESSADO, S3_BUCKET)
  if (!is.null(data_processado_s3)) {
    message("=== Arquivo processado encontrado no S3 — baixando ===")
    sincronizar_s3(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
    processado_atualizado <- TRUE
  }
}

###############################################################################
# PASSO 2: REPROCESSAR SE NECESSÁRIO
###############################################################################

if (!processado_atualizado) {
  
  message("=== Processamento necessário — sincronizando dados brutos ===")
  
  # Sincronizar arquivos brutos do S3
  sincronizar_s3(LOCAL_PIB_2002_2009, S3_PIB_2002_2009, S3_BUCKET)
  sincronizar_s3(LOCAL_PIB_2010_2021, S3_PIB_2010_2021, S3_BUCKET)
  
  # -------------------------------------------------------------------------
  # Leitura dos arquivos IBGE
  # -------------------------------------------------------------------------
  message(">> Lendo PIB Municípios 2002-2009 (.xls)...")
  df_2002_2009 <- read_excel(LOCAL_PIB_2002_2009, sheet = 1)
  message("   ", nrow(df_2002_2009), " linhas, ", ncol(df_2002_2009), " colunas")
  
  message(">> Lendo PIB Municípios 2010-2021 (.xlsx)...")
  df_2010_2021 <- read_excel(LOCAL_PIB_2010_2021, sheet = 1)
  message("   ", nrow(df_2010_2021), " linhas, ", ncol(df_2010_2021), " colunas")
  
  # -------------------------------------------------------------------------
  # Padronizar nomes de colunas econômicas
  # Os dois arquivos têm nomes idênticos exceto por quebras de linha:
  #   2002-2009 usa \n, 2010-2021 usa \r\n
  # Padronizar removendo todas as quebras de linha dos nomes
  # -------------------------------------------------------------------------
  message(">> Padronizando nomes de colunas...")
  
  limpar_nomes <- function(nomes) {
    nomes <- gsub("\r\n", " ", nomes)
    nomes <- gsub("\n", " ", nomes)
    nomes <- gsub("\\s+", " ", nomes)
    trimws(nomes)
  }
  
  names(df_2002_2009) <- limpar_nomes(names(df_2002_2009))
  names(df_2010_2021) <- limpar_nomes(names(df_2010_2021))
  
  # -------------------------------------------------------------------------
  # Combinar os dois períodos
  # 2010-2021 tem 3 colunas extras (atividades com maior VAB) que serão
  # preenchidas com NA para 2002-2009 via bind_rows
  # -------------------------------------------------------------------------
  message(">> Combinando 2002-2009 e 2010-2021...")
  df_pibmunis <- bind_rows(df_2002_2009, df_2010_2021)
  
  # -------------------------------------------------------------------------
  # Padronizar todos os nomes de colunas com make.names
  # (reproduz o padrão do import SQLite: espaços→pontos, parênteses→pontos)
  # Compatível com codes_ibge_01a.R
  # -------------------------------------------------------------------------
  names(df_pibmunis) <- make.names(names(df_pibmunis), unique = TRUE)
  
  # -------------------------------------------------------------------------
  # Renomear colunas econômicas para nomes curtos padronizados
  # -------------------------------------------------------------------------
  nomes_economicos <- c(
    "VAB_AGRO"  = "Valor.adicionado.bruto.da.Agropecuária",
    "VAB_IND"   = "Valor.adicionado.bruto.da.Indústria",
    "VAB_SERV"  = "Valor.adicionado.bruto.dos.Serviços",
    "VAB_ADM"   = "Valor.adicionado.bruto.da.Administração",
    "VAB_TOTAL" = "Valor.adicionado.bruto.total",
    "IMPOSTOS"  = "Impostos..líquidos",
    "PIB"       = "Produto.Interno.Bruto..a.preços",
    "PIB_PC"    = "Produto.Interno.Bruto.per.capita"
  )
  
  for (nome_curto in names(nomes_economicos)) {
    padrao <- nomes_economicos[[nome_curto]]
    idx <- grep(padrao, names(df_pibmunis))
    if (length(idx) == 1) {
      names(df_pibmunis)[idx] <- nome_curto
    } else if (length(idx) > 1) {
      message("   AVISO: padrão '", padrao, "' encontrou ", length(idx), " colunas")
    }
  }
  
  # Renomear colunas de atividades (só presentes no período 2010-2021)
  ativ_cols <- grep("Atividade.com", names(df_pibmunis), value = TRUE)
  if (length(ativ_cols) >= 3) {
    names(df_pibmunis)[names(df_pibmunis) == ativ_cols[1]] <- "ATIV_MAIOR_VAB"
    names(df_pibmunis)[names(df_pibmunis) == ativ_cols[2]] <- "ATIV_2_MAIOR_VAB"
    names(df_pibmunis)[names(df_pibmunis) == ativ_cols[3]] <- "ATIV_3_MAIOR_VAB"
  }
  
  # -------------------------------------------------------------------------
  # Salvar localmente e upload condicional ao S3
  # -------------------------------------------------------------------------
  dir.create(dirname(LOCAL_PROCESSADO), recursive = TRUE, showWarnings = FALSE)
  save(df_pibmunis, file = LOCAL_PROCESSADO)
  message(">> Salvo localmente: ", LOCAL_PROCESSADO)
  
  upload_s3_se_diferente(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
  
} else {
  # Arquivo processado já está atualizado — apenas carregar
  message("=== Carregando arquivo processado existente ===")
  load(LOCAL_PROCESSADO)
}

###############################################################################
# VALIDAÇÃO
###############################################################################
message(">> df_pibmunis: ", nrow(df_pibmunis), " linhas, ",
        ncol(df_pibmunis), " colunas, ",
        length(unique(df_pibmunis$Ano)), " anos (",
        min(df_pibmunis$Ano), "-", max(df_pibmunis$Ano), "), ",
        length(unique(df_pibmunis$Código.do.Município)), " municípios")
message("=== TB_municipios_01a.R concluído ===")