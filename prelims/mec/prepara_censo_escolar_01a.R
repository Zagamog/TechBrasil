###############################################################################
# prepara_censo_escolar_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Microdados do Censo Escolar INEP (2007–2024)
#
# USO NAS ABAS: Insumo para matriculas_UF_01a.R → B1, C1, C2, C3
#               (não carregado diretamente pelo BM_FGV_Propag2.R)
#
# OBJETIVO:
#   Processar microdados anuais do Censo Escolar (CSVs do INEP) para
#   extrair variáveis de matrícula por escola/município. Gera um arquivo
#   .rda por ano com colunas padronizadas de matrícula (QT_MAT_*),
#   indicadores de infraestrutura e variáveis derivadas (QT_MAT_PROF_TEC_PROPAG,
#   QT_MAT_EJA_ARTIC_EPT, QT_MAT_PROF_TEC_MED).
#
# DADOS BRUTOS (S3):
#   rawdata/mec_inep/microdados_ed_basica_YYYY.csv  (18 arquivos, 2007-2024)
#   Origem: INEP — Microdados do Censo Escolar da Educação Básica
#   https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar
#   NOTA: arquivos grandes (~140-220 MB cada, ~3.6 GB total).
#   Na primeira execução por novo desenvolvedor, o download levará tempo
#   considerável. Execuções seguintes usam cache local.
#
# DADOS PROCESSADOS (S3 e local):
#   Local:  working/mec_inep/df_censoXX.rda  (ex: df_censo07.rda, df_censo24.rda)
#   S3:     working/mec_inep/censo_escolar_YYYY.rda  (ex: censo_escolar_2007.rda)
#   NOTA: convenção de nomes difere entre local (df_censoXX) e S3
#   (censo_escolar_YYYY) por razões históricas. Manter ambas as
#   convenções para compatibilidade com scripts existentes.
#
# SINCRONIZAÇÃO S3:
#   - Primeira execução: baixa CSVs brutos e processa
#   - Execuções seguintes: verifica se .rda processado já existe no S3;
#     se sim, baixa direto sem reprocessar o CSV
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
# DEPENDÊNCIAS: dplyr, tidyr, readr, aws.s3, dotenv
# SAÍDA: df_censo07.rda – df_censo24.rda (18 arquivos)
#   Consumidos por matriculas_UF_01a.R para gerar df_censo_UF.rda
#   (agregação por UF e rede administrativa).
###############################################################################

library(dplyr)
library(tidyr)
library(readr)
library(aws.s3)
library(dotenv)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

# Carregar credenciais AWS do arquivo .env na raiz do projeto
dotenv::load_dot_env()

# Bucket S3 do projeto
S3_BUCKET <- "techbrazildata"

# Anos a processar
ANOS <- 2007:2024

# Diretórios locais (relativos à raiz do projeto)
DIR_BRUTO     <- "rawdata/mec_inep"
DIR_PROCESSADO <- "working/mec_inep"

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
# COLUNAS A EXTRAIR DOS MICRODADOS
###############################################################################

cols_manter <- c(
  # Identificação
  "NU_ANO_CENSO", "NO_UF", "SG_UF", "CO_UF", "NO_MUNICIPIO", "CO_MUNICIPIO",
  "NO_ENTIDADE", "CO_ENTIDADE", "TP_LOCALIZACAO", "TP_LOCALIZACAO_DIFERENCIADA",
  "TP_DEPENDENCIA", "IN_PODER_PUBLICO_PARCERIA",
  # Infraestrutura
  "IN_LABORATORIO_INFORMATICA", "IN_LABORATORIO_EDUC_PROF",
  "IN_SALA_OFICINAS_EDUC_PROF",
  # Docentes
  "QT_DOC_BAS", "QT_DOC_MED", "QT_DOC_PROF", "QT_DOC_PROF_TEC",
  "IN_PROF_TEC",
  # Matrículas — variáveis base
  "QT_MAT_BAS", "QT_MAT_EJA", "QT_MAT_EJA_MED", "QT_MAT_MED_PROP",
  "QT_MAT_ESP", "QT_MAT_FUND", "QT_MAT_INF", "QT_MAT_MED",
  "QT_MAT_PROF_TEC", "QT_MAT_MED_CT", "QT_MAT_PROF_TEC_CONC",
  "QT_MAT_PROF_TEC_SUBS", "QT_MAT_MED_NM",
  "QT_MAT_EJA_MED_TEC", "QT_MAT_EJA_FUND_FIC", "QT_MAT_EJA_MED_FIC",
  "QT_MAT_PROF"
)

###############################################################################
# FUNÇÃO PRINCIPAL: PROCESSAR UM ANO
###############################################################################

processar_ano <- function(ano) {
  
  short <- substr(as.character(ano), 3, 4)
  nome_obj   <- paste0("df_censo", short)
  local_rda  <- file.path(DIR_PROCESSADO, paste0(nome_obj, ".rda"))
  s3_key_rda <- paste0("working/mec_inep/censo_escolar_", ano, ".rda")
  
  # ----- Verificar se já existe processado (local ou S3) -----
  
  if (file.exists(local_rda)) {
    message("== ", ano, ": arquivo processado local encontrado ==")
    return(invisible(TRUE))
  }
  
  # Tentar baixar do S3
  data_s3 <- s3_ultima_modificacao(s3_key_rda, S3_BUCKET)
  if (!is.null(data_s3)) {
    message("== ", ano, ": arquivo processado encontrado no S3 — baixando ==")
    sincronizar_s3(local_rda, s3_key_rda, S3_BUCKET)
    return(invisible(TRUE))
  }
  
  # ----- Processamento necessário -----
  
  message("== ", ano, ": processamento necessário ==")
  
  # Sincronizar CSV bruto
  csv_nome   <- paste0("microdados_ed_basica_", ano, ".csv")
  local_csv  <- file.path(DIR_BRUTO, csv_nome)
  s3_key_csv <- paste0("rawdata/mec_inep/", csv_nome)
  
  sincronizar_s3(local_csv, s3_key_csv, S3_BUCKET)
  
  if (!file.exists(local_csv)) {
    warning("CSV não encontrado para ", ano, ": ", local_csv)
    return(invisible(FALSE))
  }
  
  # Leitura do CSV (encoding ISO-8859-1, separador ponto-e-vírgula)
  message(">> Lendo CSV: ", csv_nome, " (pode demorar ~1-2 min por arquivo)...")
  df <- tryCatch({
    readr::read_csv2(local_csv, locale = locale(encoding = "ISO-8859-1"),
                     show_col_types = FALSE)
  }, error = function(e) {
    warning("Erro ao ler CSV de ", ano, ": ", e$message)
    return(NULL)
  })
  if (is.null(df)) return(invisible(FALSE))
  
  # Selecionar colunas disponíveis (defensivo — nem todas existem em todos os anos)
  df_proc <- df %>%
    select(any_of(cols_manter))
  
  # Liberar memória do CSV original
  rm(df); gc()
  
  # -------------------------------------------------------------------------
  # Variável derivada: QT_MAT_MED_NM (Normal/Magistério)
  # Fallback para anos onde a variável não existe nos microdados
  # -------------------------------------------------------------------------
  if (!"QT_MAT_MED_NM" %in% names(df_proc)) {
    if (all(c("QT_MAT_PROF", "QT_MAT_PROF_TEC") %in% names(df_proc))) {
      df_proc$QT_MAT_MED_NM <- df_proc$QT_MAT_PROF - df_proc$QT_MAT_PROF_TEC
    } else {
      df_proc$QT_MAT_MED_NM <- 0
    }
  }
  
  # Substituir NA por 0 em colunas numéricas (exceto identificadores textuais)
  df_proc <- df_proc %>%
    mutate(across(-c(any_of(c("NO_UF", "NO_MUNICIPIO", "NO_ENTIDADE"))),
                  ~ replace_na(., 0)))
  
  # -------------------------------------------------------------------------
  # Variável derivada: QT_MAT_PROF_TEC_PROPAG
  # Matrículas técnicas profissionalizantes relevantes para o PROPAG
  # (exclui Normal/Magistério)
  # -------------------------------------------------------------------------
  df_proc <- df_proc %>%
    mutate(QT_MAT_PROF_TEC_PROPAG = QT_MAT_PROF_TEC - QT_MAT_MED_NM)
  
  # -------------------------------------------------------------------------
  # Variáveis derivadas adicionais (disponíveis apenas 2023-2024)
  # QT_MAT_EJA_ARTIC_EPT: EJA articulada com EPT
  # QT_MAT_PROF_TEC_MED: Técnico integrado + concomitante ao ensino médio
  # -------------------------------------------------------------------------
  if (ano %in% c(2023, 2024)) {
    if (all(c("QT_MAT_EJA_FUND_FIC", "QT_MAT_EJA_MED_FIC",
              "QT_MAT_EJA_MED_TEC") %in% names(df_proc))) {
      df_proc <- df_proc %>%
        mutate(
          QT_MAT_EJA_ARTIC_EPT = QT_MAT_EJA_FUND_FIC + QT_MAT_EJA_MED_FIC +
            QT_MAT_EJA_MED_TEC,
          QT_MAT_PROF_TEC_MED  = QT_MAT_MED_CT + QT_MAT_PROF_TEC_CONC
        )
    }
  }
  
  # -------------------------------------------------------------------------
  # Salvar .rda e upload condicional ao S3
  # -------------------------------------------------------------------------
  dir.create(dirname(local_rda), recursive = TRUE, showWarnings = FALSE)
  assign(nome_obj, df_proc)
  save(list = nome_obj, file = local_rda)
  message(">> Salvo: ", local_rda)
  
  upload_s3_se_diferente(local_rda, s3_key_rda, S3_BUCKET)
  
  # Liberar memória
  rm(df_proc); gc()
  
  return(invisible(TRUE))
}

###############################################################################
# EXECUÇÃO
###############################################################################

message("=== Processando Censo Escolar: ", min(ANOS), "-", max(ANOS),
        " (", length(ANOS), " anos) ===")

resultados <- sapply(ANOS, function(ano) {
  tryCatch(
    processar_ano(ano),
    error = function(e) {
      warning("Erro ao processar ", ano, ": ", e$message)
      FALSE
    }
  )
})

n_ok    <- sum(resultados, na.rm = TRUE)
n_falha <- sum(!resultados, na.rm = TRUE)

###############################################################################
# VALIDAÇÃO
###############################################################################
message(">> Processados com sucesso: ", n_ok, " / ", length(ANOS), " anos")
if (n_falha > 0) {
  anos_falha <- ANOS[!resultados]
  message(">> Falhas: ", paste(anos_falha, collapse = ", "))
}
message("=== prepara_censo_escolar_01a.R concluído ===")