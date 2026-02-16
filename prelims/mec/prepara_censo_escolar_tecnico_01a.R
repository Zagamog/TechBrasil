###############################################################################
# prepara_censo_escolar_tecnico_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Suplemento de Cursos Técnicos do Censo Escolar (2023–2024)
#
# USO NAS ABAS: C3 (Oferta EPT por Eixo e Curso), E1 (Matching Oferta-Demanda)
#
# OBJETIVO:
#   Processar o suplemento de cursos técnicos do Censo Escolar (CSVs do INEP)
#   para extrair matrículas por curso, área profissional, escola e município.
#   O suplemento contém detalhamento por curso individual que não está
#   disponível nos microdados gerais.
#
# DADOS BRUTOS (S3):
#   rawdata/mec_inep/suplemento_cursos_tecnicos_2023.csv  (3.8 MB)
#   rawdata/mec_inep/suplemento_cursos_tecnicos_2024.csv  (4.5 MB)
#   Origem: INEP — Microdados do Censo Escolar, suplemento técnico
#   https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar
#
# DADOS PROCESSADOS (S3 e local):
#   working/mec_inep/df_censo_supl_tec23.rda
#   working/mec_inep/df_censo_supl_tec24.rda
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
# SAÍDA: df_censo_supl_tec23.rda, df_censo_supl_tec24.rda
#   Carregados diretamente por BM_FGV_Propag2.R.
#   Aba C3: oferta EPT por eixo tecnológico e curso por UF.
#   Aba E1: matching cursos EPT com ocupações CBO.
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

# Anos do suplemento técnico
ANOS_SUPL <- c(2023, 2024)

# Diretórios locais (relativos à raiz do projeto)
DIR_BRUTO      <- "rawdata/mec_inep"
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
# FUNÇÃO PRINCIPAL: PROCESSAR UM ANO DO SUPLEMENTO TÉCNICO
###############################################################################

processar_suplemento <- function(ano) {
  
  short <- substr(as.character(ano), 3, 4)
  nome_obj    <- paste0("df_censo_supl_tec", short)
  local_rda   <- file.path(DIR_PROCESSADO, paste0(nome_obj, ".rda"))
  s3_key_rda  <- paste0("working/mec_inep/", nome_obj, ".rda")
  
  # ----- Verificar se já existe processado (local ou S3) -----
  
  if (file.exists(local_rda)) {
    message("== ", ano, ": suplemento técnico local encontrado ==")
    return(invisible(TRUE))
  }
  
  data_s3 <- s3_ultima_modificacao(s3_key_rda, S3_BUCKET)
  if (!is.null(data_s3)) {
    message("== ", ano, ": suplemento técnico encontrado no S3 — baixando ==")
    sincronizar_s3(local_rda, s3_key_rda, S3_BUCKET)
    return(invisible(TRUE))
  }
  
  # ----- Processamento necessário -----
  
  message("== ", ano, ": processamento do suplemento técnico necessário ==")
  
  # Sincronizar CSV bruto
  csv_nome   <- paste0("suplemento_cursos_tecnicos_", ano, ".csv")
  local_csv  <- file.path(DIR_BRUTO, csv_nome)
  s3_key_csv <- paste0("rawdata/mec_inep/", csv_nome)
  
  sincronizar_s3(local_csv, s3_key_csv, S3_BUCKET)
  
  if (!file.exists(local_csv)) {
    warning("CSV suplemento não encontrado para ", ano, ": ", local_csv)
    return(invisible(FALSE))
  }
  
  # Leitura do CSV (encoding ISO-8859-1, separador ponto-e-vírgula)
  message(">> Lendo CSV suplemento técnico: ", csv_nome, "...")
  df_proc <- readr::read_csv2(local_csv, locale = locale(encoding = "ISO-8859-1"),
                              show_col_types = FALSE) %>%
    rename(CO_MUN = CO_MUNICIPIO, ANO = NU_ANO_CENSO) %>%
    select(CO_MUN, CO_ENTIDADE, NO_ENTIDADE, ANO, TP_DEPENDENCIA,
           NO_AREA_CURSO_PROFISSIONAL, ID_AREA_CURSO_PROFISSIONAL,
           NO_CURSO_EDUC_PROFISSIONAL, CO_CURSO_EDUC_PROFISSIONAL,
           starts_with("QT_")) %>%
    mutate(across(starts_with("QT_"), ~ replace_na(., 0)))
  
  # -------------------------------------------------------------------------
  # Salvar .rda e upload condicional ao S3
  # -------------------------------------------------------------------------
  dir.create(dirname(local_rda), recursive = TRUE, showWarnings = FALSE)
  assign(nome_obj, df_proc)
  save(list = nome_obj, file = local_rda)
  message(">> Salvo: ", local_rda)
  
  upload_s3_se_diferente(local_rda, s3_key_rda, S3_BUCKET)
  
  return(invisible(TRUE))
}

###############################################################################
# EXECUÇÃO
###############################################################################

message("=== Processando suplemento técnico: ", paste(ANOS_SUPL, collapse = ", "), " ===")

for (ano in ANOS_SUPL) {
  tryCatch(
    processar_suplemento(ano),
    error = function(e) {
      warning("Erro ao processar suplemento ", ano, ": ", e$message)
    }
  )
}

###############################################################################
# VALIDAÇÃO
###############################################################################
for (ano in ANOS_SUPL) {
  short <- substr(as.character(ano), 3, 4)
  local_rda <- file.path(DIR_PROCESSADO, paste0("df_censo_supl_tec", short, ".rda"))
  if (file.exists(local_rda)) {
    load(local_rda)
    obj <- get(paste0("df_censo_supl_tec", short))
    message(">> df_censo_supl_tec", short, ": ", nrow(obj), " linhas, ",
            length(unique(obj$CO_MUN)), " municípios")
  }
}
message("=== prepara_censo_escolar_tecnico_01a.R concluído ===")