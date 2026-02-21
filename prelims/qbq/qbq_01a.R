###############################################################################
# qbq_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Quadro Brasileiro de Qualificações (QBQ)
#
# USO NAS ABAS: Insumo para qbq_02a.R → C3, E1, E2
#               (não carregado diretamente pelo BM_FGV_Propag2.R)
#
# OBJETIVO:
#   Ler a planilha do QBQ (Quadro Brasileiro de Qualificações) e extrair
#   as 6 dimensões do quadro: Ocupações CBO, Conhecimento I e II,
#   Habilidades, Atitudes e Ocupações não classificadas.
#
#   O QBQ é um instrumento do Ministério do Trabalho e Emprego que
#   sistematiza as relações entre ocupações (classificadas pelo CBO —
#   Classificação Brasileira de Ocupações) e os requisitos de formação
#   profissional necessários para exercê-las. É utilizado neste projeto
#   para conectar a demanda por ocupações (dados RAIS/PNAD-C) com a
#   oferta de cursos técnicos (dados CNCT/Censo Escolar).
#
#   As 6 planilhas da fonte correspondem às dimensões do QBQ:
#     - Ocupação: CBO código e descrição, nível de qualificação
#     - Conhecimento I: conhecimentos técnicos por ocupação
#     - Conhecimento II: conhecimentos complementares por ocupação
#     - Habilidade: habilidades requeridas por ocupação
#     - Atitude: atitudes e competências socioemocionais por ocupação
#     - Ocupações não classificadas: ocupações ainda sem enquadramento no QBQ
#
# DADOS BRUTOS (S3):
#   rawdata/qbq/qbq_2025.xlsx
#   Origem: Ministério do Trabalho e Emprego — QBQ
#   https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/quadro-brasileiro-de-qualificacoes-qbq
#   Download: julho 2025
#
# DADOS PROCESSADOS (S3 e local):
#   working/qbq/qbq_ocup.rda           (Ocupações CBO)
#   working/qbq/qbq_conhecimento1.rda  (Conhecimento I)
#   working/qbq/qbq_conhecimento2.rda  (Conhecimento II)
#   working/qbq/qbq_habilidade.rda     (Habilidades)
#   working/qbq/qbq_atitude.rda        (Atitudes)
#   working/qbq/qbq_ocnaoc.rda         (Ocupações não classificadas)
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
# DEPENDÊNCIAS: dplyr, openxlsx, aws.s3, dotenv
# SAÍDA: 6 arquivos .rda (ver acima)
#   Consumidos por qbq_02a.R para matching entre ocupações CBO e
#   cursos técnicos do CNCT (Catálogo Nacional de Cursos Técnicos).
###############################################################################

library(dplyr)
library(openxlsx)
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
S3_QBQ_XLSX <- "rawdata/qbq/qbq_2025.xlsx"

# Caminhos locais
LOCAL_QBQ_XLSX <- "rawdata/qbq/qbq_2025.xlsx"
DIR_PROCESSADO <- "working/qbq"

# Mapeamento: nome do objeto R → nome da planilha no xlsx
# A ordem corresponde às abas da planilha QBQ
PLANILHAS <- list(
  qbq_ocup          = "Ocupação",
  qbq_conhecimento1 = "Conhecimento I",
  qbq_conhecimento2 = "Conhecimento II",
  qbq_habilidade    = "Habilidade",
  qbq_atitude       = "Atitude",
  qbq_ocnaoc        = "Ocupações não classificada"
)

###############################################################################
# FUNÇÕES DE SINCRONIZAÇÃO S3
###############################################################################

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
    info <- suppressMessages(head_object(object = s3_key, bucket = bucket))
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

todos_locais <- all(sapply(names(PLANILHAS), function(nome) {
  file.exists(file.path(DIR_PROCESSADO, paste0(nome, ".rda")))
}))

if (todos_locais) {
  message("=== Todos os arquivos QBQ processados já existem localmente ===")
  
  # Verificar se S3 tem versão mais recente; upload se faltam no S3
  for (nome in names(PLANILHAS)) {
    local_path <- file.path(DIR_PROCESSADO, paste0(nome, ".rda"))
    s3_key <- paste0("working/qbq/", nome, ".rda")
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
      }
    } else {
      # Semeadura inicial: upload se não existe no S3
      message(">> Não encontrado no S3 — fazendo upload: ", s3_key)
      tryCatch({
        put_object(file = local_path, object = s3_key, bucket = S3_BUCKET)
        message("   Upload concluído.")
      }, error = function(e) {
        message("   Erro no upload: ", e$message)
      })
    }
  }
  
  # Carregar todos os .rda
  for (nome in names(PLANILHAS)) {
    load(file.path(DIR_PROCESSADO, paste0(nome, ".rda")), envir = .GlobalEnv)
  }
  
  message("=== Arquivos QBQ carregados ===")
  
} else {
  
  ###########################################################################
  # PASSO 2: REPROCESSAR
  ###########################################################################
  
  message("=== Processamento QBQ necessário ===")
  
  # Sincronizar xlsx bruto
  sincronizar_s3(LOCAL_QBQ_XLSX, S3_QBQ_XLSX, S3_BUCKET)
  
  # -------------------------------------------------------------------------
  # Ler cada planilha do xlsx e salvar como .rda
  #
  # Cada planilha do QBQ contém uma dimensão da qualificação profissional.
  # A planilha "Ocupação" é a mais importante para o matching com CNCT,
  # pois contém os códigos CBO associados a cada nível de qualificação.
  # -------------------------------------------------------------------------
  dir.create(DIR_PROCESSADO, recursive = TRUE, showWarnings = FALSE)
  
  for (nome in names(PLANILHAS)) {
    nome_planilha <- PLANILHAS[[nome]]
    message(">> Lendo planilha: '", nome_planilha, "' → ", nome, ".rda")
    
    df <- tryCatch(
      read.xlsx(LOCAL_QBQ_XLSX, sheet = nome_planilha),
      error = function(e) {
        warning("Erro ao ler planilha '", nome_planilha, "': ", e$message)
        NULL
      }
    )
    
    if (!is.null(df)) {
      assign(nome, df, envir = .GlobalEnv)
      local_path <- file.path(DIR_PROCESSADO, paste0(nome, ".rda"))
      save(list = nome, file = local_path)
      message("   Salvo: ", local_path, " (", nrow(df), " linhas, ",
              ncol(df), " colunas)")
    }
  }
  
  # -------------------------------------------------------------------------
  # Upload condicional de todos os arquivos
  # -------------------------------------------------------------------------
  for (nome in names(PLANILHAS)) {
    local_path <- file.path(DIR_PROCESSADO, paste0(nome, ".rda"))
    s3_key <- paste0("working/qbq/", nome, ".rda")
    if (file.exists(local_path)) {
      upload_s3_se_diferente(local_path, s3_key, S3_BUCKET)
    }
  }
}

###############################################################################
# VALIDAÇÃO
###############################################################################
for (nome in names(PLANILHAS)) {
  if (exists(nome)) {
    df <- get(nome)
    message(">> ", nome, ": ", nrow(df), " linhas, ", ncol(df), " colunas")
  }
}
message("=== qbq_01a.R concluído ===")