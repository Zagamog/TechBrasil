###############################################################################
# cnct_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Catálogo Nacional de Cursos Técnicos (CNCT)
#
# USO NAS ABAS: Insumo para qbq_02a.R → C3, E1, E2
#               (não carregado diretamente pelo BM_FGV_Propag2.R)
#
# OBJETIVO:
#   Ler o catálogo oficial do CNCT (Catálogo Nacional de Cursos Técnicos)
#   e criar códigos hierárquicos para identificar cada curso de forma
#   única dentro da estrutura Eixo → Área → Curso.
#
#   O CNCT é o catálogo oficial do MEC que lista todos os cursos técnicos
#   de nível médio reconhecidos no Brasil. Cada curso pertence a um
#   Eixo Tecnológico (ex: "Ambiente e Saúde", "Informação e Comunicação")
#   e a uma Área Tecnológica dentro do eixo. O catálogo contém também o
#   perfil profissional de conclusão, ocupações CBO associadas,
#   infraestrutura mínima e legislação pertinente.
#
#   O código hierárquico IDX_EIXARECUR é composto por:
#     - eixo_code  (2 dígitos): posição ordinal do eixo tecnológico
#     - area_code  (2 dígitos): posição ordinal da área dentro do eixo
#     - curso_code (2 dígitos): posição ordinal do curso dentro da área
#   Exemplo: "010203" = Eixo 01, Área 02, Curso 03
#
#   Este índice permite fazer joins eficientes entre dados do Censo
#   Escolar (que usa nomes de curso) e o CNCT (que tem a classificação
#   oficial com CBO associadas).
#
# NOTA: Substitui cnct1a.py (Python). Toda a lógica é idêntica; a
#   conversão para R elimina a dependência de Python/pyreadr do pipeline.
#
# DADOS BRUTOS (S3):
#   rawdata/mec_outros/catalogo_cnct.csv
#   Origem: MEC — Catálogo Nacional de Cursos Técnicos (4ª edição, 2024)
#   Formato: CSV com separador ponto-e-vírgula, encoding ISO-8859-1
#
# DADOS PROCESSADOS (S3 e local):
#   working/mec_outros/df_cnct2025a.rda
#   working/mec_outros/df_cnct2025a.csv (cópia para inspeção manual)
#
# NOTA: Versões anteriores usavam .rds (via cnct1a.py/pyreadr).
#   Migrado para .rda para consistência com o restante do pipeline.
#   O arquivo .rds antigo no S3 pode ser removido após confirmação.
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
# DEPENDÊNCIAS: dplyr, readr, aws.s3, dotenv
# SAÍDA: df_cnct2025a.rds, df_cnct2025a.csv
#   Consumidos por qbq_02a.R para matching entre cursos CNCT e
#   cursos do Censo Escolar, e para enriquecer dados de matrícula
#   com perfil profissional, CBO associadas e infraestrutura mínima.
###############################################################################

library(dplyr)
library(readr)
library(aws.s3)
library(dotenv)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

dotenv::load_dot_env()

S3_BUCKET      <- "techbrazildata"
S3_BRUTO       <- "rawdata/mec_outros/catalogo_cnct.csv"
S3_PROCESSADO  <- "working/mec_outros/df_cnct2025a.rda"

LOCAL_BRUTO      <- "rawdata/mec_outros/catalogo_cnct.csv"
LOCAL_PROCESSADO <- "working/mec_outros/df_cnct2025a.rda"
LOCAL_CSV        <- "working/mec_outros/df_cnct2025a.csv"

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
# PASSO 1: VERIFICAR SE PROCESSADO JÁ EXISTE
###############################################################################

# Função para verificar se um .rda é válido (não corrompido)
rda_valido <- function(caminho) {
  if (!file.exists(caminho)) return(FALSE)
  tryCatch({
    load(caminho, envir = new.env())
    TRUE
  }, error = function(e) {
    message(">> Arquivo corrompido detectado: ", caminho, " — será reprocessado")
    file.remove(caminho)
    FALSE
  })
}

processado_ok <- FALSE

# Verificar arquivo local
if (rda_valido(LOCAL_PROCESSADO)) {
  message("=== df_cnct2025a local encontrado e válido ===")
  
  # Verificar se S3 tem a versão .rda
  s3_existe <- tryCatch(
    suppressMessages(object_exists(object = S3_PROCESSADO, bucket = S3_BUCKET)),
    error = function(e) FALSE
  )
  if (isTRUE(s3_existe)) {
    # Verificar se S3 é mais recente
    data_s3 <- s3_ultima_modificacao(S3_PROCESSADO, S3_BUCKET)
    data_local <- file.mtime(LOCAL_PROCESSADO)
    if (!is.null(data_s3) && !is.na(data_local) && data_s3 > data_local) {
      message(">> S3 mais recente — baixando")
      sincronizar_s3(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
    }
  } else {
    # Semeadura inicial: upload para S3
    message(">> Não encontrado no S3 — fazendo upload: ", S3_PROCESSADO)
    tryCatch({
      put_object(file = LOCAL_PROCESSADO, object = S3_PROCESSADO, bucket = S3_BUCKET)
      message("   Upload concluído.")
    }, error = function(e) {
      message("   Erro no upload: ", e$message)
    })
  }
  processado_ok <- TRUE
  
} else {
  # Sem arquivo local válido — tentar baixar do S3
  s3_existe <- tryCatch(
    suppressMessages(object_exists(object = S3_PROCESSADO, bucket = S3_BUCKET)),
    error = function(e) FALSE
  )
  if (isTRUE(s3_existe)) {
    message("=== Processado encontrado no S3 — baixando ===")
    sincronizar_s3(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
    if (rda_valido(LOCAL_PROCESSADO)) {
      processado_ok <- TRUE
    } else {
      message(">> Arquivo baixado do S3 também está corrompido — reprocessando")
    }
  }
}

###############################################################################
# PASSO 2: REPROCESSAR SE NECESSÁRIO
###############################################################################

if (!processado_ok) {
  
  message("=== Processamento CNCT necessário ===")
  
  # Sincronizar CSV bruto
  sincronizar_s3(LOCAL_BRUTO, S3_BRUTO, S3_BUCKET)
  
  # -------------------------------------------------------------------------
  # Ler o catálogo CNCT
  # Formato: CSV com separador ; e encoding ISO-8859-1 (padrão brasileiro)
  # -------------------------------------------------------------------------
  message(">> Lendo catalogo_cnct.csv...")
  df_full <- read_delim(LOCAL_BRUTO, delim = ";",
                        locale = locale(encoding = "ISO-8859-1"),
                        show_col_types = FALSE)
  
  message("   ", nrow(df_full), " cursos lidos, ", ncol(df_full), " colunas")
  
  # -------------------------------------------------------------------------
  # Criar códigos hierárquicos: Eixo (2d) + Área (2d) + Curso (2d)
  #
  # O código IDX_EIXARECUR identifica unicamente cada curso dentro da
  # hierarquia CNCT. É usado como chave de join com dados do Censo
  # Escolar e com a tabela de ocupações CBO.
  #
  # Lógica:
  #   1. eixo_code: numerar eixos na ordem de aparição (01, 02, ...)
  #   2. area_code: numerar áreas globalmente na ordem de aparição
  #   3. curso_code: numerar cursos dentro de cada área (01, 02, ...)
  #   4. IDX_EIXARECUR = concatenação dos três códigos
  # -------------------------------------------------------------------------
  
  # Código do eixo: posição ordinal de cada eixo tecnológico
  eixos_unicos <- unique(df_full$`Eixo Tecnológico`)
  eixo_map <- setNames(sprintf("%02d", seq_along(eixos_unicos)), eixos_unicos)
  
  df_full <- df_full %>%
    mutate(eixo_code = eixo_map[`Eixo Tecnológico`])
  
  # Código da área: posição ordinal global de cada combinação eixo+área
  df_full <- df_full %>%
    mutate(eixo_area_key = paste0(eixo_code, "||", `Área Tecnológica`))
  
  areas_unicas <- unique(df_full$eixo_area_key)
  area_map <- setNames(sprintf("%02d", seq_along(areas_unicas)), areas_unicas)
  
  df_full <- df_full %>%
    mutate(area_code = area_map[eixo_area_key])
  
  # Código do curso: posição ordinal dentro de cada área
  df_full <- df_full %>%
    group_by(eixo_area_key) %>%
    mutate(curso_code = sprintf("%02d", row_number())) %>%
    ungroup()
  
  # Índice composto: Eixo + Área + Curso
  df_full <- df_full %>%
    mutate(IDX_EIXARECUR = paste0(eixo_code, area_code, curso_code))
  
  # -------------------------------------------------------------------------
  # Selecionar e reordenar colunas para saída
  # -------------------------------------------------------------------------
  colunas_originais <- c(
    "Eixo Tecnológico", "Área Tecnológica", "Denominação do Curso",
    "Perfil Profissional de Conclusão", "Carga Horária Mínima",
    "Descrição Carga Horária Mínima", "Pré-Requisitos para Ingresso",
    "Itinerários Formativos", "Campo de Atuação", "Ocupações CBO Associadas",
    "Infraestrutura Mínima", "Legislação Profissional"
  )
  
  # Selecionar IDX + colunas originais + códigos hierárquicos
  df_cnct2025a <- df_full %>%
    select(IDX_EIXARECUR, any_of(colunas_originais),
           eixo_code, area_code, curso_code)
  
  # -------------------------------------------------------------------------
  # Salvar e upload condicional
  # -------------------------------------------------------------------------
  dir.create(dirname(LOCAL_PROCESSADO), recursive = TRUE, showWarnings = FALSE)
  
  save(df_cnct2025a, file = LOCAL_PROCESSADO)
  message(">> Salvo: ", LOCAL_PROCESSADO)
  
  write_csv(df_cnct2025a, file = LOCAL_CSV)
  message(">> Salvo: ", LOCAL_CSV)
  
  upload_s3_se_diferente(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
  
} else {
  message("=== Carregando df_cnct2025a existente ===")
}

###############################################################################
# CARREGAR E VALIDAR
###############################################################################

load(LOCAL_PROCESSADO)  # carrega df_cnct2025a

n_eixos  <- length(unique(df_cnct2025a$eixo_code))
n_cursos <- nrow(df_cnct2025a)
message(">> df_cnct2025a: ", n_cursos, " cursos em ", n_eixos, " eixos tecnológicos")
message(">> Eixos: ", paste(unique(df_cnct2025a$`Eixo Tecnológico`), collapse = ", "))
message("=== cnct_01a.R concluído ===")