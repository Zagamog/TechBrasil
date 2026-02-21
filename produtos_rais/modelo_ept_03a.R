###############################################################################
# modelo_ept_03a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Modelo Econométrico OLS com Efeitos Fixos — Resíduos para Aba C4
#
# USO NAS ABAS: C4 (Modelo Econométrico)
#
# OBJETIVO:
#   Estimar regressão OLS com efeitos fixos (fixest::feols) que relaciona
#   matrículas EPT com PIB per capita e alinhamento setorial.
#   Extrair resíduos para visualização na Aba C4 do Shiny.
#
#   ESPECIFICAÇÃO DO MODELO:
#     log(matrículas_EPT) ~ log(PIB_per_capita) + alinhamento_setorial
#                         | setor_econômico + UF + dependência + ano
#
#   - Variável dependente: log de matrículas EPT por setor/UF/dependência
#   - PIB per capita: nível de desenvolvimento econômico da UF
#   - Alinhamento setorial: razão entre proporção EPT e proporção econômica
#   - Efeitos fixos: setor, UF, dependência administrativa, ano
#   - Erros-padrão clusterizados por UF
#
#   Os resíduos positivos indicam UFs com mais matrículas EPT do que o
#   esperado pelo modelo; resíduos negativos indicam déficit relativo.
#
# INSUMOS PROCESSADOS (S3):
#   working/rais/df_model_ept1a.rda (de modelo_ept_01a.R)
#   working/ibge/df_codes_ibge.rda (de codes_ibge_01a.R — para NM_UF)
#
# DADOS PROCESSADOS (S3 e local):
#   working/rais/df_residuals_ols.rda
#
# VARIÁVEIS NA SAÍDA (usadas pela Aba C4 do Shiny):
#   ANO, SG_UF, NM_UF, TP_DEPENDENCIA, economic_sector,
#   QT_MAT_CURSO_TEC, residual, sector_alignment, pib_per_capita
#
# CREDENCIAIS AWS: .env na raiz do projeto
# DEPENDÊNCIAS: fixest, dplyr, aws.s3, dotenv
# SAÍDA: df_residuals_ols.rda
###############################################################################

library(fixest)
library(dplyr)
library(aws.s3)
library(dotenv)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

dotenv::load_dot_env()
S3_BUCKET <- "techbrazildata"

# Insumos
INSUMOS <- list(
  df_model     = list(local = "working/rais/df_model_ept1a.rda",
                      s3    = "working/rais/df_model_ept1a.rda"),
  df_codes     = list(local = "working/ibge/df_codes_ibge.rda",
                      s3    = "working/ibge/df_codes_ibge.rda")
)

# Saída
S3_SAIDA    <- "working/rais/df_residuals_ols.rda"
LOCAL_SAIDA <- "working/rais/df_residuals_ols.rda"

###############################################################################
# FUNÇÕES DE SINCRONIZAÇÃO S3
###############################################################################

s3_ultima_modificacao <- function(s3_key, bucket) {
  tryCatch({
    info <- suppressMessages(head_object(object = s3_key, bucket = bucket))
    as.POSIXct(attr(info, "last-modified"), format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  }, error = function(e) NULL)
}

sincronizar_s3 <- function(caminho_local, s3_key, bucket) {
  dir.create(dirname(caminho_local), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(caminho_local)) {
    message(">> Baixando do S3: ", s3_key)
    tryCatch({
      save_object(object = s3_key, bucket = bucket, file = caminho_local)
      message("   Download concluído.")
    }, error = function(e) stop("Erro ao baixar do S3: ", e$message))
  } else {
    message(">> Versão local encontrada: ", caminho_local)
  }
}

conteudo_identico_s3 <- function(caminho_local, s3_key, bucket) {
  tryCatch({
    md5_local <- tools::md5sum(caminho_local)
    info <- suppressMessages(head_object(object = s3_key, bucket = bucket))
    etag_s3 <- gsub('"', '', attr(info, "etag"))
    return(unname(md5_local) == etag_s3)
  }, error = function(e) return(FALSE))
}

upload_s3_se_diferente <- function(caminho_local, s3_key, bucket) {
  if (conteudo_identico_s3(caminho_local, s3_key, bucket)) {
    message(">> Upload dispensado (idêntico): ", s3_key)
    return(invisible(FALSE))
  }
  tryCatch({
    put_object(file = caminho_local, object = s3_key, bucket = bucket)
    message(">> Upload ao S3 concluído: ", s3_key)
    return(invisible(TRUE))
  }, error = function(e) {
    message(">> Erro no upload: ", e$message)
    return(invisible(FALSE))
  })
}

rda_valido <- function(caminho) {
  if (!file.exists(caminho)) return(FALSE)
  tryCatch({
    load(caminho, envir = new.env())
    TRUE
  }, error = function(e) {
    message(">> Arquivo corrompido: ", caminho)
    file.remove(caminho)
    FALSE
  })
}

###############################################################################
# VERIFICAR SE SAÍDA JÁ EXISTE
###############################################################################

if (rda_valido(LOCAL_SAIDA)) {
  message("=== df_residuals_ols local encontrado e válido ===")
  
  s3_existe <- tryCatch(
    suppressMessages(object_exists(object = S3_SAIDA, bucket = S3_BUCKET)),
    error = function(e) FALSE
  )
  if (isTRUE(s3_existe)) {
    data_s3 <- s3_ultima_modificacao(S3_SAIDA, S3_BUCKET)
    data_local <- file.mtime(LOCAL_SAIDA)
    if (!is.null(data_s3) && !is.na(data_local) && data_s3 > data_local) {
      message(">> S3 mais recente — baixando")
      sincronizar_s3(LOCAL_SAIDA, S3_SAIDA, S3_BUCKET)
    }
  } else {
    message(">> Não encontrado no S3 — fazendo upload: ", S3_SAIDA)
    tryCatch({
      put_object(file = LOCAL_SAIDA, object = S3_SAIDA, bucket = S3_BUCKET)
      message("   Upload concluído.")
    }, error = function(e) message("   Erro no upload: ", e$message))
  }
  
  load(LOCAL_SAIDA)
  message(">> df_residuals_ols carregado: ", nrow(df_residuals_ols), " observações")
  
} else {
  
  ###############################################################################
  # PASSO 0: SINCRONIZAR E CARREGAR INSUMOS
  ###############################################################################
  
  message("=== Processamento necessário — sincronizando insumos ===")
  for (nome in names(INSUMOS)) {
    sincronizar_s3(INSUMOS[[nome]]$local, INSUMOS[[nome]]$s3, S3_BUCKET)
  }
  
  load(INSUMOS$df_model$local)  # → df_model_ept1a
  load(INSUMOS$df_codes$local)  # → df_codes_ibge
  
  ###############################################################################
  # PASSO 1: ESTIMAÇÃO OLS COM EFEITOS FIXOS
  ###############################################################################
  #
  # Especificação:
  #   log(QT_MAT_CURSO_TEC) ~ log(pib_per_capita) + sector_alignment
  #                          | economic_sector + SG_UF + TP_DEPENDENCIA + ANO
  #
  # Efeitos fixos absorvem:
  #   - economic_sector: diferenças estruturais entre setores
  #   - SG_UF: heterogeneidade não-observada entre estados
  #   - TP_DEPENDENCIA: diferenças por rede (federal, estadual, privada, municipal)
  #   - ANO: choques temporais comuns (tendência nacional, reformas)
  #
  # Erros-padrão clusterizados por UF para correlação intra-estadual
  ###############################################################################
  
  message("=== PASSO 1: Estimação OLS com efeitos fixos ===")
  
  model_ols <- feols(
    log_QT_MAT_CURSO_TEC ~
      log(pib_per_capita) + sector_alignment |
      economic_sector + SG_UF + TP_DEPENDENCIA + ANO,
    data = df_model_ept1a,
    cluster = ~SG_UF
  )
  
  message(">> Modelo estimado: ", model_ols$nobs, " observações")
  message(">> R² ajustado: ", sprintf("%.4f", r2(model_ols)["ar2"]))
  
  # Sumário do modelo (exibido no console durante execução)
  summary(model_ols)
  
  ###############################################################################
  # PASSO 2: EXTRAIR RESÍDUOS E CONSTRUIR DATASET
  ###############################################################################
  
  message("=== PASSO 2: Extração de resíduos ===")
  
  residuals_ols <- residuals(model_ols)
  
  residuals_df_ols <- df_model_ept1a %>%
    mutate(residual = residuals_ols) %>%
    select(ANO, SG_UF, TP_DEPENDENCIA, economic_sector,
           QT_MAT_CURSO_TEC, residual, sector_alignment, pib_per_capita)
  
  ###############################################################################
  # PASSO 3: ADICIONAR NM_UF (NOME DA UF)
  ###############################################################################
  
  message("=== PASSO 3: Adicionando NM_UF ===")
  
  # Tabela de correspondência SG_UF → NM_UF
  uf_lookup <- df_codes_ibge %>%
    select(SG_UF, NM_UF) %>%
    distinct()
  
  df_residuals_ols <- residuals_df_ols %>%
    left_join(uf_lookup, by = "SG_UF")
  
  ###############################################################################
  # SALVAR
  ###############################################################################
  
  dir.create(dirname(LOCAL_SAIDA), recursive = TRUE, showWarnings = FALSE)
  save(df_residuals_ols, file = LOCAL_SAIDA)
  message(">> Salvo: ", LOCAL_SAIDA)
  
  upload_s3_se_diferente(LOCAL_SAIDA, S3_SAIDA, S3_BUCKET)
  
} # Fim do bloco else (reprocessamento)

###############################################################################
# VALIDAÇÃO
###############################################################################

message(">> df_residuals_ols: ", nrow(df_residuals_ols), " observações")
message(">> UFs: ", length(unique(df_residuals_ols$SG_UF)),
        " | Setores: ", length(unique(df_residuals_ols$economic_sector)))
message(">> Resíduos — média: ", sprintf("%.4f", mean(df_residuals_ols$residual)),
        " | DP: ", sprintf("%.4f", sd(df_residuals_ols$residual)))
message("=== modelo_ept_03a.R concluído ===")