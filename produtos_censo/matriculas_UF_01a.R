###############################################################################
# matriculas_UF_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Matrículas do Censo Escolar Agregadas por UF e Rede Administrativa
#
# USO NAS ABAS: B1 (Financiamento PROPAG), C1 (Projeção EPT),
#               C2 (Planificação), C3 (Oferta EPT)
#
# OBJETIVO:
#   Agregar microdados anuais do Censo Escolar (df_censo07–24.rda) por UF
#   em seis categorias de rede administrativa:
#     UF_TUDO   = todas as redes
#     UF_REDEST = rede estadual (TP_DEPENDENCIA=1)
#     UF_REDMUN = rede municipal (TP_DEPENDENCIA=2)
#     UF_REDFED = rede federal (TP_DEPENDENCIA=3)
#     UF_REDPRI = rede privada (TP_DEPENDENCIA=4)
#     UF_PUB    = redes públicas combinadas (federal + estadual + municipal)
#   Inclui variáveis derivadas: QT_MAT_PROF_TEC_PROPAG (técnico sem
#   Normal/Magistério), QT_MAT_EJA_ARTIC_EPT (EJA articulada com EPT).
#
# INSUMOS PROCESSADOS (S3):
#   working/mec_inep/df_censoXX.rda (18 arquivos, gerados por
#     prepara_censo_escolar_01a.R)
#   working/ibge/df_codes_ibge.rda (gerado por codes_ibge_01a.R)
#
# DADOS PROCESSADOS (S3 e local):
#   working/mec_inep/df_censo_UF.rda
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
# DEPENDÊNCIAS: dplyr, purrr, tidyr, aws.s3, dotenv
# SAÍDA: df_censo_UF.rda
#   Carregado diretamente por BM_FGV_Propag2.R.
#   Aba B1: denominador de matrículas para cálculos financeiros PROPAG.
#   Abas C1–C3: séries históricas de oferta EPT por UF e rede.
#   Também insumo para censo_garabed_01a.R (Meta 11a).
###############################################################################

library(dplyr)
library(purrr)
library(tidyr)
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
S3_CODES_IBGE  <- "working/ibge/df_codes_ibge.rda"
S3_PROCESSADO  <- "working/mec_inep/df_censo_UF.rda"

# Caminhos locais (relativos à raiz do projeto)
LOCAL_CODES_IBGE  <- "working/ibge/df_codes_ibge.rda"
LOCAL_PROCESSADO  <- "working/mec_inep/df_censo_UF.rda"

# Anos do Censo Escolar a processar
ANOS <- 2007:2024

###############################################################################
# FUNÇÕES DE SINCRONIZAÇÃO S3
###############################################################################

s3_ultima_modificacao <- function(s3_key, bucket) {
  tryCatch({
    info <- head_object(object = s3_key, bucket = bucket)
    as.POSIXct(attr(info, "last-modified"), format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  }, error = function(e) {
    NULL
  })
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
# PASSO 1: SINCRONIZAR DADOS PROCESSADOS
###############################################################################

processado_atualizado <- FALSE

if (file.exists(LOCAL_PROCESSADO)) {
  data_local <- file.mtime(LOCAL_PROCESSADO)
  data_s3 <- s3_ultima_modificacao(S3_PROCESSADO, S3_BUCKET)
  
  if (!is.null(data_s3) && data_s3 > data_local) {
    message("=== df_censo_UF no S3 mais recente — baixando ===")
    sincronizar_s3(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
    processado_atualizado <- TRUE
  } else {
    message("=== df_censo_UF local está atualizado ===")
    processado_atualizado <- TRUE
  }
} else {
  data_s3 <- s3_ultima_modificacao(S3_PROCESSADO, S3_BUCKET)
  if (!is.null(data_s3)) {
    message("=== df_censo_UF encontrado no S3 — baixando ===")
    sincronizar_s3(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
    processado_atualizado <- TRUE
  }
}

###############################################################################
# PASSO 2: REPROCESSAR SE NECESSÁRIO
###############################################################################

if (!processado_atualizado) {
  
  message("=== Processamento necessário — sincronizando insumos ===")
  
  # -------------------------------------------------------------------------
  # Sincronizar geocódigos e carregar
  # -------------------------------------------------------------------------
  sincronizar_s3(LOCAL_CODES_IBGE, S3_CODES_IBGE, S3_BUCKET)
  load(LOCAL_CODES_IBGE)
  
  # Tabela de merge geográfico (CO_MUN → SG_UF, NM_UF)
  geo_vars <- df_codes_ibge %>%
    select(CO_MUN, SG_UF, NM_UF) %>%
    distinct()
  
  # -------------------------------------------------------------------------
  # Sincronizar arquivos anuais do Censo Escolar
  # NOTA: nomes S3 usam censo_escolar_YYYY.rda; nomes locais usam df_censoXX.rda
  # -------------------------------------------------------------------------
  for (ano in ANOS) {
    short <- substr(as.character(ano), 3, 4)
    local_rda <- file.path("working/mec_inep", paste0("df_censo", short, ".rda"))
    s3_key    <- paste0("working/mec_inep/censo_escolar_", ano, ".rda")
    if (!file.exists(local_rda)) {
      tryCatch(
        sincronizar_s3(local_rda, s3_key, S3_BUCKET),
        error = function(e) message("   Não disponível: ", ano)
      )
    }
  }
  
  # -------------------------------------------------------------------------
  # Definição das variáveis de matrícula
  # -------------------------------------------------------------------------
  
  # Variáveis base — presentes em todos os anos, NA→0
  qt_vars_base <- c("QT_MAT_PROF_TEC", "QT_MAT_PROF", "QT_MAT_MED",
                    "QT_MAT_BAS", "QT_MAT_FUND", "QT_MAT_EJA",
                    "QT_MAT_INF", "QT_MAT_ESP")
  
  # Variáveis novas — presentes apenas em anos recentes
  qt_vars_new <- c("QT_MAT_PROF_TEC_SUBS", "QT_MAT_EJA_MED_TEC",
                   "QT_MAT_EJA_FUND_FIC", "QT_MAT_EJA_MED_FIC",
                   "QT_MAT_PROF_TEC_MED", "QT_MAT_MED_CT",
                   "QT_MAT_EJA_MED", "QT_MAT_MED_PROP")
  
  # Variáveis derivadas — construídas no processamento
  qt_vars_extra <- c("QT_MAT_PROF_TEC_PROPAG", "QT_MAT_EJA_ARTIC_EPT",
                     "QT_MAT_MED_NM")
  
  qt_vars_all <- unique(c(qt_vars_base, qt_vars_new, qt_vars_extra))
  
  # -------------------------------------------------------------------------
  # Função auxiliar: processar um ano para uma agregação
  # Carrega df_censoXX.rda, faz merge geográfico, calcula derivadas,
  # agrega por UF
  # -------------------------------------------------------------------------
  processar_ano_uf <- function(ano, filtro_tp = NULL, nome_agreg = "UF_TUDO") {
    short <- substr(as.character(ano), 3, 4)
    arq <- file.path("working/mec_inep", paste0("df_censo", short, ".rda"))
    if (!file.exists(arq)) return(NULL)
    
    load(arq)
    df <- get(paste0("df_censo", short))
    
    # Merge geográfico (substituir SG_UF/NM_UF do INEP por geocódigos IBGE)
    df <- df %>%
      select(-c(SG_UF, CO_UF, NO_UF, NO_MUNICIPIO)) %>%
      rename(CO_MUN = CO_MUNICIPIO) %>%
      mutate(ANO = ano) %>%
      left_join(geo_vars, by = "CO_MUN")
    
    # Filtrar por rede administrativa se solicitado
    if (!is.null(filtro_tp)) {
      if (!"TP_DEPENDENCIA" %in% names(df)) return(NULL)
      df <- df %>% filter(TP_DEPENDENCIA == filtro_tp)
    }
    
    # Garantir que todas as variáveis esperadas existam
    missing_vars <- setdiff(qt_vars_all, names(df))
    df[missing_vars] <- NA
    
    # Fallback: QT_MAT_MED_NM (Normal/Magistério)
    if (!"QT_MAT_MED_NM" %in% names(df) || all(is.na(df$QT_MAT_MED_NM))) {
      if (all(c("QT_MAT_PROF", "QT_MAT_PROF_TEC") %in% names(df))) {
        df$QT_MAT_MED_NM <- df$QT_MAT_PROF - df$QT_MAT_PROF_TEC
      } else {
        df$QT_MAT_MED_NM <- NA
      }
    }
    
    # Variável derivada: QT_MAT_PROF_TEC_PROPAG (técnico sem Normal/Magistério)
    if (all(c("QT_MAT_PROF_TEC", "QT_MAT_MED_NM") %in% names(df))) {
      df$QT_MAT_PROF_TEC_PROPAG <- df$QT_MAT_PROF_TEC - df$QT_MAT_MED_NM
    }
    
    # Variável derivada: QT_MAT_EJA_ARTIC_EPT (EJA articulada com EPT)
    if (all(c("QT_MAT_EJA_FUND_FIC", "QT_MAT_EJA_MED_FIC") %in% names(df))) {
      df$QT_MAT_EJA_ARTIC_EPT <- df$QT_MAT_EJA_FUND_FIC + df$QT_MAT_EJA_MED_FIC
    }
    
    # Preencher variáveis base com 0 (não as novas/derivadas)
    df <- df %>%
      mutate(across(any_of(qt_vars_base), ~ replace_na(., 0)))
    
    # Agregar por UF
    df %>%
      group_by(SG_UF, NM_UF) %>%
      summarise(across(any_of(qt_vars_all), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(ANO = ano, AGREG = nome_agreg)
  }
  
  # -------------------------------------------------------------------------
  # Função auxiliar: substituir colunas inteiramente zero por NA, por ano
  # (variáveis que não existiam em determinado ano produzem zeros na soma)
  # -------------------------------------------------------------------------
  substituir_zeros_por_na <- function(df, vars) {
    map_dfr(unique(df$ANO), function(yr) {
      df_ano <- df %>% filter(ANO == yr)
      
      cols_zero <- df_ano %>%
        select(any_of(vars)) %>%
        summarise(across(everything(), ~ all(. == 0))) %>%
        pivot_longer(everything(), names_to = "var", values_to = "tudo_zero") %>%
        filter(tudo_zero) %>%
        pull(var)
      
      df_ano %>%
        mutate(across(all_of(cols_zero), ~ na_if(., 0)))
    })
  }
  
  # =========================================================================
  # AGREGAÇÃO 1: TODAS AS REDES (UF_TUDO)
  # =========================================================================
  
  message(">> Agregando: UF_TUDO (todas as redes)...")
  df_tudos_aggUF <- map_dfr(ANOS, ~ processar_ano_uf(.x))
  
  # =========================================================================
  # AGREGAÇÃO 2: POR REDE ADMINISTRATIVA
  # =========================================================================
  
  # Códigos TP_DEPENDENCIA INEP:
  #   1 = Estadual, 2 = Municipal, 3 = Federal, 4 = Privada
  tp_dependencias <- list(
    "UF_REDEST" = 1,
    "UF_REDMUN" = 2,
    "UF_REDFED" = 3,
    "UF_REDPRI" = 4
  )
  
  message(">> Agregando: por rede administrativa...")
  df_red_aggUF <- map_dfr(names(tp_dependencias), function(nome_red) {
    tp_val <- tp_dependencias[[nome_red]]
    map_dfr(ANOS, ~ processar_ano_uf(.x, filtro_tp = tp_val, nome_agreg = nome_red))
  })
  
  # =========================================================================
  # AGREGAÇÃO 3: REDES PÚBLICAS COMBINADAS (UF_PUB)
  # =========================================================================
  
  message(">> Agregando: UF_PUB (redes públicas)...")
  df_pub_aggUF <- df_red_aggUF %>%
    filter(AGREG %in% c("UF_REDFED", "UF_REDMUN", "UF_REDEST")) %>%
    group_by(SG_UF, NM_UF, ANO) %>%
    summarise(across(any_of(qt_vars_all), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(AGREG = "UF_PUB")
  
  # =========================================================================
  # LIMPEZA: SUBSTITUIR ZEROS POR NA EM VARIÁVEIS INEXISTENTES POR ANO
  # =========================================================================
  
  message(">> Limpando zeros espúrios...")
  df_tudos_aggUF <- substituir_zeros_por_na(df_tudos_aggUF, qt_vars_all)
  df_red_aggUF   <- substituir_zeros_por_na(df_red_aggUF, qt_vars_all)
  df_pub_aggUF   <- substituir_zeros_por_na(df_pub_aggUF, qt_vars_all)
  
  # =========================================================================
  # COMBINAR TODAS AS AGREGAÇÕES
  # =========================================================================
  
  df_censo_UF <- bind_rows(df_tudos_aggUF, df_red_aggUF, df_pub_aggUF)
  
  # -------------------------------------------------------------------------
  # Salvar localmente e upload condicional ao S3
  # -------------------------------------------------------------------------
  dir.create(dirname(LOCAL_PROCESSADO), recursive = TRUE, showWarnings = FALSE)
  save(df_censo_UF, file = LOCAL_PROCESSADO)
  message(">> Salvo localmente: ", LOCAL_PROCESSADO)
  
  upload_s3_se_diferente(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
  
} else {
  # Arquivo processado já está atualizado — apenas carregar
  message("=== Carregando df_censo_UF existente ===")
  load(LOCAL_PROCESSADO)
}

###############################################################################
# VALIDAÇÃO
###############################################################################
message(">> df_censo_UF: ", nrow(df_censo_UF), " linhas, ",
        length(unique(df_censo_UF$SG_UF)), " UFs, ",
        length(unique(df_censo_UF$AGREG)), " agregações (",
        paste(unique(df_censo_UF$AGREG), collapse = ", "), "), ",
        min(df_censo_UF$ANO), "-", max(df_censo_UF$ANO))
message("=== matriculas_UF_01a.R concluído ===")
