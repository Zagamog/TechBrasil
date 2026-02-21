###############################################################################
# econ_dynam_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Índice de Dinamismo Econômico Municipal
#
# USO NAS ABAS: D1 (Dinamismo Econômico)
#
# OBJETIVO:
#   Calcular índice de dinamismo econômico para todos os municípios
#   brasileiros, baseado em taxas de crescimento do PIB per capita
#   ano-a-ano, combinando dois períodos com pesos diferenciados.
#
#   METODOLOGIA:
#     1. Calcular crescimento anual do PIB pc por município
#     2. Separar em dois períodos:
#        - Período 1 (2002-2011): peso 1.0 — crescimento mais estável
#        - Período 2 (2012-2021): peso 1.5 — mais recente, mais relevante
#     3. Média ponderada dos crescimentos médios por período
#     4. Classificar em decis e percentis
#     5. Calcular contribuição ponderada por população
#
#   INTERPRETAÇÃO:
#     - Índice alto: município com crescimento consistente do PIB pc
#     - Índice baixo: estagnação ou retração econômica
#     - Decil 10: 10% mais dinâmicos
#     - Decil 1: 10% menos dinâmicos
#
#   Mínimo de 3 anos de dados por período para inclusão.
#
# INSUMOS PROCESSADOS (S3):
#   working/ibge/df_pibmunis.rda (de TB_municipios_01a.R)
#
# DADOS PROCESSADOS (S3 e local):
#   working/ibge/MUN_dyna02_21.rda
#
# VARIÁVEIS NA SAÍDA (usadas pela Aba D1 do Shiny):
#   CO_MUN, municipality_name, uf_name, avg_population,
#   period1_avg_growth, period2_avg_growth,
#   dynamism_index, dynamism_decile, dynamism_percentile,
#   pop_weighted_contribution
#
# CREDENCIAIS AWS: .env na raiz do projeto
# DEPENDÊNCIAS: dplyr, aws.s3, dotenv
# SAÍDA: MUN_dyna02_21.rda
###############################################################################

library(dplyr)
library(aws.s3)
library(dotenv)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

dotenv::load_dot_env()
S3_BUCKET <- "techbrazildata"

# Insumo
S3_INSUMO    <- "working/ibge/df_pibmunis.rda"
LOCAL_INSUMO <- "working/ibge/df_pibmunis.rda"

# Saída
S3_SAIDA    <- "working/ibge/MUN_dyna02_21.rda"
LOCAL_SAIDA <- "working/ibge/MUN_dyna02_21.rda"

# Parâmetros do índice de dinamismo
PERIODO1_ANOS <- 2002:2011
PERIODO2_ANOS <- 2012:2021
PERIODO1_PESO <- 1.0   # Peso para período mais antigo
PERIODO2_PESO <- 1.5   # Peso para período mais recente
MIN_ANOS      <- 3     # Mínimo de anos de dados por período

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
  message("=== MUN_dyna02_21 local encontrado e válido ===")
  
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
  message(">> MUN_dyna02_21 carregado: ", nrow(MUN_dyna02_21), " municípios")
  
} else {
  
  ###############################################################################
  # PASSO 0: SINCRONIZAR E CARREGAR INSUMO
  ###############################################################################
  
  message("=== Processamento necessário — sincronizando insumo ===")
  sincronizar_s3(LOCAL_INSUMO, S3_INSUMO, S3_BUCKET)
  load(LOCAL_INSUMO)  # → df_pibmunis
  
  ###############################################################################
  # PASSO 1: PREPARAR DADOS E CALCULAR POPULAÇÃO ESTIMADA
  ###############################################################################
  #
  # O IBGE não fornece população diretamente na base de PIB municipal.
  # A população é inferida pela relação: PIB total / PIB per capita.
  # PIB está em R$ mil, PIB_PC em R$ 1,00, portanto: pop = (PIB * 1000) / PIB_PC
  ###############################################################################
  
  message("=== PASSO 1: Preparação de dados e população estimada ===")
  
  df_prep <- df_pibmunis %>%
    mutate(
      populacao_estimada = ifelse(
        is.na(PIB_PC) | PIB_PC == 0,
        NA_real_,
        (PIB * 1000) / PIB_PC
      )
    ) %>%
    filter(!is.na(PIB_PC) &
             !is.na(populacao_estimada) &
             populacao_estimada > 0 &
             PIB_PC > 0)
  
  message(">> Municípios-ano com dados válidos: ", nrow(df_prep))
  
  ###############################################################################
  # PASSO 2: TAXAS DE CRESCIMENTO ANO-A-ANO
  ###############################################################################
  
  message("=== PASSO 2: Crescimento ano-a-ano do PIB per capita ===")
  
  growth_data <- df_prep %>%
    arrange(Código.do.Município, Ano) %>%
    group_by(Código.do.Município) %>%
    mutate(
      taxa_crescimento_pib_pc = (PIB_PC / lag(PIB_PC) - 1) * 100
    ) %>%
    filter(!is.na(taxa_crescimento_pib_pc)) %>%
    ungroup()
  
  message(">> Observações com crescimento calculado: ", nrow(growth_data))
  
  ###############################################################################
  # PASSO 3: MÉDIAS POR PERÍODO
  ###############################################################################
  
  message("=== PASSO 3: Médias por período ===")
  
  # Período 1 (2002-2011)
  periodo1 <- growth_data %>%
    filter(Ano %in% PERIODO1_ANOS) %>%
    group_by(Código.do.Município) %>%
    summarise(
      municipality_name     = first(Nome.do.Município),
      uf_name               = first(Nome.da.Unidade.da.Federação),
      pop_media_p1          = mean(populacao_estimada, na.rm = TRUE),
      period1_avg_growth    = mean(taxa_crescimento_pib_pc, na.rm = TRUE),
      period1_anos_disponiveis = n(),
      .groups = "drop"
    ) %>%
    filter(period1_anos_disponiveis >= MIN_ANOS)
  
  message(">> Período 1: ", nrow(periodo1), " municípios (mín. ", MIN_ANOS, " anos)")
  
  # Período 2 (2012-2021)
  periodo2 <- growth_data %>%
    filter(Ano %in% PERIODO2_ANOS) %>%
    group_by(Código.do.Município) %>%
    summarise(
      pop_media_p2          = mean(populacao_estimada, na.rm = TRUE),
      period2_avg_growth    = mean(taxa_crescimento_pib_pc, na.rm = TRUE),
      period2_anos_disponiveis = n(),
      .groups = "drop"
    ) %>%
    filter(period2_anos_disponiveis >= MIN_ANOS)
  
  message(">> Período 2: ", nrow(periodo2), " municípios (mín. ", MIN_ANOS, " anos)")
  
  ###############################################################################
  # PASSO 4: CALCULAR ÍNDICE DE DINAMISMO
  ###############################################################################
  
  message("=== PASSO 4: Índice de dinamismo ===")
  
  combined <- periodo1 %>%
    inner_join(periodo2, by = "Código.do.Município") %>%
    mutate(
      # População média entre os dois períodos (para ponderação)
      avg_population = (pop_media_p1 + pop_media_p2) / 2,
      
      # Índice de dinamismo: média ponderada dos crescimentos por período
      # Período 2 recebe peso maior (1.5) por ser mais recente
      dynamism_index = (period1_avg_growth * PERIODO1_PESO +
                          period2_avg_growth * PERIODO2_PESO) /
        (PERIODO1_PESO + PERIODO2_PESO),
      
      # Contribuição ponderada por população ao índice nacional
      pop_weighted_contribution = dynamism_index * avg_population
    ) %>%
    filter(!is.na(dynamism_index) & !is.infinite(dynamism_index))
  
  message(">> Municípios com dados em ambos os períodos: ", nrow(combined))
  
  ###############################################################################
  # PASSO 5: CLASSIFICAÇÃO EM DECIS E PERCENTIS
  ###############################################################################
  
  message("=== PASSO 5: Classificação ===")
  
  combined <- combined %>%
    arrange(dynamism_index) %>%
    mutate(
      dynamism_decile     = ntile(dynamism_index, 10),
      dynamism_percentile = percent_rank(dynamism_index) * 100
    ) %>%
    arrange(desc(dynamism_index))
  
  ###############################################################################
  # PASSO 6: SELECIONAR COLUNAS FINAIS E PADRONIZAR NOMES
  ###############################################################################
  
  MUN_dyna02_21 <- combined %>%
    transmute(
      CO_MUN = Código.do.Município,
      municipality_name,
      uf_name,
      avg_population,
      period1_avg_growth,
      period2_avg_growth,
      dynamism_index,
      dynamism_decile,
      dynamism_percentile,
      pop_weighted_contribution
    )
  
  ###############################################################################
  # SALVAR
  ###############################################################################
  
  dir.create(dirname(LOCAL_SAIDA), recursive = TRUE, showWarnings = FALSE)
  save(MUN_dyna02_21, file = LOCAL_SAIDA)
  message(">> Salvo: ", LOCAL_SAIDA)
  
  upload_s3_se_diferente(LOCAL_SAIDA, S3_SAIDA, S3_BUCKET)
  
} # Fim do bloco else (reprocessamento)

###############################################################################
# VALIDAÇÃO
###############################################################################

message(">> MUN_dyna02_21: ", nrow(MUN_dyna02_21), " municípios")
message(">> Índice — média: ", sprintf("%.2f", mean(MUN_dyna02_21$dynamism_index)),
        " | mediana: ", sprintf("%.2f", median(MUN_dyna02_21$dynamism_index)),
        " | DP: ", sprintf("%.2f", sd(MUN_dyna02_21$dynamism_index)))

# Média ponderada por população (índice nacional)
idx_nacional <- sum(MUN_dyna02_21$pop_weighted_contribution, na.rm = TRUE) /
  sum(MUN_dyna02_21$avg_population, na.rm = TRUE)
message(">> Índice nacional (pond. por população): ", sprintf("%.2f", idx_nacional))
message("=== econ_dynam_01a.R concluído ===")