###############################################################################
# codes_ibge_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Códigos Geográficos IBGE para Municípios Brasileiros
#
# USO NAS ABAS: A2 (EPT e População), D1 (Dinamismo Econômico),
#               D2 (APLs), D3 (Informalidade)
#
# OBJETIVO:
#   Criar dataframe padronizado de códigos geográficos a partir de
#   df_pibmunis (PIB municipal IBGE). Inclui todas as agregações
#   administrativas: Grande Região, UF, Município, Mesorregião,
#   Microrregião, Região Geográfica Imediata/Intermediária,
#   Concentração Urbana, Arranjo Populacional, Região Rural,
#   Amazônia Legal, Semiárido e Cidade-Região de São Paulo.
#   Adiciona CO_MUN6 (código municipal de 6 dígitos) para joins com RAIS.
#
# DADOS BRUTOS (S3):
#   working/ibge/df_pibmunis.rda (gerado por TB_municipios_01a.R)
#
# DADOS PROCESSADOS (S3 e local):
#   working/ibge/df_codes_ibge.rda
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
# DEPENDÊNCIAS: dplyr, aws.s3, dotenv, sjlabelled
# SAÍDA: df_codes_ibge.rda
#   Carregado diretamente por BM_FGV_Propag2.R (sem produtos intermediários).
#   Aba A2: geocódigos regionais para agregação de matrículas EPT.
#   Abas D1–D3: hierarquia geográfica para análises municipais
#               (dinamismo, APLs, informalidade).
###############################################################################

library(dplyr)
library(aws.s3)
library(dotenv)
library(sjlabelled)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

# Carregar credenciais AWS do arquivo .env na raiz do projeto
dotenv::load_dot_env()

# Bucket S3 do projeto
S3_BUCKET <- "techbrazildata"

# Caminhos S3
S3_INSUMO     <- "working/ibge/df_pibmunis.rda"
S3_PROCESSADO <- "working/ibge/df_codes_ibge.rda"

# Caminhos locais (relativos à raiz do projeto)
LOCAL_INSUMO     <- "working/ibge/df_pibmunis.rda"
LOCAL_PROCESSADO <- "working/ibge/df_codes_ibge.rda"

###############################################################################
# FUNÇÕES DE SINCRONIZAÇÃO S3
###############################################################################

# Obter data de última modificação de um objeto no S3
# Retorna POSIXct ou NULL se o objeto não existir
s3_ultima_modificacao <- function(s3_key, bucket) {
  tryCatch({
    info <- head_object(object = s3_key, bucket = bucket)
    as.POSIXct(attr(info, "last-modified"), format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  }, error = function(e) {
    NULL
  })
}

# Sincronizar arquivo do S3 para local
# Baixa se: (a) arquivo local não existe, ou (b) versão S3 é mais recente
# Retorna TRUE se houve download, FALSE se versão local está atualizada
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
# Retorna TRUE se o conteúdo é idêntico, FALSE se diferente ou S3 não existe
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
  
  message("=== Processamento necessário — sincronizando insumo ===")
  
  # Sincronizar df_pibmunis.rda do S3
  sincronizar_s3(LOCAL_INSUMO, S3_INSUMO, S3_BUCKET)
  load(LOCAL_INSUMO)
  
  # -------------------------------------------------------------------------
  # Renomear colunas para padrão padronizado
  #
  # CONVENÇÃO DE NOMES DAS COLUNAS GEOGRÁFICAS:
  #   CO_  = Código numérico (ex: CO_UF, CO_MUN, CO_RGIMED)
  #   NM_  = Nome por extenso (ex: NM_UF, NM_MUN, NM_RGIMED)
  #   SG_  = Sigla (ex: SG_UF)
  #   MUN_ = Indicador binário municipal (ex: MUN_AMZ_LEG, MUN_SEMIARIDO)
  #   TP_  = Tipo/categoria (ex: TP_GMCONC_URBANA)
  #   HRQ_ = Hierarquia (ex: HRQ_11URBANA, HRQ_5URBANA)
  #   CDN_ = Classificação (ex: CDN_6REG_RURAL)
  #   CO_MUN6 = Código municipal 6 dígitos (sem dígito verificador, para join RAIS)
  # -------------------------------------------------------------------------
  message(">> Renomeando colunas...")
  
  df_codes_ibge <- df_pibmunis %>%
    rename(
      CO_5RGRANDE    = `Código.da.Grande.Região`,
      NM_5RGRANDE    = `Nome.da.Grande.Região`,
      CO_UF          = `Código.da.Unidade.da.Federação`,
      SG_UF          = `Sigla.da.Unidade.da.Federação`,
      NM_UF          = `Nome.da.Unidade.da.Federação`,
      CO_MUN         = `Código.do.Município`,
      NM_MUN         = `Nome.do.Município`,
      NM_REGMET      = `Região.Metropolitana`,
      CO_MESOREG     = `Código.da.Mesorregião`,
      NM_MESOREG     = `Nome.da.Mesorregião`,
      CO_MICROREG    = `Código.da.Microrregião`,
      NM_MICROREG    = `Nome.da.Microrregião`,
      CO_RGIMED      = `Código.da.Região.Geográfica.Imediata`,
      NM_RGIMED      = `Nome.da.Região.Geográfica.Imediata`,
      MUN_RGIM_EOP   = `Município.da.Região.Geográfica.Imediata`,
      CO_RGINTM      = `Código.da.Região.Geográfica.Intermediária`,
      NM_RGIINTM     = `Nome.da.Região.Geográfica.Intermediária`,
      MUN_RGIN_EOP   = `Município.da.Região.Geográfica.Intermediária`,
      CO_CONC_URBANA = `Código.Concentração.Urbana`,
      NM_CONC_URBANA = `Nome.Concentração.Urbana`,
      TP_GMCONC_URBANA = `Tipo.Concentração.Urbana`,
      HRQ_11URBANA   = `Hierarquia.Urbana`,
      HRQ_5URBANA    = `Hierarquia.Urbana..principais.categorias.`,
      CO_ARR_POP     = `Código.Arranjo.Populacional`,
      NM_ARR_POP     = `Nome.Arranjo.Populacional`,
      CO_REG_RURAL   = `Código.da.Região.Rural`,
      NM_REG_RURAL   = `Nome.da.Região.Rural`,
      CDN_6REG_RURAL = `Região.rural..segundo.classificação.do.núcleo.`,
      MUN_AMZ_LEG    = `Amazônia.Legal`,
      MUN_SEMIARIDO  = `Semiárido`,
      MUN_CIDADE_SP  = `Cidade.Região.de.São.Paulo`
    )
  
  # -------------------------------------------------------------------------
  # Converter variáveis categóricas Sim/Não para 0/1
  # -------------------------------------------------------------------------
  df_codes_ibge$MUN_AMZ_LEG   <- ifelse(df_codes_ibge$MUN_AMZ_LEG   == "Sim", 1, 0)
  df_codes_ibge$MUN_SEMIARIDO <- ifelse(df_codes_ibge$MUN_SEMIARIDO == "Sim", 1, 0)
  df_codes_ibge$MUN_CIDADE_SP <- ifelse(df_codes_ibge$MUN_CIDADE_SP == "Sim", 1, 0)
  
  # -------------------------------------------------------------------------
  # Selecionar colunas relevantes e remover duplicatas
  # -------------------------------------------------------------------------
  df_codes_ibge <- df_codes_ibge %>%
    select(
      Ano,
      CO_5RGRANDE, NM_5RGRANDE,
      CO_UF, SG_UF, NM_UF,
      CO_MUN, NM_MUN,
      NM_REGMET,
      CO_MESOREG, NM_MESOREG,
      CO_MICROREG, NM_MICROREG,
      CO_RGIMED, NM_RGIMED, MUN_RGIM_EOP,
      CO_RGINTM, NM_RGIINTM, MUN_RGIN_EOP,
      CO_CONC_URBANA, NM_CONC_URBANA, TP_GMCONC_URBANA,
      HRQ_11URBANA, HRQ_5URBANA,
      CO_ARR_POP, NM_ARR_POP,
      CO_REG_RURAL, NM_REG_RURAL, CDN_6REG_RURAL,
      MUN_AMZ_LEG, MUN_SEMIARIDO, MUN_CIDADE_SP
    ) %>%
    distinct()
  
  # -------------------------------------------------------------------------
  # Adicionar rótulos descritivos às variáveis
  # -------------------------------------------------------------------------
  df_codes_ibge <- df_codes_ibge %>%
    set_label(list(
      Ano            = "Ano de referência do PIB Municipal",
      CO_5RGRANDE    = "Código da Grande Região",
      NM_5RGRANDE    = "Nome da Grande Região",
      CO_UF          = "Código da Unidade da Federação",
      SG_UF          = "Sigla da Unidade da Federação",
      NM_UF          = "Nome da Unidade da Federação",
      CO_MUN         = "Código do Município (IBGE)",
      NM_MUN         = "Nome do Município",
      NM_REGMET      = "Nome da Região Metropolitana",
      CO_MESOREG     = "Código da Mesorregião",
      NM_MESOREG     = "Nome da Mesorregião",
      CO_MICROREG    = "Código da Microrregião",
      NM_MICROREG    = "Nome da Microrregião",
      CO_RGIMED      = "Código da Região Geográfica Imediata",
      NM_RGIMED      = "Nome da Região Geográfica Imediata",
      MUN_RGIM_EOP   = "Município da Região Geográfica Imediata Entorno ou Polo",
      CO_RGINTM      = "Código da Região Geográfica Intermediária",
      NM_RGIINTM     = "Nome da Região Geográfica Intermediária",
      MUN_RGIN_EOP   = "Município da Região Geográfica Intermediária do Entorno ou Polo",
      CO_CONC_URBANA = "Código da Concentração Urbana",
      NM_CONC_URBANA = "Nome da Concentração Urbana",
      TP_GMCONC_URBANA = "Tipo de Concentração Urbana",
      HRQ_11URBANA   = "Hierarquia Urbana (11 categorias)",
      HRQ_5URBANA    = "Hierarquia Urbana (5 categorias principais)",
      CO_ARR_POP     = "Código do Arranjo Populacional",
      NM_ARR_POP     = "Nome do Arranjo Populacional",
      CO_REG_RURAL   = "Código da Região Rural",
      NM_REG_RURAL   = "Nome da Região Rural",
      CDN_6REG_RURAL = "Classificação do Núcleo Rural (6 categorias)",
      MUN_AMZ_LEG    = "Pertence à Amazônia Legal (0=Não, 1=Sim)",
      MUN_SEMIARIDO  = "Pertence ao Semiárido (0=Não, 1=Sim)",
      MUN_CIDADE_SP  = "Pertence à Cidade-Região de São Paulo (0=Não, 1=Sim)"
    ))
  
  # -------------------------------------------------------------------------
  # Criar CO_MUN6 (código municipal de 6 dígitos para join com RAIS)
  # -------------------------------------------------------------------------
  df_codes_ibge <- df_codes_ibge %>%
    mutate(CO_MUN6 = as.numeric(substr(CO_MUN, 1, 6))) %>%
    relocate(CO_MUN6, .after = CO_MUN)
  
  # -------------------------------------------------------------------------
  # Salvar localmente e upload condicional ao S3
  # -------------------------------------------------------------------------
  dir.create(dirname(LOCAL_PROCESSADO), recursive = TRUE, showWarnings = FALSE)
  save(df_codes_ibge, file = LOCAL_PROCESSADO)
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
message(">> df_codes_ibge: ", nrow(df_codes_ibge), " linhas, ",
        length(unique(df_codes_ibge$CO_MUN)), " municípios, ",
        length(unique(df_codes_ibge$SG_UF)), " UFs")
message("=== codes_ibge_01a.R concluído ===")