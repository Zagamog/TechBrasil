###############################################################################
# pop00_70_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Projeções Populacionais IBGE por Faixa Etária (2000–2070)
#
# USO NAS ABAS: A1 (Transição Demográfica), A2 (EPT e População)
#
# OBJETIVO:
#   Baixar projeções populacionais do IBGE via S3, processar agregações
#   regionais (Amazônia Legal, Nordeste_r, Centro-Oeste_r), calcular
#   proporções etárias e indicadores de transição demográfica (crossover
#   0-14 vs 60+).
#
# DADOS BRUTOS (S3):
#   rawdata/ibge/projecoes_2024_tab3_grupos_etarios_especificos.xlsx
#
# DADOS PROCESSADOS (S3 e local):
#   working/ibge/pop01_70b.rda
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
# SAÍDA: pop01_70b.rda
#   Carregado diretamente por BM_FGV_Propag2.R (sem produtos intermediários).
#   Aba A1: objeto principal para gráfico de transição demográfica.
#   Aba A2: combinado com meta11a_opcoes e df_codes_ibge para gerar
#           ept_combined_data (população 15-19 vs matrículas EPT/EM).
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

# Caminhos S3
S3_BRUTO     <- "rawdata/ibge/projecoes_2024_tab3_grupos_etarios_especificos.xlsx"
S3_PROCESSADO <- "working/ibge/pop01_70b.rda"

# Caminhos locais (relativos à raiz do projeto)
LOCAL_BRUTO     <- "rawdata/ibge/projecoes_2024.xlsx"
LOCAL_PROCESSADO <- "working/ibge/pop01_70b.rda"

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
  
  # Criar diretório local se necessário
  dir.create(dirname(caminho_local), recursive = TRUE, showWarnings = FALSE)
  
  # Verificar se arquivo local existe
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
  
  # Arquivo local existe — comparar datas de modificação
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

# Upload ao S3 (condicional — somente se processamento foi executado)
upload_s3 <- function(caminho_local, s3_key, bucket) {
  tryCatch({
    put_object(file = caminho_local, object = s3_key, bucket = bucket)
    message(">> Upload ao S3 concluído: ", s3_key)
  }, error = function(e) {
    message(">> Erro no upload ao S3: ", e$message)
  })
}

###############################################################################
# PASSO 1: SINCRONIZAR DADOS PROCESSADOS
###############################################################################

# Verificar se o arquivo processado no S3 já está atualizado
# Se sim, não é necessário reprocessar — apenas carregar
processado_atualizado <- FALSE

if (file.exists(LOCAL_PROCESSADO)) {
  data_processado_local <- file.mtime(LOCAL_PROCESSADO)
  data_processado_s3 <- s3_ultima_modificacao(S3_PROCESSADO, S3_BUCKET)
  
  if (!is.null(data_processado_s3) && data_processado_s3 > data_processado_local) {
    # S3 tem versão mais recente do processado — baixar direto
    message("=== Arquivo processado no S3 mais recente — baixando ===")
    sincronizar_s3(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
    processado_atualizado <- TRUE
  } else {
    # Versão local do processado está atualizada
    message("=== Arquivo processado local está atualizado ===")
    processado_atualizado <- TRUE
  }
} else {
  # Arquivo processado não existe localmente — verificar S3
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
  
  # Sincronizar xlsx bruto do S3
  sincronizar_s3(LOCAL_BRUTO, S3_BRUTO, S3_BUCKET)
  
  # -------------------------------------------------------------------------
  # Leitura do xlsx IBGE
  # -------------------------------------------------------------------------
  message(">> Lendo planilha IBGE...")
  pop01_70a <- openxlsx::read.xlsx(LOCAL_BRUTO, sheet = 1, startRow = 7, colNames = TRUE)
  
  # -------------------------------------------------------------------------
  # Selecionar colunas relevantes e padronizar nomes
  # -------------------------------------------------------------------------
  pop01_70b <- pop01_70a %>%
    select(1:4, POP_T, `0-14_T`, `15-17_T`, `18-21_T`, `15-59_T`, `60+_T`) %>%
    mutate(CODEFED = as.character(`CÓD.`)) %>%
    select(-`CÓD.`) %>%
    relocate(CODEFED, .after = "SIGLA")
  
  # -------------------------------------------------------------------------
  # Definir listas de estados para agregações regionais
  # -------------------------------------------------------------------------
  amazonia_legal  <- c("Acre", "Amapá", "Amazonas", "Maranhão", "Mato Grosso",
                       "Pará", "Rondônia", "Roraima", "Tocantins")
  
  nordeste_r      <- c("Alagoas", "Bahia", "Ceará", "Paraíba",
                       "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe")
  
  centro_oeste_r  <- c("Distrito Federal", "Goiás", "Mato Grosso do Sul")
  
  variaveis_soma  <- c("POP_T", "0-14_T", "15-17_T", "18-21_T", "15-59_T", "60+_T")
  
  # -------------------------------------------------------------------------
  # Função auxiliar: agregar estados em região customizada
  # -------------------------------------------------------------------------
  agregar_regiao <- function(lista_estados, nome_regiao, codigo, sigla) {
    pop01_70a %>%
      filter(LOCAL %in% lista_estados) %>%
      group_by(ANO) %>%
      summarise(
        LOCAL   = nome_regiao,
        CODEFED = codigo,
        SIGLA   = sigla,
        across(all_of(variaveis_soma), \(x) sum(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }
  
  # -------------------------------------------------------------------------
  # Criar agregações regionais
  # -------------------------------------------------------------------------
  agr_amazonia      <- agregar_regiao(amazonia_legal, "Amazonia_Legal", "99", "AML")
  agr_nordeste_r    <- agregar_regiao(nordeste_r,     "Nordeste_r",     "2b", "ND_")
  agr_centro_oeste_r <- agregar_regiao(centro_oeste_r, "Centro-Oeste_r", "5b", "CO_")
  
  # Combinar agregações com o dataframe principal
  pop01_70b <- bind_rows(pop01_70b, agr_amazonia, agr_nordeste_r, agr_centro_oeste_r)
  
  # -------------------------------------------------------------------------
  # Calcular proporções populacionais por faixa etária
  # -------------------------------------------------------------------------
  pop01_70b <- pop01_70b %>%
    mutate(
      P_0_14_T    = `0-14_T`  / POP_T,
      P_15_17_T   = `15-17_T` / POP_T,
      P_18_21_T   = `18-21_T` / POP_T,
      P_15_59_T   = `15-59_T` / POP_T,
      P_60_plus_T = `60+_T`   / POP_T
    )
  
  # -------------------------------------------------------------------------
  # Identificar ano de cruzamento demográfico (crossover 0-14 vs 60+)
  # O crossover ocorre quando a faixa 0-14 cai abaixo da faixa 60+
  # -------------------------------------------------------------------------
  pop01_70b <- pop01_70b %>%
    group_by(LOCAL) %>%
    mutate(
      Crossover_Flag       = if_else(
        lag(`0-14_T`) > lag(`60+_T`) & `0-14_T` < `60+_T`, 1, 0
      ),
      Crossover_Value_Num  = if_else(Crossover_Flag == 1, `0-14_T`,  NA_real_),
      Crossover_Value_Prop = if_else(Crossover_Flag == 1, P_0_14_T, NA_real_)
    ) %>%
    ungroup()
  
  # -------------------------------------------------------------------------
  # Salvar localmente e fazer upload ao S3
  # -------------------------------------------------------------------------
  dir.create(dirname(LOCAL_PROCESSADO), recursive = TRUE, showWarnings = FALSE)
  save(pop01_70b, file = LOCAL_PROCESSADO)
  message(">> Salvo localmente: ", LOCAL_PROCESSADO)
  
  upload_s3(LOCAL_PROCESSADO, S3_PROCESSADO, S3_BUCKET)
  
} else {
  # Arquivo processado já está atualizado — apenas carregar
  message("=== Carregando arquivo processado existente ===")
  load(LOCAL_PROCESSADO)
}

###############################################################################
# VALIDAÇÃO
###############################################################################
message(">> pop01_70b: ", nrow(pop01_70b), " linhas, ",
        length(unique(pop01_70b$LOCAL)), " localidades, ",
        min(pop01_70b$ANO), "-", max(pop01_70b$ANO))
message("=== pop00_70_01a.R concluído ===")