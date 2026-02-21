###############################################################################
# qbq_03a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Enriquecimento de Ocupações QBQ com Hierarquia CBO
#
# USO NAS ABAS: D2 (APLs), D3 (Informalidade), E1 (Oferta-Demanda)
#               Via rais_02a.R, rais_apo_*.R e diretamente pelo Shiny
#
# OBJETIVO:
#   Combinar as ocupações do QBQ (1.899 CBOs com metadados de perfil
#   ocupacional e nível) com a hierarquia oficial da CBO 2002,
#   produzindo uma tabela de lookup completa usada por todo o pipeline
#   RAIS e pelo Shiny.
#
#   A hierarquia CBO 2002 tem 5 níveis de agregação:
#     - Grande Grupo (1 dígito): 10 grupos   → cbo_1dig, cbo_gragru
#     - Subgrupo Principal (2 dígitos): ~48   → cbo_2dig, cbo_prigru
#     - Subgrupo (3 dígitos): ~192            → cbo_3dig, cbo_subgru
#     - Família Ocupacional (4 dígitos): ~627 → cbo_4dig, cbo_familia
#     - Ocupação (6 dígitos): ~2.719          → CodCBO, cbo_nome
#
# CORREÇÕES MANUAIS (conhecimento de domínio — não alterar):
#
#   1. Grande Grupo 8: O CSV oficial do MTE não traz o título do
#      Grande Grupo 8 separado. Na CBO 6.0.6 (Informações Gerais, p.15,
#      disponível em mtecbo.gov.br), os GGs 7 e 8 são descritos
#      conjuntamente. O título "TRABALHADORES DA PRODUÇÃO DE BENS E
#      SERVIÇOS INDUSTRIAIS CONTÍNUOS" foi atribuído manualmente ao GG 8
#      para distingui-lo do GG 7 ("TRABALHADORES DA PRODUÇÃO DE BENS E
#      SERVIÇOS INDUSTRIAIS" — processos discretos).
#
#   2. Ocupação duplicada: Existem duas ocupações "Operador de pá
#      carregadeira" em subgrupos diferentes (CBO 715xxx e outro).
#      A do subgrupo 715 recebe sufixo "715" no nome para desambiguação.
#
# INSUMOS (S3):
#   working/qbq/qbq_ocup.rda              (de qbq_01a.R)
#   working/qbq/qbq_conhecimento1.rda     (de qbq_01a.R)
#   rawdata/cbo/CBO2002 - Grande Grupo.csv
#   rawdata/cbo/CBO2002 - SubGrupo Principal.csv
#   rawdata/cbo/CBO2002 - SubGrupo.csv
#   rawdata/cbo/CBO2002 - Familia.csv
#   rawdata/cbo/CBO2002 - Ocupacao.csv
#
# SAÍDA (S3):
#   working/qbq/qbq_ocup_cmento1.rda
#
# VARIÁVEIS NA SAÍDA (14 colunas, 1.899 ocupações):
#   CodCBO              — Código CBO 6 dígitos (character)
#   Ocupação            — Nome da ocupação (QBQ)
#   Síntese             — Descrição sintética da ocupação (QBQ)
#   PerfilOcupacional   — Perfil ocupacional detalhado (QBQ)
#   NivelOcupacao       — Nível de qualificação (QBQ)
#   cbo_nome            — Nome da ocupação (CBO oficial)
#   cbo_4dig            — Código família ocupacional (4 dígitos)
#   cbo_familia         — Nome da família ocupacional
#   cbo_3dig            — Código subgrupo (3 dígitos)
#   cbo_subgru          — Nome do subgrupo
#   cbo_2dig            — Código subgrupo principal (2 dígitos)
#   cbo_prigru          — Nome do subgrupo principal
#   cbo_1dig            — Código grande grupo (1 dígito)
#   cbo_gragru          — Nome do grande grupo
#
# DEPENDÊNCIAS: dplyr, readr, janitor, stringr, aws.s3, dotenv
###############################################################################

library(dplyr)
library(readr)
library(janitor)
library(stringr)
library(aws.s3)
library(dotenv)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

dotenv::load_dot_env()
S3_BUCKET <- "techbrazildata"

# Insumos QBQ (de qbq_01a.R)
S3_QBQ_OCUP  <- "working/qbq/qbq_ocup.rda"
S3_QBQ_CONH  <- "working/qbq/qbq_conhecimento1.rda"

# Insumos CBO (arquivos CSV estáticos)
S3_CBO_BASE  <- "rawdata/cbo/"
CBO_CSVS <- c(
  "CBO2002 - Grande Grupo.csv",
  "CBO2002 - SubGrupo Principal.csv",
  "CBO2002 - SubGrupo.csv",
  "CBO2002 - Familia.csv",
  "CBO2002 - Ocupacao.csv"
)

# Saída
S3_SAIDA    <- "working/qbq/qbq_ocup_cmento1.rda"
LOCAL_SAIDA <- "working/qbq/qbq_ocup_cmento1.rda"

###############################################################################
# FUNÇÕES DE SINCRONIZAÇÃO S3
###############################################################################

s3_ultima_modificacao <- function(s3_key, bucket) {
  tryCatch({
    info <- suppressMessages(head_object(object = s3_key, bucket = bucket))
    as.POSIXct(attr(info, "last-modified"),
               format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
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
  message("=== qbq_ocup_cmento1 local encontrado e válido ===")
  
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
  message(">> qbq_ocup_cmento1 carregado: ", nrow(qbq_ocup_cmento1), " ocupações")
  
} else {
  
  ###############################################################################
  # PASSO 0: SINCRONIZAR INSUMOS
  ###############################################################################
  
  message("=== Processamento necessário — sincronizando insumos ===")
  
  # QBQ
  sincronizar_s3("working/qbq/qbq_ocup.rda", S3_QBQ_OCUP, S3_BUCKET)
  sincronizar_s3("working/qbq/qbq_conhecimento1.rda", S3_QBQ_CONH, S3_BUCKET)
  load("working/qbq/qbq_ocup.rda")            # → qbq_ocup
  load("working/qbq/qbq_conhecimento1.rda")   # → qbq_conhecimento1
  
  # CBO CSVs
  dir.create("rawdata/cbo", recursive = TRUE, showWarnings = FALSE)
  for (csv_nome in CBO_CSVS) {
    local_csv <- file.path("rawdata/cbo", csv_nome)
    s3_csv <- paste0(S3_CBO_BASE, csv_nome)
    sincronizar_s3(local_csv, s3_csv, S3_BUCKET)
  }
  
  ###############################################################################
  # PASSO 1: COMBINAR QBQ OCUPAÇÕES COM CONHECIMENTOS
  ###############################################################################
  #
  # qbq_ocup tem 1.899 ocupações com metadados do QBQ.
  # qbq_conhecimento1 tem as áreas/campos/conhecimentos por CBO (muitos-para-um).
  # Juntamos e colapsamos os conhecimentos em listas por ocupação,
  # filtrando apenas ocupações com PerfilOcupacional e desArea válidos.
  # As colunas de conhecimento são usadas apenas como filtro intermediário
  # (mantemos apenas CBOs que aparecem em ambas as fontes) e são descartadas
  # no passo final.
  ###############################################################################
  
  message("=== PASSO 1: Combinar QBQ com conhecimentos ===")
  
  qbq_ocup_cmento1 <- qbq_ocup %>%
    select(CodCBO, Ocupação, Síntese, PerfilOcupacional, NivelOcupacao) %>%
    filter(!is.na(PerfilOcupacional) &
             PerfilOcupacional != "NULL" &
             PerfilOcupacional != "") %>%
    left_join(qbq_conhecimento1, by = "CodCBO") %>%
    filter(!is.na(desArea) & desArea != "NULL" & desArea != "") %>%
    distinct() %>%
    group_by(CodCBO, Ocupação, Síntese, PerfilOcupacional, NivelOcupacao) %>%
    mutate(CodCBO = as.character(CodCBO)) %>%
    summarise(.groups = "drop") %>%
    arrange(CodCBO)
  
  message(">> Ocupações QBQ com perfil e conhecimento: ", nrow(qbq_ocup_cmento1))
  
  ###############################################################################
  # PASSO 2: CONSTRUIR HIERARQUIA CBO 2002
  ###############################################################################
  #
  # A CBO 2002 é organizada em 5 níveis hierárquicos, cada um em um CSV
  # separado do MTE. Construímos a hierarquia completa juntando por
  # prefixos do código CBO de 6 dígitos.
  #
  # Fonte: Ministério do Trabalho e Emprego — CBO 2002
  # https://www.gov.br/trabalho-e-emprego/pt-br/assuntos/
  #        classificacao-brasileira-de-ocupacoes-cbo
  ###############################################################################
  
  message("=== PASSO 2: Hierarquia CBO 2002 ===")
  
  cbo_base <- "rawdata/cbo/"
  
  # Grande Grupo (1 dígito) — 10 grupos
  df_gg <- read_delim(paste0(cbo_base, "CBO2002 - Grande Grupo.csv"),
                      delim = ";", locale = locale(encoding = "LATIN1"),
                      show_col_types = FALSE) %>%
    clean_names() %>%
    rename(cbo_1dig = codigo, cbo_gragru = titulo) %>%
    mutate(cbo_1dig = str_pad(as.character(cbo_1dig), 1, pad = "0"))
  
  # CORREÇÃO MANUAL 1: Grande Grupo 8
  # O CSV oficial agrupa GGs 7 e 8 sob o mesmo título. Na CBO 6.0.6
  # (Informações Gerais, p.15, mtecbo.gov.br), o GG 8 refere-se a
  # processos contínuos, distinto do GG 7 (processos discretos).
  df_gg$cbo_gragru[df_gg$cbo_1dig == "8"] <-
    "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS CONTINUOS"
  
  message(">> Grande Grupo: ", nrow(df_gg), " grupos")
  
  # Subgrupo Principal (2 dígitos)
  df_sgp <- read_delim(paste0(cbo_base, "CBO2002 - SubGrupo Principal.csv"),
                       delim = ";", locale = locale(encoding = "LATIN1"),
                       show_col_types = FALSE) %>%
    clean_names() %>%
    rename(cbo_2dig = codigo, cbo_prigru = titulo) %>%
    mutate(cbo_2dig = str_pad(as.character(cbo_2dig), 2, pad = "0"))
  
  message(">> Subgrupo Principal: ", nrow(df_sgp), " subgrupos")
  
  # Subgrupo (3 dígitos)
  df_sg <- read_delim(paste0(cbo_base, "CBO2002 - SubGrupo.csv"),
                      delim = ";", locale = locale(encoding = "LATIN1"),
                      show_col_types = FALSE) %>%
    clean_names() %>%
    rename(cbo_3dig = codigo, cbo_subgru = titulo) %>%
    mutate(cbo_3dig = str_pad(as.character(cbo_3dig), 3, pad = "0"))
  
  message(">> Subgrupo: ", nrow(df_sg), " subgrupos")
  
  # Família Ocupacional (4 dígitos)
  df_fam <- read_delim(paste0(cbo_base, "CBO2002 - Familia.csv"),
                       delim = ";", locale = locale(encoding = "LATIN1"),
                       show_col_types = FALSE) %>%
    clean_names() %>%
    rename(cbo_4dig = codigo, cbo_familia = titulo) %>%
    mutate(cbo_4dig = str_pad(as.character(cbo_4dig), 4, pad = "0"))
  
  message(">> Família Ocupacional: ", nrow(df_fam), " famílias")
  
  # Ocupação (6 dígitos)
  df_ocup <- read_delim(paste0(cbo_base, "CBO2002 - Ocupacao.csv"),
                        delim = ";", locale = locale(encoding = "LATIN1"),
                        show_col_types = FALSE) %>%
    clean_names() %>%
    rename(cbo_6dig = codigo, cbo_nome = titulo) %>%
    mutate(
      cbo_1dig = str_sub(cbo_6dig, 1, 1),
      cbo_2dig = str_sub(cbo_6dig, 1, 2),
      cbo_3dig = str_sub(cbo_6dig, 1, 3),
      cbo_4dig = str_sub(cbo_6dig, 1, 4)
    )
  
  # CORREÇÃO MANUAL 2: Ocupação duplicada
  # Duas ocupações "Operador de pá carregadeira" em subgrupos diferentes.
  # Desambiguação adicionando sufixo "715" à do subgrupo 715.
  df_ocup$cbo_nome[df_ocup$cbo_nome == "Operador de pá carregadeira" &
                     df_ocup$cbo_3dig == "715"] <- "Operador de pá carregadeira715"
  
  message(">> Ocupações: ", nrow(df_ocup), " ocupações (6 dígitos)")
  
  # Unir toda a hierarquia
  df_cbo_hier <- df_ocup %>%
    left_join(df_fam, by = "cbo_4dig") %>%
    left_join(df_sg,  by = "cbo_3dig") %>%
    left_join(df_sgp, by = "cbo_2dig") %>%
    left_join(df_gg,  by = "cbo_1dig") %>%
    relocate(cbo_6dig, cbo_nome, cbo_4dig, cbo_familia, cbo_3dig, cbo_subgru,
             cbo_2dig, cbo_prigru, cbo_1dig, cbo_gragru)
  
  message(">> Hierarquia CBO completa: ", nrow(df_cbo_hier), " ocupações")
  
  ###############################################################################
  # PASSO 3: JUNTAR QBQ COM HIERARQUIA CBO
  ###############################################################################
  
  message("=== PASSO 3: Juntar QBQ + hierarquia CBO ===")
  
  qbq_ocup_cmento1 <- qbq_ocup_cmento1 %>%
    left_join(df_cbo_hier, by = c("CodCBO" = "cbo_6dig"))
  
  message(">> qbq_ocup_cmento1: ", nrow(qbq_ocup_cmento1), " ocupações × ",
          ncol(qbq_ocup_cmento1), " variáveis")
  
  ###############################################################################
  # SALVAR
  ###############################################################################
  
  dir.create(dirname(LOCAL_SAIDA), recursive = TRUE, showWarnings = FALSE)
  save(qbq_ocup_cmento1, file = LOCAL_SAIDA)
  message(">> Salvo: ", LOCAL_SAIDA)
  
  upload_s3_se_diferente(LOCAL_SAIDA, S3_SAIDA, S3_BUCKET)
  
} # Fim do bloco else (reprocessamento)

###############################################################################
# VALIDAÇÃO
###############################################################################

message(">> qbq_ocup_cmento1: ", nrow(qbq_ocup_cmento1), " ocupações")
message(">> Famílias CBO (4 dígitos): ",
        length(unique(qbq_ocup_cmento1$cbo_4dig[!is.na(qbq_ocup_cmento1$cbo_4dig)])))
message(">> Grandes Grupos: ",
        length(unique(qbq_ocup_cmento1$cbo_gragru[!is.na(qbq_ocup_cmento1$cbo_gragru)])))
message(">> NAs em cbo_familia: ",
        sum(is.na(qbq_ocup_cmento1$cbo_familia)))
message("=== qbq_03a.R concluído ===")

###############################################################################
# SEÇÃO D2: EXPORTAR FAMÍLIAS CBO E CONVERTER NOMES CURTOS
###############################################################################
# Passo 1: Exportar lista de famílias CBO para encurtar via Gemini
#   (rodar cbo_short_names_gemini.py após este passo)

cbo_familias <- unique(qbq_ocup_cmento1[, c("cbo_4dig", "cbo_familia")])
cbo_familias <- cbo_familias[!is.na(cbo_familias$cbo_4dig) & !is.na(cbo_familias$cbo_familia), ]
write.csv(cbo_familias, "working/qbq/cbo_familias.csv", row.names = FALSE)
message(">> Exportado working/qbq/cbo_familias.csv: ", nrow(cbo_familias), " famílias")

put_object(
  file = "working/qbq/cbo_familias.csv",
  object = "working/qbq/cbo_familias.csv",
  bucket = S3_BUCKET
)
message(">> Upload S3: working/qbq/cbo_familias.csv")

# Passo 2: Após rodar cbo_short_names_gemini.py, converter CSV → .rda e subir ao S3
cbo_short_csv <- "working/qbq/cbo_short_names.csv"
if (file.exists(cbo_short_csv)) {
  cbo_short_names <- read.csv(cbo_short_csv, colClasses = c(cbo_4dig = "character"))
  cbo_short_names$short_name <- toupper(cbo_short_names$short_name)
  save(cbo_short_names, file = "working/qbq/cbo_short_names.rda")
  message(">> Salvo working/qbq/cbo_short_names.rda: ", nrow(cbo_short_names), " linhas")
  
  put_object(
    file = "working/qbq/cbo_short_names.rda",
    object = "working/qbq/cbo_short_names.rda",
    bucket = S3_BUCKET
  )
  message(">> Upload S3: working/qbq/cbo_short_names.rda")
} else {
  message(">> cbo_short_names.csv não encontrado — rodar cbo_short_names_gemini.py primeiro")
}







