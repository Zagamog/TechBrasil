###############################################################################
# qbq_04a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Extração de CBOs do CNCT e Preparação do CNCT para Matching Semântico
#
# USO NAS ABAS: Insumo para pipeline de matching semântico
#               qbq_cnct3a.py (via df_cnct_cbo.rda)
#               qbq_cnct4b.R → qbq_cnct5a.py (via df_cnct2025b.rda/.pkl)
#               Conexão com E1 (Oferta-Demanda) e E2 (CBO)
#
# OBJETIVO:
#   Duas tarefas complementares a partir do catálogo CNCT (df_cnct2025a):
#
#   FASE 1 — Extração de códigos CBO do CNCT (→ df_cnct_cbo)
#     O CNCT lista as "Ocupações CBO Associadas" como texto livre.
#     Extraímos e padronizamos os códigos CBO de 6 dígitos embutidos
#     nesse texto, expandindo códigos de 4 dígitos (famílias CBO) para
#     os subcódigos completos de 6 dígitos.
#
#     Desafios:
#       - Códigos aparecem em formatos: 317105, 3171-05, ou 3171
#       - Alguns cursos têm "Ocupação ainda não classificada"
#       - Códigos de 4 dígitos (famílias) precisam expansão manual
#
#     A tabela resultante (IDX_EIXARECUR × CNCT_CBO) é usada pelo
#     pipeline de matching semântico para validar correspondências
#     entre cursos técnicos e ocupações CBO.
#
#   FASE 2 — Preparação do CNCT para matching semântico (→ df_cnct2025b)
#     Renomeia e normaliza variáveis-chave do CNCT (eixo, área, curso,
#     perfil profissional, campo de atuação) para uso nos scripts Python
#     de matching semântico (TF-IDF e embeddings via qbq_cnct4b.R e
#     qbq_cnct5a.py).
#
#     A normalização remove acentos, converte para minúsculas e elimina
#     espaços extras, permitindo matching textual robusto.
#
#     Exporta em dois formatos:
#       - .rda para uso em R (qbq_cnct4b.R)
#       - .pkl (pickle) para uso em Python (qbq_cnct5a.py)
#
# INSUMOS (S3):
#   working/mec_outros/df_cnct2025a.rda  (de cnct_01a.R via qbq_02a.R)
#     Catálogo CNCT 2025 com 13 eixos, 36 áreas, ~230 cursos técnicos.
#     Variáveis: IDX_EIXARECUR, Eixo Tecnológico, Área Tecnológica,
#     Denominação do Curso, Perfil Profissional de Conclusão,
#     Campo de Atuação, Ocupações CBO Associadas, etc.
#
# DADOS PROCESSADOS (S3 e local):
#   working/qbq/df_cnct_cbo.rda              (Fase 1: IDX_EIXARECUR × CBO)
#   working/mec_outros/df_cnct2025b.rda      (Fase 2: CNCT normalizado)
#   working/mec_outros/df_cnct2025b.pkl      (Fase 2: CNCT normalizado, Python)
#
# EXPANSÃO MANUAL DE CBOs DE 4 DÍGITOS:
#   O CNCT às vezes lista apenas o código de família CBO (4 dígitos).
#   Para matching com RAIS (6 dígitos), expandimos manualmente:
#     3171 → 317105, 317110, 317115, 317120  (Técnicos em informática)
#     3172 → 317205, 317210                   (Técnicos em telecomunicações)
#     3513 → 351305, 351310, 351315           (Técnicos em administração)
#     3515 → 351505, 351510, 351515           (Técnicos em contabilidade)
#     3742 → 374205, 374210, 374215           (Técnicos em design)
#   Fonte: Classificação Brasileira de Ocupações (CBO 2002)
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
# DEPENDÊNCIAS: dplyr, tidyr, stringr, stringi, purrr, aws.s3, dotenv, reticulate
# SAÍDA: 3 arquivos (ver acima)
#   Consumidos pelo pipeline de matching semântico (qbq_cnct3a.py,
#   qbq_cnct4b.R, qbq_cnct5a.py) para conectar cursos CNCT com
#   ocupações CBO via similaridade textual e embeddings.
###############################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(purrr)
library(aws.s3)
library(dotenv)
library(reticulate)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

# Carregar credenciais AWS do arquivo .env na raiz do projeto
dotenv::load_dot_env()

# Bucket S3 do projeto
S3_BUCKET <- "techbrazildata"

# Insumos
INSUMOS <- list(
  df_cnct2025a = list(local = "working/mec_outros/df_cnct2025a.rda",
                      s3    = "working/mec_outros/df_cnct2025a.rda")
)

# Saídas
SAIDAS <- list(
  df_cnct_cbo   = list(local = "working/qbq/df_cnct_cbo.rda",
                       s3    = "working/qbq/df_cnct_cbo.rda"),
  df_cnct2025b  = list(local = "working/mec_outros/df_cnct2025b.rda",
                       s3    = "working/mec_outros/df_cnct2025b.rda"),
  df_cnct2025b_pkl = list(local = "working/mec_outros/df_cnct2025b.pkl",
                          s3    = "working/mec_outros/df_cnct2025b.pkl")
)

# Expansão manual de códigos CBO de 4 dígitos (famílias) para 6 dígitos
# Fonte: Classificação Brasileira de Ocupações (CBO 2002)
EXPAND_MANUAL <- list(
  "3171" = c("317105", "317110", "317115", "317120"),  # Técnicos em informática
  "3172" = c("317205", "317210"),                       # Técnicos em telecomunicações
  "3513" = c("351305", "351310", "351315"),             # Técnicos em administração
  "3515" = c("351505", "351510", "351515"),             # Técnicos em contabilidade
  "3742" = c("374205", "374210", "374215")              # Técnicos em design
)

###############################################################################
# FUNÇÕES AUXILIARES
###############################################################################

# Normalização de texto para matching semântico
# Remove acentos, converte para minúsculas, elimina espaços extras
normalize_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_squish() %>%
    stri_trans_general("Latin-ASCII")
}

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
# VERIFICAR SE TODOS OS ARQUIVOS PROCESSADOS JÁ EXISTEM
###############################################################################

# Nota: df_cnct2025b.pkl não é .rda, verificamos apenas existência
todos_locais <- rda_valido(SAIDAS$df_cnct_cbo$local) &&
  rda_valido(SAIDAS$df_cnct2025b$local) &&
  file.exists(SAIDAS$df_cnct2025b_pkl$local)

if (todos_locais) {
  message("=== Todos os arquivos qbq_04a processados já existem localmente ===")
  
  # Verificar se S3 tem versão mais recente dos .rda
  for (nome in c("df_cnct_cbo", "df_cnct2025b")) {
    local_path <- SAIDAS[[nome]]$local
    s3_key     <- SAIDAS[[nome]]$s3
    s3_existe <- tryCatch(
      suppressMessages(object_exists(object = s3_key, bucket = S3_BUCKET)),
      error = function(e) FALSE
    )
    if (isTRUE(s3_existe)) {
      data_s3    <- s3_ultima_modificacao(s3_key, S3_BUCKET)
      data_local <- file.mtime(local_path)
      if (!is.null(data_s3) && !is.na(data_local) && data_s3 > data_local) {
        message(">> S3 mais recente: ", nome, " — baixando")
        sincronizar_s3(local_path, s3_key, S3_BUCKET)
      }
    } else {
      message(">> Não encontrado no S3 — fazendo upload: ", s3_key)
      tryCatch({
        put_object(file = local_path, object = s3_key, bucket = S3_BUCKET)
        message("   Upload concluído.")
      }, error = function(e) message("   Erro no upload: ", e$message))
    }
  }
  
  # Carregar os .rda
  load(SAIDAS$df_cnct_cbo$local)
  load(SAIDAS$df_cnct2025b$local)
  
  message("=== Arquivos qbq_04a carregados ===")
  
} else {
  
  #############################################################################
  # REPROCESSAMENTO
  #############################################################################
  
  message("=== Processamento qbq_04a necessário ===")
  
  # -------------------------------------------------------------------------
  # PASSO 0: SINCRONIZAR INSUMOS
  # -------------------------------------------------------------------------
  
  message("=== PASSO 0: Sincronizar insumos ===")
  sincronizar_s3(INSUMOS$df_cnct2025a$local, INSUMOS$df_cnct2025a$s3, S3_BUCKET)
  load(INSUMOS$df_cnct2025a$local)  # → df_cnct2025a
  message(">> df_cnct2025a carregado: ", nrow(df_cnct2025a), " cursos")
  
  #############################################################################
  # FASE 1: EXTRAÇÃO DE CÓDIGOS CBO DO CNCT
  #############################################################################
  #
  # O CNCT lista as "Ocupações CBO Associadas" como texto livre com códigos
  # CBO embutidos. Extraímos esses códigos para criar a tabela de lookup
  # IDX_EIXARECUR × CNCT_CBO, usada pelo pipeline de matching semântico
  # (qbq_cnct3a.py) e indiretamente para matching com RAIS.
  #
  # A tabela resultante tem uma linha por par (curso, CBO), permitindo
  # join direto com dados de emprego RAIS via código CBO de 6 dígitos.
  #############################################################################
  
  message("=== FASE 1: Extração de códigos CBO do CNCT ===")
  
  df_cnct_cbo <- df_cnct2025a %>%
    select(IDX_EIXARECUR, `Ocupações CBO Associadas`) %>%
    mutate(
      # Tratar ocupações não classificadas
      Ocupacoes_CNCT = str_trim(`Ocupações CBO Associadas`),
      Ocupacoes_CNCT = if_else(
        Ocupacoes_CNCT %in% c("Ocupação ainda não classificada",
                              "Ocupação ainda não classificada."),
        "Sem CBO colocado no CNCT",
        Ocupacoes_CNCT
      ),
      # Extrair todos os padrões numéricos que parecem códigos CBO
      # (6 dígitos, 4-2 com hífen, ou 4 dígitos)
      cbo_list = str_extract_all(Ocupacoes_CNCT, "\\d{6}|\\d{4}-\\d{2}|\\d{4}"),
      # Limpar hífens e expandir códigos de 4 dígitos
      cbo_list = map(cbo_list, function(x) {
        x_clean <- str_replace_all(x, "-", "")
        expanded <- unlist(map(x_clean, function(code) {
          if (code %in% names(EXPAND_MANUAL)) EXPAND_MANUAL[[code]] else code
        }))
        unique(expanded)
      })
    ) %>%
    select(IDX_EIXARECUR, cbo_list) %>%
    unnest_longer(cbo_list, values_to = "CNCT_CBO") %>%
    filter(!is.na(CNCT_CBO))
  
  # Verificar se restam códigos de 4 dígitos não expandidos
  leftover_4digit <- df_cnct_cbo %>%
    filter(str_length(CNCT_CBO) == 4)
  
  if (nrow(leftover_4digit) > 0) {
    warning(">> ATENÇÃO: ", nrow(leftover_4digit),
            " linhas com códigos CBO de 4 dígitos não expandidos!")
  } else {
    message(">> Todos os códigos CBO expandidos para 6 dígitos")
  }
  
  # Salvar
  dir.create(dirname(SAIDAS$df_cnct_cbo$local), recursive = TRUE, showWarnings = FALSE)
  save(df_cnct_cbo, file = SAIDAS$df_cnct_cbo$local)
  message(">> Salvo: ", SAIDAS$df_cnct_cbo$local, " (",
          nrow(df_cnct_cbo), " pares curso-CBO, ",
          length(unique(df_cnct_cbo$CNCT_CBO)), " CBOs únicos)")
  
  #############################################################################
  # FASE 2: PREPARAÇÃO DO CNCT PARA MATCHING SEMÂNTICO
  #############################################################################
  #
  # Renomeia variáveis do CNCT para nomes padronizados sem acentos/espaços
  # e cria versões normalizadas (cleaned) dos eixos e áreas para matching
  # textual robusto. O resultado é exportado em .rda (para R) e .pkl
  # (para Python), alimentando o pipeline de matching semântico.
  #
  # Variáveis selecionadas:
  #   IDX_EIXARECUR                  — Chave do curso (Eixo.Área.Curso)
  #   Eixo_Tecnologico_CNCT_cleaned  — Eixo normalizado (sem acentos, minúsc.)
  #   Area_Tecnologica_CNCT_cleaned  — Área normalizada (sem acentos, minúsc.)
  #   Denominacao_Curso_CNCT         — Nome do curso
  #   Perfil_Profissional_CNCT       — Perfil profissional de conclusão
  #   Campo_de_Atuacao_CNCT          — Campo de atuação
  #############################################################################
  
  message("=== FASE 2: Preparação do CNCT para matching semântico ===")
  
  df_cnct2025b <- df_cnct2025a %>%
    rename(
      Eixo_Tecnologico_CNCT      = `Eixo Tecnológico`,
      Area_Tecnologica_CNCT      = `Área Tecnológica`,
      Denominacao_Curso_CNCT     = `Denominação do Curso`,
      Perfil_Profissional_CNCT   = `Perfil Profissional de Conclusão`,
      Campo_de_Atuacao_CNCT      = `Campo de Atuação`
    ) %>%
    mutate(
      Eixo_Tecnologico_CNCT_cleaned = normalize_text(Eixo_Tecnologico_CNCT),
      Area_Tecnologica_CNCT_cleaned = normalize_text(Area_Tecnologica_CNCT)
    ) %>%
    select(IDX_EIXARECUR,
           Eixo_Tecnologico_CNCT_cleaned,
           Area_Tecnologica_CNCT_cleaned,
           Denominacao_Curso_CNCT,
           Perfil_Profissional_CNCT,
           Campo_de_Atuacao_CNCT)
  
  message(">> df_cnct2025b: ", nrow(df_cnct2025b), " cursos × ",
          ncol(df_cnct2025b), " variáveis")
  message(">> Eixos únicos: ",
          length(unique(df_cnct2025b$Eixo_Tecnologico_CNCT_cleaned)))
  message(">> Áreas únicas: ",
          length(unique(df_cnct2025b$Area_Tecnologica_CNCT_cleaned)))
  
  # Salvar .rda
  dir.create(dirname(SAIDAS$df_cnct2025b$local), recursive = TRUE, showWarnings = FALSE)
  save(df_cnct2025b, file = SAIDAS$df_cnct2025b$local)
  message(">> Salvo: ", SAIDAS$df_cnct2025b$local)
  
  # -------------------------------------------------------------------------
  # Exportar para Python (.pkl) via reticulate
  # Necessário para qbq_cnct5a.py (matching semântico com embeddings)
  # -------------------------------------------------------------------------
  tryCatch({
    pd <- reticulate::import("pandas")
    reticulate::py_save_object(df_cnct2025b, SAIDAS$df_cnct2025b_pkl$local)
    message(">> Salvo: ", SAIDAS$df_cnct2025b_pkl$local)
  }, error = function(e) {
    message(">> AVISO: Não foi possível exportar .pkl (Python/reticulate): ", e$message)
    message("   Rodar manualmente se necessário para pipeline Python")
  })
  
  #############################################################################
  # UPLOAD CONDICIONAL DE TODOS OS ARQUIVOS
  #############################################################################
  
  message("=== Fazendo upload dos arquivos processados ===")
  for (nome in names(SAIDAS)) {
    local_path <- SAIDAS[[nome]]$local
    s3_key     <- SAIDAS[[nome]]$s3
    if (file.exists(local_path)) {
      upload_s3_se_diferente(local_path, s3_key, S3_BUCKET)
    }
  }
  
} # Fim do bloco else (reprocessamento)

###############################################################################
# VALIDAÇÃO
###############################################################################

message(">> df_cnct_cbo: ", nrow(df_cnct_cbo), " pares curso-CBO")
message(">> CBOs únicos (6 dígitos): ", length(unique(df_cnct_cbo$CNCT_CBO)))
message(">> Cursos com CBO (IDX_EIXARECUR): ",
        length(unique(df_cnct_cbo$IDX_EIXARECUR)))
message(">> df_cnct2025b: ", nrow(df_cnct2025b), " cursos")
message(">> Eixos: ",
        length(unique(df_cnct2025b$Eixo_Tecnologico_CNCT_cleaned)),
        " | Áreas: ",
        length(unique(df_cnct2025b$Area_Tecnologica_CNCT_cleaned)))
message("=== qbq_04a.R concluído ===")