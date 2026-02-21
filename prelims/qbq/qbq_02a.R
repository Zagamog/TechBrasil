###############################################################################
# qbq_02a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Matching Cursos Censo Escolar ↔ CNCT e Extração de CBO
#
# USO NAS ABAS: C3 (Oferta EPT por Eixo e Curso),
#               E1 (Matching Oferta-Demanda), E2 (Conexão CBO)
#
# OBJETIVO:
#   Este é o script mais complexo do pipeline. Conecta os cursos técnicos
#   reportados no Censo Escolar (nomes livres preenchidos pelas escolas)
#   com a classificação oficial do CNCT (Catálogo Nacional de Cursos
#   Técnicos), permitindo enriquecer cada matrícula com:
#     - Eixo Tecnológico e Área Tecnológica (classificação CNCT)
#     - Perfil Profissional de Conclusão
#     - Ocupações CBO Associadas (chave para matching com demanda RAIS)
#     - Infraestrutura Mínima
#
#   O DESAFIO: Os nomes de cursos no Censo Escolar não são padronizados
#   segundo o CNCT. Há variações ortográficas, cursos genéricos ("Outros"),
#   cursos descontinuados, e cursos que existem no Censo mas não no CNCT
#   (e vice-versa). O matching exato por nome normalizado resolve ~80% dos
#   casos. Os ~20% restantes exigem patches manuais, documentados abaixo.
#
#   FASES DO PROCESSAMENTO:
#     FASE 1: Normalização de texto e matching exato
#     FASE 2: Patches manuais para cursos não correspondidos
#     FASE 3: Enriquecimento com metadados CNCT
#     FASE 4: Separação em matched (→ Shiny) e unmatched (→ C3)
#     FASE 5: Agregações por UF, Eixo, Área e Curso (→ E1)
#     FASE 6: Extração de códigos CBO para matching com RAIS (→ E2)
#
# INSUMOS PROCESSADOS (S3):
#   working/mec_outros/df_cnct2025a.rda  (de cnct_01a.R)
#   working/mec_inep/df_censo_supl_tec23.rda  (de prepara_censo_escolar_tecnico_01a.R)
#   working/mec_inep/df_censo_supl_tec24.rda  (de prepara_censo_escolar_tecnico_01a.R)
#   working/qbq/qbq_ocup.rda  (de qbq_01a.R)
#   working/ibge/df_codes_ibge.rda  (de codes_ibge_01a.R)
#
# DADOS PROCESSADOS (S3 e local):
#   working/mec_inep/df_censo_notin_cnct.rda     (C3: cursos sem match CNCT)
#   working/qbq/df_censo_supl_tec_4qbqALL.rda    (todos os cursos matched)
#   working/mec_inep/df_mat_uf.rda                (E1: matrículas por UF)
#   working/mec_inep/df_mat_eixo.rda              (E1: matrículas por Eixo)
#   working/mec_inep/df_mat_area.rda              (E1: matrículas por Área)
#   working/mec_inep/df_mat_curso.rda             (E1: matrículas por Curso)
#   working/mec_inep/df_exarcu.rda                (E1: hierarquia Eixo→Área→Curso)
#   working/qbq/df_censo_supl_tec_4qbq.rda       (E2: cursos com CBO extraídos)
#
# CREDENCIAIS AWS:
#   Manter arquivo .env na raiz do projeto com as variáveis:
#     AWS_ACCESS_KEY_ID=...
#     AWS_SECRET_ACCESS_KEY=...
#     AWS_DEFAULT_REGION=us-east-1
#
# DEPENDÊNCIAS: dplyr, tidyr, stringr, stringi, purrr, openxlsx, aws.s3, dotenv
# SAÍDA: 8 arquivos .rda (ver acima)
###############################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(purrr)
library(openxlsx)
library(aws.s3)
library(dotenv)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

dotenv::load_dot_env()
S3_BUCKET <- "techbrazildata"

# Insumos
INSUMOS <- list(
  df_cnct2025a      = list(local = "working/mec_outros/df_cnct2025a.rda",
                           s3    = "working/mec_outros/df_cnct2025a.rda"),
  df_censo_supl_tec23 = list(local = "working/mec_inep/df_censo_supl_tec23.rda",
                             s3    = "working/mec_inep/df_censo_supl_tec23.rda"),
  df_censo_supl_tec24 = list(local = "working/mec_inep/df_censo_supl_tec24.rda",
                             s3    = "working/mec_inep/df_censo_supl_tec24.rda"),
  qbq_ocup          = list(local = "working/qbq/qbq_ocup.rda",
                           s3    = "working/qbq/qbq_ocup.rda"),
  df_codes_ibge     = list(local = "working/ibge/df_codes_ibge.rda",
                           s3    = "working/ibge/df_codes_ibge.rda")
)

# Saídas
SAIDAS <- list(
  df_censo_notin_cnct       = list(local = "working/mec_inep/df_censo_notin_cnct.rda",
                                   s3    = "working/mec_inep/df_censo_notin_cnct.rda"),
  df_censo_supl_tec_4qbqALL = list(local = "working/qbq/df_censo_supl_tec_4qbqALL.rda",
                                   s3    = "working/qbq/df_censo_supl_tec_4qbqALL.rda"),
  df_mat_uf                 = list(local = "working/mec_inep/df_mat_uf.rda",
                                   s3    = "working/mec_inep/df_mat_uf.rda"),
  df_mat_eixo               = list(local = "working/mec_inep/df_mat_eixo.rda",
                                   s3    = "working/mec_inep/df_mat_eixo.rda"),
  df_mat_area               = list(local = "working/mec_inep/df_mat_area.rda",
                                   s3    = "working/mec_inep/df_mat_area.rda"),
  df_mat_curso              = list(local = "working/mec_inep/df_mat_curso.rda",
                                   s3    = "working/mec_inep/df_mat_curso.rda"),
  df_exarcu                 = list(local = "working/mec_inep/df_exarcu.rda",
                                   s3    = "working/mec_inep/df_exarcu.rda"),
  df_censo_supl_tec_4qbq    = list(local = "working/qbq/df_censo_supl_tec_4qbq.rda",
                                   s3    = "working/qbq/df_censo_supl_tec_4qbq.rda")
)

###############################################################################
# FUNÇÕES DE SINCRONIZAÇÃO S3
###############################################################################

s3_ultima_modificacao <- function(s3_key, bucket) {
  resultado <- tryCatch({
    info <- suppressMessages(head_object(object = s3_key, bucket = bucket))
    as.POSIXct(attr(info, "last-modified"), format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  }, error = function(e) return(NULL))
  return(resultado)
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

###############################################################################
# VERIFICAR SE TODOS OS ARQUIVOS PROCESSADOS JÁ EXISTEM
###############################################################################

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

todos_locais <- all(sapply(SAIDAS, function(s) rda_valido(s$local)))

if (todos_locais) {
  message("=== Todos os arquivos processados já existem localmente ===")
  
  # Verificar S3: baixar se mais recente, upload se ausente (semeadura)
  for (nome in names(SAIDAS)) {
    local_path <- SAIDAS[[nome]]$local
    s3_key     <- SAIDAS[[nome]]$s3
    s3_existe <- tryCatch(
      suppressMessages(object_exists(object = s3_key, bucket = S3_BUCKET)),
      error = function(e) FALSE
    )
    if (isTRUE(s3_existe)) {
      data_s3 <- s3_ultima_modificacao(s3_key, S3_BUCKET)
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
  
  # Carregar todos os arquivos processados
  for (nome in names(SAIDAS)) {
    load(SAIDAS[[nome]]$local, envir = .GlobalEnv)
  }
  
  message("=== Arquivos carregados ===")
  
} else {
  
  ###############################################################################
  # PASSO 0: SINCRONIZAR INSUMOS
  ###############################################################################
  
  message("=== Sincronizando insumos ===")
  for (nome in names(INSUMOS)) {
    sincronizar_s3(INSUMOS[[nome]]$local, INSUMOS[[nome]]$s3, S3_BUCKET)
  }
  
  ###############################################################################
  # PASSO 1: CARREGAR INSUMOS
  ###############################################################################
  
  message("=== Carregando insumos ===")
  
  # CNCT — Catálogo Nacional de Cursos Técnicos (migrado de .rds para .rda)
  load(INSUMOS$df_cnct2025a$local)        # → df_cnct2025a
  
  # Suplemento técnico do Censo Escolar 2023 e 2024
  load(INSUMOS$df_censo_supl_tec23$local) # → df_censo_supl_tec23
  load(INSUMOS$df_censo_supl_tec24$local) # → df_censo_supl_tec24
  
  # QBQ — Ocupações CBO
  load(INSUMOS$qbq_ocup$local)            # → qbq_ocup
  
  # Geocódigos IBGE
  load(INSUMOS$df_codes_ibge$local)       # → df_codes_ibge
  
  ###############################################################################
  # PASSO 2: COMBINAR SUPLEMENTOS E PREPARAR DADOS
  ###############################################################################
  
  message("=== Combinando suplementos técnicos 2023-2024 ===")
  
  # Combinar 2023 e 2024, removendo "Ensino Médio - Curso Normal/Magistério"
  # que aparece nos microdados por erro mas NÃO é EPT
  df_censo_supl_tec <- bind_rows(df_censo_supl_tec23, df_censo_supl_tec24) %>%
    filter(NO_CURSO_EDUC_PROFISSIONAL != "Ensino Médio - Curso Normal/Magistério")
  
  # Selecionar colunas relevantes do CNCT para enriquecimento
  df_cnct_ <- df_cnct2025a %>%
    select(IDX_EIXARECUR, `Eixo Tecnológico`, `Área Tecnológica`,
           `Denominação do Curso`, `Perfil Profissional de Conclusão`,
           `Campo de Atuação`, `Ocupações CBO Associadas`,
           `Infraestrutura Mínima`, eixo_code, area_code, curso_code) %>%
    relocate(eixo_code, area_code, curso_code, .after = IDX_EIXARECUR)
  
  ###############################################################################
  # FASE 1: NORMALIZAÇÃO DE TEXTO E MATCHING EXATO
  ###############################################################################
  #
  # Estratégia: normalizar nomes de cursos em ambas as fontes (Censo e CNCT)
  # para minúsculas, sem acentos, sem espaços extras. Depois, matching exato.
  #
  # O CNCT usa "Técnico em [nome do curso]" como padrão de denominação.
  # O Censo usa o nome completo. Para o matching, removemos o prefixo
  # "Técnico em " do CNCT e comparamos o restante.
  #
  # IDX_EIXCUR: índice criado a partir do eixo+curso normalizados, NÃO é o
  # mesmo que IDX_EIXARECUR do CNCT. O IDX_EIXCUR é um índice de trabalho
  # para o matching; o IDX_EIXARECUR é o índice hierárquico oficial do CNCT.
  ###############################################################################
  
  message("=== FASE 1: Matching exato por nome normalizado ===")
  
  # Função de normalização de texto para matching
  normalize_text <- function(x) {
    x %>%
      str_to_lower() %>%
      str_squish() %>%
      stri_trans_general("Latin-ASCII")
  }
  
  # --- Passo 1.1: Limpar nomes do Censo Escolar ---
  df_censo_cleaned <- df_censo_supl_tec %>%
    distinct(NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL) %>%
    mutate(
      eixo_cleaned = normalize_text(NO_AREA_CURSO_PROFISSIONAL),
      curso_cleaned = normalize_text(NO_CURSO_EDUC_PROFISSIONAL)
    )
  
  # --- Passo 1.2: Limpar nomes do CNCT ---
  # Remove prefixo "Técnico em " pois o Censo não o utiliza
  df_cnct_cleaned <- df_cnct_ %>%
    distinct(`Eixo Tecnológico`, `Denominação do Curso`) %>%
    mutate(
      eixo_cleaned = normalize_text(`Eixo Tecnológico`),
      curso_cleaned = `Denominação do Curso` %>%
        str_remove(regex("^t[eé]cnico em\\s+", ignore_case = TRUE)) %>%
        normalize_text()
    )
  
  # --- Passo 1.3: Criar tabela de lookup unificada com IDs ---
  # Combina todos os cursos (Censo + CNCT), atribui IDs sequenciais
  # por eixo para permitir tracking ao longo do processamento
  df_lookup <- bind_rows(
    df_censo_cleaned %>% select(eixo_cleaned, curso_cleaned),
    df_cnct_cleaned %>% select(eixo_cleaned, curso_cleaned)
  ) %>%
    distinct() %>%
    arrange(eixo_cleaned, curso_cleaned) %>%
    group_by(eixo_cleaned) %>%
    mutate(
      IDX_EIX = sprintf("%02d", cur_group_id()),
      IDX_CUR = sprintf("%03d", row_number())
    ) %>%
    ungroup() %>%
    mutate(IDX_EIXCUR = paste0(IDX_EIX, IDX_CUR))
  
  # --- Passo 1.4: Aplicar IDs em ambos os datasets ---
  df_censo_cleaned <- df_censo_cleaned %>%
    left_join(df_lookup, by = c("eixo_cleaned", "curso_cleaned")) %>%
    arrange(IDX_EIXCUR)
  
  df_cnct_cleaned <- df_cnct_cleaned %>%
    left_join(df_lookup, by = c("eixo_cleaned", "curso_cleaned"))
  
  # --- Passo 1.5: Matching exato ---
  df_matched <- df_censo_cleaned %>%
    inner_join(df_cnct_cleaned,
               by = c("eixo_cleaned", "curso_cleaned", "IDX_EIX", "IDX_CUR", "IDX_EIXCUR")) %>%
    arrange(IDX_EIXCUR)
  
  # --- Passo 1.6: Identificar não correspondidos ---
  df_unmatched_censo <- df_censo_cleaned %>%
    anti_join(df_matched,
              by = c("eixo_cleaned", "curso_cleaned", "IDX_EIX", "IDX_CUR", "IDX_EIXCUR")) %>%
    arrange(IDX_EIXCUR)
  
  df_unmatched_cnct <- df_cnct_cleaned %>%
    anti_join(df_matched,
              by = c("eixo_cleaned", "curso_cleaned", "IDX_EIX", "IDX_CUR", "IDX_EIXCUR")) %>%
    arrange(IDX_EIXCUR)
  
  message(">> Matched: ", nrow(df_matched), " cursos")
  message(">> Unmatched Censo: ", nrow(df_unmatched_censo), " cursos")
  message(">> Unmatched CNCT: ", nrow(df_unmatched_cnct), " cursos")
  
  # Combinar matched + unmatched do Censo para aplicar patches
  df_censo_touse <- bind_rows(df_matched, df_unmatched_censo) %>%
    arrange(IDX_EIXCUR)
  
  ###############################################################################
  # FASE 2: PATCHES MANUAIS PARA CURSOS NÃO CORRESPONDIDOS
  ###############################################################################
  #
  # Os cursos do Censo que não encontraram correspondência exata no CNCT
  # precisam de correção manual. Cada patch define:
  #
  #   patch_info: tibble com IDX_EIXCUR e denom (nome correto do curso)
  #   tag_map:    tibble com IDX_EIXCUR e tag_cnct (lista de IDX_EIXCUR do
  #               CNCT que correspondem ao curso do Censo)
  #
  # tag_cnct pode ser:
  #   - Um código único: "01012" (correspondência direta)
  #   - Vetor de códigos: c("01006", "01012") (curso genérico que engloba
  #     múltiplos cursos CNCT)
  #   - NULL: curso existe no Censo mas não tem correspondente no CNCT
  #     (cursos descontinuados, genéricos sem match, etc.)
  #
  # ATENÇÃO: Estes patches foram desenvolvidos com grande esforço manual
  # comparando cada curso não correspondido com o catálogo CNCT. Modificar
  # com extremo cuidado. Se o CNCT ou o Censo Escolar forem atualizados,
  # estes patches podem precisar de revisão.
  ###############################################################################
  
  message("=== FASE 2: Patches manuais ===")
  
  # -------------------------------------------------------------------------
  # Função de patch (versão inicial — usada apenas para Eixo 01)
  # Usa left_join e coalesce para preencher campos faltantes.
  # Diferente de patch_censo_area2: não separa linhas, faz join direto.
  # -------------------------------------------------------------------------
  patch_censo_area_initial <- function(df, area_name, patch_info, tag_map) {
    df <- df %>%
      left_join(patch_info, by = "IDX_EIXCUR") %>%
      left_join(tag_map, by = "IDX_EIXCUR") %>%
      mutate(
        `Eixo Tecnológico` = case_when(
          !is.na(denom) ~ area_name,
          TRUE ~ `Eixo Tecnológico`
        ),
        `Denominação do Curso` = coalesce(denom, `Denominação do Curso`),
        tag_cnct = pmap(list(tag_cnct, IDX_EIXCUR, `Eixo Tecnológico`, `Denominação do Curso`),
                        function(x, y, eixo, curso) {
                          if (!is.null(x)) {
                            x
                          } else if (is.na(eixo) && is.na(curso)) {
                            NULL
                          } else {
                            list(y)
                          }
                        })
      ) %>%
      select(-denom)
    return(df)
  }
  
  # -------------------------------------------------------------------------
  # Função de patch (versão principal — usada para todos os eixos exceto 01)
  # Separa linhas a atualizar das demais, aplica join, recombina.
  # Evita duplicatas e problemas de .x/.y em joins repetidos.
  # -------------------------------------------------------------------------
  patch_censo_area2 <- function(df, area_name, patch_info, tag_map) {
    patch_info <- patch_info %>% mutate(IDX_EIXCUR = as.character(IDX_EIXCUR))
    tag_map    <- tag_map    %>% mutate(IDX_EIXCUR = as.character(IDX_EIXCUR))
    df         <- df         %>% mutate(IDX_EIXCUR = as.character(IDX_EIXCUR))
    
    tag_map <- tag_map %>% rename(tag_cnct_new = tag_cnct)
    
    # Linhas a atualizar
    df_patch <- df %>%
      filter(IDX_EIXCUR %in% patch_info$IDX_EIXCUR) %>%
      left_join(patch_info, by = "IDX_EIXCUR") %>%
      left_join(tag_map, by = "IDX_EIXCUR") %>%
      mutate(
        `Eixo Tecnológico` = area_name,
        `Denominação do Curso` = ifelse(!is.na(denom), denom, `Denominação do Curso`),
        tag_cnct = ifelse(
          map_lgl(tag_cnct, is.null),
          tag_cnct_new,
          tag_cnct
        )
      ) %>%
      select(-denom, -tag_cnct_new)
    
    # Linhas intactas
    df_rest <- df %>%
      filter(!IDX_EIXCUR %in% patch_info$IDX_EIXCUR)
    
    bind_rows(df_rest, df_patch) %>% arrange(IDX_EIXCUR)
  }
  
  # =========================================================================
  # EIXO 01: AMBIENTE E SAÚDE
  # =========================================================================
  # 01011: "Gerência em Saúde" no Censo → não existe no CNCT com este nome
  #         Mapeado para 01012 (curso CNCT mais próximo)
  # 01024: "Outros - Eixo Ambiente e Saúde" (curso genérico residual)
  #         Mapeado para 01006 + 01012 (cursos CNCT mais representativos)
  # =========================================================================
  
  patch_info_amb <- tibble(
    IDX_EIXCUR = c("01011", "01024"),
    denom = c("Técnico em Gerência em Saúde",
              "Técnico em Outros - Eixo Ambiente e Saúde")
  )
  tag_map_amb <- tibble(
    IDX_EIXCUR = c("01011", "01024"),
    tag_cnct = list("01012", c("01006", "01012"))
  )
  df_censo_touse <- patch_censo_area_initial(
    df = df_censo_touse,
    area_name = "Ambiente e Saúde",
    patch_info = patch_info_amb,
    tag_map = tag_map_amb
  )
  
  # =========================================================================
  # EIXO 02: CONTROLE E PROCESSOS INDUSTRIAIS
  # =========================================================================
  # 02023: "Outros - Eixo Controle e Processos Industriais" (genérico)
  #         Mapeado para 02007 (Técnico em Ferramentaria)
  # =========================================================================
  
  patch_info_ctrl <- tibble(
    IDX_EIXCUR = c("02023"),
    denom = c("Técnico em Outros - Eixo Controle e Processos Industriais")
  )
  tag_map_ctrl <- tibble(
    IDX_EIXCUR = c("02023"),
    tag_cnct = list("02007")
  )
  df_censo_touse <- patch_censo_area2(
    df = df_censo_touse,
    area_name = "Controle e Processos Industriais",
    patch_info = patch_info_ctrl,
    tag_map = tag_map_ctrl
  )
  
  # =========================================================================
  # EIXO 03: DESENVOLVIMENTO EDUCACIONAL E SOCIAL
  # =========================================================================
  # 03010: "Produção de Materiais Didáticos Bilíngues em Libras/Língua
  #          Portuguesa" — nome diferente no Censo vs CNCT → mapeado 03011
  # 03009: "Treinamento e Instrução de Cães-Guia" — não existe no CNCT
  #          com este nome exato → mapeado 03014
  # =========================================================================
  
  patch_info_dev <- tibble(
    IDX_EIXCUR = c("03010", "03009"),
    denom = c("Técnico em Produção de Materiais Didáticos Bilíngues em Libras/Língua Portuguesa",
              "Técnico em Treinamento e Instrução de Cães-Guia")
  )
  tag_map_dev <- tibble(
    IDX_EIXCUR = c("03010", "03009"),
    tag_cnct = list("03011", "03014")
  )
  df_censo_touse <- patch_censo_area2(
    df = df_censo_touse,
    area_name = "Desenvolvimento Educacional e Social",
    patch_info = patch_info_dev,
    tag_map = tag_map_dev
  )
  
  # =========================================================================
  # EIXO 04: GESTÃO E NEGÓCIOS
  # =========================================================================
  # 04010: "Outros - Eixo Gestão e Negócios" (genérico)
  #         SEM correspondente no CNCT (tag_cnct = NULL)
  # =========================================================================
  
  patch_info_gestao <- tibble(
    IDX_EIXCUR = c("04010"),
    denom = c("Técnico em Outros - Eixo Gestão e Negócios")
  )
  tag_map_gestao <- tibble(
    IDX_EIXCUR = c("04010"),
    tag_cnct = list(NULL)
  )
  df_censo_touse <- patch_censo_area2(
    df = df_censo_touse,
    area_name = "Gestão e Negócios",
    patch_info = patch_info_gestao,
    tag_map = tag_map_gestao
  )
  
  # =========================================================================
  # EIXO 05: INFORMAÇÃO E COMUNICAÇÃO
  # =========================================================================
  # 05006: "Outros - Eixo Informação e Comunicação" (genérico)
  #         SEM correspondente no CNCT (tag_cnct = NULL)
  # =========================================================================
  
  patch_info_info <- tibble(
    IDX_EIXCUR = c("05006"),
    denom = c("Técnico em Outros - Eixo Informação e Comunicação")
  )
  tag_map_info <- tibble(
    IDX_EIXCUR = c("05006"),
    tag_cnct = list(NULL)
  )
  df_censo_touse <- patch_censo_area2(
    df = df_censo_touse,
    area_name = "Informação e Comunicação",
    patch_info = patch_info_info,
    tag_map = tag_map_info
  )
  
  # =========================================================================
  # EIXO 06: INFRAESTRUTURA
  # =========================================================================
  # 06001: "Aeroportuário" — nome diferente no Censo → mapeado 06013
  # 06010: "Outros - Eixo Infraestrutura" (genérico)
  #         Mapeado para 06003 (Carpintaria) + 06009 (Hidrologia)
  # =========================================================================
  
  patch_info_infra <- tibble(
    IDX_EIXCUR = c("06001", "06010"),
    denom = c("Técnico em Aeroportuário",
              "Técnico em Outros - Eixo Infraestrutura")
  )
  tag_map_infra <- tibble(
    IDX_EIXCUR = c("06001", "06010"),
    tag_cnct = list("06013", c("06003", "06009"))
  )
  df_censo_touse <- patch_censo_area2(
    df = df_censo_touse,
    area_name = "Infraestrutura",
    patch_info = patch_info_infra,
    tag_map = tag_map_infra
  )
  
  # =========================================================================
  # EIXO 07: MILITAR
  # =========================================================================
  # Há 22 cursos militares no CNCT que NÃO aparecem no Censo Escolar
  # (são oferecidos por instituições militares fora do sistema regular).
  # Nenhum patch necessário — esses cursos serão excluídos das agregações
  # finais com filter(Eixo_Tecnologico_CNCT != "Militar").
  # =========================================================================
  
  # =========================================================================
  # EIXO 08: PRODUÇÃO ALIMENTÍCIA
  # =========================================================================
  # Todos os cursos corresponderam no matching exato. Nenhum patch necessário.
  # =========================================================================
  
  # =========================================================================
  # EIXO 09: PRODUÇÃO CULTURAL E DESIGN
  # =========================================================================
  # 09021: "Instrumento Musical" → mapeado 09020 (nome similar no CNCT)
  # 09025: "Outros - Eixo Produção Cultural e Design" (genérico)
  #         SEM correspondente CNCT
  # 09027: "Processos Fonográficos" — curso descontinuado
  #         SEM correspondente CNCT
  # =========================================================================
  
  patch_info_pcd <- tibble(
    IDX_EIXCUR = c("09021", "09025", "09027"),
    denom = c("Técnico em Instrumento Musical",
              "Técnico em Outros - Eixo Produção Cultural e Design",
              "Técnico em Processos Fonográficos")
  )
  tag_map_pcd <- tibble(
    IDX_EIXCUR = c("09021", "09025", "09027"),
    tag_cnct = list("09020", NULL, NULL)
  )
  df_censo_touse <- patch_censo_area2(
    df = df_censo_touse,
    area_name = "Produção Cultural e Design",
    patch_info = patch_info_pcd,
    tag_map = tag_map_pcd
  )
  
  # =========================================================================
  # EIXO 12: SEGURANÇA
  # =========================================================================
  # 12002: "Outros - Eixo Segurança" (genérico)
  #         SEM correspondente CNCT
  # 12004: "Prevenção e Combate a Incêndios" → mapeado 12003
  # =========================================================================
  
  patch_info_seg <- tibble(
    IDX_EIXCUR = c("12002", "12004"),
    denom = c("Técnico em Outros - Eixo Segurança",
              "Técnico em Prevenção e Combate a Incêndios")
  )
  tag_map_seg <- tibble(
    IDX_EIXCUR = c("12002", "12004"),
    tag_cnct = list(NULL, "12003")
  )
  df_censo_touse <- patch_censo_area2(
    df = df_censo_touse,
    area_name = "Segurança",
    patch_info = patch_info_seg,
    tag_map = tag_map_seg
  )
  
  # =========================================================================
  # EIXOS 10, 11, 13: PATCHES RESIDUAIS
  # =========================================================================
  # Cursos genéricos ("Outros") e cursos descontinuados que não têm
  # correspondente no CNCT. Todos recebem tag_cnct = NULL.
  #
  # 10012: "Outros - Eixo Produção Industrial"
  # 11012: "Outros - Eixo Recursos Naturais"
  # 11014: "Pós-Colheita" — curso descontinuado no CNCT
  # 11015: "Recursos Minerais" — nomenclatura diferente, sem match exato
  # 13007: "Outros - Eixo Turismo, hospitalidade e lazer"
  # =========================================================================
  
  patch_info_other <- tibble(
    IDX_EIXCUR = c("10012", "11012", "11014", "11015", "13007"),
    denom = c("Técnico em Outros - Eixo Produção Industrial",
              "Técnico em Outros - Eixo Recursos Naturais",
              "Técnico em Pós-Colheita",
              "Técnico em Recursos Minerais",
              "Técnico em Outros - Eixo Turismo, hospitalidade e lazer")
  )
  tag_map_other <- tibble(
    IDX_EIXCUR = patch_info_other$IDX_EIXCUR,
    tag_cnct = replicate(nrow(patch_info_other), list(NULL))
  )
  df_censo_touse <- patch_censo_area2(
    df = df_censo_touse,
    area_name = NA,  # Manter eixo existente (cada um pertence a eixo diferente)
    patch_info = patch_info_other,
    tag_map = tag_map_other
  )
  
  ###############################################################################
  # FASE 3: ENRIQUECIMENTO COM METADADOS CNCT
  ###############################################################################
  #
  # Agora que todos os cursos do Censo têm um tag_cnct (ou NULL para os
  # sem match), usamos o tag_cnct para buscar os metadados completos do CNCT:
  # Eixo, Área, Denominação, Perfil, Campo de Atuação, CBO, Infraestrutura.
  ###############################################################################
  
  message("=== FASE 3: Enriquecimento com metadados CNCT ===")
  
  # Tabela de referência CNCT com IDX_EIXCUR para join
  df_cnct_full <- df_cnct_ %>%
    mutate(
      eixo_cleaned = normalize_text(`Eixo Tecnológico`),
      curso_cleaned = `Denominação do Curso` %>%
        str_remove(regex("^t[eé]cnico em\\s+", ignore_case = TRUE)) %>%
        normalize_text()
    ) %>%
    left_join(df_cnct_cleaned %>% select(eixo_cleaned, curso_cleaned, IDX_EIXCUR),
              by = c("eixo_cleaned", "curso_cleaned"))
  
  # Resolver tag_cnct para obter metadados CNCT completos
  # Esta tabela é o crosswalk entre IDX_EIXCUR do Censo e metadados do CNCT.
  # Filtra apenas cursos com tag_cnct não-nulo e de comprimento 1
  # (cursos com múltiplos tags usam o primeiro como representativo)
  df_cross_censo_cnct <- df_censo_touse %>%
    filter(map_lgl(tag_cnct, ~ length(.) == 1 && !is.null(.))) %>%
    mutate(tag_cnct = map_chr(tag_cnct, 1)) %>%
    left_join(
      df_cnct_full %>%
        select(IDX_EIXCUR, `Área Tecnológica`, `Perfil Profissional de Conclusão`,
               `Campo de Atuação`, `Ocupações CBO Associadas`, `Infraestrutura Mínima`),
      by = c("tag_cnct" = "IDX_EIXCUR")
    ) %>%
    arrange(IDX_EIXCUR)
  
  message(">> Crosswalk Censo→CNCT: ", nrow(df_cross_censo_cnct), " cursos mapeados")
  
  ###############################################################################
  # FASE 4: JOIN DE VOLTA AOS MICRODADOS E SEPARAÇÃO MATCHED/UNMATCHED
  ###############################################################################
  
  message("=== FASE 4: Join com microdados e separação ===")
  
  # Passo 4.1: Atribuir IDX_EIXCUR aos microdados do suplemento técnico
  df_censo_supl_tec2 <- df_censo_supl_tec %>%
    left_join(
      df_censo_cleaned %>%
        select(NO_AREA_CURSO_PROFISSIONAL, NO_CURSO_EDUC_PROFISSIONAL,
               IDX_EIX, IDX_CUR, IDX_EIXCUR),
      by = c("NO_AREA_CURSO_PROFISSIONAL", "NO_CURSO_EDUC_PROFISSIONAL")
    )
  
  # Passo 4.2: Enriquecer com metadados CNCT via IDX_EIXCUR
  df_censo_supl_tec3 <- df_censo_supl_tec2 %>%
    left_join(
      df_cross_censo_cnct %>%
        select(IDX_EIXCUR,
               Eixo_Tecnologico_CNCT = `Eixo Tecnológico`,
               Denominacao_Curso_CNCT = `Denominação do Curso`,
               Area_Tecnologica_CNCT = `Área Tecnológica`,
               Perfil_CNCT = `Perfil Profissional de Conclusão`,
               Campo_CNCT = `Campo de Atuação`,
               Ocupacoes_CNCT = `Ocupações CBO Associadas`,
               Infra_CNCT = `Infraestrutura Mínima`),
      by = "IDX_EIXCUR"
    )
  
  # Passo 4.3: Separar matched e unmatched
  # Unmatched: cursos sem correspondência no CNCT (NA em Eixo_Tecnologico_CNCT)
  # Estes vão para df_censo_notin_cnct, usado na Aba C3 do Shiny
  df_censo_notin_cnct <- df_censo_supl_tec3 %>%
    filter(is.na(Eixo_Tecnologico_CNCT))
  
  mat_notin <- sum(df_censo_notin_cnct$QT_MAT_CURSO_TEC)
  message(">> Cursos sem match CNCT: ", nrow(df_censo_notin_cnct), " linhas, ",
          format(mat_notin, big.mark = "."), " matrículas")
  
  # Matched: cursos com correspondência CNCT confirmada
  df_censo_supl_tec_4qbqALL <- df_censo_supl_tec3 %>%
    filter(!is.na(Eixo_Tecnologico_CNCT))
  
  mat_matched <- sum(df_censo_supl_tec_4qbqALL$QT_MAT_CURSO_TEC)
  message(">> Cursos com match CNCT: ", nrow(df_censo_supl_tec_4qbqALL), " linhas, ",
          format(mat_matched, big.mark = "."), " matrículas")
  message(">> Taxa de matching: ",
          sprintf("%.1f%%", 100 * mat_matched / (mat_matched + mat_notin)))
  
  # Salvar matched e unmatched
  dir.create("working/mec_inep", recursive = TRUE, showWarnings = FALSE)
  dir.create("working/qbq", recursive = TRUE, showWarnings = FALSE)
  
  save(df_censo_notin_cnct, file = SAIDAS$df_censo_notin_cnct$local)
  save(df_censo_supl_tec_4qbqALL, file = SAIDAS$df_censo_supl_tec_4qbqALL$local)
  
  ###############################################################################
  # FASE 5: AGREGAÇÕES POR UF, EIXO, ÁREA E CURSO
  ###############################################################################
  #
  # Gerar tabelas agregadas para uso nas Abas E1 do Shiny.
  # Cada tabela inclui linha "Brasil" (CO_UF=0, SG_UF="BR") como total.
  # O eixo "Militar" é excluído (não faz parte do sistema regular de EPT).
  ###############################################################################
  
  message("=== FASE 5: Agregações para Aba E1 ===")
  
  # Tabela de UFs para merge geográfico
  temp_UFs <- df_codes_ibge %>%
    select(CO_MUN, NM_UF, SG_UF, CO_UF) %>%
    distinct() %>%
    arrange(SG_UF)
  
  # Dataset de trabalho: matched + geografia, sem Militar
  df_censo_cnct <- left_join(df_censo_supl_tec_4qbqALL, temp_UFs, by = "CO_MUN") %>%
    filter(Eixo_Tecnologico_CNCT != "Militar")
  
  # --- 5.1: Agregação por UF ---
  df_mat_uf <- df_censo_cnct %>%
    group_by(CO_UF, NM_UF, SG_UF, ANO) %>%
    summarise(QT_MAT_CURSO_TEC_UF = sum(QT_MAT_CURSO_TEC, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(SG_UF)
  
  # Linha Brasil
  total_uf <- df_mat_uf %>%
    group_by(ANO) %>%
    summarise(QT_MAT_CURSO_TEC_UF = sum(QT_MAT_CURSO_TEC_UF, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(CO_UF = 0L, NM_UF = "Brasil", SG_UF = "BR") %>%
    relocate(CO_UF, NM_UF, SG_UF, ANO)
  
  df_mat_uf <- bind_rows(df_mat_uf, total_uf) %>%
    arrange(ANO, desc(SG_UF)) %>% ungroup()
  
  # --- 5.2: Agregação por Eixo Tecnológico ---
  df_mat_eixo <- df_censo_cnct %>%
    group_by(CO_UF, NM_UF, SG_UF, ANO, Eixo_Tecnologico_CNCT) %>%
    summarise(QT_MAT_CURSO_TEC_EIX = sum(QT_MAT_CURSO_TEC, na.rm = TRUE),
              .groups = "drop") %>%
    rename(`Eixo Tecnológico` = Eixo_Tecnologico_CNCT)
  
  total_eixo <- df_mat_eixo %>%
    group_by(`Eixo Tecnológico`, ANO) %>%
    summarise(QT_MAT_CURSO_TEC_EIX = sum(QT_MAT_CURSO_TEC_EIX, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(CO_UF = 0L, NM_UF = "Brasil", SG_UF = "BR") %>%
    relocate(CO_UF, NM_UF, SG_UF, ANO)
  
  df_mat_eixo <- bind_rows(df_mat_eixo, total_eixo) %>%
    arrange(ANO, `Eixo Tecnológico`, desc(SG_UF)) %>% ungroup()
  
  # --- 5.3: Agregação por Área Tecnológica ---
  df_mat_area <- df_censo_cnct %>%
    group_by(CO_UF, NM_UF, SG_UF, ANO, Area_Tecnologica_CNCT) %>%
    summarise(QT_MAT_CURSO_TEC_ARE = sum(QT_MAT_CURSO_TEC, na.rm = TRUE),
              .groups = "drop") %>%
    rename(`Área Tecnológica` = Area_Tecnologica_CNCT)
  
  total_area <- df_mat_area %>%
    group_by(`Área Tecnológica`, ANO) %>%
    summarise(QT_MAT_CURSO_TEC_ARE = sum(QT_MAT_CURSO_TEC_ARE, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(CO_UF = 0L, NM_UF = "Brasil", SG_UF = "BR") %>%
    relocate(CO_UF, NM_UF, SG_UF, ANO)
  
  df_mat_area <- bind_rows(df_mat_area, total_area) %>%
    arrange(ANO, `Área Tecnológica`, desc(SG_UF)) %>% ungroup()
  
  # --- 5.4: Agregação por Curso ---
  df_mat_curso <- df_censo_cnct %>%
    group_by(CO_UF, NM_UF, SG_UF, ANO, IDX_EIXCUR, Denominacao_Curso_CNCT) %>%
    summarise(QT_MAT_CURSO_TEC_CUR = sum(QT_MAT_CURSO_TEC, na.rm = TRUE),
              .groups = "drop") %>%
    rename(`Denominação do Curso` = Denominacao_Curso_CNCT)
  
  total_curso <- df_mat_curso %>%
    group_by(`Denominação do Curso`, IDX_EIXCUR, ANO) %>%
    summarise(QT_MAT_CURSO_TEC_CUR = sum(QT_MAT_CURSO_TEC_CUR, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(CO_UF = 0L, NM_UF = "Brasil", SG_UF = "BR") %>%
    select(CO_UF, NM_UF, SG_UF, ANO, `Denominação do Curso`, IDX_EIXCUR, QT_MAT_CURSO_TEC_CUR)
  
  df_mat_curso <- bind_rows(df_mat_curso, total_curso) %>%
    arrange(ANO, `Denominação do Curso`, desc(SG_UF)) %>% ungroup()
  
  # --- 5.5: Hierarquia Eixo → Área → Curso ---
  df_exarcu <- df_censo_cnct %>%
    distinct(
      IDX_EIXCUR,
      `Eixo Tecnológico`     = Eixo_Tecnologico_CNCT,
      `Área Tecnológica`     = Area_Tecnologica_CNCT,
      `Denominação do Curso` = Denominacao_Curso_CNCT
    ) %>%
    arrange(`Eixo Tecnológico`, `Área Tecnológica`, `Denominação do Curso`)
  
  # Salvar agregações
  save(df_mat_uf,    file = SAIDAS$df_mat_uf$local)
  save(df_mat_eixo,  file = SAIDAS$df_mat_eixo$local)
  save(df_mat_area,  file = SAIDAS$df_mat_area$local)
  save(df_mat_curso, file = SAIDAS$df_mat_curso$local)
  save(df_exarcu,    file = SAIDAS$df_exarcu$local)
  
  write.csv(df_exarcu, file = "working/mec_inep/df_exarcu.csv", row.names = FALSE)
  
  ###############################################################################
  # FASE 6: EXTRAÇÃO DE CÓDIGOS CBO PARA MATCHING COM RAIS
  ###############################################################################
  #
  # O CNCT lista as "Ocupações CBO Associadas" como texto livre com códigos
  # CBO embutidos. Precisamos extrair esses códigos para fazer o matching
  # com dados de emprego RAIS (que usa códigos CBO de 6 dígitos).
  #
  # Desafios:
  #   - Códigos aparecem em formatos diferentes: 317105, 3171-05, ou 3171
  #   - Alguns cursos têm "Ocupação ainda não classificada"
  #   - Códigos de 4 dígitos (famílias CBO) precisam ser expandidos para
  #     os subcódigos de 6 dígitos manualmente
  ###############################################################################
  
  message("=== FASE 6: Extração de códigos CBO ===")
  
  # Resumo dos cursos matched para extração de CBO
  df_censo_supl_tec_4qbq <- df_censo_supl_tec_4qbqALL %>%
    select(IDX_EIXCUR, Eixo_Tecnologico_CNCT,
           Denominacao_Curso_CNCT, Area_Tecnologica_CNCT, Perfil_CNCT,
           Campo_CNCT, Ocupacoes_CNCT, Infra_CNCT) %>%
    unique() %>%
    arrange(IDX_EIXCUR)
  
  # -------------------------------------------------------------------------
  # Expansão manual de códigos CBO de 4 dígitos (famílias) para 6 dígitos
  #
  # O CNCT às vezes lista apenas o código de família CBO (4 dígitos).
  # Para matching com RAIS (6 dígitos), expandimos manualmente.
  # Fonte: Classificação Brasileira de Ocupações (CBO 2002)
  # -------------------------------------------------------------------------
  expand_manual <- list(
    "3171" = c("317105", "317110", "317115", "317120"),  # Técnicos em informática
    "3172" = c("317205", "317210"),                       # Técnicos em telecomunicações
    "3513" = c("351305", "351310", "351315"),             # Técnicos em administração
    "3515" = c("351505", "351510", "351515"),             # Técnicos em contabilidade
    "3742" = c("374205", "374210", "374215")              # Técnicos em design
  )
  
  # Extrair e limpar códigos CBO
  df_censo_supl_tec_4qbq <- df_censo_supl_tec_4qbq %>%
    mutate(
      # Tratar ocupações não classificadas
      Ocupacoes_CNCT = str_trim(Ocupacoes_CNCT),
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
          if (code %in% names(expand_manual)) {
            expand_manual[[code]]
          } else {
            code
          }
        }))
        unique(expanded)
      })
    ) %>%
    arrange(Ocupacoes_CNCT)
  
  # Verificar se restam códigos de 4 dígitos não expandidos
  leftover_4digit <- df_censo_supl_tec_4qbq %>%
    mutate(flat_cbo = map(cbo_list, ~ .x[str_length(.x) == 4])) %>%
    filter(map_lgl(flat_cbo, ~ length(.x) > 0))
  
  if (nrow(leftover_4digit) > 0) {
    warning(">> ATENÇÃO: ", nrow(leftover_4digit),
            " cursos com códigos CBO de 4 dígitos não expandidos!")
  } else {
    message(">> Todos os códigos CBO expandidos para 6 dígitos")
  }
  
  # Salvar
  save(df_censo_supl_tec_4qbq, file = SAIDAS$df_censo_supl_tec_4qbq$local)
  openxlsx::write.xlsx(df_censo_supl_tec_4qbq,
                       "working/qbq/df_censo_supl_tec_4qbq.xlsx", rowNames = FALSE)
  
  ###############################################################################
  # UPLOAD CONDICIONAL DE TODOS OS ARQUIVOS
  ###############################################################################
  
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
message(">> df_censo_notin_cnct: ", nrow(df_censo_notin_cnct), " linhas (",
        format(sum(df_censo_notin_cnct$QT_MAT_CURSO_TEC), big.mark = "."),
        " matrículas)")
message(">> df_mat_uf: ", nrow(df_mat_uf), " linhas")
message(">> df_mat_eixo: ", nrow(df_mat_eixo), " linhas")
message(">> df_mat_area: ", nrow(df_mat_area), " linhas")
message(">> df_mat_curso: ", nrow(df_mat_curso), " linhas")
message(">> df_exarcu: ", nrow(df_exarcu), " cursos na hierarquia")
message(">> df_censo_supl_tec_4qbq: ", nrow(df_censo_supl_tec_4qbq),
        " cursos com CBO extraídos")
message("=== qbq_02a.R concluído ===")