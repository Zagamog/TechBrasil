###############################################################################
# modelo_ept_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Dataset de Estimação para Modelo Econométrico EPT
#
# USO NAS ABAS: C4 (Modelo Econométrico), via modelo_ept_03a.R
#
# OBJETIVO:
#   Construir painel de dados para regressão econométrica que relaciona
#   matrículas de educação técnica (EPT) com estrutura econômica setorial
#   (PIB por setor) e alinhamento entre oferta EPT e demanda econômica.
#
#   O painel cruza:
#     - Matrículas EPT por curso → agrupadas por setor econômico
#     - PIB municipal → agregado por UF e decomposto em 4 setores
#     - Indicador de alinhamento: proporção EPT / proporção econômica
#
#   DIMENSÕES DO PAINEL:
#     - Setor econômico (4): agriculture, industry, services, administration
#     - UF (27 estados)
#     - Dependência administrativa (Federal, Estadual, Municipal, Privada)
#     - Ano (2007-2024, conforme disponibilidade dos dados)
#
#   MAPEAMENTO ÁREA TECNOLÓGICA → SETOR ECONÔMICO:
#     O CNCT organiza cursos em ~50 Áreas Tecnológicas dentro de 13 Eixos.
#     Cada Área é mapeada para um dos 4 setores econômicos do PIB (IBGE).
#     O mapeamento usa Área Tecnológica como chave primária, com fallback
#     para Eixo Tecnológico quando a Área não tem mapeamento direto.
#
# INSUMOS PROCESSADOS (S3):
#   working/mec_inep/Tecnico_Forma_Cursos_Garabed1.rda (de censo_garabed_01a.R)
#   working/mec_inep/df_exarcu.rda (de qbq_02a.R)
#   working/ibge/df_pibmunis.rda (de TB_municipios_01a.R — versão xlsx)
#
# DADOS PROCESSADOS (S3 e local):
#   working/rais/df_model_ept1a.rda
#
# NOTA SOBRE COLUNAS DO PIB:
#   As colunas econômicas usam nomes curtos padronizados:
#   VAB_AGRO, VAB_IND, VAB_SERV, VAB_ADM, VAB_TOTAL, PIB, PIB_PC.
#   As colunas geográficas usam make.names(): Sigla.da.Unidade.da.Federação.
#
# CREDENCIAIS AWS: .env na raiz do projeto
# DEPENDÊNCIAS: dplyr, tidyr, stringr, aws.s3, dotenv
# SAÍDA: df_model_ept1a.rda
###############################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(aws.s3)
library(dotenv)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

dotenv::load_dot_env()
S3_BUCKET <- "techbrazildata"

# Insumos
INSUMOS <- list(
  tecnico_cursos = list(local = "working/mec_inep/Tecnico_Forma_Cursos_Garabed1.rda",
                        s3    = "working/mec_inep/Tecnico_Forma_Cursos_Garabed1.rda"),
  df_exarcu      = list(local = "working/mec_inep/df_exarcu.rda",
                        s3    = "working/mec_inep/df_exarcu.rda"),
  df_pibmunis    = list(local = "working/ibge/df_pibmunis.rda",
                        s3    = "working/ibge/df_pibmunis.rda")
)

# Saída
S3_SAIDA    <- "working/rais/df_model_ept1a.rda"
LOCAL_SAIDA <- "working/rais/df_model_ept1a.rda"

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
  message("=== df_model_ept1a local encontrado e válido ===")
  
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
  message(">> df_model_ept1a carregado: ", nrow(df_model_ept1a), " observações")
  
} else {
  
  ###############################################################################
  # PASSO 0: SINCRONIZAR E CARREGAR INSUMOS
  ###############################################################################
  
  message("=== Processamento necessário — sincronizando insumos ===")
  for (nome in names(INSUMOS)) {
    sincronizar_s3(INSUMOS[[nome]]$local, INSUMOS[[nome]]$s3, S3_BUCKET)
  }
  
  load(INSUMOS$tecnico_cursos$local)  # → Tecnico_Forma_Cursos_Garabed1
  load(INSUMOS$df_exarcu$local)       # → df_exarcu
  load(INSUMOS$df_pibmunis$local)     # → df_pibmunis
  
  ###############################################################################
  # PASSO 1: LIMPEZA DE NOMES DE CURSOS E JOINING COM CLASSIFICAÇÃO
  ###############################################################################
  
  message("=== PASSO 1: Matching cursos EPT com classificação CNCT ===")
  
  # Limpar nomes de cursos no Censo (remover prefixo "Técnico em ")
  enrollment_clean <- Tecnico_Forma_Cursos_Garabed1 %>%
    mutate(
      nome_curso_clean = str_remove(NOME_CURSO, "^Técnico em\\s+"),
      nome_curso_clean = str_trim(tolower(nome_curso_clean))
    )
  
  # Limpar nomes na classificação CNCT
  classification_clean <- df_exarcu %>%
    mutate(
      denominacao_clean = str_remove(`Denominação do Curso`, "^Técnico em\\s+"),
      denominacao_clean = str_trim(tolower(denominacao_clean))
    )
  
  # Join para obter Área Tecnológica de cada curso
  enrollment_with_area <- enrollment_clean %>%
    left_join(classification_clean,
              by = c("nome_curso_clean" = "denominacao_clean")) %>%
    filter(!is.na(`Área Tecnológica`))
  
  message(">> Cursos matchados com Área Tecnológica: ",
          n_distinct(enrollment_with_area$nome_curso_clean))
  
  ###############################################################################
  # PASSO 2: MAPEAMENTO ÁREA TECNOLÓGICA → SETOR ECONÔMICO
  ###############################################################################
  #
  # Cada Área Tecnológica do CNCT é mapeada para um dos 4 setores econômicos
  # do PIB municipal (IBGE): agropecuária, indústria, serviços, administração.
  #
  # Mapeamento primário: por Área Tecnológica (mais preciso)
  # Mapeamento fallback: por Eixo Tecnológico (para áreas não mapeadas)
  #
  # Correspondência com colunas do PIB:
  #   agriculture    → VAB_AGRO  (Agropecuária)
  #   industry       → VAB_IND   (Indústria)
  #   services       → VAB_SERV  (Serviços exceto administração pública)
  #   administration → VAB_ADM   (Administração, defesa, educação, saúde pública)
  ###############################################################################
  
  message("=== PASSO 2: Mapeamento setorial ===")
  
  # Mapeamento primário: Área Tecnológica → Setor Econômico
  area_to_sector_mapping <- tribble(
    ~area_tecnologica, ~economic_sector,
    
    # AGROPECUÁRIA
    "Pesca e Aquicultura",             "agriculture",
    "Produção Agrícola e Pecuária",    "agriculture",
    "Silvicultura",                    "agriculture",
    
    # INDÚSTRIA
    "Construção de Obras",             "industry",
    "Eletrônica e Automação",          "industry",
    "Manufatura",                      "industry",
    "Materiais",                       "industry",
    "Metalmecânica",                   "industry",
    "Mineração e Extração",            "industry",
    "Química",                         "industry",
    "Sistemas de Energia",             "industry",
    "Tecnologia, Inovação e Práticas Laboratoriais", "industry",
    
    # SERVIÇOS
    "Acolhimento e Hospedagem",        "services",
    "Atividades Turísticas",           "services",
    "Comercial",                       "services",
    "Comunicação Midiática",           "services",
    "Design",                          "services",
    "Gerencial",                       "services",
    "Gestão e Promoção da Saúde e Bem-Estar", "services",
    "Infraestrutura de Informação e Comunicação", "services",
    "Manifestações Artísticas",        "services",
    "Manutenção e Operação",           "services",
    "Mensuração Espacial e Volumétrica", "services",
    "Operações de Transporte",         "services",
    "Operações Financeiras",           "services",
    "Têxtil e Vestuário",              "services",
    
    # ADMINISTRAÇÃO PÚBLICA
    "Apoio técnico a eventos",         "administration",
    "Desenvolvimento de Sistemas",     "administration",
    "Gestão Educacional",              "administration",
    "Intervenção Social",              "administration",
    "Proteção e Reabilitação de Ecossistemas", "administration",
    "Recreação e Sociabilidade",       "administration"
  )
  
  # Mapeamento fallback: Eixo Tecnológico → Setor Econômico
  eixo_to_sector_mapping <- tribble(
    ~eixo_tecnologico, ~economic_sector,
    "Ambiente e Saúde",                          "services",
    "Recursos Naturais",                         "agriculture",
    "Produção Cultural e Design",                "services",
    "Controle e Processos Industriais",          "industry",
    "Gestão e Negócios",                         "services",
    "Desenvolvimento Educacional e Social",      "administration",
    "Produção Industrial",                       "industry",
    "Turismo, Hospitalidade e Lazer",            "services",
    "Informação e Comunicação",                  "services",
    "Produção Alimentícia",                      "agriculture",
    "Infraestrutura",                            "industry",
    "Segurança",                                 "administration"
  )
  
  # Aplicar mapeamento com fallback
  enrollment_with_sectors <- enrollment_with_area %>%
    left_join(area_to_sector_mapping,
              by = c("Área Tecnológica" = "area_tecnologica")) %>%
    left_join(eixo_to_sector_mapping,
              by = c("Eixo Tecnológico" = "eixo_tecnologico")) %>%
    mutate(economic_sector = coalesce(economic_sector.x, economic_sector.y)) %>%
    select(-economic_sector.x, -economic_sector.y)
  
  message(">> Distribuição setorial: ",
          paste(names(table(enrollment_with_sectors$economic_sector)),
                table(enrollment_with_sectors$economic_sector),
                sep = "=", collapse = ", "))
  
  ###############################################################################
  # PASSO 3: AGREGAR PARA ESTRUTURA DO PAINEL
  ###############################################################################
  
  message("=== PASSO 3: Agregação para painel ===")
  
  # Painel: setor × UF × dependência × ano
  panel_enrollment <- enrollment_with_sectors %>%
    group_by(ANO, SG_UF, TP_DEPENDENCIA, economic_sector) %>%
    summarise(
      QT_MAT_CURSO_TEC = sum(QT_MAT_CURSO_TEC, na.rm = TRUE),
      n_areas = n_distinct(`Área Tecnológica`),
      .groups = "drop"
    )
  
  message(">> Painel: ", nrow(panel_enrollment), " observações")
  
  ###############################################################################
  # PASSO 4: PREPARAR DADOS DE PIB (NÍVEL UF-ANO)
  ###############################################################################
  #
  # NOTA: df_pibmunis agora usa nomes curtos para colunas econômicas:
  #   VAB_AGRO, VAB_IND, VAB_SERV, VAB_ADM, VAB_TOTAL, PIB, PIB_PC
  #   (gerados por TB_municipios_01a.R — versão xlsx)
  #
  # Colunas geográficas usam padrão make.names():
  #   Sigla.da.Unidade.da.Federação, Nome.da.Unidade.da.Federação, etc.
  ###############################################################################
  
  message("=== PASSO 4: Preparação de dados PIB por UF ===")
  
  # Selecionar colunas relevantes usando nomes (não posições!)
  pib_data <- df_pibmunis %>%
    select(
      year     = Ano,
      SG_UF    = Sigla.da.Unidade.da.Federação,
      NM_UF    = Nome.da.Unidade.da.Federação,
      CO_MUN   = Código.do.Município,
      NM_MUN   = Nome.do.Município,
      PIB, PIB_PC,
      VAB_AGRO, VAB_IND, VAB_SERV, VAB_ADM, VAB_TOTAL
    ) %>%
    mutate(
      # Calcular população implícita: PIB (R$ mil) / PIB_PC (R$ 1,00) * 1000
      population = ifelse(
        is.na(PIB_PC) | PIB_PC == 0,
        NA_real_,
        (PIB * 1000) / PIB_PC
      )
    )
  
  # Agregar para nível UF-ano
  pib_state_year <- pib_data %>%
    group_by(SG_UF, NM_UF, year) %>%
    summarise(
      pib_total    = sum(PIB, na.rm = TRUE),
      population   = sum(population, na.rm = TRUE),
      agro_va      = sum(VAB_AGRO, na.rm = TRUE),
      industry_va  = sum(VAB_IND, na.rm = TRUE),
      services_va  = sum(VAB_SERV, na.rm = TRUE),
      admin_va     = sum(VAB_ADM, na.rm = TRUE),
      total_va     = sum(VAB_TOTAL, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # PIB per capita estadual
      pib_per_capita = pib_total / population,
      
      # Participação setorial no VAB
      agro_share     = agro_va / total_va,
      industry_share = industry_va / total_va,
      services_share = services_va / total_va,
      admin_share    = admin_va / total_va
    ) %>%
    arrange(SG_UF, year) %>%
    group_by(SG_UF) %>%
    mutate(
      # Taxas de crescimento do PIB per capita
      pib_pc_growth      = (pib_per_capita / lag(pib_per_capita, 1) - 1) * 100,
      pib_pc_growth_lag1 = lag(pib_pc_growth, 1)
    ) %>%
    ungroup()
  
  message(">> PIB UF-ano: ", nrow(pib_state_year), " observações, ",
          length(unique(pib_state_year$SG_UF)), " UFs, ",
          min(pib_state_year$year), "-", max(pib_state_year$year))
  
  ###############################################################################
  # PASSO 5: VARIÁVEL DE ALINHAMENTO SETORIAL
  ###############################################################################
  #
  # sector_alignment = proporção EPT no setor / proporção econômica do setor
  #   = 1: alinhamento perfeito
  #   > 1: EPT sobre-representada em relação à estrutura econômica
  #   < 1: EPT sub-representada em relação à estrutura econômica
  ###############################################################################
  
  message("=== PASSO 5: Alinhamento setorial ===")
  
  # Proporção de matrículas EPT por setor (dentro de cada UF-ano)
  enrollment_shares <- panel_enrollment %>%
    group_by(ANO, SG_UF) %>%
    mutate(
      total_ept_state = sum(QT_MAT_CURSO_TEC, na.rm = TRUE),
      QT_MAT_CURSO_TEC_share = QT_MAT_CURSO_TEC / total_ept_state
    ) %>%
    ungroup()
  
  # PIB setorial em formato longo para merge
  economic_shares_long <- pib_state_year %>%
    select(ANO = year, SG_UF, NM_UF,
           agriculture = agro_share,
           industry = industry_share,
           services = services_share,
           administration = admin_share) %>%
    pivot_longer(cols = c(agriculture, industry, services, administration),
                 names_to = "economic_sector",
                 values_to = "econ_sect_va_prop")
  
  ###############################################################################
  # PASSO 6: MERGE E CRIAÇÃO DO DATASET DO MODELO
  ###############################################################################
  
  message("=== PASSO 6: Merge e construção do dataset ===")
  
  model_df_base <- enrollment_shares %>%
    # Merge com participação econômica setorial
    left_join(economic_shares_long,
              by = c("ANO", "SG_UF", "economic_sector")) %>%
    # Merge com variáveis de PIB
    left_join(pib_state_year %>%
                select(ANO = year, SG_UF, NM_UF,
                       pib_pc_growth, pib_pc_growth_lag1, pib_per_capita),
              by = c("ANO", "SG_UF", "NM_UF")) %>%
    # Criar variável de alinhamento e log de matrículas
    mutate(
      sector_alignment = QT_MAT_CURSO_TEC_share / econ_sect_va_prop,
      log_QT_MAT_CURSO_TEC = log(QT_MAT_CURSO_TEC)
    ) %>%
    arrange(economic_sector, SG_UF, TP_DEPENDENCIA, ANO)
  
  # Filtrar observações sem match geográfico
  model_df_base2 <- model_df_base %>% filter(!is.na(NM_UF))
  
  message(">> Antes do filtro NM_UF: ", nrow(model_df_base),
          " | Após: ", nrow(model_df_base2))
  
  ###############################################################################
  # PASSO 7: VARIÁVEIS DEFASADAS (LAGS)
  ###############################################################################
  
  message("=== PASSO 7: Variáveis defasadas ===")
  
  model_df <- model_df_base2 %>%
    group_by(economic_sector, SG_UF, TP_DEPENDENCIA) %>%
    arrange(ANO) %>%
    mutate(
      log_QT_MAT_CURSO_TEC_lag1 = lag(log_QT_MAT_CURSO_TEC, 1),
      log_QT_MAT_CURSO_TEC_lag2 = lag(log_QT_MAT_CURSO_TEC, 2),
      sector_alignment_lag1      = lag(sector_alignment, 1),
      QT_MAT_CURSO_TEC_share_lag1 = lag(QT_MAT_CURSO_TEC_share, 1),
      econ_sect_va_prop_lag1     = lag(econ_sect_va_prop, 1)
    ) %>%
    ungroup()
  
  ###############################################################################
  # PASSO 8: LIMPEZA FINAL E SELEÇÃO DE VARIÁVEIS
  ###############################################################################
  
  message("=== PASSO 8: Limpeza final ===")
  
  estimation_df <- model_df %>%
    # Remover NAs nas variáveis-chave
    filter(!is.na(log_QT_MAT_CURSO_TEC),
           !is.na(log_QT_MAT_CURSO_TEC_lag1),
           !is.na(log_QT_MAT_CURSO_TEC_lag2),
           !is.na(pib_pc_growth),
           !is.na(pib_pc_growth_lag1),
           !is.na(sector_alignment),
           !is.na(econ_sect_va_prop)) %>%
    # Criar fatores para efeitos fixos
    mutate(
      economic_sector_fe = as.factor(economic_sector),
      state_fe = as.factor(SG_UF),
      dependency_fe = as.factor(TP_DEPENDENCIA),
      year_fe = as.factor(ANO),
      state_dependency_fe = as.factor(paste(SG_UF, TP_DEPENDENCIA, sep = "_"))
    ) %>%
    # Selecionar variáveis finais
    select(
      # Identificadores do painel
      ANO, SG_UF, NM_UF, TP_DEPENDENCIA, economic_sector,
      economic_sector_fe, state_fe, dependency_fe, year_fe, state_dependency_fe,
      # Variável dependente e lags
      log_QT_MAT_CURSO_TEC, log_QT_MAT_CURSO_TEC_lag1,
      log_QT_MAT_CURSO_TEC_lag2, QT_MAT_CURSO_TEC,
      # Variáveis explicativas
      pib_pc_growth, pib_pc_growth_lag1, sector_alignment,
      # Variáveis adicionais
      QT_MAT_CURSO_TEC_share, econ_sect_va_prop, pib_per_capita
    )
  
  message(">> Antes do filtro NA: ", nrow(model_df),
          " | Após: ", nrow(estimation_df),
          " | Retenção: ", sprintf("%.1f%%", 100 * nrow(estimation_df) / nrow(model_df)))
  
  ###############################################################################
  # SALVAR
  ###############################################################################
  
  df_model_ept1a <- estimation_df
  
  dir.create(dirname(LOCAL_SAIDA), recursive = TRUE, showWarnings = FALSE)
  save(df_model_ept1a, file = LOCAL_SAIDA)
  message(">> Salvo: ", LOCAL_SAIDA)
  
  upload_s3_se_diferente(LOCAL_SAIDA, S3_SAIDA, S3_BUCKET)
  
} # Fim do bloco else (reprocessamento)

###############################################################################
# VALIDAÇÃO
###############################################################################

message(">> df_model_ept1a: ", nrow(df_model_ept1a), " observações")
message(">> Anos: ", min(df_model_ept1a$ANO), " a ", max(df_model_ept1a$ANO))
message(">> UFs: ", length(unique(df_model_ept1a$SG_UF)))
message(">> Setores: ", paste(unique(df_model_ept1a$economic_sector), collapse = ", "))
message(">> Dependências: ", paste(unique(df_model_ept1a$TP_DEPENDENCIA), collapse = ", "))
message("=== modelo_ept_01a.R concluído ===")