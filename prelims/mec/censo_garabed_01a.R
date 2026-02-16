###############################################################################
# censo_garabed_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Matrículas Censo Escolar (Ensino Médio / Técnico) e Meta 11a
#
# USO NAS ABAS: A2 (EPT e População), C1 (Projeção EPT), C2 (Planificação)
#
# OBJETIVO:
#   Processar dados de matrículas do Censo Escolar por modalidade de ensino
#   médio e técnico, calcular três definições da Meta 11a do PNE (proporção
#   de matrículas técnicas sobre ensino médio), e gerar agregado Brasil.
#
# DADOS BRUTOS (S3):
#   rawdata/mec_inep/Garabed_MTjuly25.xlsx
#   Dados de matrículas do Censo Escolar por curso, forma de oferta e
#   dependência administrativa. Extraídos do site do INEP quando os
#   microdados com detalhamento por curso estavam publicamente disponíveis.
#   Arquivo preparado pelo Prof. Garabed Kenchian, integrante da equipe
#   FGV-DGPE.
#
# INSUMOS PROCESSADOS (S3):
#   working/ibge/df_codes_ibge.rda (gerado por codes_ibge_01a.R)
#   working/mec_inep/df_censo_UF.rda (gerado por matriculas1UF.R)
#
# DADOS PROCESSADOS (S3 e local):
#   working/mec_inep/meta11a_opcoes.rda       (saída principal)
#   working/mec_inep/E_Medio_Garabed1.rda     (intermediário)
#   working/mec_inep/Tecnico_FormaR_Garabed1.rda (intermediário)
#   working/mec_inep/Tecnico_Forma_Cursos_Garabed1.rda (intermediário)
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
# DEPENDÊNCIAS: dplyr, tidyr, aws.s3, dotenv, openxlsx
# SAÍDA: meta11a_opcoes.rda
#   Carregado diretamente por BM_FGV_Propag2.R (sem produtos intermediários).
#   Aba A2: combinado com pop01_70b para gerar ept_combined_data
#           (população 15-19 vs matrículas EPT/EM, metas PNE Meta 11).
#   Abas C1–C2: projeções e planificação de oferta EPT por UF.
###############################################################################

library(dplyr)
library(tidyr)
library(aws.s3)
library(dotenv)
library(openxlsx)

options(scipen = 999)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

# Carregar credenciais AWS do arquivo .env na raiz do projeto
dotenv::load_dot_env()

# Bucket S3 do projeto
S3_BUCKET <- "techbrazildata"

# Caminhos S3 — dados brutos
S3_GARABED_XLSX <- "rawdata/mec_inep/Garabed_MTjuly25.xlsx"

# Caminhos S3 — insumos processados (gerados por outros scripts)
S3_CODES_IBGE  <- "working/ibge/df_codes_ibge.rda"
S3_CENSO_UF    <- "working/mec_inep/df_censo_UF.rda"

# Caminhos S3 — saídas deste script
S3_META11A           <- "working/mec_inep/meta11a_opcoes.rda"
S3_E_MEDIO           <- "working/mec_inep/E_Medio_Garabed1.rda"
S3_TECNICO_FORMA     <- "working/mec_inep/Tecnico_FormaR_Garabed1.rda"
S3_TECNICO_CURSOS    <- "working/mec_inep/Tecnico_Forma_Cursos_Garabed1.rda"

# Caminhos locais (relativos à raiz do projeto)
LOCAL_GARABED_XLSX   <- "rawdata/mec_inep/Garabed_MTjuly25.xlsx"
LOCAL_CODES_IBGE     <- "working/ibge/df_codes_ibge.rda"
LOCAL_CENSO_UF       <- "working/mec_inep/df_censo_UF.rda"
LOCAL_META11A        <- "working/mec_inep/meta11a_opcoes.rda"
LOCAL_E_MEDIO        <- "working/mec_inep/E_Medio_Garabed1.rda"
LOCAL_TECNICO_FORMA  <- "working/mec_inep/Tecnico_FormaR_Garabed1.rda"
LOCAL_TECNICO_CURSOS <- "working/mec_inep/Tecnico_Forma_Cursos_Garabed1.rda"

###############################################################################
# FUNÇÕES DE SINCRONIZAÇÃO S3
###############################################################################

# Obter data de última modificação de um objeto no S3
s3_ultima_modificacao <- function(s3_key, bucket) {
  tryCatch({
    info <- head_object(object = s3_key, bucket = bucket)
    as.POSIXct(attr(info, "last-modified"), format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  }, error = function(e) {
    NULL
  })
}

# Sincronizar arquivo do S3 para local
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
# PASSO 1: SINCRONIZAR SAÍDA PRINCIPAL
###############################################################################

processado_atualizado <- FALSE

if (file.exists(LOCAL_META11A)) {
  data_local <- file.mtime(LOCAL_META11A)
  data_s3 <- s3_ultima_modificacao(S3_META11A, S3_BUCKET)
  
  if (!is.null(data_s3) && data_s3 > data_local) {
    message("=== meta11a_opcoes no S3 mais recente — baixando ===")
    sincronizar_s3(LOCAL_META11A, S3_META11A, S3_BUCKET)
    processado_atualizado <- TRUE
  } else {
    message("=== meta11a_opcoes local está atualizado ===")
    processado_atualizado <- TRUE
  }
} else {
  data_s3 <- s3_ultima_modificacao(S3_META11A, S3_BUCKET)
  if (!is.null(data_s3)) {
    message("=== meta11a_opcoes encontrado no S3 — baixando ===")
    sincronizar_s3(LOCAL_META11A, S3_META11A, S3_BUCKET)
    processado_atualizado <- TRUE
  }
}

###############################################################################
# PASSO 2: REPROCESSAR SE NECESSÁRIO
###############################################################################

if (!processado_atualizado) {
  
  message("=== Processamento necessário — sincronizando insumos ===")
  
  # Sincronizar todos os insumos
  sincronizar_s3(LOCAL_GARABED_XLSX, S3_GARABED_XLSX, S3_BUCKET)
  sincronizar_s3(LOCAL_CODES_IBGE,   S3_CODES_IBGE,   S3_BUCKET)
  sincronizar_s3(LOCAL_CENSO_UF,     S3_CENSO_UF,     S3_BUCKET)
  
  # Carregar insumos
  load(LOCAL_CODES_IBGE)   # → df_codes_ibge
  load(LOCAL_CENSO_UF)     # → df_censo_UF
  
  # =========================================================================
  # PARTE A: PROCESSAR PLANILHA ENSINO MÉDIO (E_Medio_Garabed1)
  # =========================================================================
  
  message(">> Lendo planilha E_Medio_Prop_Prof...")
  E_Medio_main <- openxlsx::read.xlsx(LOCAL_GARABED_XLSX,
                                      sheet = "E_Medio_Prop_Prof") %>%
    arrange(Ano, UF)
  
  # Padronizar nomes de colunas
  E_Medio_mainR <- E_Medio_main %>%
    rename(ANO = Ano, SG_UF = UF, CO_UF = Ufcod)
  
  # Classificar modalidades de matrícula
  E_Medio_mainR <- E_Medio_mainR %>%
    mutate(tipo_matricula = case_when(
      `Ensino.Médio` == "Profissional"       ~ "QT_MAT_PROF",
      `Ensino.Médio` == "Propedeutico"       ~ "QT_MAT_MED_PROP",
      `Ensino.Médio` == "Normal/Magistério"  ~ "QT_MAT_MED_NM",
      `Ensino.Médio` == "Integrado Técnico"  ~ "QT_MAT_MED_CT",
      `Ensino.Médio` == "Integrado FIC"      ~ "QT_MAT_EJA_MED_FIC",
      TRUE ~ NA_character_
    ))
  
  # Pivotar para formato largo
  # Nota: QT_MAT_PROF = QT_MAT_MED_CT + QT_MAT_MED_NM + QT_MAT_EJA_MED_FIC
  E_Medio_mainR <- E_Medio_mainR %>%
    select(ANO, SG_UF, CO_UF, tipo_matricula, Mat) %>%
    pivot_wider(
      names_from  = tipo_matricula,
      values_from = Mat,
      values_fill = 0
    )
  
  E_Medio_mainR$AGREG <- "UF_TUDO_GARABED"
  
  # Adicionar nome da UF via geocódigos
  df_ <- df_codes_ibge %>% select(CO_UF, NM_UF) %>% unique()
  
  E_Medio_Garabed1 <- E_Medio_mainR %>%
    left_join(df_, by = "CO_UF") %>%
    relocate(NM_UF, .before = SG_UF)
  
  # Salvar intermediário
  dir.create(dirname(LOCAL_E_MEDIO), recursive = TRUE, showWarnings = FALSE)
  save(E_Medio_Garabed1, file = LOCAL_E_MEDIO)
  message(">> Salvo: ", LOCAL_E_MEDIO)
  
  # =========================================================================
  # PARTE B: PROCESSAR PLANILHA TÉCNICO POR FORMA DE OFERTA
  # =========================================================================
  
  message(">> Lendo planilha Tecnico_FormaOferta_DA...")
  Tecnico_Forma <- openxlsx::read.xlsx(LOCAL_GARABED_XLSX,
                                       sheet = "Tecnico_FormaOferta_DA") %>%
    arrange(Ano, UF)
  
  # Renomear e classificar tipos de matrícula
  Tecnico_FormaR <- Tecnico_Forma %>%
    rename(
      ANO            = Ano,
      SG_UF          = UF,
      TP_DEPENDENCIA = `Dependência.Administrativa`,
      NOME_CURSO     = `Nome.do.curso`
    ) %>%
    mutate(
      NOME_CURSO = if_else(is.na(NOME_CURSO), "Curso_desconocido", NOME_CURSO),
      
      # Variáveis padronizadas pelo INEP
      tipo_matricula = case_when(
        `Forma.de.Oferta` == "Técnico Integrado"     ~ "QT_MAT_CURSO_TEC_CT",
        `Forma.de.Oferta` == "Técnico Concomitante"   ~ "QT_MAT_CURSO_TEC_CONC",
        `Forma.de.Oferta` == "Técnico Subsequente"    ~ "QT_MAT_CURSO_TEC_SUBS",
        `Forma.de.Oferta` == "Técnico Integrado EJA"  ~ "QT_MAT_CURSO_TEC_EJA",
        `Forma.de.Oferta` == "Normal/Magistério"      ~ "QT_MAT_CURSO_TEC_NM",
        TRUE ~ NA_character_
      )
    )
  
  # Agregações por UF e dependência administrativa
  Tecnico_FormaR_Garabed1 <- Tecnico_FormaR %>%
    group_by(ANO, SG_UF, TP_DEPENDENCIA) %>%
    summarise(
      QT_MAT_PROF          = sum(Matrículas, na.rm = TRUE),
      QT_MAT_PROF_TEC      = sum(if_else(!`Forma.de.Oferta` %in% "Normal/Magistério",
                                         Matrículas, 0), na.rm = TRUE),
      QT_MAT_PROF_TEC_MED  = sum(if_else(`Forma.de.Oferta` %in%
                                           c("Técnico Concomitante", "Técnico Integrado"),
                                         Matrículas, 0), na.rm = TRUE),
      QT_MAT_CURSO_TEC_CT   = sum(if_else(tipo_matricula == "QT_MAT_CURSO_TEC_CT",
                                          Matrículas, 0), na.rm = TRUE),
      QT_MAT_CURSO_TEC_CONC = sum(if_else(tipo_matricula == "QT_MAT_CURSO_TEC_CONC",
                                          Matrículas, 0), na.rm = TRUE),
      QT_MAT_CURSO_TEC_SUBS = sum(if_else(tipo_matricula == "QT_MAT_CURSO_TEC_SUBS",
                                          Matrículas, 0), na.rm = TRUE),
      QT_MAT_CURSO_TEC_EJA  = sum(if_else(tipo_matricula == "QT_MAT_CURSO_TEC_EJA",
                                          Matrículas, 0), na.rm = TRUE),
      QT_MAT_CURSO_TEC_NM   = sum(if_else(tipo_matricula == "QT_MAT_CURSO_TEC_NM",
                                          Matrículas, 0), na.rm = TRUE),
      QT_MAT_CURSO_TEC      = QT_MAT_CURSO_TEC_CT + QT_MAT_CURSO_TEC_CONC +
        QT_MAT_CURSO_TEC_SUBS + QT_MAT_CURSO_TEC_EJA +
        QT_MAT_CURSO_TEC_NM,
      .groups = "drop"
    )
  
  save(Tecnico_FormaR_Garabed1, file = LOCAL_TECNICO_FORMA)
  message(">> Salvo: ", LOCAL_TECNICO_FORMA)
  
  # =========================================================================
  # PARTE B2: AGREGAÇÃO POR CURSO
  # =========================================================================
  
  Tecnico_Forma_Cursos_Garabed1 <- Tecnico_Forma %>%
    rename(
      ANO            = Ano,
      SG_UF          = UF,
      TP_DEPENDENCIA = `Dependência.Administrativa`,
      NOME_CURSO     = `Nome.do.curso`
    ) %>%
    mutate(NOME_CURSO = if_else(is.na(NOME_CURSO), "Curso_desconocido", NOME_CURSO)) %>%
    group_by(ANO, SG_UF, TP_DEPENDENCIA, NOME_CURSO) %>%
    summarise(QT_MAT_CURSO_TEC = sum(Matrículas, na.rm = TRUE), .groups = "drop")
  
  # Validação: totais por curso devem bater com agregação por forma de oferta
  total_cursos_check <- Tecnico_Forma_Cursos_Garabed1 %>%
    group_by(ANO, SG_UF, TP_DEPENDENCIA) %>%
    summarise(SOMA_CURSOS = sum(QT_MAT_CURSO_TEC, na.rm = TRUE), .groups = "drop")
  
  comparacao <- Tecnico_FormaR_Garabed1 %>%
    left_join(total_cursos_check, by = c("ANO", "SG_UF", "TP_DEPENDENCIA")) %>%
    mutate(DIF = QT_MAT_CURSO_TEC - SOMA_CURSOS)
  
  n_dif <- sum(abs(comparacao$DIF) > 0, na.rm = TRUE)
  if (n_dif > 0) {
    message("   AVISO: ", n_dif, " divergências encontradas na validação por curso")
  } else {
    message("   Validação por curso: OK (zero divergências)")
  }
  
  save(Tecnico_Forma_Cursos_Garabed1, file = LOCAL_TECNICO_CURSOS)
  message(">> Salvo: ", LOCAL_TECNICO_CURSOS)
  
  # =========================================================================
  # PARTE C: CALCULAR META 11a (TRÊS DEFINIÇÕES)
  # =========================================================================
  
  message(">> Calculando Meta 11a...")
  
  # -------------------------------------------------------------------------
  # Definição 1: Numerador = QT_MAT_PROF_TEC_PROPAG (do df_censo_UF)
  #              Denominador = QT_MAT_MED
  # -------------------------------------------------------------------------
  meta11a_opcao1 <- df_censo_UF %>%
    filter(AGREG == "UF_TUDO") %>%
    select(ANO, NM_UF, SG_UF, QT_MAT_PROF_TEC_PROPAG, QT_MAT_MED) %>%
    mutate(
      Meta11a_opcao1 = ifelse(QT_MAT_MED > 0,
                              QT_MAT_PROF_TEC_PROPAG / QT_MAT_MED, NA)
    )
  
  # -------------------------------------------------------------------------
  # Definição 2: Numerador = QT_MAT_CURSO_TEC_CT + QT_MAT_CURSO_TEC_CONC
  #              (do Tecnico_FormaR_Garabed1)
  #              Denominador = QT_MAT_MED
  # -------------------------------------------------------------------------
  Tecnico_FormaR_Garabed1_sum <- Tecnico_FormaR_Garabed1 %>%
    group_by(ANO, SG_UF) %>%
    summarise(
      QT_MAT_CURSO_TEC_CT   = sum(QT_MAT_CURSO_TEC_CT,   na.rm = TRUE),
      QT_MAT_CURSO_TEC_CONC = sum(QT_MAT_CURSO_TEC_CONC, na.rm = TRUE),
      .groups = "drop"
    )
  
  meta11a_opcao2 <- df_censo_UF %>%
    filter(AGREG == "UF_TUDO") %>%
    select(ANO, NM_UF, SG_UF, QT_MAT_MED) %>%
    left_join(Tecnico_FormaR_Garabed1_sum, by = c("ANO", "SG_UF")) %>%
    mutate(
      QT_MAT_TEC_NUM2 = coalesce(QT_MAT_CURSO_TEC_CT, 0) +
        coalesce(QT_MAT_CURSO_TEC_CONC, 0),
      Meta11a_opcao2  = ifelse(QT_MAT_MED > 0, QT_MAT_TEC_NUM2 / QT_MAT_MED, NA)
    )
  
  # -------------------------------------------------------------------------
  # Definição 3: Numerador = QT_MAT_CURSO_TEC_CT apenas
  #              Denominador = QT_MAT_MED
  # -------------------------------------------------------------------------
  meta11a_opcao3 <- df_censo_UF %>%
    filter(AGREG == "UF_TUDO") %>%
    select(ANO, NM_UF, SG_UF, QT_MAT_MED) %>%
    left_join(Tecnico_FormaR_Garabed1_sum, by = c("ANO", "SG_UF")) %>%
    select(ANO, NM_UF, SG_UF, QT_MAT_MED, QT_MAT_CURSO_TEC_CT) %>%
    mutate(
      QT_MAT_TEC_NUM3 = ifelse(is.na(QT_MAT_CURSO_TEC_CT), 0, QT_MAT_CURSO_TEC_CT),
      Meta11a_opcao3  = ifelse(QT_MAT_MED > 0, QT_MAT_TEC_NUM3 / QT_MAT_MED, NA)
    )
  
  # =========================================================================
  # PARTE D: COMBINAR AS TRÊS DEFINIÇÕES E GERAR AGREGADO BRASIL
  # =========================================================================
  
  message(">> Combinando definições e gerando agregado Brasil...")
  
  # Combinar as três opções em um único dataframe
  meta11a_opcoes <- meta11a_opcao1 %>%
    left_join(meta11a_opcao2, by = c("ANO", "SG_UF", "NM_UF"),
              suffix = c("", "_DROP")) %>%
    left_join(meta11a_opcao3, by = c("ANO", "SG_UF", "NM_UF"),
              suffix = c("", "_DROP2")) %>%
    select(-ends_with("_DROP"), -ends_with("_DROP2"))
  
  # Agregado Brasil
  meta11a_brasil <- meta11a_opcoes %>%
    group_by(ANO) %>%
    summarise(
      QT_MAT_MED             = sum(coalesce(QT_MAT_MED, 0)),
      QT_MAT_PROF_TEC_PROPAG = sum(coalesce(QT_MAT_PROF_TEC_PROPAG, 0)),
      QT_MAT_CURSO_TEC_CT    = sum(coalesce(QT_MAT_CURSO_TEC_CT, 0)),
      QT_MAT_CURSO_TEC_CONC  = sum(coalesce(QT_MAT_CURSO_TEC_CONC, 0)),
      .groups = "drop"
    ) %>%
    mutate(
      QT_MAT_TEC_NUM2 = QT_MAT_CURSO_TEC_CT + QT_MAT_CURSO_TEC_CONC,
      QT_MAT_TEC_NUM3 = QT_MAT_CURSO_TEC_CT,
      Meta11a_opcao1  = QT_MAT_PROF_TEC_PROPAG / QT_MAT_MED,
      Meta11a_opcao2  = QT_MAT_TEC_NUM2 / QT_MAT_MED,
      Meta11a_opcao3  = QT_MAT_TEC_NUM3 / QT_MAT_MED,
      NM_UF  = "Brasil",
      SG_UF  = "BR"
    ) %>%
    relocate(NM_UF, SG_UF)
  
  # Append Brasil ao dataset
  meta11a_opcoes <- bind_rows(meta11a_opcoes, meta11a_brasil)
  
  # =========================================================================
  # SALVAR E UPLOAD CONDICIONAL
  # =========================================================================
  
  dir.create(dirname(LOCAL_META11A), recursive = TRUE, showWarnings = FALSE)
  save(meta11a_opcoes, file = LOCAL_META11A)
  message(">> Salvo localmente: ", LOCAL_META11A)
  
  # Upload condicional de todas as saídas
  upload_s3_se_diferente(LOCAL_META11A,        S3_META11A,        S3_BUCKET)
  upload_s3_se_diferente(LOCAL_E_MEDIO,        S3_E_MEDIO,        S3_BUCKET)
  upload_s3_se_diferente(LOCAL_TECNICO_FORMA,  S3_TECNICO_FORMA,  S3_BUCKET)
  upload_s3_se_diferente(LOCAL_TECNICO_CURSOS, S3_TECNICO_CURSOS, S3_BUCKET)
  
} else {
  # Arquivo processado já está atualizado — apenas carregar
  message("=== Carregando meta11a_opcoes existente ===")
  load(LOCAL_META11A)
}

###############################################################################
# VALIDAÇÃO
###############################################################################
message(">> meta11a_opcoes: ", nrow(meta11a_opcoes), " linhas, ",
        length(unique(meta11a_opcoes$SG_UF)), " UFs (incl. Brasil), ",
        min(meta11a_opcoes$ANO), "-", max(meta11a_opcoes$ANO))
message("=== censo_garabed_01a.R concluído ===")