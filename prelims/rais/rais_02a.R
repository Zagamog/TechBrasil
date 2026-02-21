###############################################################################
# rais_02a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Agregação RAIS por CBO × UF com Hierarquia CBO
#
# USO NAS ABAS: D2 (APLs via rais_apo_*.R), D3 (Informalidade), E1 (Oferta-Demanda)
#
# OBJETIVO:
#   Agregar os microdados RAIS (de rais_01a.R) em dois níveis de CBO:
#     - CBO 6 dígitos × UF → rais_cbo6_uf{YY}.rda
#     - CBO 4 dígitos × UF → rais_cbo4_uf{YY}.rda
#   Ambos enriquecidos com a hierarquia completa da CBO 2002.
#
#   ESTRATÉGIA DE ENRIQUECIMENTO CBO:
#     O qbq_ocup_cmento1 (de qbq_03a.R) cobre 1.899 ocupações QBQ,
#     mas a RAIS contém ~2.700+ códigos CBO distintos. Para cobertura
#     completa, construímos a hierarquia CBO diretamente dos CSVs
#     oficiais (mesma fonte usada em qbq_03a.R) e juntamos com os
#     dados RAIS. CBOs presentes no QBQ recebem adicionalmente os
#     metadados de nome da ocupação (Ocupação) do qbq_ocup_cmento1.
#
#   O script processa ambos os anos em um loop, sem duplicação de código.
#   A geografia (SG_UF, CO_UF, NM_UF) já vem de rais_01a.R.
#
# INSUMOS:
#   LOCAL (de rais_01a.R — arquivos grandes, não no S3):
#     working/rais/2023/rais2023.rda
#     working/rais/2024/rais2024.rda
#
#   S3 (de qbq_03a.R):
#     working/qbq/qbq_ocup_cmento1.rda
#
#   S3 (CSVs estáticos do MTE):
#     rawdata/cbo/CBO2002 - Grande Grupo.csv
#     rawdata/cbo/CBO2002 - SubGrupo Principal.csv
#     rawdata/cbo/CBO2002 - SubGrupo.csv
#     rawdata/cbo/CBO2002 - Familia.csv
#     rawdata/cbo/CBO2002 - Ocupacao.csv
#
# SAÍDAS (S3 e local):
#   working/rais/rais_cbo6_uf23.rda  — CBO 6 dígitos × UF, 2023
#   working/rais/rais_cbo6_uf24.rda  — CBO 6 dígitos × UF, 2024
#   working/rais/rais_cbo4_uf23.rda  — CBO 4 dígitos × UF, 2023
#   working/rais/rais_cbo4_uf24.rda  — CBO 4 dígitos × UF, 2024
#
# VARIÁVEIS NAS SAÍDAS:
#   rais_cbo6_uf{YY}:
#     CodCBO, vinculos, CO_UF, SG_UF, NM_UF,
#     cbo_1dig, cbo_gragru, cbo_2dig, cbo_prigru,
#     cbo_4dig, cbo_familia, Ocupação
#
#   rais_cbo4_uf{YY}:
#     vinculos, CO_UF, SG_UF, NM_UF,
#     cbo_1dig, cbo_gragru, cbo_2dig, cbo_prigru,
#     cbo_4dig, cbo_familia
#
# DEPENDÊNCIAS: dplyr, readr, janitor, stringr, aws.s3, dotenv
# NOTA: Requer rais_01a.R executado localmente antes (arquivos grandes)
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

# NOTA: O diretório de trabalho deve ser a raiz do projeto
#   setwd("D:/Country/Brazil/TechBrazil")  # ou equivalente
# Todos os caminhos abaixo são relativos a esta raiz.

# Anos a processar
ANOS <- c(2023, 2024)

# Sufixos curtos para nomes de arquivo (23, 24)
SUFIXOS <- c("23", "24")

# Insumos S3
S3_QBQ_CMENTO <- "working/qbq/qbq_ocup_cmento1.rda"
S3_CBO_BASE   <- "rawdata/cbo/"
CBO_CSVS <- c(
  "CBO2002 - Grande Grupo.csv",
  "CBO2002 - SubGrupo Principal.csv",
  "CBO2002 - SubGrupo.csv",
  "CBO2002 - Familia.csv",
  "CBO2002 - Ocupacao.csv"
)

###############################################################################
# FUNÇÕES DE SINCRONIZAÇÃO S3
###############################################################################

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
  identico <- tryCatch(
    conteudo_identico_s3(caminho_local, s3_key, bucket),
    error = function(e) FALSE
  )
  if (isTRUE(identico)) {
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
# PASSO 0: CONSTRUIR HIERARQUIA CBO 2002
###############################################################################
#
# Construímos a hierarquia completa (~2.719 ocupações) diretamente dos CSVs
# oficiais do MTE. Isto dá cobertura total para qualquer CBO que apareça
# na RAIS, sem depender dos 1.899 do QBQ.
#
# Inclui as mesmas correções manuais documentadas em qbq_03a.R:
#   - Grande Grupo 8: título separado do GG 7
#   - Ocupação duplicada: "Operador de pá carregadeira" desambiguada
###############################################################################

message("=== PASSO 0: Hierarquia CBO 2002 ===")

# Sincronizar CSVs do S3
dir.create("rawdata/cbo", recursive = TRUE, showWarnings = FALSE)
for (csv_nome in CBO_CSVS) {
  sincronizar_s3(file.path("rawdata/cbo", csv_nome),
                 paste0(S3_CBO_BASE, csv_nome), S3_BUCKET)
}

cbo_base <- "rawdata/cbo/"

# Grande Grupo (1 dígito)
df_gg <- read_delim(paste0(cbo_base, "CBO2002 - Grande Grupo.csv"),
                    delim = ";", locale = locale(encoding = "LATIN1"),
                    show_col_types = FALSE) %>%
  clean_names() %>%
  rename(cbo_1dig = codigo, cbo_gragru = titulo) %>%
  mutate(cbo_1dig = str_pad(as.character(cbo_1dig), 1, pad = "0"))

# CORREÇÃO MANUAL: GG 8 (processos contínuos) — ver qbq_03a.R
df_gg$cbo_gragru[df_gg$cbo_1dig == "8"] <-
  "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS CONTINUOS"

# Subgrupo Principal (2 dígitos)
df_sgp <- read_delim(paste0(cbo_base, "CBO2002 - SubGrupo Principal.csv"),
                     delim = ";", locale = locale(encoding = "LATIN1"),
                     show_col_types = FALSE) %>%
  clean_names() %>%
  rename(cbo_2dig = codigo, cbo_prigru = titulo) %>%
  mutate(cbo_2dig = str_pad(as.character(cbo_2dig), 2, pad = "0"))

# Subgrupo (3 dígitos)
df_sg <- read_delim(paste0(cbo_base, "CBO2002 - SubGrupo.csv"),
                    delim = ";", locale = locale(encoding = "LATIN1"),
                    show_col_types = FALSE) %>%
  clean_names() %>%
  rename(cbo_3dig = codigo, cbo_subgru = titulo) %>%
  mutate(cbo_3dig = str_pad(as.character(cbo_3dig), 3, pad = "0"))

# Família Ocupacional (4 dígitos)
df_fam <- read_delim(paste0(cbo_base, "CBO2002 - Familia.csv"),
                     delim = ";", locale = locale(encoding = "LATIN1"),
                     show_col_types = FALSE) %>%
  clean_names() %>%
  rename(cbo_4dig = codigo, cbo_familia = titulo) %>%
  mutate(cbo_4dig = str_pad(as.character(cbo_4dig), 4, pad = "0"))

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

# CORREÇÃO MANUAL: duplicata — ver qbq_03a.R
df_ocup$cbo_nome[df_ocup$cbo_nome == "Operador de pá carregadeira" &
                   df_ocup$cbo_3dig == "715"] <- "Operador de pá carregadeira715"

# Hierarquia completa
df_cbo_hier <- df_ocup %>%
  left_join(df_fam, by = "cbo_4dig") %>%
  left_join(df_sg,  by = "cbo_3dig") %>%
  left_join(df_sgp, by = "cbo_2dig") %>%
  left_join(df_gg,  by = "cbo_1dig") %>%
  relocate(cbo_6dig, cbo_nome, cbo_4dig, cbo_familia, cbo_3dig, cbo_subgru,
           cbo_2dig, cbo_prigru, cbo_1dig, cbo_gragru)

message(">> Hierarquia CBO: ", nrow(df_cbo_hier), " ocupações (6 dígitos)")
message(">> Famílias (4 dígitos): ", n_distinct(df_cbo_hier$cbo_4dig))

# Lookup para 6 dígitos: hierarquia completa
lookup_cbo6 <- df_cbo_hier %>%
  select(cbo_6dig, cbo_nome, cbo_4dig, cbo_familia,
         cbo_3dig, cbo_subgru, cbo_2dig, cbo_prigru,
         cbo_1dig, cbo_gragru) %>%
  distinct()

# Lookup para 4 dígitos: hierarquia sem ocupação individual
lookup_cbo4 <- df_cbo_hier %>%
  select(cbo_4dig, cbo_familia, cbo_3dig, cbo_subgru,
         cbo_2dig, cbo_prigru, cbo_1dig, cbo_gragru) %>%
  distinct()

###############################################################################
# PASSO 0B: CARREGAR NOMES DE OCUPAÇÃO DO QBQ (OPCIONAL)
###############################################################################
#
# qbq_ocup_cmento1 tem o campo "Ocupação" para os 1.899 CBOs do QBQ.
# Usamos como enriquecimento adicional para rais_cbo6 — CBOs fora do QBQ
# ficam com Ocupação = cbo_nome (da hierarquia CBO oficial).
###############################################################################

message("=== PASSO 0B: Nomes de ocupação QBQ ===")

local_cmento <- "working/qbq/qbq_ocup_cmento1.rda"
sincronizar_s3(local_cmento, S3_QBQ_CMENTO, S3_BUCKET)
load(local_cmento)  # → qbq_ocup_cmento1

lookup_ocup_qbq <- qbq_ocup_cmento1 %>%
  select(CodCBO, Ocupação) %>%
  distinct()

message(">> Nomes QBQ disponíveis para ", nrow(lookup_ocup_qbq), " CBOs")

###############################################################################
# PROCESSAMENTO POR ANO
###############################################################################

for (i in seq_along(ANOS)) {
  
  ano <- ANOS[i]
  suf <- SUFIXOS[i]
  
  message("\n", strrep("=", 60))
  message("=== RAIS ", ano, " — Agregação CBO × UF ===")
  message(strrep("=", 60))
  
  # Nomes dos arquivos de saída
  nome_cbo6 <- paste0("rais_cbo6_uf", suf)
  nome_cbo4 <- paste0("rais_cbo4_uf", suf)
  local_cbo6 <- file.path("working/rais", paste0(nome_cbo6, ".rda"))
  local_cbo4 <- file.path("working/rais", paste0(nome_cbo4, ".rda"))
  s3_cbo6 <- paste0("working/rais/", nome_cbo6, ".rda")
  s3_cbo4 <- paste0("working/rais/", nome_cbo4, ".rda")
  
  # Verificar se ambas as saídas já existem
  if (rda_valido(local_cbo6) && rda_valido(local_cbo4)) {
    message(">> Saídas já existem para ", ano, " — verificando S3")
    
    # Garantir upload ao S3
    upload_s3_se_diferente(local_cbo6, s3_cbo6, S3_BUCKET)
    upload_s3_se_diferente(local_cbo4, s3_cbo4, S3_BUCKET)
    
    # Carregar para validação
    load(local_cbo6)
    load(local_cbo4)
    
  } else {
    
    ###########################################################################
    # CARREGAR RAIS DO ANO
    ###########################################################################
    
    rais_arquivo <- file.path("working/rais",
                              as.character(ano), paste0("rais", ano, ".rda"))
    
    if (!file.exists(rais_arquivo)) {
      message(">> ERRO: Arquivo não encontrado: ", rais_arquivo)
      message(">> Execute rais_01a.R primeiro")
      next
    }
    
    message(">> Carregando: ", rais_arquivo)
    load(rais_arquivo)  # → rais{ano}
    rais_raw <- get(paste0("rais", ano))
    rm(list = paste0("rais", ano))
    
    message(">> Vínculos carregados: ", format(nrow(rais_raw), big.mark = "."))
    
    # Filtrar vínculos sem geografia válida (CO_MUN6 que não casou com IBGE)
    n_sem_geo <- sum(is.na(rais_raw$SG_UF))
    if (n_sem_geo > 0) {
      rais_raw <- rais_raw[!is.na(rais_raw$SG_UF), ]
      message(">> Removidos ", format(n_sem_geo, big.mark = "."),
              " vínculos sem UF (CO_MUN6 não encontrado no IBGE)")
    }
    
    ###########################################################################
    # PASSO 1: AGREGAÇÃO CBO 6 DÍGITOS × UF
    ###########################################################################
    #
    # Agregar vínculos ativos por CBO completo (6 dígitos) e UF.
    # A coluna vinculos contém 1=ativo, 0=inativo. Somamos para
    # obter o total de vínculos ativos por CBO × UF.
    ###########################################################################
    
    message("=== PASSO 1: CBO 6 dígitos × UF ===")
    
    rais_cbo6 <- rais_raw %>%
      filter(!is.na(CodCBO) & CodCBO != "") %>%
      mutate(CodCBO = as.character(CodCBO)) %>%
      group_by(CodCBO, CO_UF, SG_UF, NM_UF) %>%
      summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
      arrange(CO_UF, desc(vinculos))
    
    message(">> Agregação bruta: ", format(nrow(rais_cbo6), big.mark = "."),
            " combinações CBO6 × UF")
    message(">> Total vínculos: ", format(sum(rais_cbo6$vinculos), big.mark = "."))
    
    # Juntar hierarquia CBO (cobertura completa via CSVs oficiais)
    rais_cbo6 <- rais_cbo6 %>%
      left_join(lookup_cbo6, by = c("CodCBO" = "cbo_6dig")) %>%
      # Adicionar nome QBQ quando disponível, senão usar cbo_nome
      left_join(lookup_ocup_qbq, by = "CodCBO") %>%
      mutate(Ocupação = coalesce(Ocupação, cbo_nome)) %>%
      select(CodCBO, vinculos, CO_UF, SG_UF, NM_UF,
             cbo_1dig, cbo_gragru, cbo_2dig, cbo_prigru,
             cbo_4dig, cbo_familia, Ocupação)
    
    # Diagnóstico de cobertura
    n_sem_hier <- sum(is.na(rais_cbo6$cbo_gragru))
    n_sem_nome <- sum(is.na(rais_cbo6$Ocupação))
    message(">> Sem hierarquia CBO: ", n_sem_hier, " linhas (",
            n_distinct(rais_cbo6$CodCBO[is.na(rais_cbo6$cbo_gragru)]),
            " CBOs distintos)")
    message(">> Sem nome de ocupação: ", n_sem_nome, " linhas")
    
    # Preencher cbo_1dig e cbo_2dig a partir do CodCBO quando possível
    rais_cbo6 <- rais_cbo6 %>%
      mutate(
        cbo_1dig = if_else(is.na(cbo_1dig), substr(CodCBO, 1, 1), cbo_1dig),
        cbo_2dig = if_else(is.na(cbo_2dig), substr(CodCBO, 1, 2), cbo_2dig),
        cbo_4dig = if_else(is.na(cbo_4dig), substr(CodCBO, 1, 4), cbo_4dig)
      )
    
    # Salvar
    assign(nome_cbo6, rais_cbo6)
    save(list = nome_cbo6, file = local_cbo6)
    message(">> Salvo: ", local_cbo6)
    upload_s3_se_diferente(local_cbo6, s3_cbo6, S3_BUCKET)
    
    ###########################################################################
    # PASSO 2: AGREGAÇÃO CBO 4 DÍGITOS × UF
    ###########################################################################
    #
    # Agregar ao nível de família ocupacional (4 dígitos) × UF.
    # Usado por rais_apo_*.R para cálculo de APLs (Location Quotients)
    # e pelo Shiny para tabelas de emprego por família ocupacional.
    ###########################################################################
    
    message("=== PASSO 2: CBO 4 dígitos × UF ===")
    
    rais_cbo4 <- rais_raw %>%
      filter(!is.na(CodCBO) & CodCBO != "") %>%
      mutate(cbo_4dig = substr(as.character(CodCBO), 1, 4)) %>%
      group_by(cbo_4dig, CO_UF, SG_UF, NM_UF) %>%
      summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
      arrange(CO_UF, desc(vinculos))
    
    message(">> Agregação bruta: ", format(nrow(rais_cbo4), big.mark = "."),
            " combinações CBO4 × UF")
    message(">> Total vínculos: ", format(sum(rais_cbo4$vinculos), big.mark = "."))
    
    # Juntar hierarquia CBO 4 dígitos
    rais_cbo4 <- rais_cbo4 %>%
      left_join(lookup_cbo4, by = "cbo_4dig") %>%
      select(vinculos, CO_UF, SG_UF, NM_UF,
             cbo_1dig, cbo_gragru, cbo_2dig, cbo_prigru,
             cbo_4dig, cbo_familia)
    
    # Preencher níveis superiores quando ausentes
    rais_cbo4 <- rais_cbo4 %>%
      mutate(
        cbo_1dig = if_else(is.na(cbo_1dig), substr(cbo_4dig, 1, 1), cbo_1dig),
        cbo_2dig = if_else(is.na(cbo_2dig), substr(cbo_4dig, 1, 2), cbo_2dig)
      )
    
    # Diagnóstico
    n_sem_familia <- sum(is.na(rais_cbo4$cbo_familia))
    message(">> Sem cbo_familia: ", n_sem_familia, " linhas")
    
    # Salvar
    assign(nome_cbo4, rais_cbo4)
    save(list = nome_cbo4, file = local_cbo4)
    message(">> Salvo: ", local_cbo4)
    upload_s3_se_diferente(local_cbo4, s3_cbo4, S3_BUCKET)
    
    # Limpar memória
    rm(rais_raw, rais_cbo6, rais_cbo4)
    gc()
    
  } # Fim do bloco else (reprocessamento)
  
  #############################################################################
  # VALIDAÇÃO
  #############################################################################
  
  obj_cbo6 <- get(nome_cbo6)
  obj_cbo4 <- get(nome_cbo4)
  
  message("\n>> Validação RAIS ", ano, ":")
  message(">> ", nome_cbo6, ": ", format(nrow(obj_cbo6), big.mark = "."),
          " linhas | vínculos: ", format(sum(obj_cbo6$vinculos), big.mark = "."))
  message(">> ", nome_cbo4, ": ", format(nrow(obj_cbo4), big.mark = "."),
          " linhas | vínculos: ", format(sum(obj_cbo4$vinculos), big.mark = "."))
  message(">> UFs: ", n_distinct(obj_cbo6$SG_UF))
  message(">> CBOs 6-dig: ", n_distinct(obj_cbo6$CodCBO),
          " | Famílias 4-dig: ", n_distinct(obj_cbo4$cbo_4dig))
  
  # Limpar objetos do ano
  rm(list = c(nome_cbo6, nome_cbo4))
  rm(obj_cbo6, obj_cbo4)
  gc()
}

message("\n=== rais_02a.R concluído ===")