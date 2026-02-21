###############################################################################
# rais_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Extração de Microdados RAIS (Vínculos) por Ano
#
# USO NAS ABAS: D2 (APLs), D3 (Informalidade), E1 (Oferta-Demanda)
#               Via rais_02a.R e rais_apo_*.R
#
# OBJETIVO:
#   Ler os arquivos regionais de microdados RAIS (formato .txt),
#   selecionar 7 colunas essenciais para o pipeline,
#   combinar todas as regiões, juntar com códigos geográficos IBGE,
#   e salvar um arquivo .rda por ano.
#
#   O script processa um ano por vez para gerenciamento de memória
#   (~57 milhões de vínculos por ano, ~10-15 GB em .txt).
#
# ═══════════════════════════════════════════════════════════════════════
# ORIGEM DOS DADOS BRUTOS
# ═══════════════════════════════════════════════════════════════════════
#
#   Os microdados RAIS são disponibilizados pelo MTE via FTP anônimo:
#     ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/{ANO}/
#
#   PROCEDIMENTO DE DOWNLOAD (manual, em Windows):
#     1. Abrir WinSCP → Nova Conexão:
#        Protocolo: FTP
#        Host: ftp.mtps.gov.br
#        Usuário: anonymous
#        Senha: (em branco)
#     2. Navegar até /pdet/microdados/RAIS/{ANO}/
#     3. Baixar os arquivos RAIS_VINC_PUB_*.7z para:
#        rawdata/rais/mintraemp_download/raw_7z_files/{ANO}/
#     4. Extrair os .7z usando 7-Zip (via Windows Explorer):
#        Botão direito → 7-Zip → Extrair para:
#        rawdata/rais/mintraemp_download/txt_extracted/{ANO}/
#
#   NOTA: 7za.exe standalone pode ser bloqueado por políticas
#   de segurança corporativas (Device Guard). Usar 7-Zip instalado
#   via instalador oficial do Windows.
#
#   NOTA: Arquivos de 2023 podem ser extraídos com extensão .COMT
#   em vez de .txt. Renomear para .txt antes de executar o script,
#   ou o script aceita ambas as extensões automaticamente.
#
# ═══════════════════════════════════════════════════════════════════════
# TAMANHO DOS ARQUIVOS (não viáveis para S3)
# ═══════════════════════════════════════════════════════════════════════
#
#   2024 (.txt extraídos, released Dec 2025 — dados definitivos):
#     RAIS_VINC_PUB_CENTRO_OESTE  ~2.1 GB
#     RAIS_VINC_PUB_MG_ES_RJ      ~4.5 GB
#     RAIS_VINC_PUB_NI             ~4 KB  (não identificado — negligível)
#     RAIS_VINC_PUB_NORDESTE       ~3.9 GB
#     RAIS_VINC_PUB_NORTE          ~1.3 GB
#     RAIS_VINC_PUB_SP             ~6.1 GB
#     RAIS_VINC_PUB_SUL            ~3.9 GB
#     Total: ~22 GB, ~57 milhões de vínculos
#
#   2023 (.txt extraídos, released Jul 2025 — dados definitivos):
#     Mesma estrutura de arquivos regionais, tamanhos similares.
#     Total: ~55 milhões de vínculos
#
# ═══════════════════════════════════════════════════════════════════════
# FORMATO DOS ARQUIVOS
# ═══════════════════════════════════════════════════════════════════════
#
#   Encoding: Latin-1
#
#   2023 (sep = ";"): 59 colunas, nomes curtos, ex:
#     "CBO Ocupação 2002", "Vínculo Ativo 31/12", "Município"
#
#   2024 (sep = ","): 61 colunas, nomes com sufixo " - Código", ex:
#     "CBO 2002 Ocupação - Código", "Ind Vínculo Ativo 31/12 - Código"
#
#   Listagem de colunas 2024 (referência — posições podem variar):
#
#   [ 1] Bairros SP - Código
#   [ 2] Bairros Fortaleza - Código
#   [ 3] Bairros RJ - Código
#   [ 4] Causa Afastamento 1 - Código
#   [ 5] Causa Afastamento 2 - Código
#   [ 6] Causa Afastamento 3 - Código
#   [ 7] Motivo Desligamento - Código
#   [ 8] CBO 2002 Ocupação - Código          *** SELECIONADA → CodCBO
#   [ 9] CNAE 2.0 Classe - Código
#   [10] CNAE 95 Classe - Código
#   [11] Distritos SP - Código
#   [12] Ind Vínculo Ativo 31/12 - Código    *** SELECIONADA → vinculos
#   [13] Faixa Etária - Código
#   [14] Faixa Rem Média (SM) - Código
#   [15] Faixa Hora Contrat - Código
#   [16] Faixa Rem Dez (SM) - Código
#   [17] Faixa Tempo Emprego - Código
#   [18] Escolaridade Após 2005 - Código     *** SELECIONADA → escolaridade
#   [19] Qtd Hora Contr
#   [20] Idade                                *** SELECIONADA → idade
#   [21] Ind CEI Vinculado - Código
#   [22] Ind Estabelecimento Participante SIMPLES - Código
#   [23] Mês Admissão - Código
#   [24] Mês Desligamento - Código
#   [25] Município Trab - Código             (município de trabalho — NÃO usada)
#   [26] Município - Código                   *** SELECIONADA → CO_MUN6
#   [27] Nacionalidade - Código
#   [28] Natureza Jurídica - Código
#   [29] Ind Portador Defic - Código
#   [30] Qtd Dias Afastamento
#   [31] Raça Cor - Código
#   [32] Região Adm DF - Código
#   [33] Vl Rem Dezembro Nom
#   [34] Vl Rem Dezembro (SM)
#   [35] Vl Rem Média Nom                    *** SELECIONADA → remuneracao
#   [36] Vl Rem Média (SM)
#   [37] CNAE 2.0 Subclasse - Código
#   [38] Sexo - Código                        *** SELECIONADA → sexo
#   [39] Tamanho Estabelecimento - Código
#   [40] Tempo Emprego
#   [41] Tipo Admissão Trabalhador - Código
#   [42] Tipo Estabelecimento - Código
#   [43] Tipo Estabelecimento - Nome
#   [44] Tipo Deficiência - Código
#   [45] Tipo Vínculo - Código
#   [46] IBGE Subsetor - Código
#   [47-57] Vl Rem Janeiro-Novembro SC
#   [58] Ano Chegada Brasil
#   [59] Ind Trabalho Intermitente - Código
#   [60] Ind Trabalho Parcial - Código
#   [61] Ind Vínculo Abandonado - Código
#
#   NOTA: A coluna [25] "Município Trab" é o município onde o
#   trabalhador exerce a atividade. A coluna [26] "Município" é onde
#   o estabelecimento está registrado. Para análise de APLs (concentração
#   de atividade econômica por estabelecimento) usamos [26].
#
#   NOTA: Formato anterior a 2023 usava sep = ";" e nomes de colunas
#   diferentes (ex: "Mun Trab", "CBO Ocupação 2002", "Vínculo Ativo 31/12").
#   Este script cobre apenas 2023+.
#
# COLUNAS SELECIONADAS (7 de 61):
#
#   CodCBO        — Código CBO 2002 (6 dígitos, identifica ocupação)
#   vinculos      — Vínculo ativo em 31/12 (1=ativo, 0=inativo)
#   escolaridade  — Escolaridade após 2005 (código):
#                     1 = Analfabeto
#                     2 = Até 5ª incompleto
#                     3 = 5ª completo fundamental
#                     4 = 6ª a 9ª fundamental
#                     5 = Fundamental completo
#                     6 = Médio incompleto
#                     7 = Médio completo
#                     8 = Superior incompleto
#                     9 = Superior completo
#                    10 = Mestrado
#                    11 = Doutorado
#                    -1 = Não identificado
#                   Para nível técnico: filtrar escolaridade %in% c(1:8, -1)
#   idade         — Idade do trabalhador
#   CO_MUN6       — Código do município do estabelecimento (6 dígitos IBGE)
#   remuneracao   — Remuneração média nominal (R$)
#   sexo          — Sexo (1=Masculino, 2=Feminino)
#
# SAÍDA (local — arquivos grandes, não vão ao S3):
#   working/rais/{ANO}/rais{ANO}.rda
#
# DEPENDÊNCIAS: data.table, dplyr
# NÃO USA S3 — script 100% local
###############################################################################

library(data.table)
library(dplyr)

###############################################################################
# CONFIGURAÇÃO — AJUSTAR CONFORME AMBIENTE LOCAL
###############################################################################

# NOTA: O diretório de trabalho deve ser a raiz do projeto
#   setwd("D:/Country/Brazil/TechBrazil")  # ou equivalente
# Todos os caminhos abaixo são relativos a esta raiz.

# Diretório base dos arquivos RAIS extraídos
RAIS_BASE <- "rawdata/rais/mintraemp_download"

# Anos a processar (extensível para anos futuros)
ANOS <- c(2023, 2024)

# Nomes padronizados para o pipeline (saída)
NOMES_PADRAO <- c(
  "CodCBO",
  "vinculos",
  "escolaridade",
  "idade",
  "CO_MUN6",
  "remuneracao",
  "sexo"
)

# Padrões grep para encontrar as 7 colunas desejadas
# (robustos às diferenças de nomenclatura entre anos)
#
# Formato 2023 (sep=";"): nomes curtos, ex:
#   "CBO Ocupação 2002", "Vínculo Ativo 31/12", "Escolaridade após 2005",
#   "Idade", "Município", "Vl Remun Média Nom", "Sexo Trabalhador"
#
# Formato 2024+ (sep=","): nomes com sufixo " - Código", ex:
#   "CBO 2002 Ocupação - Código", "Ind Vínculo Ativo 31/12 - Código",
#   "Escolaridade Após 2005 - Código", "Idade",
#   "Município - Código", "Vl Rem Média Nom", "Sexo - Código"
#
PADROES_COLUNAS <- list(
  CodCBO       = "CBO.*Ocupa.*2002|CBO 2002.*Ocupa",
  vinculos     = "V.nculo Ativo 31.12",
  escolaridade = "Escolaridade",
  idade        = "^Idade$",
  CO_MUN6      = "^Munic.pio( - C.digo)?$",
  remuneracao  = "Rem.*M.dia Nom",
  sexo         = "^Sexo"
)

# Função para detectar separador automaticamente
detectar_separador <- function(arquivo) {
  # Ler primeira linha como bytes brutos (evitar problemas de encoding)
  con <- file(arquivo, "rb")
  linha1 <- readLines(con, n = 1, warn = FALSE)
  close(con)
  # Contar ocorrências de cada separador candidato (useBytes para Latin-1)
  n_virgulas <- nchar(gsub("[^,]", "", linha1, useBytes = TRUE), type = "bytes")
  n_pontovirgula <- nchar(gsub("[^;]", "", linha1, useBytes = TRUE), type = "bytes")
  if (n_pontovirgula > n_virgulas) return(";")
  return(",")
}

# Função para encontrar coluna por padrão grep
encontrar_coluna <- function(nomes, padrao, nome_saida) {
  idx <- grep(padrao, nomes, ignore.case = TRUE, perl = TRUE)
  if (length(idx) == 0) return(NA_integer_)
  if (length(idx) > 1) {
    # Para "Município" — preferir a versão sem "Trab"
    nomes_match <- nomes[idx]
    sem_trab <- idx[!grepl("Trab", nomes_match, ignore.case = TRUE)]
    if (length(sem_trab) >= 1) idx <- sem_trab[1]
    else idx <- idx[1]
    message("   ", nome_saida, " → [", idx, "] ", nomes[idx], " (de ", length(idx), " matches)")
  } else {
    message("   ", nome_saida, " → [", idx, "] ", nomes[idx])
  }
  return(idx)
}

###############################################################################
# PROCESSAMENTO
###############################################################################

# Carregar códigos geográficos IBGE
load("working/ibge/df_codes_ibge.rda")
df_geo <- df_codes_ibge %>%
  select(CO_MUN6, NM_MUN, SG_UF, CO_UF, NM_UF) %>%
  distinct()

for (ano in ANOS) {
  
  message("\n", strrep("=", 60))
  message("=== PROCESSANDO RAIS ", ano, " ===")
  message(strrep("=", 60))
  
  # Verificar se saída já existe
  saida_dir <- file.path("working/rais", as.character(ano))
  saida_arquivo <- file.path(saida_dir, paste0("rais", ano, ".rda"))
  
  if (file.exists(saida_arquivo)) {
    message(">> Saída já existe: ", saida_arquivo)
    message(">> Para reprocessar, remova o arquivo e execute novamente")
    next
  }
  
  # Listar arquivos .txt e .COMT (2023 pode ter extensão .COMT)
  txt_dir <- file.path(RAIS_BASE, "txt_extracted", as.character(ano))
  lista_arquivos <- list.files(txt_dir, pattern = "\\.(txt|COMT)$",
                               full.names = TRUE, ignore.case = TRUE)
  
  if (length(lista_arquivos) == 0) {
    message(">> ERRO: Nenhum arquivo .txt ou .COMT encontrado em: ", txt_dir)
    message(">> Verifique se os .7z foram extraídos corretamente")
    next
  }
  
  message(">> Arquivos encontrados: ", length(lista_arquivos))
  for (f in lista_arquivos) {
    message("   ", basename(f), " (",
            format(file.size(f) / 1e6, big.mark = ".", decimal.mark = ",",
                   nsmall = 0), " MB)")
  }
  
  # Auto-detectar separador e colunas usando arquivo NI ou primeiro disponível
  arquivo_teste <- list.files(txt_dir, pattern = "NI\\.(txt|COMT)$",
                              full.names = TRUE, ignore.case = TRUE)
  if (length(arquivo_teste) == 0) arquivo_teste <- lista_arquivos[1]
  else arquivo_teste <- arquivo_teste[1]
  
  sep_ref <- detectar_separador(arquivo_teste)
  message(">> Separador de referência (NI): '", sep_ref, "'")
  
  nomes_ref <- names(fread(arquivo_teste, nrows = 0, sep = sep_ref,
                           encoding = "Latin-1"))
  message(">> Colunas no arquivo de referência: ", length(nomes_ref))
  
  # Encontrar as 7 colunas por padrão grep (na referência)
  message(">> Mapeando colunas:")
  indices_ref <- sapply(names(PADROES_COLUNAS), function(nome) {
    encontrar_coluna(nomes_ref, PADROES_COLUNAS[[nome]], nome)
  })
  
  # Verificar que todas as colunas foram encontradas
  faltantes <- names(indices_ref)[is.na(indices_ref)]
  if (length(faltantes) > 0) {
    message(">> ERRO: Colunas não encontradas: ", paste(faltantes, collapse = ", "))
    message(">> Colunas disponíveis:\n",
            paste(sprintf("   [%2d] %s",
                          seq_along(nomes_ref), nomes_ref),
                  collapse = "\n"))
    next
  }
  message(">> Todas as 7 colunas encontradas")
  
  # Ler e combinar arquivos regionais
  lista_dt <- list()
  
  for (arquivo in lista_arquivos) {
    nome_base <- basename(arquivo)
    tamanho_mb <- file.size(arquivo) / 1e6
    
    # Pular arquivo NI se muito pequeno (< 1 MB)
    if (tamanho_mb < 1) {
      message(">> Pulando ", nome_base, " (< 1 MB)")
      next
    }
    
    message(">> Lendo: ", nome_base, " (",
            format(tamanho_mb, big.mark = ".", decimal.mark = ",",
                   nsmall = 0), " MB)")
    
    # Detectar separador POR ARQUIVO (podem variar dentro do mesmo ano)
    sep_arq <- detectar_separador(arquivo)
    if (sep_arq != sep_ref) {
      message("   AVISO: separador diferente da referência: '", sep_arq, "'")
      # Remapear colunas para este arquivo
      nomes_arq <- names(fread(arquivo, nrows = 0, sep = sep_arq,
                               encoding = "Latin-1"))
      indices_arq <- sapply(names(PADROES_COLUNAS), function(nome) {
        encontrar_coluna(nomes_arq, PADROES_COLUNAS[[nome]], nome)
      })
      if (any(is.na(indices_arq))) {
        message("   ERRO: Colunas não encontradas em ", nome_base, " — pulando")
        next
      }
    } else {
      indices_arq <- indices_ref
    }
    
    dt <- fread(arquivo, sep = sep_arq, encoding = "Latin-1",
                showProgress = TRUE, select = as.integer(indices_arq),
                quote = "")
    
    # Renomear para nomes padronizados
    setnames(dt, NOMES_PADRAO)
    
    message("   ", format(nrow(dt), big.mark = "."), " vínculos")
    lista_dt <- c(lista_dt, list(dt))
  }
  
  # Combinar todas as regiões
  message("\n>> Combinando ", length(lista_dt), " regiões...")
  dt_all <- rbindlist(lista_dt, use.names = TRUE)
  rm(lista_dt)
  gc()
  
  message(">> Total bruto: ", format(nrow(dt_all), big.mark = "."),
          " vínculos")
  
  # Converter tipos
  dt_all[, CodCBO := as.character(CodCBO)]
  dt_all[, CO_MUN6 := as.integer(CO_MUN6)]
  
  # Filtrar município inválido (999999 = não identificado)
  n_invalidos <- sum(dt_all$CO_MUN6 == 999999, na.rm = TRUE)
  n_na <- sum(is.na(dt_all$CO_MUN6))
  dt_all <- dt_all[CO_MUN6 != 999999 & !is.na(CO_MUN6)]
  message(">> Removidos: ", format(n_invalidos, big.mark = "."),
          " com CO_MUN6=999999, ", format(n_na, big.mark = "."), " NAs")
  
  # Join com códigos geográficos
  message(">> Juntando com códigos geográficos IBGE...")
  rais_ano <- merge(dt_all, df_geo, by = "CO_MUN6", all.x = TRUE)
  rm(dt_all)
  gc()
  
  # Validação
  n_total <- nrow(rais_ano)
  n_ativos <- sum(rais_ano$vinculos == 1, na.rm = TRUE)
  n_ufs <- length(unique(rais_ano$SG_UF[!is.na(rais_ano$SG_UF)]))
  n_muns <- length(unique(rais_ano$CO_MUN6))
  
  message("\n>> Validação RAIS ", ano, ":")
  message(">> Total: ", format(n_total, big.mark = "."), " vínculos")
  message(">> Vínculos ativos (31/12): ", format(n_ativos, big.mark = "."))
  message(">> UFs: ", n_ufs, " | Municípios: ", format(n_muns, big.mark = "."))
  
  # Salvar
  dir.create(saida_dir, recursive = TRUE, showWarnings = FALSE)
  nome_obj <- paste0("rais", ano)
  assign(nome_obj, rais_ano)
  save(list = nome_obj, file = saida_arquivo)
  message(">> Salvo: ", saida_arquivo)
  
  # Limpar memória antes do próximo ano
  rm(rais_ano)
  rm(list = nome_obj)
  gc()
}

message("\n=== rais_01a.R concluído ===")