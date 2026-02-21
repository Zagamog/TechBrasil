###############################################################################
# pnad_ocups_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Download de Microdados PNAD Contínua — 2º Trimestre
#
# USO NAS ABAS: D3 (Informalidade via pnad_ocups_01b.R → rais_apo_04a.R)
#
# OBJETIVO:
#   Baixar os microdados da PNAD Contínua, 2º trimestre, diretamente
#   do IBGE usando o pacote PNADcIBGE. O 2º trimestre é o suplemento
#   que contém informações sobre educação profissional e técnica
#   (variáveis V3019A-V3032), essenciais para identificar trabalhadores
#   que concluíram cursos técnicos.
#
#   Cada ano é salvo como um arquivo .rds individual em rawdata/pnad/.
#   O script verifica se o arquivo já existe antes de baixar.
#
# VARIÁVEIS SELECIONADAS:
#   V2009   — Idade
#   VD3004  — Nível de instrução mais elevado
#   VD4001  — Condição em relação à força de trabalho
#   VD4002  — Condição de ocupação (1=ocupado, 2=desocupado)
#   VD4003  — Subgrupo da força de trabalho
#   VD4004A — Contribuição para a previdência
#   VD4009  — Posição na ocupação no trabalho principal
#   VD4011  — Atividade do empreendimento
#   VD4012  — Agrupamento de atividade
#   VD4013  — Contribuição CNPJ no trabalho principal
#   VD4019  — Número de empregados
#   VD4020  — Rendimento mensal habitual
#   VD4032  — Rendimento hora trabalho principal
#   V3019A  — Já concluiu algum curso de qualificação profissional?
#   V3020B  — Área do curso de qualificação 1
#   V3020C  — Área do curso de qualificação 2
#   V3021A  — Já concluiu curso técnico de nível médio?
#   V3022C  — Código do curso técnico 1
#   V3022D  — Código do curso técnico 2
#   V3022E  — Código do curso técnico 3
#   V3023A  — Concluiu curso técnico de nível médio? (Sim/Não)
#   V4019   — CNPJ do empreendimento
#   V4010   — Código da ocupação (COD)
#   V3026   — Frequenta escola/curso?
#   V3026A  — Tipo de escola/curso
#   V3029   — Já frequentou escola?
#   V3029A  — Nível mais elevado que frequentou
#   V3032   — Concluiu curso superior de graduação?
#
# DADOS BRUTOS (local — não vão ao S3):
#   rawdata/pnad/pnadc_q2_{ANO}.rds (um por ano)
#   Formato .rds (saveRDS/readRDS) — objeto único sem nome embutido,
#   usado por convenção para cache de downloads brutos.
#   Diferente do .rda (save/load) usado em working/ para dados processados.
#
# NÃO USA S3 — downloads são grandes e feitos uma única vez via IBGE API.
# DEPENDÊNCIAS: PNADcIBGE, purrr
# NOTA: Requer conexão com internet para download do IBGE
###############################################################################

if (!requireNamespace("PNADcIBGE", quietly = TRUE)) {
  install.packages("PNADcIBGE")
}
library(PNADcIBGE)
library(purrr)

# Timeout estendido para downloads grandes
options(timeout = 3600)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

# NOTA: O diretório de trabalho deve ser a raiz do projeto
#   setwd("D:/Country/Brazil/TechBrazil")  # ou equivalente

# Anos a baixar
ANOS <- 2016:2024

# Diretório de destino (rawdata — dados brutos)
PNAD_RAW_DIR <- "rawdata/pnad"

# Variáveis a extrair da PNAD-C Q2
VARS_PNADC <- c(
  "V2009", "VD3004", "VD4001", "VD4002", "VD4003", "VD4004A", "VD4009",
  "VD4011", "VD4012", "VD4013", "VD4019", "VD4020", "VD4032",
  "V3019A", "V3020B", "V3020C", "V3021A", "V3022C", "V3022D", "V3022E",
  "V3023A", "V4019", "V4010", "V3026", "V3026A", "V3029", "V3029A", "V3032"
)

###############################################################################
# DOWNLOAD
###############################################################################

dir.create(PNAD_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

baixar_trimestre <- function(year) {
  out_file <- file.path(PNAD_RAW_DIR, paste0("pnadc_q2_", year, ".rds"))
  
  if (file.exists(out_file)) {
    message(">> Já existe: ", out_file)
    return(invisible(out_file))
  }
  
  message(">> Baixando PNAD-C Q2 ", year, "...")
  df <- get_pnadc(
    year     = year,
    topic    = 2,
    design   = FALSE,
    vars     = VARS_PNADC,
    deflator = FALSE,
    labels   = TRUE
  )
  saveRDS(df, out_file)
  message("   Salvo: ", out_file, " (", format(nrow(df), big.mark = "."), " obs)")
  
  invisible(out_file)
}

# Baixar todos os anos
walk(ANOS, baixar_trimestre)

message("=== pnad_ocups_01a.R concluído ===")