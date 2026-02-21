###############################################################################
# rais_apo_01a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Cubos de Emprego RAIS por CBO × Nível Geográfico
#
# USO NAS ABAS: D2 (APLs via rais_apo_01b.R → rais_apo_02*.R → rais_apo_03*.R)
#
# OBJETIVO:
#   A partir dos microdados RAIS (de rais_01a.R), filtrar apenas:
#     - Vínculos ativos em 31/12 (vinculos == 1)
#     - CBO válido (não vazio, não NA)
#     - Escolaridade até nível técnico (escolaridade %in% c(1:8, -1))
#   E criar "cubos" de contagem de emprego em 5 níveis geográficos:
#     1. Municipal (CO_MUN6)
#     2. Região Geográfica Imediata (CO_RGIMED)
#     3. Região Geográfica Intermediária (CO_RGINTM)
#     4. UF (SG_UF)
#     5. Brasil (nacional)
#
#   O filtro de escolaridade é fundamental: para análise de EPT (Educação
#   Profissional e Técnica), queremos medir demanda por trabalhadores de
#   nível técnico, excluindo os que já possuem ensino superior completo.
#
#   Processa um ano por vez para gerenciamento de memória.
#
# INSUMOS:
#   LOCAL (de rais_01a.R — arquivos grandes):
#     working/rais/2023/rais2023.rda
#     working/rais/2024/rais2024.rda
#
#   S3:
#     working/ibge/df_codes_ibge.rda  (para regiões geográficas)
#     working/qbq/qbq_ocup_cmento1.rda (para lookup CBO 4 dígitos)
#
# SAÍDAS (local — usadas por rais_apo_01b.R):
#   working/rais/2023/cubes_2023.rda
#   working/rais/2024/cubes_2024.rda
#
#   Cada arquivo contém 10 objetos:
#     employment_{ano}      — contagem por (year, cbo_4dig, CO_MUN6)
#     mun_totals_{ano}      — total por (year, CO_MUN6)
#     employment_rgimed_{ano} — contagem por (year, cbo_4dig, CO_RGIMED)
#     rgimed_totals_{ano}   — total por (year, CO_RGIMED)
#     employment_rgintm_{ano} — contagem por (year, cbo_4dig, CO_RGINTM)
#     rgintm_totals_{ano}   — total por (year, CO_RGINTM)
#     employment_uf_{ano}   — contagem por (year, cbo_4dig, SG_UF)
#     uf_totals_{ano}       — total por (year, SG_UF)
#     br_cbo_{ano}          — contagem por (year, cbo_4dig) — Brasil
#     br_total_{ano}        — total por (year) — Brasil
#
# DEPENDÊNCIAS: data.table
# NÃO USA S3 para saídas — cubos ficam locais (usados por rais_apo_01b.R)
###############################################################################

library(data.table)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

# NOTA: O diretório de trabalho deve ser a raiz do projeto
#   setwd("D:/Country/Brazil/TechBrazil")  # ou equivalente
# Todos os caminhos abaixo são relativos a esta raiz.

# Anos a processar
ANOS <- c(2023, 2024)

###############################################################################
# PASSO 0: CARREGAR LOOKUPS
###############################################################################

message("=== Carregando lookups ===")

# Códigos geográficos — precisamos das regiões (RGIMED, RGINTM)
# que NÃO estão no rais{ANO}.rda (rais_01a.R inclui apenas UF)
load("working/ibge/df_codes_ibge.rda")
dft_geo_keys <- as.data.table(df_codes_ibge)[
  , .(CO_MUN6, SG_UF, NM_UF, CO_UF,
      CO_RGIMED, NM_RGIMED, CO_RGINTM, NM_RGIINTM)
]
dft_geo_keys <- unique(dft_geo_keys, by = "CO_MUN6")
setDT(dft_geo_keys)
message(">> Geo keys: ", nrow(dft_geo_keys), " municípios")

# Lookup CBO 4 dígitos → família ocupacional
load("working/qbq/qbq_ocup_cmento1.rda")
cbo_4dig_lookup <- as.data.table(qbq_ocup_cmento1)[
  !is.na(cbo_4dig) & !is.na(cbo_familia),
  .(cbo_4dig, cbo_familia)
]
cbo_4dig_lookup <- unique(cbo_4dig_lookup, by = "cbo_4dig")
setDT(cbo_4dig_lookup)
message(">> CBO lookup: ", nrow(cbo_4dig_lookup), " famílias")

###############################################################################
# FUNÇÃO: PROCESSAR UM ANO DE RAIS
###############################################################################
#
# Filtra microdados para vínculos ativos de nível técnico,
# junta geografia regional e CBO, retorna data.table limpo.
###############################################################################

processar_rais_ano <- function(rais_data, year) {
  
  dt <- as.data.table(rais_data)
  n_total <- nrow(dt)
  
  # Filtro 1: Vínculo ativo em 31/12
  dt <- dt[vinculos == 1]
  message("   Após filtro vinculos==1: ", format(nrow(dt), big.mark = "."))
  
  # Filtro 2: CBO válido
  dt <- dt[!is.na(CodCBO) & CodCBO != ""]
  message("   Após filtro CBO válido: ", format(nrow(dt), big.mark = "."))
  
  # Filtro 3: Escolaridade até nível técnico (≤8) ou não identificado (-1)
  # Exclui: 9=Superior completo, 10=Mestrado, 11=Doutorado
  dt <- dt[escolaridade %in% c(1:8, -1)]
  message("   Após filtro escolaridade ≤ 8: ", format(nrow(dt), big.mark = "."))
  
  # Derivar CBO 4 dígitos e ano
  dt[, `:=`(
    CodCBO = as.character(CodCBO),
    year = year
  )]
  dt[, cbo_4dig := substr(CodCBO, 1, 4)]
  
  # Join com geografia regional (CO_RGIMED, CO_RGINTM)
  dt <- dft_geo_keys[dt, on = "CO_MUN6"]
  
  # Join com CBO família
  dt <- cbo_4dig_lookup[dt, on = "cbo_4dig"]
  
  # Filtrar registros sem família CBO ou sem UF
  dt <- dt[!is.na(cbo_familia) & !is.na(CO_UF)]
  message("   Após join geo + CBO: ", format(nrow(dt), big.mark = "."),
          " vínculos válidos")
  
  # Selecionar colunas necessárias para cubos
  dt <- dt[, .(year, cbo_4dig, cbo_familia, CO_MUN6, CO_UF, SG_UF, NM_UF,
               CO_RGIMED, NM_RGIMED, CO_RGINTM, NM_RGIINTM)]
  
  return(dt)
}

###############################################################################
# PROCESSAMENTO POR ANO
###############################################################################

for (ano in ANOS) {
  
  message("\n", strrep("=", 60))
  message("=== CUBOS RAIS ", ano, " ===")
  message(strrep("=", 60))
  
  # Verificar se saída já existe
  saida_arquivo <- file.path("working/rais",
                             as.character(ano), paste0("cubes_", ano, ".rda"))
  
  if (file.exists(saida_arquivo)) {
    message(">> Saída já existe: ", saida_arquivo)
    message(">> Para reprocessar, remova o arquivo e execute novamente")
    next
  }
  
  # Carregar microdados RAIS
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
  
  message(">> Vínculos brutos: ", format(nrow(rais_raw), big.mark = "."))
  
  # Processar e filtrar
  message(">> Filtrando:")
  dt <- processar_rais_ano(rais_raw, ano)
  rm(rais_raw)
  gc()
  
  # Criar cubos em todos os níveis geográficos
  message(">> Criando cubos...")
  
  # Municipal
  employment <- dt[, .N, by = .(year, cbo_4dig, CO_MUN6)]
  mun_totals <- dt[, .N, by = .(year, CO_MUN6)]
  
  # Região Geográfica Imediata
  employment_rgimed <- dt[, .N, by = .(year, cbo_4dig, CO_RGIMED)]
  rgimed_totals     <- dt[, .N, by = .(year, CO_RGIMED)]
  
  # Região Geográfica Intermediária
  employment_rgintm <- dt[, .N, by = .(year, cbo_4dig, CO_RGINTM)]
  rgintm_totals     <- dt[, .N, by = .(year, CO_RGINTM)]
  
  # UF
  employment_uf <- dt[, .N, by = .(year, cbo_4dig, SG_UF)]
  uf_totals     <- dt[, .N, by = .(year, SG_UF)]
  
  # Brasil
  br_cbo   <- dt[, .N, by = .(year, cbo_4dig)]
  br_total <- dt[, .N, by = year]
  
  rm(dt)
  gc()
  
  # Renomear objetos com sufixo do ano para salvar no mesmo .rda
  sufixo <- as.character(ano)
  
  assign(paste0("employment_", sufixo), employment)
  assign(paste0("mun_totals_", sufixo), mun_totals)
  assign(paste0("employment_rgimed_", sufixo), employment_rgimed)
  assign(paste0("rgimed_totals_", sufixo), rgimed_totals)
  assign(paste0("employment_rgintm_", sufixo), employment_rgintm)
  assign(paste0("rgintm_totals_", sufixo), rgintm_totals)
  assign(paste0("employment_uf_", sufixo), employment_uf)
  assign(paste0("uf_totals_", sufixo), uf_totals)
  assign(paste0("br_cbo_", sufixo), br_cbo)
  assign(paste0("br_total_", sufixo), br_total)
  
  # Salvar todos os cubos
  dir.create(dirname(saida_arquivo), recursive = TRUE, showWarnings = FALSE)
  
  nomes_cubos <- paste0(
    c("employment_", "mun_totals_",
      "employment_rgimed_", "rgimed_totals_",
      "employment_rgintm_", "rgintm_totals_",
      "employment_uf_", "uf_totals_",
      "br_cbo_", "br_total_"),
    sufixo
  )
  
  save(list = nomes_cubos, file = saida_arquivo)
  message(">> Salvo: ", saida_arquivo)
  
  # Validação
  message("\n>> Validação cubos ", ano, ":")
  message(">> Total Brasil: ", format(br_total$N, big.mark = "."), " vínculos")
  message(">> Municípios: ", format(nrow(mun_totals), big.mark = "."))
  message(">> Regiões Imediatas: ", nrow(rgimed_totals))
  message(">> Regiões Intermediárias: ", nrow(rgintm_totals))
  message(">> UFs: ", nrow(uf_totals))
  message(">> Famílias CBO: ", n_distinct(employment$cbo_4dig))
  
  # Limpar
  rm(list = nomes_cubos)
  rm(employment, mun_totals, employment_rgimed, rgimed_totals,
     employment_rgintm, rgintm_totals, employment_uf, uf_totals,
     br_cbo, br_total)
  gc()
}

message("\n=== rais_apo_01a.R concluído ===")