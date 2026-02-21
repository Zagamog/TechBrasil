###############################################################################
# rais_apo_01b.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Índices de Especialização e Complexidade Ocupacional
#
# USO NAS ABAS: D2 (APLs via rais_apo_02*.R)
#
# OBJETIVO:
#   A partir dos cubos de emprego (de rais_apo_01a.R), calcular índices
#   de especialização ocupacional ao nível municipal:
#
#   1. Location Quotient (LQ): mede concentração relativa de uma
#      ocupação no município vs. a média nacional.
#      LQ = (E_mun_cbo / E_mun) / (E_br_cbo / E_br)
#      LQ > 1 → município é mais especializado que a média nacional
#
#   2. RCA (Revealed Comparative Advantage):
#      - RCA1: binário (1 se LQ ≥ 1, 0 caso contrário)
#      - RCA_squash: (LQ-1)/(LQ+1) — transforma LQ em [-1, 1]
#      - logRCA: log(LQ) — simétrico em torno de 0
#
#   3. Diversidade municipal: quantas ocupações diferentes o município tem
#      - HHI (Herfindahl-Hirschman): concentração (menor = mais diverso)
#      - Shannon: entropia (maior = mais diverso)
#
#   4. Ubiquidade ocupacional: em quantos municípios a ocupação aparece
#      com RCA ≥ 1
#
#   5. Complexidade (proxy):
#      - Ocupação: média do HHI dos municípios onde RCA ≥ 1
#      - Município: média da ubiquidade das ocupações onde RCA ≥ 1
#      (Inspirado no método Hidalgo-Hausmann, sem iteração completa)
#
#   Ambos os anos (2023, 2024) são empilhados e processados juntos,
#   com coluna "year" para distinguir.
#
# INSUMOS (de rais_apo_01a.R — local):
#   working/rais/2023/cubes_2023.rda
#   working/rais/2024/cubes_2024.rda
#
# SAÍDAS (local — usadas por rais_apo_02*.R):
#   working/rais/indices/dft_apl_MUN_2324.rda
#     → LQ, RCA por (year, cbo_4dig, CO_MUN6)
#   working/rais/indices/dft_diversity_MUN_2324.rda
#     → HHI, Shannon por (year, CO_MUN6)
#   working/rais/indices/dft_ubiquity_CBO_2324.rda
#     → ubiquidade por (year, cbo_4dig)
#   working/rais/indices/dft_occ_complexity_2324.rda
#     → complexidade ocupacional por (year, cbo_4dig)
#   working/rais/indices/dft_mun_complexity_2324.rda
#     → complexidade municipal por (year, CO_MUN6)
#
# DEPENDÊNCIAS: data.table
###############################################################################

library(data.table)

###############################################################################
# PASSO 1: CARREGAR E EMPILHAR CUBOS
###############################################################################

message("=== Carregando cubos ===")

load("working/rais/2023/cubes_2023.rda")
load("working/rais/2024/cubes_2024.rda")

# Empilhar ambos os anos (nível municipal apenas — cubes regionais
# e UF são usados em análises específicas em outros scripts)
employment <- rbindlist(list(employment_2023, employment_2024), use.names = TRUE)
mun_totals <- rbindlist(list(mun_totals_2023, mun_totals_2024), use.names = TRUE)
br_cbo     <- rbindlist(list(br_cbo_2023, br_cbo_2024), use.names = TRUE)
br_total   <- rbindlist(list(br_total_2023, br_total_2024), use.names = TRUE)

# Limpar objetos individuais
rm(employment_2023, employment_2024, mun_totals_2023, mun_totals_2024,
   br_cbo_2023, br_cbo_2024, br_total_2023, br_total_2024,
   employment_rgimed_2023, employment_rgimed_2024,
   rgimed_totals_2023, rgimed_totals_2024,
   employment_rgintm_2023, employment_rgintm_2024,
   rgintm_totals_2023, rgintm_totals_2024,
   employment_uf_2023, employment_uf_2024,
   uf_totals_2023, uf_totals_2024)
gc()

message(">> Employment: ", format(nrow(employment), big.mark = "."),
        " combinações (CBO4 × MUN × year)")
message(">> Municípios: ", uniqueN(employment$CO_MUN6))
message(">> Famílias CBO: ", uniqueN(employment$cbo_4dig))

# Indexar para joins rápidos
setkey(employment, year, CO_MUN6, cbo_4dig)
setkey(mun_totals, year, CO_MUN6)
setkey(br_cbo,     year, cbo_4dig)
setkey(br_total,   year)

###############################################################################
# PASSO 2: CALCULAR LOCATION QUOTIENT (LQ) E RCA
###############################################################################

message("=== Calculando LQ e RCA ===")

# Join progressivo: employment → mun total → BR cbo → BR total
X1 <- merge(employment, mun_totals, by = c("year", "CO_MUN6"),
            suffixes = c("", ".mun"))
X2 <- merge(X1, br_cbo, by = c("year", "cbo_4dig"),
            suffixes = c("", ".brcbo"))
X3 <- merge(X2, br_total, by = "year",
            suffixes = c("", ".brtot"))

setnames(X3, c("N", "N.mun", "N.brcbo", "N.brtot"),
         c("E_mun_cbo", "E_mun", "E_br_cbo", "E_br"))

dft_apl_MUN_2324 <- X3
rm(X1, X2, X3)

# Calcular índices
dft_apl_MUN_2324[, p_mun := E_mun_cbo / E_mun]
dft_apl_MUN_2324[, p_br  := E_br_cbo  / E_br]
dft_apl_MUN_2324[, LQ := p_mun / p_br]
dft_apl_MUN_2324[, RCA_squash := (LQ - 1) / (LQ + 1)]
dft_apl_MUN_2324[, logRCA := log(LQ)]
dft_apl_MUN_2324[, RCA1 := as.integer(LQ >= 1)]

message(">> dft_apl_MUN_2324: ", format(nrow(dft_apl_MUN_2324), big.mark = "."),
        " linhas")
message(">> LQ mediana: ", round(median(dft_apl_MUN_2324$LQ, na.rm = TRUE), 3))
message(">> % com RCA ≥ 1: ",
        round(100 * mean(dft_apl_MUN_2324$RCA1, na.rm = TRUE), 1), "%")

###############################################################################
# PASSO 3: DIVERSIDADE MUNICIPAL
###############################################################################

message("=== Calculando diversidade municipal ===")

dft_diversity_MUN_2324 <- dft_apl_MUN_2324[, .(
  HHI     = sum((E_mun_cbo / E_mun)^2),
  Shannon = -sum(fifelse(p_mun > 0, p_mun * log(p_mun), 0))
), by = .(year, CO_MUN6)]

message(">> Diversidade: ", format(nrow(dft_diversity_MUN_2324), big.mark = "."),
        " municípios × ano")

###############################################################################
# PASSO 4: UBIQUIDADE OCUPACIONAL
###############################################################################

message("=== Calculando ubiquidade ocupacional ===")

dft_ubiquity_CBO_2324 <- dft_apl_MUN_2324[, .(
  ubiquity = sum(RCA1, na.rm = TRUE)
), by = .(year, cbo_4dig)]

message(">> Ubiquidade: ", format(nrow(dft_ubiquity_CBO_2324), big.mark = "."),
        " ocupações × ano")

###############################################################################
# PASSO 5: COMPLEXIDADE (PROXY)
###############################################################################

message("=== Calculando complexidade ===")

# Complexidade ocupacional: média do HHI dos municípios revelados (RCA ≥ 1)
dft_occ_complexity_2324 <- dft_apl_MUN_2324[RCA1 == 1, .(
  avg_mun_div = mean(
    dft_diversity_MUN_2324[.SD, on = .(year, CO_MUN6), x.HHI],
    na.rm = TRUE
  )
), by = .(year, cbo_4dig), .SDcols = c("year", "CO_MUN6")]

# Complexidade municipal: média da ubiquidade das ocupações reveladas (RCA ≥ 1)
dft_mun_complexity_2324 <- dft_apl_MUN_2324[RCA1 == 1, .(
  ubi_mean = mean(
    dft_ubiquity_CBO_2324[.SD, on = .(year, cbo_4dig), ubiquity],
    na.rm = TRUE
  )
), by = .(year, CO_MUN6), .SDcols = c("year", "cbo_4dig")]

message(">> Complexidade ocupacional: ", format(nrow(dft_occ_complexity_2324), big.mark = "."),
        " ocupações × ano")
message(">> Complexidade municipal: ", format(nrow(dft_mun_complexity_2324), big.mark = "."),
        " municípios × ano")

###############################################################################
# SALVAR
###############################################################################

dir.create("working/rais/indices", recursive = TRUE, showWarnings = FALSE)

save(dft_apl_MUN_2324,         file = "working/rais/indices/dft_apl_MUN_2324.rda")
save(dft_diversity_MUN_2324,   file = "working/rais/indices/dft_diversity_MUN_2324.rda")
save(dft_ubiquity_CBO_2324,    file = "working/rais/indices/dft_ubiquity_CBO_2324.rda")
save(dft_occ_complexity_2324,  file = "working/rais/indices/dft_occ_complexity_2324.rda")
save(dft_mun_complexity_2324,  file = "working/rais/indices/dft_mun_complexity_2324.rda")

message("\n>> Todos os índices salvos em working/rais/indices/")
message("=== rais_apo_01b.R concluído ===")