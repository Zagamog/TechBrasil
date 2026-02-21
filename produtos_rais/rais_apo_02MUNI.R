###############################################################################
# rais_apo_02MUNI.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Arranjos Produtivos Locais (APLs) Persistentes — Nível Municipal
#
# USO NAS ABAS: D2 (APLs — carregado pelo Shiny e por rais_apo_03MUNI.R)
#
# OBJETIVO:
#   Identificar APLs persistentes ao nível municipal, definidos como
#   combinações (município × família CBO) que atendem AMBOS os critérios
#   em AMBOS os anos (2023 e 2024):
#     - Emprego ≥ 100 vínculos na ocupação no município
#     - Location Quotient (LQ) ≥ 1.25
#
#   O critério de persistência (presente em ambos os anos) filtra
#   concentrações espúrias ou transitórias, identificando apenas
#   especializações econômicas estáveis.
#
#   A saída contém métricas de 2024 (ano mais recente) para os APLs
#   que passaram no filtro de persistência.
#
# INSUMOS (de rais_apo_01b.R):
#   working/rais/indices/dft_apl_MUN_2324.rda
#
# INSUMOS ADICIONAIS:
#   working/ibge/df_codes_ibge.rda  (labels geográficos)
#   working/qbq/qbq_ocup_cmento1.rda  (labels CBO família)
#
# SAÍDA:
#   working/rais/indices/dft_apl_MUN_final.rda
#     → CO_MUN6, cbo_4dig, cbo_familia, SG_UF, NM_UF,
#       NM_RGIMED, NM_RGIINTM, E_mun_cbo, LQ, persist
#
# DEPENDÊNCIAS: data.table
###############################################################################

library(data.table)

###############################################################################
# CARREGAR DADOS
###############################################################################

message("=== APLs Municipais — Filtro de Persistência ===")

load("working/rais/indices/dft_apl_MUN_2324.rda")

# Geo lookup para labels
load("working/ibge/df_codes_ibge.rda")
dft_geo_keys <- unique(
  as.data.table(df_codes_ibge)[
    , .(CO_MUN6, SG_UF, NM_UF, NM_RGIMED, NM_RGIINTM)
  ],
  by = "CO_MUN6"
)

# CBO família lookup
load("working/qbq/qbq_ocup_cmento1.rda")
cbo_lookup <- unique(
  as.data.table(qbq_ocup_cmento1)[
    !is.na(cbo_4dig) & !is.na(cbo_familia),
    .(cbo_4dig, cbo_familia)
  ],
  by = "cbo_4dig"
)

###############################################################################
# FILTRO: E ≥ 100 E LQ ≥ 1.25 EM AMBOS OS ANOS
###############################################################################

# Marcar elegíveis por ano
dft_apl_MUN_2324[, eligible := (E_mun_cbo >= 100 & LQ >= 1.25)]

message(">> Elegíveis 2023: ",
        format(sum(dft_apl_MUN_2324[year == 2023]$eligible), big.mark = "."))
message(">> Elegíveis 2024: ",
        format(sum(dft_apl_MUN_2324[year == 2024]$eligible), big.mark = "."))

# Persistência: presente em AMBOS os anos
dft_apl_MUN_persistent <- dft_apl_MUN_2324[
  eligible == TRUE,
  .(persist = .N),
  by = .(CO_MUN6, cbo_4dig)
][persist == 2]

message(">> APLs persistentes: ", format(nrow(dft_apl_MUN_persistent), big.mark = "."))

###############################################################################
# TABELA FINAL COM MÉTRICAS 2024
###############################################################################

dft_apl_MUN_final <- merge(
  dft_apl_MUN_persistent,
  dft_apl_MUN_2324[year == 2024, .(CO_MUN6, cbo_4dig, E_mun_cbo, LQ)],
  by = c("CO_MUN6", "cbo_4dig"),
  all.x = TRUE
)

# Adicionar labels geográficos
dft_apl_MUN_final <- merge(dft_apl_MUN_final, dft_geo_keys,
                           by = "CO_MUN6", all.x = TRUE)

# Adicionar labels CBO família
dft_apl_MUN_final <- merge(dft_apl_MUN_final, cbo_lookup,
                           by = "cbo_4dig", all.x = TRUE)

# Ordenar
setorder(dft_apl_MUN_final, SG_UF, NM_UF, -E_mun_cbo)

###############################################################################
# SALVAR
###############################################################################

save(dft_apl_MUN_final, file = "working/rais/indices/dft_apl_MUN_final.rda")

message(">> dft_apl_MUN_final: ", format(nrow(dft_apl_MUN_final), big.mark = "."),
        " APLs persistentes")
message(">> UFs: ", uniqueN(dft_apl_MUN_final$SG_UF))
message(">> Municípios: ", uniqueN(dft_apl_MUN_final$CO_MUN6))
message(">> Famílias CBO: ", uniqueN(dft_apl_MUN_final$cbo_4dig))
message(">> Salvo: working/rais/indices/dft_apl_MUN_final.rda")
message("=== rais_apo_02MUNI.R concluído ===")