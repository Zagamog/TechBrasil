###############################################################################
# rais_apo_02UF.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Arranjos Produtivos Locais (APLs) Persistentes — Nível UF
#
# USO NAS ABAS: D2 (APLs — carregado pelo Shiny e por rais_apo_03UF.R)
#
# OBJETIVO:
#   Agregar os índices municipais ao nível estadual (UF) e identificar
#   APLs persistentes, definidos como combinações (UF × família CBO) que
#   atendem AMBOS os critérios em AMBOS os anos (2023 e 2024):
#     - Emprego ≥ 500 vínculos na ocupação na UF
#     - Location Quotient (LQ) ≥ 1
#
#   Os limiares são mais brandos que no nível municipal (E≥100, LQ≥1.25)
#   porque a agregação estadual naturalmente dilui especializações locais.
#
# INSUMOS (de rais_apo_01b.R):
#   working/rais/indices/dft_apl_MUN_2324.rda
#
# INSUMOS ADICIONAIS:
#   working/ibge/df_codes_ibge.rda  (para SG_UF por município)
#   working/qbq/qbq_ocup_cmento1.rda  (labels CBO família)
#
# SAÍDA:
#   working/rais/indices/dft_apl_UF_final.rda
#     → SG_UF, cbo_4dig, cbo_familia, E_uf_cbo, LQ, persist
#
# DEPENDÊNCIAS: data.table, dplyr
###############################################################################

library(data.table)
library(dplyr)

###############################################################################
# CARREGAR DADOS
###############################################################################

message("=== APLs Estaduais — Agregação e Filtro de Persistência ===")

load("working/rais/indices/dft_apl_MUN_2324.rda")

# Geo lookup para SG_UF
load("working/ibge/df_codes_ibge.rda")
dft_geo_keys <- unique(
  as.data.table(df_codes_ibge)[, .(CO_MUN6, SG_UF, NM_UF)],
  by = "CO_MUN6"
)

###############################################################################
# AGREGAR AO NÍVEL UF
###############################################################################

message(">> Agregando municípios → UF...")

# Juntar SG_UF aos dados municipais
dft_apl_UF <- merge(dft_apl_MUN_2324, dft_geo_keys, by = "CO_MUN6", all.x = TRUE)

# Agregar por UF × CBO × ano
dft_apl_UF <- dft_apl_UF[, .(
  E_uf_cbo = sum(E_mun_cbo, na.rm = TRUE),
  E_uf     = sum(E_mun, na.rm = TRUE),
  E_br_cbo = unique(E_br_cbo),
  E_br     = unique(E_br)
), by = .(year, SG_UF, cbo_4dig)]

# Recalcular LQ ao nível UF
dft_apl_UF[, p_uf := E_uf_cbo / E_uf]
dft_apl_UF[, p_br := E_br_cbo / E_br]
dft_apl_UF[, LQ := p_uf / p_br]
dft_apl_UF[, RCA1 := as.integer(LQ >= 1)]

message(">> Combinações UF × CBO: ", format(nrow(dft_apl_UF), big.mark = "."))

###############################################################################
# FILTRO: E ≥ 500 E LQ ≥ 1 EM AMBOS OS ANOS
###############################################################################

dft_apl_UF[, eligible := (E_uf_cbo >= 500 & LQ >= 1)]

message(">> Elegíveis 2023: ",
        sum(dft_apl_UF[year == 2023]$eligible))
message(">> Elegíveis 2024: ",
        sum(dft_apl_UF[year == 2024]$eligible))

# Persistência
dft_apl_UF_persistent <- dft_apl_UF[
  eligible == TRUE,
  .(persist = .N),
  by = .(SG_UF, cbo_4dig)
][persist == 2]

message(">> APLs persistentes (UF): ", nrow(dft_apl_UF_persistent))

###############################################################################
# TABELA FINAL COM MÉTRICAS 2024
###############################################################################

dft_apl_UF_final <- merge(
  dft_apl_UF_persistent,
  dft_apl_UF[year == 2024, .(SG_UF, cbo_4dig, E_uf_cbo, LQ)],
  by = c("SG_UF", "cbo_4dig"),
  all.x = TRUE
)

# Adicionar labels CBO família
load("working/qbq/qbq_ocup_cmento1.rda")
cbo_lookup <- unique(
  as.data.table(qbq_ocup_cmento1)[
    !is.na(cbo_4dig) & !is.na(cbo_familia),
    .(cbo_4dig, cbo_familia)
  ],
  by = "cbo_4dig"
)
dft_apl_UF_final <- merge(dft_apl_UF_final, cbo_lookup,
                          by = "cbo_4dig", all.x = TRUE)

# Ordenar
dft_apl_UF_final <- dft_apl_UF_final %>% arrange(SG_UF, desc(E_uf_cbo))

###############################################################################
# SALVAR
###############################################################################

save(dft_apl_UF_final, file = "working/rais/indices/dft_apl_UF_final.rda")

message(">> dft_apl_UF_final: ", nrow(dft_apl_UF_final), " APLs persistentes")
message(">> UFs: ", uniqueN(dft_apl_UF_final$SG_UF))
message(">> Famílias CBO: ", uniqueN(dft_apl_UF_final$cbo_4dig))
message(">> Salvo: working/rais/indices/dft_apl_UF_final.rda")
message("=== rais_apo_02UF.R concluído ===")