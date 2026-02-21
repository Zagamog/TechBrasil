###############################################################################
# pnad_ocups_01b.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Processamento PNAD-C → Taxas de Formalidade por UF × CBO
#
# USO NAS ABAS: D3 (Informalidade via rais_apo_04a.R)
#
# OBJETIVO:
#   Ler os microdados PNAD-C Q2 (de pnad_ocups_01a.R), classificar
#   vínculos como formais ou informais, e agregar taxas de formalidade
#   por UF × CBO 4 dígitos × ano.
#
#   CRITÉRIOS DE FORMALIDADE (seguem a definição IBGE):
#     Formal se:
#       - Empregado setor privado COM carteira assinada
#       - Trabalhador doméstico COM carteira assinada
#       - Empregado setor público COM carteira assinada
#       - Militar ou servidor estatutário
#       - Empregador COM CNPJ (VD4019 == 1)
#       - Conta-própria COM contribuição CNPJ (VD4013 == 1)
#     Todos os demais: Informal
#
#   FILTROS APLICADOS:
#     - Escolaridade < Superior completo (VD3004 != "Superior completo")
#     - Concluiu curso técnico profissional (V3023A == "Sim" ou V3032 == "Sim")
#     Estes filtros alinham com o universo EPT (nível técnico).
#
#   PONDERAÇÃO:
#     Todas as contagens usam o peso amostral V1028 (projeção da
#     população) multiplicado pelo indicador de ocupação VD4002.
#
# INSUMOS:
#   rawdata/pnad/pnadc_q2_{ANO}.rds  (de pnad_ocups_01a.R)
#     Formato .rds — carregados via readRDS(). Não estão no S3.
#   working/ibge/cbo_matching_result4.csv  (COD PNAD → CBO 4 dígitos)
#
# SAÍDA:
#   working/pnad/df_pnadc_occ.rda
#     → ANO, NM_UF, CBO4, Ocupado, Ocupado_Formal, Ocupado_Informal,
#       Taxa_Formalidade, Projection_Rate
#
# DEPENDÊNCIAS: tidyverse (dplyr, readr, purrr, stringr)
###############################################################################

library(tidyverse)

###############################################################################
# CONFIGURAÇÃO
###############################################################################

# NOTA: O diretório de trabalho deve ser a raiz do projeto

PNAD_RAW_DIR <- "rawdata/pnad"

###############################################################################
# PASSO 0: LOOKUP CBO (COD PNAD → CBO 4 DÍGITOS)
###############################################################################
#
# A PNAD-C usa o código COD (Classificação de Ocupações para Pesquisas
# Domiciliares) na variável V4010, que difere da CBO 2002 usada na RAIS.
# O arquivo cbo_matching_result4.csv mapeia COD_4 → cbo_4dig.
###############################################################################

message("=== Processamento PNAD-C → Taxas de Formalidade ===")

cbo4 <- read_csv(
  "working/ibge/cbo_matching_result4.csv",
  col_types = cols(
    COD_4       = col_character(),
    COD_NOME    = col_character(),
    cbo_4dig    = col_character(),
    cbo_familia = col_character()
  ),
  show_col_types = FALSE
)
message(">> CBO lookup: ", nrow(cbo4), " mapeamentos COD → CBO")

###############################################################################
# PASSO 1: CARREGAR TODOS OS .RDS EM UM DATAFRAME
###############################################################################

message(">> Carregando microdados PNAD-C...")

files <- list.files(PNAD_RAW_DIR, pattern = "\\.rds$", full.names = TRUE)
message(">> Arquivos encontrados: ", length(files))

raw <- map_dfr(files, function(f) {
  message("   ", basename(f))
  readRDS(f)
})
message(">> Total: ", format(nrow(raw), big.mark = "."), " observações")

###############################################################################
# PASSO 2: PREPARAR VARIÁVEIS
###############################################################################

message(">> Preparando variáveis...")

raw2 <- raw %>%
  mutate(
    V1028  = as.numeric(V1028),
    VD4002 = as.numeric(VD4002)
  )

###############################################################################
# PASSO 3: JOIN COM LOOKUP CBO
###############################################################################

message(">> Juntando com CBO lookup...")

raw3 <- raw2 %>%
  mutate(V4010 = as.character(V4010)) %>%
  left_join(cbo4, by = c("V4010" = "COD_4")) %>%
  filter(!is.na(cbo_4dig))

message(">> Após join CBO: ", format(nrow(raw3), big.mark = "."), " obs")

###############################################################################
# PASSO 4: CLASSIFICAR FORMALIDADE E EDUCAÇÃO TÉCNICA
###############################################################################

message(">> Classificando formalidade e educação técnica...")

raw4 <- raw3 %>%
  mutate(
    # Concluiu curso técnico profissional
    concluiu_curso_tec_prof = case_when(
      V3023A == "Sim" | V3032 == "Sim" ~ 1,
      TRUE ~ 0
    ),
    # Emprego formal (definição IBGE)
    emprego_formal = case_when(
      VD4009 %in% c(
        "Empregado no setor privado com carteira de trabalho assinada",
        "Trabalhador doméstico com carteira de trabalho assinada",
        "Empregado no setor público com carteira de trabalho assinada",
        "Militar e servidor estatutário"
      ) ~ 1,
      VD4009 == "Empregador" & V4019 == 1    ~ 1,
      VD4009 == "Conta-própria" & VD4013 == 1 ~ 1,
      TRUE ~ 0
    )
  )

###############################################################################
# PASSO 5: FILTRAR UNIVERSO EPT
###############################################################################
#
# Manter apenas quem:
#   - NÃO tem superior completo
#   - Concluiu curso técnico profissional
###############################################################################

raw5 <- raw4 %>%
  filter(VD3004 != "Superior completo",
         concluiu_curso_tec_prof == 1)

message(">> Após filtro EPT: ", format(nrow(raw5), big.mark = "."), " obs")

###############################################################################
# PASSO 6: AGREGAR POR ANO × UF × CBO
###############################################################################

message(">> Agregando por ANO × UF × CBO...")

df_pnadc_occ <- raw5 %>%
  group_by(Ano, UF, cbo_4dig) %>%
  summarise(
    Ocupado          = sum(VD4002 * V1028, na.rm = TRUE),
    Ocupado_Formal   = sum(VD4002 * emprego_formal * V1028, na.rm = TRUE),
    Ocupado_Informal = Ocupado - Ocupado_Formal,
    Taxa_Formalidade = if_else(Ocupado > 0, Ocupado_Formal / Ocupado, NA_real_),
    .groups = "drop"
  ) %>%
  rename(ANO = Ano, NM_UF = UF, CBO4 = cbo_4dig)

# Arredondar
df_pnadc_occ <- df_pnadc_occ %>%
  mutate(
    across(c(Ocupado, Ocupado_Formal, Ocupado_Informal), round),
    Taxa_Formalidade = round(Taxa_Formalidade, 2)
  )

# Taxa de projeção (informal/formal) — usada como fallback em rais_apo_04a.R
df_pnadc_occ <- df_pnadc_occ %>%
  mutate(
    Projection_Rate = case_when(
      Ocupado_Informal == 0 ~ 0,
      Ocupado_Formal   == 0 ~ NA_real_,
      TRUE                  ~ Ocupado_Informal / Ocupado_Formal
    ),
    Projection_Rate = round(Projection_Rate, 2)
  )

###############################################################################
# SALVAR
###############################################################################

dir.create("working/pnad", recursive = TRUE, showWarnings = FALSE)
save(df_pnadc_occ, file = "working/pnad/df_pnadc_occ.rda")

message("\n>> df_pnadc_occ: ", format(nrow(df_pnadc_occ), big.mark = "."), " linhas")
message(">> Anos: ", paste(sort(unique(df_pnadc_occ$ANO)), collapse = ", "))
message(">> UFs: ", n_distinct(df_pnadc_occ$NM_UF))
message(">> CBOs: ", n_distinct(df_pnadc_occ$CBO4))
message(">> Salvo: working/pnad/df_pnadc_occ.rda")
message("=== pnad_ocups_01b.R concluído ===")