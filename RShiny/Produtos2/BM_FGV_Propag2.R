###############################################################################
# BM_FGV_Propag2.R
# PROPAG/Juros por Educação — Plataforma Analítica Interativa (Shiny App)
#
# OBJETIVO:
#   Dashboard Shiny para análise estadual no contexto do PROPAG
#   (Programa de Equalização Federativa) e da iniciativa "Juros por Educação".
#   Ferramentas integradas para análise demográfica, financeira, de oferta
#   educacional, demanda laboral e correspondência oferta-demanda, apoiando
#   gestores estaduais na elaboração de Planos de Aplicação baseados em evidências.
#
# ESTRUTURA (15 abas, 5 seções temáticas):
#   A. Demografia (Azul)
#      A1. Transição Demográfica — Projeções populacionais IBGE (2000–2070)
#      A2. EPT e População — Pop. 15-19 anos vs matrículas EPT/EM
#   B. Finanças (Laranja)
#      B1. Impacto Financeiro PROPAG — Cenários de dívida com/sem adesão
#      B2. Simulação FEF — Fluxos de contribuição/retorno do Fundo de Equalização
#      B3. Finanças Comparativas — Comparação multi-UF
#   C. Oferta EPT (Verde)
#      C1. Oferta EPT (Futuro) — Matrículas vs Meta 11 do PNE
#      C2. Oferta EPT (Planificação) — Crescimento necessário para atingir metas
#      C3. Oferta EPT (Redes) — Censo Escolar detalhado por eixo/curso/rede
#      C4. Oferta EPT (Modelo) — Modelo econométrico de resíduos
#   D. Demanda EPT (Verde-Lima)
#      D1. Dinamismo Econômico — Índice de crescimento do PIB municipal
#      D2. APLs — Arranjos Produtivos Locais via especialização CBO
#      D3. Informalidade — Emprego formal/informal por município
#   E. Conexão Oferta-Demanda (Roxo)
#      E1. Oferta e Demanda — Correspondência CNCT-CBO por similaridade semântica
#      E2. Escassez de Técnicos — Indicadores CAGED/RAIS do mercado de trabalho
#
# FONTES DE DADOS:
#   Projeções demográficas IBGE, Censo Escolar (INEP), RAIS/CAGED (MTE),
#   PNAD-C (IBGE), CNCT (MEC), Classificação CBO, PIB municipal (IBGE)
#
# DEPENDÊNCIAS:
#   shiny, shinydashboard, shinyBS, tidyverse, DT, ggtext, scales, lubridate,
#   patchwork, RColorBrewer, shinyjs, shinyWidgets, shinycssloaders, sf,
#   leaflet, data.table, plotly, colorspace
#
# MANUTENÇÃO: Equipe de assistência técnica Banco Mundial / FGV
# ÚLTIMA ATUALIZAÇÃO: 2025
###############################################################################

library(shiny)
library(shinydashboard)
library(shinyBS)   # Componentes de tooltip para aba demanda/escassez
library(tidyverse)
library(DT)
library(ggtext)
library(scales)
library(lubridate)
library(patchwork)
library(RColorBrewer)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(sf)
library(leaflet)
library(data.table)
library(plotly)
library(colorspace)


options(warn=-1, dplyr.summarise.inform = FALSE) # Suprime avisos não-críticos para saída mais limpa no console


###############################################################################
# SEÇÃO A1: CARREGAMENTO DE DADOS — TRANSIÇÃO DEMOGRÁFICA
# Fonte: Projeções populacionais IBGE (pop01_70b.rda)
# Utilizado pela Aba A1 (Transição Demográfica)
###############################################################################

# Carregar projeções demográficas IBGE (2000-2070) por faixa etária e geografia
load("pop01_70b.rda")

# Definir grupos de colunas para dados demográficos
demo_number_columns <- c("POP_T", "0-14_T", "15-17_T", "18-21_T", "15-59_T", "60+_T")
demo_proportion_columns <- c("P_0_14_T", "P_15_17_T", "P_18_21_T", "P_15_59_T", "P_60_plus_T")

# Paleta de cores predefinida para localidades demográficas
demo_local_colors <- c(
  "Brasil" = "#1f77b4", "Norte" = "#ff7f0e", "Nordeste" = "#2ca02c",
  "Sudeste" = "#d62728", "Sul" = "#9467bd", "Centro-Oeste" = "#8c564b",
  "Amazonia_Legal" = "#8c564b", "Nordeste_r" = "#e377c2", "Centro-Oeste_r" = "#7f7f7f",
  "Acre" = "#1b9e77", "Amapá" = "#d95f02", "Amazonas" = "#7570b3",
  "Pará" = "#e7298a", "Rondônia" = "#66a61e", "Roraima" = "#e6ab02",
  "Tocantins" = "#a6761d", "Alagoas" = "#1f77b4", "Bahia" = "#ff7f0e",
  "Ceará" = "#2ca02c", "Maranhão" = "#d62728", "Paraíba" = "#9467bd",
  "Pernambuco" = "#8c564b", "Piauí" = "#e377c2", "Rio Grande do Norte" = "#7f7f7f",
  "Sergipe" = "#bcbd22", "Espírito Santo" = "#17becf", "Minas Gerais" = "#ff9896",
  "Rio de Janeiro" = "#c5b0d5", "São Paulo" = "#c49c94", "Paraná" = "#8c564b",
  "Rio Grande do Sul" = "#e377c2", "Santa Catarina" = "#7f7f7f",
  "Distrito Federal" = "#bcbd22", "Goiás" = "#17becf",
  "Mato Grosso" = "#ff7f0e", "Mato Grosso do Sul" = "#2ca02c"
)

###############################################################################
# SEÇÃO A2: CARREGAMENTO DE DADOS — MATRÍCULAS EPT E POPULAÇÃO
# Fontes: meta11a_opcoes.rda (Censo Escolar), pop01_70b.rda (IBGE)
# Utilizado pela Aba A2 (EPT e População)
###############################################################################


# Carregar dados adicionais para análise EPT
load("meta11a_opcoes.rda")
load("df_codes_ibge.rda")

# Obter mapeamentos regionais a partir dos geocódigos
geocodes <- df_codes_ibge %>% 
  select(SG_UF, CO_5RGRANDE, NM_5RGRANDE) %>% 
  distinct()

# Definir estados da Amazônia Legal
amazonia_legal <- c("AC", "AP", "AM", "MA", "MT", "PA", "RO", "RR", "TO")

# Selecionar colunas necessárias dos dados populacionais e construir faixa 15-19 anos
# (coorte completa 15-17 + metade da coorte 18-21 como proxy para 18-19)
ept_pop_data <- pop01_70b %>%
  select(ANO, SIGLA, LOCAL, `15-17_T`, `18-21_T`) %>%
  mutate(`15-19_T` = `15-17_T` + (`18-21_T` * 0.5)) %>%
  filter(ANO >= 2007 & ANO <= 2035) %>%
  # Excluir construtos geográficos residuais (Centro-Oeste_r, Nordeste_r)
  filter(!LOCAL %in% c("Centro-Oeste_r", "Nordeste_r"))

# Selecionar colunas necessárias dos dados de matrícula
enrollment_data_states <- meta11a_opcoes %>%
  select(ANO, SG_UF, NM_UF, QT_MAT_PROF_TEC_PROPAG, QT_MAT_MED) %>%
  filter(ANO >= 2007 & ANO <= 2035)

# Criar agregados regionais IBGE
enrollment_data_regions <- enrollment_data_states %>%
  left_join(geocodes, by = "SG_UF") %>%
  filter(!is.na(NM_5RGRANDE)) %>%
  group_by(ANO, NM_5RGRANDE) %>%
  summarise(
    QT_MAT_PROF_TEC_PROPAG = sum(QT_MAT_PROF_TEC_PROPAG, na.rm = TRUE),
    QT_MAT_MED = sum(QT_MAT_MED, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    SG_UF = case_when(
      NM_5RGRANDE == "Norte" ~ "NO",
      NM_5RGRANDE == "Nordeste" ~ "ND", 
      NM_5RGRANDE == "Sudeste" ~ "SD",
      NM_5RGRANDE == "Sul" ~ "SU",
      NM_5RGRANDE == "Centro-Oeste" ~ "CO",
      TRUE ~ NM_5RGRANDE
    ),
    NM_UF = NM_5RGRANDE
  ) %>%
  select(-NM_5RGRANDE)

# Criar agregado da Amazônia Legal
enrollment_data_amazonia <- enrollment_data_states %>%
  filter(SG_UF %in% amazonia_legal) %>%
  group_by(ANO) %>%
  summarise(
    QT_MAT_PROF_TEC_PROPAG = sum(QT_MAT_PROF_TEC_PROPAG, na.rm = TRUE),
    QT_MAT_MED = sum(QT_MAT_MED, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    SG_UF = "AML",
    NM_UF = "Amazonia_Legal"
  )

# Combinar dados de matrícula estaduais, regionais e da Amazônia Legal
enrollment_data <- enrollment_data_states %>%
  bind_rows(enrollment_data_regions) %>%
  bind_rows(enrollment_data_amazonia)

# Unir os conjuntos de dados
ept_combined_data <- ept_pop_data %>%
  left_join(enrollment_data, by = c("SIGLA" = "SG_UF", "ANO" = "ANO")) %>%
  mutate(LOCAL = ifelse(is.na(NM_UF), LOCAL, NM_UF)) %>%
  mutate(
    PCT_MAT_EPT = ifelse(`15-19_T` > 0, (QT_MAT_PROF_TEC_PROPAG / `15-19_T`) * 100, 0),
    PCT_MAT_MED = ifelse(`15-19_T` > 0, (QT_MAT_MED / `15-19_T`) * 100, 0)
  ) %>%
  select(-NM_UF, -`15-17_T`, -`18-21_T`)

# Definir variáveis disponíveis para visualização
ept_number_variables <- c(
  "Pop 15-19" = "15-19_T",
  "Matrícula EPT" = "QT_MAT_PROF_TEC_PROPAG",
  "Matrícula Ensino Médio" = "QT_MAT_MED"
)

ept_percentage_variables <- c(
  "% Matrícula EPT" = "PCT_MAT_EPT",
  "% Matrícula Ensino Médio" = "PCT_MAT_MED"
)

# Calcular metas do Meta 11
ept_location_order <- c(
  "Brasil",
  "Norte", "Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins",
  "Nordeste", "Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe",
  "Sudeste", "Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo", 
  "Sul", "Paraná", "Rio Grande do Sul", "Santa Catarina",
  "Centro-Oeste", "Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul"
)

ept_meta11_df <- ept_combined_data %>%
  filter(ANO == 2013) %>%
  group_by(LOCAL) %>%
  summarise(
    meta11_absolute = sum(QT_MAT_PROF_TEC_PROPAG, na.rm = TRUE) * 3,
    .groups = 'drop'
  ) %>%
  arrange(match(LOCAL, ept_location_order))

# Cores EPT por localidade (reutiliza demo_local_colors com adições)
ept_local_colors <- c(
  demo_local_colors,
  "Amazonia_Legal" = "#8B4513"
)

# Nomes amigáveis para variáveis EPT
ept_vars_friendly <- c(
  "Matrículas EPT para PROPAG - Integrado, Concomitante ou Subseqente" = "QT_MAT_PROF_TEC_PROPAG",
  "Matrículas Técnicas - Integrado e Concomitante" = "QT_MAT_TEC_NUM2", 
  "Matrículas Técnicas - só Integrado" = "QT_MAT_TEC_NUM3"
)

# Nomes amigáveis para opções da Meta 11a
meta11a_choices_friendly <- c(
  "Opção 1: Integrado, Concomitante, Subsequente" = "Meta11a_opcao1",
  "Opção 2: Integrado e Concomitante" = "Meta11a_opcao2",
  "Opção 3: Integrado" = "Meta11a_opcao3"
)

###############################################################################
# SEÇÃO B: CARREGAMENTO DE DADOS — CENÁRIOS FINANCEIROS
# Fontes: propag_ept_financeiro.rds (dados extraídos do PROPAG),
#          df_censo_UF.rda, meta11a_opcoes.rda, df_pibmunis.rda,
#          df_2a/b/c, df_3a/b/c, df_4a/b, df_nd (datasets por cenário)
# Utilizado pelas Abas B1 (Financiamento PROPAG), B2 (Retorno FEF), B3 (Comparativo)
###############################################################################

# Carregar dados extraídos do PROPAG
propag_ept_financeiro <- readRDS("propag_ept_financeiro.rds")

# Carregar dados do Censo Escolar 2007 a 2024, agregados por UF
load("df_censo_UF.rda")
sg_ufs <- sort(unique(na.omit(df_censo_UF$SG_UF)))

# Carregar cálculos da Meta 11a do PNE (gerados por Censo_UF_garabed1b.R)
load("meta11a_opcoes.rda")  
load("df_pibmunis.rda") 
load("df_residuals_ols.rda")
# Obter nomes dos estados para exibição
nome_ufs <- sort(unique(df_censo_UF$NM_UF))  # Garantir ordenado e único

# Carregar combinações válidas de cenários (colunas A, G, I, J definem opções PROPAG)
load("dfcen_val.rda")

# Carregar projeções financeiras pré-calculadas por opção de cenário PROPAG
load("df_2a.rda")
load("df_2b.rda")
load("df_2c.rda")

load("df_3a.rda")
load("df_3b.rda")
load("df_3c.rda")

load("df_4a.rda")
load("df_4b.rda")

# Carregar projeção financeira para cenário de não-adesão ("Não Adere")
load("df_nd.rda")

# Carregar dados CAGED/RAIS do mercado de trabalho vinculados a cursos técnicos CNCT
load("caged_rais_cnct_2020_2024_shiny.rda")
load("df_ranking_cursos_caged_rais.rda") # Tabela pré-calculada de ranking de cursos para análise de escassez


# Mapeamento dos nomes de opções PROPAG para seus respectivos data frames pré-calculados
df_list <- list(
  "II-A" = df_2a, "II-B" = df_2b, "II-C" = df_2c,
  "III-A"= df_3a, "III-B"= df_3b, "III-C"= df_3c,
  "IV-A" = df_4a, "IV-B" = df_4b,
  "ND"   = df_nd
)


df_choices <- tibble::tribble(
  ~opcao,   ~amort,            ~fef,    ~inv,     ~juros,
  "II-A",   "20% abatimento",  "1%",    "1%",     "0%",
  "II-B",   "10% abatimento",  "1,5%",  "1,5%",   "0%",
  "II-C",   "Sem abatimento",  "2%",    "2%",     "0%",
  "III-A",  "20% abatimento",  "1%",    "0%",     "1%",
  "III-B",  "10% abatimento",  "1,5%",  "0,5%",   "1%",
  "III-C",  "Sem abatimento",  "2%",    "1%",     "1%",
  "IV-A",   "10% abatimento",  "1%",    "0%",     "2%",
  "IV-B",   "Sem abatimento",  "1,5%",  "0,5%",   "2%",
  "ND",     " ",              "NA",    "NA",     "4% (Não Adere)"
)

named_scenarios <- setNames(
  df_choices$opcao,
  paste0(
    df_choices$opcao, ": ",
    df_choices$amort, ", ",
    df_choices$fef, " FEF, ",
    df_choices$inv, " Invest., ",
    df_choices$juros, " Juros"
  )
)


opcoes <- tolower(gsub("-", "", gsub("\\.", "", df_choices$opcao)))

op_labels <- setNames(
  paste0(df_choices$opcao, ": ", df_choices$juros, " Jur., ",
         df_choices$amort, ", ", df_choices$fef, " FEF, ", df_choices$inv, " Inv."),
  opcoes
)

# Atribuir cores fixas às UFs para visualização consistente entre abas
# Mapeamento UF-cor usando paleta Set1

uf_levels <- sort(unique(df_2a$NM_UF))  # ou extrair de qualquer um deles

# Cores por NM_UF
uf_colors <- setNames(
  colorRampPalette(brewer.pal(9, "Set1"))(length(uf_levels)),
  uf_levels
)

# Criar mapeamento de sigla (SG_UF) para nome completo (NM_UF)
uf_name_map <- df_censo_UF %>%
  distinct(UF = SG_UF, Estado = NM_UF) %>%
  filter(Estado %in% names(uf_colors))  # para garantir que o nome tem cor

# Criar vetor de cores indexado por SG_UF (sigla) em vez de NM_UF (nome completo)
uf_colors_bySG <- setNames(
  uf_colors[uf_name_map$Estado],
  uf_name_map$UF
)

uf_colors_compare <- map_chr(uf_colors, ~ lighten(.x, amount = 0.4))

# Definir opções de variáveis de matrícula EPT para abas de Oferta EPT
ept_vars <- c("QT_MAT_PROF_TEC_PROPAG", "QT_MAT_TEC_NUM2", "QT_MAT_TEC_NUM3" )
other_vars <- c("QT_MAT_MED")



# Operador null-coalescing: retorna o primeiro valor não-nulo de um par

`%||%` <- function(a, b) if (!is.null(a)) a else b


# Rótulos legíveis para variáveis financeiras exibidas na Aba B1
var_labels <- list(
  "saldo_mar25"           = "Saldo março de 2025",
  "amort_extr"            = "Amortizações extraordinárias - 20 % do saldo",
  "FEF_1ano_liq_cen01"    = "Fundo FEF – fluxo líquido 1 ano – cenário I",
  "FEF_1ano_liq_cen02"    = "Fundo FEF – fluxo líquido 1 ano – cenário II",
  "FEF_5ano_liq_cen01"    = "Fundo FEF – fluxo líquido 5 anos – cenário I",
  "FEF_5ano_liq_cen02"    = "Fundo FEF – fluxo líquido 5 anos – cenário II",
  "EPT_1ano_cen01"        = "Investimento EPT – 1 ano – cenário I",
  "EPT_1ano_cen02"        = "Investimento EPT – 1 ano – cenário II",
  "EPT_5ano_cen01"        = "Investimento EPT – 5 anos – cenário I",
  "EPT_5ano_cen02"        = "Investimento EPT – 5 anos – cenário II"
)

# Definir variáveis permitidas e sua ordem
allowed_vars <- c(
  "saldo_mar25",
  "amort_extr",
  "EPT_1ano_cen01",
  "EPT_1ano_cen02",
  "EPT_5ano_cen01",
  "EPT_5ano_cen02",
  "FEF_1ano_liq_cen01",
  "FEF_1ano_liq_cen02",
  "FEF_5ano_liq_cen01",
  "FEF_5ano_liq_cen02"
)

# Criar vetor nomeado: rótulo = nome_variável
fin_choices <- setNames(allowed_vars, sapply(allowed_vars, function(v) var_labels[[v]] %||% v))


# Preparar dados financeiros para exibição em DataTable na Aba B3
# Remove coluna fef_share_pct, converte números em formato brasileiro para numérico,
# e adiciona linha de total ao final

financeiro_dt_all <- {
  df <- propag_ept_financeiro
  df <- df[, !(names(df) %in% "fef_share_pct")]
  data_cols <- setdiff(names(df), c("UF", "Estado"))
  df[data_cols] <- lapply(df[data_cols], function(x) suppressWarnings(as.numeric(gsub(",", "", x))))
  
  total_row <- as.list(rep(NA, ncol(df)))
  names(total_row) <- names(df)
  for (col in data_cols) {
    total_row[[col]] <- sum(df[[col]], na.rm = TRUE)
  }
  total_row$UF <- "Todos"
  total_row$Estado <- "Todos"
  
  df_final <- rbind(df, as.data.frame(total_row, stringsAsFactors = FALSE))
  df_final
}

# Função utilitária: média móvel por janela para suavização de séries temporais (análise de escassez)
window_mean <- function(vec, lag = 6, lead = 5) {
  n <- length(vec)
  sapply(seq_along(vec), function(i) {
    window <- vec[max(1, i - lag):min(n, i + lead)]
    mean(window, na.rm = TRUE)
  })
}
plot_caged_summary_double <- function(df, geo_value, curso_values, todos_no_eixo, eixo_values) {
  color_group <- if (todos_no_eixo) {
    "Eixo_Tecnologico"
  } else {
    "Curso"
  }
  
  df_filtered <- df %>%
    filter(NM_UF %in% geo_value) %>%
    {
      if (todos_no_eixo) {
        group_by(., ANO, MES, NM_UF, Eixo_Tecnologico) %>%
          summarise(media_sal_adm = weighted.mean(media_sal_adm, w = soma_mov_adm, na.rm = TRUE),
                    media_sal_des = weighted.mean(media_sal_des, w = soma_mov_des, na.rm = TRUE),
                    dif_sal_adm_des = media_sal_adm - media_sal_des,
                    dif_sal_adm_des_pc = (media_sal_adm - media_sal_des) * 100 / media_sal_des,
                    across(c(contains("soma_mov"), total_vinculo_ativo_3112, estoque_liquido), ~ sum(.x, na.rm = TRUE)),
                    rotatividade = soma_mov_adm + soma_mov_des,
                    tx_rotatividade = rotatividade / estoque_liquido) %>%
          # Agrupar por UF e Eixo para cálculos de janela subsequentes
          group_by(NM_UF, Eixo_Tecnologico)
      } else {
        group_by(., ANO, MES, NM_UF, Curso) %>%
          summarise(media_sal_adm = weighted.mean(media_sal_adm, w = soma_mov_adm, na.rm = TRUE),
                    media_sal_des = weighted.mean(media_sal_des, w = soma_mov_des, na.rm = TRUE),
                    dif_sal_adm_des = media_sal_adm - media_sal_des,
                    dif_sal_adm_des_pc = (media_sal_adm - media_sal_des) * 100 / media_sal_des,
                    across(c(contains("soma_mov"), total_vinculo_ativo_3112, estoque_liquido), ~ sum(.x, na.rm = TRUE)),
                    rotatividade = soma_mov_adm + soma_mov_des,
                    tx_rotatividade = rotatividade / estoque_liquido,
                    across(.cols = c(dif_sal_adm_des_pc_m12, tx_rotatividade_m12, estoque_liquido),
                           .fns = ~ percent_rank(.x)*100, 
                           .names = 'p_{.col}')) %>%
          # Agrupar por UF e Curso para cálculos de janela subsequentes
          group_by(NM_UF, Curso)
      }
    } %>%
    mutate(
      across(c(dif_sal_adm_des_pc, tx_rotatividade), ~ if_else(is.infinite(.x), NA, .x)),
      across(c(dif_sal_adm_des_pc, tx_rotatividade), ~ window_mean(.x), .names = "{.col}_m12")
    ) %>%
    {
      if (todos_no_eixo) {
        group_by(., ANO, MES, NM_UF) %>%
          mutate(across(.cols = c(dif_sal_adm_des_pc_m12, tx_rotatividade_m12, estoque_liquido),
                        .fns = ~ percent_rank(.x)*100, 
                        .names = 'p_{.col}')) %>%
          filter(., Eixo_Tecnologico %in% eixo_values) %>%
          group_by(., NM_UF, Eixo_Tecnologico)
      } else {
        group_by(., ANO, MES, NM_UF) %>%
          mutate(across(.cols = c(dif_sal_adm_des_pc_m12, tx_rotatividade_m12, estoque_liquido),
                        .fns = ~ percent_rank(.x)*100, 
                        .names = 'p_{.col}')) %>%
          filter(., Curso %in% curso_values) %>%
          group_by(., NM_UF, Curso)
      }
    } %>%
    mutate(soma_mov_adm_media_ano = window_mean(soma_mov_adm, lag = 11, lead = 0)) %>%
    ungroup() %>%
    mutate(
      data_ordem = ymd(sprintf("%04d-%02d-01", ANO, MES)),
      color_label = paste(.data[[color_group]], "-", NM_UF),
      linetype_group = if (length(geo_value) > 1) if_else(NM_UF == geo_value[2], "Comparação", "Principal") else "Principal"
    ) %>%
    filter(!is.na(dif_sal_adm_des_pc_m12) | !is.na(tx_rotatividade_m12))
  
  df_escassez <- df_filtered %>%
    filter(ANO == max(ANO, na.rm = TRUE)) %>%
    filter(MES == max(MES, na.rm = TRUE)) %>%
    {
      if (todos_no_eixo) {
        group_by(., Eixo_Tecnologico)
      } else {
        group_by(., Curso)
      }
    } %>%
    mutate(
      tipologia_escassez = case_when(
        p_dif_sal_adm_des_pc_m12 >= 75 & p_tx_rotatividade_m12 >= 50 & p_estoque_liquido >= 60 ~ "Alerta de Escassez",
        (p_dif_sal_adm_des_pc_m12 >= 75 & p_tx_rotatividade_m12 >= 50 & p_estoque_liquido < 60) |
          (between(p_dif_sal_adm_des_pc_m12, 74.9999, 50) & p_tx_rotatividade_m12 >= 50 & p_estoque_liquido >= 50) ~ "Tendência de Escassez",
        (between(p_dif_sal_adm_des_pc_m12, 49.999, 25) | p_estoque_liquido >= 50) ~ "Situação Estável",
        p_dif_sal_adm_des_pc_m12 < 25 ~ "Sinal de Excesso",
        soma_mov_adm_media_ano < 30 ~ "Sem fluxo suficiente - INDICADORES DEIXAM DE SER INFORMATIVOS",
        TRUE ~ "Indisponível/Indeterminado"
      )
    )
  
  list(
    dif_sal_pc_plot = ggplot(df_filtered, aes(x = data_ordem, y = dif_sal_adm_des_pc_m12, color = color_label, linetype = linetype_group)) +
      geom_line(size = 1) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      scale_y_continuous(labels = label_number(suffix = "%", decimal_mark = ",", big.mark = ".")) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
      scale_linetype_manual(values = c("Principal" = "solid", "Comparação" = "dotdash")) +
      labs(
        x = "Ano-Mês", y = "Diferença salarial Admitidos e Desligados (%)",
        color = "Legenda", linetype = "Seleção",
        title = "(Salário adm - Salário des)/Salário des",
        caption = "Fonte: CAGED / Elaboração equipe Banco Mundial e FGV"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)),
    
    rotatividade_plot = ggplot(df_filtered, aes(x = data_ordem, y = tx_rotatividade_m12, color = color_label, linetype = linetype_group)) +
      geom_line(size = 1) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      scale_y_continuous(labels = label_number(decimal_mark = ",", big.mark = ".")) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
      scale_linetype_manual(values = c("Principal" = "solid", "Comparação" = "dotdash")) +
      labs(
        x = "Ano-Mês", y = "Taxa de rotatividade",
        color = "Legenda", linetype = "Seleção",
        title = "(Admitidos + Desligados) / Estoque",
        caption = "Fonte: CAGED e RAIS / Elaboração equipe Banco Mundial e FGV"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)),
    
    tipologia_escassez = df_escassez,
    df_filtered = df_filtered
  )
}

###############################################################################
# SEÇÃO D2: CARREGAMENTO DE DADOS — ARRANJOS PRODUTIVOS LOCAIS (APLs)
# Fontes: dft_apl_MUN_final.rda, df_codes_ibge.rda, qbq_ocup_cmento1.rda,
#          sf_regioes.gpkg, apl_matri_UF/MUN.rda
# Utilizado pela Aba D2 (Demanda EPT: APLs)
###############################################################################

load("dft_apl_MUN_final.rda")
# dft_apl_MUN_final contém: CO_MUN6, cbo_4dig, E_mun_cbo, LQ, cbo_familia, persist, QL_2023, QL_2024, E_cm_2023, E_cm_2024
load("df_codes_ibge.rda") 
# df_codes_ibge contém: CO_MUN, CO_MUN6, SG_UF, NM_UF, NM_MUN, CO_UF, CO_RGIMED, NM_RGIMED, CO_RGINTM, NM_RGIINTM
# Junção simples: dados APL (CO_MUN6) + códigos geográficos (CO_MUN6)  
load("qbq_ocup_cmento1.rda")

gpkg_local_path <- "sf_regioes.gpkg"
sf_regioes <- st_read(dsn = gpkg_local_path, layer = "sf_regioes_ibge", quiet = TRUE)


# ===== CARREGAMENTO DE DADOS APL (SIMPLIFICADO) =====
# Corrigir chaves geo para incluir CO_MUN (utilizado por sf_regioes)
dft_geo_keys <- as.data.table(df_codes_ibge)[
  , .(CO_MUN6, CO_MUN, SG_UF, NM_UF, CO_UF, NM_MUN,
      CO_RGIMED, NM_RGIMED, CO_RGINTM, NM_RGIINTM)
]
dft_geo_keys <- unique(dft_geo_keys, by = "CO_MUN6")

# Junção simples: selecionar apenas colunas necessárias para evitar duplicação .x/.y
apl_geo <- merge(
  dft_apl_MUN_final[, .(CO_MUN6, cbo_4dig, persist, E_mun_cbo, LQ, cbo_familia)],  # Apenas dados APL
  dft_geo_keys,                                                                      # Chaves geográficas completas
  by = "CO_MUN6",         
  all.x = TRUE
)


# Juntar códigos UF ao sf_regioes 
df_ufs_apl <- dft_geo_keys[, .(CO_UF, SG_UF, NM_UF)] %>% 
  unique() %>%
  mutate(CO_UF = as.character(CO_UF))  # Converter para character para compatibilidade com sf_regioes

sf_regioes <- sf_regioes %>%
  left_join(df_ufs_apl, by = "CO_UF")

# Obter nomes de famílias CBO
cbo_familias <- unique(qbq_ocup_cmento1[, c("cbo_4dig", "cbo_familia")])
##########
# Carregar dados de cursos em diferentes níveis de agregação
load("apl_matri_UF.rda")     # Para correspondência de cursos no nível UF
load("apl_matri_MUN.rda")    # Para correspondência de cursos no nível municipal

# Criar dados de cursos municipais com hierarquia geográfica completa
apl_matri_MUN_geo <- merge(
  apl_matri_MUN,           # Esquerda: dados de cursos municipais (contém CO_MUN6)
  dft_geo_keys,            # Direita: hierarquia geográfica completa  
  by = "CO_MUN6",          # Junção por CO_MUN6
  all.x = TRUE
)

###############################################################################
# SEÇÃO D1: CARREGAMENTO DE DADOS — DINAMISMO ECONÔMICO
# Fonte: MUN_dyna02_21.rda (índice de dinamismo municipal baseado no crescimento do PIB)
# Utilizado pela Aba D1 (Demanda EPT: Dinamismo Econômico)
###############################################################################

# Carregar resultados do índice de dinamismo municipal
load("MUN_dyna02_21.rda")

# Converter para data.table se necessário
if (!is.data.table(MUN_dyna02_21)) {
  setDT(MUN_dyna02_21)
}

# Extrair variáveis de dinamismo econômico para painel de análise
# Manter CO_MUN como numérico para compatibilidade com dft_geo_keys
dynamism_base <- MUN_dyna02_21[, .(
  CO_MUN = `Código.do.Município`,  # Manter como numérico
  dynamism_index = dynamism_index,
  dynamism_decile = dynamism_decile, 
  dynamism_percentile = dynamism_percentile,
  avg_population = avg_population,
  period1_avg_growth = period1_avg_growth,
  period2_avg_growth = period2_avg_growth,
  pop_weighted_contribution = pop_weighted_contribution
)]

# Junção limpa - nomes geográficos vêm apenas de dft_geo_keys
dynamism_geo <- merge(
  dft_geo_keys,           # Hierarquia geográfica (lado esquerdo) 
  dynamism_base,          # Dados econômicos (lado direito)
  by = "CO_MUN",
  all.y = TRUE            # Manter todos os municípios com dados de dinamismo
)

# Converter para mapeamento sf APÓS junção
dynamism_geo$CO_MUN <- as.character(dynamism_geo$CO_MUN)

# Remover linhas com dados geográficos ausentes
dynamism_geo <- dynamism_geo[!is.na(NM_UF)]


# Juntar com dados mais recentes de PIB para contexto econômico adicional
# Juntar com dados mais recentes de PIB para contexto econômico adicional


latest_pib_data <- df_pibmunis %>%
  filter(Ano == 2021) %>%  # Ano mais recente
  select(
    CO_MUN = 7,            # Código.do.Município
    pib_total_2021 = 39,   # Produto.Interno.Bruto
    pib_per_capita_2021 = 40,  # PIB per capita
    agro_va = 33,          # Agropecuária VA
    industria_va = 34,     # Indústria VA
    servicos_va = 35,      # Serviços VA
    admin_va = 36,         # Administração VA
    total_va = 37,         # VA Total
    main_activity = 41,    # Atividade principal
    second_activity = 42,  # Segunda atividade
    third_activity = 43    # Terceira atividade
  ) %>%
  mutate(
    CO_MUN = as.character(CO_MUN),
    # Calcular percentagens por setor
    agro_pct = ifelse(total_va > 0, round((agro_va / total_va) * 100, 1), NA),
    industria_pct = ifelse(total_va > 0, round((industria_va / total_va) * 100, 1), NA),
    servicos_pct = ifelse(total_va > 0, round((servicos_va / total_va) * 100, 1), NA),
    admin_pct = ifelse(total_va > 0, round((admin_va / total_va) * 100, 1), NA),
    # Criar string combinada das 3 principais atividades
    top_activities = paste(
      ifelse(is.na(main_activity) | main_activity == "", "", paste("1:", substr(main_activity, 1, 12))),
      ifelse(is.na(second_activity) | second_activity == "", "", paste("2:", substr(second_activity, 1, 12))),
      ifelse(is.na(third_activity) | third_activity == "", "", paste("3:", substr(third_activity, 1, 12))),
      sep = " | "
    ),
    # Limpar separadores extras
    top_activities = gsub("^\\s*\\|\\s*|\\s*\\|\\s*$", "", top_activities),
    top_activities = gsub("\\s*\\|\\s*\\|\\s*", " | ", top_activities)
  )

# Juntar dados de PIB com dados de dinamismo
dynamism_geo_enhanced <- dynamism_geo %>%
  left_join(latest_pib_data, by = "CO_MUN")

# Substituir o dynamism_geo original
dynamism_geo <- dynamism_geo_enhanced

###############################################################################
# SEÇÃO D3: CARREGAMENTO DE DADOS — EMPREGO FORMAL/INFORMAL
# Fontes: df_cbocod_mun23/24.rda (dados de emprego CBO municipal),
#          taxas de formalidade PNAD-C para estimativa de emprego total
# Utilizado pela Aba D3 (Demanda EPT: Informalidade)
###############################################################################
# Chaves geográficas dedicadas à análise de informalidade  
dft_informality_geo_codes <- as.data.table(df_codes_ibge)[
  , .(CO_MUN6, CO_MUN, SG_UF, NM_UF, CO_UF, NM_MUN,
      CO_RGIMED, NM_RGIMED, CO_RGINTM, NM_RGIINTM)
]
dft_informality_geo_codes <- unique(dft_informality_geo_codes, by = "CO_MUN6")

# Carregar dados municipais de emprego pré-processados
load("df_cbocod_mun23.rda")  
load("df_cbocod_mun24.rda")

uf_choices_all <- sort(unique(dft_informality_geo_codes$NM_UF))
default_uf <- "Alagoas"


###############################################################################
# SEÇÃO C3: CARREGAMENTO DE DADOS — OFERTA EPT DETALHADA (CENSO ESCOLAR)
# Fontes: df_censo_supl_tec23/24.rda, df_censo_notin_cnct.rda
# Utilizado pela Aba C3 (Oferta EPT: Redes)
###############################################################################

# Carregar todos os dados de educação técnica do Censo Escolar
load("df_censo_supl_tec23.rda") 
load("df_censo_supl_tec24.rda")   
load("df_censo_notin_cnct.rda")

# ===== COMBINAR CONJUNTOS DE DADOS =====
common_cols <- c(
  "CO_MUN", "ANO", "TP_DEPENDENCIA",
  "NO_AREA_CURSO_PROFISSIONAL", "NO_CURSO_EDUC_PROFISSIONAL", 
  "QT_CURSO_TEC", "QT_MAT_CURSO_TEC",
  "QT_MAT_CURSO_TEC_CT", "QT_MAT_CURSO_TEC_NM",
  "QT_MAT_CURSO_TEC_CONC", "QT_MAT_TEC_SUBS", "QT_MAT_TEC_EJA"
)

df_censo_combined <- bind_rows(
  df_censo_supl_tec23[, common_cols],
  df_censo_supl_tec24[, common_cols], 
  df_censo_notin_cnct[, common_cols]
) %>%
  left_join(dft_informality_geo_codes, by = "CO_MUN")

# ===== DEFAULTS =====
default_uf_censo <- "Rio Grande do Sul"
default_year_censo <- 2024
uf_choices_censo <- sort(unique(dft_informality_geo_codes$NM_UF))

###############################################################################
# SEÇÃO E1: CARREGAMENTO DE DADOS — CORRESPONDÊNCIA OFERTA-DEMANDA (CNCT-CBO)
# Fontes: df_mat_*.rda (matrículas por nível), rais_cbo6_uf23/24.rda,
#          cnct_qbq_matches2.rda, qbq_cnct_matches2.rda (correspondências geradas por IA)
# Utilizado pela Aba E1 (Oferta e Demanda EPT)
###############################################################################
# Carregar arquivos de dados necessários
load("df_mat_uf.rda")       # Matrículas EPT por UF e ANO 
load("df_mat_eixo.rda")     # Matrículas por Eixo Tecnológico
load("df_mat_area.rda")     # Matrículas por Área Tecnológica
load("df_mat_curso.rda")    # Matrículas por Curso
load("df_exarcu.rda")       # Metadados do curso (IDX_EIXCUR, Eixo, Área, Denominação)
load("rais_cbo6_uf23.rda")  # Dados de emprego RAIS 2023
load("rais_cbo6_uf24.rda")  # Dados de emprego RAIS 2024
load("cnct_qbq_matches2.rda") # Correspondências curso para ocupação
load("qbq_cnct_matches2.rda") # Correspondências ocupação para curso

# Criar datasets em formato wide para matrículas
df_mat_uf_wide <- df_mat_uf %>%
  select(CO_UF, NM_UF, SG_UF, ANO, QT_MAT_CURSO_TEC_UF) %>%
  pivot_wider(
    names_from  = ANO,
    values_from = QT_MAT_CURSO_TEC_UF,
    values_fill = list(QT_MAT_CURSO_TEC_UF = 0),
    values_fn   = sum
  ) %>%
  rename(`Matrículas 2023` = `2023`, `Matrículas 2024` = `2024`)

df_mat_eixo_wide <- df_mat_eixo %>%
  select(CO_UF, NM_UF, SG_UF, `Eixo Tecnológico`, ANO, QT_MAT_CURSO_TEC_EIX) %>%
  pivot_wider(
    names_from  = ANO,
    values_from = QT_MAT_CURSO_TEC_EIX,
    values_fill = list(QT_MAT_CURSO_TEC_EIX = 0),
    values_fn   = sum
  ) %>%
  rename(`Matrículas 2023` = `2023`, `Matrículas 2024` = `2024`)

df_mat_area_wide <- df_mat_area %>%
  select(CO_UF, NM_UF, SG_UF, `Área Tecnológica`, ANO, QT_MAT_CURSO_TEC_ARE) %>%
  pivot_wider(
    names_from  = ANO,
    values_from = QT_MAT_CURSO_TEC_ARE,
    values_fill = list(QT_MAT_CURSO_TEC_ARE = 0),
    values_fn   = sum
  ) %>%
  rename(`Matrículas 2023` = `2023`, `Matrículas 2024` = `2024`)

df_mat_curso_wide <- df_mat_curso %>%
  select(CO_UF, NM_UF, SG_UF, IDX_EIXCUR, `Denominação do Curso`, ANO, QT_MAT_CURSO_TEC_CUR) %>%
  pivot_wider(
    names_from  = ANO,
    values_from = QT_MAT_CURSO_TEC_CUR,
    values_fill = list(QT_MAT_CURSO_TEC_CUR = 0),
    values_fn   = sum
  ) %>%
  rename(`Matrículas 2023` = `2023`, `Matrículas 2024` = `2024`)

# Combinar dados RAIS e criar agregações
df_rais_all <- bind_rows(
  rais_cbo6_uf23 %>% mutate(ANO = 2023),
  rais_cbo6_uf24 %>% mutate(ANO = 2024)
)

# Criar agregações no nível Brasil
rais_br_cbo1 <- df_rais_all %>% 
  filter(!is.na(cbo_gragru)) %>%
  group_by(ANO, cbo_1dig, cbo_gragru) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
  mutate(NM_UF = "Brasil", SG_UF = "BR")

rais_br_cbo4 <- df_rais_all %>% 
  filter(!is.na(cbo_familia)) %>%
  group_by(ANO, cbo_4dig, cbo_familia) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
  mutate(NM_UF = "Brasil", SG_UF = "BR")

rais_br_codcbo <- df_rais_all %>% 
  filter(!is.na(`Ocupação`)) %>%
  group_by(ANO, CodCBO, `Ocupação`) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
  mutate(NM_UF = "Brasil", SG_UF = "BR")

# Combinar dados no nível UF e Brasil
df_rais1dig <- bind_rows(
  df_rais_all %>% 
    group_by(ANO, NM_UF, SG_UF, cbo_1dig, cbo_gragru) %>%
    summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop"),
  rais_br_cbo1
)

df_rais4dig <- bind_rows(
  df_rais_all %>% 
    group_by(ANO, NM_UF, SG_UF, cbo_4dig, cbo_familia) %>%
    summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop"),
  rais_br_cbo4
)

df_raisCodCBO <- bind_rows(
  df_rais_all %>% 
    group_by(ANO, NM_UF, SG_UF, CodCBO, `Ocupação`) %>%
    summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop"),
  rais_br_codcbo
)

# Converter para formato wide
df_rais1dig_wide <- df_rais1dig %>%
  pivot_wider(
    names_from  = ANO,
    values_from = vinculos,
    values_fill = list(vinculos = 0),
    values_fn   = sum
  ) %>% 
  filter(!is.na(cbo_gragru)) %>% 
  rename(`Vínculos 2023` = `2023`, `Vínculos 2024` = `2024`)

df_rais4dig_wide <- df_rais4dig %>%
  pivot_wider(
    names_from  = ANO,
    values_from = vinculos,
    values_fill = list(vinculos = 0),
    values_fn   = sum
  ) %>% 
  filter(!is.na(cbo_familia)) %>% 
  rename(`Vínculos 2023` = `2023`, `Vínculos 2024` = `2024`)

df_raisCodCBO_wide <- df_raisCodCBO %>%
  pivot_wider(
    names_from  = ANO,
    values_from = vinculos,
    values_fill = list(vinculos = 0),
    values_fn   = sum
  ) %>%  
  filter(!is.na(NM_UF), !is.na(`Ocupação`)) %>% 
  rename(`Vínculos 2023` = `2023`, `Vínculos 2024` = `2024`) %>%
  mutate(
    CodCBO   = as.character(CodCBO),
    cbo_1dig = substr(CodCBO, 1, 1)
  )


###############################################################################
# DEFINIÇÃO DA UI
# Layout dashboard sem cabeçalho/barra lateral, CSS customizado e painéis com abas
###############################################################################
ui <- dashboardPage(

# Layout do dashboard: cabeçalho e barra lateral desativados; conteúdo em largura total
dashboardHeader(disable = TRUE),
dashboardSidebar(disable = TRUE),
  
dashboardBody(
###############################################################################
# CUSTOMIZAÇÃO CSS
# Estilos customizados para cores viridis nas abas, painéis escuros, controles de formulário.
# Alguns estilos definidos inline porque custom.css é sobrescrito pelo Bootstrap.
###############################################################################
useShinyjs(),
# jQuery-UI (para arrastável)
tags$head(tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js")),

    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tags$head(
      tags$style(HTML("
        .main-header { display: none; }
        .content-wrapper, .right-side { margin-top: 0px !important; }
      ")), 
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tags$head(
      tags$style(HTML("
    /* Force all checkbox labels to black */
    .form-group.shiny-input-checkbox label,
    .checkbox label,
    .checkbox-inline label,
    .shiny-input-container input[type='checkbox'] + span {
      color: black !important;
    }

    /* Existing label styling */
    label,
    .selectize-control.single .selectize-input,
    .irs-single,
    .irs-min,
    .irs-max,
    .irs-grid-text,
    .shiny-input-container > .control-label {
      color: black !important;
    }

    /* Slider tweaks */
    .irs {
      background-color: transparent !important;
    }
    .irs-bar,
    .irs-line {
      background: #ccc !important;
    }
    .irs-slider {
      background-color: #555 !important;
      border: 1px solid #999 !important;
    }
    .irs-grid-text {
      color: #333 !important;
      font-weight: bold;
    }
  "))
    ),
tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.datatables.net/buttons/2.4.1/css/buttons.dataTables.min.css"),
      tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/dataTables.buttons.min.js"),
      tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.flash.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.1.3/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.53/pdfmake.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.53/vfs_fonts.js"),
      tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.html5.min.js"),
      tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.print.min.js")
    ),


###########  MOBILE ACCESS WARNING
# Overlay exibido em telas pequenas recomendando uso em desktop
tags$head(tags$style(HTML("
  .mobile-blocker{position:fixed; inset:0; display:none; z-index:20000;
    align-items:center; justify-content:center; padding:24px;
    background:rgba(15,23,42,.92); pointer-events:auto;}
  .mobile-blocker .mbox{max-width:640px; width:100%;
    background:#fee2e2; border:1px solid #fca5a5; border-radius:14px;
    padding:22px 20px; text-align:center; box-shadow:0 12px 28px rgba(0,0,0,.35);}
  .mobile-blocker h2{margin:6px 0 8px; font-weight:800;}
  .mobile-blocker p{margin:0; font-size:16px;}
  .mobile-blocker .rowbtn{margin-top:14px; display:flex; gap:10px; justify-content:center;}
  .mobile-blocker .ack, .mobile-blocker .skip{
    border:0; padding:8px 14px; border-radius:8px; cursor:pointer;}
  .mobile-blocker .ack{background:#ef4444; color:#fff;}
  .mobile-blocker .skip{background:#20313d; color:#e8eef5; border:1px solid #355468;}
  body.no-scroll{overflow:hidden;}
"))),


# markup do overlay (fora do head)
div(
  id = "mobile-blocker", class = "mobile-blocker", tabindex = "0",
  div(class="mbox",
      tags$span(style="font-size:28px;display:inline-block;", "❗"),
      h2("Esta ferramenta foi desenvolvida para uso em computador."),
      p("Por favor, acesse em um notebook ou desktop."),
      div(class="rowbtn",
          tags$button(
            class="ack",
            onclick="
              document.getElementById('mobile-blocker').style.display='none';
              document.body.classList.remove('no-scroll');
              localStorage.setItem('propag_mobile_dismiss','1');
            ",
            "Entendi"
          ),
          tags$button(
            class="skip",
            onclick="
              document.getElementById('mobile-blocker').style.display='none';
              document.body.classList.remove('no-scroll');
              localStorage.setItem('propag_mobile_dismiss','1');
            ",
            "Continuar assim mesmo"
          )
      )
  )
),

# script detector (no head)
tags$head(tags$script(HTML("
  function showMobileBlocker(){
    try {
      if(localStorage.getItem('propag_mobile_dismiss')==='1'){ return; }
    } catch(e) {}
    var isSmall = window.matchMedia('(max-width: 991.98px)').matches; // ~Bootstrap md
    var isUA    = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
    var show    = isSmall || isUA;
    var el = document.getElementById('mobile-blocker');
    if(!el) return;
    el.style.display = show ? 'flex' : 'none';
    if(show){ document.body.classList.add('no-scroll'); } else { document.body.classList.remove('no-scroll'); }
    if(window.Shiny){ Shiny.setInputValue('is_mobile', show, {priority:'event'}); }
  }
  window.addEventListener('load', showMobileBlocker, {once:true});
  window.addEventListener('resize', showMobileBlocker);
"))),




###############################################################################################################
#      # Header title area    # Header title area    # Header title area    # Header title area    # Header title area
###############################################################################################################

tags$div(
      style = "padding: 10px 20px; font-size: 24px; font-weight: bold; color: #0000ff;",
      HTML('Faça adesão ao <span style="color: #FFD700; text-shadow: 1px 1px #333;">Propag</span> !')
    ),
  

    fluidRow(
      
      ## TITLE  LINE
      column(
        12,
        div(
          style = "background-color: #1f5673; padding: 10px; text-align: center; color: white; font-weight: bold; font-size: 16px; margin-bottom: 10px;",
          "Ferramenta de apoio",
          span("analítico ", style = "color: #ff6619;"),
          "usando a",
          span("Inteligência Artificial", style = "color: #ffcc00;"),
          " – desenvolvida por uma equipe do",
          span("Banco Mundial", style = "color: #ffcc00;"),
          " com apoio da ",
          span("FGV/DGPE", style = "color: #ffcc00;")
        )
      )
    ),
    
    tabsetPanel(id = "tab_selection", selected = "Introdução",

                ###############################################################################################################
                # TAB  0  INTRODUCTION #### # TAB  0  INTRODUCTION ##### TAB  0  INTRODUCTION ##### TAB  0  INTRODUCTION ##### TAB  0 
                ###############################################################################################################
                tabPanel(
                  "Introdução",
                  tags$style(HTML("
    .tab-pane[data-value='Introdução'] {
      background-color: #f9f9f9 !important;
      color: #222 !important;
      padding: 30px;
      font-size: 17px;
    }

    .intro-heading {
      text-align: center;
      font-size: 26px;
      font-weight: bold;
      color: #c0392b;
      margin-bottom: 30px;
    }

    .tab-link-line {
      margin-bottom: 4px;
      font-size: 17px;
      font-weight: bold;
    }

    .tab-link-line a {
      color: #2c3e90 !important;
      text-decoration: none;
    }

    .tab-link-line a:hover {
      text-decoration: underline;
    }

    .tab-explanation {
      margin-bottom: 20px;
      margin-left: 32px;
      font-weight: normal;
      font-size: 17px;
      color: #333;
    }

    .tab-link-line .arrow {
      display: inline-block;
      margin-right: 10px;
      font-size: 17px;
      color: #3E4A89;
    }
  ")),
                  
                  # 0 - Heading
                  div(class = "intro-heading",
                      "Introdução: Ferramenta customizada pela UF – Análise e Simulação sobre o PROPAG"
                  ),
                  
                  
                  # Adicionar instrução de navegação com explicação da codificação de cores
                  div(style = "text-align: center; margin-bottom: 20px; padding: 15px; background-color: #e8f4fd; border-radius: 5px; border: 1px solid #b3d9ff;",
                      HTML("<strong>Navegação:</strong> Acesse as análises clicando nos links abaixo ou nas abas coloridas acima.<br>"),
                      HTML("<span style='font-size: 15px; line-height: 1.8;'>"),
                      HTML("As abas seguem código de cores por tema: "),
                      HTML("<span style='color: #3f4788; font-weight: bold;'>■ Azul</span> = Demografia | "),
                      HTML("<span style='color: #f98e09; font-weight: bold;'>■ Laranja</span> = Finanças | "),
                      HTML("<span style='color: #2db27d; font-weight: bold;'>■ Verde</span> = Oferta EPT | "),
                      HTML("<span style='color: #B4DD2C; font-weight: bold;'>■ Verde-Lima</span> = Demanda EPT | "),
                      HTML("<span style='color: #450a69; font-weight: bold;'>■ Roxo</span> = Conexão Oferta-Demanda"),
                      HTML("</span>")
                  ),
                  
                  # Layout de duas colunas começa aqui
                  fluidRow(
                    # COLUNA ESQUERDA — A, B, C1, C2
                    column(
                      width = 6,
                      
                      # 1
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_demo", "A1. Transição Demográfica: Projeções Populacionais por Faixa Etária")
                      ),
                      div(class = "tab-explanation", "Análise das mudanças no perfil etário da população brasileira (2000-2070) por região e estado, 
                  identificando pontos de transição demográfica relevantes para o planejamento do EPT."),
                      
                      # 2
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_ept", "A2. EPT e População: População 15-19 anos e Matrículas EPT/Ensino Médio")
                      ),
                      div(class = "tab-explanation", "Análise comparativa entre população de 15-19 anos e matrículas na Educação Profissional Técnica (EPT) e 
                  Ensino Médio (2007-2035), incluindo acompanhamento das metas do PNE Meta 11 por estado e região."),
                      
                      # 3 
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_impacto", "B1. Impacto Financeiro do PROPAG: Simulações com e sem Adesão")
                      ),
                      div(class = "tab-explanation", "Situação do endividamento e do fluxos financieros relevantes do estado em cenários com e sem adesão ao PROPAG."),
                      
                      # 4 
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_fef", "B2. Cenário Financeiro e Simulações de Investimento em EPT e Fundo FEF")
                      ),
                      div(class = "tab-explanation", "Simulação do valor da contribuição estadual ao Fundo de Equalização Fiscal (FEF), dos aportes 
                  recebidos e do fluxo líquido estimado, com base nas decisões de adesão."),
                      
                      # 5 
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_finuf", "B3. Contribuição, Aporte e Fluxo Líquido - Visualização Comparativa")
                      ),
                      div(class = "tab-explanation", "Situação do endividamento e do fluxos financieros - visualiazação comparativa de conjunto de UFs"),
                      
                      # 6 (era B1)
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_meta11pro", "C1. Oferta EPT e Meta PNE Projeção: Oferta de EPT versus a Meta 11 do PNE")
                      ),
                      div(class = "tab-explanation", "Comparação da oferta de educação técnica em relação à Meta 11 do PNE 
                  vigente ou PNE aplicado para PROPAG (prenchido pelo usuario"),
                      
                      # 7 (era B2)
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_meta11pla", "C2. Oferta EPT e Meta PNE Planificação: Matrículas para atingir a Meta PNE")
                      ),
                      div(class = "tab-explanation", "Projeção da expansão de matrículas na EPT requerido por ano alvo de Meta (publicado p preenchido por usuario) 
                  por simulações de ano")
                    ),
                    
                    # COLUNA DIREITA — C3, C4, D1, D2, D3, E1, E2
                    column(
                      width = 6,
                      
                      # 8 (era B3)
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_ofertadet", "C3. Detalhe da Oferta - Análise de Matrículas EPT por Eixo e Curso")
                      ),
                      div(class = "tab-explanation", "Dados detalhados do Censo Escolar 2023-2024 com matrículas em EPT ,
                  segmentados por eixo tecnológico, curso e dependência administrativa, permitindo análise hierárquica por UF e município."),
                      
                      # 9 (era B4)
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_modelo", "C4. Modelo Econometrico de Matrícula - Análise de Matrículas EPT por Rede e UF")
                      ),
                      div(class = "tab-explanation", "Modelo econométrico básico, segmentado por setor econômico e dependência administrativa, 
                  permitindo análise de efectos por UF e rede."),
                      
                      # 10
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_dinamismo", "D1. Demanda EPT: Dinamismo Econômico")
                      ),
                      div(class = "tab-explanation", "Análise do dinamismo econômico municipal baseado no crescimento do PIB per capita, identificando 
                  municípios em expansão ou queda econômica."),
                      
                      # 11
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_arranjo", "D2. Demanda EPT: Arranjos Produtivos Laborais")
                      ),
                      div(class = "tab-explanation", "Identificação de Arranjos Produtivos Laborais (APL) baseados em especialização ocupacional (CBO), 
                              mapeando concentrações de emprego formal por ocupação."),
                      
                      # 12
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_informal", "D3. Demanda EPT: Mercado Formal e Informal")
                      ),
                      div(class = "tab-explanation", "Análise da distribuição de emprego formal e informal por município, classificando territórios em decis 
                  baseado na proporção de vínculos formais em relação ao total de vínculos."),
                      
                      # 13
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_oferta_demanda", "E1. Oferta e Demanda EPT")
                      ),
                      div(class = "tab-explanation", "Conexão gerado por IA entre oferta educacional (matrículas EPT por eixo, área e curso) e demanda laboral 
                  (vínculos por ocupação CBO), identificando correspondências entre cursos técnicos e ocupações."),
                      
                      # 14
                      div(class = "tab-link-line",
                          span(class = "arrow", HTML("▸")),
                          actionLink("link_escassez", "E2. Demanda e Escassez de Profissionais Técnicos no Brasil")
                      ),
                      div(class = "tab-explanation", "Diagnóstico da escassez de profissionais técnicos por curso e UF com base nos dados de mercado de trabalho (RAIS/CAGED).")
                    )
                  )
                ),
			              

###############################################################################
# ABA A1 UI: TRANSIÇÃO DEMOGRÁFICA
# Projeções populacionais IBGE (2000-2070) com visualização da linha de cruzamento
###############################################################################
tabPanel(
  "A1. Transição Demográfica",
  div(class = "tab-simple-layout",  # Wrapper class
  fluidPage(
    h3("Transição Demográfica: Projeções Populacionais por Faixa Etária",
       style = "color: #1f5673; font-weight: bold;"),
    
    div(class = "topbar-info",  
        
        # Linha de instruções
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;",
              tagList(
                tags$strong("Instruções: "),
                "Esta análise mostra as projeções populacionais do IBGE (2000-2070) por faixa etária. ",
                "A 'Linha de Transição Demográfica' marca o ponto onde a população em declínio de 0-14 anos cruza com a população crescente de 60+ anos. ",
                "Use os controles abaixo para explorar diferentes regiões e tipos de dados."
              )
            )
          )
        ),
        
        # Linha de controles
        fluidRow(
          column(
            width = 3,
            tags$label("Selecionar Localização(ões):",
                       style = "font-weight: bold; color: #333; font-size: 16px;"),
            pickerInput(
              "demo_localInput",
              label = NULL,
              choices = names(demo_local_colors),
              options = list(`actions-box` = TRUE),
              multiple = TRUE,
              selected = "Amazonas"
            )
          ),
          
          column(
            width = 3,
            tags$label("Escolher Tipo de Dados:",
                       style = "font-weight: bold; color: #333; font-size: 16px;"),
            radioButtons(
              "demo_dataType",
              label = NULL,
              choices = list("Números Populacionais" = "numbers", "Proporções Populacionais" = "proportions"),
              selected = "numbers"
            )
          ),
          
          column(
            width = 3,
            tags$label("Selecionar Variável(eis):",
                       style = "font-weight: bold; color: #333; font-size: 16px;"),
            uiOutput("demo_variableInput")
          ),
          
          column(
            width = 3,
            tags$label("Opções de Visualização:",
                       style = "font-weight: bold; color: #333; font-size: 16px;"),
            div(style = "margin-top: 10px;",
                checkboxInput(
                  "demo_showTransition",
                  label = "Mostrar Linha de Transição Demográfica",
                  value = TRUE
                )
            )
          )
        )
    ),
    

    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-top: 0px; margin-bottom: 5px; color: #1f5673; text-align: justify; font-size: 20px;",
          HTML("<strong>Nota:</strong> Os dados mostram a evolução demográfica brasileira de 2000 a 2070, 
               permitindo identificar períodos críticos para planejamento educacional e políticas públicas.")
        )
      )
    ),
    
    # Seção do gráfico
    fluidRow(
      column(12,
             plotlyOutput("demo_linePlot", height = "600px", width = "100%")
      )
    )
  )
)),

###############################################################################
# ABA A2 UI: MATRÍCULAS EPT E POPULAÇÃO
# População 15-19 vs matrículas EPT/Ensino Médio (2007-2035) com metas do PNE
###############################################################################

tabPanel("A2. EPT e População",
     
         fluidPage(
           h3("População 15-19 anos e Matrículas EPT/Ensino Médio", 
              style = "color: #1f5673; font-weight: bold; font-size: 24px;"),
           
           div(class = "topbar-info",
               div(style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;", 
              tagList(
                     tags$strong("Instruções: "),
               "Esta análise compara a população elegível (15-19 anos) com as matrículas na Educação Profissional Técnica (EPT) e Ensino Médio (2007-2035). Inclui
               acompanhamento das metas do PNE Meta 11 por estado e região, permitindo configurar cenários alternativos. Use os controles 
                 abaixo para explorar diferentes localizações e configurar metas personalizadas para análise.", 
                ))),
           
           sidebarLayout(
             sidebarPanel(
               width = 3,
               style = "color: black; font-size: 16px;",
               
               h4("Controles de Análise", style = "color: #1f5673; font-size: 18px;"),
               
               div(style = "color: black; font-size: 18px;",
                   tags$label("Selecionar Localização(ões):", 
                              style = "font-weight: bold; color: #333; font-size: 16px;"),
                   pickerInput(
                     "ept_localInput",
                     label = NULL,
                     choices = sort(unique(ept_combined_data$LOCAL)),
                     selected = "Piauí",
                     multiple = TRUE,
                     options = list(`actions-box` = TRUE, `live-search` = TRUE)
                   )
               ),
               
               div(style = "color: black; font-size: 18px;",
                   tags$label("Escolher Tipo de Dados:", 
                              style = "font-weight: bold; color: #333; font-size: 16px;"),
                   radioButtons(
                     "ept_dataType",
                     label = NULL,
                     choices = list("Números Absolutos" = "numbers", "Percentagens" = "percentages"),
                     selected = "numbers"
                   )
               ),
               
               div(style = "color: black; font-size: 18px;", 
                   tags$label("Selecionar Variável(eis):", 
                              style = "font-weight: bold; color: #333; font-size: 16px;"),
                   uiOutput("ept_variableInput")),
               
               div(style = "color: black; font-size: 18px;",
                   tags$label("Opções de Visualização:", 
                              style = "font-weight: bold; color: #333; font-size: 16px;"),
                   checkboxInput(
                     "ept_showMeta",
                     "Mostrar Linhas de Meta PNE 11",
                     value = TRUE
                   )
               ),
               
               conditionalPanel(
                 condition = "input.ept_showMeta",
                 style = "color: black; font-size: 16px;",
                 hr(),
                 h4("Configuração de Metas", style = "color: #1f5673; font-size: 18px;"),
                 p("Meta Targets (para localizações selecionadas):", 
                   style = "font-weight: bold; color: black; font-size: 16px;"),
                 div(style = "color: black; font-size: 16px;", uiOutput("ept_metaTargetsUI"))
               ),
               
               hr(),
               h5("Resumo da Seleção:", style = "color: #1f5673; font-size: 16px;"),
               div(style = "color: black; font-size: 16px;", uiOutput("ept_summary"))
             ),
             
             mainPanel(
               width = 9,
               style = "font-size: 18px;",
               h4("Evolução Populacional e de Matrículas EPT/EM", 
                  style = "color: #1f5673; font-size: 20px;"),
               withSpinner(plotlyOutput("ept_linePlot", height = "600px")),
               
               br(),
               
               div(style = "text-align: center; margin-top: 15px; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
                   HTML("<p style='font-size: 14px; color: #666; margin: 0;'>
                    <strong>Fontes:</strong> 
                    <a href='https://www.ibge.gov.br/estatisticas/sociais/populacao/9109-projecao-da-populacao.html' 
                    target='_blank' style='color: #1f5673;'>Projeções Populacionais IBGE</a> | 
                    <a href='http://portal.inep.gov.br/web/guest/censo-escolar' 
                    target='_blank' style='color: #1f5673;'>Censo Escolar INEP</a>
                   </p>")
               )
             )
           )
         )
), 


###############################################################################################################
###############################################################################
# ABA B1 UI: IMPACTO FINANCEIRO PROPAG
# Construtor interativo de cenários para dívida, FEF, investimento EPT e taxa de juros
###############################################################################
###############################################################################################################  
tabPanel(
  "B1. Financiamento PROPAG",
  fluidPage(
    
    div(class = "topbar-info",
        div(style = "margin-top: 0px; margin-bottom: 0px; color: #333; text-align: justify; font-size: 20px;", 
            tagList(
              tags$strong(""),
              
            ))),
    
    # JS: forçar seleção única por grupo
    tags$head(
      tags$script(HTML("
        $(document).on('shiny:connected', function () {
          ['A','G','I','J'].forEach(function(dim){
            $(document).on('click', '#choice_' + dim + ' input[type=checkbox]', function () {
              var $grp = $('#choice_' + dim);
              $grp.find('input[type=checkbox]').not(this).prop('checked', false);
              $(this).trigger('change');
            });
          });
        });
      "))
    ),
    
    
    tags$head(
      tags$script(HTML("
    // lock/unlock A,G,I when 'Não Adere' is chosen
    Shiny.addCustomMessageHandler('toggleAGI', function(lock){
      ['A','G','I'].forEach(function(dim){
        var $grp = $('#choice_' + dim);
        $grp.find('input[type=checkbox]').prop('disabled', lock);
        // optional visual dimming
        $grp.css('opacity', lock ? 0.35 : 1);
      });
    });
  "))
    ),
    
    tags$head(tags$style(HTML("
  .pretty input + label {
    font-size: 16px !important;
  }
"))),
    
    
    div(class = "checkbox-dark-panel",
        # ---- LINHA 1: UF + 4 grupos + resumo (tudo em uma linha) ----
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-top: 5px; margin-bottom: 10px; color: #f5f5f5; text-align: justify; font-size: 20px;",
              tagList(
                tags$strong("Instruções: "),
                "Por favor, primeiro, seleccione o seu Estado, e depois, marque as opções relevantes para o seu Estado. ",
                "À medida que você clicar em uma escolha, o painel à direita mostrará ",
                "apenas opções válidas que podem ser escolhidas. ",
                "Você pode começar por qualquer uma das quatro escolhas: ",
                tags$span(class = "highlighted-note", 
                          "(i) Abatimento (0%, 10% ou 20%) ; (ii) Aporte ao FEF (1%, 1.5% ou 2%); (iii) Investimento Direto (cinco opções);
                                     (iv) Taxa de juro (0%, 1% ou 2%)"),
                "Depois, você pode escolher a variavel financiero que você quer - saldo de dívida, aporte para o FED, investimento
                direito para EPT e juros pagos; você escolha as opções que quer comparar e o periodo cuberto entre 2025 e 2054."
              )
            )
          )
        ),
        
        
        fluidRow(
          # Seletor de UF
          column(
            width = 2,
            tags$label("Selecione a UF:",
                       style = "font-weight: bold; color: #f5f5f5; font-size: 16px;"),
            selectizeInput("uf_select", label = NULL, choices = nome_ufs)
          ),
          
          # Four groups
          column(
            width = 8,
            fluidRow(
              column(
                width = 3,
                tags$label("(i) Taxa de Juros:",
                           style = "font-weight: bold; display: block;"),
                prettyCheckboxGroup(
                  inputId  = "choice_J",
                  label    = NULL,
                  choices  = c("0%" = "J1",
                               "1%" = "J2",
                               "2%" = "J3",
                               "4% (Não Adere)" = "J4"),
                  selected = character(0),
                  icon     = icon("check"),
                  fill     = TRUE,
                  status   = "danger",
                  bigger   = TRUE,
                  inline   = TRUE
                )
              ),
              
              column(
                width = 3,
                tags$label("(ii) Amortização Inicial",
                           style = "font-weight: bold; display: block;"),
                prettyCheckboxGroup(
                  inputId  = "choice_A",
                  label    = NULL,
                  choices  = c("Sem abatimento" = "A1",
                               "10% abatimento" = "A2",
                               "20% abatimento" = "A3"),
                  selected = character(0),
                  icon     = icon("check"),
                  fill     = TRUE,
                  status   = "info",
                  bigger   = TRUE,
                  inline   = TRUE
                )
              ),
              column(
                width = 2,
                tags$label("(iii) Contribuição para FEF:",
                           style = "font-weight: bold; display: block;"),
                prettyCheckboxGroup(
                  inputId  = "choice_G",
                  label    = NULL,
                  choices  = c("1%" = "G1",
                               "1.5%" = "G2",
                               "2%" = "G3"),
                  selected = character(0),
                  icon     = icon("check"),
                  fill     = TRUE,
                  status   = "primary",
                  bigger   = TRUE,
                  inline   = TRUE
                )
              ),
              column(
                width = 3,
                tags$label("(iv) Investimento Direto:",
                           style = "font-weight: bold; display: block;"),
                
                prettyCheckboxGroup(
                  inputId  = "choice_I",
                  label    = NULL,
                  choices  = c("0%"   = "I1",
                               "0,5%" = "I2",
                               "1%"   = "I3",
                               "1,5%" = "I4",
                               "2%"   = "I5"),
                  selected = character(0),
                  icon     = icon("check"),
                  fill     = TRUE,
                  status   = "success",
                  bigger   = TRUE,
                  inline   = TRUE
                )
              )
              
            )
          ),
          
          # Coluna de resumo (mesma linha)
          column(
            width = 2,
            div(
              style = "color: #f5f5f5; font-style: italic; font-size: 16px; text-align: center;",
              uiOutput("choice_summary")
            )
          )
        )  # fim fluidRow 2
        
        
    )
    
    
  ),
  # Texto explicativo + selectizeInput com fonte atualizada
  fluidRow(
    # Texto explicativo em largura total
    column(
      width = 12,
      div(
        style = "margin-top: 0px; margin-bottom: 5px; color: #1f5673; text-align: justify; font-size: 20px;",
        HTML("<strong>Nota:</strong> Uma vez selecionada a opção arriba, você pode selecionar entre quatro variaveis para ver a evolução para 
                        o period estimado do Propag: 2025 ate 2054")
      )
    ),
    
    # Input de seleção com estilo de fonte correspondente
    column(
      width = 2,
      div(
        style = "margin-bottom: 10px;",
        tags$label("Selecionar a variavel:", style = "font-weight: bold; color: #1f5673; font-size: 18px;"),
        selectizeInput("var_select", label = NULL,
                       choices = c(
                         "Saldo da Dívida"     = "Saldo",
                         "Aporte para o FEF"   = "ApoFEF",
                         "Investimento Direto" = "InvDir",
                         "Juros Pagos"         = "JurPag"
                       ), selected = "Saldo"
        )
      )
    ),
    column(
      width = 2,
      div(
        style = "margin-bottom: 10px;",
        tags$label("Comparar com outro cenário:", 
                   style = "font-weight: bold; color: #1f5673; font-size: 18px; display: block;"),
        selectizeInput(
          inputId  = "compare_with",
          label    = NULL,
          choices  = c("Nenhum" = "", named_scenarios),
          selected = "",
          options  = list(
            placeholder = "Escolha um cenário para comparar...",
            allowEmptyOption = TRUE,
            onInitialize = I('function() { this.clear(); }'),
            persist = FALSE,
            closeAfterSelect = TRUE
          )
        )
        
      )
    ),
    column(
      width = 4,
      div(
        style = "margin-bottom: 10px; display: flex; align-items: flex-start; gap: 20px;",
        
        # Rótulo alinhado ao topo
        tags$label("Selecionar intervalo de anos:",
                   style = "font-weight: bold; color: #1f5673; font-size: 18px; margin-top: 5px; white-space: nowrap;"),
        
        # Slider ocupa espaço restante
        div(
          style = "flex-grow: 1;",
          sliderInput(
            inputId = "year_range",
            label   = NULL,
            min     = 2025,
            max     = 2054,
            value   = c(2025, 2054),
            step    = 1,
            sep     = ""
          )
        )
      )
    )
    
  ),
  
  
  fluidRow(
    
    column(12,
           plotOutput("PloTab1b",height = "600px", width = "100%")
    )
  )
  
),


###############################################################################
# ABA B2 UI: SIMULAÇÃO FEF
# Simulação de contribuição/retorno do Fundo de Equalização Fiscal com matriz de seleção por UF
###############################################################################  
tabPanel("B2. Retorno FEF",
         fluidPage(
           useShinyjs(),
           div(class = "checkbox-dark-panel",
               # ---- Bloco de instruções (inserido primeiro) ----
               fluidRow(
                 column(
                   width = 12,
                   div(
                     style = "margin-top: 5px; margin-bottom: 10px; color: #f5f5f5; text-align: justify; font-size: 20px;",
                     tagList(
                       tags$strong("Instruções: "),
                       "Nesta aba, você pode simular a escolha de opções pelas UFs no contexto do PROPAG, ",
                       "especificamente para calcular a contribuição total ao Fundo de Equalização Fiscal (FEF). ",
                       "Você pode trabalhar em dois modos distintos: ",
                       tags$span(class = "highlighted-note",
                                 "(i) Todos os Estados seguem uma mesma opção ; (ii) Cada Estado escolhe sua própria opção. "),
                       "A seleção feita aqui impacta diretamente a projeção do tamanho do FEF e seu fluxo líquido ao longo dos anos.",
                       
                       # Inline radio buttons follow directly
                       div(
                         style = "margin-top: 5px; font-size: 18px;",
                         radioButtons("selection_mode", label = NULL,
                                      choices = c("Todos os Estados seguem uma Opção" = "uniform",
                                                  "Cada Estado escolhe uma Opção"     = "per_uf"),
                                      selected = "uniform", inline = TRUE
                         )
                       )
                     )
                   )
                 )  # fim da coluna
                 
                 
               ), # fim das fluid rows
               
               
               
               div(class = "checkbox-dark-panel matrix-wrapper",
                   
                   # Linha de cabeçalho
                   div(class = "matrix-row", style = "display: flex; align-items: center; margin-bottom: 6px;",
                       div(style = "width: 300px;", ""),  # Opção + description
                       div(style = "width: 40px; text-align: center; color: #f5f5f5; font-weight: bold;", "Todos"),
                       lapply(sg_ufs, function(uf) {
                         div(style = "width: 24px; text-align: center; font-size: 14px; font-weight: 500; color: #f5f5f5;", uf)
                       })
                   ),
                   
                   # Matrix rows per Opção
                   lapply(opcoes, function(op) {
                     div(class = "matrix-row", style = "display: flex; align-items: center; margin-bottom: 2px; height: 22px;",
                         div(style = "width: 300px; text-align: left; padding-right: 6px; font-size: 14px; color: #f5f5f5;",
                             op_labels[[op]]
                         ),
                         div(style = "width: 40px; text-align: center;",
                             checkboxInput(paste0("chk_all_", op), label = NULL, value = FALSE)
                         ),
                         lapply(sg_ufs, function(uf) {
                           div(style = "width: 24px; text-align: center; padding: 0; margin: 0;",
                               checkboxInput(inputId = paste0("chk_", op, "_", uf), label = NULL, value = FALSE)
                           )
                         })
                     )
                   })
               ),
               div(style = "margin-top: -20px; margin-bottom: 10px; text-align: justify; font-size: 40px;"),
               fluidRow(
                 column(4,
                        selectInput(
                          "uf_select",
                          label = tags$span("Escolha uma UF para visualizar os fluxos do FEF:", style = "color: white;"),
                          choices = nome_ufs,
                          selected = "Alagoas"
                        )
                 ),
                 column(
                   width = 4,
                   div(
                     style = "margin-bottom: 10px; display: flex; align-items: flex-start; gap: 20px;",
                     
                     # Rótulo alinhado ao topo
                     tags$label("Selecionar intervalo de anos:",
                                style = "font-weight: bold; color: #1f5673; font-size: 18px; margin-top: 5px; white-space: nowrap;"),
                     
                     # Slider ocupa espaço restante
                     div(
                       style = "flex-grow: 1;",
                       sliderInput(
                         inputId = "year_range",
                         label   = NULL,
                         min     = 2025,
                         max     = 2054,
                         value   = c(2025, 2054),
                         step    = 1,
                         sep     = ""
                       )
                     )
                   )
                 )
                 
               )
               
           ),
           
           fluidRow(
             column(12,
                    plotOutput("plotab2", height = "450px")
             )
           )# fim do div para checkbox-dark-panel
         ) # fim de fluidPage
), # fim do tabPanel para opções FEF


###############################################################################
# ABA B3 UI: FINANÇAS COMPARATIVAS
# Visualização financeira multi-UF com painéis separados para estados altamente endividados
###############################################################################    
tabPanel("B3. Financiamento (Comparativo)",
         fluidPage(
           h3("Visualização Financeira do PROPAG", style = "color: #1f5673; font-weight: bold;"),
   
           div(class = "topbar-info",
               div(style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;", 
                   tagList(
                     tags$strong("Instruções: "),
                     "Esta análise simplesemente permite visualização de fluxos do PROPAG numa maneira conjunta de todas UFs. Selecione a variavel
                     financiera e automaticamente aparece el gráfico correspondiente e a tabela com os dados. 
                     ", 
                   ))),
           
                   
           fluidRow(
             column(6,
                    selectizeInput(
                      "fin_variable",
                      "Selecionar variável financeira",
                      choices = fin_choices,
                      selected = "FEF_5ano_liq_cen01",
                      width = "100%"
                    )
             )
           ),
           
           plotOutput("tab1_fin_plot", height = "600px"),
           br(),
           h3("Tabela 1: Variáveis Financeiras", style = "color: #1f5673; font-weight: bold; margin-top: 30px;"),
           div(class = "checkbox-dark-panel",
              tags$style(HTML("
               #tab1_fin_table thead { background-color: #34495e !important; }
               #tab1_fin_table thead th { 
                background-color: #34495e !important; 
                color: #ffffff !important; 
}
             ")),     
               
                    DT::dataTableOutput("tab1_fin_table")),
           
           br(), br(),
         )
),

###############################################################################
# ABA C1 UI: OFERTA EPT — PROJEÇÕES FUTURAS
# Tendências de matrícula vs metas Meta 11 do PNE com configuração de metas personalizadas
###############################################################################            
tabPanel("C1. Oferta EPT (Futuro)",
         div(class = "tab-simple-layout",  #
             fluidPage(
               h3("Evolução da Oferta de EPT por UF", 
                  style = "color: #1f5673; font-weight: bold;"),
               
               div(class = "topbar-info",
                   fluidRow(
                     column(
                       width = 12,
                       div(
                         style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;",
                         tagList(
                           tags$strong("Instruções: "),
                           "Esta análise compara a oferta de educação técnica em relação à meta vigente do PNE. Seleccione
                           a UF, tipo de meta (PNE vigente ou meta preenchida) e a definição da Matriculas EPT desejada - 
                           com tres opções - como definido por PROPAG - integrado, concomitante e subsequente (EJA);
                           integrado e susequente ou só integrado."
                         )
                       )
                     )
                   ),
                   
                   fluidRow(
                     column(
                       width = 4,
                       tags$label("Selecionar UF ou Brasil:",
                                  style = "font-weight: bold; color: #333; font-size: 16px;"),
                       selectizeInput("oferta_uf", 
                                      label = NULL,
                                      choices = c("Brasil", sort(unique(meta11a_opcoes$NM_UF))),
                                      selected = "Rio Grande do Norte")
                     ),
                     
                     column(
                       width = 4,
                       tags$label("Tipo de Meta:",
                                  style = "font-weight: bold; color: #333; font-size: 16px;"),
                       selectizeInput("meta_target_type", 
                                      label = NULL,
                                      choices = list("Opção A: Meta PNE 11 vigente" = "pne11", 
                                                     "Opção B: Meta Definida" = "custom"),
                                      selected = "pne11"),
                       tags$small("Selecione Opção A (PNE 11 vigente) ou Opção B (Meta definida por você)", 
                                  style = "color: #666; display: block; margin-top: 5px;font-size: 16px"),
                       conditionalPanel(
                         condition = "input.meta_target_type == 'custom'",
                         style = "margin-top: 10px;",
                         numericInput("custom_meta_value", "Valor Meta:", 
                                      value = NA, min = 0, step = 1000, width = "100%")
                       )
                     ),
                     
                     column(
                       width = 4,
                       tags$label("Variável EPT:",
                                  style = "font-weight: bold; color: #333; font-size: 16px;"),
                       selectizeInput("oferta_ept_var", 
                                      label = NULL,
                                      choices = ept_vars_friendly,
                                      selected = "QT_MAT_PROF_TEC_PROPAG")
                     )
                   )
               ),
               
               fluidRow(
                 column(12,
                        plotOutput("oferta_ept_plot", height = "500px")
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(12,
                        h3("Tabela de Dados (2007–2024)", 
                           style = "color: #1f5673; font-weight: bold;"),
                        div(class = "checkbox-dark-panel",
                            tags$style(HTML("
          #oferta_ept_table thead { 
            background-color: #34495e !important; 
          }
          #oferta_ept_table thead th { 
            background-color: #34495e !important; 
            color: #ffffff !important;
            border-bottom: 2px solid #4f5f6f !important;
          }
          #oferta_ept_table tbody td {
            color: #ffffff !important;
          }
        ")),
                            DTOutput("oferta_ept_table")
                        )
                 )
               )
             )
         )
),
###############################################################################
# ABA C2 UI: OFERTA EPT — PLANIFICAÇÃO (COMPARAÇÕES META 11a)
# Crescimento anual de matrículas necessário para atingir meta no ano selecionado
###############################################################################     

tabPanel("C2. Oferta EPT (Planificação)",
         fluidPage(
           h3("Meta 11a – Comparação das Definições", style = "color: #1f5673; font-weight: bold;"),
            div(class = "topbar-info",
               div(style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;", 
                   tagList(
                     tags$strong("Instruções: "),
                     "Esta é uma continuação da aba anterior. Aqui, você pode escolher a UF, escolher o Meta 11 (predefinido como 
                     50% da matrícula do Ensino Médio) ou especificar seu próprio Meta 11 (para qualquer uma das três definições de EPT,
                     como antes). Se você escolher o Meta 11 predefinido, também precisará definir a projeção de crescimento do
                     Ensino Médio: igual ao anterior, menor que o anterior ou maior que o anterior, movendo a escala indicada. 
                     Por fim, escolha o ano em que a meta do Meta 11 deverá ser atingida. O gráfico mostrará o aumento anual que você 
                     precisará planejar para atingir a meta. Você pode revisar qualquer uma das opções acima para uma exploração 
                     interativa de diferentes opções.", 
                   ))),
           fluidRow(
             column(3,
                    # Linha superior: seletor de UF
                    selectizeInput(
                      inputId = "meta11a_nova_uf",
                      label = "Selecionar UF or Brasil:",
                      choices = sort(unique(meta11a_opcoes$NM_UF)),
                      selected = "Rio de Janeiro"
                    ),
                    
                    # Linha inferior: seleção Meta lado a lado
                    fluidRow(
                      column(6,
                             selectizeInput(
                               inputId = "meta11a_target_type",
                               label = "Tipo de Meta:",
                               choices = list("Meta PNE 11a (50%)" = "pne11a", 
                                              "Meta Definida" = "custom"),
                               selected = "pne11a"
                             )
                      ),
                      column(6,
                             conditionalPanel(
                               condition = "input.meta11a_target_type == 'custom'",
                               numericInput(
                                 inputId = "meta11a_custom_target",
                                 label = "Meta (alunos):",
                                 value = 100000,
                                 min = 0,
                                 step = 1000
                               )
                             )
                      )
                    )
             ),
             column(3,
                    checkboxGroupInput(
                      inputId = "meta11a_nova_definicoes",
                      label = "Escolher definições para comparar:",
                      choices = meta11a_choices_friendly,  # Usar o vetor nomeado
                      selected = c("Meta11a_opcao1", "Meta11a_opcao2", "Meta11a_opcao3"),
                      inline = FALSE  # Alterado para FALSE pois os rótulos são longos
                    )
             ),
             column(3,
                    selectInput(
                      inputId = "meta11a_target_year",
                      label = "Ano Alvo para atingir meta:",
                      choices = 2025:2035,
                      selected = 2030
                    )
             ),
             column(3,
                    sliderInput(
                      inputId = "ensino_slope_factor",
                      label = "Ajuste no crescimento do EM:",
                      min = 0.5, max = 1.5, step = 0.1, value = 1
                    )
             )
           ),
           
           plotOutput("meta11a_nova_plot", height = "600px")
         )
),
###############################################################################
# ABA C3 UI: OFERTA EPT — REDES (CENSO ESCOLAR DETALHADO)
# Matrículas por eixo, curso, dependência administrativa e município
###############################################################################  



tabPanel("C3. Oferta EPT (Redes)",
         fluidPage(
           h3("Detalhe da Oferta - Censo Escolar EPT", 
              style = "color: #1f5673; font-weight: bold;"),
           div(class = "topbar-info",
               div(style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;", 
                   tagList(
                     tags$strong("Instruções: "),
                     "Este painel apresenta dados detalhados de matrículas em educação profissional técnica por eixo tecnológico e curso.
          Dados do Censo Escolar 2023-2024 com informações por dependência administrativa e modalidades de ensino.
          Use os filtros hierárquicos para navegar entre níveis geográficos e dependências administrativas. Este aplicativo fornece ao servidor a capacidade de obter 
           estatísticas de inscrição por qualquer divisão de rede de serviço e hierarquia geográfica.",
                   ))),
           
           sidebarLayout(
             # UI - Remover seletores de região intermediária/imediata
             sidebarPanel(
               width = 3,
               
               h4("Filtros Temporais", style = "color: #1f5673;"),
               
               div(style = "margin-bottom: 15px;",
                   tags$style(HTML("
        .radio label { color: black !important; }
        .radio input[type='radio'] + span { color: black !important; }
      ")),
                   radioButtons("censo_year", "Ano:",
                                choices = c("2023" = 2023, "2024" = 2024),
                                selected = default_year_censo, inline = TRUE)
               ),
               
               hr(),
               h4("Filtros Geográficos", style = "color: #1f5673;"),
               
               pickerInput("censo_uf", 
                           "UF(s):",
                           choices = uf_choices_censo,
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE,
                           selected = default_uf_censo),
               
               pickerInput("censo_municipio",
                           "Município(s):",
                           choices = character(0),
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE),
               
               hr(),
               h4("Filtros Administrativos", style = "color: #1f5673;"),
               
               pickerInput("censo_dependencia",
                           "Dependência Administrativa:",
                           choices = c("Federal" = "1", "Estadual" = "2", "Municipal" = "3", "Privada" = "4"),
                           options = list(`actions-box` = TRUE),
                           multiple = TRUE,
                           selected = c("1", "2", "3", "4"))
             ),
             
             mainPanel(
               width = 9,
               # Gráfico de barras empilhado: matrículas EPT por modalidade e dependência administrativa
               fluidRow(
                 column(12,
                        h4("Visualização de Matrículas EPT por Modalidade", style = "color: #1f5673;"),
                        
                        withSpinner(plotlyOutput("censo_stacked_plot", height = "500px"))
                 )
               ),
               
               br(),
               hr(),
               # Tabela 1a - Nível Eixo
               fluidRow(
                 column(12,
                        h4("1a - Matrículas por Eixo Tecnológico", style = "color: #1f5673;"),
                        div(class = "checkbox-dark-panel",
                            tags$style(HTML("
          #censo_table_eixo thead { 
            background-color: #34495e !important; 
          }
          #censo_table_eixo thead th { 
            background-color: #34495e !important; 
            color: #ffffff !important;
            border-bottom: 2px solid #4f5f6f !important;
          }
          #censo_table_eixo tbody td {
            color: #ffffff !important;
          }
        ")),            
                        withSpinner(DTOutput("censo_table_eixo"))
                 )
                 ) 
               ),
               
               br(),
               
               # Tabela 1b - Nível Curso  
               fluidRow(
                 column(12,
                        h4("1b - Matrículas por Curso", style = "color: #1f5673;"),
                        div(class = "checkbox-dark-panel",
                            tags$style(HTML("
          #censo_table_curso thead { 
            background-color: #34495e !important; 
          }
          #censo_table_curso thead th { 
            background-color: #34495e !important; 
            color: #ffffff !important;
            border-bottom: 2px solid #4f5f6f !important;
          }
          #censo_table_curso tbody td {
            color: #ffffff !important;
          }
        ")),  
                        withSpinner(DTOutput("censo_table_curso"))
                        )    
                 )
               )
             )
           )
         )
),



###############################################################################
# ABA C4 UI: OFERTA EPT — MODELO ECONOMÉTRICO
# Análise de resíduos: matrículas EPT reais vs previstas por UF e rede
###############################################################################  

tabPanel("C4. Oferta EPT (Modelo)",
         
         h3("Modelo Econométrico de Matrículas EPT - Análise de Desempenho", 
            style = "color: #1f5673; font-weight: bold;"),
         
         div(class = "topbar-info",
             div(style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;", 
                 tagList(
                   tags$strong("Instruções: "),
                   "Este modelo econométrico prevê matrículas EPT baseado em PIB per capita e alinhamento setorial. ",
                   "Residuais positivos (direita) indicam desempenho acima do esperado; negativos (esquerda) abaixo do esperado. ",
                   "Selecione ano, rede, setor e estados para análise comparativa."
                 ))),
         
         sidebarLayout(
           sidebarPanel(
             width = 3,
             
             h4("Controles Temporais", style = "color: #1f5673;"),
             
             sliderInput(
               "tab_resi_year", "Ano:",
               min = 2011, max = 2021, value = 2016, step = 1
             ),
             
             hr(),
             h4("Filtros de Programa", style = "color: #1f5673;"),
             
             # Substituir widgets selectInput por pickerInput para seleção múltipla:
             
             pickerInput("tab_resi_dependency",
                         label = h5("Dependência Administrativa:", style = "color: black;"),
                         choices = c("Federal" = "Federal",
                                     "Estadual" = "Estadual", 
                                     "Municipal" = "Municipal",
                                     "Privada" = "Privada"),
                         selected = "Federal",
                         multiple = TRUE,
                         options = list(`actions-box` = TRUE)),
             
             pickerInput("tab_resi_sector",
                         label = h5("Setor Econômico:", style = "color: black;"),
                         choices = c("Agricultura" = "agriculture",
                                     "Indústria" = "industry",
                                     "Serviços" = "services", 
                                     "Administração" = "administration"),
                         selected = "industry",
                         multiple = TRUE,
                         options = list(`actions-box` = TRUE)),
             hr(),
             h4("Configuração do Gráfico", style = "color: #1f5673;"),
             
             selectInput("tab_resi_yaxis",
                         label = h5("Eixo Y:", style = "color: black;"),
                         choices = c("PIB per Capita" = "pib_per_capita",
                                     "Alinhamento Setorial" = "sector_alignment"),
                         selected = "pib_per_capita"),
             
             pickerInput(
               "tab_resi_states", "Estados Selecionados:",
               choices = NULL,
               selected = NULL,
               multiple = TRUE,
               options = list(`actions-box` = TRUE, `live-search` = TRUE)
             ),
             
             hr(),
             h5("Resumo da Seleção:", style = "color: #1f5673;"),
             uiOutput("tab_resi_summary")
           ),
           
           mainPanel(
             width = 9,
             h4("Desempenho Institucional vs Contexto Econômico", style = "color: #1f5673;"),
             withSpinner(plotlyOutput("tab_resiPlot", height = "600px")),
             
             br(),
             
             h4("Dados dos Estados Selecionados", style = "color: #1f5673;"),
             div(class = "checkbox-dark-panel",
                 tags$style(HTML("
                #tab_resiTable thead { background-color: #34495e !important; }
                #tab_resiTable thead th { 
                background-color: #34495e !important; 
                 color: #ffffff !important; 
                 }
             ")),     
             withSpinner(DTOutput("tab_resiTable"))
           )
           )
         )
),



###############################################################################
# ABA D1 UI: DINAMISMO ECONÔMICO
# Mapa e tabela de índice de dinamismo municipal com indicadores de crescimento do PIB
###############################################################################    

  tabPanel("D1. Demanda EPT: Dinamismo Econômico",
           fluidPage(
             h3("Dinamismo Econômico Municipal - Índice de Crescimento", style = "color: #1f5673; font-weight: bold;"),
             
             div(class = "topbar-info",
                 div(style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;", 
                     tagList(
                       tags$strong("Instruções: "),
                       "Este painel permite explorar o dinamismo econômico municipal com base no crescimento do PIB per capita. ", 
                       "O índice combina dois períodos: 2002-2011 (peso 1.0) e 2012-2021 (peso 1.5), classificando municípios em decis. ", 
                       "Use os filtros hierárquicos para navegar entre níveis geográficos e identificar regiões prioritárias para investimento EPT."
                     )
                 )
             ),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 
                 h4("Filtros Geográficos", style = "color: #1f5673;"),
                 
                 pickerInput(
                   "uf_dyn", "UF(s):",
                   choices = NULL,
                   selected = "São Paulo",
                   multiple = TRUE,
                   options = list(`actions-box` = TRUE, `live-search` = TRUE)
                 ),
                 
                 pickerInput(
                   "rgintm_dyn", "Região(ões) Intermediária(s):",
                   choices = NULL, selected = NULL, multiple = TRUE,
                   options = list(`actions-box` = TRUE, `live-search` = TRUE)
                 ),
                 
                 pickerInput(
                   "rgimed_dyn", "Região(ões) Imediata(s):",
                   choices = NULL, selected = NULL, multiple = TRUE,
                   options = list(`actions-box` = TRUE, `live-search` = TRUE)
                 ),
                 
                 pickerInput(
                   "mun_dyn", "Município(s):",
                   choices = NULL, selected = NULL, multiple = TRUE,
                   options = list(`actions-box` = TRUE, `live-search` = TRUE)
                 ),
                 
                 hr(),
                 h4("Filtros de Performance", style = "color: #1f5673;"),
                 
                 sliderInput(
                   "min_decile_dyn", "Decil Mínimo:",
                   min = 1, max = 10, value = 1, step = 1
                 ),
                 
                 sliderInput(
                   "min_pop_dyn", "População Mínima:",
                   min = 1000, max = 100000, value = 5000, step = 1000
                 ),
                 
                 hr(),
                 h5("Resumo da Seleção:", style = "color: #1f5673;"),
                 uiOutput("dyn_summary")
               ),
               
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(12,
                          h4("Mapa de Dinamismo Econômico", style = "color: #1f5673;"),
                          withSpinner(leafletOutput("dyn_map", height = "500px"))
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h4("Municípios na Região Selecionada", style = "color: #1f5673;"),
                          div(class = "checkbox-dark-panel",
                              tags$style(HTML("
          #dyn_table thead { 
            background-color: #34495e !important; 
          }
          #dyn_table thead th { 
            background-color: #34495e !important; 
            color: #ffffff !important;
            border-bottom: 2px solid #4f5f6f !important;
          }
          #dyn_table tbody td {
            color: #ffffff !important;
          }
        ")),
                              withSpinner(DTOutput("dyn_table"))
                          )
                   )
                 )
               )
             )
           )
  ),
  

###############################################################################
# ABA D2 UI: ARRANJOS PRODUTIVOS LOCAIS (APLs)
# Mapeamento de especialização ocupacional por CBO com correspondência de cursos
###############################################################################    

  tabPanel("D2. Demanda EPT: APLs",
           
           fluidPage(
             
             
             tags$style(HTML("
/* Scope to the DT with id 'apl_table' only */
#apl_table table.dataTable thead th,
#apl_table table.dataTable thead td {
  background-color: #20313d !important;   /* dark header */
  color: #e8eef5 !important;               /* light text */
  border-color: #2b3b48 !important;
}

/* When columns are sortable, DT adds these classes—cover them too */
#apl_table table.dataTable thead th.sorting,
#apl_table table.dataTable thead th.sorting_asc,
#apl_table table.dataTable thead th.sorting_desc {
  background-color: #20313d !important;
  background-image: none !important;
  color: #e8eef5 !important;
}

/* Optional: keep search + length controls readable in this block */
#apl_table .dataTables_filter input,
#apl_table .dataTables_length select {
  background: #0f1b22 !important;
  color: #e8eef5 !important;
  border: 1px solid #355468 !important;
}
")),
             
             h3("Arranjos Produtivos Locais - Base CBO", style = "color: #1f5673; font-weight: bold;"),
             
             div(class = "topbar-info",
                 div(style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;", 
                     tagList(
                       tags$strong("Instruções: "),
                       "Este painel identifica Arranjos Produtivos Locais baseados em especialização ocupacional (CBO), usando quociente locacional (QL≥1) com mínimo de 30 vínculos. ",
                       "A análise mapeia concentrações de emprego formal persistentes (2023-2024) e sugere cursos técnicos correspondentes via similaridade semântica. ",
                       "Use os filtros hierárquicos para navegar entre UF, regiões e municípios, ajustando critérios de especialização conforme necessário."
                     ))),
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 
                 div(style = "margin-bottom: 15px;",
                     tags$style(HTML("
                    .radio label {
                     color: black !important;
                        }
                     .radio input[type='radio'] + span {
                       color: black !important;
                             }
                    ")),
                     radioButtons("apl_analysis_mode",
                                  label = h5("Modo de Análise:", style = "color: black; margin-bottom: 8px;"),
                                  choices = c("Análise de APLs" = "apl_only",
                                              "Correspondência APL-Cursos" = "apl_course_match"),
                                  selected = "apl_only",
                                  inline = FALSE)
                 ),
                 h4("Filtros Geográficos", style = "color: #1f5673;"),
                 
                 pickerInput(
                   "uf_apl", "UF(s):",
                   choices = NULL,
                   selected = "Ceará",
                   multiple = TRUE,
                   options = list(`actions-box` = TRUE, `live-search` = TRUE)
                 ),
                 
                 pickerInput(
                   "rgintm_apl", "Região(ões) Intermediária(s):",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(`actions-box` = TRUE, `live-search` = TRUE)
                 ),
                 
                 pickerInput(
                   "rgimed_apl", "Região(ões) Imediata(s):",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(`actions-box` = TRUE, `live-search` = TRUE)
                 ),
                 
                 pickerInput(
                   "mun_apl", "Município(s):",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(`actions-box` = TRUE, `live-search` = TRUE)
                 ),
                 
                 hr(),
                 h4("Filtros de Especialização", style = "color: #1f5673;"),
                 
                 sliderInput(
                   "min_lq_apl", "QL Mínimo:",
                   min = 1, max = 5, value = 1.25, step = 0.1
                 ),
                 
                 sliderInput(
                   "min_emp_apl", "Emprego Mínimo:",
                   min = 50, max = 500, value = 100, step = 10
                 ),
                 
                 hr(),
                 h5("Resumo da Seleção:", style = "color: #1f5673;"),
                 uiOutput("apl_summary")
               ),
               
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(12,
                          h4("Mapa de APLs", style = "color: #1f5673;"),
                          
                          withSpinner(leafletOutput("apl_map", height = "500px"))
                   )
                 ),
                 
                 br(),
                 div(class = "checkbox-dark-panel",
                 fluidRow(
                   column(12,
                          h4("APLs na Região Selecionada", style = "color: #1f5673;"),
                          withSpinner(DTOutput("apl_table"))
                   ))
                 )
               )
             )
           )
  ),

###############################################################################
# ABA D3 UI: ANÁLISE DE INFORMALIDADE
# Distribuição de emprego formal/informal com mapeamento de oportunidades de formalização
###############################################################################    


tabPanel("D3. Demanda EPT (Informalidade)",
         fluidPage(
           h3("EPT e Informalidade - Análise de Oportunidades de Formalização", 
              style = "color: #1f5673; font-weight: bold;"),
           div(class = "topbar-info",
               div(style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;", 
                   tagList(
                     tags$strong("Instruções: "),
                     "Este painel analisa a estrutura do emprego formal e informal por município, usando dados RAIS (vínculos formais) expandidos com fatores de 
          estimação da PNAD-C para capturar emprego total. ",
                     "Municípios são classificados em decis (1-10) baseados na taxa de formalização dentro de cada estado e setor econômico, organizados pela
          Classificação Brasileira de Ocupações (CBO). ",
                     "Decil 10 indica alta formalização; decil 1 indica alta informalidade - sinalizando oportunidades para expansão EPT. ",
                     "Use os filtros para identificar territórios e setores onde a qualificação técnica pode apoiar melhoria
          de produtividade através do aprimoramento de competências profissionais."
                   ))), 
           sidebarLayout(
             sidebarPanel(
               width = 3,
               
               h4("Filtros Temporais e Geográficos", style = "color: #1f5673;"),
               
               # Seletor de ano
               div(style = "margin-bottom: 15px;",
                   tags$style(HTML("
              .radio label { color: black !important; }
              .radio input[type='radio'] + span { color: black !important; }
            ")),
                   radioButtons("informality_year", "Ano:",
                                choices = c("2023" = 2023, "2024" = 2024),
                                selected = 2023, inline = TRUE)
               ),
               
               radioButtons("map_employment_type", 
                            label = h5("Métrica do Mapa:", style = "color: black; margin-bottom: 8px;"),
                            choices = c("Vínculos Formais" = "formal", 
                                        "Vínculos Totais" = "total"),
                            selected = "total", inline = FALSE),
               
               # Hierarquia geográfica
               pickerInput("informality_uf", 
                           "UF(s):",
                           choices = uf_choices_all,
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE,
                           selected = default_uf),
               
               pickerInput("informality_reg_inter",
                           "Região(ões) Intermediária(s):",
                           choices = character(0),
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE),
               
               pickerInput("informality_reg_imed",
                           "Região(ões) Imediata(s):",
                           choices = character(0),
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE),
               
               pickerInput("informality_municipio",
                           "Município(s):",
                           choices = character(0),
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE),
               
               hr(),
               h4("Filtros Ocupacionais", style = "color: #1f5673;"),
               
               pickerInput("cbo_grande_grupo",
                           "Grande Grupo:",
                           choices = NULL,
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE),
               
               pickerInput("cbo_grupo_primario", 
                           "Grupo Primário:",
                           choices = character(0),
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE),
               
               pickerInput("cbo_subgrupo",
                           "Subgrupo:",
                           choices = character(0),
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE),
               
               pickerInput("cbo_familia",
                           "Família:",
                           choices = character(0),
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE),
               
               pickerInput("nivel_ocupacao", 
                           "Nível Ocupação:",
                           choices = NULL,
                           options = list(`actions-box` = TRUE, `live-search` = TRUE),
                           multiple = TRUE),
               
               hr(),
               h5("Resumo da Seleção:", style = "color: #1f5673;"),
               uiOutput("informality_summary")
             ),
             
             mainPanel(
               width = 9,
               fluidRow(
                 column(12,
                        h4("Mapa de Emprego por Decil", style = "color: #1f5673;"),
                        withSpinner(leafletOutput("informality_map", height = "500px"))
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(12,
                        h4("Detalhamento por Município/Região", style = "color: #1f5673;"),
                        div(class = "checkbox-dark-panel",
                            tags$style(HTML("
          #informality_table thead { 
            background-color: #34495e !important; 
          }
          #informality_table thead th { 
            background-color: #34495e !important; 
            color: #ffffff !important;
            border-bottom: 2px solid #4f5f6f !important;
          }
          #informality_table tbody td {
            color: #ffffff !important;
          }
        ")),
            withSpinner(DTOutput("informality_table"))
                        )
                 )
               )
             )
           )
         )
),

###############################################################################
# ABA E1 UI: CORRESPONDÊNCIA OFERTA-DEMANDA (CNCT ↔ CBO)
# Correspondência curso-ocupação via TF-IDF e embeddings semânticos com IA
############################################################################### 

tabPanel("E1. Oferta e Demanda EPT",
         h3("Oferta e Demanda EPT", 
           style = "color: #1f5673; font-weight: bold;"),
         fluidPage(
           useShinyjs(),
           tags$head(includeCSS("www/custom.css")),
           div(class = "topbar-info",
               div(style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;", 
                   tagList(
                     tags$strong("Instruções: "),
                     "Este painel conecta oferta educacional (cursos EPT) com demanda laboral (ocupações) através de análise computacional de similaridade textual. ",
                     "O sistema compara descrições de ocupações do QBQ (Quadro Básico de Qualificações) com descrições de cursos do CNCT (Catálogo Nacional de Cursos Técnicos) usando duas técnicas: TF-IDF (frequência de termos) e embeddings semânticos (vetores de significado). ",
                     "A pontuação final combina ambas métricas para identificar correspondências entre cursos e ocupações. ",
                     tags$strong("Importante: "),
                     "A conexão curso-ocupação ocorre apenas no nível mais detalhado (tabela 1c). As tabelas 1a e 1b mostram agregações hierárquicas, mas somente seleções na tabela 1c ativam as correspondências na coluna direita. ",
                     "Selecione 'Oferta→Demanda' para ver ocupações correspondentes aos cursos, ou 'Demanda→Oferta' para identificar cursos para ocupações. ",
                     "Ajuste o limite de proximidade (0-1) para controlar precisão - valores menores exigem maior similaridade."
                   ))),
           
           div(class = "checkbox-dark-panel",
               fluidRow(
                 column(
                   width = 3,
                   selectizeInput("uf_selectCOCN", "Selecionar UF ou Brasil:",
                                  choices = NULL,
                                  options = list(placeholder = "Brasil"))
                 ),
                 column(
                   width = 3,
                   radioButtons("match_direction", "Direção da Conexão:",
                                choices = c("Oferta → Demanda", "Demanda → Oferta"),
                                selected = "Oferta → Demanda",
                                inline = TRUE)
                 ),
                 column(
                   width = 2,
                   sliderInput("score_thresh", "Limite de Proximidade:",
                               min = 0, max = 1, value = 0.2, step = 0.01)
                 ),
                 column(
                   width = 2,
                   numericInput("top_n", "Melhores Correspondências:",
                                value = 5, min = 1, max = 10, step = 1),
                   helpText("Atualizado automaticamente com base no limite de proximidade.")
                 )
               ),
               fluidRow(
                 # OFERTA → DEMANDA
                 conditionalPanel(
                   condition = "input.match_direction == 'Oferta \u2192 Demanda'",
                   column(width = 6,
                          h4("1a - Matrículas por Eixo, Área e Curso"),
                          DTOutput("agg_table"), hr(),
                          h4("1b - Matrículas por Área"),
                          DTOutput("area_table"), hr(),
                          h4("1c - Matrículas por Curso"),
                          DTOutput("curso_table")
                   ),
                   column(width = 6,
                          h4("Melhores Ocupações e Vínculos"),
                          DTOutput("course_occ_table")
                   )
                 ),
                 # DEMANDA → OFERTA
                 conditionalPanel(
                   condition = "input.match_direction == 'Demanda \u2192 Oferta'",
                   column(width = 6,
                          h4("1a - Vínculos por Grande Grupo"),
                          DTOutput("cbo1_table"), hr(),
                          h4("1b - Vínculos por Família"),
                          DTOutput("cbo4_table"), hr(),
                          h4("1c - Vínculos por Ocupação"),
                          DTOutput("cbo6b_table")
                   ),
                   column(width = 6,
                          h4("Melhores Cursos e Matrículas"),
                          DTOutput("course_agg_table_rev")
                   )
                 )
               )
           )
         )
),

###############################################################################################################
###############################################################################
# ABA E2 UI: ESCASSEZ DE PROFISSIONAIS TÉCNICOS
# Indicadores de escassez laboral CAGED/RAIS por curso e UF
###############################################################################
############################################################################################################### 

tabPanel("E2. Escassez de Profissionais Técnicos", 
         fluidPage(h3("🔎 Explore a Demanda e Escassez de Profissionais Técnicos no Brasil"),
                   tags$head(
                     tags$style(HTML(
                       ".bigtext {font-size: 1.2em; color: #333 !important;}
   .bullet-list {font-size: 1.1em; margin-top: 12px; color: #333 !important;}
   .input-box {
     background: #F6F6F6; 
     padding: 15px; 
     border-radius: 12px; 
     margin-bottom: 10px;
     color: #333 !important;
   }
   .input-box label {color: #333 !important;}
   .input-box .form-control {color: #333 !important;}
   .input-box .selectize-input {color: #333 !important;}
   .topbar-info {
     background: #F0F8FF; 
     padding: 10px; 
     border-radius: 10px; 
     margin-bottom: 14px;
     color: #333 !important;
   }
   .topbar-info p {color: #333 !important;}
   
   /* Fix for all text in this tab */
   h3, h4, h5, p, label, .control-label {
     color: #333 !important;
   }
   
   /* Fix for selectize inputs */
   .selectize-control .selectize-input {
     color: #333 !important;
   }
   .selectize-dropdown {
     color: #333 !important;
   }

   table {
     color: #333 !important;
   }
   table th {
     color: #333 !important;
     background-color: #f5f5f5 !important;
   }
   table td {
     color: #333 !important;
   }
   .table-striped > tbody > tr:nth-of-type(odd) {
     background-color: #f9f9f9 !important;
   }
   .table-bordered {
     border-color: #ddd !important;
   }"
                       
                       
                       
                     ))
                   ),
                   
                   #  titlePanel("🔎 Explore a Demanda e Escassez de Profissionais Técnicos no Brasil"),
                   
      div(class = "topbar-info",
     div(style = "margin-top: 5px; margin-bottom: 10px; color: #333; text-align: justify; font-size: 20px;", 
     tagList(
       tags$strong("Instruções: "),
         "Este painel permite explorar a demanda e escassez de profissionais técnicos no Brasil, por estado, eixo e curso, com base nos dados
          do CAGED e da RAIS. As ocupações são associadas aos cursos técnicos correspondentes, permitindo observar a movimentação do mercado de trabalho agregados no nível do curso técnico específico.
          O indicador principal de escassez é o diferencial salarial entre admitidos e desligados. Valor alto sinaliza escassez e valores baixos sinalizam abundância de trabalhadores.
          De forma complementar, a taxa de rotatividade e a participação da ocupação no total de vínculos na UF também são utilizadas: se o diferencial salarial se torna menor (indicando maior pressão salarial), a taxa de rotatividade é crescente 
          e as ocupações para as quais o curso forma têm historicamente muitos vínculos, acende-se um alerta de escassez.",
                           ))),
                   
                   fluidRow(
                     column(2, class = "input-box",
                            selectInput("ranking_criterio", "Critério de Ranking:",
                                        choices = c(
                                          "Diferença Salarial Admitidos e Desligados (%)" = "dif_sal_adm_des_pc_m12",
                                          "Total Vínculos" = "estoque_liquido",
                                          "Diferença Salarial Ponderada pelo Total de Vínculos (escala 0 a 1000)" = "dif_sal_adm_des_pc_pond_m12"
                                        ),
                                        selected = "dif_sal_adm_des_pc_pond_m12"
                            ),
                            bsTooltip("ranking_criterio", "Escolha o indicador para ordenar o ranking dos Top 5 cursos.", placement = "right", trigger = "hover"),
                            selectInput("uf1", "Selecionar UF ou Brasil:", 
                                        choices = sort(unique(caged_rais_curso$NM_UF)),
                                        selected = "Brasil"
                            ),
                            bsTooltip("uf1", "Selecione uma unidade da federação para analisar os dados de mercado de trabalho técnico. A opção \"Brasil\" mostra o resultado agregando todas as UFs.", placement = "right", trigger = "hover")
                     ),
                     column(10,
                            uiOutput("ranking_uf_title"),
                            bsTooltip("ranking_uf_title", "Ranking dos 5 cursos técnicos com maior destaque segundo o critério selecionado.", placement = "top", trigger = "hover"),
                            withSpinner(tableOutput("ranking_uf"))
                     )
                   ),
                   
                   fluidRow(
                     column(2, selectizeInput("eixos", "Selecionar Eixo(s) Tecnológico(s):", choices = NULL, multiple = TRUE)),
                     column(2, checkboxInput("todos_no_eixo", "\U0001F4CC Considerar todos os Cursos nos Eixo(s) selecionados", value = FALSE)),
                     column(2, selectizeInput("curso", "Selecionar Curso(s):", choices = NULL, multiple = TRUE)),
                     column(2, checkboxInput("comparar_brasil", "🇧🇷 Adiciona comparação com Brasil no gráfico", value = FALSE)),
                     bsTooltip("comparar_brasil", "Marque para comparar o gráfico da UF escolhida com o Brasil.", placement = "top", trigger = "hover")
                   ),
                   
                   fluidRow(
                     column(2, 
                            h5("Seleção atual:"),
                            uiOutput("selecao_atual"),
                     ),
                     column(2, actionButton("clear_filters", "Limpar Seleções", icon = icon("broom")))
                   ),
                   
                   fluidRow(
                     column(6,
                            h4("\U0001F4C8 Diferença Salarial Admitidos vs Desligados (%)", id = "plot_dif_salarial_title"),
                            bsTooltip("plot_dif_salarial_title", "Variação percentual entre salários dos admitidos e desligados. Valores altos sugerem escassez de mão de obra. É o indicador principal para a análise de escassez relativa.", placement = "top", trigger = "hover"),
                            withSpinner(plotOutput("plot_dif_salarial"))
                     ),
                     column(6,
                            h4("\U0001F501 Taxa de Rotatividade", id = "plot_rotatividade_title"),
                            bsTooltip("plot_rotatividade_title", "Mede o fluxo de admissões e desligamentos em relação ao estoque de vínculos ativos. Serve para complementar a análise de escassez relativa.", placement = "top", trigger = "hover"),
                            withSpinner(plotOutput("plot_rotatividade"))
                     )
                   ),
                   
                   fluidRow(
                     column(12,
                            h4("\U0001F4AC Avaliação Escassez a Partir do Gráfico", id = "avaliacao_escassez_title"),
                            bsTooltip("avaliacao_escassez_title", "Classificação da situação de escassez laboral considerando conjuntamente percentis do indicador de diferença salarial, taxa de rotatividade e estoque. Verificar nota técnica xxxx para detalhamento da metodologia.", placement = "top", trigger = "hover"),
                            htmlOutput("texto_escassez")
                     )
                   )
                   
         
         )) ## FIM DA ABA


    )
  )
)

###############################################################################
# LÓGICA DO SERVIDOR
###############################################################################

server <- function(input, output, session) {

  # Handlers de links de navegação: clique nos links da aba de introdução muda para aba destino
  
  # Aba A1: Transição Demográfica  
  observeEvent(input$link_demo, {
    updateTabsetPanel(session, "tab_selection", selected = "A1. Transição Demográfica")
  })
  
  # Aba A2: EPT e População  
  observeEvent(input$link_ept, {
    updateTabsetPanel(session, "tab_selection", selected = "A2. EPT e População")
  })
  
  # Aba B1: Impacto Financeiro PROPAG
  observeEvent(input$link_impacto, {
    updateTabsetPanel(session, "tab_selection", selected = "B1. Financiamento PROPAG")
  })
  
  # Aba B2: Simulação FEF
  observeEvent(input$link_fef, {
    updateTabsetPanel(session, "tab_selection", selected = "B2. Retorno FEF")
  })
  
  # Aba B3: Finanças Comparativas
  observeEvent(input$link_finuf, {
    updateTabsetPanel(session, "tab_selection", selected = "B3. Financiamento (Comparativo)")
  })

  # Aba C1: Oferta EPT (Futuro)  
  observeEvent(input$link_meta11pro, {
    updateTabsetPanel(session, "tab_selection", selected = "C1. Oferta EPT (Futuro)")
  })
  # Aba C2: Oferta EPT (Planificação)  
  observeEvent(input$link_meta11pla, {
    updateTabsetPanel(session, "tab_selection", selected = "C2. Oferta EPT (Planificação)")
  })

  # Aba C3: Oferta EPT (Redes)  
  observeEvent(input$link_ofertadet, {
    updateTabsetPanel(session, "tab_selection", selected = "C3. Oferta EPT (Redes)")
  })    
 
  # Aba C4: Oferta EPT (Modelo)
    observeEvent(input$link_modelo, {
    updateTabsetPanel(session, "tab_selection", selected = "C4. Oferta EPT (Modelo)")
  })    

  
  # Aba D1: Dinamismo Econômico
  observeEvent(input$link_dinamismo, {
    updateTabsetPanel(session, "tab_selection", selected = "D1. Demanda EPT: Dinamismo Econômico")
  })

  # Aba D2: APLs
  observeEvent(input$link_arranjo, {
    updateTabsetPanel(session, "tab_selection", selected = "D2. Demanda EPT: APLs")
  })
    
  # Aba D3: Informalidade
  observeEvent(input$link_informal, {
    updateTabsetPanel(session, "tab_selection", selected = "D3. Demanda EPT (Informalidade)")
  })
  
  # Aba E1: Correspondência Oferta-Demanda
  observeEvent(input$link_oferta_demanda, {
    updateTabsetPanel(session, "tab_selection", selected = "E1. Oferta e Demanda EPT")
  })
  
  # Aba E2: Escassez de Profissionais Técnicos  
  observeEvent(input$link_escassez, {
    updateTabsetPanel(session, "tab_selection", selected = "E2. Escassez de Profissionais Técnicos")
  })
  

  ###############################################################################
  # SERVIDOR — ABA A1: TRANSIÇÃO DEMOGRÁFICA
  # Seleção reativa de variáveis e renderização plotly de projeções populacionais
  ###############################################################################
  
  # Atualizar dinamicamente seletor de variáveis com base no tipo de dados (números vs proporções)
output$demo_variableInput <- renderUI({
  if (input$demo_dataType == "numbers") {
    div(style = "background-color: white; border-radius: 4px;",
        pickerInput(
          "demo_yVariables",
          label = NULL,
          choices = demo_number_columns,
          options = list(`actions-box` = TRUE, style = "btn-default"),
          multiple = TRUE,
          selected = c("0-14_T", "60+_T")
        )
    )
  } else {
    div(style = "background-color: white; border-radius: 4px;",
        pickerInput(
          "demo_yVariables", 
          label = NULL,
          choices = demo_proportion_columns,
          options = list(`actions-box` = TRUE, style = "btn-default"),
          multiple = TRUE,
          selected = c("P_0_14_T", "P_60_plus_T")
        )
    )
  }
})
  
  # Renderizar gráfico de transição demográfica com anotações de cruzamento
  output$demo_linePlot <- renderPlotly({
    req(input$demo_yVariables)
    
    # Filtrar dados com base nas localidades selecionadas
    filtered_data <- pop01_70b %>%
      filter(LOCAL %in% input$demo_localInput)
    
    # Determinar rótulos e limites do eixo Y
    y_labels <- if (input$demo_dataType == "numbers") scales::comma else waiver()
    y_min <- 0
    y_max <- max(filtered_data[input$demo_yVariables], na.rm = TRUE)
    
    # Criar objeto base ggplot
    p <- ggplot(filtered_data, aes(x = ANO, color = LOCAL)) +
      labs(
        x = "Ano",
        y = ifelse(input$demo_dataType == "numbers", "Contagem Populacional", "Proporção"),
        title = "Projeções Populacionais IBGE 2000-2070",
        color = "Localização"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)
      ) +
      scale_y_continuous(limits = c(y_min, y_max), labels = y_labels) +
      scale_x_continuous(breaks = seq(2000, 2070, by = 10)) +
      scale_color_manual(values = demo_local_colors)
    
    # Tipos de linha e anotações Plotly
    line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    annotations <- list()
    
    # Adicionar linhas e anotações para cada variável Y selecionada
    for (loc in unique(filtered_data$LOCAL)) {
      loc_data <- filtered_data %>% filter(LOCAL == loc)
      loc_color <- demo_local_colors[loc]
      
      for (i in seq_along(input$demo_yVariables)) {
        y_var <- input$demo_yVariables[i]
        y_sym <- sym(y_var)
        line_type <- line_types[(i - 1) %% length(line_types) + 1]
        
        # Adicionar a linha para cada variável Y e LOCAL
        p <- p + geom_line(data = loc_data, aes(y = !!y_sym), 
                           linetype = line_type, linewidth = 1, color = loc_color)
        
        # Obter último ano e valor para rotulagem
        last_year <- max(loc_data$ANO)
        last_value <- loc_data %>%
          filter(ANO == last_year) %>%
          pull(!!y_sym)
        
        # Construir texto do rótulo
        label_text <- paste(y_var, "-", loc)
        
        # Anotação: texto preto, fonte grande, seta reta
        annotations <- append(annotations, list(
          list(
            x = last_year,
            y = last_value,
            text = label_text,
            showarrow = TRUE,
            arrowhead = 2,
            ax = 0,
            ay = 40,
            font = list(color = "black", size = 20, family = "Arial")
          )
        ))
      }
    }
    
    # Adicionar linha de cruzamento demográfico (onde 0-14 cruza 60+)
    if (input$demo_showTransition) {
      crossover_data <- filtered_data %>% filter(Crossover_Flag == 1)
      if (nrow(crossover_data) > 0) {
        for (loc in unique(crossover_data$LOCAL)) {
          crossover_year <- crossover_data %>% filter(LOCAL == loc) %>% pull(ANO)
          crossover_value <- if (input$demo_dataType == "numbers") {
            crossover_data %>% filter(LOCAL == loc) %>% pull(Crossover_Value_Num)
          } else {
            crossover_data %>% filter(LOCAL == loc) %>% pull(Crossover_Value_Prop)
          }
          
          # Anotação de cruzamento: texto preto, fonte grande, seta reta
          annotations <- append(annotations, list(
            list(
              x = crossover_year,
              y = crossover_value,
              text = paste(crossover_year, loc),
              showarrow = TRUE,
              arrowhead = 2,
              ax = 0,  # Straight arrow
              ay = 40,
              font = list(color = "black", size = 20, family = "Arial")  # Texto preto, fonte grande
            )
          ))
        }
      }
    }
    
    # Converter para gráfico Plotly interativo e adicionar anotações
    plotly_obj <- ggplotly(p)
    plotly_obj <- plotly_obj %>% layout(annotations = annotations)
    
    return(plotly_obj)
  })
  
  # NOTA: Observador de navegação duplicado para Aba A1 (mantido por compatibilidade)
  observeEvent(input$link_demo, {
    updateTabsetPanel(session, "tab_selection", selected = "Transição Demográfica")
  })
  
  ###############################################################################
  # SERVIDOR — ABA A2: MATRÍCULAS EPT E POPULAÇÃO
  # Seleção de variáveis, configuração de meta Meta 11, séries temporais plotly
  ###############################################################################
  
  
  # Atualizar dinamicamente seletor de variáveis com base no tipo de dados (absoluto vs percentual)
  output$ept_variableInput <- renderUI({
    if (input$ept_dataType == "numbers") {
      pickerInput(
        "ept_yVariables",
        label = "Selecionar Variável(eis) do Eixo Y:",
        choices = ept_number_variables,
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = c("15-19_T", "QT_MAT_PROF_TEC_PROPAG")
      )
    } else {
      pickerInput(
        "ept_yVariables",
        label = "Selecionar Variável(eis) do Eixo Y:",
        choices = ept_percentage_variables,
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = c("PCT_MAT_EPT")
      )
    }
  })
  
  output$ept_metaTargetsUI <- renderUI({
    req(input$ept_localInput)
    
    # Criar cabeçalho da tabela
    table_header <- tags$thead(
      tags$tr(
        tags$th("UF/REG/BRASIL", style = "width: 30%;"),
        tags$th("META 11 VIGENTE", style = "width: 35%;"),
        tags$th("META DEFINIDO (preenche)", style = "width: 35%;")
      )
    )
    
    # Criar linhas da tabela
    table_rows <- map(input$ept_localInput, ~{
      loc <- .x
      safe_id <- str_replace_all(loc, "[^A-Za-z0-9]", "_")
      
      # Consulta direta no data frame
      meta_value <- ept_meta11_df$meta11_absolute[ept_meta11_df$LOCAL == loc]
      if(length(meta_value) == 0) meta_value <- 0
      
      tags$tr(
        tags$td(strong(loc)),
        tags$td(
          checkboxInput(paste0("ept_use_meta11_", safe_id), 
                        paste0("Meta 11: ", round(meta_value)), 
                        value = TRUE)
        ),
        tags$td(
          numericInput(paste0("ept_custom_meta_", safe_id), 
                       "", 
                       value = NA, 
                       min = 0)
        )
      )
    })
    
    # Tabela completa
    tags$table(class = "table table-condensed",
               table_header,
               tags$tbody(table_rows)
    )
  })
  
  # Exclusão mútua: selecionar checkbox Meta 11 limpa entrada customizada e vice-versa
  observe({
    req(input$ept_localInput)
    
    walk(input$ept_localInput, ~{
      loc <- .x
      safe_id <- str_replace_all(loc, "[^A-Za-z0-9]", "_")
      meta11_id <- paste0("ept_use_meta11_", safe_id)
      custom_id <- paste0("ept_custom_meta_", safe_id)
      
      # Verificar se inputs existem antes de usá-los
      if(!is.null(input[[meta11_id]]) && !is.null(input[[custom_id]])) {
        # Se Meta 11 está marcada, limpar e desabilitar entrada customizada
        if(isTruthy(input[[meta11_id]])) {
          updateNumericInput(session, custom_id, value = NA)
        }
        
        # Se valor customizado é inserido, desmarcar Meta 11
        if(!is.na(input[[custom_id]]) && input[[custom_id]] > 0) {
          updateCheckboxInput(session, meta11_id, value = FALSE)
        }
      }
    })
  })
  
  # Lógica de plotagem atualizada
  output$ept_linePlot <- renderPlotly({
    req(input$ept_yVariables, input$ept_localInput)
    
    # Filtrar dados com base nas localidades selecionadas
    filtered_data <- ept_combined_data %>%
      filter(LOCAL %in% input$ept_localInput)
    
    # Obter opções de variáveis atuais com base no tipo de dados
    current_variables <- if (input$ept_dataType == "numbers") ept_number_variables else ept_percentage_variables
    
    # Determinar limites e rótulos do eixo Y com base no tipo de dados
    if (input$ept_dataType == "numbers") {
      y_min <- 0
      y_max <- max(filtered_data[input$ept_yVariables], na.rm = TRUE) * 1.1
      y_labels <- scales::comma
      y_axis_title <- "Contagem"
      plot_title <- "População 15-19 anos e Matrículas EPT/Ensino Médio (2007-2035)"
    } else {
      # Para modo percentual, definir limites apropriados para percentagens
      y_min <- 0
      y_max <- max(c(100, max(filtered_data[input$ept_yVariables], na.rm = TRUE) * 1.1))
      y_labels <- function(x) paste0(x, "%")
      y_axis_title <- "Percentagem (%)"
      plot_title <- "Percentagem de Matrículas EPT/Ensino Médio (2007-2035)"
    }
    
    # Criar objeto base ggplot
    p <- ggplot(filtered_data, aes(x = ANO, color = LOCAL)) +
      labs(
        x = "Ano",
        y = y_axis_title,
        title = plot_title,
        color = "Localização"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)
      ) +
      scale_y_continuous(limits = c(y_min, y_max), labels = y_labels) +
      scale_x_continuous(breaks = c(2007, seq(2010, 2035, by = 5))) +
      scale_color_manual(values = ept_local_colors)
    
    # Tipos de linha e anotações Plotly
    line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    annotations <- list()
    
    # Cálculo atualizado de metas usando preferências do usuário
    meta_targets <- tibble()
    
    for(loc in input$ept_localInput) {
      safe_id <- str_replace_all(loc, "[^A-Za-z0-9]", "_")
      meta11_id <- paste0("ept_use_meta11_", safe_id)
      custom_id <- paste0("ept_custom_meta_", safe_id)
      
      # Padrão para Meta 11 se inputs ainda não existem
      if(is.null(input[[meta11_id]]) || is.null(input[[custom_id]])) {
        # Usar Meta 11 por padrão
        meta_absolute <- ept_meta11_df$meta11_absolute[ept_meta11_df$LOCAL == loc]
        if(length(meta_absolute) == 0) meta_absolute <- 0
      } else if(isTruthy(input[[meta11_id]])) {
        # Usar meta Meta 11
        meta_absolute <- ept_meta11_df$meta11_absolute[ept_meta11_df$LOCAL == loc]
        if(length(meta_absolute) == 0) meta_absolute <- 0
      } else if(!is.na(input[[custom_id]]) && input[[custom_id]] > 0) {
        # Usar meta customizada
        meta_absolute <- input[[custom_id]]
      } else {
        meta_absolute <- NA
      }
      
      if(!is.na(meta_absolute)) {
        meta_targets <- bind_rows(meta_targets, 
                                  tibble(LOCAL = loc, meta_target = meta_absolute))
      }
    }
    
    # Adicionar linhas de meta para cada localidade
    for (loc in input$ept_localInput) {
      loc_target <- meta_targets %>% filter(LOCAL == loc)
      
      if (nrow(loc_target) > 0 && !is.na(loc_target$meta_target)) {
        target_absolute <- loc_target$meta_target
        loc_color <- ept_local_colors[loc]
        if(is.na(loc_color)) loc_color <- "darkorange"
        
        if (input$ept_dataType == "numbers") {
          # No modo números, mostrar a meta absoluta
          target_line_value <- target_absolute
          target_label <- paste0("Meta PNE 11 - ", loc, ": ", round(target_line_value))
        } else {
          # No modo percentual, calcular qual % isto representa da população recente para esta localidade
          recent_population <- ept_combined_data %>%
            filter(LOCAL == loc, ANO >= 2020) %>%
            summarise(avg_pop = mean(`15-19_T`, na.rm = TRUE)) %>%
            pull(avg_pop)
          
          if (length(recent_population) > 0 && !is.na(recent_population) && recent_population > 0) {
            target_line_value <- (target_absolute / recent_population) * 100
            target_label <- paste0("Meta PNE 11 - ", loc, ": ", round(target_line_value, 1), "%")
          } else {
            target_line_value <- NULL
            target_label <- NULL
          }
        }
        
        # Adicionar linha horizontal para meta desta localidade
        if (!is.null(target_line_value) && target_line_value <= y_max) {
          p <- p + 
            geom_hline(yintercept = target_line_value, 
                       linetype = "dotdash", 
                       color = loc_color, 
                       linewidth = 1.0, 
                       alpha = 0.7) +
            annotate("text", 
                     x = 2012, 
                     y = target_line_value+(0.1*target_line_value), 
                     label = paste0("Meta 11 vigente triplicar matricula EPT - ", loc),
                     color = loc_color, 
                     fontface = "bold",
                     size = 4.0,
                     alpha = 1)
        }
      }
    }
    
    # Adicionar linhas e anotações para cada variável Y selecionada
    for (loc in unique(filtered_data$LOCAL)) {
      loc_data <- filtered_data %>% filter(LOCAL == loc)
      loc_color <- ept_local_colors[loc]
      if(is.na(loc_color)) loc_color <- "#000000"
      
      for (i in seq_along(input$ept_yVariables)) {
        y_var <- input$ept_yVariables[i]
        y_sym <- sym(y_var)
        line_type <- line_types[(i - 1) %% length(line_types) + 1]
        
        # Adicionar texto de tooltip aos dados
        loc_data <- loc_data %>%
          mutate(
            tooltip_text = paste0(
              names(current_variables)[current_variables == y_var], "<br>",
              "Ano: ", ANO, "<br>",
              "Valor: ", if (input$ept_dataType == "numbers") {
                round(!!y_sym)
              } else {
                paste0(round(!!y_sym, 2), "%")
              },
              "<br>Estado: ", loc
            )
          )
        
        # Adicionar a linha para cada variável Y e LOCAL
        p <- p + geom_line(data = loc_data, 
                           aes(y = !!y_sym), 
                           linetype = line_type, linewidth = 1, color = loc_color)
        
        # Determinar o último ano para esta variável
        is_enrollment <- y_var %in% c("QT_MAT_PROF_TEC_PROPAG", "QT_MAT_MED", "PCT_MAT_EPT", "PCT_MAT_MED")
        last_year <- if (is_enrollment) 2024 else 2035
        
        # Filtrar dados até o ano final apropriado para esta variável
        var_data <- loc_data %>% filter(ANO <= last_year)
        
        # Obter último valor para rotulagem
        if (nrow(var_data) > 0) {
          final_year <- max(var_data$ANO, na.rm = TRUE)
          last_value <- var_data %>%
            filter(ANO == final_year) %>%
            pull(!!y_sym)
          
          if(length(last_value) > 0 && !is.na(last_value)) {
            # Obter nome de exibição para a variável
            var_display_name <- names(current_variables)[current_variables == y_var]
            
            # Construir texto do rótulo
            label_text <- paste(var_display_name, "-", loc)
            
            # Adicionar anotação Plotly para o rótulo
            annotations <- append(annotations, list(
              list(
                x = final_year,
                y = last_value,
                text = label_text,
                showarrow = TRUE,
                arrowhead = 2,
                ax = 0,
                ay = 40,
                font = list(color = "black", size = 12, family = "Arial")
              )
            ))
          }
        }
      }
    }
    
    # Converter para gráfico Plotly interativo e adicionar anotações
    plotly_obj <- ggplotly(p)
    plotly_obj <- plotly_obj %>% layout(annotations = annotations)
    
    return(plotly_obj)
  })
  
  # Exibição de resumo do estado atual de seleção
  output$ept_summary <- renderUI({
    req(input$ept_localInput)
    
    locations_count <- length(input$ept_localInput)
    data_type <- ifelse(input$ept_dataType == "numbers", "Números Absolutos", "Percentagens")
    variables_selected <- length(if(is.null(input$ept_yVariables)) character(0) else input$ept_yVariables)
    
    HTML(paste0(
      "<div style='color: black; font-size: 13px;'>",
      "<strong>Localizações:</strong> ", locations_count, "<br>",
      "<strong>Tipo de dados:</strong> ", data_type, "<br>",
      "<strong>Variáveis:</strong> ", variables_selected, "<br>",
      "<strong>Metas PNE:</strong> ", ifelse(input$ept_showMeta, "Ativas", "Inativas"),
      "</div>"
    ))
  })
  
  
  
  ###############################################################################
  # SERVIDOR — ABA B1: IMPACTO FINANCEIRO PROPAG
  # Lógica de seleção de cenários (dimensões AGIJ), validação e renderização de gráficos de barras
  ###############################################################################
  
  # Opções de dimensões de cenários PROPAG (devem corresponder às definições de checkbox na UI)
  # A = Amortização, G = Contribuição FEF, I = Investimento Direto, J = Taxa de Juros
  all_choices <- list(
    A = c("Sem abatimento"  = "A1",
          "10% abatimento" = "A2",
          "20% abatimento" = "A3"),
    G = c("1%"   = "G1",
          "1.5%" = "G2",
          "2%"   = "G3"),
    I = c("0%"   = "I1",
          "0.5%" = "I2",
          "1%"   = "I3",
          "1.5%" = "I4",
          "2%"   = "I5"),
    J = c("0%" = "J1",
          "1%" = "J2",
          "2%" = "J3",
          "4% (Não Adere)" = "J4")
  )
  
  pretty_opts <- list(
    A = list(fill=TRUE,bigger=TRUE,status="info",    icon=icon("check")),
    G = list(fill=TRUE,bigger=TRUE,status="primary", icon=icon("check")),
    I = list(fill=TRUE,bigger=TRUE,status="success", icon=icon("check")),
    J = list(fill=TRUE,bigger=TRUE,status="danger",  icon=icon("check"))
  )
  
  # Retorna códigos válidos para uma dimensão, filtrados pelas seleções atuais em outras dimensões
  valid_codes <- function(dim, sel){
    df <- dfcen_val
    for (nm in names(sel)) {
      if (length(sel[[nm]]) > 0) {
        df <- df[df[[nm]] %in% sel[[nm]], , drop = FALSE]
      }
    }
    sort(unique(df[[dim]]))
  }
  
  # Atualização dinâmica de grupos de checkbox: filtra opções disponíveis com base em combinações válidas
  observeEvent(
    list(input$choice_A, input$choice_G, input$choice_I, input$choice_J),
    {
      sel <- list(A = input$choice_A,
                  G = input$choice_G,
                  I = input$choice_I,
                  J = input$choice_J)
      
      # ----- 1) J4 escolhido primeiro → travar A/G/I e manter apenas J4 -----
      if (identical(sel$J, "J4")) {
        sel$A <- sel$G <- sel$I <- character(0)
        session$sendCustomMessage("toggleAGI", TRUE)
        
        updatePrettyCheckboxGroup(session, "choice_J",
                                  choices       = all_choices$J["4% (Não Adere)"],
                                  selected      = "J4", inline = TRUE,
                                  prettyOptions = pretty_opts$J
        )
        # limpar os demais
        for(id in c("choice_A","choice_G","choice_I")){
          updatePrettyCheckboxGroup(session, id,
                                    choices       = all_choices[[substr(id,8,8)]],
                                    selected      = character(0), inline = TRUE,
                                    prettyOptions = pretty_opts[[substr(id,8,8)]]
          )
        }
        return(invisible(NULL))
      } else {
        session$sendCustomMessage("toggleAGI", FALSE)
      }
      
      # ----- 2) Se QUALQUER de A/G/I estiver selecionado, REMOVER J4 imediatamente -----
      somePicked <- any(lengths(sel[c("A","G","I")]) > 0)
      if (somePicked) {
        j_keep <- all_choices$J[names(all_choices$J) != "4% (Não Adere)"]
      } else {
        j_keep <- all_choices$J
      }
      updatePrettyCheckboxGroup(session, "choice_J",
                                choices       = j_keep,
                                selected      = intersect(sel$J, j_keep),
                                inline        = TRUE,
                                prettyOptions = pretty_opts$J
      )
      
      # ----- 3) Filtragem normal para A/G/I (J já atualizado) -----
      for (dim in c("A","G","I")) {
        others <- sel[names(sel) != dim]
        ok   <- if (all(lengths(others) > 0)) valid_codes(dim, others) else all_choices[[dim]]
        keep <- all_choices[[dim]][ all_choices[[dim]] %in% ok ]
        
        updatePrettyCheckboxGroup(session,
                                  inputId       = paste0("choice_", dim),
                                  choices       = keep,
                                  selected      = intersect(sel[[dim]], keep),
                                  inline        = TRUE,
                                  prettyOptions = pretty_opts[[dim]]
        )
      }
    },
    ignoreInit = TRUE
  )
  
  # Construir tabela HTML de todas as combinações válidas de opções PROPAG para exibição modal
  labA <- c(A1 = "Sem abatimento",
            A2 = "10% abatimento",
            A3 = "20% abatimento",
            ND1 = "NA")
  
  labG <- c(G1 = "1%", G2 = "1,5%", G3 = "2%", ND1 = "NA")
  labI <- c(I1 = "0%", I2 = "0,5%", I3 = "1%", I4 = "1,5%", I5 = "2%", ND1 = "NA")
  labJ <- c(J1 = "0%", J2 = "1%", J3 = "2%", J4 = "4% (Não Adere)")
  
  # Nomes amigáveis para cada cenário válido (exibido no modal e caixa de resumo)
  op_names <- c("II-A","II-B","II-C","III-A","III-B","III-C","IV-A","IV-B","ND")
  
  ## valid_set DEVE conter colunas A,G,I,J em códigos.
  valid_set <- subset(dfcen_val, valid, select = c(A,G,I,J))
  # Construir tabela legível a partir das combinações válidas
  valid_tbl <- valid_set |>
    dplyr::mutate(
      Opção          = op_names,
      Amortização    = labA[A],
      `Contribuição p/ FEF` = labG[G],
      `Invest. Direto`       = labI[I],
      Juros          = labJ[J]
    ) |>
    dplyr::select(Opção, Amortização, `Contribuição p/ FEF`, `Invest. Direto`, Juros)
  
  # CSS para tabela de combinações válidas (tema painel escuro)
  valid_css <- "
<style>
#tbl-valid thead th{
  background:#1f5673; color:#fff; text-align:center; padding:6px 10px; border:1px solid #4e6f84;
}
#tbl-valid tbody td{
  color:#f5f5f5; padding:6px 10px; border:1px solid #4e6f84;
}
#tbl-valid tbody tr:nth-child(even){ background:rgba(255,255,255,.05); }
#tbl-valid tbody tr:nth-child(odd){  background:rgba(255,255,255,.02); }
</style>
"

## Construir tabela HTML sem dependências externas
make_table_html <- function(df){
  hdr <- paste0("<tr>", paste0(sprintf("<th>%s</th>", names(df)), collapse=""), "</tr>")
  rows <- apply(df, 1, function(r){
    paste0("<tr>", paste0(sprintf("<td>%s</td>", r), collapse=""), "</tr>")
  })
  paste0(
    '<table id="tbl-valid" style="width:100%; border-collapse:collapse;">',
    "<thead>", hdr, "</thead>",
    "<tbody>", paste0(rows, collapse=""), "</tbody></table>"
  )
}

valid_html <- paste0(valid_css, make_table_html(valid_tbl))

## Helper null-coalescing (redefinido para escopo do servidor)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

## Reativo: seleções atuais das dimensões AGIJ
RKT_picked <- reactive({
  list(A = input$choice_A,
       G = input$choice_G,
       I = input$choice_I,
       J = input$choice_J)
})

# Reativo: encontrar linha de cenário válido correspondente em dfcen_val, ou NULL se incompleto
RKT_sel_row <- reactive({
  s <- RKT_picked()
  if (any(lengths(s) == 0)) return(NULL)
  hit <- dfcen_val[dfcen_val$valid &
                     dfcen_val$A == s$A &
                     dfcen_val$G == s$G &
                     dfcen_val$I == s$I &
                     dfcen_val$J == s$J, ]
  if (nrow(hit)) hit[1, ] else NULL
})

## Valores reativos para rastrear última seleção válida e estado de rollback
rv <- reactiveValues(
  last_ok    = list(A=character(0), G=character(0), I=character(0), J=character(0)),
  last_dim   = NULL,
  lock_dim   = NULL        # qual dimensão precisamos reverter
)
modal_shown <- reactiveVal(FALSE)

observeEvent(input$choice_A, { rv$last_dim <- "A" }, ignoreInit = TRUE)
observeEvent(input$choice_G, { rv$last_dim <- "G" }, ignoreInit = TRUE)
observeEvent(input$choice_I, { rv$last_dim <- "I" }, ignoreInit = TRUE)
observeEvent(input$choice_J, { rv$last_dim <- "J" }, ignoreInit = TRUE)

## Lógica principal de validação: verifica validade parcial contra todas as combinações PROPAG válidas
observeEvent(RKT_picked(), {
  
  sel <- RKT_picked()
  
  # 1) “Não Adere” rule: picking J4 clears A/G/I immediately
  if (identical(sel$J, "J4") &&
      any(lengths(sel[c("A","G","I")]) > 0)) {
    sel$A <- sel$G <- sel$I <- character(0)
    updatePrettyCheckboxGroup(session, "choice_A", selected = character(0))
    updatePrettyCheckboxGroup(session, "choice_G", selected = character(0))
    updatePrettyCheckboxGroup(session, "choice_I", selected = character(0))
  }
  
  # 2) Se ainda incompleto **mas não inválido**, apenas fechar modal aberto
  if (any(lengths(sel) == 0) && modal_shown()) {
    removeModal(); modal_shown(FALSE)
    shinyjs::enable(selector = ".pcg")
  }
  
  # 3) Verificar “validade parcial” filtrando contra TODAS as linhas válidas
  vp <- dfcen_val[dfcen_val$valid, ]        # começar com as 9 linhas válidas
  for (dim in names(sel)) {
    if (length(sel[[dim]]) > 0) {
      vp <- vp[ vp[[dim]] %in% sel[[dim]] , , drop = FALSE ]
    }
  }
  if (nrow(vp) == 0) {
    # não há linha válida que corresponda à seleção parcial atual → inválido
    rv$lock_dim <- rv$last_dim %||% names(sel)[which.max(vapply(sel, length, 0))]
    shinyjs::disable(selector = ".pcg")
    
        # Combinação inválida detectada — construir modal de feedback ao usuário
    
    # 1) detect the “culprit” dimension (where no valid codes remain)
    bad_dims <- names(sel)[
      vapply(names(sel), function(dim) {
        # Excluir dimensão atual do filtro
        others <- sel[names(sel) != dim]
        # Não restam códigos válidos para esta dimensão
        length(valid_codes(dim, others)) == 0L
      }, logical(1))
    ]
    bad_dim <- bad_dims[1]  # Primeira dimensão conflitante
    
        # Recuperar mapas de rótulos legíveis para a dimensão conflitante
    maps     <- list(A=labA, G=labG, I=labI, J=labJ)
    dim_names<- c(A="Amortização", G="Contribuição p/ FEF",
                  I="Invest. Direto", J="Juros")
    bad_map   <- maps[[bad_dim]]
    bad_label <- dim_names[bad_dim]
    
        # Identificar a seleção inválida (exibida em vermelho)
    bad_code  <- sel[[bad_dim]]
    bad_text  <- bad_map[bad_code]
    
        # Identificar alternativas válidas (exibidas em verde)
    ok_codes  <- valid_codes(bad_dim, sel[names(sel)!=bad_dim])
    ok_text   <- bad_map[ok_codes]
    
        # Construir HTML de dica modal com opções inválidas vs válidas
    hint_html <- sprintf(
      "<p>Você escolheu <strong style='color:#e74c3c'>%s</strong> para <em>%s</em>,<br/>
           mas apenas <strong style='color:#27ae60'>%s</strong> %s válidas.</p>",
      bad_text,
      bad_label,
      paste(ok_text, collapse = ", "),
      if (length(ok_text)>1) "são" else "é"
    )
    
        # Exibir diálogo modal com dica e tabela completa de combinações válidas
    showModal(modalDialog(
      title = HTML("💥 Combinação inválida"),
      tagList(
        tags$style(HTML("
      .modal-body table th {
        background-color: #2c3e50 !important;
        color: #fff !important;
      }
    ")),
        HTML("Essa escolha não é permitida pelo <b>PROPAG</b>.<br>",
             "Ajuste os percentuais para atender uma combinação válida."),
        HTML(hint_html),
        HTML(valid_html)
      ),
      easyClose = FALSE,
      footer = actionButton("invalid_ok", "OK", class="btn-primary"),
      size = "l"
    ))
    
    
    session$onFlushed(function(){
      runjs("
        var dlg = $('#shiny-modal .modal-dialog');
        if(!dlg.hasClass('ui-draggable')){
          dlg.draggable({ handle: '.modal-header' });
          dlg.css('cursor','move');
        }
      ")
    }, once = TRUE)
    modal_shown(TRUE)
    return()
  }
  
  # 4) Se combinação completa escolhida e corresponde a linha válida, salvar e fechar modal
  if (all(lengths(sel) == 1)) {
    hit <- vp[ vp$A == sel$A & vp$G == sel$G &
                 vp$I == sel$I & vp$J == sel$J , , drop = FALSE]
    if (nrow(hit) == 1) {
      rv$last_ok <- sel
      if (modal_shown()) {
        removeModal(); modal_shown(FALSE)
        shinyjs::enable(selector = ".pcg")
      }
      return()
    }
  }
  
  # caso contrário: ainda incompleto mas não inválido → não fazer nada (aguardar próxima seleção)
}, ignoreInit = TRUE)

## -------- usuário clica OK no modal ----------------------------------------
observeEvent(input$invalid_ok, {
  removeModal()
  modal_shown(FALSE)
  
  # reverter grupo culpado para último válido
  dim <- rv$lock_dim %||% "A"
  id  <- paste0("choice_", dim)
  updatePrettyCheckboxGroup(
    session, inputId = id,
    selected = rv$last_ok[[dim]] %||% character(0)
  )
  
  # re-enable UI
  shinyjs::enable(selector = ".pcg")
})



tags$head(tags$style(HTML("
.scenario-box{
  border:1px solid #cc0000;          /* red frame (change if you want)       */
  padding:14px 18px;
  width:260px;
  margin-left:auto;                  /* keep it at the right column          */
  color:#fff;                        /* works on your dark panel             */
  background:rgba(255,255,255,.05);
}
.scenario-box h4{
  margin:0 0 8px 0;
  font-weight:700;
  text-align:center;
}
.scenario-box .lbl {font-weight:600;}

/* Fix modal table header for dark theme */
#shiny-modal .modal-body table th {
  background-color: #2c3e50 !important;
  color: #fff !important;
}
")))

## Função auxiliar para obter nome legível de uma linha em dfcen_val (assume coluna 'valid')
row_to_name <- function(r){
  # r is a 1-row data.frame
  key <- paste(r$A,r$G,r$I,r$J, sep = "_")
  key_vec <- paste(valid_set$A,valid_set$G,valid_set$I,valid_set$J, sep="_")
  op_names[ match(key, key_vec) ]
}

## ---- choice summary card --------------------------------------------------
output$choice_summary <- renderUI({
  r <- RKT_sel_row()          # NULL se seleção estiver incompleta ou inválida
  if (is.null(r)) return(NULL)
  
  opcao <- row_to_name(r)
  htmltools::div(class="scenario-box",
                 htmltools::h4(sprintf("Opção : %s", opcao)),
                 htmltools::div(span(class="lbl","Amortização: "), labA[r$A]),
                 htmltools::div(span(class="lbl","Contribuição ao FEF: "), labG[r$G]),
                 htmltools::div(span(class="lbl","Investimento Direto: "), labI[r$I]),
                 htmltools::div(span(class="lbl","Juros: "), labJ[r$J])
  )
})


observe({
  session$sendCustomMessage("toggleAGI", "J4" %in% input$choice_J)
})

# Reativo: nome do cenário resolvido com base nas seleções atuais
RKT_scenario_name <- reactive({
  # Caso: J4 (Não Adere) sobrepõe todas as outras escolhas
  if ("J4" %in% input$choice_J) {
    return("ND")
  }
  
  # Retorno à lógica existente
  r <- RKT_sel_row()
  req(r)
  row_to_name(r)  # e.g. "II-A", "III-B", … or "ND"
})


# Reativo: recuperar o data frame pré-calculado para o cenário selecionado
RKT_scenario_data <- reactive({
  nm <- RKT_scenario_name()
  df_list[[nm]]
})

# Reativo: filtrar dados do cenário para a UF selecionada
RKT_uf_data <- reactive({
  df <- RKT_scenario_data()
  req(input$uf_select)
  df[df$NM_UF == input$uf_select, ]
})


# Reativo: pivotar colunas de variáveis financeiras selecionadas para formato longo
RKT_plot_data <- reactive({
  df <- RKT_uf_data()
  req(input$var_select)
  years <- 2025:2054
  sel_cols <- paste0(input$var_select, years)
  df %>%
    select(all_of(sel_cols)) %>%
    setNames(years) %>%
    pivot_longer(
      cols      = everything(),
      names_to  = "Ano",
      values_to = "Valor"
    ) %>%
    mutate(Ano = as.integer(Ano))
})


RKT_plot_data_compare <- reactive({
  req(input$var_select, input$uf_select)
  
  var     <- input$var_select
  year_bounds <- input$year_range
  years       <- seq(year_bounds[1], year_bounds[2])
  
  selcols <- paste0(var, years)
  
  main_op <- RKT_scenario_name()
  cmp_op  <- input$compare_with %||% ""
  
  safe_named_scenario_label <- function(key) {
    if (!is.null(key) && nzchar(key) && key %in% names(named_scenarios)) {
      named_scenarios[[key]]
    } else {
      paste("Cenário", key %||% "ND")
    }
  }
  label_main <- safe_named_scenario_label(main_op)
  label_cmp  <- safe_named_scenario_label(cmp_op)
  
  df_sel <- RKT_scenario_data() %>%
    filter(NM_UF == input$uf_select) %>%
    select(all_of(selcols)) %>%
    setNames(years) %>%
    pivot_longer(cols = everything(), names_to = "Ano", values_to = "Valor") %>%
    mutate(
      Ano        = as.integer(Ano),
      UF         = input$uf_select,
      fill_key   = UF,
      fill_label = label_main    # Rótulo do cenário para legenda
    )
  
  if (input$compare_with == "") return(df_sel)
  
  df_cmp <- df_list[[cmp_op]] %>%
    filter(NM_UF == input$uf_select) %>%
    select(all_of(selcols)) %>%
    setNames(years) %>%
    pivot_longer(cols = everything(), names_to = "Ano", values_to = "Valor") %>%
    mutate(
      Ano        = as.integer(Ano),
      UF         = input$uf_select,
      fill_key   = paste0(UF, "_compare"),
      fill_label = label_cmp     # Rótulo do cenário de comparação para legenda
    )
  
  bind_rows(df_sel, df_cmp)
})

RKT_plot_title <- reactive({
  req(input$uf_select, RKT_scenario_name())
  
  uf      <- input$uf_select
  main_op <- RKT_scenario_name()
  cmp_op  <- input$compare_with
  
  main_row <- valid_tbl[valid_tbl$Opção == main_op, ]
  main_desc <- if (nrow(main_row) == 1) {
    paste0("Cenário ", main_op, " [", 
           main_row$Amortização, ", ",
           main_row$`Contribuição p/ FEF`, ", ",
           main_row$`Invest. Direto`, ", ",
           main_row$Juros, "]")
  } else {
    paste0("Cenário ", main_op)
  }
  
  if (!nzchar(cmp_op)) {
    paste0("Gráfico: UF ", uf, " — ", main_desc)
  } else {
    cmp_row <- valid_tbl[valid_tbl$Opção == cmp_op, ]
    cmp_desc <- if (nrow(cmp_row) == 1) {
      paste0("Cenário ", cmp_op, " [", 
             cmp_row$Amortização, ", ",
             cmp_row$`Contribuição p/ FEF`, ", ",
             cmp_row$`Invest. Direto`, ", ",
             cmp_row$Juros, "]")
    } else {
      paste0("Cenário ", cmp_op)
    }
    
    paste0("Gráfico: UF ", uf, " — ", main_desc, " comparado com ", cmp_desc)
  }
})

# Renderizar gráfico de barras do cenário financeiro

output$PloTab1b <- renderPlot({
  
  pd <- RKT_plot_data_compare()
  req(nrow(pd) > 0)
  
  # Color palette: full (original + comparison-shaded)
  all_colors <- c(
    uf_colors,
    setNames(uf_colors_compare, paste0(names(uf_colors), "_compare"))
  )
  
  # Verificar se comparação está ativa
  is_comparing <- any(grepl("_compare$", pd$fill_key))
  
  # Base ggplot
  gp2b <- ggplot(pd, aes(
    x    = factor(Ano),
    y    = Valor,
    fill = fill_key
  )) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x     = "Ano",
      y     = switch(input$var_select,
                     Saldo   = "Saldo da Dívida",
                     ApoFEF  = "Aporte ao FEF",
                     InvDir  = "Investimento Direto",
                     JurPag  = "Juros Pagos"),
      title = RKT_plot_title()
    ) +
    theme_minimal() +
    theme(
      legend.position = if (is_comparing) c(0.95, 0.95) else "none",
      axis.text.x     = element_text(size = 16, angle = 90, vjust = 0.5,color="blue"),
      axis.text.y     = element_text(size = 16,color="blue"),
      axis.title.x    = element_text(size = 16, color="blue", face = "bold"),
      axis.title.y    = element_text(size = 16,color="blue", face = "bold"),
      plot.title      = element_text(size = 22, face = "bold", hjust = 0.5, color = "#1f5673")
    )
  
  # Conditional labels:
  
  # Calcular cores de texto do rótulo com base no brilho da cor de preenchimento
  text_colors <- sapply(pd$fill_key, function(key) {
    col_hex <- all_colors[[key]]
    rgb_vals <- hex2RGB(col_hex)@coords
    luminance <- sum(rgb_vals * c(0.299, 0.587, 0.114))  # perceptual brightness
    if (luminance > 0.6) "black" else "white"
  })
  
  # Calcular número de anos selecionados
  n_years <- length(unique(pd$Ano))
  
  # Adjust text size based on range
  label_size <- case_when(
    n_years <= 10 ~ 8,       # Very large if only a few years
    n_years <= 20 ~ 6,     # Medium-large for mid-range
    TRUE          ~ 4.5      # Default for full range
  )
  
  gp2b <- gp2b +
    geom_text(
      aes(label = paste0(scales::comma(Valor / 1e6), " mi")),
      position = position_dodge(width = 0.9),
      color    = text_colors,
      size     = label_size,
      fontface = "bold",
      angle    = if (is_comparing) 90 else 0,
      vjust    = if (is_comparing) 1.2 else 1.2,  # same vertical reference point (but for rotated text, this pulls it downward)
      hjust    = if (is_comparing) 1.1 else 0.5   # shift right slightly so it doesn't clip left edge
    )
  
  legend_labels <- setNames(pd$fill_label, pd$fill_key)
  
  gp2b <- gp2b +
    scale_fill_manual(
      values = all_colors,
      labels = legend_labels,
      name = "Leyenda"
    )+
    theme(
      legend.title = element_text(size = 16, face = "bold"),
      legend.text  = element_text(size = 14))   
  
  
  gp2b
})

###############################################################################
# SERVIDOR — ABA B2: SIMULAÇÃO FEF
# Matriz de cenários por UF, cálculos de contribuição/retorno FEF, gráfico de barras
###############################################################################

# Rastrear modo atual
current_mode <- reactiveVal("uniform")

# Aplicar estados iniciais de toggle na inicialização
observe({
  mode <- current_mode()
  
  for (op in opcoes) {
    toggleState(id = paste0("chk_all_", op), condition = (mode == "uniform"))
    for (uf in sg_ufs) {
      toggleState(id = paste0("chk_", op, "_", uf), condition = (mode == "per_uf"))
    }
  }
})


# Observar mudança de modo
observeEvent(input$selection_mode, {
  isolate({
    # Mostrar modal apenas se current_mode já foi inicializado
    if (!is.null(current_mode()) && input$selection_mode != current_mode()) {
      showModal(modalDialog(
        title = "Mudar Modo de Seleção?",
        "Essa ação limpará as seleções atuais. Deseja continuar?",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_mode_change", "Sim, mudar", class = "btn-danger")
        )
      ))
    } else {
      # Se primeiro carregamento ou mesma seleção — definir modo silenciosamente
      current_mode(input$selection_mode)
    }
  })
})


# Confirmed switch
observeEvent(input$confirm_mode_change, {
  removeModal()
  new_mode <- isolate(input$selection_mode)
  current_mode(new_mode)
  
  # 1. Limpar todos os checkboxes
  for (op in opcoes) {
    updateCheckboxInput(session, paste0("chk_all_", op), value = FALSE)
    for (uf in sg_ufs) {
      updateCheckboxInput(session, paste0("chk_", op, "_", uf), value = FALSE)
    }
  }
  
  # 2. Desabilitar/habilitar controles após atualização do DOM
  session$onFlushed(function() {
    if (new_mode == "uniform") {
      # Enable 'Todos' checkboxes, disable individual ones
      for (op in opcoes) {
        shinyjs::enable(paste0("chk_all_", op))
        for (uf in sg_ufs) {
          shinyjs::disable(paste0("chk_", op, "_", uf))
        }
      }
    } else {
      # Disable 'Todos' checkboxes, enable individual ones
      for (op in opcoes) {
        shinyjs::disable(paste0("chk_all_", op))
        for (uf in sg_ufs) {
          shinyjs::enable(paste0("chk_", op, "_", uf))
        }
      }
    }
  }, once = TRUE)
})


# Uniform: handle “Todos” row selection
observe({
  for (op in opcoes) {
    local({
      op_local <- op
      observeEvent(input[[paste0("chk_all_", op_local)]], {
        req(current_mode() == "uniform")
        selected <- isTRUE(input[[paste0("chk_all_", op_local)]])
        for (other_op in setdiff(opcoes, op_local)) {
          updateCheckboxInput(session, paste0("chk_all_", other_op), value = FALSE)
          for (uf in sg_ufs) {
            updateCheckboxInput(session, paste0("chk_", other_op, "_", uf), value = FALSE)
          }
        }
        for (uf in sg_ufs) {
          updateCheckboxInput(session, paste0("chk_", op_local, "_", uf), value = selected)
        }
      }, ignoreInit = TRUE)
    })
  }
})

# Per-UF: column logic
observe({
  for (uf in sg_ufs) {
    for (op in opcoes) {
      local({
        uf_local <- uf
        op_local <- op
        id <- paste0("chk_", op_local, "_", uf_local)
        observeEvent(input[[id]], {
          if (current_mode() == "per_uf" && isTRUE(input[[id]])) {
            for (other_op in setdiff(opcoes, op_local)) {
              updateCheckboxInput(session, paste0("chk_", other_op, "_", uf_local), value = FALSE)
            }
          }
        }, ignoreInit = TRUE)
      })
    }
  }
})


# NOTA: BLOCO DUPLICADO — As ~115 linhas seguintes (alternância de modo, uniforme/por-UF
# handlers) duplicam o bloco iniciado na ~linha 4122. Ambos são mantidos pois
# remover qualquer um pode afetar o comportamento em tempo de execução devido à ordem de avaliação reativa do R.
# Mantenedores futuros devem verificar e consolidar.

# Rastrear modo atual
current_mode <- reactiveVal("uniform")

# Aplicar estados iniciais de toggle na inicialização
observe({
  mode <- current_mode()
  
  for (op in opcoes) {
    toggleState(id = paste0("chk_all_", op), condition = (mode == "uniform"))
    for (uf in sg_ufs) {
      toggleState(id = paste0("chk_", op, "_", uf), condition = (mode == "per_uf"))
    }
  }
})


# Observar mudança de modo
observeEvent(input$selection_mode, {
  isolate({
    # Mostrar modal apenas se current_mode já foi inicializado
    if (!is.null(current_mode()) && input$selection_mode != current_mode()) {
      showModal(modalDialog(
        title = "Mudar Modo de Seleção?",
        "Essa ação limpará as seleções atuais. Deseja continuar?",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_mode_change", "Sim, mudar", class = "btn-danger")
        )
      ))
    } else {
      # Se primeiro carregamento ou mesma seleção — definir modo silenciosamente
      current_mode(input$selection_mode)
    }
  })
})


# Confirmed switch
observeEvent(input$confirm_mode_change, {
  removeModal()
  new_mode <- isolate(input$selection_mode)
  current_mode(new_mode)
  
  # 1. Limpar todos os checkboxes
  for (op in opcoes) {
    updateCheckboxInput(session, paste0("chk_all_", op), value = FALSE)
    for (uf in sg_ufs) {
      updateCheckboxInput(session, paste0("chk_", op, "_", uf), value = FALSE)
    }
  }
  
  # 2. Desabilitar/habilitar controles após atualização do DOM
  session$onFlushed(function() {
    if (new_mode == "uniform") {
      # Enable 'Todos' checkboxes, disable individual ones
      for (op in opcoes) {
        shinyjs::enable(paste0("chk_all_", op))
        for (uf in sg_ufs) {
          shinyjs::disable(paste0("chk_", op, "_", uf))
        }
      }
    } else {
      # Disable 'Todos' checkboxes, enable individual ones
      for (op in opcoes) {
        shinyjs::disable(paste0("chk_all_", op))
        for (uf in sg_ufs) {
          shinyjs::enable(paste0("chk_", op, "_", uf))
        }
      }
    }
  }, once = TRUE)
})


# Uniform: handle “Todos” row selection
observe({
  for (op in opcoes) {
    local({
      op_local <- op
      observeEvent(input[[paste0("chk_all_", op_local)]], {
        req(current_mode() == "uniform")
        selected <- isTRUE(input[[paste0("chk_all_", op_local)]])
        for (other_op in setdiff(opcoes, op_local)) {
          updateCheckboxInput(session, paste0("chk_all_", other_op), value = FALSE)
          for (uf in sg_ufs) {
            updateCheckboxInput(session, paste0("chk_", other_op, "_", uf), value = FALSE)
          }
        }
        for (uf in sg_ufs) {
          updateCheckboxInput(session, paste0("chk_", op_local, "_", uf), value = selected)
        }
      }, ignoreInit = TRUE)
    })
  }
})

# Per-UF: column logic
observe({
  for (uf in sg_ufs) {
    for (op in opcoes) {
      local({
        uf_local <- uf
        op_local <- op
        id <- paste0("chk_", op_local, "_", uf_local)
        observeEvent(input[[id]], {
          if (current_mode() == "per_uf" && isTRUE(input[[id]])) {
            for (other_op in setdiff(opcoes, op_local)) {
              updateCheckboxInput(session, paste0("chk_", other_op, "_", uf_local), value = FALSE)
            }
          }
        }, ignoreInit = TRUE)
      })
    }
  }
})


# Criar reativo que retorna vetor nomeado: nomes = sg_uf, valores = opção escolhida
RKT_fef_choices <- reactive({ 
  mode <- current_mode()
  selected <- character(length(sg_ufs))
  names(selected) <- sg_ufs
  
  if (mode == "uniform") {
    # Verificar qual chk_all_* está selecionado
    for (op in opcoes) {
      if (isTRUE(input[[paste0("chk_all_", op)]])) {
        selected[] <- op
        break
      }
    }
  } else {
    for (uf in sg_ufs) {
      for (op in opcoes) {
        if (isTRUE(input[[paste0("chk_", op, "_", uf)]])) {
          selected[uf] <- op
          break
        }
      }
    }
  }
  selected
})


uf_map <- df_censo_UF |>
  dplyr::select(sg_uf = SG_UF, NM_UF) |>
  dplyr::distinct()



RKT_fef_all_options <- reactive({
  purrr::map_dfr(df_choices$opcao, function(op_label) {
    df_base <- df_list[[op_label]]
    if (is.null(df_base)) return(NULL)
    
    # Selecionar colunas necessárias — NM_UF deve estar presente e consistente com uf_map
    df_base_min <- df_base |>
      dplyr::select(NM_UF, Distr_FEF, dplyr::matches("^ApoFEF"))
    
    # Junção por NM_UF com uf_map conhecido e limpo
    df_joined <- dplyr::left_join(df_base_min, uf_map, by = "NM_UF")
    
    if (any(is.na(df_joined$sg_uf))) {
      unmatched <- df_joined$NM_UF[is.na(df_joined$sg_uf)]
      warning("Could not match all NM_UF values to sg_uf for option ", op_label,
              ". Unmatched: ", paste(unique(unmatched), collapse = ", "))
    }
    
    df_joined$opcao <- op_label
    df_joined
  })
})


RKT_fef_table <- reactive({
  choices <- RKT_fef_choices()
  all_df <- RKT_fef_all_options()
  
  if (is.null(all_df)) return(NULL)
  
  mode <- current_mode()
  
  if (mode == "uniform") {
    op_code <- unique(choices)
    if (length(op_code) != 1 || !nzchar(op_code)) return(NULL)
    
    op_label <- df_choices$opcao[match(toupper(op_code), toupper(opcoes))]
    return(dplyr::filter(all_df, opcao == op_label))
  } else {
    choice_df <- tibble::tibble(
      sg_uf = names(choices),
      op_code = choices
    ) |>
      dplyr::filter(nzchar(op_code)) |>
      dplyr::mutate(opcao = df_choices$opcao[match(toupper(op_code), toupper(opcoes))]) |>
      dplyr::select(sg_uf, opcao, op_code)      |>
      dplyr::filter(nzchar(op_code)) |>
      dplyr::mutate(opcao = df_choices$opcao[match(toupper(op_code), toupper(opcoes))])
    
    # Junção apenas por sg_uf e opcao — NM_UF virá limpo de all_df
    dplyr::inner_join(all_df, choice_df, by = c("sg_uf","opcao"))
  }
})


RKT_fef_total_by_year <- reactive({
  df <- RKT_fef_table()
  req(df)
  
  # Selecionar colunas ApoFEF
  cols <- grep("^ApoFEF", names(df), value = TRUE)
  
  total <- colSums(df[, cols, drop = FALSE], na.rm = TRUE)
  
  # Converter para tibble com ano extraído dos nomes de colunas
  tibble::tibble(
    year = as.integer(sub("ApoFEF", "", names(total))),
    total_fef = as.numeric(total)
  )
})

RKT_fef_liq_flow_by_uf <- reactive({
  df <- RKT_fef_table()
  total <- RKT_fef_total_by_year()
  
  req(df, total)
  
  # Passo 1: Identificar todas as colunas ApoFEF
  fef_cols <- grep("^ApoFEF", names(df), value = TRUE)
  years <- as.integer(sub("ApoFEF", "", fef_cols))
  
  # Passo 2: Obter FEF total por ano como vetor nomeado
  total_vec <- setNames(total$total_fef, paste0("ApoFEF", total$year))
  
  # Passo 3: Preparar df inicial com identidade e colunas ApoFEF
  liq_df <- df[, c("sg_uf", "NM_UF", "opcao", "Distr_FEF", fef_cols)]
  
  # Passo 4: Adicionar colunas LiqFEF
  for (col in fef_cols) {
    year <- sub("ApoFEF", "", col)
    total_amt <- total_vec[[col]]
    liq_df[[paste0("LiqFEF", year)]] <- (liq_df$Distr_FEF * total_amt) - df[[col]]
  }
  
  liq_df
})


# Tab2: Preparar dados para plotagem
RKT_fef_plot_data <- reactive({
  df <- RKT_fef_liq_flow_by_uf()
  req(df)
  
  uf_input <- input$uf_select
  year_bounds <- input$year_range
  df_uf <- dplyr::filter(df, NM_UF == uf_input)
  
  # Reunir e filtrar por intervalo de anos
  df_long <- df_uf |>
    tidyr::pivot_longer(
      cols = matches("^(ApoFEF|LiqFEF)\\d+"),
      names_to = "var",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::if_else(stringr::str_starts(var, "ApoFEF"), "ApoFEF", "LiqFEF"),
      year = as.integer(stringr::str_remove(var, "^(ApoFEF|LiqFEF)"))
    ) |>
    dplyr::filter(year >= year_bounds[1], year <= year_bounds[2])  # Aplicar filtro de intervalo de anos
  
  df_long
})

RKT_plot_title_tab2 <- reactive({
  req(input$uf_select, input$year_range, RKT_fef_table())
  
  uf_code <- input$uf_select
  df <- RKT_fef_table()
  
  # Recuperar nome da UF e opção selecionada para título do gráfico
  row <- df |>
    dplyr::filter(NM_UF == uf_code) |>
    dplyr::select(NM_UF, opcao) |>
    dplyr::distinct()
  
  uf_name <- unique(row$NM_UF)
  uf_op   <- unique(row$opcao)
  
  uf_name <- uf_name %||% uf_code
  uf_op   <- uf_op   %||% "ND"
  
  paste0("UF: ", uf_name,
         " — Opção: ", uf_op,
         " — Anos: ", input$year_range[1], "–", input$year_range[2],
         " — Aporte e Fluxo Líquido do FEF")
})


output$plotab2 <- renderPlot({
  df <- RKT_fef_plot_data()
  req(nrow(df) > 0)
  
  # Definir cores manualmente
  fill_colors <- c("ApoFEF" = "#fbb4ae", "LiqFEF" = "#FF4D4D")
  
  # Criar coluna de rótulo
  df$label <- paste0(scales::comma(df$value / 1e6), " mi")
  
  df$hjust <- dplyr::case_when(
    df$value >= 0 ~ 1.2,  # slightly above the bar (negative = upward in y)
    df$value <  0 ~  -0.2   # slightly below the bar
  )
  
  # Centralização horizontal consistente do texto rotacionado
  df$vjust <- 0.5 # center
  
  dodge_width <- 0.75
  pos_dodge <- position_dodge(width = dodge_width)
  
  # Tamanho da fonte pode ser ajustado para intervalo de anos
  n_years <- length(unique(df$year))
  text_size <- dplyr::case_when(
    n_years <= 10 ~ 10,
    n_years <= 20 ~ 8,
    TRUE          ~ 6
  )
  
  # Main ggplot
  ggplot(df, aes(x = factor(year), y = value, fill = type)) +
    geom_col(position = pos_dodge, width = dodge_width) +
    geom_text(
      aes(label = label, hjust = hjust),
      position = pos_dodge,
      color= "black",
      size = text_size,
      angle = 90,
      vjust    = 0.5,
      fontface = "bold"
    ) +
    scale_color_identity()+
    scale_fill_manual(values = fill_colors,
                      labels = c("ApoFEF" = "Aporte FEF", "LiqFEF" = "Fluxo Líquido")) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
    labs(
      x = "Ano", y = "Valor (R$)", fill = NULL,
      title = RKT_plot_title_tab2()
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.text.x     = element_text(size = 16, angle = 90, vjust = 0.5, color = "blue"),
      axis.text.y     = element_text(size = 16, color = "blue"),
      axis.title.x    = element_text(size = 16, color = "blue", face = "bold"),
      axis.title.y    = element_text(size = 16, color = "blue", face = "bold"),
      plot.title      = element_text(size = 22, face = "bold", hjust = 0.5, color = "#1f5673")
    )
})

###############################################################################
# SERVIDOR — ABA B3: FINANÇAS COMPARATIVAS
# Gráficos de barras lado a lado: estados gerais vs altamente endividados (MG,SP,RJ,RS)
###############################################################################

output$tab1_fin_plot <- renderPlot({
  library(patchwork)
  
  req(input$fin_variable)
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  df <- propag_ept_financeiro
  df$valor <- suppressWarnings(as.numeric(gsub(",", "", df[[input$fin_variable]])))
  
  uf_endividado <- c("MG", "SP", "RJ", "RS")
  
  df_endividado <- df[df$UF %in% uf_endividado, ]
  df_geral <- df[!df$UF %in% uf_endividado, ]
  
  df_geral <- df_geral[order(df_geral$Estado), ]
  df_endividado <- df_endividado[order(df_endividado$Estado), ]
  
  plot_label <- var_labels[[input$fin_variable]] %||% input$fin_variable
  
  # ----- CONDITIONAL Y-AXIS SETTINGS -----
  if (input$fin_variable == "saldo_mar25") {
    y_limits_geral <- c(0, 22e9)
    y_breaks_geral <- seq(0, 22e9, by = 1e9)
    
    y_limits_divida <- c(0, 375e9)
    y_breaks_divida <- seq(0, 375e9, by = 100e9)
    
  } else if (input$fin_variable == "amort_extr") {
    y_limits_geral <- c(0, 5e9)
    y_breaks_geral <- seq(0, 5e9, by = 0.5e9)
    
    y_limits_divida <- c(0, 75e9)
    y_breaks_divida <- seq(0, 75e9, by = 5e9)
    
  }  else if (input$fin_variable == "EPT_1ano_cen01") {
    y_limits_geral <- c(0, 120e6)
    y_breaks_geral <- seq(0, 120e6, by = 20e6)
    
    y_limits_divida <- c(0, 1.75e9)
    y_breaks_divida <- seq(0, 1.75e9, by = 250e6)
  }  
  
  else if (input$fin_variable == "EPT_1ano_cen02") {
    y_limits_geral <- c(0, 200e6)
    y_breaks_geral <- seq(0, 200e6, by = 25e6)
    
    y_limits_divida <- c(0, 3.5e9)
    y_breaks_divida <- seq(0, 3.5e9, by = 500e6)
  }
  
  
  else if (input$fin_variable == "EPT_5ano_cen01") {
    y_limits_geral <- c(0, 600e6)
    y_breaks_geral <- seq(0, 600e6, by = 100e6)
    
    y_limits_divida <- c(0, 85e8)
    y_breaks_divida <- seq(0, 85e8, by = 1e9)
  }
  
  else if (input$fin_variable == "EPT_5ano_cen02") {
    y_limits_geral <- c(0, 1000e6)
    y_breaks_geral <- seq(0, 1000e6, by = 100e6)
    
    y_limits_divida <- c(0, 175e8)
    y_breaks_divida <- seq(0, 175e8, by = 1e9)
  }
  
  else if (input$fin_variable == "FEF_1ano_liq_cen01") {
    y_limits_geral <- c(-2.5e9, 800e6)
    y_breaks_geral <- seq(-2.5e9, 800e6, by = 500e6)
    
    y_limits_divida <- y_limits_geral
    y_breaks_divida <- y_breaks_geral
    
  } 
  
  else if (input$fin_variable == "FEF_1ano_liq_cen02") {
    y_limits_geral <- c(-5e9, 1200e6)
    y_breaks_geral <- seq(-5e9, 1200e6, by = 500e6)
    
    y_limits_divida <- y_limits_geral
    y_breaks_divida <- y_breaks_geral
    
  } 
  
  else if (input$fin_variable == "FEF_5ano_liq_cen01") {
    y_limits_geral <- c(-11.37e9, 3800e6)
    y_breaks_geral <- seq(-11.37e9, 3800e6, by = 1000e6)
    
    y_limits_divida <- y_limits_geral
    y_breaks_divida <- y_breaks_geral
    
  } 
  
  else if (input$fin_variable == "FEF_5ano_liq_cen02") {
    y_limits_geral <- c(-22.8e9, 7500e6)
    y_breaks_geral <- seq(-22.8e9, 7500e6, by = 1000e6)
    
    y_limits_divida <- y_limits_geral
    y_breaks_divida <- y_breaks_geral
    
  } 
  
  
  else {
    y_min <- min(df$valor, na.rm = TRUE)
    y_max <- max(df$valor, na.rm = TRUE)
    y_limits_geral <- c(y_min, y_max)
    y_breaks_geral <- waiver()
    y_limits_divida <- c(y_min, y_max)
    y_breaks_divida <- waiver()
  }
  
  # ----- PLOTS -----
  # Gráfico para estados gerais (em milhões)
  p_geral <- ggplot(df_geral, aes(x = factor(Estado, levels = df_geral$Estado), y = valor, fill = UF)) +
    geom_col() +
    geom_text(
      aes(label = paste0(format(round(valor / 1e6), big.mark = ".", decimal.mark = ",", scientific = FALSE), " M")),
      angle = 90, vjust = 0.2, hjust=-0.1, size = 5, color = "blue",fontface = "bold"
    ) +
    scale_y_continuous(
      limits = y_limits_geral,
      breaks = y_breaks_geral,
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    scale_fill_manual(values = uf_colors_bySG)+
    labs(title = paste("Demais Estados –", plot_label), x = "Estado", y = "Valor (R$)") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", color = "#1f5673"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  # Gráfico para estados endividados (em bilhões)
  p_divida <- ggplot(df_endividado, aes(x = factor(Estado, levels = df_endividado$Estado), y = valor, fill = UF)) +
    geom_col() +
    geom_text(
      aes(label = paste0(format(round(valor / 1e6), big.mark = ".", decimal.mark = ",", scientific = FALSE), " M")),
      angle = 90, vjust = 0.2, hjust=-0.1, size = 7, color = "blue",fontface = "bold"
    ) +
    scale_y_continuous(
      limits = y_limits_divida,
      breaks = y_breaks_divida,
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    scale_fill_manual(values = uf_colors_bySG)+
    labs(title = "Estados com Alta Dívida", x = "Estado", y = "Valor (R$)") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", color = "#1f5673"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      panel.background = element_rect(fill = "antiquewhite", color = NA)
    )
  
  
  p_geral + p_divida + plot_layout(ncol = 2, widths = c(2, 1))
})


h3("Tabela 1: Variáveis Financeiras", style = "color: #1f5673; font-weight: bold; margin-top: 30px;")
output$tab1_fin_table <- DT::renderDataTable({
  df <- financeiro_dt_all
  # Drop unused column
  df <- df[, !(names(df) %in% c("fef_share_pct"))]
  
  # Identifica colunas numéricas (exceto UF/Estado)
  data_cols <- setdiff(names(df), c("UF", "Estado"))
  
  # Formata para exibição: números com separador de milhar
  df[data_cols] <- lapply(df[data_cols], function(x) {
    if (is.numeric(x)) format(round(x, 0), big.mark = ".", decimal.mark = ",") else x
  })
  
  DT::datatable(
    df,
    extensions = 'Buttons',
    options = list(
      pageLength = 30,
      scrollX = TRUE,
      scrollY = "600px",
      autoWidth = FALSE,
      dom = 'Bfrtip',
      buttons = list(
        list(extend = "copy", text = "Copiar"),
        list(extend = "csv", filename = "Tabela_Financeira_PROPAG", text = "CSV"),
        list(extend = "excel", filename = "Tabela_Financeira_PROPAG", text = "Excel"),
        list(
          extend = "pdf",
          filename = "Tabela_Financeira_PROPAG",
          text = "PDF",
          orientation = "landscape",
          pageSize = "A4",
          messageTop = "Tabela 1: Variáveis Financeiras"
        )
      ),
      columnDefs = list(
        list(className = 'dt-nowrap', targets = "_all")
      )
    ),
    rownames = FALSE,
    class = "stripe nowrap display"
  )
})

  
  
  
  
  
  
  
  
  
  
  
  

  ######### TAB3 T OF 0 COUNT AB3 TAB3 TAB3  TAB3 TAB3 TAB3 TAB3  TAB3 TAB3 TAB3 TAB3  TAB3 TAB3 TAB3 TAB3  TAB3 TAB3 TAB3 TAB3  TAB3 TAB3 TAB3 TAB3  
  ##############################################################################################################################
  ### OUTPUT PLOT TAB 3  META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE 
  ##############################################################################################################################

    
  # --- Saída de gráfico de linhas com projeção ---
  output$oferta_ept_plot <- renderPlot({
    req(input$oferta_uf, input$oferta_ept_var)
    
    # Passo 1: Dados observados (EPT e ENSINO_MEDIO)
    df_filtered <- meta11a_opcoes |>
      filter(NM_UF == input$oferta_uf) |>
      group_by(ANO) |>
      summarise(
        EPT = sum(.data[[input$oferta_ept_var]], na.rm = TRUE),
        ENSINO_MEDIO = sum(QT_MAT_MED, na.rm = TRUE),  # Hardcoded to QT_MAT_MED
        .groups = "drop"
      )
    
    # Step 2: Determine meta target based on user selection
    if (is.null(input$meta_target_type) || input$meta_target_type == "pne11") {
      # Usar cálculo da Meta 11 do PNE (3x2013)
      base_2013_val <- df_filtered |> filter(ANO == 2013) |> pull(EPT)
      meta_target <- if (!is.na(base_2013_val)) 3 * base_2013_val else NA
      meta_label <- "Meta PNE 11 (3x2013)"
    } else {
      # Usar valor de meta customizado
      meta_target <- input$custom_meta_value
      meta_label <- "Meta Definida"
    }
    
    # Passo 3: Projeção linear de 2024-2035 para ambas variáveis
    df_recent <- df_filtered |> filter(ANO %in% 2020:2024)
    
    # EPT projection
    linear_model_ept <- lm(EPT ~ ANO, data = df_recent)
    slope_ept <- coef(linear_model_ept)["ANO"]
    start_val_ept <- df_filtered |> filter(ANO == 2024) |> pull(EPT) |> mean(na.rm = TRUE)
    
    # ENSINO_MEDIO projection  
    linear_model_med <- lm(ENSINO_MEDIO ~ ANO, data = df_recent)
    slope_med <- coef(linear_model_med)["ANO"]
    start_val_med <- df_filtered |> filter(ANO == 2024) |> pull(ENSINO_MEDIO) |> mean(na.rm = TRUE)
    
    future_years <- 2024:2035
    years_from_start <- future_years - 2024
    
    future_df <- data.frame(
      ANO = rep(future_years, 2),
      VALOR = c(start_val_ept + slope_ept * years_from_start,
                start_val_med + slope_med * years_from_start),
      TIPO = rep(c("EPT", "ENSINO_MEDIO"), each = length(future_years)),
      GRUPO = "PROJECAO"
    )
    
    # Passo 4: Preparar dados observados para plotagem
    observed_df <- data.frame(
      ANO = rep(df_filtered$ANO, 2),
      VALOR = c(df_filtered$EPT, df_filtered$ENSINO_MEDIO),
      TIPO = rep(c("EPT", "ENSINO_MEDIO"), each = nrow(df_filtered)),
      GRUPO = "OBSERVADO"
    )
    
    plot_data <- rbind(observed_df, future_df)
    
    # Passo 5: Criar o gráfico
    p <- ggplot(plot_data, aes(x = ANO, y = VALOR, color = GRUPO, linetype = TIPO)) +
      geom_line(size = 1.2) +
      geom_point(data = plot_data[plot_data$GRUPO == "OBSERVADO", ], size = 2) +
      scale_color_manual(
        values = c("OBSERVADO" = "steelblue", "PROJECAO" = "orange"),
        labels = c("OBSERVADO" = "Observado", "PROJECAO" = "Projeção")
      ) +
      scale_linetype_manual(
        values = c("EPT" = "solid", "ENSINO_MEDIO" = "dashed"),
        labels = c("EPT" = "EPT", "ENSINO_MEDIO" = "Ensino Médio")
      ) +
      labs(
        title = paste0("UF: ", input$oferta_uf),
        subtitle = paste("Variável EPT:", names(ept_vars)[ept_vars == input$oferta_ept_var]),
        x = "Ano",
        y = "Total de Matrículas",
        color = "Grupo",
        linetype = "Variável"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#1f5673"),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom"
      ) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      scale_x_continuous(breaks = c(2007, seq(2010, 2035, by = 5)))
    
    # Passo 6: Adicionar linha de meta se disponível
    if (!is.na(meta_target) && meta_target > 0) {
      p <- p + 
        geom_hline(yintercept = meta_target, 
                   linetype = "dashed", 
                   color = "red", 
                   size = 1, 
                   alpha = 0.8) +
        annotate("text", 
                 x = 2030, 
                 y = meta_target * 1.05, 
                 label = paste0(meta_label, ": ", formatC(round(meta_target), format="d", big.mark=".")),
                 color = "red", 
                 fontface = "bold",
                 size = 4)
    }
    
    return(p)
  })
  
  
  output$oferta_ept_table <- DT::renderDT({
    req(input$oferta_uf, input$oferta_ept_var)
    
    # Passo 1: Dados observados de meta11a_opcoes (EPT e ENSINO_MEDIO)
    df_obs <- meta11a_opcoes |>
      filter(NM_UF == input$oferta_uf) |>
      group_by(ANO) |>
      summarise(
        EPT = sum(.data[[input$oferta_ept_var]], na.rm = TRUE),
        ENSINO_MEDIO = sum(QT_MAT_MED, na.rm = TRUE),  # Hardcoded to QT_MAT_MED
        .groups = "drop"
      ) |>
      mutate(TIPO = "OBSERVADO")
    
    # Passo 2: Projeção (2024–2035) para ambas variáveis
    years_future <- 2024:2035
    slope_ept <- coef(lm(EPT ~ ANO, data = df_obs |> filter(ANO %in% 2020:2024)))["ANO"]
    slope_med <- coef(lm(ENSINO_MEDIO ~ ANO, data = df_obs |> filter(ANO %in% 2020:2024)))["ANO"]
    start_ept <- df_obs |> filter(ANO == 2024) |> pull(EPT) |> mean(na.rm = TRUE)
    start_med <- df_obs |> filter(ANO == 2024) |> pull(ENSINO_MEDIO) |> mean(na.rm = TRUE)
    
    df_proj <- tibble(
      ANO = years_future,
      EPT = start_ept + slope_ept * (years_future - 2024),
      ENSINO_MEDIO = start_med + slope_med * (years_future - 2024),
      TIPO = "PROJECAO"
    )
    
    # Passo 3: Combinar observados e projeção
    df_all <- bind_rows(df_obs, df_proj)
    
    # Passo 4: Adicionar meta Meta 11 do PNE (padrão por enquanto)
    base_2013_val <- df_all |> filter(ANO == 2013, TIPO == "OBSERVADO") |> pull(EPT)
    triplo_2013 <- if (!is.na(base_2013_val)) 3 * base_2013_val else NA
    df_all$PNE_META11 <- if (!is.na(triplo_2013)) round(triplo_2013) else NA
    
    # Passo 5: Transpor formato (incluir EPT e ENSINO_MEDIO)
    df_transposed <- df_all |>
      select(ANO, TIPO, EPT, ENSINO_MEDIO) |>
      pivot_longer(cols = c(EPT, ENSINO_MEDIO), names_to = "VAR", values_to = "VAL") |>
      unite("ROW", VAR, TIPO, sep = "_") |>
      pivot_wider(names_from = ANO, values_from = VAL)
    
    # Passo 6: Formatar numéricos com separadores de milhar
    df_transposed <- df_transposed |>
      mutate(across(where(is.numeric), ~ format(round(.), big.mark = ".", decimal.mark = ",")))
    
    # Passo 7: Exibir DataTable
    datatable(
      df_transposed,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        pageLength = 50,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "copy", text = "Copiar"),
          list(extend = "csv", filename = "Tabela_EPT", text = "CSV"),
          list(extend = "excel", filename = "Tabela_EPT", text = "Excel"),
          list(
            extend = "pdf",
            filename = "Tabela_EPT",
            text = "PDF",
            orientation = "landscape",
            pageSize = "A4",
            messageTop = "Tabela Transposta de Matrículas"
          )
        ),
        scrollX = TRUE
      ),
      class = "stripe nowrap display"
    )
  })
  
  
  ###############################################################################
  # SERVIDOR — ABA C1: OFERTA EPT (PROJEÇÕES FUTURAS)
  # Gráfico de tendência de matrículas com linhas Meta 11 e tabela de dados
  ###############################################################################
  output$meta11a_nova_plot <- renderPlot({
    req(input$meta11a_nova_uf, input$meta11a_nova_definicoes, input$meta11a_target_year, input$meta11a_target_type)
    
    # Abordagem simples - apenas forçar dependência na meta customizada
    if (!is.null(input$meta11a_target_type) && input$meta11a_target_type == "custom") {
      req(input$meta11a_custom_target)
    }
    
    df <- meta11a_opcoes %>%
      filter(NM_UF == input$meta11a_nova_uf) %>%
      select(ANO, all_of(input$meta11a_nova_definicoes)) %>%
      pivot_longer(
        cols = -ANO,
        names_to = "Definicao", 
        values_to = "Meta11a"
      )
    
    # Criar tooltip text com dados brutos
    df_tooltip <- meta11a_opcoes %>%
      filter(NM_UF == input$meta11a_nova_uf) %>%
      select(ANO, SG_UF, NM_UF, QT_MAT_MED, QT_MAT_PROF_TEC_PROPAG, QT_MAT_CURSO_TEC_CT, QT_MAT_CURSO_TEC_CONC) %>%
      mutate(across(where(is.numeric), ~format(round(.), big.mark = ".", decimal.mark = ","))) %>%
      unite("tooltip_text",
            QT_MAT_PROF_TEC_PROPAG, QT_MAT_CURSO_TEC_CT, QT_MAT_CURSO_TEC_CONC, QT_MAT_MED,
            sep = " | ",
            na.rm = TRUE
      ) %>%
      mutate(
        tooltip_text = paste0("Matrículas: ", tooltip_text),
        ANO = as.numeric(ANO)
      )
    
    df <- df %>%
      left_join(df_tooltip %>% select(ANO, tooltip_text), by = "ANO")
    
    # Definir cores para as opções
    definicao_colors <- c(
      "Meta11a_opcao1" = "blue",
      "Meta11a_opcao2" = "red",
      "Meta11a_opcao3" = "#9c6100"
    )
    
    df <- df %>%
      mutate(
        color_label = definicao_colors[Definicao],
        vjust_label = case_when(
          Definicao == "Meta11a_opcao3" ~ 1.5,
          TRUE ~ -0.8
        )
      )
    
    # PROJEÇÕES PARA CADA DEFINIÇÃO DE META 11a
    target_year <- as.numeric(input$meta11a_target_year)
    anos_proj <- 2024:target_year
    years_left <- target_year - 2024
    
    # Extrair ponto de partida por definição (valores de 2024)
    df_2024_pct <- df %>%
      filter(ANO == 2024) %>%
      select(Definicao, Meta11a)
    
    # === Ensino Médio projection logic ===
    df_em <- meta11a_opcoes %>%
      filter(NM_UF == input$meta11a_nova_uf) %>%
      distinct(ANO, QT_MAT_MED) %>%
      filter(ANO %in% 2020:2024) %>%
      rename(MEDIO = QT_MAT_MED)
    
    lm_med <- lm(MEDIO ~ ANO, data = df_em)
    base_slope <- coef(lm_med)["ANO"]
    adjusted_slope <- if (base_slope < 0) {
      base_slope * (2 - input$ensino_slope_factor)
    } else {
      base_slope * input$ensino_slope_factor
    }
    current_med <- df_em %>% filter(ANO == 2024) %>% pull(MEDIO)
    projected_med <- current_med + adjusted_slope * years_left
    
    # === LÓGICA CONDICIONAL PARA PERCENTAGEM META ===
    if (is.null(input$meta11a_target_type) || input$meta11a_target_type == "pne11a") {
      target_percentage <- 0.5
      meta_label <- "Meta 11a: EPT forma 50% da matricula EM"
      target_pct_text <- "50%"
    } else {
      custom_value <- input$meta11a_custom_target
      if (is.null(custom_value) || is.na(custom_value) || custom_value <= 0) {
        custom_value <- 100000
      }
      target_percentage <- custom_value / projected_med
      target_pct_text <- paste0(round(target_percentage * 100, 1), "%")
      meta_label <- paste0("Meta Definida: EPT forma ", target_pct_text, " da matricula EM")
    }
    #  y_top <- max(0.6, target_percentage + 0.2)
    y_top <- target_percentage + 0.05
    em_text <- paste0(
      "<b>Projeção EM</b><br>",
      format(round(projected_med), big.mark = ".", decimal.mark = ",")
    )
    
    # === Raw EPT numeradores de 2024 por opção ===
    df_2024_raw <- meta11a_opcoes %>%
      filter(NM_UF == input$meta11a_nova_uf, ANO == 2024) %>%
      select(QT_MAT_PROF_TEC_PROPAG, QT_MAT_CURSO_TEC_CT, QT_MAT_CURSO_TEC_CONC, QT_MAT_MED) %>%
      summarise(
        Meta11a_opcao1 = QT_MAT_PROF_TEC_PROPAG,
        Meta11a_opcao2 = coalesce(QT_MAT_CURSO_TEC_CT, 0) + coalesce(QT_MAT_CURSO_TEC_CONC, 0),
        Meta11a_opcao3 = QT_MAT_CURSO_TEC_CT,
        MEDIO = QT_MAT_MED
      )
    
    # Cálculo dos textos para cada definição (usando target_percentage dinâmico)
    popup_info <- df_2024_pct %>%
      mutate(
        raw_EPT = map_dbl(Definicao, ~ df_2024_raw[[.x]]),
        growth_abs = (target_percentage * projected_med - raw_EPT) / max(1, years_left),
        growth_pct = (target_percentage * 100 - Meta11a * 100) / years_left,
        label_text = paste0(
          "<b>Para atingir ", target_pct_text, "</b><br>",
          "em ", target_year, ":<br>",
          "crescimento de<br>",
          "<span style='color:", definicao_colors[Definicao], "'><b>",
          round(growth_pct, 1), "%</b></span> ao ano<br>",
          "(", formatC(round(growth_abs), format = "d", big.mark = "."), " alunos/ano)"
        ),
        label_x = target_year + c(-1.8, 0, 1.8)[match(Definicao, names(definicao_colors))],
        #    label_y = 0.43 + c(0.08, 0, 0.08)[match(Definicao, names(definicao_colors))],
        
        label_y = if (is.null(input$meta11a_target_type) || input$meta11a_target_type == "pne11a") {
          pmin(0.43 + c(0.08, 0, 0.08)[match(Definicao, names(definicao_colors))], y_top * 0.98)
        } else {
          pmin((target_percentage + 0.15) + c(0.08, 0, 0.08)[match(Definicao, names(definicao_colors))], y_top * 0.98)         
        },
        
        
        
        label_col = definicao_colors[Definicao]
      )
    
    # Criar linhas convergentes para target_percentage a partir de cada ponto de partida
    proj_lines <- df_2024_pct %>%
      mutate(
        proj = map(Meta11a, ~ {
          tibble(
            ANO = anos_proj,
            Meta11a = .x + ((target_percentage - .x) / years_left) * (anos_proj - 2024)
          )
        })
      ) %>%
      select(Definicao, proj) %>%
      unnest(proj)
    
    # Target point at dynamic percentage
    target_df <- data.frame(
      ANO = target_year,
      Meta11a = target_percentage
    )
    
    # Gráfico
    gg <- ggplot(df, aes(x = ANO, y = Meta11a, color = Definicao)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      geom_hline(yintercept = target_percentage, color = "darkgreen", linetype = "dashed", linewidth = 1.1) +
      annotate(
        "text", x = 2025, y = target_percentage,
        label = meta_label, size = 6,
        color = "darkorange", vjust = -1, fontface = "bold"
      ) +
      ggtext::geom_richtext(
        aes(label = paste0("<b>", scales::percent(Meta11a, accuracy = 0.1), "</b>"),
            color = Definicao,
            vjust = vjust_label),
        label.color = df$color_label,
        fill = "white",
        size = 4,
        label.size = 0.25,
        label.r = unit(5, "pt"),
        label.padding = unit(c(3, 5, 3, 5), "pt")
      ) +
      scale_color_manual(values = definicao_colors) +
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, y_top)) +
      scale_x_continuous(breaks = 2007:2035, limits = c(2007, 2035)) +
      labs(
        title = paste("Meta 11a – Comparação para:", input$meta11a_nova_uf),
        x = "Ano",
        y = "Atingimento PNE Meta 11a: Porcentagem Matricula de EPT sobre EM",
        color = "Definição"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14, colour = "blue", face = "bold"),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "blue")
      ) +
      ggtext::geom_textbox(
        data = data.frame(x = 2012.5, y = 0.9*(target_percentage)),
        aes(x = x, y = y, label = paste(
          "<b>META 11a (Parte 1): </b> DEFINIÇÕES do Numerador de Matrículas:",
          " ",
          "Opção 1: Técnico Médio (Integrado, Concomitante, Subsequente e EJA) (PROPAG)",
          " ",
          "Opção 2: Técnico Médio (Integrado e Concomitante)",
          " ",
          "Opção 3: Técnico Médio (Somente Integrado)",
          " ",
          "Definição Denominador (comum para as 3 opções): Matrículas no Ensino Médio <br> (Variavel: QT_MAT_MED do Censo Escolar do INEP)",
          sep = "<br>"
        )),
        width = unit(0.4, "npc"),
        fill = "#fdf6e3",
        box.color = "gray40",
        halign = 0,
        color = "black",
        size = 5,
        fontface = "plain",
        lineheight = 1.1,
        box.padding = unit(c(5, 6, 5, 6), "pt"),
        r = unit(6, "pt")
      )
    
    # Adicionar linhas de projeção
    if (nrow(proj_lines) > 0) {
      gg <- gg +
        geom_line(
          data = proj_lines,
          mapping = aes(x = ANO, y = Meta11a, color = Definicao),
          linetype = "dashed",
          linewidth = 1,
          inherit.aes = FALSE
        )
    }
    
    # Adicionar ponto de meta
    gg <- gg +
      geom_point(
        data = target_df,
        aes(x = ANO, y = Meta11a),
        color = "red",
        fill = "white",
        size = 4,
        shape = 21,
        stroke = 1.5,
        inherit.aes = FALSE
      ) +
      geom_point(
        data = target_df,
        aes(x = ANO, y = Meta11a),
        color = "red",
        size = 2,
        shape = 16,
        inherit.aes = FALSE
      )
    
    # EM projected label
    em_text <- paste0(
      "<b>EM: Projetado</b><br>",
      format(round(projected_med), big.mark = ".", decimal.mark = ","), " alunos<br>matriculados"
    )
    
    gg <- gg +
      ggtext::geom_richtext(
        data = data.frame(x = target_year, y = max(0.61, target_percentage * 1.05)),
        aes(x = x, y = y, label = em_text),
        fill = "white",
        label.color = "blue",
        color = "blue",
        size = 3.5,
        label.size = 0.5,
        label.r = unit(5, "pt"),
        label.padding = unit(c(4, 6, 4, 6), "pt"),
        vjust = 1,
        hjust = 0.5,
        inherit.aes = FALSE
      )
    
    # POPUPS POR DEFINICAO SELECIONADA
    for (i in 1:nrow(popup_info)) {
      gg <- gg +
        ggtext::geom_richtext(
          data = popup_info[i, ],
          aes(x = label_x, y = label_y, label = label_text),
          fill = "white",
          label.color = popup_info$label_col[i],
          color = "black",
          size = 3.5,
          label.size = 0.5,
          label.r = unit(6, "pt"),
          label.padding = unit(c(4, 6, 4, 6), "pt"),
          vjust = 1,
          hjust = 0.5,
          inherit.aes = FALSE
        )
    }
    
    # Adicionar bolha EM
    gg <- gg +
      ggtext::geom_richtext(
        data = data.frame(x = target_year, y = max(0.52, target_percentage * 0.9)),
        aes(x = x, y = y, label = em_text),
        fill = "#d0ebff",
        label.color = "steelblue",
        color = "black",
        size = 4,
        label.size = 0.5,
        label.r = unit(5, "pt"),
        label.padding = unit(c(4, 6, 4, 6), "pt"),
        fontface = "plain",
        hjust = 0.5,
        vjust = 0,
        inherit.aes = FALSE
      )
    
    return(gg)
  })
  
  ##############################################################################################################
  ## ABA C3 — OFERTA EPT REDES 
  ##############################################################################################################
  
  
  observeEvent(input$censo_uf, {
    if (is.null(input$censo_uf) || length(input$censo_uf) == 0) {
      updatePickerInput(session, "censo_municipio", choices = character(0), selected = character(0))
    } else {
      choices <- unique(dft_informality_geo_codes[NM_UF %in% input$censo_uf, NM_MUN])
      choices <- sort(choices[!is.na(choices)])
      updatePickerInput(session, "censo_municipio", choices = choices, selected = choices)  # AUTO-SELECT ALL
    }
  })
  
  # --- SINGLE Title Creation (simplified) ---
  create_censo_title <- reactive({
    if (is.null(input$censo_uf) || length(input$censo_uf) == 0) {
      return("Nenhuma seleção")
    }
    
    uf_text <- paste(input$censo_uf, collapse = ", ")
    
    mun_count <- if (!is.null(input$censo_municipio) && length(input$censo_municipio) > 0) {
      length(input$censo_municipio)
    } else {
      length(unique(dft_informality_geo_codes[NM_UF %in% input$censo_uf, NM_MUN]))
    }
    
    return(paste(uf_text, "-", mun_count, "municípios"))
  })
  
  # --- Reativo Principal para Tabelas - FILTRAR Curso Normal ---
  rkt_censo_filtered <- reactive({
    req(input$censo_year)
    req(input$censo_uf)
    req(length(input$censo_uf) > 0)
    req(input$censo_dependencia)
    req(length(input$censo_dependencia) > 0)
    
    data <- df_censo_combined %>%
      filter(ANO == input$censo_year) %>%
      filter(NM_UF %in% input$censo_uf) %>%
      filter(TP_DEPENDENCIA %in% input$censo_dependencia) %>%
      # FILTRAR Curso Normal/Magistério
      filter(!grepl("Normal|Magistério|Magisterio", NO_AREA_CURSO_PROFISSIONAL, ignore.case = TRUE))
    
    if (!is.null(input$censo_municipio) && length(input$censo_municipio) > 0) {
      data <- data %>% filter(NM_MUN %in% input$censo_municipio)
    }
    
    # RECALCULATE TOTAL WITHOUT NORMAL/MAGISTÉRIO
    data <- data %>%
      mutate(QT_MAT_CURSO_TEC_TOTNOMAG = QT_MAT_CURSO_TEC_CT + 
               QT_MAT_CURSO_TEC_CONC + 
               QT_MAT_TEC_SUBS + 
               QT_MAT_TEC_EJA)
    
    return(data)
  })
  
  # --- Novo Reativo para Gráfico - FILTRAR Curso Normal ---
  rkt_ept_redeplot <- reactive({
    req(input$censo_year)
    req(input$censo_uf)
    req(length(input$censo_uf) > 0)
    
    data <- df_censo_combined %>%
      filter(ANO == input$censo_year) %>%
      filter(NM_UF %in% input$censo_uf) %>%
      # FILTRAR Curso Normal/Magistério
      filter(!grepl("Normal|Magistério|Magisterio", NO_AREA_CURSO_PROFISSIONAL, ignore.case = TRUE))
    
    if (!is.null(input$censo_municipio) && length(input$censo_municipio) > 0) {
      data <- data %>% filter(NM_MUN %in% input$censo_municipio)
    }
    
    # RECALCULATE TOTAL WITHOUT NORMAL/MAGISTÉRIO
    data <- data %>%
      mutate(QT_MAT_CURSO_TEC_TOTNOMAG = QT_MAT_CURSO_TEC_CT + 
               QT_MAT_CURSO_TEC_CONC + 
               QT_MAT_TEC_SUBS + 
               QT_MAT_TEC_EJA)
    
    return(data)
  })


  output$censo_table_eixo <- renderDT({
    # Simple validation
    if (is.null(input$censo_uf) || length(input$censo_uf) == 0 ||
        is.null(input$censo_dependencia) || length(input$censo_dependencia) == 0) {
      empty_data <- data.frame(Mensagem = "Selecione UF e Dependência Administrativa")
      return(datatable(empty_data, options = list(dom = 't'), rownames = FALSE))
    }
    
    # Obter dados base filtrados
    data <- df_censo_combined %>%
      filter(ANO == input$censo_year) %>%
      filter(NM_UF %in% input$censo_uf) %>%
      filter(TP_DEPENDENCIA %in% input$censo_dependencia) %>%
      filter(!grepl("Ensino Médio - Curso Normal", NO_AREA_CURSO_PROFISSIONAL, ignore.case = TRUE))
    
    # Aplicar filtro de município se selecionado
    if (!is.null(input$censo_municipio) && length(input$censo_municipio) > 0) {
      data <- data %>% filter(NM_MUN %in% input$censo_municipio)
    }
    
    # Verificar se existem dados
    if (nrow(data) == 0) {
      empty_data <- data.frame(Mensagem = "Nenhum dado encontrado para a seleção atual")
      return(datatable(empty_data, options = list(dom = 't'), rownames = FALSE))
    }
    
    # Criar título
    uf_text <- paste(input$censo_uf, collapse = ", ")
    mun_count <- if (!is.null(input$censo_municipio) && length(input$censo_municipio) > 0) {
      length(input$censo_municipio)
    } else {
      length(unique(data$NM_MUN))
    }
    title_content <- paste(uf_text, "-", mun_count, "municípios")
    
    # Agregar por eixo - usar nomes simples primeiro
    eixo_data <- data %>%
      group_by(Eixo = NO_AREA_CURSO_PROFISSIONAL) %>%
      summarise(
        Cursos = sum(QT_CURSO_TEC, na.rm = TRUE),
        Matriculas_Total = sum(QT_MAT_CURSO_TEC_CT + QT_MAT_CURSO_TEC_CONC + 
                                 QT_MAT_TEC_SUBS + QT_MAT_TEC_EJA, na.rm = TRUE),
        Integrado = sum(QT_MAT_CURSO_TEC_CT, na.rm = TRUE),
        Concomitante = sum(QT_MAT_CURSO_TEC_CONC, na.rm = TRUE),
        Subsequente = sum(QT_MAT_TEC_SUBS, na.rm = TRUE),
        EJA_Nivel_Medio = sum(QT_MAT_TEC_EJA, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Matriculas_Total))
    
    # Adicionar linha de total com nomes de colunas correspondentes
    total_row <- data.frame(
      Eixo = paste("TOTAL -", title_content),
      Cursos = sum(eixo_data$Cursos),
      Matriculas_Total = sum(eixo_data$Matriculas_Total),
      Integrado = sum(eixo_data$Integrado),
      Concomitante = sum(eixo_data$Concomitante),
      Subsequente = sum(eixo_data$Subsequente),
      EJA_Nivel_Medio = sum(eixo_data$EJA_Nivel_Medio)
    )
    
    final_data <- bind_rows(total_row, eixo_data)
    
    # AGORA renomear colunas para exibição
    names(final_data) <- c("Eixo", "Cursos", "Matrículas Total EPT", 
                           "Integrado", "Concomitante", "Subsequente", "EJA Nível Médio")
    
    datatable(final_data, rownames = FALSE, extensions = 'Buttons',
              options = list(pageLength = 15, scrollX = TRUE, dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel'),
                             selection = 'multiple'))
  })
  
  # --- Tabela 1b - COM colunas de modalidade, usando total recalculado ---
  output$censo_table_curso <- renderDT({
    data <- rkt_censo_filtered()  # This already has QT_MAT_CURSO_TEC_TOTNOMAG
    selected_rows <- input$censo_table_eixo_rows_selected
    
    req(nrow(data) > 0)
    req(!is.null(selected_rows) && length(selected_rows) > 0)
    
    # Obter dados de eixo para encontrar eixos selecionados
    eixo_list <- data %>%
      group_by(Eixo = NO_AREA_CURSO_PROFISSIONAL) %>%
      summarise(`Matrículas Total` = sum(QT_MAT_CURSO_TEC_TOTNOMAG, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(`Matrículas Total`))
    
    # Ajustar para linha de total
    selected_eixos <- eixo_list$Eixo[selected_rows - 1]
    selected_eixos <- selected_eixos[!is.na(selected_eixos)]
    
    if (length(selected_eixos) == 0) return(NULL)
    
    # Atualizado para usar total recalculado
    curso_data <- data %>%
      filter(NO_AREA_CURSO_PROFISSIONAL %in% selected_eixos) %>%
      group_by(Eixo = NO_AREA_CURSO_PROFISSIONAL, Curso = NO_CURSO_EDUC_PROFISSIONAL) %>%
      summarise(
        `Matrículas Total EPT` = sum(QT_MAT_CURSO_TEC_TOTNOMAG, na.rm = TRUE),  # USING NEW TOTAL
        Integrado = sum(QT_MAT_CURSO_TEC_CT, na.rm = TRUE),
        Concomitante = sum(QT_MAT_CURSO_TEC_CONC, na.rm = TRUE),
        Subsequente = sum(QT_MAT_TEC_SUBS, na.rm = TRUE),
        `EJA Nível Médio` = sum(QT_MAT_TEC_EJA, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Eixo, desc(`Matrículas Total EPT`))
    
    datatable(curso_data, rownames = FALSE, extensions = 'Buttons',
              caption = htmltools::tags$caption(
                style = "caption-side: top; text-align: left; color: #1f5673; font-size: 16px; font-weight: bold; margin-bottom: 10px;",
                paste("Cursos para Eixos Selecionados:", paste(selected_eixos, collapse = ", "))
              ),
              options = list(pageLength = 15, scrollX = TRUE, dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel')))
  })
  output$censo_stacked_plot <- renderPlotly({
    data <- rkt_ept_redeplot()
    req(nrow(data) > 0)
    
    # Agregar por Dependência para TODOS os eixos
    plot_data <- data %>%
      group_by(Dependencia = case_when(
        TP_DEPENDENCIA == "1" ~ "Federal",
        TP_DEPENDENCIA == "2" ~ "Estadual", 
        TP_DEPENDENCIA == "3" ~ "Municipal",
        TP_DEPENDENCIA == "4" ~ "Privada",
        TRUE ~ "Outro"
      )) %>%
      summarise(
        Total = sum(QT_MAT_CURSO_TEC_TOTNOMAG, na.rm = TRUE),
        Integrado = sum(QT_MAT_CURSO_TEC_CT, na.rm = TRUE),
        Concomitante = sum(QT_MAT_CURSO_TEC_CONC, na.rm = TRUE),
        Subsequente = sum(QT_MAT_TEC_SUBS, na.rm = TRUE),
        `EJA Nível Médio` = sum(QT_MAT_TEC_EJA, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calcular total para percentagem
    total_matriculas <- sum(plot_data$Total)
    
    # Pivotar para longo para plotagem
    plot_long <- plot_data %>%
      pivot_longer(cols = c(Total, Integrado, Concomitante, Subsequente, `EJA Nível Médio`),
                   names_to = "Modalidade",
                   values_to = "Matriculas")
    
    # Calcular percentagem para cada modalidade (soma entre todas as dependências)
    modalidade_totals <- plot_long %>%
      group_by(Modalidade) %>%
      summarise(Total_Modalidade = sum(Matriculas)) %>%
      mutate(Percentage = round((Total_Modalidade / total_matriculas) * 100, 1))
    
    # Adicionar percentagem aos dados do gráfico
    plot_long <- plot_long %>%
      left_join(modalidade_totals, by = "Modalidade")
    
    # Usar cores padrão do plotly
    dep_colors <- c("Federal" = "#1f77b4",    # Blue
                    "Estadual" = "#ff7f0e",   # Orange
                    "Municipal" = "#2ca02c",  # Green
                    "Privada" = "#d62728")    # Red
    
    # Factor to control order
    plot_long$Modalidade <- factor(plot_long$Modalidade, 
                                   levels = c("Total", "Integrado", "Concomitante", 
                                              "Subsequente", "EJA Nível Médio"))
    
    # Criar o gráfico WITHOUT text labels on bars
    p <- plot_ly(plot_long, 
                 x = ~Modalidade, 
                 y = ~Matriculas,
                 color = ~Dependencia,
                 colors = dep_colors,
                 type = 'bar',
                 text = ~paste("Dependência:", Dependencia, 
                               "<br>Modalidade:", Modalidade,
                               "<br>Matrículas:", format(Matriculas, big.mark = ".", decimal.mark = ","),
                               "<br>Percentual do Total:", Percentage, "%"),
                 hovertemplate = "%{text}<extra></extra>",
                 hoverlabel = list(font = list(size = 14)),
                 textposition = "none")  %>%  # This removes the text labels on bars
      layout(barmode = 'stack',
             title = list(text = "Matrículas EPT por Modalidade e Dependência Administrativa",
                          font = list(color = "#1f5673", size = 20)),
             xaxis = list(
               title = list(text = "Modalidade", font = list(size = 16)),
               tickfont = list(size = 14)
             ),
             yaxis = list(
               title = list(text = "Número de Matrículas EPT", font = list(size = 16)),
               tickformat = ",.0f",
               tickfont = list(size = 14)
             ),
             legend = list(
               title = list(text = "Dependência", font = list(size = 16)),
               font = list(size = 14)
             ),
             hovermode = 'closest',
             margin = list(l = 80, r = 80, t = 100, b = 100),
             annotations = list())  # Clear any auto-generated annotations
    
    # Adicionar rótulos de percentagem acima de cada barra empilhada
    for(i in 1:nrow(modalidade_totals)) {
      p <- p %>% add_annotations(
        x = modalidade_totals$Modalidade[i],
        y = modalidade_totals$Total_Modalidade[i],
        text = paste0("<b>", modalidade_totals$Percentage[i], "%</b>"),
        xref = "x",
        yref = "y",
        showarrow = FALSE,
        yshift = 20,
        font = list(size = 18, color = "#1f5673")
      )
    }
    
    return(p)
  })
  

  
  ##############################################################################################################################
  ###  ABA C4 — MODELO DE RESÍDUOS EPT (PREVISÃO LINEAR OLS)
  ##############################################################################################################################
  
  
  observe({
    updatePickerInput(session, "tab_resi_states",
                      choices = setNames(sort(unique(df_residuals_ols$SG_UF)), 
                                         sort(unique(df_residuals_ols$NM_UF))),
                      selected = head(sort(unique(df_residuals_ols$SG_UF)), 10))
  })
  
  # Reactive filtered data
  # Atualizar reativo de dados filtrados para lidar com múltiplas seleções:
  rkt_residuals_filtered <- reactive({
    req(input$tab_resi_year, input$tab_resi_dependency, input$tab_resi_sector)
    
    df_filtered <- df_residuals_ols %>%
      filter(ANO == input$tab_resi_year,
             TP_DEPENDENCIA %in% input$tab_resi_dependency,
             economic_sector %in% input$tab_resi_sector)
    
    if (!is.null(input$tab_resi_states) && length(input$tab_resi_states) > 0) {
      df_filtered <- df_filtered %>% filter(SG_UF %in% input$tab_resi_states)
    }
    
    return(df_filtered)
  })
  
  
  # Plot output
  output$tab_resiPlot <- renderPlotly({
    df_plot <- rkt_residuals_filtered()
    req(nrow(df_plot) > 0)
    
    # Calculate fixed axis ranges
    x_range <- range(df_residuals_ols$residual, na.rm = TRUE)
    
    if (input$tab_resi_yaxis == "pib_per_capita") {
      y_var <- log(df_plot$pib_per_capita)
      y_title <- "Log PIB per Capita"
      y_range <- range(log(df_residuals_ols$pib_per_capita), na.rm = TRUE)
    } else {
      y_var <- df_plot$sector_alignment
      y_title <- "Alinhamento Setorial (Ratio)"
      y_range <- range(df_residuals_ols$sector_alignment, na.rm = TRUE)
    }
    
    # Construir título fora do layout
    dep_labels <- paste(input$tab_resi_dependency, collapse = ", ")
    sector_labels <- c("agriculture" = "Agricultura", "industry" = "Indústria", 
                       "services" = "Serviços", "administration" = "Administração")
    selected_sectors <- sector_labels[input$tab_resi_sector]
    sector_text <- paste(selected_sectors, collapse = ", ")
    
    plot_title <- paste0("Predição de Matrículas EPT por Dependência e Setor por UF - ",
                         dep_labels, " / ", sector_text, " (", input$tab_resi_year, ")")
    
    # Mapear dependências para formas e setores para bordas
    shape_map <- c("Federal" = "circle", "Estadual" = "square", 
                   "Municipal" = "triangle-up", "Privada" = "diamond")
    border_map <- c("agriculture" = "red", "industry" = "blue", 
                    "services" = "green", "administration" = "pink")
    
    df_plot$border_color <- border_map[df_plot$economic_sector]
    
    p <- plot_ly(df_plot, 
                 x = ~residual, 
                 y = y_var,
                 size = ~QT_MAT_CURSO_TEC,
                 color = ~NM_UF,
                 symbol = ~TP_DEPENDENCIA,
                 symbols = shape_map,
                 type = "scatter",
                 mode = "markers",
                 sizes = c(40, 160),
                 marker = list(opacity = 1,
                               line = list(width = 2, 
                                           color = ~border_color))) %>%
      layout(
        xaxis = list(title = "Residual (Desempenho vs Predição)", 
                     range = x_range, zeroline = TRUE),
        yaxis = list(title = y_title, range = y_range),
        title = plot_title
      )
    
    return(p)
  })
  
  # Saída de tabela com formatação brasileira
  # Saída de tabela com formatação brasileira - SORTED BY RESIDUALS DESCENDING
  output$tab_resiTable <- renderDT({
    df_table <- rkt_residuals_filtered()
    req(nrow(df_table) > 0)
    
    df_display <- df_table %>%
      arrange(desc(residual)) %>%  # ADD THIS LINE - Sort by residuals descending
      select(Estado = NM_UF, 
             Residual = residual, 
             `PIB per Capita` = pib_per_capita,
             `Alinhamento Setorial` = sector_alignment, 
             `Matrículas` = QT_MAT_CURSO_TEC) %>%
      mutate(
        Residual = format(round(Residual, 3), decimal.mark = ","),
        `PIB per Capita` = format(round(`PIB per Capita`, 1), big.mark = ".", decimal.mark = ","),
        `Alinhamento Setorial` = format(round(`Alinhamento Setorial`, 3), decimal.mark = ","),
        `Matrículas` = format(`Matrículas`, big.mark = ".", decimal.mark = ",")
      )
    
    datatable(df_display, 
              options = list(pageLength = 15, 
                             order = list(list(1, 'desc'))),  # Also set default sort on Residual column
              rownames = FALSE)
  })
  # Summary output
  output$tab_resi_summary <- renderUI({
    df_summary <- rkt_residuals_filtered()
    n_states <- nrow(df_summary)
    n_positive <- sum(df_summary$residual > 0, na.rm = TRUE)
    
    tagList(
      tags$p(paste("Ano:", input$tab_resi_year)),
      tags$p(paste("Estados selecionados:", n_states)),
      tags$p(paste("Desempenho superior:", n_positive), style = "color: green;")
    )
  })
  
  
  
  ###############################################################################
  # SERVIDOR — ABA D1: DINAMISMO ECONÔMICO
  # Renderização de mapa Leaflet e tabela de dados para indicadores de dinamismo municipal
  ###############################################################################


# Inicializar opções geográficas para dinamismo
observe({
  updatePickerInput(session, "uf_dyn", 
                    choices = sort(unique(dynamism_geo$NM_UF[!is.na(dynamism_geo$NM_UF)])),
                    selected = "São Paulo")
})

# Filtragem geográfica hierárquica (mesmo padrão do APL)
observeEvent(input$uf_dyn, {
  if (is.null(input$uf_dyn) || length(input$uf_dyn) == 0) {
    updatePickerInput(session, "rgintm_dyn", choices = character(0), selected = character(0))
  } else {
    uf_filtered <- dynamism_geo[dynamism_geo$NM_UF %in% input$uf_dyn & !is.na(dynamism_geo$NM_RGIINTM), ]
    rgi_choices <- sort(unique(uf_filtered$NM_RGIINTM))
    updatePickerInput(session, "rgintm_dyn", choices = rgi_choices, selected = rgi_choices)
  }
}, ignoreInit = TRUE)

observeEvent(input$rgintm_dyn, {
  if (is.null(input$rgintm_dyn) || length(input$rgintm_dyn) == 0) {
    updatePickerInput(session, "rgimed_dyn", choices = character(0), selected = character(0))
  } else {
    rgi_filtered <- dynamism_geo[dynamism_geo$NM_RGIINTM %in% input$rgintm_dyn & !is.na(dynamism_geo$NM_RGIMED), ]
    rgimed_choices <- sort(unique(rgi_filtered$NM_RGIMED))
    updatePickerInput(session, "rgimed_dyn", choices = rgimed_choices, selected = rgimed_choices)
  }
}, ignoreInit = TRUE)

observeEvent(input$rgimed_dyn, {
  if (is.null(input$rgimed_dyn) || length(input$rgimed_dyn) == 0) {
    updatePickerInput(session, "mun_dyn", choices = character(0), selected = character(0))
  } else {
    rgimed_filtered <- dynamism_geo[dynamism_geo$NM_RGIMED %in% input$rgimed_dyn & !is.na(dynamism_geo$NM_MUN), ]
    mun_choices <- sort(unique(rgimed_filtered$NM_MUN))
    updatePickerInput(session, "mun_dyn", choices = mun_choices, selected = character(0))
  }
}, ignoreInit = TRUE)

# Filtered dynamism data reactive
rkt_filtered_dynamism_data <- reactive({
  data <- dynamism_geo
  
  # Apply geographic filters
  if (!is.null(input$uf_dyn) && length(input$uf_dyn) > 0) {
    data <- data[data$NM_UF %in% input$uf_dyn, ]
  }
  if (!is.null(input$rgintm_dyn) && length(input$rgintm_dyn) > 0) {
    data <- data[data$NM_RGIINTM %in% input$rgintm_dyn, ]
  }
  if (!is.null(input$rgimed_dyn) && length(input$rgimed_dyn) > 0) {
    data <- data[data$NM_RGIMED %in% input$rgimed_dyn, ]
  }
  if (!is.null(input$mun_dyn) && length(input$mun_dyn) > 0) {
    data <- data[data$NM_MUN %in% input$mun_dyn, ]
  }
  
  # Apply performance filters
  data <- data[data$dynamism_decile >= input$min_decile_dyn & 
               data$avg_population >= input$min_pop_dyn, ]
  
  return(data)
})

# Dados do mapa para coroplético
# Dados do mapa para coroplético - SIMPLIFIED VERSION
rkt_dynamism_map_data <- reactive({
  req(nrow(rkt_filtered_dynamism_data()) > 0)
  
  # Obter dados de dinamismo filtrados
  filtered_data <- rkt_filtered_dynamism_data()
  
  # Criar resumo simples por município (evitando conflitos de colunas)
  dyn_summary <- filtered_data[, .(
    dynamism_index = first(dynamism_index),
    dynamism_decile = first(dynamism_decile),
    avg_population = first(avg_population)
  ), by = .(CO_MUN, NM_MUN)]
  
  # Converter CO_MUN para character para junção
  dyn_summary$CO_MUN <- as.character(dyn_summary$CO_MUN)
  
  # Filtrar sf_regioes para UFs selecionadas
  map_data <- sf_regioes
  if (!is.null(input$uf_dyn) && length(input$uf_dyn) > 0) {
    map_data <- map_data %>% filter(NM_UF %in% input$uf_dyn)
  }
  
  # Left join simples - apenas adicionar variáveis de dinamismo
  map_data <- map_data %>%
    left_join(dyn_summary %>% select(CO_MUN, dynamism_index, dynamism_decile, avg_population), 
              by = "CO_MUN")
  
  # Preencher NAs para municípios sem dados de dinamismo
  map_data$dynamism_decile[is.na(map_data$dynamism_decile)] <- 0
  map_data$dynamism_index[is.na(map_data$dynamism_index)] <- 0
  
  return(map_data)
})

# Summary output
output$dyn_summary <- renderUI({
  data <- rkt_filtered_dynamism_data()
  
  HTML(paste0(
    "<strong>Municípios analisados:</strong> ", nrow(data), "<br>",
    "<strong>Dinamismo médio:</strong> ", round(mean(data$dynamism_index, na.rm=TRUE), 2), "<br>", 
    "<strong>Top decil (9-10):</strong> ", sum(data$dynamism_decile >= 9), "<br>",
    "<strong>População total:</strong> ", formatC(sum(data$avg_population, na.rm=TRUE), format="d", big.mark=".")
  ))
})

# Map output
output$dyn_map <- renderLeaflet({
  map_data <- rkt_dynamism_map_data()
  
  # Paleta de cores para decis
  pal <- colorNumeric(
    palette = "RdYlGn", # Red-Yellow-Green (low to high performance)
    domain = 1:10,
    na.color = "transparent"
  )
  
  leaflet(map_data) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(dynamism_decile),
      weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
      popup = ~paste0(
        "<strong>", NM_MUN, "</strong><br/>",
        "UF: ", NM_UF, "<br/>",
        "Dinamismo: ", round(dynamism_index, 2), "<br/>", 
        "Decil: ", dynamism_decile, "<br/>",
        "População: ", formatC(round(avg_population), format="d", big.mark=".")
      )
    ) %>%
    addLegend(pal = pal, values = 1:10, opacity = 0.7, title = "Decil Dinamismo", position = "bottomright")
})

# Saída de tabela - COM TODOS OS 4 SETORES (AGRO, INDÚSTRIA, SERVIÇOS, ADMIN)
output$dyn_table <- renderDT({
  data <- rkt_filtered_dynamism_data()
  req(nrow(data) > 0)
  
  # Tabela completa com todos os 4 setores
  table_data <- data[, .(
    UF = SG_UF,
    Município = NM_MUN, 
    `Índice Dinamismo` = round(dynamism_index, 2),
    Decil = dynamism_decile,
    `População Média` = formatC(round(avg_population), format="d", big.mark="."),
    `PIB per capita 2021 (R$)` = formatC(round(pib_per_capita_2021), format="d", big.mark="."),
    `Agro (%)` = agro_pct,
    `Indústria (%)` = industria_pct,
    `Serviços (%)` = servicos_pct,
    `Admin (%)` = admin_pct,  # Added missing admin sector
    `Crescimento P1 (%)` = round(period1_avg_growth, 1),
    `Crescimento P2 (%)` = round(period2_avg_growth, 1)
  )]
  
  # Sort by dynamism index
  table_data <- table_data[order(-`Índice Dinamismo`)]
  
  datatable(table_data, rownames = FALSE, extensions = 'Buttons',
            options = list(
              pageLength = 15, 
              scrollX = TRUE, 
              dom = 'Bfrtip',
              buttons = list(
                list(extend = "copy", text = "Copiar"),
                list(extend = "csv", filename = "Dinamismo_Municipal_Completo", text = "CSV"),
                list(extend = "excel", filename = "Dinamismo_Municipal_Completo", text = "Excel")
              ),
              columnDefs = list(
                list(width = '30px', targets = 0),   # UF
                list(width = '120px', targets = 1),  # Município
                list(width = '70px', targets = c(2, 3)),  # Index, Decil
                list(width = '90px', targets = c(4, 5)),  # População, PIB
                list(width = '55px', targets = c(6, 7, 8, 9, 10, 11)),  # All 4 sectors + 2 growth percentages
                list(className = 'dt-center', targets = c(2, 3, 6, 7, 8, 9, 10, 11))
              )
            ), 
            class = "stripe nowrap display compact"
  ) %>%
    formatStyle("Índice Dinamismo", backgroundColor = styleInterval(c(15, 25), c("#8B0000", "#B8860B", "#006400"))) %>%
    formatStyle("Agro (%)", backgroundColor = styleInterval(c(20, 40), c("#2F4F4F", "#4682B4", "#1E90FF"))) %>%  
    formatStyle("Indústria (%)", backgroundColor = styleInterval(c(15, 30), c("#8B4513", "#CD853F", "#DEB887"))) %>%
    formatStyle("Serviços (%)", backgroundColor = styleInterval(c(30, 50), c("#483D8B", "#6A5ACD", "#9370DB"))) %>%
    formatStyle("Admin (%)", backgroundColor = styleInterval(c(20, 35), c("#8B008B", "#9932CC", "#BA55D3")))  # Dark magenta, dark orchid, medium orchid
})

  ###############################################################################
  # SERVIDOR — ABA D2: ARRANJOS PRODUTIVOS LOCAIS (APLs)
  # Cascata de filtros geográficos, mapa leaflet, tabela APL, correspondência de cursos
  ###############################################################################


observe({
  updatePickerInput(session, "uf_apl", 
                    choices = sort(unique(apl_geo$NM_UF[!is.na(apl_geo$NM_UF)])),
                    selected = "Ceará")
})

# Atualizar Região Intermediária com base na seleção de UF
observeEvent(input$uf_apl, {
  if (is.null(input$uf_apl) || length(input$uf_apl) == 0) {
    updatePickerInput(session, "rgintm_apl", choices = character(0), selected = character(0))
    updatePickerInput(session, "rgimed_apl", choices = character(0), selected = character(0))
    updatePickerInput(session, "mun_apl", choices = character(0), selected = character(0))
  } else {
    uf_filtered <- apl_geo[apl_geo$NM_UF %in% input$uf_apl & !is.na(apl_geo$NM_RGIINTM), ]
    rgi_choices <- sort(unique(uf_filtered$NM_RGIINTM))
    updatePickerInput(session, "rgintm_apl", choices = rgi_choices, selected = rgi_choices)
  }
}, ignoreInit = TRUE)

# Atualizar Região Imediata com base na Região Intermediária
observeEvent(input$rgintm_apl, {
  if (is.null(input$rgintm_apl) || length(input$rgintm_apl) == 0) {
    updatePickerInput(session, "rgimed_apl", choices = character(0), selected = character(0))
    updatePickerInput(session, "mun_apl", choices = character(0), selected = character(0))
  } else {
    rgi_filtered <- apl_geo[apl_geo$NM_RGIINTM %in% input$rgintm_apl & !is.na(apl_geo$NM_RGIMED), ]
    rgimed_choices <- sort(unique(rgi_filtered$NM_RGIMED))
    updatePickerInput(session, "rgimed_apl", choices = rgimed_choices, selected = rgimed_choices)
  }
}, ignoreInit = TRUE)

# Atualizar municípios com base na Região Imediata
observeEvent(input$rgimed_apl, {
  if (is.null(input$rgimed_apl) || length(input$rgimed_apl) == 0) {
    updatePickerInput(session, "mun_apl", choices = character(0), selected = character(0))
  } else {
    rgimed_filtered <- apl_geo[apl_geo$NM_RGIMED %in% input$rgimed_apl & !is.na(apl_geo$NM_MUN), ]
    mun_choices <- sort(unique(rgimed_filtered$NM_MUN))
    updatePickerInput(session, "mun_apl", choices = mun_choices, selected = character(0))
  }
}, ignoreInit = TRUE)

# Filtered APL data reactive
rkt_filtered_apl_data <- reactive({
  data <- apl_geo
  
  # Apply geographic filters
  if (!is.null(input$uf_apl) && length(input$uf_apl) > 0) {
    data <- data[data$NM_UF %in% input$uf_apl, ]
  }
  if (!is.null(input$rgintm_apl) && length(input$rgintm_apl) > 0) {
    data <- data[data$NM_RGIINTM %in% input$rgintm_apl, ]
  }
  if (!is.null(input$rgimed_apl) && length(input$rgimed_apl) > 0) {
    data <- data[data$NM_RGIMED %in% input$rgimed_apl, ]
  }
  if (!is.null(input$mun_apl) && length(input$mun_apl) > 0) {
    data <- data[data$NM_MUN %in% input$mun_apl, ]
  }
  
  # Apply specialization filters
  data <- data[data$LQ >= input$min_lq_apl & data$E_mun_cbo >= input$min_emp_apl, ]
  
  return(data)
})

# Determine current aggregation level
rkt_apl_aggregation_level <- reactive({
  if (!is.null(input$mun_apl) && length(input$mun_apl) > 0) {
    return("municipal")
  } else if (!is.null(input$rgimed_apl) && length(input$rgimed_apl) > 0) {
    return("rgimed") 
  } else if (!is.null(input$rgintm_apl) && length(input$rgintm_apl) > 0) {
    return("rgintm")
  } else if (!is.null(input$uf_apl) && length(input$uf_apl) > 0) {
    return("uf")
  } else {
    return("brasil")
  }
})

# Dynamic aggregated data based on level
# Dados agregados dinâmicos com exibição aprimorada de ocupações
rkt_apl_dynamic_data <- reactive({
  level <- rkt_apl_aggregation_level()
  data <- rkt_filtered_apl_data()
  
  req(nrow(data) > 0)
  
  switch(level,
         "municipal" = {
           # Individual APLs - keep current format
           data %>% select(SG_UF, NM_RGIINTM, NM_RGIMED, NM_MUN, cbo_familia, LQ, E_mun_cbo) %>%
             arrange(desc(LQ))
         },
         "rgimed" = {
           # Agregar por Região Imediata com distribuição de especialização
           data %>% 
             group_by(SG_UF, NM_RGIINTM, NM_RGIMED) %>%
             summarise(
               n_apls = n(),
               n_municipios = n_distinct(CO_MUN6),
               avg_lq = round(mean(LQ, na.rm = TRUE), 2),
               total_emp = sum(E_mun_cbo, na.rm = TRUE),
               especializacao = paste0(
                 sum(LQ >= 5), " muito alta, ", 
                 sum(LQ >= 2.5 & LQ < 5), " alta, ", 
                 sum(LQ >= 1.25 & LQ < 2.5), " moderada"
               ),
               .groups = 'drop'
             ) %>%
             arrange(desc(avg_lq))
         },
         "rgintm" = {
           # Agregar por Região Intermediária com distribuição de especialização  
           data %>%
             group_by(SG_UF, NM_RGIINTM) %>%
             summarise(
               n_apls = n(),
               n_municipios = n_distinct(CO_MUN6),
               avg_lq = round(mean(LQ, na.rm = TRUE), 2), 
               total_emp = sum(E_mun_cbo, na.rm = TRUE),
               especializacao = paste0(
                 sum(LQ >= 5), " muito alta, ", 
                 sum(LQ >= 2.5 & LQ < 5), " alta, ", 
                 sum(LQ >= 1.25 & LQ < 2.5), " moderada"
               ),
               .groups = 'drop'
             ) %>%
             arrange(desc(avg_lq))
         },
         "uf" = {
           # Agregar por UF com distribuição de especialização
           data %>%
             group_by(SG_UF) %>%
             summarise(
               n_apls = n(),
               n_municipios = n_distinct(CO_MUN6),
               avg_lq = round(mean(LQ, na.rm = TRUE), 2),
               total_emp = sum(E_mun_cbo, na.rm = TRUE), 
               especializacao = paste0(
                 sum(LQ >= 5), " muito alta, ", 
                 sum(LQ >= 2.5 & LQ < 5), " alta, ", 
                 sum(LQ >= 1.25 & LQ < 2.5), " moderada"
               ),
               .groups = 'drop'
             ) %>%
             arrange(desc(avg_lq))
         }
  )
})

# Map data reactive
rkt_apl_map_data <- reactive({
  req(nrow(rkt_filtered_apl_data()) > 0)
  
  # Agregar dados APL por município para mapeamento
  # Agregar dados APL por município para mapeamento - convert CO_MUN to character
  apl_summary <- rkt_filtered_apl_data() %>%
    group_by(CO_MUN6, CO_MUN, NM_MUN, SG_UF) %>%
    summarise(
      n_apls = n(),
      avg_lq = mean(LQ, na.rm = TRUE),
      total_emp = sum(E_mun_cbo, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(CO_MUN = as.character(CO_MUN))  # Convert to match sf_regioes
  
  # Filtrar sf_regioes para geografia selecionada
  map_data <- sf_regioes
  if (!is.null(input$uf_apl) && length(input$uf_apl) > 0) {
    map_data <- map_data %>% filter(NM_UF %in% input$uf_apl)
  }
  
  # Juntar com resumo APL
  # Juntar com resumo APL - now use CO_MUN
  map_data <- map_data %>%
    left_join(apl_summary, by = c("CO_MUN", "NM_MUN"))
  # Substituir NA por 0 para municípios sem APLs
  map_data$n_apls[is.na(map_data$n_apls)] <- 0
  
  return(map_data)
})

# Reativo de resumo
output$apl_summary <- renderUI({
  level <- rkt_apl_aggregation_level()
  data <- rkt_filtered_apl_data()
  
  level_names <- c(
    "municipal" = "Municipal",
    "rgimed" = "Região Imediata", 
    "rgintm" = "Região Intermediária",
    "uf" = "UF",
    "brasil" = "Brasil"
  )
  
  HTML(paste0(
    "<strong>Nível de Agregação:</strong> ", level_names[level], "<br>",
    "<strong>APLs encontrados:</strong> ", nrow(data), "<br>",
    "<strong>Municípios:</strong> ", length(unique(data$CO_MUN6)), "<br>",
    "<strong>Ocupações (4-dig):</strong> ", length(unique(data$cbo_4dig))
  ))
})

# Course data based on current aggregation level
rkt_course_data <- reactive({
  req(input$apl_analysis_mode == "apl_course_match")
  req(input$uf_apl)  # Don't process until UF is selected
  
  withProgress(message = 'Loading course data...', value = 0, {
    incProgress(0.3, detail = "Determining aggregation level")
    level <- rkt_apl_aggregation_level()
    
    incProgress(0.3, detail = "Loading appropriate dataset")
    # Usar dataset de cursos apropriado com base no nível de agregação
    if (level %in% c("municipal", "rgimed", "rgintm")) {
      # Para níveis sub-estaduais, agregar a partir de dados municipais de cursos
      course_data <- apl_matri_MUN_geo
    } else {
      # Para nível UF, usar dados de cursos UF
      course_data <- apl_matri_UF
    }
    
    incProgress(0.3, detail = "Applying geographic filters")
    # Converter nomes completos de UF para códigos para filtragem (CORRIGIR INCOMPATIBILIDADE)
    if (!is.null(input$uf_apl) && length(input$uf_apl) > 0) {
      uf_lookup <- unique(apl_geo[, c("NM_UF", "SG_UF")])
      selected_codes <- uf_lookup[uf_lookup$NM_UF %in% input$uf_apl, "SG_UF"]
      course_data <- course_data[course_data$SG_UF %in% selected_codes, ]
    }
    
    incProgress(0.1, detail = "Complete")
    return(course_data)
  })
})

# Map output
output$apl_map <- renderLeaflet({
  map_data <- rkt_apl_map_data()
  
  # Criar paleta de cores
  pal <- colorNumeric(
    palette = "viridis",
    domain = map_data$n_apls,
    na.color = "transparent"
  )
  
  leaflet(map_data) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(n_apls),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste0(NM_MUN, ": ", n_apls, " APLs"),
      popup = ~paste0(
        "<strong>", NM_MUN, "</strong><br/>",
        "UF: ", NM_UF, "<br/>",
        "APLs: ", n_apls, "<br/>",
        "QL Médio: ", round(avg_lq, 2), "<br/>",
        "Emprego Total: ", formatC(total_emp, format="d", big.mark=".")
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~n_apls,
      opacity = 0.7,
      title = "Número de APLs",
      position = "bottomright"
    )
})



# Table output
# Tabela aprimorada com nomes de colunas atualizados
output$apl_table <- renderDT({
  if (input$apl_analysis_mode == "apl_only") {
    # APL Mode
    level <- rkt_apl_aggregation_level()
    table_data <- rkt_apl_dynamic_data()
    req(nrow(table_data) > 0)
    
    # Nomes e formatação de colunas dinâmicos com base no nível de agregação
    if (level == "municipal") {
      table_data$LQ <- round(table_data$LQ, 2)
      table_data$E_mun_cbo <- formatC(table_data$E_mun_cbo, format = "d", big.mark = ".")
      colnames(table_data) <- c("UF", "Região Intermediária", "Região Imediata", "Município", "Ocupação (Família CBO)", "QL", "Emprego")
    } else {
      table_data$total_emp <- formatC(table_data$total_emp, format = "d", big.mark = ".")
      if (level == "rgimed") {
        colnames(table_data) <- c("UF", "Região Intermediária", "Região Imediata", "APLs", "Municípios", "QL Médio", "Emprego Total", "Especialização")
      } else if (level == "rgintm") {
        colnames(table_data) <- c("UF", "Região Intermediária", "APLs", "Municípios", "QL Médio", "Emprego Total", "Especialização")
      } else if (level == "uf") {
        colnames(table_data) <- c("UF", "APLs", "Municípios", "QL Médio", "Emprego Total", "Especialização")
      }
    }
    level_for_export <- level  # Store for export filename
    
  } else {
    # Modo de Correspondência de Cursos - mostrar alinhamento APL vs Curso
    course_data <- rkt_course_data()
    apl_data <- rkt_filtered_apl_data()
    
    req(nrow(course_data) > 0)
    req(nrow(apl_data) > 0)
    
    # Criar análise de correspondência
    matching_data <- merge(
      apl_data[, .(cbo_4dig, cbo_familia, LQ, E_mun_cbo)],
      course_data[, .(cbo_4dig, nome_curso_clean, QT_MAT_CURSO_TEC_TOT)],
      by = "cbo_4dig",
      all.x = TRUE  # Keep all APLs, show which have courses
    )
    
    # Criar indicador de correspondência
    matching_data$tem_curso <- ifelse(is.na(matching_data$QT_MAT_CURSO_TEC_TOT), "Sem curso", "Com curso")
    matching_data$QT_MAT_CURSO_TEC_TOT[is.na(matching_data$QT_MAT_CURSO_TEC_TOT)] <- 0
    
    table_data <- matching_data %>%
      select(cbo_familia, LQ, E_mun_cbo, nome_curso_clean, QT_MAT_CURSO_TEC_TOT, tem_curso) %>%
      arrange(desc(QT_MAT_CURSO_TEC_TOT)) %>%
      head(30)
    
    colnames(table_data) <- c("Ocupação APL", "QL", "Emprego APL", "Curso Técnico", "Matrículas", "Status Match")
    level_for_export <- "matching"
  }
  
  datatable(
    table_data,
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(extend = "copy", text = "Copiar"),
        list(extend = "csv", filename = paste0("APL_", toupper(level_for_export)), text = "CSV"),
        list(extend = "excel", filename = paste0("APL_", toupper(level_for_export)), text = "Excel")
      )
    ),
    class = "stripe nowrap display"
  )
})



  ###############################################################################
  # SERVIDOR — ABA D3: ANÁLISE DE INFORMALIDADE
  # Filtros em cascata da hierarquia CBO, mapa por decil, tabela detalhada de emprego
  ###############################################################################


# Popular opções iniciais de Grande Grupo CBO
observe({
  cbo_gragru_choices <- sort(unique(qbq_ocup_cmento1$cbo_gragru))
  updatePickerInput(session, "cbo_grande_grupo", 
                    choices = cbo_gragru_choices,
                    selected = cbo_gragru_choices[3]) # Default: all selected
})

# Populate initial Nivel Ocupacao choices
observe({
  nivel_choices <- sort(unique(qbq_ocup_cmento1$NivelOcupacao))
  updatePickerInput(session, "nivel_ocupacao", 
                    choices = nivel_choices,
                    selected = nivel_choices) # Default: all selected
})

# --- Geographic Hierarchy Updates ---
# Atualizar opções de Região Intermediária com base na seleção de UF
observeEvent(input$informality_uf, {
  current_uf_selection <- input$informality_uf
  if (is.null(current_uf_selection) || length(current_uf_selection) == 0) {
    updatePickerInput(session, "informality_reg_inter", 
                      choices = character(0), selected = character(0))
  } else {
    choices <- dft_informality_geo_codes %>%
      filter(NM_UF %in% current_uf_selection) %>%
      pull(NM_RGIMED) %>%
      unique() %>%
      sort()
    updatePickerInput(session, "informality_reg_inter", 
                      choices = choices, selected = choices)
  }
})

# Atualizar opções de Região Imediata com base na seleção de Região Intermediária
observeEvent(input$informality_reg_inter, {
  current_inter_selection <- input$informality_reg_inter
  if (is.null(current_inter_selection) || length(current_inter_selection) == 0) {
    updatePickerInput(session, "informality_reg_imed", 
                      choices = character(0), selected = character(0))
  } else {
    choices <- dft_informality_geo_codes %>%
      filter(NM_RGIMED %in% current_inter_selection) %>%
      pull(NM_RGIINTM) %>%
      unique() %>%
      sort()
    updatePickerInput(session, "informality_reg_imed", 
                      choices = choices, selected = choices)
  }
})

# Atualizar opções de Município com base na seleção de Região Imediata
observeEvent(input$informality_reg_imed, {
  current_imed_selection <- input$informality_reg_imed
  if (is.null(current_imed_selection) || length(current_imed_selection) == 0) {
    updatePickerInput(session, "informality_municipio", 
                      choices = character(0), selected = character(0))
  } else {
    choices <- dft_informality_geo_codes %>%
      filter(NM_RGIINTM %in% current_imed_selection) %>%
      pull(NM_MUN) %>%
      unique() %>%
      sort()
    updatePickerInput(session, "informality_municipio", 
                      choices = choices, selected = choices)
  }
})

# --- CBO Hierarchy Updates ---
# Atualizar Grupo Primário com base na seleção de Grande Grupo
observeEvent(input$cbo_grande_grupo, {
  current_gragru_selection <- input$cbo_grande_grupo
  if (is.null(current_gragru_selection) || length(current_gragru_selection) == 0) {
    updatePickerInput(session, "cbo_grupo_primario", 
                      choices = character(0), selected = character(0))
  } else {
    choices <- qbq_ocup_cmento1 %>%
      filter(cbo_gragru %in% current_gragru_selection) %>%
      pull(cbo_prigru) %>%
      unique() %>%
      sort()
    updatePickerInput(session, "cbo_grupo_primario", 
                      choices = choices, selected = choices)
  }
})

# Atualizar Subgrupo com base na seleção de Grupo Primário
observeEvent(input$cbo_grupo_primario, {
  current_prigru_selection <- input$cbo_grupo_primario
  if (is.null(current_prigru_selection) || length(current_prigru_selection) == 0) {
    updatePickerInput(session, "cbo_subgrupo", 
                      choices = character(0), selected = character(0))
  } else {
    choices <- qbq_ocup_cmento1 %>%
      filter(cbo_prigru %in% current_prigru_selection) %>%
      pull(cbo_subgru) %>%
      unique() %>%
      sort()
    updatePickerInput(session, "cbo_subgrupo", 
                      choices = choices, selected = choices)
  }
})

# Atualizar Família com base na seleção de Subgrupo
observeEvent(input$cbo_subgrupo, {
  current_subgru_selection <- input$cbo_subgrupo
  if (is.null(current_subgru_selection) || length(current_subgru_selection) == 0) {
    updatePickerInput(session, "cbo_familia", 
                      choices = character(0), selected = character(0))
  } else {
    choices <- qbq_ocup_cmento1 %>%
      filter(cbo_subgru %in% current_subgru_selection) %>%
      pull(cbo_familia) %>%
      unique() %>%
      sort()
    updatePickerInput(session, "cbo_familia", 
                      choices = choices, selected = choices)
  }
})

# --- Main Reactive Data ---
# Obter dados do ano selecionado
rkt_year_data <- reactive({
  if (input$informality_year == 2023) {
    return(df_cbocod_mun23)
  } else {
    return(df_cbocod_mun24)
  }
})

# Filtrar dados de emprego por todas as seleções
rkt_filtered_employment <- reactive({
  year_data <- rkt_year_data()
  
  # Obter códigos CBO para nível hierárquico selecionado
  selected_cbos <- NULL
  
  if (!is.null(input$cbo_familia) && length(input$cbo_familia) > 0) {
    # Nível mais específico - usar seleções de família
    selected_cbos <- qbq_ocup_cmento1 %>%
      filter(cbo_familia %in% input$cbo_familia) %>%
      pull(cbo_4dig)
  } else if (!is.null(input$cbo_subgrupo) && length(input$cbo_subgrupo) > 0) {
    # Subgrupo level
    selected_cbos <- qbq_ocup_cmento1 %>%
      filter(cbo_subgru %in% input$cbo_subgrupo) %>%
      pull(cbo_4dig)
  } else if (!is.null(input$cbo_grupo_primario) && length(input$cbo_grupo_primario) > 0) {
    # Grupo primário level
    selected_cbos <- qbq_ocup_cmento1 %>%
      filter(cbo_prigru %in% input$cbo_grupo_primario) %>%
      pull(cbo_4dig)
  } else if (!is.null(input$cbo_grande_grupo) && length(input$cbo_grande_grupo) > 0) {
    # Grande grupo level
    selected_cbos <- qbq_ocup_cmento1 %>%
      filter(cbo_gragru %in% input$cbo_grande_grupo) %>%
      pull(cbo_4dig)
  }
  
  # Filtrar por CBO se selecionado
  if (!is.null(selected_cbos)) {
    year_data <- year_data %>%
      filter(cbo_4dig %in% selected_cbos)
  }
  
  # Filtrar por Nível Ocupação se selecionado
  if (!is.null(input$nivel_ocupacao) && length(input$nivel_ocupacao) > 0) {
    nivel_cbos <- qbq_ocup_cmento1 %>%
      filter(NivelOcupacao %in% input$nivel_ocupacao) %>%
      pull(cbo_4dig)
    year_data <- year_data %>%
      filter(cbo_4dig %in% nivel_cbos)
  }
  
  # Agregar por município - apenas as duas colunas necessárias
  aggregated_data <- year_data %>%
    group_by(CO_MUN6, NM_MUN, SG_UF, NM_UF) %>%
    summarise(
      vinculos_formais = sum(vinculos_formais, na.rm = TRUE),
      vinculos_total = sum(vinculos_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Taxa_Formalidade = ifelse(vinculos_total > 0, 
                                vinculos_formais / vinculos_total, 
                                NA_real_)
    )
  
  return(aggregated_data)
})

# Calculate deciles within UF
rkt_muni_deciles <- reactive({
  employment_data <- rkt_filtered_employment()
  
  # Determinar métrica de emprego para cálculo de decil
  employment_metric <- if (input$map_employment_type == "formal") {
    "vinculos_formais"
  } else {
    "vinculos_total"
  }
  
  # Calcular decis dentro de cada UF
  decile_data <- employment_data %>%
    filter(!!sym(employment_metric) > 0) %>%  # Exclude zero employment
    group_by(NM_UF) %>%
    mutate(
      decile = ntile(!!sym(employment_metric), 10)
    ) %>%
    ungroup()
  
  return(decile_data)
})

# Filtragem geográfica para exibição no mapa
rkt_map_data <- reactive({
  decile_data <- rkt_muni_deciles()
  
  # Apply geographic filters
  if (!is.null(input$informality_uf) && length(input$informality_uf) > 0) {
    decile_data <- decile_data %>%
      filter(NM_UF %in% input$informality_uf)
  }
  
  # Adicionar filtragem geográfica via geocódigos
  if (!is.null(input$informality_reg_inter) && length(input$informality_reg_inter) > 0) {
    mun_codes <- dft_informality_geo_codes %>%
      filter(NM_RGIMED %in% input$informality_reg_inter) %>%
      pull(CO_MUN6)
    decile_data <- decile_data %>%
      filter(CO_MUN6 %in% mun_codes)
  }
  
  if (!is.null(input$informality_reg_imed) && length(input$informality_reg_imed) > 0) {
    mun_codes <- dft_informality_geo_codes %>%
      filter(NM_RGIINTM %in% input$informality_reg_imed) %>%
      pull(CO_MUN6)
    decile_data <- decile_data %>%
      filter(CO_MUN6 %in% mun_codes)
  }
  
  return(decile_data)
})

# DT filtered data (includes municipality filter)
rkt_filtered_dt <- reactive({
  map_data <- rkt_map_data()
  
  # Aplicar filtro de município apenas para DT
  if (!is.null(input$informality_municipio) && length(input$informality_municipio) > 0) {
    map_data <- map_data %>%
      filter(NM_MUN %in% input$informality_municipio)
  }
  
  return(map_data)
})



# Adicionado temporariamente para debug

# --- Outputs ---
# Map rendering
output$informality_map <- renderLeaflet({
  map_data <- rkt_map_data()
  
  if (nrow(map_data) == 0) {
    leaflet() %>% addTiles() %>% setView(lng = -54, lat = -15, zoom = 4)
  } else {
    # Obter UFs selecionadas para filtragem antecipada
    selected_ufs <- unique(map_data$NM_UF)
    
    # CORRETO: Manter junção-ponte mas filtrar sf_regioes antecipadamente
    map_with_geom <- sf_regioes %>%
      filter(NM_UF %in% selected_ufs) %>%  # Filter spatial data early (performance)
      left_join(
        dft_geo_keys %>% mutate(CO_MUN = as.character(CO_MUN)), 
        by = "CO_MUN"  # sf_regioes has CO_MUN
      ) %>%
      left_join(map_data, by = "CO_MUN6") %>%  # dft_geo_keys provides CO_MUN6
      filter(!is.na(decile))
    
    if (nrow(map_with_geom) == 0) {
      leaflet() %>% addTiles() %>% setView(lng = -54, lat = -15, zoom = 4)
    } else {
      color_palette <- colorFactor(
        palette = c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598",
                    "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142"),
        domain = 1:10, na.color = "#CCCCCC"
      )
      
      leaflet(map_with_geom) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~color_palette(decile),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          popup = ~paste0("<strong>", NM_MUN, "</strong><br>",
                          "Emprego Total: ", scales::comma(vinculos_total), "<br>",
                          "Decil: ", decile)
        ) %>%
        addLegend("bottomright", pal = color_palette, values = ~decile,
                  title = "Decil de Emprego", opacity = 1)
      # Deixar leaflet ajustar limites automaticamente para performance
    }
  }
})


# Data table rendering (FIXED - arrange before select)
output$informality_table <- renderDT({
  dt_data <- rkt_filtered_dt()
  req(nrow(dt_data) > 0)
  
  # Correção: ordenar ANTES de selecionar, ou usar colunas renomeadas
  table_data <- dt_data %>%
    arrange(desc(vinculos_total)) %>%  # Sort BEFORE renaming columns
    mutate(
      UF = NM_UF,
      Município = NM_MUN,
      `Vínculos Formais` = formatC(round(vinculos_formais), format="d", big.mark="."),
      `Vínculos Totais` = formatC(round(vinculos_total), format="d", big.mark="."), 
      `Taxa Formalidade (%)` = round(Taxa_Formalidade * 100, 1),
      Decil = decile
    ) %>%
    select(UF, Município, `Vínculos Formais`, `Vínculos Totais`, 
           `Taxa Formalidade (%)`, Decil)
  
  datatable(table_data, rownames = FALSE, extensions = 'Buttons',
            options = list(
              pageLength = 15, 
              scrollX = TRUE, 
              dom = 'Bfrtip',
              buttons = list(
                list(extend = "copy", text = "Copiar"),
                list(extend = "csv", filename = "EPT_Informalidade", text = "CSV"),
                list(extend = "excel", filename = "EPT_Informalidade", text = "Excel")
              ),
              columnDefs = list(
                list(width = '40px', targets = 0),   # UF
                list(width = '120px', targets = 1),  # Município  
                list(width = '80px', targets = c(2, 3)),  # Employment numbers
                list(width = '70px', targets = 4),   # Formality rate
                list(width = '50px', targets = 5),   # Decil
                list(className = 'dt-center', targets = c(2, 3, 4, 5))
              )
            ), 
            class = "stripe nowrap display compact"
  ) %>%
    formatStyle("Taxa Formalidade (%)", 
                backgroundColor = styleInterval(c(40, 70), c("#8B0000", "#B8860B", "#006400"))) %>%
    formatStyle("Decil", 
                backgroundColor = styleInterval(1:9, 
                                                c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#E6F598",
                                                  "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F", "#9E0142")),
                color = "white", fontWeight = "bold")
})

# Summary output
output$informality_summary <- renderText({
  map_data <- rkt_map_data()
  
  if (nrow(map_data) == 0) {
    return("Nenhum dado selecionado")
  }
  
  total_munis <- nrow(map_data)
  total_employment <- sum(map_data$vinculos_total, na.rm = TRUE)
  avg_formality <- mean(map_data$Taxa_Formalidade, na.rm = TRUE) * 100
  
  paste0("Municípios: ", total_munis, "\n",
         "Emprego Total: ", scales::comma(total_employment), "\n",
         "Taxa Formalidade Média: ", round(avg_formality, 1), "%")
})

  ###############################################################################
  # SERVIDOR — ABA E1: CORRESPONDÊNCIA OFERTA-DEMANDA (CNCT ↔ CBO)
  # Tabelas hierárquicas de matrículas/emprego, correspondência curso-ocupação com IA
  ###############################################################################
# Inicializar opções de UF
observe({
  all_ufs <- sort(unique(df_mat_eixo_wide$NM_UF))
  updateSelectizeInput(
    session, "uf_selectCOCN",
    choices = c("Brasil", setdiff(all_ufs, "Brasil")),
    selected = "Brasil"
  )
})



#### OFERTA → DEMANDA ####

# Table 1a - Matrículas por Eixo
RKT_agg_df <- reactive({
  req(input$match_direction == "Oferta \u2192 Demanda")
  req(input$uf_selectCOCN)
  
  df_mat_eixo_wide %>%
    filter(NM_UF == input$uf_selectCOCN) %>%
    select(
      NM_UF,
      `Eixo Tecnológico`,
      `Matrículas 2023`,
      `Matrículas 2024`
    ) %>%
    arrange(desc(`Matrículas 2024`))
})

RKT_selected_eixo <- reactive({
  req(input$agg_table_rows_selected)
  df <- RKT_agg_df()
  df$`Eixo Tecnológico`[input$agg_table_rows_selected]
})



DTOutput("agg_table")

output$agg_table <- renderDT({
  datatable(
    RKT_agg_df(),
    selection = "single",
    rownames  = FALSE,
    class = "compact stripe dark-dt",  
    options   = list(
      pageLength = nrow(RKT_agg_df()),
      autoWidth  = TRUE,
      language   = list(
        url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
      )
    )
  ) %>%
    formatCurrency(
      c("Matrículas 2023","Matrículas 2024"),
      currency = "", interval = 3, mark = ".", dec.mark = ","
    ) %>%
    formatStyle(
      c("Matrículas 2023","Matrículas 2024"),
      `text-align` = "right"
    )
})

# Reset selections when eixo changes
observeEvent(input$agg_table_rows_selected, {
  DT::dataTableProxy("curso_table") %>% 
    DT::selectRows(NULL)
}, ignoreNULL = FALSE)

# Table 1b - Matrículas por Área
RKT_area_df <- reactive({
  req(input$match_direction == "Oferta → Demanda", input$agg_table_rows_selected)
  sel_eixo <- RKT_selected_eixo()
  df_exarcu %>%
    filter(`Eixo Tecnológico` == sel_eixo) %>%
    distinct(`Área Tecnológica`) %>%
    left_join(
      df_mat_area_wide %>% filter(NM_UF == input$uf_selectCOCN),
      by = "Área Tecnológica"
    ) %>%
    select(
      `Área Tecnológica`,
      `Matrículas 2023`,
      `Matrículas 2024`
    ) %>%
    arrange(desc(`Matrículas 2024`))
})

output$area_table <- renderDT({
  datatable(
    RKT_area_df(),
    selection = "single",
    rownames  = FALSE,
    class = "compact stripe dark-dt",  
    options   = list(
      pageLength = nrow(RKT_area_df()),
      autoWidth  = TRUE,
      language   = list(
        url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
      )
    )
  ) %>%
    formatCurrency(
      c("Matrículas 2023","Matrículas 2024"),
      currency = "", interval = 3, mark = ".", dec.mark = ","
    ) %>%
    formatStyle(
      c("Matrículas 2023","Matrículas 2024"),
      `text-align` = "right"
    )
})

observeEvent(input$area_table_rows_selected, {
  DT::dataTableProxy("curso_table") %>% 
    DT::selectRows(NULL)
}, ignoreNULL = FALSE)

# Table 1c - Matrículas por Curso
RKT_selected_area <- reactive({
  req(input$area_table_rows_selected)
  RKT_area_df()$`Área Tecnológica`[input$area_table_rows_selected]
})

RKT_curso_df <- reactive({
  req(input$area_table_rows_selected)
  uf <- input$uf_selectCOCN
  eixo <- RKT_selected_eixo()
  area <- RKT_selected_area()
  df_exarcu %>%
    filter(
      `Eixo Tecnológico` == eixo,
      `Área Tecnológica` == area
    ) %>%
    distinct(IDX_EIXCUR, `Denominação do Curso`) %>%
    left_join(
      df_mat_curso_wide %>% filter(NM_UF == uf),
      by = c("IDX_EIXCUR", "Denominação do Curso")
    ) %>%
    select(
      IDX_EIXCUR,
      `Denominação do Curso`,
      `Matrículas 2023`,
      `Matrículas 2024`
    ) %>%
    arrange(desc(`Matrículas 2024`))
})

output$curso_table <- renderDT({
  datatable(
    RKT_curso_df(),
    selection = "single",
    rownames  = FALSE,
    class = "compact stripe dark-dt",  
    options   = list(
      pageLength = nrow(RKT_curso_df()),
      autoWidth  = TRUE,
      language   = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json")
    )
  ) %>%
    formatCurrency(
      c("Matrículas 2023","Matrículas 2024"),
      currency = "", interval = 3, mark = ".", dec.mark = ","
    ) %>%
    formatStyle(
      c("Matrículas 2023","Matrículas 2024"),
      `text-align` = "right"
    )
})

RKT_selected_course <- reactive({
  req(input$curso_table_rows_selected)
  df <- RKT_curso_df()
  df$IDX_EIXCUR[input$curso_table_rows_selected]
})

# Auto-atualizar top_n com base nas correspondências disponíveis dentro do limiar
observeEvent(list(RKT_selected_course(), input$score_thresh), {
  req(input$match_direction == "Oferta \u2192 Demanda")
  req(input$curso_table_rows_selected)
  req(input$score_thresh)
  
  selected_course_id <- RKT_selected_course()
  
  # Contar correspondências acima do limiar atual
  n_matches <- cnct_qbq_matches2 %>%
    filter(
      IDX_EIXCUR == selected_course_id,
      final_score >= input$score_thresh
    ) %>%
    nrow()
  
  # Definir top_n para o número de correspondências disponíveis
  new_top_n <- min(n_matches, 50)
  new_max <- max(n_matches, 50)
  
  updateNumericInput(
    session,
    "top_n",
    value = new_top_n,
    min = 1,
    max = new_max
  )
})

# Table 2a - Course to Occupation matches
RKT_course_occ_df <- reactive({
  req(input$match_direction == "Oferta \u2192 Demanda")
  req(input$curso_table_rows_selected)
  req(input$score_thresh, input$top_n)
  
  selected_course_id <- RKT_selected_course()
  
  # Filtrar por limiar de pontuação e aplicar limite top_n
  matches_final <- cnct_qbq_matches2 %>%
    filter(
      IDX_EIXCUR == selected_course_id,
      final_score >= input$score_thresh
    ) %>%
    arrange(desc(final_score)) %>%
    slice_head(n = input$top_n)
  
  if (nrow(matches_final) == 0) {
    return(data.frame(
      CodCBO = character(0),
      Ocupação = character(0),
      `Vínculos 2023` = integer(0),
      `Vínculos 2024` = integer(0),
      final_score = numeric(0),
      semantic = numeric(0),
      tfidf = numeric(0),
      check.names = FALSE
    ))
  }
  
  # Bring in RAIS info
  rais_filtered <- df_raisCodCBO_wide %>%
    filter(NM_UF == input$uf_selectCOCN) %>%
    select(CodCBO, `Ocupação`, `Vínculos 2023`, `Vínculos 2024`)
  
  # Mesclar e retornar
  result <- left_join(matches_final, rais_filtered, by = "CodCBO") %>%
    select(CodCBO, `Ocupação`, `Vínculos 2023`, `Vínculos 2024`, final_score, semantic, tfidf) %>%
    arrange(desc(final_score))
  
  return(result)
})

output$course_occ_table <- renderDT({
  df <- RKT_course_occ_df()
  
  validate(need(nrow(df) > 0, "Nenhuma correspondência encontrada."))
  
  # Formatar colunas de pontuação para melhor exibição
  df_display <- df %>%
    mutate(
      final_score = round(final_score, 3),
      semantic = round(semantic, 3),
      tfidf = round(tfidf, 3)
    )
  
  datatable(
    df_display,
    rownames = FALSE,
    selection = "single",
    class = "compact stripe dark-dt",  
    options = list(
      pageLength = nrow(df_display),
      autoWidth = TRUE,
      language = list(
        url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
      )
    )
  ) %>%
    formatCurrency(
      c("Vínculos 2023", "Vínculos 2024"),
      currency = "", interval = 3, mark = ".", dec.mark = ","
    ) %>%
    formatStyle(
      c("Vínculos 2023", "Vínculos 2024"),
      `text-align` = "right"
    ) %>%
    formatStyle(
      c("final_score", "semantic", "tfidf"),
      `text-align` = "center"
    )
})

#### DEMANDA → OFERTA ####

# Table 1a - Vínculos por Grande Grupo (1-digit)
RKT_cbo1_df <- reactive({
  req(input$match_direction == "Demanda \u2192 Oferta", input$uf_selectCOCN)
  df <- df_rais1dig_wide %>%
    dplyr::filter(NM_UF == input$uf_selectCOCN) %>%
    dplyr::select(
      cbo_1dig,
      `Grande Grupo` = cbo_gragru,
      `Vínculos 2023`,
      `Vínculos 2024`
    )
  
  # Ensure numeric columns
  vcols <- c("Vínculos 2023", "Vínculos 2024")
  df[vcols] <- lapply(df[vcols], function(x) suppressWarnings(as.numeric(x)))
  
  df %>% dplyr::arrange(dplyr::desc(`Vínculos 2024`))
})

output$cbo1_table <- DT::renderDT({
  DT::datatable(
    RKT_cbo1_df(),
    selection = "single",
    rownames  = FALSE,
    class = "compact stripe dark-dt",  
    options   = list(
      pageLength = 10,
      autoWidth  = TRUE,
      columnDefs = list(list(visible = FALSE, targets = 0))
    )
  ) %>%
    DT::formatCurrency(
      columns  = c("Vínculos 2023","Vínculos 2024"),
      currency = "",
      digits   = 0,
      interval = 3,
      mark     = ".",
      dec.mark = ","
    ) %>%
    DT::formatStyle(
      c("Vínculos 2023","Vínculos 2024"),
      `text-align` = "right"
    )
})

# Clear lower level selections when Grande Grupo changes
observeEvent(input$cbo1_table_rows_selected, {
  DT::dataTableProxy("cbo4_table")  %>% DT::selectRows(NULL)
  DT::dataTableProxy("cbo6b_table") %>% DT::selectRows(NULL)
}, ignoreInit = TRUE)

RKT_selected_cbo1_code <- reactive({
  req(input$match_direction == "Demanda \u2192 Oferta")
  idx <- input$cbo1_table_rows_selected
  req(length(idx) == 1)
  as.character(RKT_cbo1_df()$cbo_1dig[idx])
})

# Table 1b - Família (CBO 4 dígitos)
RKT_cbo4_df <- reactive({
  req(input$match_direction == "Demanda \u2192 Oferta",
      input$uf_selectCOCN, input$cbo1_table_rows_selected)
  sel1 <- RKT_selected_cbo1_code()
  df <- df_rais4dig_wide %>%
    dplyr::filter(
      NM_UF == input$uf_selectCOCN,
      substr(cbo_4dig, 1, 1) == sel1
    ) %>%
    dplyr::select(
      cbo_4dig,
      `Família (CBO 4d)` = cbo_familia,
      `Vínculos 2023`,
      `Vínculos 2024`
    )
  
  vcols <- c("Vínculos 2023", "Vínculos 2024")
  df[vcols] <- lapply(df[vcols], function(x) suppressWarnings(as.numeric(x)))
  
  df %>% dplyr::arrange(dplyr::desc(`Vínculos 2024`))
})

output$cbo4_table <- DT::renderDT({
  DT::datatable(
    RKT_cbo4_df(),
    selection = "single",
    rownames  = FALSE,
    class = "compact stripe dark-dt",  
    options   = list(
      pageLength = 10,
      autoWidth  = TRUE,
      columnDefs = list(list(visible = FALSE, targets = 0))
    )
  ) %>%
    DT::formatCurrency(
      columns  = c("Vínculos 2023","Vínculos 2024"),
      currency = "",
      digits   = 0,
      interval = 3,
      mark     = ".",
      dec.mark = ","
    ) %>%
    DT::formatStyle(
      c("Vínculos 2023","Vínculos 2024"),
      `text-align` = "right"
    )
})

RKT_selected_cbo4_code <- reactive({
  req(input$match_direction == "Demanda \u2192 Oferta")
  idx <- input$cbo4_table_rows_selected
  req(length(idx) == 1)
  as.character(RKT_cbo4_df()$cbo_4dig[idx])
})

# Table 1c - Ocupações (CBO 6 dígitos)
RKT_cbo6_df <- reactive({
  req(input$match_direction == "Demanda \u2192 Oferta",
      input$uf_selectCOCN,
      input$cbo4_table_rows_selected)
  
  sel4 <- RKT_selected_cbo4_code()
  
  df <- df_raisCodCBO_wide %>%
    dplyr::filter(
      NM_UF == input$uf_selectCOCN,
      substr(CodCBO, 1, 4) == sel4
    ) %>%
    dplyr::select(
      CodCBO,
      Ocupação = `Ocupação`,
      `Vínculos 2023`,
      `Vínculos 2024`
    )
  
  vcols <- c("Vínculos 2023", "Vínculos 2024")
  df[vcols] <- lapply(df[vcols], function(x) suppressWarnings(as.numeric(x)))
  
  df %>% dplyr::arrange(dplyr::desc(`Vínculos 2024`))
})

output$cbo6b_table <- DT::renderDT({
  df <- RKT_cbo6_df()
  
  DT::datatable(
    df %>% dplyr::select(CodCBO, Ocupação, `Vínculos 2023`, `Vínculos 2024`),
    selection = "single",
    rownames  = FALSE,
    class = "compact stripe dark-dt",  
    options   = list(
      pageLength = 10,
      autoWidth  = TRUE,
      columnDefs = list(list(visible = FALSE, targets = 0)),
      language   = list(
        url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
      )
    )
  ) %>%
    DT::formatCurrency(
      columns  = c("Vínculos 2023", "Vínculos 2024"),
      currency = "",
      digits   = 0,
      interval = 3,
      mark     = ".",
      dec.mark = ","
    ) %>%
    DT::formatStyle(
      c("Vínculos 2023","Vínculos 2024"),
      `text-align` = "right"
    )
})

# Table 2a - Occupation to Course matches
RKT_selected_cbo_code <- reactive({
  req(input$cbo6b_table_rows_selected)
  RKT_cbo6_df()$CodCBO[input$cbo6b_table_rows_selected]
})

RKT_course_matches_df <- reactive({
  req(input$match_direction == "Demanda → Oferta")
  req(input$cbo6b_table_rows_selected)
  req(input$score_thresh)
  req(input$top_n)
  
  cbo_code <- RKT_selected_cbo_code()
  
  # Get matches
  matches <- qbq_cnct_matches2 %>%
    filter(
      CodCBO == cbo_code,
      final_score >= input$score_thresh
    ) %>%
    arrange(desc(final_score)) %>%
    slice_head(n = input$top_n)
  
  if (nrow(matches) == 0) {
    return(data.frame(
      `Denominação do Curso` = character(0),
      `Matrículas 2023` = integer(0),
      `Matrículas 2024` = integer(0),
      final_score = numeric(0),
      check.names = FALSE
    ))
  }
  
  # Obter dados de matrícula para a UF selecionada
  enrollments <- df_mat_curso_wide %>%
    filter(NM_UF == input$uf_selectCOCN) %>%
    select(IDX_EIXCUR, `Matrículas 2023`, `Matrículas 2024`)
  
  # Juntar e combinar
  result <- matches %>%
    left_join(enrollments, by = "IDX_EIXCUR") %>%
    select(
      `Denominação do Curso`,
      `Matrículas 2023`, 
      `Matrículas 2024`, 
      final_score
    ) %>%
    mutate(
      `Matrículas 2023` = ifelse(is.na(`Matrículas 2023`), 0, `Matrículas 2023`),
      `Matrículas 2024` = ifelse(is.na(`Matrículas 2024`), 0, `Matrículas 2024`)
    ) %>%
    arrange(desc(final_score))
  
  return(result)
})

# Renderizar tabela de correspondências ocupação-curso
output$course_agg_table_rev <- renderDT({
  df <- RKT_course_matches_df()
  
  if (nrow(df) == 0) {
    return(datatable(
      data.frame(`Denominação do Curso` = "Nenhuma correspondência encontrada",
                 `Matrículas 2023` = 0,
                 `Matrículas 2024` = 0,
                 final_score = 0,
                 check.names = FALSE),
      options = list(dom = 't'),
      rownames = FALSE
    ))
  }
  
  # Formatar pontuações para exibição
  df_display <- df %>%
    mutate(final_score = round(final_score, 3))
  
  datatable(
    df_display,
    selection = "single",
    rownames = FALSE,
    class = "compact stripe dark-dt",  
    options = list(
      pageLength = nrow(df_display),
      autoWidth = TRUE,
      language = list(
        url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
      )
    )
  ) %>%
    formatCurrency(
      c("Matrículas 2023", "Matrículas 2024"),
      currency = "", interval = 3, mark = ".", dec.mark = ","
    ) %>%
    formatStyle(
      c("Matrículas 2023", "Matrículas 2024"),
      `text-align` = "right"
    ) %>%
    formatStyle(
      "final_score",
      `text-align` = "center"
    )
})

# Auto-atualizar top_n com base nas correspondências disponíveis para ocupação-curso
observeEvent(list(RKT_selected_cbo_code(), input$score_thresh), {
  req(input$match_direction == "Demanda → Oferta")
  req(input$cbo6b_table_rows_selected)
  
  cbo_code <- RKT_selected_cbo_code()
  
  # Count available matches
  n_matches <- qbq_cnct_matches2 %>%
    filter(CodCBO == cbo_code, final_score >= input$score_thresh) %>%
    nrow()
  
  # Atualizar input top_n
  if (n_matches > 0) {
    updateNumericInput(session, "top_n", 
                       value = min(n_matches, 10), 
                       max = min(n_matches, 50))
  }
}, ignoreInit = TRUE)


  ###############################################################################
  # SERVIDOR — ABA E2: ESCASSEZ DE PROFISSIONAIS TÉCNICOS
  # Indicadores de escassez CAGED/RAIS, tabela de ranking, classificação tipológica
  ###############################################################################
observe({
  eixos_choices <- sort(unique(caged_rais_curso$Eixo_Tecnologico))
  
  # Encontrar qual Eixo contém Enfermagem
  eixo_with_enfermagem <- caged_rais_curso %>%
    filter(Curso == "Enfermagem") %>%
    pull(Eixo_Tecnologico) %>%
    unique() %>%
    first()
  
  # Definir esse Eixo como padrão
  updateSelectizeInput(session, "eixos",
                       choices = eixos_choices, 
                       selected = eixo_with_enfermagem,
                       server = TRUE)
})

# Atualizar lista de cursos quando seleção de eixo muda
observeEvent(input$eixos, {
  cursos <- caged_rais_curso %>%
    filter(Eixo_Tecnologico %in% input$eixos) %>%
    distinct(Curso) %>%
    arrange(Curso) %>%
    pull()
  
  # Verificar se Enfermagem está disponível, usar como padrão
  default_curso <- if("Enfermagem" %in% cursos) {
    "Enfermagem"
  } else if(length(cursos) > 0) {
    cursos[1]
  } else {
    NULL
  }
  
  updateSelectizeInput(session, "curso", 
                       choices = cursos, 
                       selected = default_curso,
                       server = TRUE)
}, ignoreNULL = FALSE)

# Botão limpar filtros: reiniciar todas as seleções ao estado inicial
observeEvent(input$clear_filters, {
  updateSelectInput(session, "uf1", selected = sort(unique(caged_rais_curso$NM_UF))[1])
  updateSelectizeInput(session, "eixos", selected = character(0))
  updateSelectizeInput(session, "curso", selected = character(0))
  updateCheckboxInput(session, "todos_no_eixo", value = TRUE)
  updateCheckboxInput(session, "comparar_brasil", value = TRUE)
})

# Reativo: gerar dados filtrados e gráficos com base nas seleções atuais de filtro
plots_reactive <- reactive({
  geo_value <- input$uf1
  if (input$comparar_brasil && input$uf1 != "Brasil") {
    geo_value <- c(input$uf1, "Brasil")
  }
  
  curso_input <- if (input$todos_no_eixo) {
    caged_rais_curso %>%
      filter(Eixo_Tecnologico %in% input$eixos) %>%
      distinct(Curso) %>%
      pull()
  } else input$curso
  
  plot_caged_summary_double(
    df = caged_rais_curso,
    geo_value = geo_value,
    curso_values = curso_input,
    todos_no_eixo = input$todos_no_eixo,
    eixo_values = input$eixos
  )
})
###################################
# Renderizar gráfico de diferença salarial (salário de admissão vs desligamento)
output$plot_dif_salarial <- renderPlot({
  req(plots_reactive()$dif_sal_pc_plot)
  plots_reactive()$dif_sal_pc_plot
})

# Renderizar gráfico de rotatividade
output$plot_rotatividade <- renderPlot({
  req(plots_reactive()$rotatividade_plot)
  plots_reactive()$rotatividade_plot
})

# Renderizar título da seção de ranking
output$ranking_uf_title <- renderUI({
  req(input$uf1)
  h4(paste("⚠️ Relação de Cursos com Maior Escassez -", input$uf1), id = "ranking_uf_title")
})

#################################
output$ranking_uf <- renderTable({
  req(input$uf1)
  
  tabela <- caged_rais_curso %>%
    group_by(ANO, MES, Eixo_Tecnologico, Curso) %>%
    mutate(
      estoque_liquido_br = sum(if_else(NM_UF == "Brasil", NA, estoque_liquido), na.rm = TRUE)
    ) %>%
    filter(NM_UF == input$uf1) %>%
    group_by(ANO, MES) %>%
    mutate(
      across(.cols = c(dif_sal_adm_des_pc_m12, tx_rotatividade_m12, estoque_liquido),
             .fns = ~ percent_rank(.x)*100,
             .names = 'p_{.col}')
    ) %>%
    group_by(Curso) %>%
    mutate(
      dif_sal_adm_des_pc = if_else(is.infinite(dif_sal_adm_des_pc), NA, dif_sal_adm_des_pc),
      dif_sal_adm_des_pc_m12 = window_mean(dif_sal_adm_des_pc, lag = 11, lead = 0),
      
      soma_mov_adm_media_ano = window_mean(soma_mov_adm, lag = 11, lead = 0),
      `Estimativa Demanda Vagas` = window_mean(estoque_liquido, lag = 11, lead = 0)*0.15,
      
      `Demanda de vagas em relação ao total Brasil` = `Estimativa Demanda Vagas`/(window_mean(estoque_liquido_br, lag = 11, lead = 0)*0.15),
      
      tipologia_escassez = case_when(
        p_dif_sal_adm_des_pc_m12 >= 75 & p_tx_rotatividade_m12 >= 50 & p_estoque_liquido >= 60 ~ "Alerta de Escassez",
        (p_dif_sal_adm_des_pc_m12 >= 75 & p_tx_rotatividade_m12 >= 50 & p_estoque_liquido < 60) |
          (between(p_dif_sal_adm_des_pc_m12, 74.9999, 50) & p_tx_rotatividade_m12 >= 50 & p_estoque_liquido >= 50) ~ "Tendência de Escassez",
        (between(p_dif_sal_adm_des_pc_m12, 49.999, 25) | p_estoque_liquido >= 50) ~ "Situação Estável",
        p_dif_sal_adm_des_pc_m12 < 25 ~ "Sinal de Excesso",
        soma_mov_adm_media_ano < 30 ~ "Sem fluxo suficiente - INDICADORES DEIXAM DE SER INFORMATIVOS",
        TRUE ~ "Indisponível/Indeterminado"
      )
    ) %>%
    ungroup() %>%
    filter(ANO == max(ANO, na.rm = TRUE)) %>%
    filter(MES == max(MES, na.rm = TRUE)) %>%
    mutate(dif_sal_adm_des_pc_pond_m12 = scales::rescale(dif_sal_adm_des_pc_m12*estoque_liquido,  to = c(0, 1000))) %>%
    arrange(desc(dif_sal_adm_des_pc_pond_m12)) %>%
    filter(tipologia_escassez %in% c("Alerta de Escassez", "Tendência de Escassez")) %>%
    slice_head(n = 10) %>%
    transmute(
      `Eixo - Curso` = paste(Eixo_Tecnologico, Curso, sep = " - "),
      `Situação da Escassez` = tipologia_escassez,
      `Estimativa Demanda Vagas` = round(`Estimativa Demanda Vagas`),
      `Demanda de vagas em relação ao total Brasil (%)` = `Demanda de vagas em relação ao total Brasil`*100,
      #    `Demanda de vagas em relação ao total Brasil (%)` = `Demanda de vagas em relação ao total Brasil`*100,
      #  `Dif. Salarial adm e des (%)` = round(dif_sal_adm_des_pc_m12, 2),
      #  `Total Vínculos (milhares)` = round(estoque_liquido)/1000,
      `Indicador de Escassez (Dif. Salarial*Total Vínculos (escala 0 a 1000))` = round(dif_sal_adm_des_pc_pond_m12)
    )
  
  if (nrow(tabela) == 0) return(data.frame("Mensagem" = "Sem dados para este filtro."))
  tabela
}, striped = TRUE, bordered = TRUE, spacing = "xs", digits = 1)

## NOTA: A lógica de ranking inline abaixo substitui uma versão anterior (BM_FGV_Propag3.R)
## que chamava tabela_ranking_cursos, nunca definida neste arquivo.
# output$ranking_uf <- renderTable({
#   req(input$uf1)
#   
#   tabela <- tabela_ranking_cursos %>%
#     filter(NM_UF == input$uf1) 
#   
#   if (nrow(tabela) == 0) return(data.frame("Mensagem" = "Sem dados para este filtro."))
#   tabela
# }, striped = TRUE, bordered = TRUE, spacing = "xs", digits = 1)
# 
##########
# Gerar texto de avaliação tipológica de escassez com base na seleção atual de filtros
output$texto_escassez <- renderUI({
  df_escassez <- plots_reactive()$tipologia_escassez
  req(df_escassez)
  
  if (input$todos_no_eixo) {
    grupo_nome <- "Eixo Tecnológico"
    valores <- input$eixos
    col_grupo <- "Eixo_Tecnologico"
  } else {
    grupo_nome <- "Curso"
    valores <- input$curso
    col_grupo <- "Curso"
  }
  
  df_resumo <- df_escassez %>%
    filter(NM_UF == input$uf1) %>%
    filter(.data[[col_grupo]] %in% valores) %>%
    filter(ANO == max(ANO, na.rm = TRUE), MES == max(MES, na.rm = TRUE)) %>%
    select(!!col_grupo, tipologia_escassez) %>%
    distinct()
  
  if (nrow(df_resumo) == 0) {
    return(HTML("<b style='color: #333;'>Sem resultados para a seleção atual.</b>"))
  }
  
  resumos <- df_resumo %>%
    mutate(txt = paste0("<b>", grupo_nome, ":</b> ", .data[[col_grupo]], " — <b>", tipologia_escassez, "</b>")) %>%
    pull(txt)
  
  HTML(sprintf(
    "<div style='font-size:1.2em; line-height:1.5; color: #333 !important;'>
<b>Resultado da tipologia de escassez para %s:</b><br>%s
</div>",
    input$uf1,
    paste(resumos, collapse = "<br>")
  ))
})
#############
# Renderizar resumo dos filtros atualmente selecionados
output$selecao_atual <- renderUI({
  eixos <- if (is.null(input$eixos) || length(input$eixos) == 0) "Nenhum" else paste(input$eixos, collapse = ", ")
  cursos <- if (is.null(input$curso) || length(input$curso) == 0) "Nenhum" else paste(input$curso, collapse = ", ")
  HTML(sprintf(
    "<ul class='bullet-list'><li><b>UF:</b> %s</li>
      <li><b>Eixo(s):</b> %s</li>
      <li><b>Curso(s):</b> %s</li></ul>",
    input$uf1, eixos, cursos
  ))
})





}  
  

shinyApp(ui = ui, server = server)