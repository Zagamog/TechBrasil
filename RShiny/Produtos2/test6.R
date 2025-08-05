# test6.R
library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(DT)

# ————— 1) Load your four “_wide” tables exactly as before —————
load("df_mat_uf.rda")
load("df_mat_eixo.rda")
load("df_mat_area.rda")
load("df_mat_curso.rda")
load("meta11a_opcoes.rda")
load("df_exarcu.rda")      # << new
load("rais_cbo6_uf23.rda")  
load("rais_cbo6_uf24.rda")



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

###
#CBO
# Combine and add year label
df_rais_all <- bind_rows(
  rais_cbo6_uf23 %>% mutate(ANO = 2023),
  rais_cbo6_uf24 %>% mutate(ANO = 2024)
)


# Build df_gg from df_rais_all — ensures no hidden dependency
df_gg <- df_rais_all %>%
  filter(!is.na(cbo_1dig), !is.na(cbo_gragru)) %>%
  distinct(cbo_1dig, cbo_gragru) %>%
  arrange(cbo_1dig)

df_gg_clean <- df_gg %>%
  group_by(cbo_gragru) %>%
  slice(1) %>%
  ungroup()


# ——— Aggregates by Brazil level ———

# 1. CBO 1-digit (cbo_1dig)
rais_br_cbo1 <- df_rais_all %>% filter(!is.na(cbo_gragru)) %>%
  group_by(ANO, cbo_1dig, cbo_gragru) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
  mutate(NM_UF = "Brasil", SG_UF = "BR")
sum(is.na(rais_br_cbo1$cbo_1dig))


# 2. CBO 4-digit
rais_br_cbo4 <- df_rais_all %>% filter(!is.na(cbo_familia)) %>%
  group_by(ANO, cbo_4dig, cbo_familia) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
  mutate(NM_UF = "Brasil", SG_UF = "BR")

# 3. Full CodCBO level (occupation level)
rais_br_codcbo <- df_rais_all %>% filter(!is.na(`Ocupação`)) %>%
  group_by(ANO, CodCBO, `Ocupação`) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
  mutate(NM_UF = "Brasil", SG_UF = "BR")

# ——— Merge back Brasil with UF-level if needed ———
df_rais1dig <- bind_rows(df_rais_all %>% 
                           group_by(ANO, NM_UF, SG_UF, cbo_1dig, cbo_gragru) %>%
                           summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop"),
                         rais_br_cbo1)

df_rais4dig <- bind_rows(df_rais_all %>% 
                           group_by(ANO, NM_UF, SG_UF, cbo_4dig, cbo_familia) %>%
                           summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop"),
                         rais_br_cbo4)

df_raisCodCBO <- bind_rows(df_rais_all %>% 
                             group_by(ANO, NM_UF, SG_UF, CodCBO, `Ocupação`) %>%
                             summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop"),
                           rais_br_codcbo)


df_rais1dig_wide <- df_rais1dig %>%
  pivot_wider(
    names_from  = ANO,
    values_from = vinculos,
    values_fill = list(vinculos = 0),
    values_fn   = sum
  ) %>%
  rename(`Vínculos 2023` = `2023`, `Vínculos 2024` = `2024`)


df_rais4dig_wide <- df_rais4dig %>%
  pivot_wider(
    names_from  = ANO,
    values_from = vinculos,
    values_fill = list(vinculos = 0),
    values_fn   = sum
  ) %>%
  rename(`Vínculos 2023` = `2023`, `Vínculos 2024` = `2024`)

df_raisCodCBO_wide <- df_raisCodCBO %>%
  pivot_wider(
    names_from  = ANO,
    values_from = vinculos,
    values_fill = list(vinculos = 0),
    values_fn   = sum
  ) %>%
  rename(`Vínculos 2023` = `2023`, `Vínculos 2024` = `2024`)

df_raisCodCBO_wide <- df_raisCodCBO_wide %>%
  mutate(
    CodCBO   = as.character(CodCBO),
    cbo_1dig = substr(CodCBO, 1, 1)
  )



# For third part of tab both supply and demand together
load("qbq_ocup_cmento1.rda")
load("cnct_qbq_matches.rda")
load("qbq_cnct_matches.rda")



# ————— 2) UI: just UF selector + two DTs —————
# Simplified UI layout based on new design:
# 1. Top row: UF + match direction selector
# 2. Main content: two conditional columns based on match direction
ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/custom.css")),
  
  div(class = "checkbox-dark-panel",
      # Top controls row (unchanged + new top_n)
      fluidRow(
        column(
          width = 3,
          selectizeInput("uf_select", "Selecionar UF ou Brasil:",
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
          checkboxGroupInput("level_filter", "Níveis de Trabalhador Técnico:",
                             choices = 1:6,
                             selected = 1:3,
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
                       value = 5, min = 1, max = 10, step = 1)
        )
      ),
      fluidRow(
        # OFERTA → DEMANDA
        conditionalPanel(
          condition = "input.match_direction == 'Oferta \u2192 Demanda'",
          # LHS: three drill-down tables
          column(width = 6,
                 h4("1a - Matrículas por Eixo, Área e Curso"),
                 DTOutput("agg_table"), hr(),
                 h4("1b - Matrículas por Área"),
                 DTOutput("area_table"), hr(),
                 h4("1c - Matrículas por Curso"),
                 DTOutput("curso_table")
          ),
          # RHS: single top-n occupations
          column(width = 6,
                 h4("Melhores Ocupações e Vínculos"),
                 DTOutput("course_occ_table")
          )
        ),
        # DEMANDA → OFERTA
        conditionalPanel(
          condition = "input.match_direction == 'Demanda \u2192 Oferta'",
          # LHS: three drill-down tables
          column(width = 6,
                 h4("1a - Vínculos por Grande Grupo"),
                 DTOutput("cbo1_table"), hr(),
                 h4("1b - Vínculos por Família"),
                 DTOutput("cbo4_table"), hr(),
                 h4("1c - Vínculos por Ocupação"),
                 DTOutput("cbo6b_table")
          ),
          # RHS: single top-n courses
          column(width = 6,
                 h4("Melhores Cursos e Matrículas"),
                 DTOutput("course_agg_table_rev")
          )
        )
      ) # end fluidRow
  ) # end div
) # end fluidPage




# ————— 3) Server: UF → Eixo-table → Área-table —————
server <- function(input, output, session) {
  
  
  observe({
    all_ufs <- sort(unique(df_mat_eixo_wide$NM_UF))
    updateSelectizeInput(
      session, "uf_select",
      choices  = c("Brasil", all_ufs),
      selected = "Brasil"
    )
  })
  
  #### GLOBAL REACTIVES (BOTH MODES) ####
  
  # 1) Eixo data for selected UF
  RKT_agg_df <- reactive({
    req(input$uf_select)
    df_mat_eixo_wide %>%
      filter(NM_UF == input$uf_select) %>%
      select(
        NM_UF,
        `Eixo Tecnológico`,
        `Matrículas 2023`,
        `Matrículas 2024`
      ) %>%
      arrange(desc(`Matrículas 2024`))
  })
  
  # 2) Selected Eixo from agg_table
  RKT_selected_eixo <- reactive({
    req(input$agg_table_rows_selected)
    df <- RKT_agg_df()
    df$`Eixo Tecnológico`[input$agg_table_rows_selected]
  })
  
  #### 0a) Base matches for Oferta → Demanda ####
  RKT_base_matches <- reactive({
    req(
      input$match_direction == "Oferta → Demanda",
      input$uf_select,
      input$level_filter,
      input$score_thresh
    )
    eixo_sel <- RKT_selected_eixo()
    cnct_qbq_matches %>%
      filter(final_score >= input$score_thresh) %>%
      mutate(
        CodCBO = as.character(as.integer(CodCBO)),
        IDX_EIXCUR = {
          ace <- as.character(IDX_EIXARECUR)
          paste0(substr(ace, 1, 2), sprintf("%03d", as.integer(substr(ace, 5, 6))))
        }
      ) %>%
      inner_join(
        qbq_ocup_cmento1 %>%
          mutate(CodCBO = as.character(CodCBO)) %>%
          select(CodCBO, NivelOcupacao, NomeOcupacao = `Ocupação`),
        by = "CodCBO"
      ) %>%
      filter(NivelOcupacao %in% input$level_filter) %>%
      inner_join(
        df_exarcu %>% select(IDX_EIXCUR, `Denominação do Curso`, `Eixo Tecnológico`),
        by = "IDX_EIXCUR"
      ) %>%
      filter(`Eixo Tecnológico` == eixo_sel) %>%
      left_join(
        df_raisCodCBO_wide %>%
          filter(NM_UF == input$uf_select) %>%
          select(CodCBO, `Vínculos 2023`, `Vínculos 2024`),
        by = "CodCBO"
      )
  })
  
  # DEBUG: print columns of RKT_base_matches()
  observe({
    req(input$agg_table_rows_selected, input$match_direction == "Oferta → Demanda")
    bm <- RKT_base_matches()
    cat("[DEBUG] RKT_base_matches columns:", paste(names(bm), collapse=", "), "
")
  })
  
  #### OFERTA → DEMANDA SECTION ####
  
  ### 1a) Matrículas por Eixo, Área e Curso ###
  # Use RKT_agg_df
  output$agg_table <- renderDT({
    datatable(
      RKT_agg_df(),
      selection = "single",
      rownames  = FALSE,
      class     = "compact stripe",
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
  
  ### 1b) Matrículas por Área ###
  RKT_area_df <- reactive({
    req(input$match_direction == "Oferta → Demanda", input$agg_table_rows_selected)
    sel_eixo <- RKT_selected_eixo()
    df_exarcu %>%
      filter(`Eixo Tecnológico` == sel_eixo) %>%
      distinct(`Área Tecnológica`) %>%
      left_join(
        df_mat_area_wide %>% filter(NM_UF == input$uf_select),
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
      class     = "compact stripe",
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
  
  #### OFERTA → DEMANDA SECTION ####
  
  ### 1) Grande Grupo summary ###
  output$match_summary_table <- renderDT({
    df <- RKT_base_matches() %>%
      distinct(cbo_1dig, CodCBO, .keep_all = TRUE) %>%
      group_by(cbo_1dig) %>%
      summarise(
        `Vínculos 2023` = sum(`Vínculos 2023`, na.rm = TRUE),
        `Vínculos 2024` = sum(`Vínculos 2024`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(df_gg, by = "cbo_1dig") %>%
      select(`Grande Grupo` = cbo_gragru, `Vínculos 2023`, `Vínculos 2024`) %>%
      arrange(desc(`Vínculos 2024`))
    datatable(
      df,
      selection = "single",
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json")
      )
    )
  })
  
  ### 2) Área detail ###
  RKT_selected_grande_grupo <- reactive({
    req(input$match_summary_table_rows_selected)
    df <- isolate(
      RKT_base_matches() %>%
        distinct(cbo_1dig, CodCBO, .keep_all = TRUE) %>%
        left_join(df_gg, by = "cbo_1dig") %>%
        select(cbo_gragru = `Grande Grupo`, cbo_1dig)
    )
    df$cbo_1dig[input$match_summary_table_rows_selected]
  })
  output$match_familia_table <- renderDT({
    sel1 <- RKT_selected_grande_grupo()
    df <- RKT_base_matches() %>%
      filter(cbo_1dig == sel1) %>%
      distinct(`Área Tecnológica`, CodCBO, .keep_all = TRUE) %>%
      group_by(`Área Tecnológica`) %>%
      summarise(
        `Vínculos 2023` = sum(`Vínculos 2023`, na.rm = TRUE),
        `Vínculos 2024` = sum(`Vínculos 2024`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`Vínculos 2024`))
    datatable(df, selection = "single", rownames = FALSE, class = "compact stripe") %>%
      formatCurrency(c("Vínculos 2023","Vínculos 2024"),"",3,".",",")
  })
  
  ### 3) Cursos drill-down (LHS 1c) ###
  RKT_selected_area <- reactive({
    req(input$area_table_rows_selected)
    RKT_area_df()$`Área Tecnológica`[input$area_table_rows_selected]
  })
  
  RKT_curso_df <- reactive({
    req(input$area_table_rows_selected)
    uf <- input$uf_select
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
      class     = "compact stripe",
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
  
  ### 3d) Capture selected course ###
  ### 3d) Capture selected course ###
  RKT_selected_course <- reactive({
    req(input$curso_table_rows_selected)
    sel_row <- input$curso_table_rows_selected
    crs_df <- RKT_curso_df()
    cat("[DEBUG] curso_table_rows_selected:", sel_row, "of", nrow(crs_df), "rows
")
    if("IDX_EIXCUR" %in% names(crs_df)) {
      cat("[DEBUG] Available IDX_EIXCUR values:", paste(crs_df$IDX_EIXCUR, collapse=", "), "
")
    } else {
      cat("[DEBUG] IDX_EIXCUR not found in curso_df columns
")
    }
    crs_df$IDX_EIXCUR[sel_row]
  })
  
  ### 4) Top‑N occupations for selected course (RHS) ###
  RKT_course_occ_df <- reactive({
    # 4) Top‑N occupations for selected course (debugging)
    req(input$curso_table_rows_selected)
    sel_idx <- RKT_selected_course()
    cat("[DEBUG] Selected course IDX_EIXCUR:", sel_idx, "
")
    
    bm <- RKT_base_matches()
    cat("[DEBUG] Base matches rows:", nrow(bm),
        "columns:", paste(names(bm), collapse=", "), "
")
    if("IDX_EIXCUR" %in% names(bm)) {
      cat("[DEBUG] Head IDX_EIXCUR values:",
          paste(head(bm$IDX_EIXCUR, 10), collapse=", "), "
")
    }
    
    df <- bm %>%
      filter(IDX_EIXCUR %in% sel_idx) %>%
      arrange(desc(final_score)) %>%
      slice_head(n = input$top_n) %>%
      select(
        Ocupação        = NomeOcupacao,
        `Vínculos 2023`,
        `Vínculos 2024`,
        Score           = final_score
      )
    cat("[DEBUG] Resulting df rows:", nrow(df), "
")
    df
  })
  
  output$course_occ_table <- renderDT({
    df <- RKT_course_occ_df()
    validate(need(nrow(df) > 0, "Nenhuma correspondência encontrada."))
    datatable(
      df,
      selection = "single",
      rownames  = FALSE,
      class     = "compact stripe",
      options   = list(
        pageLength = input$top_n,
        autoWidth  = TRUE,
        language   = list(
          url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
        )
      )
    ) %>%
      formatCurrency(
        c("Vínculos 2023","Vínculos 2024"),
        currency = "", interval = 3, mark = ".", dec.mark = ","
      ) %>%
      formatStyle(
        c("Vínculos 2023","Vínculos 2024"),
        `text-align` = "right"
      )
  })
  
  #### 0b) Base matches for Demanda → Oferta ####
  RKT_base_matches_rev <- reactive({
    req(input$match_direction == "Demanda → Oferta",
        input$uf_select, input$level_filter, input$score_thresh)
    qbq_cnct_matches %>%
      filter(final_score >= input$score_thresh) %>%
      mutate(
        CodCBO = as.character(as.integer(CodCBO)),
        IDX_EIXCUR = {
          ace <- as.character(IDX_EIXARECUR)
          paste0(substr(ace, 1, 2), sprintf("%03d", as.integer(substr(ace, 5, 6))))
        }
      ) %>%
      inner_join(
        df_exarcu %>% select(IDX_EIXCUR, `Denominação do Curso`),
        by = "IDX_EIXCUR"
      ) %>%
      left_join(
        df_raisCodCBO_wide %>% filter(NM_UF == input$uf_select)
        %>% select(CodCBO, `Vínculos 2023`, `Vínculos 2024`),
        by = "CodCBO"
      )
  })
  
  #### DEMANDA → OFERTA SECTION ####
  
  #### Reactives for Demanda → Oferta Drilldown ####
  # 1a) Vínculos por Grande Grupo (1-digit)
  RKT_cbo1_df <- reactive({
    req(input$match_direction == "Demanda → Oferta", input$uf_select)
    df_rais1dig_wide %>%
      filter(NM_UF == input$uf_select) %>%
      select(
        cbo_gragru,
        `Vínculos 2023`,
        `Vínculos 2024`
      ) %>%
      arrange(desc(`Vínculos 2024`))
  })
  
  # 1b) Vínculos por Família (4-digit)
  RKT_cbo4_df <- reactive({
    req(input$match_direction == "Demanda → Oferta", input$uf_select, input$cbo1_table_rows_selected)
    # get selected 1-digit code
    sel1 <- RKT_cbo1_df()$cbo_gragru[input$cbo1_table_rows_selected]
    df_rais4dig_wide %>%
      filter(
        NM_UF == input$uf_select,
        substr(cbo_4dig, 1, 1) == sel1
      ) %>%
      select(
        cbo_familia,
        `Vínculos 2023`,
        `Vínculos 2024`
      ) %>%
      arrange(desc(`Vínculos 2024`))
  })
  
  
  
  ### 1) Grande Grupo (LHS) ###
  output$cbo1_table <- renderDT({
    datatable(
      RKT_cbo1_df(),
      selection = "single",
      rownames = FALSE,
      class = "compact stripe",
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  ### 2) Família (LHS) ###
  output$cbo4_table <- renderDT({
    datatable(
      RKT_cbo4_df(),
      selection = "single",
      rownames = FALSE,
      class = "compact stripe",
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  ### 3) Ocupação (LHS) ###
  RKT_cbo6b_df <- reactive({
    req(input$cbo4_table_rows_selected)
    selected_fam <- RKT_cbo4_df()$cbo_familia[input$cbo4_table_rows_selected]
    df_rais4dig_wide %>%
      filter(
        NM_UF == input$uf_select,
        substr(cbo_4dig,1,4) %in% selected_fam
      ) %>%
      select(CodCBO, Ocupação, `Vínculos 2023`, `Vínculos 2024`)
  })
  output$cbo6b_table <- renderDT({
    datatable(
      RKT_cbo6b_df(),
      selection = "single",
      rownames = FALSE,
      class = "compact stripe",
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  ### 4) Top‑N courses (RHS) ###
  RKT_selected_cbo_rev <- reactive({
    req(input$cbo6b_table_rows_selected)
    RKT_cbo6b_df()$CodCBO[input$cbo6b_table_rows_selected]
  })
  RKT_course_agg_rev_df <- reactive({
    req(input$cbo6b_table_rows_selected)
    sel_cbo <- RKT_selected_cbo_rev()
    RKT_base_matches_rev() %>%
      filter(CodCBO == sel_cbo) %>%
      arrange(desc(final_score)) %>%
      slice_head(n = input$top_n) %>%
      select(`Denominação do Curso`, `Matrículas 2023`, `Matrículas 2024`, Score = final_score)
  })
  output$course_agg_table_rev <- renderDT({
    df <- RKT_course_agg_rev_df()
    validate(need(nrow(df)>0, "Nenhum curso encontrado."))
    datatable(df, rownames=FALSE, class="compact stripe", options=list(pageLength=input$top_n))
  })
}

shinyApp(ui, server)




