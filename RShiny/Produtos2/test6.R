# test6.R
library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(DT)

# Load required data files
load("df_mat_uf.rda")       # Matricula in EPT by UF and ANO 
load("df_mat_eixo.rda")     # Matriculas by Eixo Tecnológico
load("df_mat_area.rda")     # Matriculas by Área Tecnológica
load("df_mat_curso.rda")    # Matriculas by Course
load("df_exarcu.rda")       # Course metadata (IDX_EIXCUR, Eixo, Area, Denominação)
load("rais_cbo6_uf23.rda")  # RAIS employment data 2023
load("rais_cbo6_uf24.rda")  # RAIS employment data 2024
load("cnct_qbq_matches2.rda") # Course to occupation matches
load("qbq_cnct_matches2.rda") # Occupation to course matches

# Create wide format datasets for matriculas
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

# Combine RAIS data and create aggregations
df_rais_all <- bind_rows(
  rais_cbo6_uf23 %>% mutate(ANO = 2023),
  rais_cbo6_uf24 %>% mutate(ANO = 2024)
)

# Create Brazil-level aggregations
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

# Combine UF and Brazil level data
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

# Convert to wide format
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

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/custom.css")),
  
  div(class = "checkbox-dark-panel",
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

# Server
server <- function(input, output, session) {
  
  # Initialize UF choices
  observe({
    all_ufs <- sort(unique(df_mat_eixo_wide$NM_UF))
    updateSelectizeInput(
      session, "uf_select",
      choices = c("Brasil", setdiff(all_ufs, "Brasil")),
      selected = "Brasil"
    )
  })
  
  #### OFERTA → DEMANDA ####
  
  # Table 1a - Matrículas por Eixo
  RKT_agg_df <- reactive({
    req(input$match_direction == "Oferta \u2192 Demanda")
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
  
  RKT_selected_eixo <- reactive({
    req(input$agg_table_rows_selected)
    df <- RKT_agg_df()
    df$`Eixo Tecnológico`[input$agg_table_rows_selected]
  })
  
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
  
  RKT_selected_course <- reactive({
    req(input$curso_table_rows_selected)
    df <- RKT_curso_df()
    df$IDX_EIXCUR[input$curso_table_rows_selected]
  })
  
  # Auto-update top_n based on available matches within threshold
  observeEvent(list(RKT_selected_course(), input$score_thresh), {
    req(input$match_direction == "Oferta \u2192 Demanda")
    req(input$curso_table_rows_selected)
    req(input$score_thresh)
    
    selected_course_id <- RKT_selected_course()
    
    # Count matches above the current threshold
    n_matches <- cnct_qbq_matches2 %>%
      filter(
        IDX_EIXCUR == selected_course_id,
        final_score >= input$score_thresh
      ) %>%
      nrow()
    
    # Set top_n to the number of available matches
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
    
    # Filter by score threshold and apply top_n limit
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
      filter(NM_UF == input$uf_select) %>%
      select(CodCBO, `Ocupação`, `Vínculos 2023`, `Vínculos 2024`)
    
    # Merge and return
    result <- left_join(matches_final, rais_filtered, by = "CodCBO") %>%
      select(CodCBO, `Ocupação`, `Vínculos 2023`, `Vínculos 2024`, final_score, semantic, tfidf) %>%
      arrange(desc(final_score))
    
    return(result)
  })
  
  output$course_occ_table <- renderDT({
    df <- RKT_course_occ_df()
    
    validate(need(nrow(df) > 0, "Nenhuma correspondência encontrada."))
    
    # Format the score columns for better display
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
      class = "compact stripe",
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
    req(input$match_direction == "Demanda \u2192 Oferta", input$uf_select)
    df <- df_rais1dig_wide %>%
      dplyr::filter(NM_UF == input$uf_select) %>%
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
      class     = "compact stripe",
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
        input$uf_select, input$cbo1_table_rows_selected)
    sel1 <- RKT_selected_cbo1_code()
    df <- df_rais4dig_wide %>%
      dplyr::filter(
        NM_UF == input$uf_select,
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
      class     = "compact stripe",
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
        input$uf_select,
        input$cbo4_table_rows_selected)
    
    sel4 <- RKT_selected_cbo4_code()
    
    df <- df_raisCodCBO_wide %>%
      dplyr::filter(
        NM_UF == input$uf_select,
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
      class     = "compact stripe",
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
    
    # Get enrollment data for the selected UF
    enrollments <- df_mat_curso_wide %>%
      filter(NM_UF == input$uf_select) %>%
      select(IDX_EIXCUR, `Matrículas 2023`, `Matrículas 2024`)
    
    # Join and combine
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
  
  # Render the occupation to course matches table
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
    
    # Format scores for display
    df_display <- df %>%
      mutate(final_score = round(final_score, 3))
    
    datatable(
      df_display,
      selection = "single",
      rownames = FALSE,
      class = "compact stripe",
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
  
  # Auto-update top_n based on available matches for occupation to course
  observeEvent(list(RKT_selected_cbo_code(), input$score_thresh), {
    req(input$match_direction == "Demanda → Oferta")
    req(input$cbo6b_table_rows_selected)
    
    cbo_code <- RKT_selected_cbo_code()
    
    # Count available matches
    n_matches <- qbq_cnct_matches2 %>%
      filter(CodCBO == cbo_code, final_score >= input$score_thresh) %>%
      nrow()
    
    # Update top_n input
    if (n_matches > 0) {
      updateNumericInput(session, "top_n", 
                         value = min(n_matches, 10), 
                         max = min(n_matches, 50))
    }
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)