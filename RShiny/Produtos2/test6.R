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

# ————— 2) UI: just UF selector + two DTs —————
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
        )
      ),
      
      fluidRow(
        column(
          width = 3,
          h4("1) Matrículas por Eixo"),
          DTOutput("agg_table"),
          hr(),
          h4("2) Matrículas por Área"),
          DTOutput("area_table"),
          hr(),
          h4("3) Matrículas por Curso"),
          DTOutput("curso_table")
        ),
        column(
          width = 3,
          div(h4("Conteúdo do lado direito aqui"))
        )
      )
  )
)

# ————— 3) Server: UF → Eixo-table → Área-table —————
server <- function(input, output, session) {
  
  # populate UF dropdown
  observe({
    all_ufs <- sort(unique(df_mat_eixo_wide$NM_UF))
    updateSelectizeInput(
      session, "uf_select",
      choices  = c("Brasil", all_ufs),
      selected = "Brasil",
      server   = TRUE
    )
  })
  
  # reactive DF of Eixo rows for the chosen UF
  RKT_agg_df <- reactive({
    req(input$uf_select)
    df_mat_eixo_wide %>%
      filter(NM_UF == input$uf_select) %>%
      select(NM_UF, `Eixo Tecnológico`, `Matrículas 2023`, `Matrículas 2024`) %>%
      arrange(desc(`Matrículas 2024`))
  })
  
  # 1) render the Eixo-table with single selection + total row + formatting
  output$agg_table <- renderDT({
    df0 <- RKT_agg_df()
    
    # tack on the Total row
    total_row <- df0 %>%
      summarise(
        NM_UF               = "Total",
        `Eixo Tecnológico` = "",
        `Matrículas 2023`   = sum(`Matrículas 2023`, na.rm = TRUE),
        `Matrículas 2024`   = sum(`Matrículas 2024`, na.rm = TRUE)
      )
    df_final <- bind_rows(df0, total_row)
    
    datatable(
      df_final,
      selection = "single",
      rownames  = FALSE,
      class     = "compact stripe",
      options   = list(
        pageLength = nrow(df_final),
        autoWidth  = TRUE,
        language   = list(
          url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
        )
      )
    ) %>%
      formatCurrency(
        columns   = c("Matrículas 2023", "Matrículas 2024"),
        currency  = "",
        interval  = 3,
        mark      = ".",
        dec.mark  = ","
      ) %>%
      formatStyle(
        columns     = c("Matrículas 2023", "Matrículas 2024"),
        `text-align` = "right"
      ) %>%
      formatStyle(
        "NM_UF",
        target     = "row",
        fontWeight = styleEqual("Total", "bold"),
        background = styleEqual("Total", "#0d0863")
      )
  })
  
  # grab the clicked Eixo from agg_table
  # Option A: base R [[
  RKT_selected_eixo <- reactive({
    req(input$agg_table_rows_selected)
    df <- RKT_agg_df()
    df[[ "Eixo Tecnológico" ]][ input$agg_table_rows_selected ]
  })
  
  # 2) render the Área-table once an Eixo is clicked
 
  # --- 3) build a reactive DF for Área so selection indices line up ---
  RKT_area_df <- reactive({
    eixo <- RKT_selected_eixo()
    req(eixo, input$uf_select)
    # valid Áreas for this eixo
    df_exarcu %>%
      filter(`Eixo Tecnológico` == eixo) %>%
      distinct(`Área Tecnológica`) %>%
      # join in the UF counts
      left_join(
        df_mat_area_wide %>% filter(NM_UF == input$uf_select),
        by = "Área Tecnológica"
      ) %>%
      select(
        NM_UF,
        `Área Tecnológica`,
        `Matrículas 2023`,
        `Matrículas 2024`
      ) %>%
      arrange(desc(`Matrículas 2024`))
  })
  
  # replace your existing renderDT for area_table with this (enabling single selection)
  output$area_table <- renderDT({
    dat <- RKT_area_df()
    datatable(
      dat,
      selection = "single",
      rownames  = FALSE,
      class     = "compact stripe",
      options   = list(
        pageLength = nrow(dat),
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
  
  # capture the clicked Área
  RKT_selected_area <- reactive({
    req(input$area_table_rows_selected)
    RKT_area_df()$`Área Tecnológica`[ input$area_table_rows_selected ]
  })
  
  # --- 4) cursos for the selected UF ↦ Eixo ↦ Área ---
  RKT_curso_df <- reactive({
    uf   <- input$uf_select
    eixo <- RKT_selected_eixo()
    area <- RKT_selected_area()
    req(uf, eixo, area)
    
    # which cursos belong?
    df_exarcu %>%
      filter(
        `Eixo Tecnológico` == eixo,
        `Área Tecnológica` == area
      ) %>%
      distinct(IDX_EIXCUR, `Denominação do Curso`) %>%
      # join in the UF×curso counts
      left_join(
        df_mat_curso_wide %>% filter(NM_UF == uf),
        by = c("IDX_EIXCUR", "Denominação do Curso")
      ) %>%
      select(
        NM_UF,
        `Denominação do Curso`,
        `Matrículas 2023`,
        `Matrículas 2024`
      ) %>%
      arrange(desc(`Matrículas 2024`))
  })
  
  # 5) render the Curso‐table
  output$curso_table <- renderDT({
    dat <- RKT_curso_df()
    datatable(
      dat,
      rownames = FALSE,
      class    = "compact stripe",
      options  = list(
        pageLength = nrow(dat),
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
  
  
}

shinyApp(ui, server)
