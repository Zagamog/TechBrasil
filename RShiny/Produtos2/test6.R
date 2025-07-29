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

# ————— 2) UI: only Eixo + UF + DT —————
ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/custom.css")),
  div(class = "checkbox-dark-panel",
      
      fluidRow(
        column(
          width = 3,
          selectizeInput("eixo_select", "Selecione o Eixo:",
                         choices = NULL, options = list(placeholder="Todos os Eixos"))
        ),
        column(
          width = 3,
          selectizeInput("uf_select", "Selecionar UF ou Brasil:",
                         choices = NULL, options = list(placeholder="Brasil"))
        )
      ),
      
      # Now split the table half & half:
      fluidRow(
        column(
          width = 3,
          DT::DTOutput("agg_table")
        ),
        column(
          width = 3,
          # Here goes whatever you want on the right half
          div(h4("Conteúdo do lado direito aqui"))
        )
      )
      
  )
)

# ————— 3) Server: filter eixo‐wide by UF + Eixo and show Name+2023+2024 —————
server <- function(input, output, session) {
 
  # 1) populate eixo dropdown on startup
  observe({
    all_eixos <- sort(unique(df_mat_eixo_wide$`Eixo Tecnológico`))
    all_eixos <- setdiff(all_eixos, "Militar")
    choices <- c("Total de todos eixos", all_eixos)
    updateSelectizeInput(
      session, "eixo_select",
      choices = choices,
      selected = "Total de todos eixos",
      server = TRUE
    )
  })
  
  observe({
    all_ufs <- sort(unique(df_mat_eixo_wide$NM_UF))
    updateSelectizeInput(
      session, "uf_select",
      choices  = c("Brasil", all_ufs),
      selected = "Brasil",
      server   = TRUE
    )
  })

  output$agg_table <- renderDT({
    
    req(input$uf_select, input$eixo_select)
    
    # 1) pick only the chosen UF (or Brasil)
    df_out <- df_mat_eixo_wide %>%
      filter(NM_UF == input$uf_select) 
     
    
    # 2) if they selected one eixo, filter that too
    if (input$eixo_select != "Total de todos eixos") {
      df_out <- df_out %>%
        filter(`Eixo Tecnológico` == input$eixo_select)
    }
    
    df_out <- df_out %>% select(NM_UF, `Eixo Tecnológico`, `Matrículas 2023`, `Matrículas 2024`) %>%
    arrange(desc(`Matrículas 2024`))
    
    # 4) build a one-row “totals” DF
    total_row <- df_out %>%
      summarise(
        NM_UF               = "Total",
        `Eixo Tecnológico` = "",
        `Matrículas 2023`   = sum(`Matrículas 2023`, na.rm = TRUE),
        `Matrículas 2024`   = sum(`Matrículas 2024`, na.rm = TRUE)
      )
    
    # 5) bind it on
    df_final <- bind_rows(df_out, total_row)
    
    datatable(
      df_final,
      rownames = FALSE,
      class    = "compact stripe",
      options  = list(
        pageLength = 13,
        autoWidth  = TRUE,
        # load Portuguese-Brazil translations:
        language   = list(url = 
                            "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
        )
      )
    ) %>%
      # format the matrícula columns with “.” as thousands, “,” as decimal
      formatCurrency(
        columns   = c("Matrículas 2023", "Matrículas 2024"),
        currency  = "",        # no currency symbol
        interval  = 3,         # every 3 digits
        mark      = ".",       # thousands separator
        dec.mark  = ","        # decimal separator
      ) %>%
      # right-align those numbers
      formatStyle(
        columns   = c("Matrículas 2023", "Matrículas 2024"),
        'text-align' = 'right'
      ) %>%
      # bold the total row
      formatStyle(
        'NM_UF',
        target     = 'row',
        fontWeight = styleEqual("Total", "bold"),
        background = styleEqual("Total", "#0d0863")
      )
    

  })
  
  
  
  
}

shinyApp(ui, server)
