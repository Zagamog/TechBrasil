# BM_FGV_Propag2a.R
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(scales)
library(dplyr)

# --- Load Data ---
load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")
propag_ept_financeiro <- readRDS("D:/Country/Brazil/TechBrazil/working/mec/propag_ept_financeiro.rds")

nome_ufs <- sort(unique(df_codes_ibge$NM_UF))
ufs <- sort(unique(propag_ept_financeiro$UF))
uf_nome_map <- df_codes_ibge %>% select(UF = CO_UF, NM_UF) %>% distinct()

# --- UI ---
ui <- dashboardPage(
  dashboardHeader(title = textOutput("dynamic_title")),
  
  dashboardSidebar(
    width = 350,
    
    selectizeInput(
      "UF",
      "Selecionar Unidade da Federação",
      choices = ufs,
      selected = "SP",
      multiple = FALSE
    ),
    
    br(), br(),
    
    div(
      style = "position: absolute; bottom: 20px; width: 100%; padding: 10px; font-size: 12px; color: #ccc;",
      HTML("<strong>Contato:</strong><br>
           Equipe FGV/DGPE: <a href='mailto:blix@fgv.br' style='color:#cccccc;'>blix@fgv.br</a><br>
           Banco Mundial: <a href='mailto:blax@worldbank.org' style='color:#cccccc;'>blax@worldbank.org</a>")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(".main-header { display: none; } .content-wrapper { margin-top: 0px !important; }"))
    ),
    
    fluidRow(
      column(
        12,
        div(
          style = "padding: 10px 20px; font-size: 24px; font-weight: bold; color: #0000ff;",
          textOutput("dynamic_title")
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        h3("Indicadores Financeiros do PROPAG", style = "color: #1f5673; font-weight: bold;"),
        plotOutput("fin_plot", height = "500px"),
        br(),
        h3("Tabela Financeira", style = "color: #1f5673; font-weight: bold;"),
        DTOutput("fin_table")
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  output$dynamic_title <- renderText({
    uf_name <- df_codes_ibge %>% filter(CO_UF == input$UF | UF == input$UF) %>% pull(NM_UF) %>% unique()
    paste0(uf_name %||% input$UF, ": Faça adesão ao Propag!")
  })
  
  filtered_data <- reactive({
    req(input$UF)
    propag_ept_financeiro %>% filter(UF == input$UF)
  })
  
  output$fin_plot <- renderPlot({
    df <- filtered_data()
    df_long <- df %>%
      tidyr::pivot_longer(cols = where(is.numeric) & !names(.) %in% c("UF", "Estado"),
                          names_to = "variavel", values_to = "valor") %>%
      filter(!is.na(valor))
    
    ggplot(df_long, aes(x = variavel, y = valor)) +
      geom_col(fill = "#1f5673") +
      coord_flip() +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      labs(x = NULL, y = "Valor (R$)", title = paste("Indicadores Financeiros -", input$UF)) +
      theme_minimal(base_size = 14)
  })
  
  output$fin_table <- renderDT({
    df <- filtered_data()
    
    # Clean up and format
    df <- df %>% select(-fef_share_pct) %>% 
      mutate(across(where(is.numeric), ~format(round(.x, 0), big.mark = ".", decimal.mark = ",")))
    
    datatable(
      df,
      options = list(pageLength = 30, scrollX = TRUE),
      rownames = FALSE,
      class = "stripe nowrap display"
    )
  })
}

shinyApp(ui, server)
