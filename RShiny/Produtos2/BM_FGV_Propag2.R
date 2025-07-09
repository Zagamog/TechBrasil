# BM_FGV_Propag2.R
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(ggtext)
library(DT)
library(scales)
library(patchwork)
library(dplyr)
library(tidyr)
library(purrr)

options(warn=-1) # Too many pesky warnings, terrain, terrain, terrain, pull up, pull up 

# Load Propag scraped data
propag_ept_financeiro <- readRDS("propag_ept_financeiro.rds")

# Load Censo Escolar data 2007 to 2024 UF aggregates
load("df_censo_UF.rda")

load("meta11a_opcoes.rda")  

# Get State names for display
nome_ufs <- sort(unique(df_censo_UF$NM_UF))  # Ensure sorted and unique


# --- Define variable choices for Oferta EPT ---
ept_vars <- c("QT_MAT_PROF_TEC_PROPAG", "QT_MAT_PROF_TEC", "QT_MAT_PROF_TEC_SUBS", "QT_MAT_EJA_MED_TEC")
other_vars <- c("QT_MAT_MED", "QT_MAT_BAS", "QT_MAT_FUND", "QT_MAT_EJA")



library(RColorBrewer)

uf_levels <- sort(unique(propag_ept_financeiro$UF))  # sorted for consistency
uf_colors <- setNames(
  colorRampPalette(brewer.pal(9, "Set1"))(length(uf_levels)),
  uf_levels
)

`%||%` <- function(a, b) if (!is.null(a)) a else b


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

# Define allowed variables and their order
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

# Create a named vector: label = varname
fin_choices <- setNames(allowed_vars, sapply(allowed_vars, function(v) var_labels[[v]] %||% v))


# UI drop-in replacement (only the ui object)
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
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
    # Header title area
    tags$div(
      style = "padding: 10px 20px; font-size: 24px; font-weight: bold; color: #0000ff;",
      HTML('Faça adesão ao <span style="color: #FFD700; text-shadow: 1px 1px #333;">Propag</span> !')
    ),
    fluidRow(
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
          " e ",
          span("FGV/DGPE", style = "color: #ffcc00;")
        )
      )
    ),
    
    tabsetPanel(id = "tab_selection", selected = "Situação - Meta 11 (vigente)",
                tabPanel("Tema Financiero",
                         fluidPage(
                           h3("Visualização Financeira do PROPAG", style = "color: #1f5673; font-weight: bold;"),
                           
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
                           DT::dataTableOutput("tab1_fin_table"),
                           
                           br(), br(),
                           div(
                             style = "font-size: 12px; color: #999; text-align: center; padding: 10px;",
                             HTML(
                               "<strong>Para dúvidas ou consultas:</strong><br>
               Envie mensagem para:<br>
               <strong>Equipe FGV/DGPE:</strong> <a href='mailto:blix@fgv.br' style='color:#999;'>blix@fgv.br</a><br>
               <strong>Equipe BM:</strong> <a href='mailto:blax@worldbank.org' style='color:#999;'>blax@worldbank.org</a>"
                             )
                           )
                         )
                ),
                
                tabPanel("Tema Demanda EPT",
                         fluidRow(
                           column(6,
                                  tags$label("Choose Designation →", style = "color: black; font-weight: bold;"),
                                  selectizeInput("P02_designation", "", choices = NULL, multiple = FALSE)
                           ),
                           column(6,
                                  tags$label("Choose Person", style = "color: black; font-weight: bold;"),
                                  selectizeInput("P02_name", "", choices = NULL, multiple = FALSE)
                           )
                         ),
                         DTOutput("P02_table")
                ),
                
                tabPanel("Situação - Meta 11 (vigente)",
                         fluidPage(
                           h3("Evolução da Oferta de EPT por UF", style = "color: #1f5673; font-weight: bold;"),
                           fluidRow(
                             column(4,
                                    selectizeInput("oferta_uf", "Selecionar UF (NM_UF):",
                                                   choices = sort(unique(df_censo_UF$NM_UF)),
                                                   selected = "Rio Grande do Norte")
                             ),
                             column(4,
                                    selectizeInput("oferta_ept_var", "Variável EPT:",
                                                   choices = ept_vars,
                                                   selected = "QT_MAT_PROF_TEC_PROPAG")
                             ),
                             column(4,
                                    selectizeInput("oferta_other_var", "Outra variável:",
                                                   choices = other_vars,
                                                   selected = "QT_MAT_MED")
                             )
                           ),
                           plotOutput("oferta_ept_plot", height = "500px"),
                           br(),
                           h3("Tabela de Dados (2007–2024)", style = "color: #1f5673; font-weight: bold;"),
                           DTOutput("oferta_ept_table")
                         )
                ),
                
                tabPanel("Situação - Meta 11a",
                         fluidPage(
                           h3("Meta 11a: EPT como % do Ensino Médio", style = "color: #1f5673; font-weight: bold;"),
                           
                           fluidRow(
                             column(2,
                                    selectizeInput("meta11a_uf", "Selecionar UF:", choices = sort(unique(df_censo_UF$NM_UF)), selected = "Rio Grande do Norte")
                             ),
                             column(2,
                                    selectizeInput("meta11a_ept_var", "Variável EPT:", choices = ept_vars, selected = "QT_MAT_PROF_TEC_PROPAG")
                             ),
                             column(2,
                                    selectizeInput("meta11a_med_var", "Variável Ensino Médio:", choices = other_vars, selected = "QT_MAT_MED")
                             ),
                             column(2,
                                    selectInput("meta11a_target_year", "Meta 11a Pública atingida até o ano:",
                                                choices = 2024:2035, selected = 2030)
                             ),
                             column(2,
                                    sliderInput(inputId = "ensino_slope_factor",label = "Crescimento EM projetado:",
                                      min = 0.5,max = 1.5,step = 0.01,value = 1
                                    )
                             ),
                             column(2,
                                    checkboxInput("meta11a_show_profmed", "Mostrar Ensino Médio Profissional (Int. e Conc.)", value = FALSE)
                             )
                             
                             
                              ),
                           
                           plotOutput("meta11a_plot", height = "500px"),
                           br(),
                           h4("Tabela de Dados (EPT, Médio, %)", style = "color: #1f5673; font-weight: bold;"),
                           DTOutput("meta11a_table")
                         )
                ),
                
                
                
                tabPanel("Situação - Meta 11a - Público",
                         fluidPage(
                           h3("Meta 11a: EPT como % do Ensino Médio", style = "color: #1f5673; font-weight: bold;"),
                           
                           fluidRow(
                             column(2,
                                    selectizeInput("meta11a_uf2", "Selecionar UF:", choices = sort(unique(df_censo_UF$NM_UF)), selected = "Rio Grande do Norte")
                             ),
                             column(2,
                                    selectizeInput("meta11a_ept_var2", "Variável EPT:", choices = ept_vars, selected = "QT_MAT_PROF_TEC_PROPAG")
                             ),
                             column(2,
                                    selectizeInput("meta11a_med_va2r", "Variável Ensino Médio:", choices = other_vars, selected = "QT_MAT_MED")
                             ),
                             column(2,
                                    selectInput("meta11a_target_year2", "Meta 11a Pública atingida até o ano:",
                                                choices = 2024:2035, selected = 2030)
                             ),
                             column(2,
                                    sliderInput(inputId = "ensino_slope_factor2",label = "Crescimento EM projetado:",
                                                min = 0.5,max = 1.5,step = 0.01,value = 1
                                    )
                             ),
                             column(2,
                                    checkboxInput("mostrar_expansao_publica",
                                                  "Mostrar PNE11a – Expansão nas redes públicas",
                                                  value = FALSE)
                             )
                             
                           ),
                           
                           plotOutput("meta11a_plot2", height = "500px"),
                           br()
                           
                         )
                ),
                
                
                
                tabPanel("Novo Tab Meta 11a - parte 1",
                         fluidPage(
                           h3("Meta 11a – Comparação das Definições", style = "color: #1f5673; font-weight: bold;"),
                           
                           fluidRow(
                             column(3,
                                    selectizeInput(
                                      inputId = "meta11a_nova_uf",
                                      label = "Selecionar UF:",
                                      choices = sort(unique(meta11a_opcoes$NM_UF)),
                                      selected = "Rio de Janeiro"
                                    )
                             ),
                             column(6,
                                    checkboxGroupInput(
                                      inputId = "meta11a_nova_definicoes",
                                      label = "Escolher definições para comparar:",
                                      choices = c("Meta11a_opcao1", "Meta11a_opcao2", "Meta11a_opcao3"),
                                      selected = c("Meta11a_opcao1", "Meta11a_opcao2", "Meta11a_opcao3"),
                                      inline = TRUE
                                    )
                             )
                           ),
                           
                           plotOutput("meta11a_nova_plot", height = "600px")
                         )
                ),
                
                tabPanel("Painel 7", h4("Placeholder Content for Panel 7")),
                tabPanel("Estatísticas Relevantes", DTOutput("P8_table"))
    )
  )
)


server <- function(input, output, session) {
  
  
  filtered_fin_data_plot <- reactive({
    req(input$fin_variable)
    
    df <- propag_ept_financeiro
    df$valor <- as.numeric(gsub(",", "", df[[input$fin_variable]]))
    
    df$highlight <- ifelse(input$NM_UF != "Todos" & df$UF == input$NM_UF, "Selecionado", "Outros")
    
    df_plot <- df[, c("UF", "Estado", "valor", "highlight")]
    df_plot
  })
  
  financeiro_dt_all <- reactive({
    df <- propag_ept_financeiro
    
    # Remove coluna indesejada
    df <- df[, !(names(df) %in% "fef_share_pct")]
    
    # Identifica colunas a serem somadas (todas exceto UF e Estado)
    data_cols <- setdiff(names(df), c("UF", "Estado"))
    
    # Converte as colunas numéricas
    df[data_cols] <- lapply(df[data_cols], function(x) suppressWarnings(as.numeric(gsub(",", "", x))))
    
    # Cria linha de totais
    total_row <- as.list(rep(NA, ncol(df)))
    names(total_row) <- names(df)
    
    # Preenche totais nas colunas numéricas
    for (col in data_cols) {
      total_row[[col]] <- sum(df[[col]], na.rm = TRUE)
    }
    
    # Preenche identificadores
    total_row$UF <- "Todos"
    total_row$Estado <- "Todos"
    
    # Concatena
    df_final <- rbind(df, as.data.frame(total_row, stringsAsFactors = FALSE))
    return(df_final)
  })
  
  
  
  
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
    # Plot for general states (in millions)
    p_geral <- ggplot(df_geral, aes(x = factor(Estado, levels = df_geral$Estado), y = valor, fill = UF)) +
      geom_col() +
      geom_text(
        aes(label = paste0(format(round(valor / 1e6), big.mark = ".", decimal.mark = ",", scientific = FALSE), " M")),
        angle = 90, vjust = 0.2, hjust=-0.1, size = 5, color = "blue",fontface = "bold"
      ) +
      scale_fill_manual(values = uf_colors) +
      scale_y_continuous(
        limits = y_limits_geral,
        breaks = y_breaks_geral,
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      labs(title = paste("Demais Estados –", plot_label), x = "Estado", y = "Valor (R$)") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", color = "#1f5673"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    # Plot for indebted states (in billions)
    p_divida <- ggplot(df_endividado, aes(x = factor(Estado, levels = df_endividado$Estado), y = valor, fill = UF)) +
      geom_col() +
      geom_text(
        aes(label = paste0(format(round(valor / 1e6), big.mark = ".", decimal.mark = ",", scientific = FALSE), " M")),
        angle = 90, vjust = 0.2, hjust=-0.1, size = 7, color = "blue",fontface = "bold"
      ) +
      scale_fill_manual(values = uf_colors) +
      scale_y_continuous(
        limits = y_limits_divida,
        breaks = y_breaks_divida,
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
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
    df <- financeiro_dt_all()
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
  
  ## PAINEL OFERTA #############################
  ##########################################################################################################################################
  ##########################################################################################################################################
  ##############################################

  
  # --- Reactive filtered dataset for oferta ---
  oferta_data_by_year <- reactive({
    req(input$oferta_uf, input$oferta_ept_var, input$oferta_other_var)
    
    df_filtered <- df_censo_UF |>
      filter(NM_UF == input$oferta_uf) |>
      select(ANO, EPT = all_of(input$oferta_ept_var), ENSINO_MEDIO = all_of(input$oferta_other_var))
    
    base_2013_val <- df_filtered |> filter(ANO == 2013) |> summarise(val = sum(EPT, na.rm = TRUE)) |> pull(val)
    triplo_2013 <- if (!is.na(base_2013_val)) 3 * base_2013_val else NA
    
    df_filtered |> mutate(PNE_META11 = triplo_2013)
  })
  
  
  # --- Line chart output with projection ---
  output$oferta_ept_plot <- renderPlot({
    req(input$oferta_uf, input$oferta_ept_var, input$oferta_other_var)
    
    # Filter and aggregate only "UF_TUDO"
    df_filtered <- df_censo_UF |>
      filter(NM_UF == input$oferta_uf, AGREG == "UF_TUDO") |>
      group_by(ANO) |>
      summarise(
        EPT = sum(.data[[input$oferta_ept_var]], na.rm = TRUE),
        ENSINO_MEDIO = sum(.data[[input$oferta_other_var]], na.rm = TRUE),
        .groups = "drop"
      )
    
    # Compute triplo da base de 2013 como meta PNE
    base_2013_val <- df_filtered |> filter(ANO == 2013) |> pull(EPT)
    triplo_2013 <- if (!is.na(base_2013_val)) 3 * base_2013_val else NA
    
    # Linear model based on 2020–2024 data
    ### BOTTOM
    # Step 1: Fit model to recent trend (2020–2024)
    df_recent <- df_filtered |> filter(ANO %in% 2020:2024)
    linear_model <- lm(EPT ~ ANO, data = df_recent)
    
    # Step 2: Extract slope only
    slope <- coef(linear_model)["ANO"]
    
    # Step 3: Anchor to actual 2024 observed value
    start_val <- df_filtered |> filter(ANO == 2024) |> pull(EPT) |> mean(na.rm = TRUE)
    
    # Step 4: Build future projection using that slope and starting point
    future_years <- 2024:2035
    years_from_start <- future_years - 2024
    
    future_df <- data.frame(
      ANO = future_years,
      VALOR = start_val + slope * years_from_start,
      TIPO = "EPT",
      GRUPO = "PROJECAO"
    )
    ## TOP
    # ---- OUTRA projection from 2024 to 2035 using slope-preserving shift ----
    
    # 1. Fit linear model to OUTRA from 2020 to 2024
    lm_outra <- lm(ENSINO_MEDIO ~ ANO, data = df_filtered |> filter(ANO %in% 2020:2024))
    
    # 2. Extract only the slope (ignore intercept)
    slope_outra <- coef(lm_outra)["ANO"]
    
    # 3. Get the actual observed value in 2024
    start_val_outra <- df_filtered |> 
      filter(ANO == 2024) |> 
      pull(ENSINO_MEDIO) |> 
      mean(na.rm = TRUE)
    
    # 4. Generate projected values for 2024–2035
    future_years <- 2024:2035
    years_from_start <- future_years - 2024
    
    future_outra_df <- data.frame(
      ANO = future_years,
      VALOR = start_val_outra + slope_outra * years_from_start,
      TIPO = "ENSINO_MEDIO",
      GRUPO = "PROJECAO"
    )
    
    
    # Long format for observed values
    df_long <- df_filtered |>
      mutate(GRUPO = "OBSERVADO") |>
      pivot_longer(cols = c("EPT", "ENSINO_MEDIO"), names_to = "TIPO", values_to = "VALOR") |>
      mutate(GRUPO = ifelse(TIPO == "ENSINO_MEDIO", "ENSINO_MEDIO", GRUPO))
    
    df_plot <- bind_rows(
      df_long,          # OBSERVADO and OUTRA (observed)
      future_df,        # EPT PROJECAO
      future_outra_df   # OUTRA PROJECAO
    )
    
    
    # Value of EPT in 2013
    ept_2013 <- df_plot |> 
      filter(ANO == 2013, GRUPO == "OBSERVADO") |> 
      summarise(val = sum(VALOR, na.rm = TRUE)) |> 
      pull(val)
    
    x_label <- 2013
    y_label <- ept_2013 + 5000  # Adjust vertical space as needed
    
    
    # Plot
    ggplot(df_plot, aes(x = ANO, y = VALOR, color = GRUPO)) +
      
      # All observed and projected lines except dotted OUTRA
      geom_line(
        data = df_plot |> filter(!(TIPO == "ENSINO_MEDIO" & GRUPO == "PROJECAO")),
        size = 1.5
      ) +
      
      # Dotted line for OUTRA projection only
      geom_line(
        data = df_plot |> filter(TIPO == "ENSINO_MEDIO" & GRUPO == "PROJECAO"),
        aes(x = ANO, y = VALOR),
        linetype = "longdash",
        linewidth = 1.5,
        color = "#00BFC4"  # consistent with ggplot default for OUTRA
      ) +
      
      # Points for all
      geom_point(size = 2, alpha = 0.8) +
      
      # Meta 11 horizontal line
      geom_hline(yintercept = triplo_2013, linetype = "dashed", color = "darkorange", linewidth = 1.1) +
      annotate(
        "text", x = 2008, y = triplo_2013,
        label = paste0("Meta 11 (Triplo 2013): ", format(triplo_2013, big.mark = ".")),
        color = "darkorange", vjust = -1, fontface = "bold"
      ) +
      
      # Dot on 2013 EPT point
      geom_point(
        data = data.frame(ANO = x_label, VALOR = ept_2013),
        aes(x = ANO, y = VALOR),
        color = "black", size = 3, shape = 21, fill = "white"
      ) +
      
      # Arrow from label to point
      geom_segment(
        data = data.frame(
          x = x_label + 0.6,
          y = y_label,
          xend = x_label,
          yend = ept_2013
        ),
        aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE,
        arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
        linewidth = 0.8,
        color = "black"
      ) +
      
      # Rich label
      ggtext::geom_richtext(
        data = data.frame(x = x_label + 1, y = y_label),
        aes(x = x, y = y, label = paste0("<b>EPT 2013:</b><br>", format(ept_2013, big.mark = "."))),
        fill = "white",
        label.color = "#00bfc4",
        color = "black",
        size = 4,
        label.size = 0.5,
        label.padding = grid::unit(c(4, 6, 4, 6), "pt"),
        label.r = unit(6, "pt"),
        inherit.aes = FALSE
      ) +
      
      # Axis and title formatting
      scale_x_continuous(breaks = 2007:2035, limits = c(2007, 2035)) +
      scale_y_continuous(labels = scales::comma) +
      
      labs(
        title = paste("UF:", input$oferta_uf),
        subtitle = paste(input$oferta_ept_var, "vs", input$oferta_other_var),
        x = "Ano",
        y = "Total de Matrículas"
      ) +
      
      coord_cartesian(clip = "off") +
      
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    
    
    
  })
  
  output$oferta_ept_table <- DT::renderDT({
    req(input$oferta_uf, input$oferta_ept_var, input$oferta_other_var)
    
    # Step 1: Observed data
    df_obs <- df_censo_UF |>
      filter(NM_UF == input$oferta_uf, AGREG == "UF_TUDO") |>
      group_by(ANO) |>
      summarise(
        EPT = sum(.data[[input$oferta_ept_var]], na.rm = TRUE),
        ENSINO_MEDIO = sum(.data[[input$oferta_other_var]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(TIPO = "OBSERVADO")
    
    # Step 2: Projection (2024–2035)
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
    
    # Step 3: Combine all
    df_all <- bind_rows(df_obs, df_proj)
    
    # Step 4: Add PNE Meta 11 (triplo de 2013)
    base_2013_val <- df_all |> filter(ANO == 2013, TIPO == "OBSERVADO") |> pull(EPT)
    triplo_2013 <- if (!is.na(base_2013_val)) 3 * base_2013_val else NA
    df_all$PNE_META11 <- if (!is.na(triplo_2013)) round(triplo_2013) else NA
    
    # Step 5: Transpose safely
    df_transposed <- df_all |>
      select(ANO, TIPO, EPT, ENSINO_MEDIO) |>
      pivot_longer(cols = c(EPT, ENSINO_MEDIO), names_to = "VAR", values_to = "VAL") |>
      unite("ROW", VAR, TIPO, sep = "_") |>
      pivot_wider(names_from = ANO, values_from = VAL) |>
      mutate(across(
        where(is.numeric),
        ~ format(round(.), big.mark = ".", decimal.mark = ",")
      ))
    
    
    # Step 6: Format numerics
    df_transposed <- df_transposed |>
      mutate(across(where(is.numeric), ~ format(., big.mark = ".", decimal.mark = ",")))
    
    # Step 7: Return datatable
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
  
    
    
    
    ## PAINEL META11a ############################# META META METTA WORLD PEAECE
    ##########################################################################################################################################
    ##########################################################################################################################################
    ##############################################
    ######
    meta11a_data <- reactive({
      req(input$meta11a_uf, input$meta11a_ept_var, input$meta11a_med_var)
      
      df <- df_censo_UF |>
        filter(NM_UF == input$meta11a_uf, AGREG == "UF_TUDO") |>
        group_by(ANO) |>
        summarise(
          EPT = sum(.data[[input$meta11a_ept_var]], na.rm = TRUE),
          MEDIO = sum(.data[[input$meta11a_med_var]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(PCT_EPT = ifelse(MEDIO > 0, 100 * EPT / MEDIO, NA))
      
      return(df)
    })
    
    
    
    
    
    
    ###############################################################
    ###############################################################

    meta11a_expansao_publica_hist <- reactive({
      req(input$meta11a_uf, input$meta11a_ept_var)
      
      df_total <- df_censo_UF |>
        filter(NM_UF == input$meta11a_uf, AGREG == "UF_TUDO") |>
        group_by(ANO) |>
        summarise(EPT_TOTAL = sum(.data[[input$meta11a_ept_var]], na.rm = TRUE), .groups = "drop")
      
      df_pub <- df_censo_UF |>
        filter(NM_UF == input$meta11a_uf, AGREG == "UF_PUB") |>
        group_by(ANO) |>
        summarise(EPT_PUB = sum(.data[[input$meta11a_ept_var]], na.rm = TRUE), .groups = "drop")
      
      df_joined <- full_join(df_total, df_pub, by = "ANO") |>
        arrange(ANO) |>
        mutate(
          DELTA_TOTAL = pmax(EPT_TOTAL - lag(EPT_TOTAL), 0),
          DELTA_PUB = pmax(EPT_PUB - lag(EPT_PUB), 0),
          SHARE_PUB = ifelse(DELTA_TOTAL > 0, DELTA_PUB / DELTA_TOTAL, NA)
        ) |>
        filter(!is.na(ANO), ANO >= 2008, ANO <= 2035)  # include up to 2035
      
      return(df_joined)
    })
    
    
    
    
  output$meta11a_plot <- renderPlot({
  df <- meta11a_data() 
  
  # Define anos de base para tendência histórica
  years_trend <- 2020:2024
  
  # Último ano observado
  latest_year <- max(df$ANO, na.rm = TRUE)
  
  # Ano-alvo definido pelo usuário
  target_year <- as.numeric(input$meta11a_target_year)
  
  # Quantos anos faltam até o ano-alvo
  years_left <- target_year - 2024
  
  # Valores de 2024
  current_row <- df |> filter(ANO == 2024)
  current_pct <- current_row$PCT_EPT
  current_ept <- current_row$EPT
  current_med <- current_row$MEDIO
  
  latest_pct <- df |> filter(ANO == latest_year) |> pull(PCT_EPT)
  
  # Estimar tendência linear para o Ensino Médio
  lm_med <- lm(MEDIO ~ ANO, data = df |> filter(ANO %in% years_trend))
  base_slope <- coef(lm_med)["ANO"]
  
  # Aplicar ajuste com o fator do slider
  slope_factor <- input$ensino_slope_factor  # entre 0.5 e 1.5
  adjusted_slope <- base_slope * slope_factor
  
  # Projeção do Ensino Médio no ano-alvo
  projected_med <- current_med + adjusted_slope * years_left
  
  # Quantidade de estudantes necessária para atingir 50% da meta
  target_students <- 0.5 * projected_med
  
  # Crescimento absoluto necessário (em matrículas EPT)
  growth_abs <- if (!is.na(current_ept) && years_left > 0) {
    (target_students - current_ept) / years_left
  } else {
    NA_real_
  }
  
  # Crescimento percentual anual necessário (em pontos percentuais)
  required_growth <- if (!is.na(current_pct) && years_left > 0) {
    (50 - current_pct) / years_left
  } else {
    NA_real_
  }
  
  
  
  # Texto da projeção do Ensino Médio
em_text <- paste0(
  "<b>EM: Projetado</b><br>",
  format(round(projected_med), big.mark = ".", decimal.mark = ","), " alunos<br>matriculados"
)

  
  
  
  
  # Prepare the label
  growth_text <- paste0(
    "<b>Para atingir 50%</b><br>",
    "em ", input$meta11a_target_year, ":<br>",
    "crescimento de<br>",
    "<span style='color:#1f5673'><b>", round(required_growth, 1), "%</b></span> ao ano<br>",
    "(", formatC(growth_abs, format = "d", big.mark = "."), " alunos/ano)"
  )
  
  
  # with checkbox
  
  
  
  
  


  # Base plot: line and points
  gg <- ggplot(df, aes(x = ANO, y = PCT_EPT)) +
    geom_line(color = "#1f5673", size = 1.5) +
    geom_point(color = "#1f5673", size = 2) +

    # Percent label at each point
    ggtext::geom_richtext(
      aes(label = paste0("<b>", round(PCT_EPT, 1), "%</b>")),
      fill = "white", label.color = "#1f5673", color = "black",
      size = 3.5, label.size = 0.3, label.r = unit(4, "pt"),
      label.padding = unit(c(2, 4, 2, 4), "pt"), vjust = -0.8
    ) +

    # Meta 11a fixed line
    geom_hline(yintercept = 50, color = "darkgreen", linetype = "dashed", linewidth = 1.1) +
    annotate("text", x = 2008, y = 50, label = "Meta 11a: 50%", color = "darkgreen", vjust = -1, fontface = "bold")

  # Only show effort line and red dot if below 50%
  if (!is.na(latest_pct) && latest_pct < 50 && target_year > latest_year) {
    # Create one-row df for red dot
    dot_df <- data.frame(ANO = target_year, PCT_EPT = 50)

    gg <- gg +
      geom_segment(
        aes(x = latest_year, y = latest_pct, xend = target_year, yend = 50),
        linetype = "dashed", color = "blue", linewidth = 1.2
      ) +
      geom_point(data = dot_df, aes(x = ANO, y = PCT_EPT),
                 color = "red", size = 4, shape = 21, fill = "white", stroke = 1.5)+
      geom_point(data = dot_df,aes(x = ANO, y = PCT_EPT),
                 color = "red",size = 2.5, shape = 16)+
      ggtext::geom_richtext(
        data = data.frame(x = as.numeric(input$meta11a_target_year), y = 43),
        aes(x = x, y = y, label = growth_text),
        fill = "white",
        label.color = "red",
        color = "black",
        size = 3.5,
        label.size = 0.5,
        label.r = unit(6, "pt"),
        label.padding = unit(c(4, 6, 4, 6), "pt"),
        vjust = 1,
        hjust = 0.5,
        inherit.aes = FALSE
      )+
      # Arrow pointing upward from label to red circle
      geom_segment(
        aes(
          x = target_year,
          xend = target_year,
          y = 49 - 5,         # bottom (label)
          yend = 49             # top (circle)
        ),
        arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
        color = "red",
        linewidth = 0.8
      )+
      ggtext::geom_richtext(
        data = data.frame(x = target_year, y = 60),  # y ajusta altura; mude se quiser mais pra cima
        aes(x = x, y = y, label = em_text),
        inherit.aes = FALSE,
        fill = "white",
        color = "blue",
        label.color = "blue",
        size = 3.5,
        label.size = 0.5,
        label.r = unit(5, "pt"),
        label.padding = unit(c(4, 6, 4, 6), "pt"),
        vjust = 1
      )
    
    
  }

  gg <- gg +
    scale_x_continuous(breaks = 2013:2030, limits = c(2013, 2030)) +
    scale_y_continuous(limits = c(0, 70), labels = scales::percent_format(scale = 1)) +
    labs(
      title = paste("UF:", input$meta11a_uf),
      subtitle = "EPT como % do Ensino Médio",
      x = "Ano",
      y = "% de Matrículas em EPT"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"))
  
  
  
  # Add red circles for QT_MAT_PROF_TEC_MED if checkbox is checked
  if (isTRUE(input$meta11a_show_profmed)) {
    df_profmed <- df_censo_UF |>
      filter(NM_UF == input$meta11a_uf,
             AGREG == "UF_TUDO",
             ANO %in% c(2023, 2024)) |>
      group_by(ANO) |>
      summarise(
        PROFTEC_MED = sum(QT_MAT_PROF_TEC_MED, na.rm = TRUE),
        MEDIO = sum(.data[[input$meta11a_med_var]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        PCT_PROFTEC_MED = ifelse(MEDIO > 0, 100 * PROFTEC_MED / MEDIO, NA)
      ) |>
      filter(!is.na(PCT_PROFTEC_MED))
    
    if (nrow(df_profmed) > 0) {
      # Custom SW/SE offsets per year
      df_profmed <- df_profmed |>
        mutate(
          LABEL_X = case_when(
            ANO == 2023 ~ ANO - 1.5,
            ANO == 2024 ~ ANO + 1.5
          ),
          LABEL_Y = PCT_PROFTEC_MED - 10  # push label below
        )
      
      gg <- gg +
        # Red dots at actual values
        geom_point(
          data = df_profmed,
          aes(x = ANO, y = PCT_PROFTEC_MED),
          shape = 21, size = 6, fill = "red", color = "black", stroke = 1.5
        ) +
        
        # Upward arrows from label to dot
        geom_segment(
          data = df_profmed,
          aes(x = LABEL_X, y = LABEL_Y, xend = ANO, yend = PCT_PROFTEC_MED),
          arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
          color = "red", linewidth = 0.8
        ) +
        
        # Labels placed SW and SE, just below
        ggtext::geom_richtext(
          data = df_profmed,
          aes(
            x = LABEL_X,
            y = LABEL_Y,
            label = paste0(
              "<b>Ensino Prof. Técnico</b><br>",
              "Somente Médio<br>",
              "Int. e Conc.<br>",
              round(PCT_PROFTEC_MED, 1), "%"
            )
          ),
          fill = "white",
          label.color = "red",
          color = "black",
          size = 3.5,
          label.size = 0.5,
          label.r = unit(5, "pt"),
          label.padding = unit(c(4, 6, 4, 6), "pt"),
          vjust = 1,  # anchor top of label to Y pos (so label appears below)
          hjust = 0.5,
          inherit.aes = FALSE
        )
    }
  }
  
  
  
  

return(gg)
  
  
})

    
    #####
    ##RENDER DATATABLE
    ###
    output$meta11a_table <- DT::renderDT({
      df <- meta11a_data()
      req(nrow(df) > 0)
      
      df_fmt <- df |>
        filter(ANO <= 2024) |>
        mutate(
          EPT = format(round(EPT), big.mark = ".", decimal.mark = ","),
          MEDIO = format(round(MEDIO), big.mark = ".", decimal.mark = ","),
          `PCT EPT/Médio` = paste0(round(PCT_EPT, 1), "%")
        ) |>
        select(ANO, EPT, MEDIO, `PCT EPT/Médio`)
      
      datatable(
        df_fmt,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          pageLength = 30,
          dom = 'Bfrtip',
          buttons = list(
            list(extend = "copy", text = "Copiar"),
            list(extend = "csv", filename = "Meta11a_Tabela", text = "CSV"),
            list(extend = "excel", filename = "Meta11a_Tabela", text = "Excel"),
            list(
              extend = "pdf",
              filename = "Meta11a_Tabela",
              text = "PDF",
              orientation = "portrait",
              pageSize = "A4",
              messageTop = "Tabela com dados históricos da Meta 11a (2007–2024)"
            )
          ),
          scrollX = TRUE
        ),
        class = "stripe nowrap display"
      )
      

      
    })
    
  #########################################################?????????????????????????????????????????
    #########################################################?????????????????????????????????????????
    #########################################################?????????????????????????????????????????
    #########################################################?????????????????????????????????????????
    #########################################################?????????????????????????????????????????
    #########################################################?????????????????????????????????????????
    ## REACTIVES
    
    ######
    meta11a_data2 <- reactive({
      req(input$meta11a_uf, input$meta11a_ept_var2, input$meta11a_med_var2)
      
      df <- df_censo_UF |>
        filter(NM_UF == input$meta11a_uf, AGREG == "UF_TUDO") |>
        group_by(ANO) |>
        summarise(
          EPT = sum(.data[[input$meta11a_ept_var2]], na.rm = TRUE),
          MEDIO = sum(.data[[input$meta11a_med_var2]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(PCT_EPT = ifelse(MEDIO > 0, 100 * EPT / MEDIO, NA))
      
      return(df)
    })
    
    
    # Meta 11a second part
    meta11a_expansao_publica_hist2 <- reactive({
      req(input$meta11a_uf, input$meta11a_ept_var2)
      
      df_total <- df_censo_UF |>
        filter(NM_UF == input$meta11a_uf, AGREG == "UF_TUDO") |>
        group_by(ANO) |>
        summarise(EPT_TOTAL = sum(.data[[input$meta11a_ept_var2]], na.rm = TRUE), .groups = "drop")
      
      df_pub <- df_censo_UF |>
        filter(NM_UF == input$meta11a_uf, AGREG == "UF_PUB") |>
        group_by(ANO) |>
        summarise(EPT_PUB = sum(.data[[input$meta11a_ept_var2]], na.rm = TRUE), .groups = "drop")
      
      df_joined <- full_join(df_total, df_pub, by = "ANO") |>
        arrange(ANO) |>
        mutate(
          DELTA_TOTAL = pmax(EPT_TOTAL - lag(EPT_TOTAL), 0),
          DELTA_PUB = pmax(EPT_PUB - lag(EPT_PUB), 0),
          SHARE_PUB = ifelse(DELTA_TOTAL > 0, DELTA_PUB / DELTA_TOTAL, NA)
        ) |>
        filter(!is.na(ANO), ANO >= 2008, ANO <= 2035)  # include up to 2035
      
      return(df_joined)
    })
    
    
    
    output$meta11a_plot2 <- renderPlot({
      df <- meta11a_data() 
      
      # Define anos de base para tendência histórica
      years_trend <- 2020:2024
      
      # Último ano observado
      latest_year <- max(df$ANO, na.rm = TRUE)
      
      # Ano-alvo definido pelo usuário
      target_year <- as.numeric(input$meta11a_target_year)
      
      # Quantos anos faltam até o ano-alvo
      years_left <- target_year - 2024
      
      # Valores de 2024
      current_row <- df |> filter(ANO == 2024)
      current_pct <- current_row$PCT_EPT
      current_ept <- current_row$EPT
      current_med <- current_row$MEDIO
      
      latest_pct <- df |> filter(ANO == latest_year) |> pull(PCT_EPT)
      
      # Estimar tendência linear para o Ensino Médio
      lm_med <- lm(MEDIO ~ ANO, data = df |> filter(ANO %in% years_trend))
      base_slope <- coef(lm_med)["ANO"]
      
      # Aplicar ajuste com o fator do slider
      slope_factor <- input$ensino_slope_factor  # entre 0.5 e 1.5
      adjusted_slope <- base_slope * slope_factor
      
      # Projeção do Ensino Médio no ano-alvo
      projected_med <- current_med + adjusted_slope * years_left
      
      # Quantidade de estudantes necessária para atingir 50% da meta
      target_students <- 0.5 * projected_med
      
      # Crescimento absoluto necessário (em matrículas EPT)
      growth_abs <- if (!is.na(current_ept) && years_left > 0) {
        (target_students - current_ept) / years_left
      } else {
        NA_real_
      }
      
      # Crescimento percentual anual necessário (em pontos percentuais)
      required_growth <- if (!is.na(current_pct) && years_left > 0) {
        (50 - current_pct) / years_left
      } else {
        NA_real_
      }
      
      
      
      # Texto da projeção do Ensino Médio
      em_text <- paste0(
        "<b>EM: Projetado</b><br>",
        format(round(projected_med), big.mark = ".", decimal.mark = ","), " alunos<br>matriculados"
      )
      
      
      
      
      
      # Prepare the label
      growth_text <- paste0(
        "<b>Para atingir 50%</b><br>",
        "em ", input$meta11a_target_year, ":<br>",
        "crescimento de<br>",
        "<span style='color:#1f5673'><b>", round(required_growth, 1), "%</b></span> ao ano<br>",
        "(", formatC(growth_abs, format = "d", big.mark = "."), " alunos/ano)"
      )
      
      
      # with checkbox
      
      
      
      
      
      
      
      # Base plot: line and points
      gg <- ggplot(df, aes(x = ANO, y = PCT_EPT)) +
        
        
        # Meta 11a fixed line
        geom_hline(yintercept = 45, color = "darkgreen", linetype = "dashed", linewidth = 1.1) +
        annotate("text", x = 2008, y = 45, label = "Meta 11a público: 45%", color = "darkgreen", vjust = -1, fontface = "bold")
      

      gg <- gg +
        scale_x_continuous(breaks = 2013:2026, limits = c(2012, 2026)) +
        scale_y_continuous(limits = c(0, 110), labels = scales::percent_format(scale = 1)) +
        labs(
          title = paste("UF:", input$meta11a_uf),
          subtitle = "Expansão no EPT na rede Pública como % da expansão no EPT",
          x = "Ano",
          y = "% de Matrículas em EPT"
        ) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      
      
      
      # Dentro do renderPlot principal
      if (input$mostrar_expansao_publica) {
        df_pub_share <- meta11a_expansao_publica_hist()
        
        gg <- gg +
          geom_point(
            data = df_pub_share,
            aes(x = ANO, y = 100*SHARE_PUB),
            color = "#FF6600",
            size = 5,
            inherit.aes = FALSE
          )+ 
          ggtext::geom_richtext(
            data = df_pub_share |> filter(!is.na(SHARE_PUB)),
            aes(
              x = ANO,
              y = 100 * SHARE_PUB,
              label = paste0(
                "<b>Expansão ", ANO - 1, "–", ANO, "</b><br>",
                "EPT total: ", format(round(DELTA_TOTAL), big.mark = "."), "<br>",
                "EPT pública: ", format(round(DELTA_PUB), big.mark = "."), "<br>",
                "Público: ", round(100 * SHARE_PUB, 1), "%"
              )
            ),
            fill = "white",
            label.color = "#FF6600",
            color = "black",
            size = 3,
            label.size = 0.4,
            label.r = unit(6, "pt"),
            label.padding = unit(c(3, 5, 3, 5), "pt"),
            vjust = -0.5,
            inherit.aes = FALSE
          )
      }
      
      
      return(gg)
      
      
    })
    
############^^&^(^(^(*^(*^(*^(^(*^(*))))))))
    ############^^&^(^(^(*^(*^(*^(^(*^(*))))))))
    ############^^&^(^(^(*^(*^(*^(^(*^(*))))))))
    ############^^&^(^(^(*^(*^(*^(^(*^(*))))))))
    
    output$meta11a_nova_plot <- renderPlot({
      req(input$meta11a_nova_uf, input$meta11a_nova_definicoes)
      
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
          ANO = as.numeric(ANO)  # <-- Corrige o tipo
        )
      
      
      df <- df %>%
        left_join(df_tooltip %>% select(ANO, tooltip_text), by = "ANO")
      
      # Color labels
      
      # Definir cores para as opções
      definicao_colors <- c(
        "Meta11a_opcao1" = "blue",
        "Meta11a_opcao2" = "red",
        "Meta11a_opcao3" = "#594712"
      )
      
      
      df <- df %>%
        mutate(
          color_label = definicao_colors[Definicao]
        )
      
      
      # PROJEÇÕES PARA CADA DEFINIÇÃO DE META 11a
      
      # Agrupar por Definicao e ajustar linha linear
      # Agrupar por Definicao e aplicar projeção com ajuste via slider
      proj_lines <- df %>%
        filter(ANO %in% 2020:2024, !is.na(Meta11a)) %>%
        group_by(Definicao) %>%
        nest() %>%
        mutate(
          model = map(data, ~ lm(Meta11a ~ ANO, data = .x)),
          slope = map_dbl(model, ~ coef(.x)["ANO"]),
          slope_adj = slope * input$ensino_slope_factor,
          intercept = map_dbl(model, ~ coef(.x)["(Intercept)"]),
          start_2024 = map_dbl(data, ~ .x$Meta11a[.x$ANO == 2024]),
    
          proj = map2(slope_adj, start_2024, ~ {
            years_proj <- 2024:as.numeric(input$meta11a_target_year)
            tibble(
              ANO = years_proj,
              Meta11a = .y + .x * (years_proj - 2024)
            )
          })
          
          
        ) %>%
        select(Definicao, proj) %>%
        unnest(proj)
      
      # Adiciona as linhas tracejadas de projeção
      
      
      
      # Gráfico
  gg <- ggplot(df, aes(x = ANO, y = Meta11a, color = Definicao)) +
        geom_line(size = 1.2) +
        geom_point(size = 2.5) +
        geom_hline(yintercept = 0.5, color = "darkgreen", linetype = "dashed", linewidth = 1.1) +
        annotate(
          "text", x = 2025, y = 0.5,
          label = "Meta 11a: EPT forma 50% da matricula EM",size=6,
          color = "darkorange", vjust = -1, fontface = "bold"
        ) +
        ggtext::geom_richtext(
          aes(label = paste0("<b>", scales::percent(Meta11a, accuracy = 0.1), "</b>"),
              color= Definicao),
          label.color = df$color_label, 
          fill = "white",
          size = 4,
          label.size = 0.25,
          label.r = unit(5, "pt"),
          label.padding = unit(c(3, 5, 3, 5), "pt"),
          vjust = -0.8
        )+
      scale_color_manual(values = definicao_colors) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 0.6)) +
        scale_x_continuous(breaks = 2007:2035, limits=c(2007,2035)) +
        labs(
          title = paste("Meta 11a – Comparação para:", input$meta11a_nova_uf),
          x = "Ano",
          y = "Atingimento PNE Meta 11a: Porcentagem Matricula de EPT sobre EM",
          color = "Definição"
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none",
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 14, colour = "blue", face= "bold"),
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "blue")  
                  ) +
        
        ggtext::geom_textbox(
          data = data.frame(x = 2012.5, y = 0.5),  # posição mais ao centro e abaixo do topo
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
          width = unit(0.4, "npc"),        # ajuste fino (50% da largura do gráfico)
          fill = "#fdf6e3",                # bege claro
          box.color = "gray40",            # borda discreta
          halign = 0,                      # texto à esquerda
          color = "black",
          size = 5,
          fontface = "plain",
          lineheight = 1.1,
          box.padding = unit(c(5, 6, 5, 6), "pt"),
          r = unit(6, "pt")                # cantos arredondados
        )
      
  # PROJEÇÕES PARA CADA DEFINIÇÃO DE META 11a
  
  # Agrupar por Definicao e ajustar linha linear
  proj_lines <- df %>%
    filter(ANO >= 2020 & ANO <= 2024) %>%
    group_by(Definicao) %>%
    filter(!is.na(Meta11a)) %>%
    nest() %>%
    mutate(model = map(data, ~ lm(Meta11a ~ ANO, data = .x)),
           last_value = map_dbl(data, ~ .x$Meta11a[.x$ANO == 2024])) %>%
    mutate(proj = map2(model, last_value, ~ {
      years <- 2024:input$meta11a_target_year
      base_pred <- predict(.x, newdata = data.frame(ANO = years))
      shift <- .y - predict(.x, newdata = data.frame(ANO = 2024))  # shift amount
      tibble(
        ANO = years,
        Meta11a = base_pred + shift
      )
    })) %>%
    select(Definicao, proj) %>%
    unnest(proj)
  
  # Adiciona as linhas tracejadas de projeção
  gg <- gg +
    geom_line(
      data = proj_lines,
      aes(x = ANO, y = Meta11a, color = Definicao),
      linetype = "dashed",
      linewidth = 1
    )
  
  return(gg)
    
    })
    
      
      
}  
  

shinyApp(ui = ui, server = server)
