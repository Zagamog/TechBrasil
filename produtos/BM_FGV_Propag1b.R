# BM_FGV_Propag1b.R
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(DT)
library(scales)
library(patchwork)

# Load IBGE codes
load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")

# Load Propag scraped data
propag_ept_financeiro <- readRDS("D:/Country/Brazil/TechBrazil/working/mec/propag_ept_financeiro.rds")

nome_ufs <- sort(unique(df_codes_ibge$NM_UF))  # Ensure sorted and unique


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


ui <- dashboardPage(
  dashboardHeader(title = HTML('Adira à <span style="color: #ffcc00;">Propag</span> !')),
  dashboardSidebar(
    #       selectizeInput(
    #   "NM_UF",
    #   "Selecionar Unidade da Federação",
    #   choices = c("Todos", nome_ufs),
    #   selected = "AL"
    # ),
    
 
    
    # In your UI:
    selectizeInput(
      "fin_variable",
      "Selecionar variável financeira",
      choices = fin_choices,
      selected = "FEF_5ano_liq_cen01"
    ),
    
    
    
    
    br(), br(),  # Add some spacing
    
    ## Contact Info Box
    div(
      style = "position: absolute; bottom: 20px; width: 100%; padding: 10px; font-size: 12px; color: #ccc;",
      HTML(
        "<strong>Para dúvidas ou consultas:</strong><br>
     Envie mensagem para:<br>
     <strong>Equipe FGV/DGPE:</strong> <a href='mailto:blix@fgv.br' style='color:#cccccc;'>blix@fgv.br</a><br>
     <strong>Equipe BM:</strong> <a href='mailto:blax@worldbank.org' style='color:#cccccc;'>blax@worldbank.org</a>"
      )
    )
    
    
  ),
  
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    
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
    
    tabsetPanel(id = "tab_selection",
                
                tabPanel("Tema Financiero",
                         fluidPage(
                           h3("Visualização Financeira do PROPAG", style = "color: #1f5673; font-weight: bold;"),
                           
                           
                           plotOutput("tab1_fin_plot", height = "600px"),
                           
                           br(),
                           
                           DT::dataTableOutput("tab1_fin_table")
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
                tabPanel("Tema Oferta EPT", h4("Placeholder Content for Panel 3")),
                tabPanel("Painel 4", h4("Placeholder Content for Panel 4")),
                tabPanel("Painel 5", h4("Placeholder Content for Panel 5")),
                tabPanel("Painel 6", h4("Placeholder Content for Panel 6")),
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
    
    # Convert all numeric-looking columns to numeric
    numeric_cols <- names(df)[sapply(df, function(x) all(grepl("^[0-9,.]+$", x[!is.na(x)])))]
    df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(gsub(",", "", x)))
    
    # Create total row with NA for character columns
    total_row <- as.list(rep(NA, ncol(df)))
    names(total_row) <- names(df)
    
    # Fill in totals for numeric columns
    for (col in numeric_cols) {
      total_row[[col]] <- sum(df[[col]], na.rm = TRUE)
    }
    
    # Fill in identifiers
    total_row$UF <- "Todos"
    total_row$Estado <- "Todos"
    
    # Convert to data.frame before binding
    total_row <- as.data.frame(total_row, stringsAsFactors = FALSE)
    df_final <- rbind(df, total_row)
    
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
  
  
  
  
  
  
  
  
  
  
  
  output$tab1_fin_table <- DT::renderDataTable({
    df <- financeiro_dt_all()
    
    # Identify numeric-looking columns and convert
    num_cols <- names(df)[sapply(df, function(x) all(grepl("^[0-9,.]+$", x[!is.na(x)])))]
    df[num_cols] <- lapply(df[num_cols], function(x) as.numeric(gsub(",", "", x)))
    
    # Drop the `fef_share_pct` column
    df <- df[, !(names(df) %in% c("fef_share_pct"))]
    
    DT::datatable(
      df,
      options = list(pageLength = 30, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = names(df)[sapply(df, is.numeric)], digits = 0)
  })
  
  
}

shinyApp(ui = ui, server = server)
