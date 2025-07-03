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


# Load Propag scraped data
propag_ept_financeiro <- readRDS("propag_ept_financeiro.rds")
nome_ufs <- sort(unique(df_codes_ibge$NM_UF))  # Ensure sorted and unique

load("df_censo_UF.rda")



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
    
    tabsetPanel(id = "tab_selection", selected = "Tema Oferta EPT",
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
                
                tabPanel("Tema Oferta EPT",
                         fluidPage(
                           h3("Evolução da Oferta de EPT por UF", style = "color: #1f5673; font-weight: bold;"),
                           fluidRow(
                             column(4,
                                    selectizeInput("oferta_uf", "Selecionar UF (NM_UF):",
                                                   choices = sort(unique(df_codes_ibge$NM_UF)),
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
  
  
  ##############################################
  # --- Load pre-aggregated data ---
  load("df_censo_UF.rda")  # loads df_censo_estados1a
  
  # --- Reactive filtered dataset for oferta ---
  oferta_data_by_year <- reactive({
    req(input$oferta_uf, input$oferta_ept_var, input$oferta_other_var)
    
    df_filtered <- df_censo_UF |>
      filter(NM_UF == input$oferta_uf) |>
      select(ANO, EPT = all_of(input$oferta_ept_var), OUTRA = all_of(input$oferta_other_var))
    
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
        OUTRA = sum(.data[[input$oferta_other_var]], na.rm = TRUE),
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
    lm_outra <- lm(OUTRA ~ ANO, data = df_filtered |> filter(ANO %in% 2020:2024))
    
    # 2. Extract only the slope (ignore intercept)
    slope_outra <- coef(lm_outra)["ANO"]
    
    # 3. Get the actual observed value in 2024
    start_val_outra <- df_filtered |> 
      filter(ANO == 2024) |> 
      pull(OUTRA) |> 
      mean(na.rm = TRUE)
    
    # 4. Generate projected values for 2024–2035
    future_years <- 2024:2035
    years_from_start <- future_years - 2024
    
    future_outra_df <- data.frame(
      ANO = future_years,
      VALOR = start_val_outra + slope_outra * years_from_start,
      TIPO = "OUTRA",
      GRUPO = "PROJECAO"
    )
    
    
    # Long format for observed values
    df_long <- df_filtered |>
      mutate(GRUPO = "OBSERVADO") |>
      pivot_longer(cols = c("EPT", "OUTRA"), names_to = "TIPO", values_to = "VALOR") |>
      mutate(GRUPO = ifelse(TIPO == "OUTRA", "OUTRA", GRUPO))
    
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
    
    x_label <- 2012.5
    y_label <- ept_2013 + 5000  # Adjust vertical space as needed
    
    
    # Plot
    ggplot(df_plot, aes(x = ANO, y = VALOR, color = GRUPO)) +
      geom_line(size = 1.5) +
      geom_point(size = 2, alpha = 0.8) +
      geom_hline(yintercept = triplo_2013, linetype = "dashed", color = "darkorange", linewidth = 1.1) +
      annotate("text", x = 2008, y = triplo_2013,
               label = paste0("Meta 11 (Triplo 2013): ", format(triplo_2013, big.mark = ".")),
               color = "darkorange", vjust = -1, fontface = "bold") +
      scale_x_continuous(breaks = 2007:2035, limits = c(2007, 2035)) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = paste("UF:", input$oferta_uf),
        subtitle = paste(input$oferta_ept_var, "vs", input$oferta_other_var),
        x = "Ano",
        y = "Total de Matrículas"
      ) +
      geom_point(
        data = data.frame(ANO = x_label, VALOR = ept_2013),
        aes(x = ANO, y = VALOR),
        color = "black", size = 3, shape = 21, fill = "white"
      ) +
      # Arrow from label to 2013 point (black straight arrow)
      geom_segment(
        aes(x = x_label + 0.6, y = y_label, xend = x_label, yend = ept_2013),
        inherit.aes = FALSE,
        arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
        linewidth = 0.8,
        color = "black"
      ) +
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
    theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    
    
  })
  
  
  # --- Table output ---
  output$oferta_ept_table <- DT::renderDT({
    df <- df_censo_UF |>
      filter(NM_UF == input$oferta_uf, AGREG == "UF_TUDO") |>
      group_by(ANO) |>
      summarise(
        EPT = sum(.data[[input$oferta_ept_var]], na.rm = TRUE),
        OUTRA = sum(.data[[input$oferta_other_var]], na.rm = TRUE),
        .groups = "drop"
      )
    
    base_2013_val <- df |> filter(ANO == 2013) |> pull(EPT)
    triplo_2013 <- if (!is.na(base_2013_val)) 3 * base_2013_val else NA
    
    df$PNE_META11 <- if (!is.na(triplo_2013)) format(round(triplo_2013), big.mark = ".", decimal.mark = ",") else NA
    df$EPT <- format(round(df$EPT), big.mark = ".", decimal.mark = ",")
    df$OUTRA <- format(round(df$OUTRA), big.mark = ".", decimal.mark = ",")
    
    datatable(df, rownames = FALSE, options = list(pageLength = 30))
  })
  
}  
  

shinyApp(ui = ui, server = server)
