# AmazEduPopu2_ShinyPT.R

options(scipen = 999)

library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(scales)
library(here)

# Load the data
load("meta11a_opcoes.rda")
load("pop01_70b.rda")

# Data preparation before UI
# Select needed columns from population data and create 15-19 age group
pop_data <- pop01_70b %>%
  select(ANO, SIGLA, LOCAL, `15-17_T`, `18-21_T`) %>%
  mutate(`15-19_T` = `15-17_T` + (`18-21_T` * 0.5)) %>%
  filter(ANO >= 2007 & ANO <= 2035)

# Select needed columns from enrollment data
enrollment_data <- meta11a_opcoes %>%
  select(ANO, SG_UF, NM_UF, QT_MAT_PROF_TEC_PROPAG, QT_MAT_MED) %>%
  filter(ANO >= 2007 & ANO <= 2035)

# Join the datasets
combined_data <- pop_data %>%
  left_join(enrollment_data, by = c("SIGLA" = "SG_UF", "ANO" = "ANO")) %>%
  # Use LOCAL from pop_data if NM_UF is missing
  mutate(LOCAL = ifelse(is.na(NM_UF), LOCAL, NM_UF)) %>%
  # Calculate percentages
  mutate(
    PCT_MAT_EPT = ifelse(`15-19_T` > 0, (QT_MAT_PROF_TEC_PROPAG / `15-19_T`) * 100, 0),
    PCT_MAT_MED = ifelse(`15-19_T` > 0, (QT_MAT_MED / `15-19_T`) * 100, 0)
  ) %>%
  select(-NM_UF, -`15-17_T`, -`18-21_T`)

# Define the variables available for plotting
number_variables <- c(
  "Pop 15-19" = "15-19_T",
  "Matrícula EPT" = "QT_MAT_PROF_TEC_PROPAG",
  "Matrícula Ensino Médio" = "QT_MAT_MED"
)

percentage_variables <- c(
  "Pop 15-19" = "15-19_T",
  "% Matrícula EPT" = "PCT_MAT_EPT",
  "% Matrícula Ensino Médio" = "PCT_MAT_MED"
)

# Define a predefined color palette for each LOCAL
local_colors <- c(
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

# Get available locations from the combined data
available_locations <- unique(combined_data$LOCAL)
available_locations <- available_locations[!is.na(available_locations)]

# UI
ui <- fluidPage(
  div(
    style = "text-align: center; margin-bottom: 20px;",
    div(
      style = "font-size: 28px; font-weight: bold; color: #333;",
      "Brasil: População EPT (15-19 anos) vs Matrículas por Estado"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "localInput",
        label = "Selecionar Localização(ões):",
        choices = available_locations,
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = if("Brasil" %in% available_locations) "Brasil" else available_locations[1]
      ),
      radioButtons(
        "dataType",
        label = "Escolher Tipo de Dados:",
        choices = list("Números Absolutos" = "numbers", "Percentagens" = "percentages"),
        selected = "numbers"
      ),
      uiOutput("variableInput")
    ),
    mainPanel(
      plotlyOutput("linePlot", height = "75vh"),
      HTML("<p style='font-size: 12px; color: #555; margin-top: 10px;'>
        Fonte: <a href='https://www.ibge.gov.br/estatisticas/sociais/populacao/9109-projecao-da-populacao.html' 
        target='_blank'>Projeções Populacionais IBGE</a> e Censo Escolar
       </p>")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dynamically update the variable input based on selected data type
  output$variableInput <- renderUI({
    if (input$dataType == "numbers") {
      pickerInput(
        "yVariables",
        label = "Selecionar Variável(eis) do Eixo Y:",
        choices = number_variables,
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = c("15-19_T", "QT_MAT_PROF_TEC_PROPAG")
      )
    } else {
      pickerInput(
        "yVariables",
        label = "Selecionar Variável(eis) do Eixo Y:",
        choices = percentage_variables,
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = c("15-19_T", "PCT_MAT_EPT")
      )
    }
  })
  
  output$linePlot <- renderPlotly({
    req(input$yVariables, input$localInput)
    
    # Filter data based on selected locations
    filtered_data <- combined_data %>%
      filter(LOCAL %in% input$localInput)
    
    # Get the current variable choices based on data type
    current_variables <- if (input$dataType == "numbers") number_variables else percentage_variables
    
    # Determine y-axis limits and labels
    y_min <- 0
    y_max <- max(filtered_data[input$yVariables], na.rm = TRUE)
    y_labels <- if (input$dataType == "numbers") scales::comma else function(x) paste0(x, "%")
    
    # Create the base ggplot object
    p <- ggplot(filtered_data, aes(x = ANO, color = LOCAL)) +
      labs(
        x = "Ano",
        y = if (input$dataType == "numbers") "Contagem" else "Percentagem (%)",
        title = if (input$dataType == "numbers") {
          "População 15-19 anos e Matrículas EPT/Ensino Médio (2007-2035)"
        } else {
          "População 15-19 anos e Percentagem de Matrículas EPT/Ensino Médio (2007-2035)"
        },
        color = "Localização"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)
      ) +
      scale_y_continuous(limits = c(y_min, y_max), labels = y_labels) +
      scale_x_continuous(breaks = seq(2007, 2035, by = 5)) +
      scale_color_manual(values = local_colors)
    
    # Line types and Plotly annotations
    line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    annotations <- list()
    
    # Add lines and annotations for each selected y-variable
    for (loc in unique(filtered_data$LOCAL)) {
      loc_data <- filtered_data %>% filter(LOCAL == loc)
      loc_color <- local_colors[loc]
      if(is.na(loc_color)) loc_color <- "#000000"  # fallback color
      
      for (i in seq_along(input$yVariables)) {
        y_var <- input$yVariables[i]
        y_sym <- sym(y_var)
        line_type <- line_types[(i - 1) %% length(line_types) + 1]
        
        # Add the line for each y-variable and LOCAL
        p <- p + geom_line(data = loc_data, aes(y = !!y_sym), linetype = line_type, linewidth = 1, color = loc_color)
        
        # Get the last year and value for labeling
        last_year <- max(loc_data$ANO, na.rm = TRUE)
        last_value <- loc_data %>%
          filter(ANO == last_year) %>%
          pull(!!y_sym)
        
        if(length(last_value) > 0 && !is.na(last_value)) {
          # Get the display name for the variable
          var_display_name <- names(current_variables)[current_variables == y_var]
          
          # Construct the label text
          label_text <- paste(var_display_name, "-", loc)
          
          # Add Plotly annotation for the label
          annotations <- append(annotations, list(
            list(
              x = last_year,
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
    
    # Convert to interactive Plotly plot and add annotations
    plotly_obj <- ggplotly(p)
    plotly_obj <- plotly_obj %>% layout(annotations = annotations)
    
    return(plotly_obj)
  })
}

# Run the application
shinyApp(ui = ui, server = server)