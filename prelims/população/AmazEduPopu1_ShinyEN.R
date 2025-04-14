options(scipen = 999)

library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(scales)
library(here)

# Load the data dynamically based on the project root
load(here("working", "ibge", "pop01_70b.rda"))

# Define column groups
number_columns <- c("POP_T", "0-14_T", "15-17_T", "18-21_T", "15-59_T", "60+_T")
proportion_columns <- c("P_0_14_T", "P_15_17_T", "P_18_21_T", "P_15_59_T", "P_60_plus_T")

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

# UI
ui <- fluidPage(
  div(
    style = "text-align: center; margin-bottom: 20px;",
    div(
      style = "font-size: 28px; font-weight: bold; color: #333;",
      "Brazil: Demographic Transition of Age-Groups by Region and State"
    ),
    div(
      style = "font-size: 18px; font-weight: normal; color: #555;",
      "Crisis or Opportunity for the Legal Amazon?"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "localInput",
        label = "Select Location(s):",
        choices = names(local_colors),
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = "Brasil"
      ),
      radioButtons(
        "dataType",
        label = "Choose Data Type:",
        choices = list("Population Numbers" = "numbers", "Population Proportions" = "proportions"),
        selected = "numbers"
      ),
      uiOutput("variableInput"),
      checkboxInput(
        "showTransition",
        label = "Show Demographic Transition Line (Point where declining 0-14 age group population crosses increasing 60+ age group population)",
        value = FALSE
      )
    ),
    mainPanel(
      plotlyOutput("linePlot", height = "75vh"),
      HTML("<p style='font-size: 12px; color: #555; margin-top: 10px;'>
        Source: <a href='https://www.ibge.gov.br/estatisticas/sociais/populacao/9109-projecao-da-populacao.html' 
        target='_blank'>IBGE Population Projections</a>
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
        label = "Select Y-axis Variable(s):",
        choices = number_columns,
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = c("0-14_T", "60+_T")
      )
    } else {
      pickerInput(
        "yVariables",
        label = "Select Y-axis Variable(s):",
        choices = proportion_columns,
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = c("P_0_14_T", "P_60_plus_T")
      )
    }
  })
  
  output$linePlot <- renderPlotly({
    req(input$yVariables)
    
    # Filter data based on selected locations
    filtered_data <- pop01_70b %>%
      filter(LOCAL %in% input$localInput)
    
    # Determine y-axis labels and limits
    y_labels <- if (input$dataType == "numbers") scales::comma else waiver()
    y_min <- 0
    y_max <- max(filtered_data[input$yVariables], na.rm = TRUE)
    
    # Create the base ggplot object
    p <- ggplot(filtered_data, aes(x = ANO, color = LOCAL)) +
      labs(
        x = "Year",
        y = ifelse(input$dataType == "numbers", "Population Count", "Proportion"),
        title = "IBGE Population Projections 2000-2070",
        color = "Location"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,size=14),
        axis.text.y = element_text(size = 14)
      ) +
      scale_y_continuous(limits = c(y_min, y_max), labels = y_labels) +
      scale_x_continuous(breaks = seq(2000, 2070, by = 10)) +
      scale_color_manual(values = local_colors)
    
    # Line types and Plotly annotations
    line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    annotations <- list()
    
    # Add lines and annotations for each selected y-variable
    # Loop through each selected LOCAL and y-variable to add annotations
    for (loc in unique(filtered_data$LOCAL)) {
      loc_data <- filtered_data %>% filter(LOCAL == loc)
      loc_color <- local_colors[loc]
      
      for (i in seq_along(input$yVariables)) {
        y_var <- input$yVariables[i]
        y_sym <- sym(y_var)
        line_type <- line_types[(i - 1) %% length(line_types) + 1]
        
        # Add the line for each y-variable and LOCAL
        p <- p + geom_line(data = loc_data, aes(y = !!y_sym), linetype = line_type, linewidth = 1, color = loc_color)
        
        # Get the last year and value for labeling
        last_year <- max(loc_data$ANO)
        last_value <- loc_data %>%
          filter(ANO == last_year) %>%
          pull(!!y_sym)
        
        # Construct the label text
        label_text <- paste(y_var, "-", loc)
        
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
            font = list(color = "black", size = 20, family = "Arial")
          )
        ))
      }
    }
    
    
    # Add demographic transition line if checkbox is selected
    if (input$showTransition) {
      crossover_data <- filtered_data %>% filter(Crossover_Flag == 1)
      if (nrow(crossover_data) > 0) {
        for (loc in unique(crossover_data$LOCAL)) {
          crossover_year <- crossover_data %>% filter(LOCAL == loc) %>% pull(ANO)
          crossover_value <- if (input$dataType == "numbers") {
            crossover_data %>% filter(LOCAL == loc) %>% pull(Crossover_Value_Num)
          } else {
            crossover_data %>% filter(LOCAL == loc) %>% pull(Crossover_Value_Prop)
          }
          loc_color <- local_colors[loc]
          
          # Create a Plotly annotation for the demographic transition point
          annotations <- append(annotations, list(
            list(
              x = crossover_year,
              y = crossover_value,
              text = paste(crossover_year, loc),
              showarrow = TRUE,
              arrowhead = 2,
              ax = 0,  # Adjust arrow position to the left
              ay = 40,
              font = list(color = "black", size = 20, family = "Arial")
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
