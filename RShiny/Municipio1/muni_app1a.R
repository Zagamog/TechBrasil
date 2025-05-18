options(scipen = 999)

library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(scales)
library(RSQLite)

# Define path to your SQLite database
db_path <- "D:/Country/Brazil/TechBrazil/working/sqlite/TB_municipios.sqlite"

# Define a predefined color palette for regions and states
region_state_colors <- c(
  "Norte" = "#ff7f0e", "Nordeste" = "#2ca02c", "Sudeste" = "#d62728",
  "Sul" = "#9467bd", "Centro-Oeste" = "#8c564b", "Amazonia_Legal" = "#8c564b",
  "Nordeste_r" = "#e377c2", "Centro-Oeste_r" = "#7f7f7f",
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
      "Análise Econômica por Região e Município" # "Economic Analysis by Region and Municipality"
    ),
    div(
      style = "font-size: 18px; font-weight: normal; color: #555;",
      "Projeções do Produto Interno Bruto (PIB)" # "Gross Domestic Product (GDP) Projections"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "localInput",
        label = "Selecionar Local(is):", # "Select Location(s):"
        choices = c("Nome da Grande Região", "Nome da Unidade da Federação", "Nome do Município"),
        options = list(`actions-box` = TRUE),
        multiple = FALSE, # Changed to FALSE
        selected = "Nome da Grande Região"
      ),
      pickerInput(
        "municipioInput",
        label = "Selecionar Município(s):", # "Select Municipality(s):"
        choices = NULL,  # Will be updated dynamically
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      ),
      radioButtons(
        "yAxisVariable",
        label = "Selecionar Variável do Eixo Y:", # "Choose Y-axis Variable:"
        choices = c(
          "Valor adicionado bruto total, \na preços correntes\n(R$ 1.000)" = "Valor adicionado bruto total, \na preços correntes\n(R$ 1.000)",
          "Produto Interno Bruto, \na preços correntes\n(R$ 1.000)" = "Produto Interno Bruto, \na preços correntes\n(R$ 1.000)",
          "Produto Interno Bruto per capita, \na preços correntes\n(R$ 1,00)"   = "Produto Interno Bruto per capita, \na preços correntes\n(R$ 1,00)"
        ),
        selected = "Produto Interno Bruto, \na preços correntes\n(R$ 1.000)"
      ),
      sliderInput(
        "projectionYears",
        label = "Anos para Projeção:", # "Years to Project:"
        min = 2002,
        max = 2021,
        value = 5,
        step = 1
      )
    ),
    mainPanel(
      plotlyOutput("economicPlot", height = "75vh"),
      HTML("<p style='font-size: 12px; color: #555; margin-top: 10px;'>
             Fonte: <a href='https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9057-produto-interno-bruto-dos-municipios.html'
             target='_blank'>IBGE - Produto Interno Bruto dos Municípios</a>
           </p>") # Source
    )
  )
)

# Server
server <- function(input, output, session) {
  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  # Function to fetch data from the database
  fetch_data <- function(query) {
    dbGetQuery(con, query)
  }
  
  # Dynamically update the list of municipalities based on the selected region/state
  observeEvent(input$localInput, {
    selected_location_type <- input$localInput # corrected variable name
    
    if (!is.null(selected_location_type)) {
      
      # Construct the query to fetch distinct locations based on user selection
      query <- if (selected_location_type == "Nome da Grande Região") {
        "SELECT DISTINCT `Nome da Grande Região` FROM PIB_dos_Municipios"
      } else if (selected_location_type == "Nome da Unidade da Federação") {
        "SELECT DISTINCT `Nome da Unidade da Federação` FROM PIB_dos_Municipios"
      } else {
        "SELECT DISTINCT `Nome do Município` FROM PIB_dos_Municipios"
      }
      
      locations_df <- fetch_data(query)
      locations <- locations_df[[1]]  # Extract the vector of names
      
      # Update the choices in the municipalityInput
      updatePickerInput(session, "municipioInput", choices = locations, selected = head(locations, 1)) # Select the first item by default
      
    } else {
      updatePickerInput(session, "municipioInput", choices = NULL, selected = NULL)
    }
  })
  
  # Render the plot
  output$economicPlot <- renderPlotly({
    req(input$municipioInput, input$yAxisVariable, input$localInput) # Added input$localInput
    
    # Construct the SQL query to fetch the relevant data
    location_type = input$localInput
    municipio_filter <- paste0("'", input$municipioInput, "'", collapse = ", ")
    
    query <- paste("SELECT Ano, `Nome do Município`, `Nome da Grande Região`, `Nome da Unidade da Federação`, `", input$yAxisVariable, "` FROM PIB_dos_Municipios ",
                   "WHERE `", location_type, "` IN (", municipio_filter, ")") # Use location_type
    data <- fetch_data(query)
    
    # Convert 'Ano' to numeric
    data$Ano <- as.numeric(data$Ano)
    
    # Add projections
    last_year <- max(data$Ano)
    projection_years <- seq(last_year + 1, last_year + input$projectionYears)
    
    # Create a data frame for the projections (for each selected municipality)
    projection_data <- data.frame(
      Ano = rep(projection_years, length(input$municipioInput)),
      `Nome do Município` = rep(input$municipioInput, each = length(projection_years)),
      `Nome da Grande Região` = rep(unique(data$`Nome da Grande Região`), each = length(projection_years)), # Include Região
      `Nome da Unidade da Federação` =  rep(unique(data$`Nome da Unidade da Federação`), each = length(projection_years)),
      predicted = NA  # Initialize with NA, will be filled in the loop
    )
    
    # Combine original data and projection data
    plot_data <- rbind(data, projection_data)
    
    # Perform linear regression for each municipality and predict future values
    for (municipio in input$municipioInput) {
      municipio_data <- data[data$`Nome do Município` == municipio, ]
      
      # Check if there are enough data points for regression
      if (nrow(municipio_data) > 1) {
        model <- lm(as.formula(paste("`", input$yAxisVariable, "` ~ Ano")), data = municipio_data)
        
        # Predict for the projection years
        predicted_values <- predict(model, newdata = data.frame(Ano = projection_years))
        
        # Store the predicted values in the combined dataframe
        plot_data[plot_data$`Nome do Município` == municipio & plot_data$Ano %in% projection_years, "predicted"] <- predicted_values
      }
    }
    
    # Determine y-axis label
    y_label <- input$yAxisVariable
    
    # Create the plot
    p <- ggplot(plot_data, aes(x = Ano, y = get(input$yAxisVariable), 
                               color = switch(input$localInput,
                                              "Nome da Grande Região" = `Nome da Grande Região`,
                                              "Nome da Unidade da Federação" = `Nome da Unidade da Federação`,
                                              "Nome do Município" = `Nome do Município`))) + # Dynamic color
      geom_line(data = plot_data %>% filter(!is.na(get(input$yAxisVariable))), linewidth = 1) + # Original data
      geom_line(data = plot_data %>% filter(!is.na(predicted)), aes(y = predicted), linetype = "dashed", linewidth = 1) + # Projections
      geom_point(data = plot_data %>% filter(!is.na(get(input$yAxisVariable))), size = 1) +
      geom_point(data = plot_data %>% filter(!is.na(predicted)), aes(y = predicted), shape = 1) +
      scale_color_manual(values = region_state_colors) +
      labs(
        title = "Projeções do PIB por Município", # "GDP Projections by Municipality"
        x = "Ano", # "Year"
        y = y_label,
        color = input$localInput  # Dynamic color label
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom"
      ) +
      scale_x_continuous(breaks = unique(plot_data$Ano))
    
    # Convert to interactive Plotly plot
    plotly_obj <- ggplotly(p)
    
    return(plotly_obj)
  })
  
  # Disconnect from the database when the app is closed
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
