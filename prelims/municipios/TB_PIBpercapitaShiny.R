# TB_PIBpercapitaShiny.R
# This script creates a Shiny app to visualize Brazilian GDP (PIB) data
# by geographical aggregates (Brazil, Major Regions, States).

options(scipen = 999) # To prevent scientific notation on large numbers

library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(scales)
library(here) # For managing file paths if you save df_pib

# --- Data Loading and Preparation ---
# Load the processed df_pib directly into memory before UI/Server definitions.
# This ensures column names are as they are in the loaded data.
load("D:/Country/Brazil/TechBrazil/working/ibge/TB_municipios.rda")

# --- IMPORTANT: Ensure numeric conversion happened correctly before saving TB_municipios.rda ---
# If your TB_municipios.rda was saved *before* the numeric conversion loop in TB_municipios1a.R,
# you will need to run the conversion here. Otherwise, the data will be character/factor.
# Assuming you've already ensured the conversion happened in TB_municipios1a.R before saving.

# Get the exact column names after loading
all_df_pib_names <- names(df_pib)

# Define column names for numerical variables using their *exact* positions and names from the loaded df_pib
# Based on your TB_municipios1a.txt, these are the columns from index 33 to 40
pib_value_columns <- all_df_pib_names[33:40] # Use exact names from loaded df_pib

# Define the geographical columns by their exact names
nome_grande_regiao_col <- all_df_pib_names[3] # "Nome da Grande Região"
nome_uf_col <- all_df_pib_names[6] # "Nome da Unidade da Federação"
ano_col <- all_df_pib_names[1] # "Ano"


# --- Create "Brasil" aggregate ---
# Fix: Use anonymous function for `across` to avoid deprecation warning
df_pib_brasil <- df_pib %>%
  group_by(!!sym(ano_col)) %>% # Dynamically refer to 'Ano' column
  summarise(
    across(all_of(pib_value_columns), \(x) sum(x, na.rm = TRUE)), # Fix: Use \(x) sum(x, ...)
    # Assign placeholder names using the exact column names
    !!sym(nome_grande_regiao_col) := "Brasil",
    !!sym(nome_uf_col) := "Brasil",
    .groups = "drop" # Good practice for summarise
  )

# Combine Brasil aggregate with the original data
# Ensure consistent column names and structure for combining
df_pib_processed <- bind_rows(
  df_pib_brasil %>%
    select(!!sym(ano_col), !!sym(nome_grande_regiao_col), !!sym(nome_uf_col), all_of(pib_value_columns)),
  df_pib %>%
    select(!!sym(ano_col), !!sym(nome_grande_regiao_col), !!sym(nome_uf_col), all_of(pib_value_columns))
)

# Define choices for location picker
# Get unique regions and states, plus 'Brasil'
# Ensure these match the exact column names from df_pib_processed
location_choices <- c("Brasil",
                      unique(df_pib_processed[[nome_grande_regiao_col]][df_pib_processed[[nome_grande_regiao_col]] != "Brasil"]),
                      unique(df_pib_processed[[nome_uf_col]][df_pib_processed[[nome_uf_col]] != "Brasil"]))
location_choices <- sort(unique(location_choices)) # Sort for cleaner UI

# Define a predefined color palette for each geographical unit
# Extend your color palette from AmazEduPopu1_ShinyEN.R if needed, or define a new one.
# For simplicity, I'll use a generic palette here. You might want to assign specific colors.
pib_colors <- scales::hue_pal()(length(location_choices))
names(pib_colors) <- location_choices

# --- UI ---
ui <- fluidPage(
  div(
    style = "text-align: center; margin-bottom: 20px;",
    div(
      style = "font-size: 28px; font-weight: bold; color: #333;",
      "Brazil: Gross Domestic Product (PIB) Trends by Region and State"
    ),
    div(
      style = "font-size: 18px; font-weight: normal; color: #555;",
      "Analysis of Sectoral and Per Capita PIB"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "localInput",
        label = "Select Location(s):",
        choices = location_choices,
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = "Brasil"
      ),
      pickerInput(
        "yVariable",
        label = "Select PIB Variable:",
        choices = pib_value_columns,
        options = list(`actions-box` = FALSE), # Only one variable for Y-axis
        selected = pib_value_columns[7] # Default to "Produto Interno Bruto, a preços correntes (R$ 1.000)"
      )
    ),
    mainPanel(
      plotlyOutput("pibLinePlot", height = "75vh"),
      HTML("<p style='font-size: 12px; color: #555; margin-top: 10px;'>
        Source: <a href='https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html'
        target='_blank'>IBGE Produto Interno Bruto dos Municípios</a>
       </p>")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  output$pibLinePlot <- renderPlotly({
    req(input$localInput, input$yVariable)
    
    # Filter data based on selected locations
    filtered_data <- df_pib_processed %>%
      filter(
        (input$localInput == "Brasil" & !!sym(nome_grande_regiao_col) == "Brasil") |
          (!!sym(nome_grande_regiao_col) %in% input$localInput & !!sym(nome_grande_regiao_col) != "Brasil") |
          (!!sym(nome_uf_col) %in% input$localInput & !!sym(nome_uf_col) != "Brasil")
      )
    
    # If "Brasil" is NOT selected, explicitly remove the Brasil aggregate row
    if (!("Brasil" %in% input$localInput)) {
      filtered_data <- filtered_data %>% filter(!!sym(nome_grande_regiao_col) != "Brasil")
    }
    
    # Prepare data for plotting, mapping selected location to a 'Display_Location' column
    plot_data <- filtered_data %>%
      mutate(Display_Location = case_when(
        # Prioritize matching exact location selected by user
        !!sym(nome_grande_regiao_col) %in% input$localInput & !!sym(nome_grande_regiao_col) == "Brasil" ~ "Brasil", # Handle Brasil specifically
        !!sym(nome_grande_regiao_col) %in% input$localInput ~ !!sym(nome_grande_regiao_col),
        !!sym(nome_uf_col) %in% input$localInput ~ !!sym(nome_uf_col),
        TRUE ~ "Unknown Location" # Fallback if none match (shouldn't happen with correct choices)
      )) %>%
      # Ensure distinct data points for plotting if any duplicates arise from filtering logic
      distinct(!!sym(ano_col), Display_Location, .keep_all = TRUE) %>%
      # Filter out 'Unknown Location' if it appears due to filtering logic not perfectly aligning
      filter(Display_Location != "Unknown Location")
    
    
    # Determine y-axis labels
    y_labels <- scales::comma # All PIB values are numbers, use comma formatting
    
    # Create the base ggplot object
    p <- ggplot(plot_data, aes(x = !!sym(ano_col), color = Display_Location)) +
      labs(
        x = "Year",
        y = input$yVariable, # Dynamic Y-axis label
        title = paste("Brazilian PIB Trends (2002-2021) -", input$yVariable),
        color = "Location"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)
      ) +
      scale_y_continuous(labels = y_labels) +
      scale_x_continuous(breaks = unique(plot_data[[ano_col]])) + # Show all available years
      scale_color_manual(values = pib_colors)
    
    # Add lines for the selected variable
    y_sym <- sym(input$yVariable)
    p <- p + geom_line(aes(y = !!y_sym), linewidth = 1) +
      geom_point(aes(y = !!y_sym), size = 2) # Add points for clarity
    
    # Add Plotly annotations (similar to your population app)
    annotations <- list()
    for (loc_display in unique(plot_data$Display_Location)) {
      loc_data_subset <- plot_data %>% filter(Display_Location == loc_display)
      last_year <- max(loc_data_subset[[ano_col]])
      last_value <- loc_data_subset %>%
        filter(!!sym(ano_col) == last_year) %>%
        pull(!!y_sym)
      
      # Construct the label text
      label_text <- paste(loc_display, "-", scales::comma(last_value, accuracy = 1))
      
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
    
    plotly_obj <- ggplotly(p)
    plotly_obj <- plotly_obj %>% layout(annotations = annotations)
    
    return(plotly_obj)
  })
}

# Run the application
shinyApp(ui = ui, server = server)