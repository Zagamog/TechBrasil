# TB_PIBShiny1a.R
# This script creates a Shiny app to visualize Brazilian GDP (PIB) data
# by geographical aggregates (Brazil, Major Regions, States), calculated on the fly.

options(scipen = 999) # To prevent scientific notation on large numbers

library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(scales)
# library(here) # Not needed as we're using a direct absolute path

# --- Data Loading and Initial Preparation (only what's always needed) ---
# Load the processed df_pibmunis directly into memory using the absolute path.
load("D:/Country/Brazil/TechBrazil/working/ibge/df_pibmunis.rda")

# Get the exact column names after loading
all_df_pibmunis_names <- names(df_pibmunis)

# Define column names for numerical variables using their *exact* positions and names from the loaded df_pibmunis
pib_value_columns_all <- all_df_pibmunis_names[33:40] # All variables for UI selection

# Define the geographical columns by their exact names (using the provided column indices)
ano_col <- all_df_pibmunis_names[1] # "Ano"
cod_grande_regiao_col <- all_df_pibmunis_names[2] # "Código da Grande Região"
nome_grande_regiao_col <- all_df_pibmunis_names[3] # "Nome da Grande Região"
cod_uf_col <- all_df_pibmunis_names[4] # "Código da Unidade da Federação"
sigla_uf_col <- all_df_pibmunis_names[5] # "Sigla da Unidade da Federação"
nome_uf_col <- all_df_pibmunis_names[6] # "Nome da Unidade da Federação"
cod_municipio_col <- all_df_pibmunis_names[7] # "Código do Município"
nome_municipio_col <- all_df_pibmunis_names[8] # "Nome do Município"
regiao_metropolitana_col <- all_df_pibmunis_names[9] # "Região Metropolitana"
cod_mesorregiao_col <- all_df_pibmunis_names[10] # "Código da Mesorregião"
nome_mesorregiao_col <- all_df_pibmunis_names[11] # "Nome da Mesorregião"
cod_microrregiao_col <- all_df_pibmunis_names[12] # "Código da Microrregião"
nome_microrregiao_col <- all_df_pibmunis_names[13] # "Nome da Microrregião"
cod_regiao_geog_imediata_col <- all_df_pibmunis_names[14] # "Código da Região Geográfica Imediata"
nome_regiao_geog_imediata_col <- all_df_pibmunis_names[15] # "Nome da Região Geográfica Imediata"
cod_regiao_geog_intermediaria_col <- all_df_pibmunis_names[17] # "Código da Região Geográfica Intermediária"
nome_regiao_geog_intermediaria_col <- all_df_pibmunis_names[18] # "Nome da Região Geográfica Intermediária"
cod_concentracao_urbana_col <- all_df_pibmunis_names[20] # "Código Concentração Urbana"
nome_concentracao_urbana_col <- all_df_pibmunis_names[21] # "Nome Concentração Urbana"
cod_arranjo_populacional_col <- all_df_pibmunis_names[23] # "Código Arranjo Populacional"
nome_arranjo_populacional_col <- all_df_pibmunis_names[24] # "Nome Arranjo Populacional"
cod_regiao_rural_col <- all_df_pibmunis_names[27] # "Código da Região Rural"
nome_regiao_rural_col <- all_df_pibmunis_names[28] # "Nome da Região Rural"

# Specific PIB columns for calculation
total_pib_col <- all_df_pibmunis_names[39] # "Produto Interno Bruto, a preços correntes (R$ 1.000)"
pib_per_capita_col <- all_df_pibmunis_names[40] # "Produto Interno Bruto per capita, a preços correntes (R$ 1,00)"

# --- Step 1: Calculate Inferred Municipal Population upfront ---
# This is done once when the app starts, as it's a base calculation.
df_pibmunis_base <- df_pibmunis %>%
  mutate(
    # Handle division by zero or NA in pib_per_capita_col
    # If per capita is 0 or NA, population cannot be inferred, set to NA
    # Multiply total PIB by 1000 because it's in thousands of R$
    Inferred_Population = if_else(
      is.na(!!sym(pib_per_capita_col)) | !!sym(pib_per_capita_col) == 0,
      NA_real_,
      (!!sym(total_pib_col) * 1000) / !!sym(pib_per_capita_col)
    )
  )

# --- Define choices for location picker with groups ---
# Get unique regions and states, sorted alphabetically within their groups
country_choice <- "Brasil"
# Get unique regions and states from the original df_pibmunis (before any aggregation)
region_choices <- sort(unique(df_pibmunis_base[[nome_grande_regiao_col]]))
uf_choices <- sort(unique(df_pibmunis_base[[nome_uf_col]]))

# Create a named list for pickerInput choices to create groups
grouped_location_choices <- list(
  "Brasil" = country_choice,
  "Grandes Regiões" = region_choices,
  "Unidades de Federa;cão (UF)" = uf_choices
  # Add other geographical aggregations if desired, e.g.:
  # "Metropolitan Regions" = sort(unique(df_pibmunis_base[[regiao_metropolitana_col]])),
  # "Mesorregions" = sort(unique(df_pibmunis_base[[nome_mesorregiao_col]]))
)

# Define a predefined color palette for each geographical unit
pib_colors <- scales::hue_pal()(length(unique(unlist(grouped_location_choices))))
names(pib_colors) <- unique(unlist(grouped_location_choices))

# --- UI ---
ui <- fluidPage(
  div(
    style = "text-align: center; margin-bottom: 20px;",
    div(
      style = "font-size: 28px; font-weight: bold; color: #333;",
      "Brasil: PIB e outros indicadores econômicos por Região e por Estado"
    ),
    div(
      style = "font-size: 18px; font-weight: normal; color: #555;",
      "PIB, PIB per capita, Valor Adicional Bruto da Agropecuária, Indústria,Comércio e Serviços,Administração e Total"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "localInput",
        label = "Selecionar Localidade(s):",
        choices = grouped_location_choices, # Use the grouped choices here
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = "Brasil"
      ),
      pickerInput(
        "yVariable",
        label = "Selecionar Variável Econômica:",
        choices = pib_value_columns_all, # All variables are available in UI
        options = list(`actions-box` = FALSE),
        selected = pib_value_columns_all[7] # Default to "Produto Interno Bruto, a preços correntes (R$ 1.000)"
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
  
  # Reactive expression to perform aggregation based on user selection
  aggregated_data <- reactive({
    req(input$localInput)
    
    current_selection <- input$localInput
    data_to_plot <- data.frame() # Initialize empty dataframe
    
    for (selection in current_selection) {
      temp_df <- NULL
      if (selection == "Brasil") {
        # Aggregate for Brazil
        temp_df <- df_pibmunis_base %>%
          group_by(!!sym(ano_col)) %>%
          summarise(
            across(all_of(all_df_pibmunis_names[33:39]), \(x) sum(x, na.rm = TRUE)), # Sum total values
            Sum_Inferred_Population = sum(Inferred_Population, na.rm = TRUE),
            # Calculate correct per capita for Brasil
            !!sym(pib_per_capita_col) := if_else(
              Sum_Inferred_Population == 0, NA_real_,
              (sum(!!sym(total_pib_col), na.rm = TRUE) * 1000) / Sum_Inferred_Population
            ),
            Display_Location = "Brasil", # Assign Display_Location
            .groups = "drop"
          ) %>%
          select(-Sum_Inferred_Population)
        
      } else if (selection %in% region_choices) {
        # Aggregate for Major Regions
        temp_df <- df_pibmunis_base %>%
          filter(!!sym(nome_grande_regiao_col) == selection) %>%
          group_by(!!sym(ano_col), !!sym(nome_grande_regiao_col)) %>%
          summarise(
            across(all_of(all_df_pibmunis_names[33:39]), \(x) sum(x, na.rm = TRUE)),
            Sum_Inferred_Population = sum(Inferred_Population, na.rm = TRUE),
            !!sym(pib_per_capita_col) := if_else(
              Sum_Inferred_Population == 0, NA_real_,
              (sum(!!sym(total_pib_col), na.rm = TRUE) * 1000) / Sum_Inferred_Population
            ),
            Display_Location = selection,
            .groups = "drop"
          ) %>%
          select(-Sum_Inferred_Population, -!!sym(nome_grande_regiao_col)) # Remove original region column
        
      } else if (selection %in% uf_choices) {
        # Aggregate for States (UF)
        temp_df <- df_pibmunis_base %>%
          filter(!!sym(nome_uf_col) == selection) %>%
          group_by(!!sym(ano_col), !!sym(nome_uf_col)) %>%
          summarise(
            across(all_of(all_df_pibmunis_names[33:39]), \(x) sum(x, na.rm = TRUE)),
            Sum_Inferred_Population = sum(Inferred_Population, na.rm = TRUE),
            !!sym(pib_per_capita_col) := if_else(
              Sum_Inferred_Population == 0, NA_real_,
              (sum(!!sym(total_pib_col), na.rm = TRUE) * 1000) / Sum_Inferred_Population
            ),
            Display_Location = selection,
            .groups = "drop"
          ) %>%
          select(-Sum_Inferred_Population, -!!sym(nome_uf_col)) # Remove original UF column
      }
      # If you add more aggregation levels (e.g., Mesorregião, Microrregião),
      # you would add more `else if` blocks here following a similar pattern.
      # For individual municipalities, you might filter directly without summarising further
      # if they are part of the 'Display_Location' choice.
      
      if (!is.null(temp_df)) {
        data_to_plot <- bind_rows(data_to_plot, temp_df)
      }
    }
    
    # Ensure all selected pib_value_columns_all are present in final data_to_plot
    # Fill missing columns with NA if a specific aggregation type doesn't produce them.
    missing_cols <- setdiff(pib_value_columns_all, names(data_to_plot))
    for (col in missing_cols) {
      data_to_plot[[col]] <- NA_real_
    }
    
    
    # Ensure required columns for plotting are present
    final_cols_needed <- c(ano_col, pib_value_columns_all, "Display_Location")
    data_to_plot <- data_to_plot %>% select(all_of(final_cols_needed))
    
    return(data_to_plot)
  })
  
  
  output$pibLinePlot <- renderPlotly({
    req(input$yVariable)
    
    plot_data <- aggregated_data() # Use the reactive aggregated data
    
    # Filter out NA values for the selected y-variable to prevent plot errors
    plot_data <- plot_data %>%
      filter(!is.na(!!sym(input$yVariable)))
    
    # Determine y-axis labels
    y_labels <- scales::comma # All PIB values are numbers, use comma formatting
    
    # Create the base ggplot object
    p <- ggplot(plot_data, aes(x = !!sym(ano_col), color = Display_Location)) +
      labs(
        x = "Ano",
        y = input$yVariable,
        title = input$yVariable,
        color = "Localidade"
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
      geom_point(aes(y = !!y_sym), size = 2)
    
    # Add Plotly annotations
    annotations <- list()
    # Only add annotations if there's data to plot
    if (nrow(plot_data) > 0) {
      for (loc_display in unique(plot_data$Display_Location)) {
        loc_data_subset <- plot_data %>% filter(Display_Location == loc_display)
        
        # Ensure there's data for the current location before trying to find max year
        if (nrow(loc_data_subset) > 0) {
          last_year <- max(loc_data_subset[[ano_col]], na.rm = TRUE)
          last_value <- loc_data_subset %>%
            filter(!!sym(ano_col) == last_year) %>%
            pull(!!y_sym)
          
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
      }
    }
    
    
    plotly_obj <- ggplotly(p)
    plotly_obj <- plotly_obj %>% layout(annotations = annotations)
    
    return(plotly_obj)
  })
}

# Run the application
shinyApp(ui = ui, server = server)