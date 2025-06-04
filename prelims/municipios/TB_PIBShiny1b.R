# TB_PIBShiny1b.R
# This script creates a Shiny app to visualize Brazilian GDP (PIB) data
# with hierarchical geographical aggregation (UF -> Intermediate Region -> Immediate Region/Municipality).

options(scipen = 999) # To prevent scientific notation on large numbers

library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(scales)
library(data.table)
# library(here) # Not needed as we're using a direct absolute path

# --- Data Loading and Initial Preparation ---
# Load the processed data from the specified absolute path.
load("D:/Country/Brazil/TechBrazil/working/ibge/df_pibmunis.rda")

# Rename the loaded object from 'df_pib' to 'df_pibmunis' for consistency in this app.
if (exists("df_pib")) {
  df_pibmunis <- df_pib
  rm(df_pib) # Remove the original name to avoid confusion
}


# Get the exact column names after loading
all_df_pibmunis_names <- names(df_pibmunis)

# Define column names using their *exact* names from the loaded df_pibmunis
# Geographical columns
ano_col <- all_df_pibmunis_names[1]
nome_grande_regiao_col <- all_df_pibmunis_names[3]
nome_uf_col <- all_df_pibmunis_names[6]
nome_municipio_col <- all_df_pibmunis_names[8]
nome_regiao_geog_intermediaria_col <- all_df_pibmunis_names[18]
nome_regiao_geog_imediata_col <- all_df_pibmunis_names[15]

# Specific PIB columns for calculation
total_pib_col <- all_df_pibmunis_names[39]
pib_per_capita_col <- all_df_pibmunis_names[40]

# All PIB value columns for UI selection
pib_value_columns_all <- all_df_pibmunis_names[33:40]

# --- Step 1: Calculate Inferred Municipal Population upfront ---
df_pibmunis_base <- df_pibmunis %>%
  mutate(
    Inferred_Population = if_else(
      is.na(!!sym(pib_per_capita_col)) | !!sym(pib_per_capita_col) == 0,
      NA_real_,
      (!!sym(total_pib_col) * 1000) / !!sym(pib_per_capita_col)
    )
  )

# --- Define choices for location pickers ---
uf_choices_all <- sort(unique(df_pibmunis_base[[nome_uf_col]]))

<<<<<<< HEAD
default_uf <- "Bahia"
default_intermediate_regions <- df_pibmunis_base %>%
=======
# Set specific default values based on the screenshot
default_uf <- "Alagoas"
default_intermediate_regions_selected <- "Arapiraca" # This is now the selected one, not just a choice
default_immediate_regions_selected <- "Delmiro Gouveia" # This is now the selected one

# Calculate choices for intermediate regions based on the default UF
default_intermediate_regions_choices <- df_pibmunis_base %>%
>>>>>>> 93e0834c91feba83d764064ae49613f9995dfabd
  filter(!!sym(nome_uf_col) == default_uf) %>%
  pull(!!sym(nome_regiao_geog_intermediaria_col)) %>%
  unique() %>%
  sort()

# Calculate choices for immediate regions based on the default selected intermediate region
default_immediate_regions_choices_all <- if (length(default_intermediate_regions_selected) > 0) {
  df_pibmunis_base %>%
    filter(!!sym(nome_regiao_geog_intermediaria_col) %in% default_intermediate_regions_selected) %>%
    pull(!!sym(nome_regiao_geog_imediata_col)) %>%
    unique() %>%
    sort()
} else {
  character(0)
}

# Calculate choices for municipalities based on the default selected immediate region
default_muni_selection_choices_all <- if (length(default_immediate_regions_selected) > 0) {
  df_pibmunis_base %>%
    filter(!!sym(nome_regiao_geog_imediata_col) %in% default_immediate_regions_selected) %>%
    pull(!!sym(nome_municipio_col)) %>%
    unique() %>%
    sort()
} else {
  character(0)
}

# The specific default municipalities as shown in the screenshot
default_muni_selection <- c("Água Branca", "Delmiro Gouveia", "Inhapi", "Mata Grande", "Olho D'Água do Casado", "Palestina", "Piranhas")


# pib_colors será gerado dinamicamente no renderPlotly, não mais aqui
# all_possible_display_locations <- c(
#   uf_choices_all,
#   unique(df_pibmunis_base[[nome_regiao_geog_intermediaria_col]]),
#   unique(df_pibmunis_base[[nome_regiao_geog_imediata_col]]),
#   unique(df_pibmunis_base[[nome_municipio_col]])
# )
# pib_colors <- scales::hue_pal()(length(unique(all_possible_display_locations)))
# names(pib_colors) <- unique(all_possible_display_locations)


# --- UI ---
ui <- fluidPage(
  div(
    style = "text-align: center; margin-bottom: 20px;",
    div(
      style = "font-size: 28px; font-weight: bold; color: #333;",
      "Brasil: Tendências das variaveis econômicas por hierarquia geográfica"
    ),
    div(
      style = "font-size: 18px; font-weight: normal; color: #555;",
      "Análise Detalhada por Estado, Região Intermediária, Região Imediata e Município"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      # 1. State (UF) Selection (always visible)
      pickerInput(
        "ufInput",
        label = "Selecionar Estado(s) (UF):",
        choices = uf_choices_all,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
        multiple = TRUE,
        selected = default_uf # Set default UF
      ),
      # 2. Intermediate Region Selection (always visible, dynamically updated)
      pickerInput(
        "intermediateRegionInput",
        label = "Selecionar Região(ões) Geográfica(s) Intermediária(s):",
        choices = default_intermediate_regions_choices, # Use calculated choices
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
        multiple = TRUE,
        selected = default_intermediate_regions_selected # Set default selected intermediate region
      ),
      # 3. Immediate Region Selection (always visible, dynamically updated)
      pickerInput(
        "immediateRegionInput",
        label = "Selecionar Região(ões) Geográfica(s) Imediata(s):",
        choices = default_immediate_regions_choices_all, # Use calculated choices
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
        multiple = TRUE,
        selected = default_immediate_regions_selected # Set default selected immediate region
      ),
      # 4. Municipality Selection (new picker, reactive to Immediate Region)
      uiOutput("municipalityInput"), # New UI element for municipalities
      
      # PIB Variable Selection
      pickerInput(
        "yVariable",
        label = "Selecionar Variável Econômica:",
        choices = pib_value_columns_all,
        options = list(`actions-box` = FALSE),
        selected = pib_value_columns_all[7]
      )
    ),
    mainPanel(
      plotlyOutput("pibLinePlot", height = "75vh"),
      HTML("<p style='font-size: 12px; color: #555; margin-top: 10px;'>
        Fonte: <a href='https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html'
        target='_blank'>IBGE Produto Interno Bruto dos Municípios</a>
       </p>")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Reactive expression for data filtered by UF
  filtered_by_uf_reactive <- reactive({
    req(input$ufInput) # Ensure UF input is available
    if (length(input$ufInput) == 0) { # Check length for empty selection
      return(df_pibmunis_base)
    } else {
      df_pibmunis_base %>%
        filter(!!sym(nome_uf_col) %in% input$ufInput)
    }
  })
  
  # Reactive expression to update Intermediate Region choices based on UF selection
  observeEvent(input$ufInput, {
    # Ensure current_uf_selection is not NULL/empty before processing
    current_uf_selection <- input$ufInput
    req(current_uf_selection)
    
    choices <- filtered_by_uf_reactive() %>%
      pull(!!sym(nome_regiao_geog_intermediaria_col)) %>%
      unique() %>%
      sort()
    
    # Determine selected value for intermediate regions
    # If the default UF is chosen and the default intermediate region exists, select it
    if (default_uf %in% current_uf_selection && "Arapiraca" %in% choices && length(current_uf_selection) == 1) {
      selected_value <- "Arapiraca"
    } else {
      # Try to keep previous selections if valid, otherwise select all current choices
      previous_selection <- isolate(input$intermediateRegionInput)
      selected_value <- intersect(previous_selection, choices)
      if (length(selected_value) == 0 && length(choices) > 0) {
        selected_value <- choices
      }
    }
    updatePickerInput(session, "intermediateRegionInput", choices = choices, selected = selected_value)
  }, ignoreNULL = FALSE, ignoreInit = FALSE) # ignoreInit = FALSE for initial default propagation
  
  # Reactive expression to update Immediate Region choices based on Intermediate Region selection
  observeEvent(input$intermediateRegionInput, {
    current_inter_region_selection <- input$intermediateRegionInput
    req(current_inter_region_selection) # Ensure input is available
    
    choices <- filtered_by_rgi_reactive() %>%
      pull(!!sym(nome_regiao_geog_imediata_col)) %>%
      unique() %>%
      sort()
    
    # Determine selected value for immediate regions
    # If the default intermediate region is chosen and the default immediate region exists, select it
    if ("Arapiraca" %in% current_inter_region_selection && "Delmiro Gouveia" %in% choices && length(current_inter_region_selection) == 1) {
      selected_value <- "Delmiro Gouveia"
    } else {
      # Try to keep previous selections if valid, otherwise select all current choices
      previous_selection <- isolate(input$immediateRegionInput)
      selected_value <- intersect(previous_selection, choices)
      if (length(selected_value) == 0 && length(choices) > 0) {
        selected_value <- choices
      }
    }
    updatePickerInput(session, "immediateRegionInput", choices = choices, selected = selected_value)
  }, ignoreNULL = FALSE, ignoreInit = FALSE) # ignoreInit = FALSE
  
  # Reactive expression for data filtered by Intermediate Region
  filtered_by_rgi_reactive <- reactive({
    req(input$intermediateRegionInput) # Ensure Intermediate Region input is available
    if (length(input$intermediateRegionInput) == 0) { # Check length for empty selection
      return(filtered_by_uf_reactive())
    } else {
      filtered_by_uf_reactive() %>%
        filter(!!sym(nome_regiao_geog_intermediaria_col) %in% input$intermediateRegionInput)
    }
  })
  
  # Reactive expression for data filtered by Immediate Region
  filtered_by_rgimed_reactive <- reactive({
    req(input$immediateRegionInput) # Ensure Immediate Region input is available
    if (length(input$immediateRegionInput) == 0) { # Check length for empty selection
      return(filtered_by_rgi_reactive())
    } else {
      filtered_by_rgi_reactive() %>%
        filter(!!sym(nome_regiao_geog_imediata_col) %in% input$immediateRegionInput)
    }
  })
  
  # NEW: Reactive expression for Municipality choices based on Immediate Region selection
  municipality_choices_reactive <- reactive({
    req(input$immediateRegionInput) # Requires Immediate Region input
    current_immediate_region_selection <- input$immediateRegionInput
    
    if (length(current_immediate_region_selection) == 0) { # Check length for empty selection
      return(character(0))
    } else {
      municipalities <- filtered_by_rgimed_reactive() %>% # Use filtered_by_rgimed_reactive
        pull(!!sym(nome_municipio_col)) %>%
        unique() %>%
        sort()
      return(municipalities)
    }
  })
  
  # NEW: ObserveEvent to update Municipality picker
  output$municipalityInput <- renderUI({
    choices <- municipality_choices_reactive()
    if (is.null(choices) || length(choices) == 0) {
      return(NULL) # Only show if there are municipalities
    }
    
    # Determine selected municipalities:
    # 1. If it's the exact default state/inter/immediate region path, use the screenshot's default municipalities.
    # 2. Otherwise, try to preserve previous selections.
    # 3. If no previous valid selections, select all current choices.
    
    # Safely check input values before comparison for default path
    is_default_path <- (length(input$ufInput) == 1 && input$ufInput == default_uf &&
                          length(input$intermediateRegionInput) == 1 && !is.null(input$intermediateRegionInput) && input$intermediateRegionInput == default_intermediate_regions_selected &&
                          length(input$immediateRegionInput) == 1 && !is.null(input$immediateRegionInput) && input$immediateRegionInput == default_immediate_regions_selected)
    
    if (is_default_path) {
      selected_muni <- default_muni_selection
      selected_muni <- intersect(selected_muni, choices) # Ensure they are valid choices
    } else {
      previous_selection <- isolate(input$municipalityInput)
      selected_muni <- intersect(previous_selection, choices)
      if (length(selected_muni) == 0 && length(choices) > 0) {
        selected_muni <- choices # Default to selecting all if nothing valid from previous
      }
    }
    
    pickerInput(
      "municipalityInput",
      label = "Selecionar Município(s):",
      choices = choices,
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE,
      selected = selected_muni
    )
  })
  
  
  # Reactive expression to perform aggregation based on user selection
  aggregated_data <- reactive({
    req(input$ufInput) # UF input is always required, as it's the top level.
    
    current_selection_ufs <- input$ufInput
    current_selection_inter_regions <- input$intermediateRegionInput
    current_selection_immediate_regions <- input$immediateRegionInput
    current_selection_municipalities <- input$municipalityInput
    
    # Start with the most granular filtered data available
    df_filtered <- filtered_by_rgimed_reactive() # Use the output of the previous reactive
    
    # Apply municipality filter if selected
    if (!is.null(current_selection_municipalities) && length(current_selection_municipalities) > 0) {
      df_filtered <- df_filtered %>% filter(!!sym(nome_municipio_col) %in% current_selection_municipalities)
    }
    
    # Determine the most granular level selected by the user for aggregation and plotting
    grouping_var_sym <- NULL
    # This logic now ensures the correct grouping variable is picked based on selection granularity
    if (!is.null(current_selection_municipalities) && length(current_selection_municipalities) > 0) {
      grouping_var_sym <- sym(nome_municipio_col)
      # If municipalities are selected, we want to see individual municipality lines,
      # even if the user later deselects the immediate region, as long as the municipalities remain selected.
    } else if (!is.null(current_selection_immediate_regions) && length(current_selection_immediate_regions) > 0) {
      grouping_var_sym <- sym(nome_regiao_geog_imediata_col)
    } else if (!is.null(current_selection_inter_regions) && length(current_selection_inter_regions) > 0) {
      grouping_var_sym <- sym(nome_regiao_geog_intermediaria_col)
    } else if (!is.null(current_selection_ufs) && length(current_selection_ufs) > 0) {
      grouping_var_sym <- sym(nome_uf_col)
    } else {
      # Fallback if no selection, group by Grande Região
      grouping_var_sym <- sym(nome_grande_regiao_col)
    }
    
    # Ensure there's data after filtering
    if (nrow(df_filtered) == 0) {
      return(data.frame(
        !!sym(ano_col) := numeric(0),
        Display_Location = character(0),
        !!!setNames(lapply(pib_value_columns_all, function(x) numeric(0)), pib_value_columns_all)
      ))
    }
    
    # Aggregation step for all PIB values except the per capita one
    data_to_plot <- df_filtered %>%
      group_by(!!sym(ano_col), !!grouping_var_sym) %>%
      summarise(
        across(all_of(setdiff(pib_value_columns_all, pib_per_capita_col)), ~ sum(.x, na.rm = TRUE)),
        Sum_Inferred_Population = sum(Inferred_Population, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate PIB per capita AFTER aggregation
    # FIX: Use setNames and list for robust dynamic column naming, COMPLETELY AVOIDING ':=
    data_to_plot <- data_to_plot %>%
      mutate(
        # This is the corrected line to create the pib_per_capita_col dynamically
        # It uses setNames(list(value), name) which is a robust dplyr pattern.
        setNames(list(
          if_else(
            Sum_Inferred_Population == 0, NA_real_,
            (!!sym(total_pib_col) * 1000) / Sum_Inferred_Population
          )
        ), pib_per_capita_col), # This creates the column with the name from pib_per_capita_col
        # Create a more descriptive Display_Location
        Display_Location = case_when(
          grouping_var_sym == sym(nome_municipio_col) ~ paste0("Município - ", as.character(!!grouping_var_sym)),
          grouping_var_sym == sym(nome_regiao_geog_imediata_col) ~ paste0("Região Imediata - ", as.character(!!grouping_var_sym)),
          grouping_var_sym == sym(nome_regiao_geog_intermediaria_col) ~ paste0("Região Intermediária - ", as.character(!!grouping_var_sym)),
          grouping_var_sym == sym(nome_uf_col) ~ paste0("Estado - ", as.character(!!grouping_var_sym)),
          TRUE ~ as.character(!!grouping_var_sym) # Fallback
        )
      ) %>%
      select(-Sum_Inferred_Population) # Remove temporary column
    
    missing_cols <- setdiff(pib_value_columns_all, names(data_to_plot))
    for (col in missing_cols) {
      data_to_plot[[col]] <- NA_real_
    }
    
    final_cols_needed <- c(ano_col, pib_value_columns_all, "Display_Location")
    data_to_plot <- data_to_plot %>% select(all_of(final_cols_needed))
    
    return(data_to_plot)
  })
  
  
  output$pibLinePlot <- renderPlotly({
    req(input$yVariable)
    
    plot_data <- aggregated_data()
    
    plot_data <- plot_data %>%
      filter(!is.na(!!sym(input$yVariable)))
    
    if (nrow(plot_data) == 0) {
      return(ggplotly(ggplot() + 
                        annotate("text", x = 0.5, y = 0.5, label = "Não há dados para exibir com os filtros selecionados.") +
                        theme_void()))
    }
    
    y_labels <- scales::comma
    
    # Dynamically create color mapping based on actual Display_Location values
    unique_locations <- unique(plot_data$Display_Location)
    dynamic_colors <- scales::hue_pal()(length(unique_locations))
    names(dynamic_colors) <- unique_locations
    
    p <- ggplot(plot_data, aes(x = !!sym(ano_col), color = Display_Location)) +
      labs(
        x = "Ano",
        y = input$yVariable,
        title = paste("Tendências do (",
                      min(plot_data[[ano_col]], na.rm = TRUE), "-", 
                      max(plot_data[[ano_col]], na.rm = TRUE), ") - ",
                      input$yVariable, sep = ""),
        color = "Localidade"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)
      ) +
      scale_y_continuous(labels = y_labels) +
      scale_x_continuous(breaks = unique(plot_data[[ano_col]])) +
<<<<<<< HEAD
      scale_color_manual(values = setNames(scales::hue_pal()(length(unique(plot_data$Display_Location))), unique(plot_data$Display_Location)))
    
=======
      scale_color_manual(values = dynamic_colors)  # use dynamic colors here
>>>>>>> 93e0834c91feba83d764064ae49613f9995dfabd
    
    y_sym <- sym(input$yVariable)
    p <- p + geom_line(aes(y = !!y_sym), linewidth = 1) +
      geom_point(aes(y = !!y_sym), size = 2)
    
    annotations <- list()
    if (nrow(plot_data) > 0) {
      for (loc_display in unique(plot_data$Display_Location)) {
        loc_data_subset <- plot_data %>% filter(Display_Location == loc_display)
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