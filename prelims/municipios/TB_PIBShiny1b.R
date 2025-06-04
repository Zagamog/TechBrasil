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

default_uf <- "Bahia"
default_intermediate_regions <- df_pibmunis_base %>%
  filter(!!sym(nome_uf_col) == default_uf) %>%
  pull(!!sym(nome_regiao_geog_intermediaria_col)) %>%
  unique() %>%
  sort()

# Default immediate regions based on the first default intermediate region
# These will be the initial choices for the "Immediate Region" dropdown
default_immediate_regions_choices <- if (length(default_intermediate_regions) > 0) {
  df_pibmunis_base %>%
    filter(!!sym(nome_regiao_geog_intermediaria_col) == default_intermediate_regions[1]) %>%
    pull(!!sym(nome_regiao_geog_imediata_col)) %>%
    unique() %>%
    sort()
} else {
  character(0)
}

# The initial "Municipality" selection will be all municipalities within the *first default immediate region*
# This is the key change for the default selected value in the third picker
default_muni_selection <- if (length(default_immediate_regions_choices) > 0) {
  df_pibmunis_base %>%
    filter(!!sym(nome_regiao_geog_imediata_col) %in% default_immediate_regions_choices) %>% # Changed to %in% for multiple defaults
    pull(!!sym(nome_municipio_col)) %>%
    unique() %>%
    sort()
} else {
  character(0)
}


all_possible_display_locations <- c(
  uf_choices_all,
  unique(df_pibmunis_base[[nome_regiao_geog_intermediaria_col]]),
  unique(df_pibmunis_base[[nome_regiao_geog_imediata_col]]),
  unique(df_pibmunis_base[[nome_municipio_col]])
)
pib_colors <- scales::hue_pal()(length(unique(all_possible_display_locations)))
names(pib_colors) <- unique(all_possible_display_locations)


# --- UI ---
ui <- fluidPage(
  div(
    style = "text-align: center; margin-bottom: 20px;",
    div(
      style = "font-size: 28px; font-weight: bold; color: #333;",
      "Brasil: Tendências do PIB por Hierarquia Geográfica"
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
        selected = default_uf
      ),
      # 2. Intermediate Region Selection (always visible, dynamically updated)
      pickerInput(
        "intermediateRegionInput",
        label = "Selecionar Região(ões) Geográfica(s) Intermediária(s):",
        choices = default_intermediate_regions,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
        multiple = TRUE,
        selected = default_intermediate_regions
      ),
      # 3. Immediate Region Selection (always visible, dynamically updated)
      pickerInput(
        "immediateRegionInput",
        label = "Selecionar Região(ões) Geográfica(s) Imediata(s):",
        choices = default_immediate_regions_choices,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
        multiple = TRUE,
        selected = default_immediate_regions_choices
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
  
  # Reactive expression to update Intermediate Region choices based on UF selection
  observeEvent(input$ufInput, {
    current_uf_selection <- input$ufInput
    if (is.null(current_uf_selection) || length(current_uf_selection) == 0) {
      updatePickerInput(session, "intermediateRegionInput", choices = character(0), selected = character(0))
    } else {
      choices <- df_pibmunis_base %>%
        filter(!!sym(nome_uf_col) %in% current_uf_selection) %>%
        pull(!!sym(nome_regiao_geog_intermediaria_col)) %>%
        unique() %>%
        sort()
      updatePickerInput(session, "intermediateRegionInput", choices = choices, selected = choices)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Reactive expression to update Immediate Region choices based on Intermediate Region selection
  immediate_region_choices_reactive <- reactive({
    current_inter_region_selection <- input$intermediateRegionInput
    if (is.null(current_inter_region_selection) || length(current_inter_region_selection) == 0) {
      return(character(0))
    } else {
      df_filtered_by_inter_region <- df_pibmunis_base %>%
        filter(!!sym(nome_regiao_geog_intermediaria_col) %in% current_inter_region_selection)
      
      immediate_regions <- df_filtered_by_inter_region %>%
        pull(!!sym(nome_regiao_geog_imediata_col)) %>%
        unique() %>%
        sort()
      return(immediate_regions)
    }
  })
  
  observeEvent(immediate_region_choices_reactive(), {
    choices <- immediate_region_choices_reactive()
    updatePickerInput(session, "immediateRegionInput", choices = choices, selected = choices)
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  
  # NEW: Reactive expression for Municipality choices based on Immediate Region selection
  municipality_choices_reactive <- reactive({
    req(input$immediateRegionInput) # Requires Immediate Region input
    current_immediate_region_selection <- input$immediateRegionInput
    if (is.null(current_immediate_region_selection) || length(current_immediate_region_selection) == 0) {
      return(character(0))
    } else {
      df_filtered_by_immediate_region <- df_pibmunis_base %>%
        filter(!!sym(nome_regiao_geog_imediata_col) %in% current_immediate_region_selection)
      
      municipalities <- df_filtered_by_immediate_region %>%
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
    pickerInput(
      "municipalityInput",
      label = "Selecionar Município(s):",
      choices = choices,
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = TRUE,
      selected = choices # Default to selecting all municipalities
    )
  })
  
  
  # Reactive expression to perform aggregation based on user selection
  aggregated_data <- reactive({
    req(input$ufInput) # UF input is always required, as it's the top level.
    
    current_selection_ufs <- input$ufInput
    current_selection_inter_regions <- input$intermediateRegionInput
    current_selection_immediate_regions <- input$immediateRegionInput # Renamed input
    current_selection_municipalities <- input$municipalityInput # New input for municipalities
    
    data_to_plot <- data.frame()
    grouping_var_sym <- NULL
    df_filtered <- df_pibmunis_base # Start with base data
    
    # Apply filters hierarchically
    if (!is.null(current_selection_ufs) && length(current_selection_ufs) > 0) {
      df_filtered <- df_filtered %>% filter(!!sym(nome_uf_col) %in% current_selection_ufs)
    }
    if (!is.null(current_selection_inter_regions) && length(current_selection_inter_regions) > 0) {
      df_filtered <- df_filtered %>% filter(!!sym(nome_regiao_geog_intermediaria_col) %in% current_selection_inter_regions)
    }
    if (!is.null(current_selection_immediate_regions) && length(current_selection_immediate_regions) > 0) {
      df_filtered <- df_filtered %>% filter(!!sym(nome_regiao_geog_imediata_col) %in% current_selection_immediate_regions)
    }
    if (!is.null(current_selection_municipalities) && length(current_selection_municipalities) > 0) {
      df_filtered <- df_filtered %>% filter(!!sym(nome_municipio_col) %in% current_selection_municipalities)
    }
    
    # Determine the most granular level selected by the user for aggregation and plotting
    if (!is.null(current_selection_municipalities) && length(current_selection_municipalities) > 0) {
      grouping_var_sym <- sym(nome_municipio_col)
    } else if (!is.null(current_selection_immediate_regions) && length(current_selection_immediate_regions) > 0) {
      grouping_var_sym <- sym(nome_regiao_geog_imediata_col)
    } else if (!is.null(current_selection_inter_regions) && length(current_selection_inter_regions) > 0) {
      grouping_var_sym <- sym(nome_regiao_geog_intermediaria_col)
    } else if (!is.null(current_selection_ufs) && length(current_selection_ufs) > 0) {
      grouping_var_sym <- sym(nome_uf_col)
    } else {
      # Fallback if no selection, return empty data
      return(data.frame(
        !!sym(ano_col) := numeric(0),
        Display_Location = character(0),
        !!!setNames(lapply(pib_value_columns_all, function(x) numeric(0)), pib_value_columns_all)
      ))
    }
    
    # Ensure there's data after filtering
    if (nrow(df_filtered) == 0) {
      return(data.frame(
        !!sym(ano_col) := numeric(0),
        Display_Location = character(0),
        !!!setNames(lapply(pib_value_columns_all, function(x) numeric(0)), pib_value_columns_all)
      ))
    }
    
    if (!is.null(grouping_var_sym)) {
      data_to_plot <- df_filtered %>%
        group_by(!!sym(ano_col), !!grouping_var_sym) %>%
        summarise(
          across(all_of(all_df_pibmunis_names[33:39]), \(x) sum(x, na.rm = TRUE)),
          Sum_Inferred_Population = sum(Inferred_Population, na.rm = TRUE),
          !!sym(pib_per_capita_col) := if_else(
            Sum_Inferred_Population == 0, NA_real_,
            (sum(!!sym(total_pib_col), na.rm = TRUE) * 1000) / Sum_Inferred_Population
          ),
          Display_Location = as.character(!!grouping_var_sym),
          .groups = "drop"
        ) %>%
        select(-Sum_Inferred_Population, -!!grouping_var_sym)
    } else {
      data_to_plot <- data.frame(
        !!sym(ano_col) := numeric(0),
        Display_Location = character(0),
        !!!setNames(lapply(pib_value_columns_all, function(x) numeric(0)), pib_value_columns_all)
      )
    }
    
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
      return(ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Não há dados para exibir com os filtros selecionados.") + theme_void()))
    }
    
    y_labels <- scales::comma
    
    p <- ggplot(plot_data, aes(x = !!sym(ano_col), color = Display_Location)) +
      labs(
        x = "Ano",
        y = input$yVariable,
        title = paste("Tendências do PIB Brasileiro (", min(plot_data[[ano_col]], na.rm = TRUE), "-", max(plot_data[[ano_col]], na.rm = TRUE), ") - ", input$yVariable, sep=""),
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
      scale_color_manual(values = setNames(scales::hue_pal()(length(unique(plot_data$Display_Location))), unique(plot_data$Display_Location)))
    
    
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