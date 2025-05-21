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
# This loads an object named 'df_pib' (as per your save command in TB_municipios1a.R).
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
ano_col <- all_df_pibmunis_names[1] # "Ano"
# cod_grande_regiao_col <- all_df_pibmunis_names[2] # "Código da Grande Região"
nome_grande_regiao_col <- all_df_pibmunis_names[3] # "Nome da Grande Região"
# cod_uf_col <- all_df_pibmunis_names[4] # "Código da Unidade da Federação"
# sigla_uf_col <- all_df_pibmunis_names[5] # "Sigla da Unidade da Federação"
nome_uf_col <- all_df_pibmunis_names[6] # "Nome da Unidade da Federação"
# cod_municipio_col <- all_df_pibmunis_names[7] # "Código do Município"
nome_municipio_col <- all_df_pibmunis_names[8] # "Nome do Município"
# nome_regiao_geog_imediata_col <- all_df_pibmunis_names[15] # "Nome da Região Geográfica Imediata"
nome_regiao_geog_intermediaria_col <- all_df_pibmunis_names[18] # "Nome da Região Geográfica Intermediária"
nome_regiao_geog_imediata_col <- all_df_pibmunis_names[15] # "Nome da Região Geográfica Imediata"

# Specific PIB columns for calculation
total_pib_col <- all_df_pibmunis_names[39] # "Produto Interno Bruto, a preços correntes (R$ 1.000)"
pib_per_capita_col <- all_df_pibmunis_names[40] # "Produto Interno Bruto per capita, a preços correntes (R$ 1,00)"

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

default_uf <- "Maranhão"
default_intermediate_regions <- df_pibmunis_base %>%
  filter(!!sym(nome_uf_col) == default_uf) %>%
  pull(!!sym(nome_regiao_geog_intermediaria_col)) %>%
  unique() %>%
  sort()

default_immediate_muni_choices <- if (length(default_intermediate_regions) > 0) {
  df_filtered_by_default_inter_region <- df_pibmunis_base %>%
    filter(!!sym(nome_regiao_geog_intermediaria_col) == default_intermediate_regions[1])
  
  immediate_regions_default <- df_filtered_by_default_inter_region %>%
    pull(!!sym(nome_regiao_geog_imediata_col)) %>%
    unique() %>%
    sort()
  
  municipalities_default <- df_filtered_by_default_inter_region %>%
    pull(!!sym(nome_municipio_col)) %>%
    unique() %>%
    sort()
  
  list(
    "Regiões Geográficas Imediatas" = immediate_regions_default, # Changed to Portuguese
    "Municípios" = municipalities_default # Changed to Portuguese
  )
} else {
  list()
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
      "Brasil: Tendências do PIB por Hierarquia Geográfica" # Changed to Portuguese
    ),
    div(
      style = "font-size: 18px; font-weight: normal; color: #555;",
      "Análise Detalhada por Estado, Região Intermediária, Região Imediata e Município" # Changed to Portuguese
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      # 1. State (UF) Selection (always visible)
      pickerInput(
        "ufInput",
        label = "Selecionar Estado(s) (UF):", # Changed to Portuguese
        choices = uf_choices_all,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
        multiple = TRUE,
        selected = default_uf
      ),
      # 2. Intermediate Region Selection (always visible, dynamically updated)
      pickerInput(
        "intermediateRegionInput",
        label = "Selecionar Região(ões) Geográfica(s) Intermediária(s):", # Changed to Portuguese
        choices = default_intermediate_regions,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
        multiple = TRUE,
        selected = default_intermediate_regions
      ),
      # 3. Immediate Region / Municipality Selection (always visible, dynamically updated)
      pickerInput(
        "immediateRegionMunicipalityInput",
        label = "Selecionar Região(ões) Geográfica(s) Imediata(s) / Município(s):", # Changed to Portuguese
        choices = default_immediate_muni_choices,
        options = list(`actions-box` = TRUE, `live-search` = TRUE),
        multiple = TRUE,
        selected = unlist(default_immediate_muni_choices)
      ),
      
      # PIB Variable Selection
      pickerInput(
        "yVariable",
        label = "Selecionar Variável Econômica:", # Changed to Portuguese
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
       </p>") # Changed to Portuguese
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
  
  # Reactive expression for Immediate Region / Municipality choices
  actual_immediate_muni_choices_list <- reactive({
    current_inter_region_selection <- input$intermediateRegionInput
    if (is.null(current_inter_region_selection) || length(current_inter_region_selection) == 0) {
      return(list("Regiões Geográficas Imediatas" = character(0), "Municípios" = character(0))) # Changed to Portuguese
    } else {
      df_filtered_by_inter_region <- df_pibmunis_base %>%
        filter(!!sym(nome_regiao_geog_intermediaria_col) %in% current_inter_region_selection)
      
      immediate_regions <- df_filtered_by_inter_region %>%
        pull(!!sym(nome_regiao_geog_imediata_col)) %>%
        unique() %>%
        sort()
      
      municipalities <- df_filtered_by_inter_region %>%
        pull(!!sym(nome_municipio_col)) %>%
        unique() %>%
        sort()
      
      return(list(
        "Regiões Geográficas Imediatas" = immediate_regions, # Changed to Portuguese
        "Municípios" = municipalities # Changed to Portuguese
      ))
    }
  })
  
  observeEvent(actual_immediate_muni_choices_list(), {
    choices <- actual_immediate_muni_choices_list()
    if (is.null(choices) || (length(choices$`Regiões Geográficas Imediatas`) == 0 && length(choices$Municípios) == 0)) { # Changed to Portuguese
      updatePickerInput(session, "immediateRegionMunicipalityInput", choices = list(), selected = character(0))
    } else {
      updatePickerInput(session, "immediateRegionMunicipalityInput", choices = choices, selected = unlist(choices))
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  
  # Reactive expression to perform aggregation based on user selection
  aggregated_data <- reactive({
    req(input$ufInput)
    
    current_selection_ufs <- input$ufInput
    current_selection_inter_regions <- input$intermediateRegionInput
    current_selection_immediate_or_mun <- input$immediateRegionMunicipalityInput
    
    possible_immediate_muni_choices_at_this_moment <- actual_immediate_muni_choices_list()
    
    data_to_plot <- data.frame()
    grouping_var_sym <- NULL
    
    if (!is.null(current_selection_immediate_or_mun) && length(current_selection_immediate_or_mun) > 0) {
      munis_in_input <- intersect(current_selection_immediate_or_mun, possible_immediate_muni_choices_at_this_moment$Municipalities)
      immediates_in_input <- intersect(current_selection_immediate_or_mun, possible_immediate_muni_choices_at_this_moment$`Regiões Geográficas Imediatas`) # Changed to Portuguese
      
      if (length(munis_in_input) > 0) {
        grouping_var_sym <- sym(nome_municipio_col)
        df_filtered <- df_pibmunis_base %>%
          filter(!!sym(nome_municipio_col) %in% munis_in_input)
      } else if (length(immediates_in_input) > 0) {
        grouping_var_sym <- sym(nome_regiao_geog_imediata_col)
        df_filtered <- df_pibmunis_base %>%
          filter(!!sym(nome_regiao_geog_imediata_col) %in% immediates_in_input)
      } else {
        df_filtered <- df_pibmunis_base %>%
          filter(!!sym(nome_uf_col) %in% current_selection_ufs)
      }
    } else if (!is.null(input$intermediateRegionInput) && length(input$intermediateRegionInput) > 0) {
      grouping_var_sym <- sym(nome_regiao_geog_intermediaria_col)
      df_filtered <- df_pibmunis_base %>%
        filter(!!sym(nome_regiao_geog_intermediaria_col) %in% current_selection_inter_regions)
    } else {
      grouping_var_sym <- sym(nome_uf_col)
      df_filtered <- df_pibmunis_base %>%
        filter(!!sym(nome_uf_col) %in% current_selection_ufs)
    }
    
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
      return(ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Não há dados para exibir com os filtros selecionados.") + theme_void())) # Changed to Portuguese
    }
    
    y_labels <- scales::comma
    
    p <- ggplot(plot_data, aes(x = !!sym(ano_col), color = Display_Location)) +
      labs(
        x = "Ano", # Changed to Portuguese
        y = input$yVariable,
        title = paste("Tendências do PIB Brasileiro (", min(plot_data[[ano_col]], na.rm = TRUE), "-", max(plot_data[[ano_col]], na.rm = TRUE), ") - ", input$yVariable, sep=""), # Changed to Portuguese
        color = "Localidade" # Changed to Portuguese
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)
      ) +
      scale_y_continuous(labels = y_labels) +
      scale_x_continuous(breaks = unique(plot_data[[ano_col]])) +
      scale_color_manual(values = pib_colors)
    
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