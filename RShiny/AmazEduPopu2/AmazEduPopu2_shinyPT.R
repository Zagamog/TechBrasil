# AmazEduPopu2_ShinyPT.R

options(scipen = 999)

library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(scales)
library(here)
library(stringr)
library(purrr)

# Load the data
load("meta11a_opcoes.rda")
load("pop01_70b.rda")
load("df_codes_ibge.rda")

# Get regional mappings from geocodes
geocodes <- df_codes_ibge %>% 
  select(SG_UF, CO_5RGRANDE, NM_5RGRANDE) %>% 
  distinct()

# Define Amazonia Legal states
amazonia_legal <- c("AC", "AP", "AM", "MA", "MT", "PA", "RO", "RR", "TO")

# Data preparation before UI
# Select needed columns from population data and create 15-19 age group
pop_data <- pop01_70b %>%
  select(ANO, SIGLA, LOCAL, `15-17_T`, `18-21_T`) %>%
  mutate(`15-19_T` = `15-17_T` + (`18-21_T` * 0.5)) %>%
  filter(ANO >= 2007 & ANO <= 2035) %>%
  # Drop the _r constructs  
  filter(!LOCAL %in% c("Centro-Oeste_r", "Nordeste_r"))

# Select needed columns from enrollment data
enrollment_data_states <- meta11a_opcoes %>%
  select(ANO, SG_UF, NM_UF, QT_MAT_PROF_TEC_PROPAG, QT_MAT_MED) %>%
  filter(ANO >= 2007 & ANO <= 2035)

# Create IBGE regional aggregates
enrollment_data_regions <- enrollment_data_states %>%
  left_join(geocodes, by = "SG_UF") %>%
  filter(!is.na(NM_5RGRANDE)) %>%
  group_by(ANO, NM_5RGRANDE) %>%
  summarise(
    QT_MAT_PROF_TEC_PROPAG = sum(QT_MAT_PROF_TEC_PROPAG, na.rm = TRUE),
    QT_MAT_MED = sum(QT_MAT_MED, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Map to correct SIGLA codes to match population data
  mutate(
    SG_UF = case_when(
      NM_5RGRANDE == "Norte" ~ "NO",
      NM_5RGRANDE == "Nordeste" ~ "ND", 
      NM_5RGRANDE == "Sudeste" ~ "SD",
      NM_5RGRANDE == "Sul" ~ "SU",
      NM_5RGRANDE == "Centro-Oeste" ~ "CO",
      TRUE ~ NM_5RGRANDE
    ),
    NM_UF = NM_5RGRANDE
  ) %>%
  select(-NM_5RGRANDE)

# Create Amazonia Legal aggregate
enrollment_data_amazonia <- enrollment_data_states %>%
  filter(SG_UF %in% amazonia_legal) %>%
  group_by(ANO) %>%
  summarise(
    QT_MAT_PROF_TEC_PROPAG = sum(QT_MAT_PROF_TEC_PROPAG, na.rm = TRUE),
    QT_MAT_MED = sum(QT_MAT_MED, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    SG_UF = "AML",  # SIGLA for Amazonia_Legal in population data
    NM_UF = "Amazonia_Legal"
  )

# Combine state, regional, and Amazonia Legal enrollment data
enrollment_data <- enrollment_data_states %>%
  bind_rows(enrollment_data_regions) %>%
  bind_rows(enrollment_data_amazonia)

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
  "% Matrícula EPT" = "PCT_MAT_EPT",
  "% Matrícula Ensino Médio" = "PCT_MAT_MED"
)

# Define a predefined color palette for each LOCAL
local_colors <- c(
  "Brasil" = "#1f77b4", "Norte" = "#ff7f0e", "Nordeste" = "#2ca02c",
  "Sudeste" = "#d62728", "Sul" = "#9467bd", "Centro-Oeste" = "#8c564b",
  "Acre" = "#e377c2", "Alagoas" = "#7f7f7f", "Amapá" = "#bcbd22", "Amazonas" = "#17becf",
  "Bahia" = "#aec7e8", "Ceará" = "#ffbb78", "Distrito Federal" = "#98df8a", "Espírito Santo" = "#ff9896",
  "Goiás" = "#c5b0d5", "Maranhão" = "#c49c94", "Mato Grosso" = "#f7b6d3", "Mato Grosso do Sul" = "#c7c7c7",
  "Minas Gerais" = "#dbdb8d", "Pará" = "#9edae5", "Paraíba" = "#ad494a", "Paraná" = "#8c6d31",
  "Pernambuco" = "#393b79", "Piauí" = "#5254a3", "Rio de Janeiro" = "#6b6ecf", "Rio Grande do Norte" = "#9c9ede",
  "Rio Grande do Sul" = "#637939", "Rondônia" = "#8ca252", "Roraima" = "#b5cf6b", "Santa Catarina" = "#cedb9c",
  "São Paulo" = "#8c6d31", "Sergipe" = "#bd9e39", "Tocantins" = "#e7ba52"
)

# Calculate Meta 11 targets (add this after the existing data prep)
location_order <- c(
  "Brasil",
  "Norte", "Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins",
  "Nordeste", "Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe",
  "Sudeste", "Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo", 
  "Sul", "Paraná", "Rio Grande do Sul", "Santa Catarina",
  "Centro-Oeste", "Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul"
)

meta11_df <- combined_data %>%
  filter(ANO == 2013) %>%
  group_by(LOCAL) %>%
  summarise(
    meta11_absolute = sum(QT_MAT_PROF_TEC_PROPAG, na.rm = TRUE) * 3,
    .groups = 'drop'
  ) %>%
  arrange(match(LOCAL, location_order)) 

# UI
# UI
ui <- fluidPage(
  titlePanel("Brasil: População 15-19 anos e Matrículas EM e EPT"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "localInput",
        label = "Selecionar Localização(ões):",
        choices = sort(unique(combined_data$LOCAL)),
        options = list(`actions-box` = TRUE),
        multiple = TRUE,
        selected = "Piauí"
      ),
      radioButtons(
        "dataType",
        label = "Escolher Tipo de Dados:",
        choices = list("Números Absolutos" = "numbers", "Percentagens" = "percentages"),
        selected = "numbers"
      ),
      uiOutput("variableInput"),
      
      # New Meta Target Controls
      hr(),
      h5("Meta Targets (para localizações selecionadas):"),
      uiOutput("metaTargetsUI")
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
# Server
server <- function(input, output, session) {
  
  # Dynamically update the variable input based on selected data type (existing)
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
        selected = c("PCT_MAT_EPT")
      )
    }
  })
  
  output$metaTargetsUI <- renderUI({
    req(input$localInput)
    
    # Create table header
    table_header <- tags$thead(
      tags$tr(
        tags$th("UF/REG/BRASIL", style = "width: 30%;"),
        tags$th("META 11 VIGENTE", style = "width: 35%;"),
        tags$th("META DEFINIDO (preenche)", style = "width: 35%;")
      )
    )
    
    # Create table rows
    table_rows <- map(input$localInput, ~{
      loc <- .x
      safe_id <- str_replace_all(loc, "[^A-Za-z0-9]", "_")
      
      # Direct lookup from data frame
      meta_value <- meta11_df$meta11_absolute[meta11_df$LOCAL == loc]
      if(length(meta_value) == 0) meta_value <- 0
      
      tags$tr(
        tags$td(strong(loc)),
        tags$td(
          checkboxInput(paste0("use_meta11_", safe_id), 
                        paste0("Meta 11: ", round(meta_value)), 
                        value = TRUE)
        ),
        tags$td(
          numericInput(paste0("custom_meta_", safe_id), 
                       "", 
                       value = NA, 
                       min = 0)
        )
      )
    })
    
    # Complete table
    tags$table(class = "table table-condensed",
               table_header,
               tags$tbody(table_rows)
    )
  })
  
  # New: Mutual exclusion logic for meta controls
  observe({
    req(input$localInput)
    
    walk(input$localInput, ~{
      loc <- .x
      safe_id <- str_replace_all(loc, "[^A-Za-z0-9]", "_")
      meta11_id <- paste0("use_meta11_", safe_id)
      custom_id <- paste0("custom_meta_", safe_id)
      
      # Check if inputs exist before using them
      if(!is.null(input[[meta11_id]]) && !is.null(input[[custom_id]])) {
        # If Meta 11 is checked, clear and disable custom input
        if(isTruthy(input[[meta11_id]])) {
          updateNumericInput(session, custom_id, value = NA)
        }
        
        # If custom value is entered, uncheck Meta 11
        if(!is.na(input[[custom_id]]) && input[[custom_id]] > 0) {
          updateCheckboxInput(session, meta11_id, value = FALSE)
        }
      }
    })
  })
  
  # Updated plotting logic
  output$linePlot <- renderPlotly({
    req(input$yVariables, input$localInput)
    
    # Filter data based on selected locations
    filtered_data <- combined_data %>%
      filter(LOCAL %in% input$localInput)
    
    # Get the current variable choices based on data type
    current_variables <- if (input$dataType == "numbers") number_variables else percentage_variables
    
    # Determine y-axis limits and labels based on data type
    if (input$dataType == "numbers") {
      y_min <- 0
      y_max <- max(filtered_data[input$yVariables], na.rm = TRUE) * 1.1
      y_labels <- scales::comma
      y_axis_title <- "Contagem"
      plot_title <- "População 15-19 anos e Matrículas EPT/Ensino Médio (2007-2035)"
    } else {
      # For percentage mode, set appropriate limits for percentages
      y_min <- 0
      y_max <- max(c(100, max(filtered_data[input$yVariables], na.rm = TRUE) * 1.1))
      y_labels <- function(x) paste0(x, "%")
      y_axis_title <- "Percentagem (%)"
      plot_title <- "Percentagem de Matrículas EPT/Ensino Médio (2007-2035)"
    }
    
    # Create the base ggplot object
    p <- ggplot(filtered_data, aes(x = ANO, color = LOCAL)) +
      labs(
        x = "Ano",
        y = y_axis_title,
        title = plot_title,
        color = "Localização"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)
      ) +
      scale_y_continuous(limits = c(y_min, y_max), labels = y_labels) +
      scale_x_continuous(breaks = c(2007, seq(2010, 2035, by = 5))) +
      scale_color_manual(values = local_colors)
    
    # Line types and Plotly annotations
    line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    annotations <- list()
    
    # Updated meta targets calculation using user preferences
    meta_targets <- tibble()
    
    for(loc in input$localInput) {
      safe_id <- str_replace_all(loc, "[^A-Za-z0-9]", "_")
      meta11_id <- paste0("use_meta11_", safe_id)
      custom_id <- paste0("custom_meta_", safe_id)
      
      # Default to Meta 11 if no inputs exist yet
      if(is.null(input[[meta11_id]]) || is.null(input[[custom_id]])) {
        # Use Meta 11 by default
        meta_absolute <- meta11_df$meta11_absolute[meta11_df$LOCAL == loc]
        if(length(meta_absolute) == 0) meta_absolute <- 0
      } else if(isTruthy(input[[meta11_id]])) {
        # Use Meta 11 target
        meta_absolute <- meta11_df$meta11_absolute[meta11_df$LOCAL == loc]
        if(length(meta_absolute) == 0) meta_absolute <- 0
      } else if(!is.na(input[[custom_id]]) && input[[custom_id]] > 0) {
        # Use custom target
        meta_absolute <- input[[custom_id]]
      } else {
        meta_absolute <- NA
      }
      
      if(!is.na(meta_absolute)) {
        meta_targets <- bind_rows(meta_targets, 
                                  tibble(LOCAL = loc, meta_target = meta_absolute))
      }
    }
    
    # Add meta lines for each location
    for (loc in input$localInput) {
      loc_target <- meta_targets %>% filter(LOCAL == loc)
      
      if (nrow(loc_target) > 0 && !is.na(loc_target$meta_target)) {
        target_absolute <- loc_target$meta_target
        loc_color <- local_colors[loc]
        if(is.na(loc_color)) loc_color <- "darkorange"
        
        if (input$dataType == "numbers") {
          # In numbers mode, show the absolute target
          target_line_value <- target_absolute
          target_label <- paste0("Meta PNE 11 - ", loc, ": ", round(target_line_value))
        } else {
          # In percentage mode, calculate what % this represents of recent population for this location
          recent_population <- combined_data %>%
            filter(LOCAL == loc, ANO >= 2020) %>%
            summarise(avg_pop = mean(`15-19_T`, na.rm = TRUE)) %>%
            pull(avg_pop)
          
          if (length(recent_population) > 0 && !is.na(recent_population) && recent_population > 0) {
            target_line_value <- (target_absolute / recent_population) * 100
            target_label <- paste0("Meta PNE 11 - ", loc, ": ", round(target_line_value, 1), "%")
          } else {
            target_line_value <- NULL
            target_label <- NULL
          }
        }
        
        # Add horizontal line for this location's target
        if (!is.null(target_line_value) && target_line_value <= y_max) {
          p <- p + 
            geom_hline(yintercept = target_line_value, 
                       linetype = "dotdash", 
                       color = loc_color, 
                       linewidth = 1.0, 
                       alpha = 0.7) +
            annotate("text", 
                     x = 2012, 
                     y = target_line_value+(0.1*target_line_value), 
                     label = paste0("Meta 11 vigente triplicar matricula EPT - ", loc),
                     color = loc_color, 
                     fontface = "bold",
                     size = 4.0,
                     alpha = 1)
        }
      }
    }
    
    # Add lines and annotations for each selected y-variable
    for (loc in unique(filtered_data$LOCAL)) {
      loc_data <- filtered_data %>% filter(LOCAL == loc)
      loc_color <- local_colors[loc]
      if(is.na(loc_color)) loc_color <- "#000000"
      
      for (i in seq_along(input$yVariables)) {
        y_var <- input$yVariables[i]
        y_sym <- sym(y_var)
        line_type <- line_types[(i - 1) %% length(line_types) + 1]
        
        # Add tooltip text to the data
        loc_data <- loc_data %>%
          mutate(
            tooltip_text = paste0(
              names(current_variables)[current_variables == y_var], "<br>",
              "Ano: ", ANO, "<br>",
              "Valor: ", if (input$dataType == "numbers") {
                round(!!y_sym)
              } else {
                paste0(round(!!y_sym, 2), "%")
              },
              "<br>Estado: ", loc
            )
          )
        
        # Add the line for each y-variable and LOCAL
        p <- p + geom_line(data = loc_data, 
                           aes(y = !!y_sym), 
                           linetype = line_type, linewidth = 1, color = loc_color)
        
        # Determine the last year for this variable
        is_enrollment <- y_var %in% c("QT_MAT_PROF_TEC_PROPAG", "QT_MAT_MED", "PCT_MAT_EPT", "PCT_MAT_MED")
        last_year <- if (is_enrollment) 2024 else 2035
        
        # Filter data to the appropriate end year for this variable
        var_data <- loc_data %>% filter(ANO <= last_year)
        
        # Get the last value for labeling
        if (nrow(var_data) > 0) {
          final_year <- max(var_data$ANO, na.rm = TRUE)
          last_value <- var_data %>%
            filter(ANO == final_year) %>%
            pull(!!y_sym)
          
          if(length(last_value) > 0 && !is.na(last_value)) {
            # Get the display name for the variable
            var_display_name <- names(current_variables)[current_variables == y_var]
            
            # Construct the label text
            label_text <- paste(var_display_name, "-", loc)
            
            # Add Plotly annotation for the label
            annotations <- append(annotations, list(
              list(
                x = final_year,
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
    }
    
    # Convert to interactive Plotly plot and add annotations
    plotly_obj <- ggplotly(p)
    plotly_obj <- plotly_obj %>% layout(annotations = annotations)
    
    return(plotly_obj)
  })
}

# Run the application
shinyApp(ui = ui, server = server)