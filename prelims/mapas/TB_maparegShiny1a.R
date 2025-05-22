library(shiny)
library(sf)
library(tmap)
library(dplyr)
library(shinyWidgets)

# --- Carregamento do GeoPackage ---
gpkg_local_path <- "D:/Country/Brazil/TechBrazil/working/ibge/mapas/sf_regioes.gpkg"
sf_regioes <- st_read(dsn = gpkg_local_path, layer = "sf_regioes_ibge", quiet = TRUE)

sf_regioes$CO_UF <- as.character(sf_regioes$CO_UF)
sf_regioes$NM_RGIINTM <- as.character(sf_regioes$NM_RGIINTM)
sf_regioes$NM_RGIMED <- as.character(sf_regioes$NM_RGIMED)

default_uf <- "23"  # Ceará

# --- UI ---
ui <- fluidPage(
  titlePanel("Mapa Interativo: Regiões Geográficas IBGE"),
  sidebarLayout(
    sidebarPanel(
      h4("Filtros Hierárquicos"),
      
      pickerInput(
        "uf_input", "1. UF(s):",
        choices = sort(unique(sf_regioes$CO_UF)),
        selected = "23",  # Ceará
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      pickerInput(
        "rgiintm_input", "2. Região(ões) Intermediária(s):",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      pickerInput(
        "rgimed_input", "3. Região(ões) Imediata(s):",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      tags$hr(),
      selectInput(
        "map_level", "Colorir por:",
        choices = c("Região Intermediária" = "NM_RGIINTM", "Região Imediata" = "NM_RGIMED"),
        selected = "NM_RGIINTM"
      )
    ),
    
    mainPanel(
      tmapOutput("interactive_map", height = "800px")
    )
  )
)



# --- SERVER ---
server <- function(input, output, session) {
  
  
  # Atualiza Região Intermediária com base em UF
  observeEvent(input$uf_input, {
    if (is.null(input$uf_input) || length(input$uf_input) == 0) {
      updatePickerInput(session, "rgiintm_input", choices = character(0), selected = character(0))
      updatePickerInput(session, "rgimed_input", choices = character(0), selected = character(0))
    } else {
      uf_filtered <- sf_regioes %>% filter(CO_UF %in% input$uf_input)
      rgi_choices <- sort(unique(uf_filtered$NM_RGIINTM))
      updatePickerInput(session, "rgiintm_input", choices = rgi_choices, selected = rgi_choices)
    }
  }, ignoreInit = FALSE)
  
  # Atualiza Região Imediata com base em Região Intermediária
  observeEvent(input$rgiintm_input, {
    if (is.null(input$rgiintm_input) || length(input$rgiintm_input) == 0) {
      updatePickerInput(session, "rgimed_input", choices = character(0), selected = character(0))
    } else {
      rgi_filtered <- sf_regioes %>% filter(NM_RGIINTM %in% input$rgiintm_input)
      rgimed_choices <- sort(unique(rgi_filtered$NM_RGIMED))
      updatePickerInput(session, "rgimed_input", choices = rgimed_choices, selected = rgimed_choices)
    }
  }, ignoreInit = FALSE)
  
  # Dados filtrados para o mapa
  final_map_data <- reactive({
    data <- sf_regioes
    if (!is.null(input$uf_input) && length(input$uf_input) > 0) {
      data <- data %>% filter(CO_UF %in% input$uf_input)
    }
    if (!is.null(input$rgiintm_input) && length(input$rgiintm_input) > 0) {
      data <- data %>% filter(NM_RGIINTM %in% input$rgiintm_input)
    }
    if (!is.null(input$rgimed_input) && length(input$rgimed_input) > 0) {
      data <- data %>% filter(NM_RGIMED %in% input$rgimed_input)
    }
    data
  })
  
  # Mapa
  output$interactive_map <- renderTmap({
    req(nrow(final_map_data()) > 0)
    tmap_mode("view")
    tm_shape(final_map_data()) +
      tm_polygons(
        col = input$map_level,
        palette = "Set3",
        title = input$map_level,
        id = "NM_MUN",
        popup.vars = c("CO_UF", "NM_RGIINTM", "NM_RGIMED", "NM_MUN")
      ) +
      tm_layout(legend.outside = TRUE, frame = FALSE, bg.color = "lightblue")
  })
  
  
}

shinyApp(ui = ui, server = server)
