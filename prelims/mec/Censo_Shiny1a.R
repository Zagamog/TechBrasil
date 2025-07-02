library(shiny)
library(tidyverse)

# --- Load and merge all data ---
df_all <- purrr::map_dfr(2007:2023, function(ano) {
  short <- substr(as.character(ano), 3, 4)
  file <- here::here("working", "mec_inep", paste0("df_censo", short, ".rda"))
  if (file.exists(file)) {
    load(file)
    df <- get(paste0("df_censo", short))
    df$ANO <- ano
    df
  } else {
    NULL
  }
})

# --- Unique lists of variables ---
ept_vars <- c("QT_MAT_PROF_TEC_PROPAG", "QT_MAT_PROF_TEC", "QT_MAT_PROF_TEC_SUBS", "QT_MAT_EJA_MED_TEC")
other_vars <- c("QT_MAT_MED", "QT_MAT_BAS", "QT_MAT_FUND", "QT_MAT_EJA")

# --- UI ---
ui <- fluidPage(
  titlePanel("Comparação de Matrículas por UF"),
  sidebarLayout(
    sidebarPanel(
      selectInput("uf", "Selecionar UF (NO_UF):",
                  choices = sort(unique(df_all$NO_UF)),
                  selected = "Bahia"),
      selectInput("ept_var", "Variável EPT:", choices = ept_vars, selected = "QT_MAT_PROF_TEC_PROPAG"),
      selectInput("other_var", "Outra variável:", choices = other_vars, selected = "QT_MAT_MED")
    ),
    mainPanel(
      plotOutput("seriePlot", height = "600px")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  output$seriePlot <- renderPlot({
    req(input$uf, input$ept_var, input$other_var)
    
    df_filtered <- df_all %>%
      filter(NO_UF == input$uf) %>%
      group_by(ANO) %>%
      summarise(
        EPT = sum(.data[[input$ept_var]], na.rm = TRUE),
        OUTRA = sum(.data[[input$other_var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c("EPT", "OUTRA"), names_to = "VARIAVEL", values_to = "VALOR")
    
    ggplot(df_filtered, aes(x = ANO, y = VALOR, color = VARIAVEL)) +
      geom_line(size = 1.2) +
      geom_point() +
      geom_text(data = df_filtered %>% filter(ANO == max(ANO)),
                aes(label = format(VALOR, big.mark = ",")),
                hjust = -0.1, size = 3) +
      labs(x = "Ano", y = "Total de Matrículas",
           title = paste("UF:", input$uf),
           subtitle = paste(input$ept_var, "vs", input$other_var)) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)
