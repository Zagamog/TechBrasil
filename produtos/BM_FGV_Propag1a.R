library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Template UI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("sliders-h")),
      selectInput("var1a", "Filter Var1A:", choices = NULL),
      selectInput("var1b", "Filter Var1B:", choices = NULL),
      selectInput("var2a", "Filter Var2A:", choices = NULL),
      selectInput("var2b", "Filter Var2B:", choices = NULL)
    )
  ),
  dashboardBody(
    fluidRow(
      box(title = "Main Panel Placeholder", width = 12, status = "primary", solidHeader = TRUE,
          p("This is where your main content (e.g., DT, maps, summaries) will go."))
    )
  )
)

server <- function(input, output, session) {
  # Server logic will go here later
}

shinyApp(ui = ui, server = server)
