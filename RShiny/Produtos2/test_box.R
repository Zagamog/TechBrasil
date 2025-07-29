library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$head(includeCSS("www/custom.css")),  # ← Loads your persistent CSS
  
  titlePanel("Pretty Checkbox Styling Test"),
  
  prettyCheckboxGroup(
    inputId = "choice_test",
    label = "Select options below",
    choices = c("Option A" = "A", "Option B" = "B", "Option C" = "C"),
    selected = c("A", "C"),
    icon = icon("check"),
    fill = TRUE,
    status = "info",
    bigger = TRUE,
    thick=TRUE,
    animation = "pulse",
    inline = TRUE,
    )
)

server <- function(input, output, session) {}

shinyApp(ui, server)

