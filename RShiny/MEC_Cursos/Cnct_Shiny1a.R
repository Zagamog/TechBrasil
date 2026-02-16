library(shiny)
library(shinydashboard)
library(DT)

# Load data
# Assume df_cnct2025a.rds is in working directory
df <- readRDS("df_cnct2025a.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Catálogo CNCT 2025"),
  
  dashboardSidebar(
    sidebarMenu(id = "main_nav",
                menuItem("Course Explorer", tabName = "explorer", icon = icon("book-open")),
                menuItem("Career Path Finder", tabName = "career", icon = icon("sitemap")),
                menuItem("Curriculum Viewer", tabName = "curriculum", icon = icon("clipboard-check"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "explorer",
              fluidRow(
                box(title = "Filter Courses", width = 4, solidHeader = TRUE, status = "primary",
                    selectInput("eixo", "Eixo Tecnológico", choices = c("All", unique(df[["Eixo Tecnológico"]]))),
                    uiOutput("area_ui"),
                    textInput("search", "Search in Course Name", "")
                ),
                box(title = "Matching Courses", width = 8, solidHeader = TRUE, status = "primary",
                    DTOutput("table_explorer")
                )
              )
      ),
      
      tabItem(tabName = "career",
              fluidRow(
                box(title = "Search by Keyword in Pathways", width = 4, solidHeader = TRUE, status = "info",
                    textInput("career_keyword", "Keyword (e.g., Biomedicina)", ""),
                    actionButton("find_paths", "Search")
                ),
                box(title = "Courses Mentioning This Keyword", width = 8, solidHeader = TRUE, status = "info",
                    DTOutput("career_table")
                )
              )
      ),
      
      tabItem(tabName = "curriculum",
              fluidRow(
                box(title = "Select a Course", width = 4, solidHeader = TRUE, status = "success",
                    selectInput("course_id", "Course ID", choices = df$course_id)
                ),
                box(title = "Curriculum and Requirements", width = 8, solidHeader = TRUE, status = "success",
                    verbatimTextOutput("curriculum_text")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically update Area choices based on Eixo
  output$area_ui <- renderUI({
    if (input$eixo == "All") return(NULL)
    area_choices <- unique(df[df[["Eixo Tecnológico"]] == input$eixo, "Área Tecnológica"])
    selectInput("area", "Área Tecnológica", choices = c("All", area_choices))
  })
  
  # Filtered table for Explorer
  output$table_explorer <- renderDT({
    filtered <- df
    if (input$eixo != "All") filtered <- filtered[filtered[["Eixo Tecnológico"]] == input$eixo, ]
    if (!is.null(input$area) && input$area != "All") filtered <- filtered[filtered[["Área Tecnológica"]] == input$area, ]
    if (input$search != "") {
      filtered <- filtered[grepl(input$search, filtered[["Denominação do Curso"]], ignore.case = TRUE), ]
    }
    datatable(filtered[, c("course_id", "Denominação do Curso", "Eixo Tecnológico", "Área Tecnológica", "Carga Horária Mínima")],
              options = list(pageLength = 10))
  })
  
  # Keyword search in Itinerários Formativos
  observeEvent(input$find_paths, {
    keyword <- tolower(input$career_keyword)
    matched <- df[grepl(keyword, tolower(df[["Itinerários Formativos"]])), ]
    output$career_table <- renderDT({
      datatable(matched[, c("course_id", "Denominação do Curso", "Eixo Tecnológico", "Área Tecnológica")],
                options = list(pageLength = 10))
    })
  })
  
  # Curriculum tab output
  output$curriculum_text <- renderText({
    selected <- df[df$course_id == input$course_id, ]
    if (nrow(selected) == 0) return("No match.")
    paste0(
      "\nCourse: ", selected[["Denominação do Curso"]],
      "\n\nPerfil Profissional:\n", selected[["Perfil Profissional de Conclusão"]],
      "\n\nInfraestrutura:\n", selected[["Infraestrutura Mínima"]],
      "\n\nLegislação:\n", selected[["Legislação Profissional"]]
    )
  })
}

shinyApp(ui, server)
