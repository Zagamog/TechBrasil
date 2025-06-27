library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(rmarkdown)
library(dplyr)
library(httr)
library(jsonlite)
library(dotenv)

load_dot_env("D:/AdvancedR/knowbankedu/openai/.env")
openai_key <- Sys.getenv("OPENAI_API_KEY")

ui <- dashboardPage(
  dashboardHeader(title = "Projeto Inova Bahia"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(title = "Chat com GPT", status = "primary", solidHeader = TRUE, width = 6,
          textAreaInput("mensagem", "Sua mensagem:", width = "100%", height = "100px"),
          actionButton("enviar", "Enviar"),
          htmlOutput("respostaGPT"),
          br(),
          downloadButton("baixar_pdf_chat", "Gerar PDF do Chat")
      ),
      box(title = "Histórico de Conversas", status = "info", solidHeader = TRUE, width = 6,
          DTOutput("historico_chat"),
          br(),
          downloadButton("baixar_pdf", "Gerar PDF das Linhas Selecionadas")
      )
    ),
    fluidRow(
      box(title = "Visualizador de Arquivo", status = "warning", solidHeader = TRUE, width = 12,
          selectInput("arquivo", "Selecione o Arquivo", choices = list.files("www/Inova_Bahia", full.names = FALSE)),
          uiOutput("visualizador"),
          DTOutput("tabela")
      )
    )
  )
)

server <- function(input, output, session) {
  historico <- reactiveVal(data.frame(Mensagem = character(), Resposta = character(), stringsAsFactors = FALSE))
  
  output$visualizador <- renderUI({
    req(input$arquivo)
    file_path <- paste0("www/Inova_Bahia/", input$arquivo)
    
    if (grepl("\\.pdf$", input$arquivo)) {
      tags$iframe(style = "height:700px;width:100%", src = file_path)
    } else if (grepl("\\.docx$|\\.pptx$", input$arquivo)) {
      html_file <- tempfile(fileext = ".html")
      system2("pandoc", args = c(shQuote(file_path), "-o", shQuote(html_file)))
      includeHTML(html_file)
    } else if (grepl("\\.xlsx$|\\.csv$", input$arquivo)) {
      NULL  # A tabela será mostrada no DTOutput abaixo
    } else {
      verbatimTextOutput("nao_suportado")
    }
  })
  
  output$tabela <- renderDT({
    req(input$arquivo)
    file_path <- paste0("www/Inova_Bahia/", input$arquivo)
    
    if (grepl("\\.xlsx$", input$arquivo)) {
      read_excel(file_path)
    } else if (grepl("\\.csv$", input$arquivo)) {
      read.csv(file_path)
    }
  })
  
  output$nao_suportado <- renderText("Formato ainda não suportado.")
  
  observeEvent(input$enviar, {
    req(input$mensagem)
    url <- "https://api.openai.com/v1/chat/completions"
    headers <- add_headers(Authorization = paste("Bearer", openai_key), `Content-Type` = "application/json")
    body <- toJSON(list(
      model = "gpt-4o",
      messages = list(list(role = "user", content = input$mensagem))
    ), auto_unbox = TRUE)
    
    res <- POST(url, headers, body = body)
    res_content <- content(res, as = "parsed")
    resposta_texto <- res_content$choices[[1]]$message$content
    
    resposta_html <- paste("<b>Usuário:</b> ", input$mensagem, "<br><b>Resposta GPT:</b> ", resposta_texto)
    novo_historico <- rbind(historico(), data.frame(Mensagem = input$mensagem, Resposta = resposta_texto, stringsAsFactors = FALSE))
    historico(novo_historico)
    output$respostaGPT <- renderUI(HTML(resposta_html))
  })
  
  output$historico_chat <- renderDT({
    datatable(historico(), selection = 'multiple')
  })
  
  output$baixar_pdf <- downloadHandler(
    filename = function() {
      paste0("chat_selecionado_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      selected <- input$historico_chat_rows_selected
      dados <- historico()
      if (!is.null(selected)) {
        dados <- dados[selected, ]
      }
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("chat_template.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        params = list(chat = dados),
                        envir = new.env(parent = globalenv()))
    }
  )
  
  output$baixar_pdf_chat <- downloadHandler(
    filename = function() {
      paste0("chat_completo_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      dados <- historico()
      tempReport <- tempfile(fileext = ".Rmd")
      file.copy("chat_template.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        params = list(chat = dados),
                        envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui, server)
