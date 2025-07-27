library(shiny)
library(tibble)
library(shinyjs)

# --- Option descriptions ---
df_choices <- tibble::tribble(
  ~opcao,   ~amort,       ~fef,   ~inv,   ~juros,
  "II-A",   "20% ab.",    "1%",   "1%",   "0%",
  "II-B",   "10% ab.",    "1,5%", "1,5%", "0%",
  "II-C",   "Sem ab.",    "2%",   "2%",   "0%",
  "III-A",  "20% ab.",    "1%",   "0%",   "1%",
  "III-B",  "10% ab.",    "1,5%", "0,5%", "1%",
  "III-C",  "Sem ab.",    "2%",   "1%",   "1%",
  "IV-A",   "10% ab.",    "1%",   "0%",   "2%",
  "IV-B",   "Sem ab.",    "1,5%", "0,5%", "2%",
  "ND",     " ",          "NA",   "NA",   "4% (Não Adere)"
)

ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
         "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
         "RS", "RO", "RR", "SC", "SE", "SP", "TO")

opcoes <- tolower(gsub("-", "", gsub("\\.", "", df_choices$opcao)))

op_labels <- setNames(
  paste0(df_choices$opcao, ": ", df_choices$juros, " Jur., ",
         df_choices$amort, ", ", df_choices$fef, " FEF, ", df_choices$inv, " Inv."),
  opcoes
)

# --- UI ---
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  
  tags$head(includeCSS("www/custom.css")),
  
  radioButtons("selection_mode", "Modo de Seleção:",
               choices = c("Todos os Estados seguem uma Opção" = "uniform",
                           "Cada Estado escolhe uma Opção"     = "per_uf"),
               selected = "uniform", inline = TRUE),
  
  div(class = "checkbox-dark-panel matrix-wrapper",
      
      div(class = "matrix-row", style = "display: flex; align-items: center; margin-bottom: 6px;",
          div(style = "width: 240px;", ""),
          div(style = "width: 40px; text-align: center; color: #f5f5f5; font-weight: bold;", "Todos"),
          lapply(ufs, function(uf) {
            div(style = "width: 24px; text-align: center; font-size: 11px; font-weight: 500; color: #f5f5f5;", uf)
          })
      ),
      
      lapply(opcoes, function(op) {
        div(class = "matrix-row", style = "display: flex; align-items: center; margin-bottom: 2px; height: 22px;",
            div(style = "width: 240px; text-align: left; padding-right: 6px; font-size: 11px; color: #f5f5f5;",
                op_labels[[op]]
            ),
            div(style = "width: 40px; text-align: center;",
                checkboxInput(paste0("chk_all_", op), label = NULL, value = FALSE)
            ),
            lapply(ufs, function(uf) {
              div(style = "width: 24px; text-align: center; padding: 0; margin: 0;",
                  checkboxInput(inputId = paste0("chk_", op, "_", uf), label = NULL, value = FALSE)
              )
            })
        )
      })
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Track current mode
  current_mode <- reactiveVal("uniform")
  
  # Watch for mode switch
  observeEvent(input$selection_mode, {
    if (input$selection_mode != current_mode()) {
      showModal(modalDialog(
        title = "Mudar Modo de Seleção?",
        "Essa ação limpará as seleções atuais. Deseja continuar?",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_mode_change", "Sim, mudar", class = "btn-danger")
        )
      ))
    }
  })
  
  # Confirmed switch
  observeEvent(input$confirm_mode_change, {
    removeModal()
    new_mode <- isolate(input$selection_mode)
    current_mode(new_mode)
    
    # Clear all checkboxes
    for (op in opcoes) {
      updateCheckboxInput(session, paste0("chk_all_", op), value = FALSE)
      for (uf in ufs) {
        updateCheckboxInput(session, paste0("chk_", op, "_", uf), value = FALSE)
      }
    }
    
    # Enable/disable “Todos” checkboxes based on mode
    for (op in opcoes) {
      toggleState(id = paste0("chk_all_", op), condition = (new_mode == "uniform"))
    }
  })
  
  # Uniform: handle “Todos” row selection
  observe({
    for (op in opcoes) {
      local({
        op_local <- op
        observeEvent(input[[paste0("chk_all_", op_local)]], {
          req(current_mode() == "uniform")
          selected <- isTRUE(input[[paste0("chk_all_", op_local)]])
          for (other_op in setdiff(opcoes, op_local)) {
            updateCheckboxInput(session, paste0("chk_all_", other_op), value = FALSE)
            for (uf in ufs) {
              updateCheckboxInput(session, paste0("chk_", other_op, "_", uf), value = FALSE)
            }
          }
          for (uf in ufs) {
            updateCheckboxInput(session, paste0("chk_", op_local, "_", uf), value = selected)
          }
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Per-UF: column logic
  observe({
    for (uf in ufs) {
      for (op in opcoes) {
        local({
          uf_local <- uf
          op_local <- op
          id <- paste0("chk_", op_local, "_", uf_local)
          observeEvent(input[[id]], {
            req(current_mode() == "per_uf")
            if (isTRUE(input[[id]])) {
              for (other_op in setdiff(opcoes, op_local)) {
                updateCheckboxInput(session, paste0("chk_", other_op, "_", uf_local), value = FALSE)
              }
            }
          }, ignoreInit = TRUE)
        })
      }
    }
  })
}

shinyApp(ui, server)
