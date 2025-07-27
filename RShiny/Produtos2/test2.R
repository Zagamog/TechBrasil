library(shiny)
library(shinyWidgets)
library(tibble)

# Data for option descriptions
df_choices <- tibble::tribble(
  ~opcao,   ~amort,     ~fef,    ~inv,     ~juros,
  "II-A",   "20% ab.",  "1%",    "1%",     "0%",
  "II-B",   "10% ab.",  "1,5%",  "1,5%",   "0%",
  "II-C",   "Sem ab.",  "2%",    "2%",     "0%",
  "III-A",  "20% ab.",  "1%",    "0%",     "1%",
  "III-B",  "10% ab.",  "1,5%",  "0,5%",   "1%",
  "III-C",  "Sem ab.",  "2%",    "1%",     "1%",
  "IV-A",   "10% ab.",  "1%",    "0%",     "2%",
  "IV-B",   "Sem ab.",  "1,5%",  "0,5%",   "2%",
  "ND",     " ",        "NA",    "NA",     "4% (Não Adere)"
)

ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
         "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
         "RS", "RO", "RR", "SC", "SE", "SP", "TO")

opcoes <- tolower(gsub("-", "", gsub("\\.", "", df_choices$opcao)))
op_labels <- setNames(
  paste0(df_choices$opcao, ": ", df_choices$juros, " Jur., ", df_choices$amort, ", ", df_choices$fef, " FEF, ", df_choices$inv, " Inv."),
  opcoes
)

ui <- fluidPage(
  tags$head(includeCSS("www/custom.css")),
  
  div(class = "checkbox-dark-panel matrix-wrapper",
      
      # Header row
      div(class = "matrix-row", style = "display: flex; align-items: center; margin-bottom: 6px;",
          div(style = "width: 240px;", ""),  # space for Opção + desc
          div(style = "width: 40px; text-align: center; color: #f5f5f5; font-weight: bold;", "Todos"),
          lapply(ufs, function(uf) {
            div(style = "width: 24px; text-align: center; font-size: 11px; font-weight: 500; color: #f5f5f5;", uf)
          })
      ),
      
      # Matrix rows
      lapply(opcoes, function(op) {
        div(class = "matrix-row", style = "display: flex; align-items: center; margin-bottom: 2px; height: 22px;",
            # Left label with full description
            div(style = "width: 240px; text-align: left; padding-right: 6px; font-size: 11px; color: #f5f5f5;",
                op_labels[[op]]
            ),
            # “Todos” checkbox for row
            div(style = "width: 40px; text-align: center;",
                prettyCheckbox(
                  inputId = paste0("chk_all_", op),
                  label = NULL,
                  value = FALSE,
                  status = "info",
                  icon = icon("check"),
                  inline = TRUE,
                  fill = TRUE,
                  bigger = TRUE
                )
            ),
            # One checkbox per UF
            lapply(ufs, function(uf) {
              div(style = "width: 24px; text-align: center; padding: 0; margin: 0;",
                  prettyCheckbox(
                    inputId = paste0("chk_", op, "_", uf),
                    label = NULL,
                    value = FALSE,
                    status = "primary",
                    icon = icon("check"),
                    inline = TRUE,
                    fill = TRUE,
                    bigger = TRUE
                  )
              )
            })
        )
      })
  )
)

server <- function(input, output, session) {
  
  # Row-level “Todos” checkboxes
  observe({
    for (op in opcoes) {
      local({
        op_local <- op
        observeEvent(input[[paste0("chk_all_", op_local)]], {
          selected <- isTRUE(input[[paste0("chk_all_", op_local)]])
          for (uf in ufs) {
            updatePrettyCheckbox(
              session, inputId = paste0("chk_", op_local, "_", uf), value = selected
            )
          }
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Per-UF radio behavior — one option per UF
  observe({
    for (uf in ufs) {
      for (op in opcoes) {
        local({
          op_local <- op
          uf_local <- uf
          id <- paste0("chk_", op_local, "_", uf_local)
          observeEvent(input[[id]], {
            if (isTRUE(input[[id]])) {
              for (other_op in setdiff(opcoes, op_local)) {
                updatePrettyCheckbox(session, paste0("chk_", other_op, "_", uf_local), value = FALSE)
              }
            }
          }, ignoreInit = TRUE)
        })
      }
    }
  })
}

shinyApp(ui, server)
