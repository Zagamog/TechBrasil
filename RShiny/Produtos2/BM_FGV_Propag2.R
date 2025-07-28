# BM_FGV_Propag2.R
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(ggtext)
library(DT)
library(scales)
library(patchwork)
library(dplyr)
library(tidyr)
library(purrr)
library(RColorBrewer)
library(shinyjs)
library(shinyWidgets)

options(warn=-1) # Too many pesky warnings, terrain, terrain, terrain, pull up, pull up 

#############################################################
# LOAD DATA AND ANY NEEDED DATA OPERATIONS
#############################################################


# Load Propag scraped data
propag_ept_financeiro <- readRDS("propag_ept_financeiro.rds")

# Load Censo Escolar data 2007 to 2024 UF aggregates
load("df_censo_UF.rda")
sg_ufs <- sort(unique(na.omit(df_censo_UF$SG_UF)))

# Load calculations of PNE meta11a in the script: # Censo_UF_garabed1b.R 
load("meta11a_opcoes.rda")  

# Get State names for display
nome_ufs <- sort(unique(df_censo_UF$NM_UF))  # Ensure sorted and unique

# Load options
load("dfcen_val.rda")  # make sure A,G,I,J exist

# Load propag fin data by option
load("df_2a.rda")
load("df_2b.rda")
load("df_2c.rda")

load("df_3a.rda")
load("df_3b.rda")
load("df_3c.rda")

load("df_4a.rda")
load("df_4b.rda")

# Load df_nd for "Não Adere" option
load("df_nd.rda")


# map from option names → data frames
df_list <- list(
  "II-A" = df_2a, "II-B" = df_2b, "II-C" = df_2c,
  "III-A"= df_3a, "III-B"= df_3b, "III-C"= df_3c,
  "IV-A" = df_4a, "IV-B" = df_4b,
  "ND"   = df_nd
)


df_choices <- tibble::tribble(
  ~opcao,   ~amort,            ~fef,    ~inv,     ~juros,
  "II-A",   "20% abatimento",  "1%",    "1%",     "0%",
  "II-B",   "10% abatimento",  "1,5%",  "1,5%",   "0%",
  "II-C",   "Sem abatimento",  "2%",    "2%",     "0%",
  "III-A",  "20% abatimento",  "1%",    "0%",     "1%",
  "III-B",  "10% abatimento",  "1,5%",  "0,5%",   "1%",
  "III-C",  "Sem abatimento",  "2%",    "1%",     "1%",
  "IV-A",   "10% abatimento",  "1%",    "0%",     "2%",
  "IV-B",   "Sem abatimento",  "1,5%",  "0,5%",   "2%",
  "ND",     " ",              "NA",    "NA",     "4% (Não Adere)"
)

named_scenarios <- setNames(
  df_choices$opcao,
  paste0(
    df_choices$opcao, ": ",
    df_choices$amort, ", ",
    df_choices$fef, " FEF, ",
    df_choices$inv, " Invest., ",
    df_choices$juros, " Juros"
  )
)


opcoes <- tolower(gsub("-", "", gsub("\\.", "", df_choices$opcao)))

op_labels <- setNames(
  paste0(df_choices$opcao, ": ", df_choices$juros, " Jur., ",
         df_choices$amort, ", ", df_choices$fef, " FEF, ", df_choices$inv, " Inv."),
  opcoes
)

# To assign fixed colors to UFs

# prepare your UF‐color mapping up front
library(RColorBrewer)
uf_levels <- sort(unique(df_2a$NM_UF))  # or pull from any of them

# Colors by NM_UF
uf_colors <- setNames(
  colorRampPalette(brewer.pal(9, "Set1"))(length(uf_levels)),
  uf_levels
)

# 1. Criar mapa UF → NM_UF (sigla para nome completo)
uf_name_map <- df_censo_UF %>%
  distinct(UF = SG_UF, Estado = NM_UF) %>%
  filter(Estado %in% names(uf_colors))  # para garantir que o nome tem cor

# 2. Criar novo vetor com nomes de UF e cores baseadas no nome completo
uf_colors_bySG <- setNames(
  uf_colors[uf_name_map$Estado],
  uf_name_map$UF
)


library(colorspace)

uf_colors_compare <- map_chr(uf_colors, ~ lighten(.x, amount = 0.4))

# --- Define variable choices for Oferta EPT ---
ept_vars <- c("QT_MAT_PROF_TEC_PROPAG", "QT_MAT_TEC_NUM2", "QT_MAT_TEC_NUM3" )
other_vars <- c("QT_MAT_MED")



# Define the `%||%` operator which returns the first non-null value of a pair of values 

`%||%` <- function(a, b) if (!is.null(a)) a else b


# For Tab 1 on financial variables
var_labels <- list(
  "saldo_mar25"           = "Saldo março de 2025",
  "amort_extr"            = "Amortizações extraordinárias - 20 % do saldo",
  "FEF_1ano_liq_cen01"    = "Fundo FEF – fluxo líquido 1 ano – cenário I",
  "FEF_1ano_liq_cen02"    = "Fundo FEF – fluxo líquido 1 ano – cenário II",
  "FEF_5ano_liq_cen01"    = "Fundo FEF – fluxo líquido 5 anos – cenário I",
  "FEF_5ano_liq_cen02"    = "Fundo FEF – fluxo líquido 5 anos – cenário II",
  "EPT_1ano_cen01"        = "Investimento EPT – 1 ano – cenário I",
  "EPT_1ano_cen02"        = "Investimento EPT – 1 ano – cenário II",
  "EPT_5ano_cen01"        = "Investimento EPT – 5 anos – cenário I",
  "EPT_5ano_cen02"        = "Investimento EPT – 5 anos – cenário II"
)

# Define allowed variables and their order
allowed_vars <- c(
  "saldo_mar25",
  "amort_extr",
  "EPT_1ano_cen01",
  "EPT_1ano_cen02",
  "EPT_5ano_cen01",
  "EPT_5ano_cen02",
  "FEF_1ano_liq_cen01",
  "FEF_1ano_liq_cen02",
  "FEF_5ano_liq_cen01",
  "FEF_5ano_liq_cen02"
)

# Create a named vector: label = varname
fin_choices <- setNames(allowed_vars, sapply(allowed_vars, function(v) var_labels[[v]] %||% v))


# DF of financial data for DT in Tab 1 on Financials
# Removes fef_share_pct column and converts Brazilian style numeric columns to numeric type
# Also adds a total row at the end with sums of each numeric column

financeiro_dt_all <- {
  df <- propag_ept_financeiro
  df <- df[, !(names(df) %in% "fef_share_pct")]
  data_cols <- setdiff(names(df), c("UF", "Estado"))
  df[data_cols] <- lapply(df[data_cols], function(x) suppressWarnings(as.numeric(gsub(",", "", x))))
  
  total_row <- as.list(rep(NA, ncol(df)))
  names(total_row) <- names(df)
  for (col in data_cols) {
    total_row[[col]] <- sum(df[[col]], na.rm = TRUE)
  }
  total_row$UF <- "Todos"
  total_row$Estado <- "Todos"
  
  df_final <- rbind(df, as.data.frame(total_row, stringsAsFactors = FALSE))
  df_final
}



# UI drop-in replacement (only the ui object)
ui <- dashboardPage(

#  Using Shiny dashboard template but some modifications, here dispensing with sidebar and header
  
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
##############################################################################################################
# CUSTOMIZATION OF CSS
# Introducing custom CSS and styles, including www/custom.css for viridis panel tab colors
# Need some css style elements here, because custom.css gets overriden by Bootstrap defaults
###############################################################################################################

useShinyjs(),
# jQuery‑UI (for draggable)
tags$head(tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js")),

    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tags$head(
      tags$style(HTML("
        .main-header { display: none; }
        .content-wrapper, .right-side { margin-top: 0px !important; }
      ")), 
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tags$head(
      tags$style(HTML("
    /* Force all checkbox labels to black */
    .form-group.shiny-input-checkbox label,
    .checkbox label,
    .checkbox-inline label,
    .shiny-input-container input[type='checkbox'] + span {
      color: black !important;
    }

    /* Existing label styling */
    label,
    .selectize-control.single .selectize-input,
    .irs-single,
    .irs-min,
    .irs-max,
    .irs-grid-text,
    .shiny-input-container > .control-label {
      color: black !important;
    }

    /* Slider tweaks */
    .irs {
      background-color: transparent !important;
    }
    .irs-bar,
    .irs-line {
      background: #ccc !important;
    }
    .irs-slider {
      background-color: #555 !important;
      border: 1px solid #999 !important;
    }
    .irs-grid-text {
      color: #333 !important;
      font-weight: bold;
    }
  "))
    ),
  tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.datatables.net/buttons/2.4.1/css/buttons.dataTables.min.css"),
      tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/dataTables.buttons.min.js"),
      tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.flash.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.1.3/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.53/pdfmake.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.53/vfs_fonts.js"),
      tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.html5.min.js"),
      tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.print.min.js")
    ),
    # Header title area
    tags$div(
      style = "padding: 10px 20px; font-size: 24px; font-weight: bold; color: #0000ff;",
      HTML('Faça adesão ao <span style="color: #FFD700; text-shadow: 1px 1px #333;">Propag</span> !')
    ),
  
  
  
    fluidRow(
      
      ## TITLE  LINE
      column(
        12,
        div(
          style = "background-color: #1f5673; padding: 10px; text-align: center; color: white; font-weight: bold; font-size: 16px; margin-bottom: 10px;",
          "Ferramenta de apoio",
          span("analítico ", style = "color: #ff6619;"),
          "usando a",
          span("Inteligência Artificial", style = "color: #ffcc00;"),
          " – desenvolvida por uma equipe do",
          span("Banco Mundial", style = "color: #ffcc00;"),
          " e ",
          span("FGV/DGPE", style = "color: #ffcc00;")
        )
      )
    ),
    
    tabsetPanel(id = "tab_selection", selected = "Retorno FEF por opções",
           ### UI - TAB 1 : FINANCE ##################################################
                  tabPanel("Tema Financiero",
                         fluidPage(
                           h3("Visualização Financeira do PROPAG", style = "color: #1f5673; font-weight: bold;"),
                           
                           fluidRow(
                             column(6,
                                    selectizeInput(
                                      "fin_variable",
                                      "Selecionar variável financeira",
                                      choices = fin_choices,
                                      selected = "FEF_5ano_liq_cen01",
                                      width = "100%"
                                    )
                             )
                           ),
                           
                           plotOutput("tab1_fin_plot", height = "600px"),
                           br(),
                           h3("Tabela 1: Variáveis Financeiras", style = "color: #1f5673; font-weight: bold; margin-top: 30px;"),
                           DT::dataTableOutput("tab1_fin_table"),
                           br(), br(),
                           div(
                             style = "font-size: 12px; color: #999; text-align: center; padding: 10px;",
                             HTML(
                               "<strong>Para dúvidas ou consultas:</strong><br>
               Envie mensagem para:<br>
               <strong>Equipe FGV/DGPE:</strong> <a href='mailto:blix@fgv.br' style='color:#999;'>blix@fgv.br</a><br>
               <strong>Equipe BM:</strong> <a href='mailto:blax@worldbank.org' style='color:#999;'>blax@worldbank.org</a>"
                             )
                           )
                         )
                ),
           
           ### UI - TAB 1b : FINANCE 1b  ##################################################        
           ### TAB 1B (UPDATED)
           ### TAB 1B (UPDATED)
           tabPanel(
             "Finance 1b",
             fluidPage(
               h3("Conteúdo em desenvolvimento: Finance 1b",
                  style = "color: #1f5673; font-weight: bold;"),
               
               # JS: enforce single selection per group
               tags$head(
                 tags$script(HTML("
        $(document).on('shiny:connected', function () {
          ['A','G','I','J'].forEach(function(dim){
            $(document).on('click', '#choice_' + dim + ' input[type=checkbox]', function () {
              var $grp = $('#choice_' + dim);
              $grp.find('input[type=checkbox]').not(this).prop('checked', false);
              $(this).trigger('change');
            });
          });
        });
      "))
               ),
               
               
               tags$head(
                 tags$script(HTML("
    // lock/unlock A,G,I when 'Não Adere' is chosen
    Shiny.addCustomMessageHandler('toggleAGI', function(lock){
      ['A','G','I'].forEach(function(dim){
        var $grp = $('#choice_' + dim);
        $grp.find('input[type=checkbox]').prop('disabled', lock);
        // optional visual dimming
        $grp.css('opacity', lock ? 0.35 : 1);
      });
    });
  "))
               ),
  
               tags$head(tags$style(HTML("
  .pretty input + label {
    font-size: 16px !important;
  }
"))),
                            
               
               div(class = "checkbox-dark-panel",
                   # ---- ROW 1: UF + 4 groups + summary (all in one row) ----
                   fluidRow(
                     column(
                       width = 12,
                       div(
                         style = "margin-top: 5px; margin-bottom: 10px; color: #f5f5f5; text-align: justify; font-size: 20px;",
                         tagList(
                           tags$strong("Instruções: "),
                           "Por favor, primeiro, seleccione o seu Estado, e depois, marque as opções relevantes para o seu Estado. ",
                           "À medida que você clicar em uma escolha, o painel à direita mostrará ",
                           "apenas opções válidas que podem ser escolhidas. ",
                           "Você pode começar por qualquer uma das quatro escolhas: ",
                           tags$span(class = "highlighted-note", 
                                     "(i) Abatimento (0%, 10% ou 20%) ; (ii) Aporte ao FEF (1%, 1.5% ou 2%); (iii) Investimento Direto (cinco opções);
                                     (iv) Taxa de juro (0%, 1% ou 2%)")
                         )
                       )
                     )
                   ),
                   
                   
                   fluidRow(
                     # UF selector
                     column(
                       width = 2,
                       tags$label("Selecione a UF:",
                                  style = "font-weight: bold; color: #f5f5f5; font-size: 16px;"),
                       selectizeInput("uf_select", label = NULL, choices = nome_ufs)
                     ),
                     
                     # Four groups
                     column(
                       width = 8,
                       fluidRow(
                         column(
                           width = 3,
                           tags$label("(iv) Taxa de Juros:",
                                      style = "font-weight: bold; display: block;"),
                           prettyCheckboxGroup(
                             inputId  = "choice_J",
                             label    = NULL,
                             choices  = c("0%" = "J1",
                                          "1%" = "J2",
                                          "2%" = "J3",
                                          "4% (Não Adere)" = "J4"),
                             selected = character(0),
                             icon     = icon("check"),
                             fill     = TRUE,
                             status   = "danger",
                             bigger   = TRUE,
                             inline   = TRUE
                           )
                         ),
                         
                         column(
                           width = 3,
                           tags$label("(i) Amortização Inicial",
                                      style = "font-weight: bold; display: block;"),
                           prettyCheckboxGroup(
                             inputId  = "choice_A",
                             label    = NULL,
                             choices  = c("Sem abatimento" = "A1",
                                          "10% abatimento" = "A2",
                                          "20% abatimento" = "A3"),
                             selected = character(0),
                             icon     = icon("check"),
                             fill     = TRUE,
                             status   = "info",
                             bigger   = TRUE,
                             inline   = TRUE
                           )
                         ),
                         column(
                           width = 2,
                           tags$label("(ii) Contribuição para FEF:",
                                      style = "font-weight: bold; display: block;"),
                           prettyCheckboxGroup(
                             inputId  = "choice_G",
                             label    = NULL,
                             choices  = c("1%" = "G1",
                                          "1.5%" = "G2",
                                          "2%" = "G3"),
                             selected = character(0),
                             icon     = icon("check"),
                             fill     = TRUE,
                             status   = "primary",
                             bigger   = TRUE,
                             inline   = TRUE
                           )
                         ),
                         column(
                           width = 3,
                           tags$label("(iii) Investimento Direto:",
                                      style = "font-weight: bold; display: block;"),
                           # UI  (group III – Investimento Direto)
                           prettyCheckboxGroup(
                             inputId  = "choice_I",
                             label    = NULL,
                             choices  = c("0%"   = "I1",
                                          "0,5%" = "I2",
                                          "1%"   = "I3",
                                          "1,5%" = "I4",
                                          "2%"   = "I5"),
                             selected = character(0),
                             icon     = icon("check"),
                             fill     = TRUE,
                             status   = "success",
                             bigger   = TRUE,
                             inline   = TRUE
                           )
                             )
          
                       )
                     ),
                     
                     # Summary column (same row)
                     column(
                       width = 2,
                       div(
                         style = "color: #f5f5f5; font-style: italic; font-size: 16px; text-align: center;",
                         uiOutput("choice_summary")
                       )
                     )
                   )  # end fluidRow 2
                   

               )
               
               
             ),
             # Explanation text + selectizeInput with updated font
             fluidRow(
               # Full-width explanation text
               column(
                 width = 12,
                 div(
                   style = "margin-top: 0px; margin-bottom: 5px; color: #1f5673; text-align: justify; font-size: 20px;",
                   HTML("<strong>Nota:</strong> Uma vez selecionada a opção arriba, você pode selecionar entre quatro variaveis para ver a evolução para 
                        o period estimado do Propag: 2025 ate 2054")
                 )
               ),
               
               # Select input with matching font style
               column(
                 width = 2,
                 div(
                   style = "margin-bottom: 10px;",
                   tags$label("Selecionar a variavel:", style = "font-weight: bold; color: #1f5673; font-size: 18px;"),
                   selectizeInput("var_select", label = NULL,
                                  choices = c(
                                    "Saldo da Dívida"     = "Saldo",
                                    "Aporte para o FEF"   = "ApoFEF",
                                    "Investimento Direto" = "InvDir",
                                    "Juros Pagos"         = "JurPag"
                                  ), selected = "Saldo"
                   )
                 )
               ),
               column(
                 width = 2,
                 div(
                   style = "margin-bottom: 10px;",
                   tags$label("Comparar com outro cenário:", 
                              style = "font-weight: bold; color: #1f5673; font-size: 18px; display: block;"),
                   selectizeInput(
                     inputId  = "compare_with",
                     label    = NULL,
                     choices  = c("Nenhum" = "", named_scenarios),
                     selected = "",
                     options  = list(
                       placeholder = "Escolha um cenário para comparar...",
                       allowEmptyOption = TRUE,
                       onInitialize = I('function() { this.clear(); }'),
                       persist = FALSE,
                       closeAfterSelect = TRUE
                     )
                   )
                   
                 )
               ),
               column(
                 width = 4,
                 div(
                   style = "margin-bottom: 10px; display: flex; align-items: flex-start; gap: 20px;",
                   
                   # Top-aligned label
                   tags$label("Selecionar intervalo de anos:",
                              style = "font-weight: bold; color: #1f5673; font-size: 18px; margin-top: 5px; white-space: nowrap;"),
                   
                   # Slider takes remaining space
                   div(
                     style = "flex-grow: 1;",
                     sliderInput(
                       inputId = "year_range",
                       label   = NULL,
                       min     = 2025,
                       max     = 2054,
                       value   = c(2025, 2054),
                       step    = 1,
                       sep     = ""
                     )
                   )
                 )
               )
               
               
               
               
               
             ),
             
             
             fluidRow(
               column(12,
                        plotOutput("PloTab1b",height = "600px", width = "100%")
               )
             )
             
             
             
           ),
           
           
           ### UI - TAB 2 : FEF Opçoes  ##################################################      
           tabPanel("Retorno FEF por opções",
                    fluidPage(
                      useShinyjs(),
                      div(class = "checkbox-dark-panel",
                      # ---- Instruction block (inserted first) ----
                      fluidRow(
                        column(
                          width = 12,
                          div(
                            style = "margin-top: 5px; margin-bottom: 10px; color: #f5f5f5; text-align: justify; font-size: 20px;",
                            tagList(
                              tags$strong("Instruções: "),
                              "Nesta aba, você pode simular a escolha de opções pelas UFs no contexto do PROPAG, ",
                              "especificamente para calcular a contribuição total ao Fundo de Equalização Fiscal (FEF). ",
                              "Você pode trabalhar em dois modos distintos: ",
                              tags$span(class = "highlighted-note",
                                        "(i) Todos os Estados seguem uma mesma opção ; (ii) Cada Estado escolhe sua própria opção. "),
                              "A seleção feita aqui impacta diretamente a projeção do tamanho do FEF e seu fluxo líquido ao longo dos anos.",
                              
                              # Inline radio buttons follow directly
                              div(
                                style = "margin-top: 5px; font-size: 18px;",
                                radioButtons("selection_mode", label = NULL,
                                             choices = c("Todos os Estados seguem uma Opção" = "uniform",
                                                         "Cada Estado escolhe uma Opção"     = "per_uf"),
                                             selected = "uniform", inline = TRUE
                                )
                              )
                            )
                          )
                        )  # end of column
                        
                        
                      ), # end of fluid rows
                      

                      
                      div(class = "checkbox-dark-panel matrix-wrapper",
                          
                          # Header row
                          div(class = "matrix-row", style = "display: flex; align-items: center; margin-bottom: 6px;",
                              div(style = "width: 300px;", ""),  # Opção + description
                              div(style = "width: 40px; text-align: center; color: #f5f5f5; font-weight: bold;", "Todos"),
                              lapply(sg_ufs, function(uf) {
                                div(style = "width: 24px; text-align: center; font-size: 14px; font-weight: 500; color: #f5f5f5;", uf)
                              })
                          ),
                          
                          # Matrix rows per Opção
                          lapply(opcoes, function(op) {
                            div(class = "matrix-row", style = "display: flex; align-items: center; margin-bottom: 2px; height: 22px;",
                                div(style = "width: 300px; text-align: left; padding-right: 6px; font-size: 14px; color: #f5f5f5;",
                                    op_labels[[op]]
                                ),
                                div(style = "width: 40px; text-align: center;",
                                    checkboxInput(paste0("chk_all_", op), label = NULL, value = FALSE)
                                ),
                                lapply(sg_ufs, function(uf) {
                                  div(style = "width: 24px; text-align: center; padding: 0; margin: 0;",
                                      checkboxInput(inputId = paste0("chk_", op, "_", uf), label = NULL, value = FALSE)
                                  )
                                })
                            )
                          })
                      ),
                      div(style = "margin-top: -20px; margin-bottom: 10px; text-align: justify; font-size: 40px;"),
                              fluidRow(
                        column(4,
                               selectInput(
                                 "uf_select",
                                 label = tags$span("Escolha uma UF para visualizar os fluxos do FEF:", style = "color: white;"),
                                 choices = nome_ufs,
                                 selected = "Alagoas"
                               )
                        ),
                        column(
                          width = 4,
                          div(
                            style = "margin-bottom: 10px; display: flex; align-items: flex-start; gap: 20px;",
                            
                            # Top-aligned label
                            tags$label("Selecionar intervalo de anos:",
                                       style = "font-weight: bold; color: #1f5673; font-size: 18px; margin-top: 5px; white-space: nowrap;"),
                            
                            # Slider takes remaining space
                            div(
                              style = "flex-grow: 1;",
                              sliderInput(
                                inputId = "year_range",
                                label   = NULL,
                                min     = 2025,
                                max     = 2054,
                                value   = c(2025, 2054),
                                step    = 1,
                                sep     = ""
                              )
                            )
                          )
                        )
                        
                        
                        
                      )
    
                    ),
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    fluidRow(
                      column(12,
                             plotOutput("plotab2", height = "450px")
                      )
                    )# end of div for checkbox-dark-panel
                    ) # end of fluidPage
           ), # end of tabPanel for FEF options
 
    
           
           
           
           
           
           
           
           
           
           
           
           
           
              
           ### UI - TAB 3 : META 11 VIGENTE  ##################################################
           
                tabPanel("Situação - Meta 11 (vigente)",
                         fluidPage(
                           h3("Evolução da Oferta de EPT por UF", style = "color: #1f5673; font-weight: bold;"),
                           fluidRow(
                             column(4,
                              selectizeInput("oferta_uf", "Selecionar UF ou Brasil:",
               choices = c("Brasil", sort(unique(meta11a_opcoes$NM_UF))),
               selected = "Rio Grande do Norte")

                             ),
                             column(4,
                                    selectizeInput("oferta_ept_var", "Variável EPT:",
                                                   choices = ept_vars,
                                                   selected = "QT_MAT_PROF_TEC_PROPAG")
                             ),
                             column(4,
                                    selectizeInput("oferta_other_var", "Outra variável:",
                                                   choices = other_vars,
                                                   selected = "QT_MAT_MED")
                             )
                           ),
                           plotOutput("oferta_ept_plot", height = "500px"),
                           br(),
                           h3("Tabela de Dados (2007–2024)", style = "color: #1f5673; font-weight: bold;"),
                           DTOutput("oferta_ept_table")
                         )
                ),
                
           ### UI - TAB 4 : META 11a NOVA ##################################################
           
                tabPanel("Situação - Meta 11a",
                         fluidPage(
                           h3("Meta 11a – Comparação das Definições", style = "color: #1f5673; font-weight: bold;"),
                           
                           fluidRow(
                             column(3,
                                    selectizeInput(
                                      inputId = "meta11a_nova_uf",
                                      label = "Selecionar UF or Brasil:",
                                      choices = sort(unique(meta11a_opcoes$NM_UF)),
                                      selected = "Rio de Janeiro"
                                    )
                             ),
                             column(3,
                                    checkboxGroupInput(
                                      inputId = "meta11a_nova_definicoes",
                                      label = "Escolher definições para comparar:",
                                      choices = c("Meta11a_opcao1", "Meta11a_opcao2", "Meta11a_opcao3"),
                                      selected = c("Meta11a_opcao1", "Meta11a_opcao2", "Meta11a_opcao3"),
                                      inline = TRUE
                                    )
                             ),
                             column(3,  # New block for projection inputs
                                    selectInput(
                                      inputId = "meta11a_target_year",
                                      label = "Ano alvo para atingir 50%:",
                                      choices = 2025:2035,
                                      selected = 2030
                                    )),
                             column(3,  # New block for projection inputs
                                    sliderInput(
                                      inputId = "ensino_slope_factor",
                                      label = "Ajuste no crescimento do EM:",
                                      min = 0.5, max = 1.5, step = 0.1, value = 1
                                    )
                             )
                           ),
                           
                           plotOutput("meta11a_nova_plot", height = "600px")
                         )
                       
                ),
                
           ### UI - TAB 5 : SITUAÇÃO - META 11a - PÚBLICO ##################################################    
           
           ## ONLY PROPAG VARIANT OF META11a APT
                
                tabPanel("Situação - Meta 11a - Público",
                         fluidPage(
                           h3("Meta 11a: EPT como % do Ensino Médio", style = "color: #1f5673; font-weight: bold;"),
                           
                           fluidRow(
                             column(2,
                                    selectizeInput("meta11a_uf", "Selecionar UF:", choices = sort(unique(df_censo_UF$NM_UF)), selected = "Rio Grande do Norte")
                             ),
                             column(2,
                                    selectizeInput("meta11a_ept_var2", "Variável EPT:", choices = c("QT_MAT_PROF_TEC_PROPAG"), selected = "QT_MAT_PROF_TEC_PROPAG")
                             ),
                             column(2,
                                    selectizeInput("meta11a_med_var2", "Variável Ensino Médio:", choices = c("QT_MAT_MED"), selected = "QT_MAT_MED")
                             )
                             
                           ),
                           
                           plotOutput("meta11a_plot2", height = "500px"),
                           br()
                           
                         )
                ),
                
                
                
                tabPanel("Painel 6", h4("Placeholder Content for Panel ") ),
                tabPanel("Painel 7", h4("Placeholder Content for Panel 7")),
                tabPanel("Estatísticas Relevantes", DTOutput("P8_table"))
    )
  )
)

########################################################################
#  SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER 
########################################################################

server <- function(input, output, session) {


### REACTIVES OF USE ACROSS TABS
##################  

  
  ######
  # Reactive data for Meta 11a - input of UF and variable choice for computing  first part
  
  RKT_meta11a_data <- reactive({
    req(input$meta11a_uf, input$meta11a_ept_var2, input$meta11a_med_var2)
    
    df <- df_censo_UF |>
      filter(NM_UF == input$meta11a_uf, AGREG == "UF_TUDO") |>
      group_by(ANO) |>
      summarise(
        EPT = sum(.data[[input$meta11a_ept_var2]], na.rm = TRUE),
        MEDIO = sum(.data[[input$meta11a_med_var2]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(PCT_EPT = ifelse(MEDIO > 0, 100 * EPT / MEDIO, NA))
    
    return(df)
  })
  
  
  
  
    
  
  # Reactive data for Meta 11a - input of UF and variable choice for computing second part
  # Meta 11a second part
  
  # Share of Expansion of public can be greater than 100 because private enrollment might decline,
  # so public enrollment increase can exceed the total enrollment increase.
  
  
  RKT_meta11a_expansao_publica_hist <- reactive({
    req(input$meta11a_uf)
    req(input$meta11a_ept_var2 == "QT_MAT_PROF_TEC_PROPAG")
    req(input$meta11a_med_var2 == "QT_MAT_MED")  
    
    df_total <- df_censo_UF |>
      filter(NM_UF == input$meta11a_uf, AGREG == "UF_TUDO") |>
      group_by(ANO) |>
      summarise(EPT_TOTAL = sum(.data[[input$meta11a_ept_var2]], na.rm = TRUE), .groups = "drop")
    
    df_pub <- df_censo_UF |>
      filter(NM_UF == input$meta11a_uf, AGREG == "UF_PUB") |>
      group_by(ANO) |>
      summarise(EPT_PUB = sum(.data[[input$meta11a_ept_var2]], na.rm = TRUE), .groups = "drop")
    
    df_joined <- full_join(df_total, df_pub, by = "ANO") |>
      arrange(ANO) |>
      mutate(
        DELTA_TOTAL = pmax(EPT_TOTAL - lag(EPT_TOTAL), 0),
        DELTA_PUB = pmax(EPT_PUB - lag(EPT_PUB), 0),
        SHARE_PUB = ifelse(DELTA_TOTAL > 0, DELTA_PUB / DELTA_TOTAL, NA)
      ) |>
      filter(!is.na(ANO), ANO >= 2008, ANO <= 2035)  # include up to 2035
    
    return(df_joined)
  })  
  
  
  
    
## OBSERVE EVENTS ETC. ACROSS TABS
  
##################
  
  
    
######### TAB1 TAB1 TAB1 TAB1  TAB1 TAB1 TAB1 TAB1  TAB1 TAB1 TAB1 TAB1  TAB1 TAB1 TAB1 TAB1  TAB1 TAB1 TAB1 TAB1  TAB1 TAB1 TAB1 TAB1  
##############################################################################################################################
### OUTPUT  PLOT TAB 1  FINANCIALS TAB 1  FINANCIALS  TAB 1  FINANCIALS  TAB 1  FINANCIALS TAB 1  FINANCIALS  TAB 1  FINANCIALS  TAB 1  FINANCIALS 
##############################################################################################################################
  
  output$tab1_fin_plot <- renderPlot({
    library(patchwork)
    
    req(input$fin_variable)
    
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    df <- propag_ept_financeiro
    df$valor <- suppressWarnings(as.numeric(gsub(",", "", df[[input$fin_variable]])))
    
    uf_endividado <- c("MG", "SP", "RJ", "RS")
    
    df_endividado <- df[df$UF %in% uf_endividado, ]
    df_geral <- df[!df$UF %in% uf_endividado, ]
    
    df_geral <- df_geral[order(df_geral$Estado), ]
    df_endividado <- df_endividado[order(df_endividado$Estado), ]
    
    plot_label <- var_labels[[input$fin_variable]] %||% input$fin_variable
    
    # ----- CONDITIONAL Y-AXIS SETTINGS -----
    if (input$fin_variable == "saldo_mar25") {
      y_limits_geral <- c(0, 22e9)
      y_breaks_geral <- seq(0, 22e9, by = 1e9)
      
      y_limits_divida <- c(0, 375e9)
      y_breaks_divida <- seq(0, 375e9, by = 100e9)
      
    } else if (input$fin_variable == "amort_extr") {
      y_limits_geral <- c(0, 5e9)
      y_breaks_geral <- seq(0, 5e9, by = 0.5e9)
      
      y_limits_divida <- c(0, 75e9)
      y_breaks_divida <- seq(0, 75e9, by = 5e9)
      
    }  else if (input$fin_variable == "EPT_1ano_cen01") {
      y_limits_geral <- c(0, 120e6)
      y_breaks_geral <- seq(0, 120e6, by = 20e6)
      
      y_limits_divida <- c(0, 1.75e9)
      y_breaks_divida <- seq(0, 1.75e9, by = 250e6)
    }  
    
    else if (input$fin_variable == "EPT_1ano_cen02") {
      y_limits_geral <- c(0, 200e6)
      y_breaks_geral <- seq(0, 200e6, by = 25e6)
      
      y_limits_divida <- c(0, 3.5e9)
      y_breaks_divida <- seq(0, 3.5e9, by = 500e6)
    }
    
    
    else if (input$fin_variable == "EPT_5ano_cen01") {
      y_limits_geral <- c(0, 600e6)
      y_breaks_geral <- seq(0, 600e6, by = 100e6)
      
      y_limits_divida <- c(0, 85e8)
      y_breaks_divida <- seq(0, 85e8, by = 1e9)
    }
    
    else if (input$fin_variable == "EPT_5ano_cen02") {
      y_limits_geral <- c(0, 1000e6)
      y_breaks_geral <- seq(0, 1000e6, by = 100e6)
      
      y_limits_divida <- c(0, 175e8)
      y_breaks_divida <- seq(0, 175e8, by = 1e9)
    }
    
    else if (input$fin_variable == "FEF_1ano_liq_cen01") {
      y_limits_geral <- c(-2.5e9, 800e6)
      y_breaks_geral <- seq(-2.5e9, 800e6, by = 500e6)
      
      y_limits_divida <- y_limits_geral
      y_breaks_divida <- y_breaks_geral
      
    } 
    
    else if (input$fin_variable == "FEF_1ano_liq_cen02") {
      y_limits_geral <- c(-5e9, 1200e6)
      y_breaks_geral <- seq(-5e9, 1200e6, by = 500e6)
      
      y_limits_divida <- y_limits_geral
      y_breaks_divida <- y_breaks_geral
      
    } 
    
    else if (input$fin_variable == "FEF_5ano_liq_cen01") {
      y_limits_geral <- c(-11.37e9, 3800e6)
      y_breaks_geral <- seq(-11.37e9, 3800e6, by = 1000e6)
      
      y_limits_divida <- y_limits_geral
      y_breaks_divida <- y_breaks_geral
      
    } 
    
    else if (input$fin_variable == "FEF_5ano_liq_cen02") {
      y_limits_geral <- c(-22.8e9, 7500e6)
      y_breaks_geral <- seq(-22.8e9, 7500e6, by = 1000e6)
      
      y_limits_divida <- y_limits_geral
      y_breaks_divida <- y_breaks_geral
      
    } 
    
    
    else {
      y_min <- min(df$valor, na.rm = TRUE)
      y_max <- max(df$valor, na.rm = TRUE)
      y_limits_geral <- c(y_min, y_max)
      y_breaks_geral <- waiver()
      y_limits_divida <- c(y_min, y_max)
      y_breaks_divida <- waiver()
    }
    
    # ----- PLOTS -----
    # Plot for general states (in millions)
    p_geral <- ggplot(df_geral, aes(x = factor(Estado, levels = df_geral$Estado), y = valor, fill = UF)) +
      geom_col() +
      geom_text(
        aes(label = paste0(format(round(valor / 1e6), big.mark = ".", decimal.mark = ",", scientific = FALSE), " M")),
        angle = 90, vjust = 0.2, hjust=-0.1, size = 5, color = "blue",fontface = "bold"
      ) +
      scale_fill_manual(values = uf_colors) +
      scale_y_continuous(
        limits = y_limits_geral,
        breaks = y_breaks_geral,
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      scale_fill_manual(values = uf_colors_bySG)+
      labs(title = paste("Demais Estados –", plot_label), x = "Estado", y = "Valor (R$)") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", color = "#1f5673"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    # Plot for indebted states (in billions)
    p_divida <- ggplot(df_endividado, aes(x = factor(Estado, levels = df_endividado$Estado), y = valor, fill = UF)) +
      geom_col() +
      geom_text(
        aes(label = paste0(format(round(valor / 1e6), big.mark = ".", decimal.mark = ",", scientific = FALSE), " M")),
        angle = 90, vjust = 0.2, hjust=-0.1, size = 7, color = "blue",fontface = "bold"
      ) +
      scale_fill_manual(values = uf_colors) +
      scale_y_continuous(
        limits = y_limits_divida,
        breaks = y_breaks_divida,
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      scale_fill_manual(values = uf_colors_bySG)+
      labs(title = "Estados com Alta Dívida", x = "Estado", y = "Valor (R$)") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", color = "#1f5673"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "antiquewhite", color = NA)
      )
    
    
    p_geral + p_divida + plot_layout(ncol = 2, widths = c(2, 1))
  })
  
  ##############################################################################################################################
  ### OUTPUT  DATA TABLE TAB 1  FINANCIALS TAB 1  FINANCIALS  TAB 1  FINANCIALS  TAB 1  FINANCIALS TAB 1  FINANCIALS  TAB 1  FINANCIALS  TAB 1  FINANCIALS 
  ##############################################################################################################################
  
  
  h3("Tabela 1: Variáveis Financeiras", style = "color: #1f5673; font-weight: bold; margin-top: 30px;")
  output$tab1_fin_table <- DT::renderDataTable({
    df <- financeiro_dt_all
    # Drop unused column
    df <- df[, !(names(df) %in% c("fef_share_pct"))]
    
    # Identifica colunas numéricas (exceto UF/Estado)
    data_cols <- setdiff(names(df), c("UF", "Estado"))
    
    # Formata para exibição: números com separador de milhar
    df[data_cols] <- lapply(df[data_cols], function(x) {
      if (is.numeric(x)) format(round(x, 0), big.mark = ".", decimal.mark = ",") else x
    })
    
    DT::datatable(
      df,
      extensions = 'Buttons',
      options = list(
        pageLength = 30,
        scrollX = TRUE,
        scrollY = "600px",
        autoWidth = FALSE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "copy", text = "Copiar"),
          list(extend = "csv", filename = "Tabela_Financeira_PROPAG", text = "CSV"),
          list(extend = "excel", filename = "Tabela_Financeira_PROPAG", text = "Excel"),
          list(
            extend = "pdf",
            filename = "Tabela_Financeira_PROPAG",
            text = "PDF",
            orientation = "landscape",
            pageSize = "A4",
            messageTop = "Tabela 1: Variáveis Financeiras"
          )
        ),
        columnDefs = list(
          list(className = 'dt-nowrap', targets = "_all")
        )
      ),
      rownames = FALSE,
      class = "stripe nowrap display"
    )
  })

  
  
  
  ######### TAB1b TAB1b TAB1b TAB1b  TAB1b TAB1b TAB1b TAB1b  TAB1b TAB1b TAB1b TAB1b  TAB1b TAB1b TAB1b TAB1b  TAB1b TAB1b TAB1b TAB1b  TAB1b TAB1b TAB1b TAB1b  
  ##############################################################################################################################
  ###  TAB 1b  FINANCIALS TAB 1b  FINANCIALS  TAB 1b  FINANCIALS  TAB 1b  FINANCIALS TAB 1b  FINANCIALS  TAB 1b  FINANCIALS  TAB 1b  FINANCIALS 
  ##############################################################################################################################
  # 3) helper to pull valid codes for a single dimension, given the OTHER three

  ## ---------- TAB 1b logic ----------
  
  
  
  # 2) Label → value maps (must match UI)
  # mappings used to rebuild widgets
  ## 1. Choices exactly like the UI (add I5 if you now have 1.5%)
  # exact maps as in the UI
  all_choices <- list(
    A = c("Sem abatimento"  = "A1",
          "10% abatimento" = "A2",
          "20% abatimento" = "A3"),
    G = c("1%"   = "G1",
          "1.5%" = "G2",
          "2%"   = "G3"),
    I = c("0%"   = "I1",
          "0.5%" = "I2",
          "1%"   = "I3",
          "1.5%" = "I4",
          "2%"   = "I5"),
    J = c("0%" = "J1",
          "1%" = "J2",
          "2%" = "J3",
          "4% (Não Adere)" = "J4")
  )
  
  pretty_opts <- list(
    A = list(fill=TRUE,bigger=TRUE,status="info",    icon=icon("check")),
    G = list(fill=TRUE,bigger=TRUE,status="primary", icon=icon("check")),
    I = list(fill=TRUE,bigger=TRUE,status="success", icon=icon("check")),
    J = list(fill=TRUE,bigger=TRUE,status="danger",  icon=icon("check"))
  )
  
  # if you still don't have the 'valid' column, this just returns all matches
  valid_codes <- function(dim, sel){
    df <- dfcen_val
    for (nm in names(sel)) {
      if (length(sel[[nm]]) > 0) {
        df <- df[df[[nm]] %in% sel[[nm]], , drop = FALSE]
      }
    }
    sort(unique(df[[dim]]))
  }
  
  ## ------------ dynamic update ---------------------
  observeEvent(
    list(input$choice_A, input$choice_G, input$choice_I, input$choice_J),
    {
      sel <- list(A = input$choice_A,
                  G = input$choice_G,
                  I = input$choice_I,
                  J = input$choice_J)
      
      # ----- 1) J4 chosen first → lock A/G/I and keep only J4 -----
      if (identical(sel$J, "J4")) {
        sel$A <- sel$G <- sel$I <- character(0)
        session$sendCustomMessage("toggleAGI", TRUE)
        
        updatePrettyCheckboxGroup(session, "choice_J",
                                  choices       = all_choices$J["4% (Não Adere)"],
                                  selected      = "J4", inline = TRUE,
                                  prettyOptions = pretty_opts$J
        )
        # clear the others
        for(id in c("choice_A","choice_G","choice_I")){
          updatePrettyCheckboxGroup(session, id,
                                    choices       = all_choices[[substr(id,8,8)]],
                                    selected      = character(0), inline = TRUE,
                                    prettyOptions = pretty_opts[[substr(id,8,8)]]
          )
        }
        return(invisible(NULL))
      } else {
        session$sendCustomMessage("toggleAGI", FALSE)
      }
      
      # ----- 2) If ANY of A/G/I is selected, DROP J4 immediately -----
      somePicked <- any(lengths(sel[c("A","G","I")]) > 0)
      if (somePicked) {
        j_keep <- all_choices$J[names(all_choices$J) != "4% (Não Adere)"]
      } else {
        j_keep <- all_choices$J
      }
      updatePrettyCheckboxGroup(session, "choice_J",
                                choices       = j_keep,
                                selected      = intersect(sel$J, j_keep),
                                inline        = TRUE,
                                prettyOptions = pretty_opts$J
      )
      
      # ----- 3) Normal filtering for A/G/I (J already updated) -----
      for (dim in c("A","G","I")) {
        others <- sel[names(sel) != dim]
        ok   <- if (all(lengths(others) > 0)) valid_codes(dim, others) else all_choices[[dim]]
        keep <- all_choices[[dim]][ all_choices[[dim]] %in% ok ]
        
        updatePrettyCheckboxGroup(session,
                                  inputId       = paste0("choice_", dim),
                                  choices       = keep,
                                  selected      = intersect(sel[[dim]], keep),
                                  inline        = TRUE,
                                  prettyOptions = pretty_opts[[dim]]
        )
      }
    },
    ignoreInit = TRUE
  )
  
  # VALID TABLE FOR MODAL
  ## ---- lookups used to print human labels ----
  labA <- c(A1 = "Sem abatimento",
            A2 = "10% abatimento",
            A3 = "20% abatimento",
            ND1 = "NA")
  
  labG <- c(G1 = "1%", G2 = "1,5%", G3 = "2%", ND1 = "NA")
  labI <- c(I1 = "0%", I2 = "0,5%", I3 = "1%", I4 = "1,5%", I5 = "2%", ND1 = "NA")
  labJ <- c(J1 = "0%", J2 = "1%", J3 = "2%", J4 = "4% (Não Adere)")
  
  # OPTIONAL: give each valid row a friendly name (first column)
  op_names <- c("II-A","II-B","II-C","III-A","III-B","III-C","IV-A","IV-B","ND")
  
  ## valid_set MUST contain columns A,G,I,J in codes.
  valid_set <- subset(dfcen_val, valid, select = c(A,G,I,J))
  ################TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
  valid_tbl <- valid_set |>
    dplyr::mutate(
      Opção          = op_names,
      Amortização    = labA[A],
      `Contribuição p/ FEF` = labG[G],
      `Invest. Direto`       = labI[I],
      Juros          = labJ[J]
    ) |>
    dplyr::select(Opção, Amortização, `Contribuição p/ FEF`, `Invest. Direto`, Juros)
  
  ## ---- tiny CSS just for the table (uses your dark panel bg) ----
  valid_css <- "
<style>
#tbl-valid thead th{
  background:#1f5673; color:#fff; text-align:center; padding:6px 10px; border:1px solid #4e6f84;
}
#tbl-valid tbody td{
  color:#f5f5f5; padding:6px 10px; border:1px solid #4e6f84;
}
#tbl-valid tbody tr:nth-child(even){ background:rgba(255,255,255,.05); }
#tbl-valid tbody tr:nth-child(odd){  background:rgba(255,255,255,.02); }
</style>
"

## ---- build the html table without extra pkgs ----
make_table_html <- function(df){
  hdr <- paste0("<tr>", paste0(sprintf("<th>%s</th>", names(df)), collapse=""), "</tr>")
  rows <- apply(df, 1, function(r){
    paste0("<tr>", paste0(sprintf("<td>%s</td>", r), collapse=""), "</tr>")
  })
  paste0(
    '<table id="tbl-valid" style="width:100%; border-collapse:collapse;">',
    "<thead>", hdr, "</thead>",
    "<tbody>", paste0(rows, collapse=""), "</tbody></table>"
  )
}

valid_html <- paste0(valid_css, make_table_html(valid_tbl))

  
  
  
  
  
  ## ---- helpers -----------------------------------------------------------
  # helper for null/empty
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  ## -------- reactive pickers ------------------------------------------------
  RKT_picked <- reactive({
    list(A = input$choice_A,
         G = input$choice_G,
         I = input$choice_I,
         J = input$choice_J)
  })
  
  # find matching valid row or NULL
 RKT_sel_row <- reactive({
    s <- RKT_picked()
    if (any(lengths(s) == 0)) return(NULL)
    hit <- dfcen_val[dfcen_val$valid &
                       dfcen_val$A == s$A &
                       dfcen_val$G == s$G &
                       dfcen_val$I == s$I &
                       dfcen_val$J == s$J, ]
    if (nrow(hit)) hit[1, ] else NULL
  })
  
  ## -------- state needed to rollback & block UI -----------------------------
  rv <- reactiveValues(
    last_ok    = list(A=character(0), G=character(0), I=character(0), J=character(0)),
    last_dim   = NULL,
    lock_dim   = NULL        # which dim we need to rollback
  )
  modal_shown <- reactiveVal(FALSE)
  
  observeEvent(input$choice_A, { rv$last_dim <- "A" }, ignoreInit = TRUE)
  observeEvent(input$choice_G, { rv$last_dim <- "G" }, ignoreInit = TRUE)
  observeEvent(input$choice_I, { rv$last_dim <- "I" }, ignoreInit = TRUE)
  observeEvent(input$choice_J, { rv$last_dim <- "J" }, ignoreInit = TRUE)
  
  ## -------- main logic ------------------------------------------------------
  observeEvent(RKT_picked(), {
    
    sel <- RKT_picked()
    
    # 1) “Não Adere” rule: picking J4 clears A/G/I immediately
    if (identical(sel$J, "J4") &&
        any(lengths(sel[c("A","G","I")]) > 0)) {
      sel$A <- sel$G <- sel$I <- character(0)
      updatePrettyCheckboxGroup(session, "choice_A", selected = character(0))
      updatePrettyCheckboxGroup(session, "choice_G", selected = character(0))
      updatePrettyCheckboxGroup(session, "choice_I", selected = character(0))
    }
    
    # 2) If still incomplete **but not yet dead**, just close any open modal
    if (any(lengths(sel) == 0) && modal_shown()) {
      removeModal(); modal_shown(FALSE)
      shinyjs::enable(selector = ".pcg")
    }
    
    # 3) Check “partial validity” by filtering against ALL valid rows
    vp <- dfcen_val[dfcen_val$valid, ]        # start with the 9 valid rows
    for (dim in names(sel)) {
      if (length(sel[[dim]]) > 0) {
        vp <- vp[ vp[[dim]] %in% sel[[dim]] , , drop = FALSE ]
      }
    }
    if (nrow(vp) == 0) {
      # there is no valid row that matches the current partial sel → invalid
      rv$lock_dim <- rv$last_dim %||% names(sel)[which.max(vapply(sel, length, 0))]
      shinyjs::disable(selector = ".pcg")
      
      ##  -- inside your observeEvent(RKT_picked(), { … }) once you know it's invalid:
      
      # 1) detect the “culprit” dimension (where no valid codes remain)
      bad_dims <- names(sel)[
        vapply(names(sel), function(dim) {
          # consider everything *but* this dim
          others <- sel[names(sel) != dim]
          # if zero codes remain valid for 'dim', that's bad
          length(valid_codes(dim, others)) == 0L
        }, logical(1))
      ]
      bad_dim <- bad_dims[1]  # first offending dimension
      
      # 2) pull our human‐label maps and dimension name
      maps     <- list(A=labA, G=labG, I=labI, J=labJ)
      dim_names<- c(A="Amortização", G="Contribuição p/ FEF",
                    I="Invest. Direto", J="Juros")
      bad_map   <- maps[[bad_dim]]
      bad_label <- dim_names[bad_dim]
      
      # 3) what they *just* picked (in red)
      bad_code  <- sel[[bad_dim]]
      bad_text  <- bad_map[bad_code]
      
      # 4) what *would* be valid here (in green)
      ok_codes  <- valid_codes(bad_dim, sel[names(sel)!=bad_dim])
      ok_text   <- bad_map[ok_codes]
      
      # 5) craft the little hint HTML
      hint_html <- sprintf(
        "<p>Você escolheu <strong style='color:#e74c3c'>%s</strong> para <em>%s</em>,<br/>
           mas apenas <strong style='color:#27ae60'>%s</strong> %s válidas.</p>",
        bad_text,
        bad_label,
        paste(ok_text, collapse = ", "),
        if (length(ok_text)>1) "são" else "é"
      )
      
      # 6) show the modal (with your existing valid_html table below)
      showModal(modalDialog(
        title = HTML("💥 Combinação inválida"),
        tagList(
          HTML("Essa escolha não é permitida pelo <b>PROPAG</b>.<br>",
               "Ajuste os percentuais para atender uma combinação válida."),
          HTML(hint_html),
          HTML(valid_html)
        ),
        easyClose = FALSE,
        footer = actionButton("invalid_ok", "OK", class="btn-primary"),
        size = "l"
      ))
      
      
      session$onFlushed(function(){
        runjs("
        var dlg = $('#shiny-modal .modal-dialog');
        if(!dlg.hasClass('ui-draggable')){
          dlg.draggable({ handle: '.modal-header' });
          dlg.css('cursor','move');
        }
      ")
      }, once = TRUE)
      modal_shown(TRUE)
      return()
    }
    
    # 4) If full combo chosen and matches one valid row, store it & close modal
    if (all(lengths(sel) == 1)) {
      hit <- vp[ vp$A == sel$A & vp$G == sel$G &
                   vp$I == sel$I & vp$J == sel$J , , drop = FALSE]
      if (nrow(hit) == 1) {
        rv$last_ok <- sel
        if (modal_shown()) {
          removeModal(); modal_shown(FALSE)
          shinyjs::enable(selector = ".pcg")
        }
        return()
      }
    }
    
    # otherwise: still incomplete but not dead → do nothing (wait next pick)
  }, ignoreInit = TRUE)
  
  
  
  
  
  
  
  
  ## -------- user clicks OK on modal ----------------------------------------
  observeEvent(input$invalid_ok, {
    removeModal()
    modal_shown(FALSE)
    
    # rollback culprit group to last valid
    dim <- rv$lock_dim %||% "A"
    id  <- paste0("choice_", dim)
    updatePrettyCheckboxGroup(
      session, inputId = id,
      selected = rv$last_ok[[dim]] %||% character(0)
    )
    
    # re-enable UI
    shinyjs::enable(selector = ".pcg")
  })
  
  
  ## (optional) summary

  
  # (Optional) simple summary text
  ## ---- CSS once in UI (or put in your custom.css) ---------------------------
  tags$head(tags$style(HTML("
.scenario-box{
  border:1px solid #cc0000;          /* red frame (change if you want)       */
  padding:14px 18px;
  width:260px;
  margin-left:auto;                  /* keep it at the right column          */
  color:#fff;                        /* works on your dark panel             */
  background:rgba(255,255,255,.05);  /* light transparent fill               }
.scenario-box h4{
  margin:0 0 8px 0;
  font-weight:700;
  text-align:center;
}
.scenario-box .lbl {font-weight:600;}
")))
  
  
  ## helper to pull pretty name for a row in dfcen_val (assumes column 'valid')
  row_to_name <- function(r){
    # r is a 1-row data.frame
    key <- paste(r$A,r$G,r$I,r$J, sep = "_")
    key_vec <- paste(valid_set$A,valid_set$G,valid_set$I,valid_set$J, sep="_")
    op_names[ match(key, key_vec) ]
  }
  
  ## ---- choice summary card --------------------------------------------------
  output$choice_summary <- renderUI({
    r <- RKT_sel_row()          # from your earlier code; NULL if incomplete/invalid
    if (is.null(r)) return(NULL)
    
    opcao <- row_to_name(r)
    htmltools::div(class="scenario-box",
                   htmltools::h4(sprintf("Opção : %s", opcao)),
                   htmltools::div(span(class="lbl","Amortização: "), labA[r$A]),
                   htmltools::div(span(class="lbl","Contribuição ao FEF: "), labG[r$G]),
                   htmltools::div(span(class="lbl","Investimento Direto: "), labI[r$I]),
                   htmltools::div(span(class="lbl","Juros: "), labJ[r$J])
    )
  })
  
 
  
  ###################### REACTIVE TAB1b  REACTIVE Tab1b REACTIVE REACTIVE Tab1b REACTIVE REACTIVE Tab1b REACTIVE ############################
  ###################### REACTIVE TAB1b  REACTIVE Tab1b REACTIVE REACTIVE Tab1b REACTIVE REACTIVE Tab1b REACTIVE ############################
  
  
  # === DEBUG: print to R console ===
  # observe({
  #   # once ANY of the four checkboxes changes, this will run
  #   sel <- RKT_picked()
  #   cat(">>> picked()\n")
  #   print(sel)
  #   cat(">>> sel_row()\n")
  #   sr <- RKT_sel_row()
  #   if (is.null(sr)) {
  #     cat("sel_row() is NULL\n\n")
  #   } else {
  #     # print the one‐row data.frame
  #     print(sr)
  #     cat("\n")
  #   }
  # })
  observe({
    session$sendCustomMessage("toggleAGI", "J4" %in% input$choice_J)
  })
  
  
  
  
  # 1) what the user has chosen, NULL until valid:
  RKT_scenario_name <- reactive({
    # Case: J4 (Não Adere) overrides all other choices
    if ("J4" %in% input$choice_J) {
      return("ND")
    }
    
    # Fallback to the existing logic
    r <- RKT_sel_row()
    req(r)
    row_to_name(r)  # e.g. "II-A", "III-B", … or "ND"
  })
  
  
  # 2) pull the right df from the list
  RKT_scenario_data <- reactive({
    nm <- RKT_scenario_name()
    df_list[[nm]]
  })
  
  # 3) filter to the chosen UF
  RKT_uf_data <- reactive({
    df <- RKT_scenario_data()
    req(input$uf_select)
    df[df$NM_UF == input$uf_select, ]
  })
  
  
  ### 4) Pivot into long form for the ggplot ###
  RKT_plot_data <- reactive({
    df <- RKT_uf_data()
    req(input$var_select)
    years <- 2025:2054
    sel_cols <- paste0(input$var_select, years)
    df %>%
      select(all_of(sel_cols)) %>%
      setNames(years) %>%
      pivot_longer(
        cols      = everything(),
        names_to  = "Ano",
        values_to = "Valor"
      ) %>%
      mutate(Ano = as.integer(Ano))
  })
  

  
  
  RKT_plot_data_compare <- reactive({
    req(input$var_select, input$uf_select)
    
    var     <- input$var_select
    year_bounds <- input$year_range
    years       <- seq(year_bounds[1], year_bounds[2])
    
    selcols <- paste0(var, years)
    
    main_op <- RKT_scenario_name()
    cmp_op  <- input$compare_with %||% ""
    
    safe_named_scenario_label <- function(key) {
      if (!is.null(key) && nzchar(key) && key %in% names(named_scenarios)) {
        named_scenarios[[key]]
      } else {
        paste("Cenário", key %||% "ND")
      }
    }
    label_main <- safe_named_scenario_label(main_op)
    label_cmp  <- safe_named_scenario_label(cmp_op)
    
    df_sel <- RKT_scenario_data() %>%
      filter(NM_UF == input$uf_select) %>%
      select(all_of(selcols)) %>%
      setNames(years) %>%
      pivot_longer(cols = everything(), names_to = "Ano", values_to = "Valor") %>%
      mutate(
        Ano        = as.integer(Ano),
        UF         = input$uf_select,
        fill_key   = UF,
        fill_label = label_main    # 👈 Add this
      )
    
    if (input$compare_with == "") return(df_sel)
    
    df_cmp <- df_list[[cmp_op]] %>%
      filter(NM_UF == input$uf_select) %>%
      select(all_of(selcols)) %>%
      setNames(years) %>%
      pivot_longer(cols = everything(), names_to = "Ano", values_to = "Valor") %>%
      mutate(
        Ano        = as.integer(Ano),
        UF         = input$uf_select,
        fill_key   = paste0(UF, "_compare"),
        fill_label = label_cmp     # 👈 Add this
      )
    
    bind_rows(df_sel, df_cmp)
  })
  
  
  RKT_plot_title <- reactive({
    req(input$uf_select, RKT_scenario_name())
    
    uf      <- input$uf_select
    main_op <- RKT_scenario_name()
    cmp_op  <- input$compare_with
    
    main_row <- valid_tbl[valid_tbl$Opção == main_op, ]
    main_desc <- if (nrow(main_row) == 1) {
      paste0("Cenário ", main_op, " [", 
             main_row$Amortização, ", ",
             main_row$`Contribuição p/ FEF`, ", ",
             main_row$`Invest. Direto`, ", ",
             main_row$Juros, "]")
    } else {
      paste0("Cenário ", main_op)
    }
    
    if (!nzchar(cmp_op)) {
      paste0("Gráfico: UF ", uf, " — ", main_desc)
    } else {
      cmp_row <- valid_tbl[valid_tbl$Opção == cmp_op, ]
      cmp_desc <- if (nrow(cmp_row) == 1) {
        paste0("Cenário ", cmp_op, " [", 
               cmp_row$Amortização, ", ",
               cmp_row$`Contribuição p/ FEF`, ", ",
               cmp_row$`Invest. Direto`, ", ",
               cmp_row$Juros, "]")
      } else {
        paste0("Cenário ", cmp_op)
      }
      
      paste0("Gráfico: UF ", uf, " — ", main_desc, " comparado com ", cmp_desc)
    }
  })
  
  
  
  
  
  ###################### OUTPUT TAB1b  OUTPUT Tab1b OUTPUT OUTPUT Tab1b OUTPUT OUTPUT Tab1b OUTPUT ############################
  ###################### OUTPUT TAB1b  OUTPUT Tab1b OUTPUT OUTPUT Tab1b OUTPUT OUTPUT Tab1b OUTPUT ############################
  
  
  # render the bar chart

  output$PloTab1b <- renderPlot({
    

    
    pd <- RKT_plot_data_compare()
    req(nrow(pd) > 0)
    
    # Color palette: full (original + comparison-shaded)
    all_colors <- c(
      uf_colors,
      setNames(uf_colors_compare, paste0(names(uf_colors), "_compare"))
    )
    
    # Check if comparison is active
    is_comparing <- any(grepl("_compare$", pd$fill_key))
    
    # Base ggplot
    gp2b <- ggplot(pd, aes(
      x    = factor(Ano),
      y    = Valor,
      fill = fill_key
    )) +
      geom_col(position = position_dodge(width = 0.9), width = 0.8) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x     = "Ano",
        y     = switch(input$var_select,
                       Saldo   = "Saldo da Dívida",
                       ApoFEF  = "Aporte ao FEF",
                       InvDir  = "Investimento Direto",
                       JurPag  = "Juros Pagos"),
        title = RKT_plot_title()
      ) +
      theme_minimal() +
      theme(
        legend.position = if (is_comparing) c(0.95, 0.95) else "none",
        axis.text.x     = element_text(size = 16, angle = 90, vjust = 0.5,color="blue"),
        axis.text.y     = element_text(size = 16,color="blue"),
        axis.title.x    = element_text(size = 16, color="blue", face = "bold"),
        axis.title.y    = element_text(size = 16,color="blue", face = "bold"),
        plot.title      = element_text(size = 22, face = "bold", hjust = 0.5, color = "#1f5673")
      )
    
    # Conditional labels:
    
    # Compute appropriate label text colors based on brightness of fill color
    text_colors <- sapply(pd$fill_key, function(key) {
      col_hex <- all_colors[[key]]
      rgb_vals <- hex2RGB(col_hex)@coords
      luminance <- sum(rgb_vals * c(0.299, 0.587, 0.114))  # perceptual brightness
      if (luminance > 0.6) "black" else "white"
    })
    
    # Calculate number of years selected
    n_years <- length(unique(pd$Ano))
    
    # Adjust text size based on range
    label_size <- case_when(
      n_years <= 10 ~ 8,       # Very large if only a few years
      n_years <= 20 ~ 6,     # Medium-large for mid-range
      TRUE          ~ 4.5      # Default for full range
    )

    
    
    
    gp2b <- gp2b +
      geom_text(
        aes(label = paste0(scales::comma(Valor / 1e6), " mi")),
        position = position_dodge(width = 0.9),
        color    = text_colors,
        size     = label_size,
        fontface = "bold",
        angle    = if (is_comparing) 90 else 0,
        vjust    = if (is_comparing) 1.2 else 1.2,  # same vertical reference point (but for rotated text, this pulls it downward)
        hjust    = if (is_comparing) 1.1 else 0.5   # shift right slightly so it doesn't clip left edge
      )
    
 
    legend_labels <- setNames(pd$fill_label, pd$fill_key)
    
    gp2b <- gp2b +
      scale_fill_manual(
        values = all_colors,
        labels = legend_labels,
        name = "Leyenda"
      )+
      theme(
        legend.title = element_text(size = 16, face = "bold"),  # 👈 title size
        legend.text  = element_text(size = 14))   
    
    
    gp2b
  })
  
  
  
  

  
  
  
  #################
  # TAB 2 TAB2 TAB2 
  #################
  
  # Track current mode
  current_mode <- reactiveVal("uniform")
  
  # Apply initial toggle states on startup
  observe({
    mode <- current_mode()
    
    for (op in opcoes) {
      toggleState(id = paste0("chk_all_", op), condition = (mode == "uniform"))
      for (uf in sg_ufs) {
        toggleState(id = paste0("chk_", op, "_", uf), condition = (mode == "per_uf"))
      }
    }
  })
  
  
  # Watch for mode switch
  observeEvent(input$selection_mode, {
    isolate({
      # Only show modal if we've already initialized current_mode once
      if (!is.null(current_mode()) && input$selection_mode != current_mode()) {
        showModal(modalDialog(
          title = "Mudar Modo de Seleção?",
          "Essa ação limpará as seleções atuais. Deseja continuar?",
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancelar"),
            actionButton("confirm_mode_change", "Sim, mudar", class = "btn-danger")
          )
        ))
      } else {
        # If first load or same selection — just set mode silently
        current_mode(input$selection_mode)
      }
    })
  })
  
  
  # Confirmed switch
  observeEvent(input$confirm_mode_change, {
    removeModal()
    new_mode <- isolate(input$selection_mode)
    current_mode(new_mode)
    
    # 1. Clear all checkboxes
    for (op in opcoes) {
      updateCheckboxInput(session, paste0("chk_all_", op), value = FALSE)
      for (uf in sg_ufs) {
        updateCheckboxInput(session, paste0("chk_", op, "_", uf), value = FALSE)
      }
    }
    
    # 2. Disable/enable controls after DOM is flushed
    session$onFlushed(function() {
      if (new_mode == "uniform") {
        # Enable 'Todos' checkboxes, disable individual ones
        for (op in opcoes) {
          shinyjs::enable(paste0("chk_all_", op))
          for (uf in sg_ufs) {
            shinyjs::disable(paste0("chk_", op, "_", uf))
          }
        }
      } else {
        # Disable 'Todos' checkboxes, enable individual ones
        for (op in opcoes) {
          shinyjs::disable(paste0("chk_all_", op))
          for (uf in sg_ufs) {
            shinyjs::enable(paste0("chk_", op, "_", uf))
          }
        }
      }
    }, once = TRUE)
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
            for (uf in sg_ufs) {
              updateCheckboxInput(session, paste0("chk_", other_op, "_", uf), value = FALSE)
            }
          }
          for (uf in sg_ufs) {
            updateCheckboxInput(session, paste0("chk_", op_local, "_", uf), value = selected)
          }
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Per-UF: column logic
  observe({
    for (uf in sg_ufs) {
      for (op in opcoes) {
        local({
          uf_local <- uf
          op_local <- op
          id <- paste0("chk_", op_local, "_", uf_local)
          observeEvent(input[[id]], {
            if (current_mode() == "per_uf" && isTRUE(input[[id]])) {
              for (other_op in setdiff(opcoes, op_local)) {
                updateCheckboxInput(session, paste0("chk_", other_op, "_", uf_local), value = FALSE)
              }
            }
          }, ignoreInit = TRUE)
        })
      }
    }
  })
  
  
  
  
  # Track current mode
  current_mode <- reactiveVal("uniform")
  
  # Apply initial toggle states on startup
  observe({
    mode <- current_mode()
    
    for (op in opcoes) {
      toggleState(id = paste0("chk_all_", op), condition = (mode == "uniform"))
      for (uf in sg_ufs) {
        toggleState(id = paste0("chk_", op, "_", uf), condition = (mode == "per_uf"))
      }
    }
  })
  
  
  # Watch for mode switch
  observeEvent(input$selection_mode, {
    isolate({
      # Only show modal if we've already initialized current_mode once
      if (!is.null(current_mode()) && input$selection_mode != current_mode()) {
        showModal(modalDialog(
          title = "Mudar Modo de Seleção?",
          "Essa ação limpará as seleções atuais. Deseja continuar?",
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancelar"),
            actionButton("confirm_mode_change", "Sim, mudar", class = "btn-danger")
          )
        ))
      } else {
        # If first load or same selection — just set mode silently
        current_mode(input$selection_mode)
      }
    })
  })
  
  
  # Confirmed switch
  observeEvent(input$confirm_mode_change, {
    removeModal()
    new_mode <- isolate(input$selection_mode)
    current_mode(new_mode)
    
    # 1. Clear all checkboxes
    for (op in opcoes) {
      updateCheckboxInput(session, paste0("chk_all_", op), value = FALSE)
      for (uf in sg_ufs) {
        updateCheckboxInput(session, paste0("chk_", op, "_", uf), value = FALSE)
      }
    }
    
    # 2. Disable/enable controls after DOM is flushed
    session$onFlushed(function() {
      if (new_mode == "uniform") {
        # Enable 'Todos' checkboxes, disable individual ones
        for (op in opcoes) {
          shinyjs::enable(paste0("chk_all_", op))
          for (uf in sg_ufs) {
            shinyjs::disable(paste0("chk_", op, "_", uf))
          }
        }
      } else {
        # Disable 'Todos' checkboxes, enable individual ones
        for (op in opcoes) {
          shinyjs::disable(paste0("chk_all_", op))
          for (uf in sg_ufs) {
            shinyjs::enable(paste0("chk_", op, "_", uf))
          }
        }
      }
    }, once = TRUE)
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
            for (uf in sg_ufs) {
              updateCheckboxInput(session, paste0("chk_", other_op, "_", uf), value = FALSE)
            }
          }
          for (uf in sg_ufs) {
            updateCheckboxInput(session, paste0("chk_", op_local, "_", uf), value = selected)
          }
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Per-UF: column logic
  observe({
    for (uf in sg_ufs) {
      for (op in opcoes) {
        local({
          uf_local <- uf
          op_local <- op
          id <- paste0("chk_", op_local, "_", uf_local)
          observeEvent(input[[id]], {
            if (current_mode() == "per_uf" && isTRUE(input[[id]])) {
              for (other_op in setdiff(opcoes, op_local)) {
                updateCheckboxInput(session, paste0("chk_", other_op, "_", uf_local), value = FALSE)
              }
            }
          }, ignoreInit = TRUE)
        })
      }
    }
  })
  
############################################  #####
# REACTIVE TABE TAB2 TAB2
  

  # Create a reactive that returns a named vector: names = sg_uf, values = chosen option
  RKT_fef_choices <- reactive({ 
    mode <- current_mode()
    selected <- character(length(sg_ufs))
    names(selected) <- sg_ufs
    
    if (mode == "uniform") {
      # Check which chk_all_* is selected
      for (op in opcoes) {
        if (isTRUE(input[[paste0("chk_all_", op)]])) {
          selected[] <- op
          break
        }
      }
    } else {
      for (uf in sg_ufs) {
        for (op in opcoes) {
          if (isTRUE(input[[paste0("chk_", op, "_", uf)]])) {
            selected[uf] <- op
            break
          }
        }
      }
    }
    selected
  })
  
  
  uf_map <- df_censo_UF |>
    dplyr::select(sg_uf = SG_UF, NM_UF) |>
    dplyr::distinct()
  
  
  
  RKT_fef_all_options <- reactive({
    purrr::map_dfr(df_choices$opcao, function(op_label) {
      df_base <- df_list[[op_label]]
      if (is.null(df_base)) return(NULL)
      
      # Select required columns — NM_UF must be present and consistent with uf_map
      df_base_min <- df_base |>
        dplyr::select(NM_UF, Distr_FEF, dplyr::matches("^ApoFEF"))
      
      # Join on NM_UF with known-clean uf_map
      df_joined <- dplyr::left_join(df_base_min, uf_map, by = "NM_UF")
      
      if (any(is.na(df_joined$sg_uf))) {
        unmatched <- df_joined$NM_UF[is.na(df_joined$sg_uf)]
        warning("Could not match all NM_UF values to sg_uf for option ", op_label,
                ". Unmatched: ", paste(unique(unmatched), collapse = ", "))
      }
      
      df_joined$opcao <- op_label
      df_joined
    })
  })
  
  
  

  RKT_fef_table <- reactive({
    choices <- RKT_fef_choices()
    all_df <- RKT_fef_all_options()
    
    if (is.null(all_df)) return(NULL)
    
    mode <- current_mode()
    
    if (mode == "uniform") {
      op_code <- unique(choices)
      if (length(op_code) != 1 || !nzchar(op_code)) return(NULL)
      
      op_label <- df_choices$opcao[match(toupper(op_code), toupper(opcoes))]
      return(dplyr::filter(all_df, opcao == op_label))
    } else {
      choice_df <- tibble::tibble(
        sg_uf = names(choices),
        op_code = choices
      ) |>
        dplyr::filter(nzchar(op_code)) |>
        dplyr::mutate(opcao = df_choices$opcao[match(toupper(op_code), toupper(opcoes))]) |>
        dplyr::select(sg_uf, opcao, op_code)      |>
        dplyr::filter(nzchar(op_code)) |>
        dplyr::mutate(opcao = df_choices$opcao[match(toupper(op_code), toupper(opcoes))])
      
      # Join only on sg_uf and opcao — NM_UF will come cleanly from all_df
      dplyr::inner_join(all_df, choice_df, by = c("sg_uf","opcao"))
    }
  })
  
  
  
  
  RKT_fef_total_by_year <- reactive({
    df <- RKT_fef_table()
    req(df)

    # Select ApoFEF columns
    cols <- grep("^ApoFEF", names(df), value = TRUE)

    total <- colSums(df[, cols, drop = FALSE], na.rm = TRUE)

    # Convert to tibble with year extracted from column names
    tibble::tibble(
      year = as.integer(sub("ApoFEF", "", names(total))),
      total_fef = as.numeric(total)
    )
  })

  
  
  RKT_fef_liq_flow_by_uf <- reactive({
    df <- RKT_fef_table()
    total <- RKT_fef_total_by_year()
    
    req(df, total)
    
    # Step 1: Identify all ApoFEF columns
    fef_cols <- grep("^ApoFEF", names(df), value = TRUE)
    years <- as.integer(sub("ApoFEF", "", fef_cols))
    
    # Step 2: Get total FEF per year as a named vector
    total_vec <- setNames(total$total_fef, paste0("ApoFEF", total$year))
    
    # Step 3: Prepare starting df with core identity and ApoFEF columns
    liq_df <- df[, c("sg_uf", "NM_UF", "opcao", "Distr_FEF", fef_cols)]
    
    # Step 4: Add LiqFEF columns
    for (col in fef_cols) {
      year <- sub("ApoFEF", "", col)
      total_amt <- total_vec[[col]]
      liq_df[[paste0("LiqFEF", year)]] <- (liq_df$Distr_FEF * total_amt) - df[[col]]
    }
    
    liq_df
  })
  
  
  # Tab2: Prepare data for plotting
  RKT_fef_plot_data <- reactive({
    df <- RKT_fef_liq_flow_by_uf()
    req(df)
    
    uf_input <- input$uf_select
    year_bounds <- input$year_range
    df_uf <- dplyr::filter(df, NM_UF == uf_input)
    
    # Gather and filter by year range
    df_long <- df_uf |>
      tidyr::pivot_longer(
        cols = matches("^(ApoFEF|LiqFEF)\\d+"),
        names_to = "var",
        values_to = "value"
      ) |>
      dplyr::mutate(
        type = dplyr::if_else(stringr::str_starts(var, "ApoFEF"), "ApoFEF", "LiqFEF"),
        year = as.integer(stringr::str_remove(var, "^(ApoFEF|LiqFEF)"))
      ) |>
      dplyr::filter(year >= year_bounds[1], year <= year_bounds[2])  # 👈 filtra aqui
    
    df_long
  })
  
  RKT_plot_title_tab2 <- reactive({
    req(input$uf_select, input$year_range, RKT_fef_table())
    
    uf_code <- input$uf_select
    df <- RKT_fef_table()
    
    # Buscar o nome da UF e sua opção
    row <- df |>
      dplyr::filter(NM_UF == uf_code) |>
      dplyr::select(NM_UF, opcao) |>
      dplyr::distinct()
    
    uf_name <- unique(row$NM_UF)
    uf_op   <- unique(row$opcao)
    
    uf_name <- uf_name %||% uf_code
    uf_op   <- uf_op   %||% "ND"
    
    paste0("UF: ", uf_name,
           " — Opção: ", uf_op,
           " — Anos: ", input$year_range[1], "–", input$year_range[2],
           " — Aporte e Fluxo Líquido do FEF")
  })
  
  
  
  
  # # Print RKT_fef_choices to console
  # observe({
  #   cat("=== RKT_fef_choices() ===\n")
  #   print(RKT_fef_choices())
  # })
  # 
  # observe({
  #   df <- RKT_fef_table()
  #   if (!is.null(df)) {
  #     cat("=== RKT_fef_choices() ===\n")
  #     print(RKT_fef_choices())
  # 
  #     cat("=== RKT_fef_table() ===\n")
  #     print(df, n = 100, na.print="NA")
  # 
  #     cat("=== Total aporte_fef across UFs ===\n")
  #     cols <- grep("^ApoFEF", names(df), value = TRUE)
  #     totals <- colSums(df[, cols, drop = FALSE], na.rm = TRUE)
  #     print(round(totals, 2))
  #   }
  # })
  # 
  # observe({
  #   total_df <- RKT_fef_total_by_year()
  #   if (!is.null(total_df)) {
  #     cat("=== Total FEF by Year ===\n")
  #     print(total_df, n = nrow(total_df))
  #   }
  # })
  
  # observe({
  #   liq_df <- RKT_fef_liq_flow_by_uf()
  #   if (!is.null(liq_df)) {
  #     cat("=== Net FEF Flow (LiqFEF) by UF ===\n")
  #     print(liq_df, n = nrow(liq_df), na.print = "NA")
  #   }
  # })
  # 
  
  
  ###################### OUTPUT Tab2  OUTPUT Tab2 OUTPUT OUTPUT Tab2 OUTPUT OUTPUT Tab2 OUTPUT ############################
  ###################### OUTPUT Tab2  OUTPUT Tab2 OUTPUT OUTPUT Tab2 OUTPUT OUTPUT Tab2 OUTPUT ############################
  
  output$plotab2 <- renderPlot({
    df <- RKT_fef_plot_data()
    req(nrow(df) > 0)
    
    # Define colors manually
    fill_colors <- c("ApoFEF" = "#fbb4ae", "LiqFEF" = "#FF4D4D")
    
    # Create label column
    df$label <- paste0(scales::comma(df$value / 1e6), " mi")
    
    df$hjust <- dplyr::case_when(
      df$value >= 0 ~ 1.2,  # slightly above the bar (negative = upward in y)
      df$value <  0 ~  -0.2   # slightly below the bar
    )
    
    # Consistent horontal centering of rotated text
    df$vjust <- 0.5 # center
    
    dodge_width <- 0.75
    pos_dodge <- position_dodge(width = dodge_width)
    
    # Font size can be adjusted for year range
    n_years <- length(unique(df$year))
    text_size <- dplyr::case_when(
      n_years <= 10 ~ 10,
      n_years <= 20 ~ 8,
      TRUE          ~ 6
    )


    # Main ggplot
    ggplot(df, aes(x = factor(year), y = value, fill = type)) +
      geom_col(position = pos_dodge, width = dodge_width) +
      geom_text(
        aes(label = label, hjust = hjust),
        position = pos_dodge,
        color= "black",
        size = text_size,
        angle = 90,
        vjust    = 0.5,
        fontface = "bold"
      ) +
      scale_color_identity()+
      scale_fill_manual(values = fill_colors,
                        labels = c("ApoFEF" = "Aporte FEF", "LiqFEF" = "Fluxo Líquido")) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      labs(
        x = "Ano", y = "Valor (R$)", fill = NULL,
        title = RKT_plot_title_tab2()
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.text.x     = element_text(size = 16, angle = 90, vjust = 0.5, color = "blue"),
        axis.text.y     = element_text(size = 16, color = "blue"),
        axis.title.x    = element_text(size = 16, color = "blue", face = "bold"),
        axis.title.y    = element_text(size = 16, color = "blue", face = "bold"),
        plot.title      = element_text(size = 22, face = "bold", hjust = 0.5, color = "#1f5673")
      )
  })
  
  
  
  
  
  ######### TAB3 TAB3 TAB3 TAB3  TAB3 TAB3 TAB3 TAB3  TAB3 TAB3 TAB3 TAB3  TAB3 TAB3 TAB3 TAB3  TAB3 TAB3 TAB3 TAB3  TAB3 TAB3 TAB3 TAB3  
  ##############################################################################################################################
  ### OUTPUT PLOT TAB 3  META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE 
  ##############################################################################################################################
  
  # --- Line chart output with projection ---
  output$oferta_ept_plot <- renderPlot({
    req(input$oferta_uf, input$oferta_ept_var, input$oferta_other_var)
    
    # Filter and aggregate only "UF_TUDO"
    # Step 1: Observed data
    df_filtered <- meta11a_opcoes |>
      filter(NM_UF == input$oferta_uf) |>
      group_by(ANO) |>
      summarise(
        EPT = sum(.data[[input$oferta_ept_var]], na.rm = TRUE),
        ENSINO_MEDIO = sum(.data[[input$oferta_other_var]], na.rm = TRUE),
        .groups = "drop"
      )
    
    # Compute triplo da base de 2013 como meta PNE
    base_2013_val <- df_filtered |> filter(ANO == 2013) |> pull(EPT)
    triplo_2013 <- if (!is.na(base_2013_val)) 3 * base_2013_val else NA
    
    # Linear model based on 2020–2024 data
    ### BOTTOM
    # Step 1: Fit model to recent trend (2020–2024)
    df_recent <- df_filtered |> filter(ANO %in% 2020:2024)
    linear_model <- lm(EPT ~ ANO, data = df_recent)
    
    # Step 2: Extract slope only
    slope <- coef(linear_model)["ANO"]
    
    # Step 3: Anchor to actual 2024 observed value
    start_val <- df_filtered |> filter(ANO == 2024) |> pull(EPT) |> mean(na.rm = TRUE)
    
    # Step 4: Build future projection using that slope and starting point
    future_years <- 2024:2035
    years_from_start <- future_years - 2024
    
    future_df <- data.frame(
      ANO = future_years,
      VALOR = start_val + slope * years_from_start,
      TIPO = "EPT",
      GRUPO = "PROJECAO"
    )
    ## TOP
    # ---- OUTRA projection from 2024 to 2035 using slope-preserving shift ----
    
    # 1. Fit linear model to OUTRA from 2020 to 2024
    lm_outra <- lm(ENSINO_MEDIO ~ ANO, data = df_filtered |> filter(ANO %in% 2020:2024))
    
    # 2. Extract only the slope (ignore intercept)
    slope_outra <- coef(lm_outra)["ANO"]
    
    # 3. Get the actual observed value in 2024
    start_val_outra <- df_filtered |> 
      filter(ANO == 2024) |> 
      pull(ENSINO_MEDIO) |> 
      mean(na.rm = TRUE)
    
    # 4. Generate projected values for 2024–2035
    future_years <- 2024:2035
    years_from_start <- future_years - 2024
    
    future_outra_df <- data.frame(
      ANO = future_years,
      VALOR = start_val_outra + slope_outra * years_from_start,
      TIPO = "ENSINO_MEDIO",
      GRUPO = "PROJECAO"
    )
    
    
    # Long format for observed values
    df_long <- df_filtered |>
      mutate(GRUPO = "OBSERVADO") |>
      pivot_longer(cols = c("EPT", "ENSINO_MEDIO"), names_to = "TIPO", values_to = "VALOR") |>
      mutate(GRUPO = ifelse(TIPO == "ENSINO_MEDIO", "ENSINO_MEDIO", GRUPO))
    
    df_plot <- bind_rows(
      df_long,          # OBSERVADO and OUTRA (observed)
      future_df,        # EPT PROJECAO
      future_outra_df   # OUTRA PROJECAO
    )
    
    
    # Value of EPT in 2013
    ept_2013 <- df_plot |> 
      filter(ANO == 2013, GRUPO == "OBSERVADO") |> 
      summarise(val = sum(VALOR, na.rm = TRUE)) |> 
      pull(val)
    
    x_label <- 2013
    y_label <- ept_2013 + 5000  # Adjust vertical space as needed
    
    
    # Plot
    ggplot(df_plot, aes(x = ANO, y = VALOR, color = GRUPO)) +
      
      # All observed and projected lines except dotted OUTRA
      geom_line(
        data = df_plot |> filter(!(TIPO == "ENSINO_MEDIO" & GRUPO == "PROJECAO")),
        size = 1.5
      ) +
      
      # Dotted line for OUTRA projection only
      geom_line(
        data = df_plot |> filter(TIPO == "ENSINO_MEDIO" & GRUPO == "PROJECAO"),
        aes(x = ANO, y = VALOR),
        linetype = "longdash",
        linewidth = 1.5,
        color = "#00BFC4"  # consistent with ggplot default for OUTRA
      ) +
      
      # Points for all
      geom_point(size = 2, alpha = 0.8) +
      
      # Meta 11 horizontal line
      geom_hline(yintercept = triplo_2013, linetype = "dashed", color = "darkorange", linewidth = 1.1) +
      annotate(
        "text", x = 2008, y = triplo_2013,
        label = paste0("Meta 11 (Triplo 2013): ", format(triplo_2013, big.mark = ".")),
        color = "darkorange", vjust = -1, fontface = "bold"
      ) +
      
      # Dot on 2013 EPT point
      geom_point(
        data = data.frame(ANO = x_label, VALOR = ept_2013),
        aes(x = ANO, y = VALOR),
        color = "black", size = 3, shape = 21, fill = "white"
      ) +
      
      # Arrow from label to point
      geom_segment(
        data = data.frame(
          x = x_label + 0.6,
          y = y_label,
          xend = x_label,
          yend = ept_2013
        ),
        aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE,
        arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
        linewidth = 0.8,
        color = "black"
      ) +
      
      # Rich label
      ggtext::geom_richtext(
        data = data.frame(x = x_label + 1, y = y_label),
        aes(x = x, y = y, label = paste0("<b>EPT 2013:</b><br>", format(ept_2013, big.mark = "."))),
        fill = "white",
        label.color = "#00bfc4",
        color = "black",
        size = 4,
        label.size = 0.5,
        label.padding = grid::unit(c(4, 6, 4, 6), "pt"),
        label.r = unit(6, "pt"),
        inherit.aes = FALSE
      ) +
      
      # Axis and title formatting
      scale_x_continuous(breaks = 2007:2035, limits = c(2007, 2035)) +
      scale_y_continuous(labels = scales::comma) +
      
      labs(
        
        title = if (input$oferta_uf == "Brasil") {
          "Brasil – Total Nacional"
        } else {
          paste("UF:", input$oferta_uf)
        },
        
        
        subtitle = paste(input$oferta_ept_var, "vs", input$oferta_other_var),
        x = "Ano",
        y = "Total de Matrículas"
      ) +
      
      coord_cartesian(clip = "off") +
      
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  

  ##############################################################################################################################
  ### OUTPUT  DATA TABLE TAB 3 META 11 VIGENTE DATA TABLE TAB 3 META 11 VIGENTE DATA TABLE TAB 3 META 11 VIGENTE DATA TABLE TAB 3 META 11 VIGENTE 
  ##############################################################################################################################
  
  output$oferta_ept_table <- DT::renderDT({
    req(input$oferta_uf, input$oferta_ept_var, input$oferta_other_var)
    
    # Step 1: Observed data from meta11a_opcoes
    df_obs <- meta11a_opcoes |>
      filter(NM_UF == input$oferta_uf) |>
      group_by(ANO) |>
      summarise(
        EPT = sum(.data[[input$oferta_ept_var]], na.rm = TRUE),
        ENSINO_MEDIO = sum(.data[[input$oferta_other_var]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(TIPO = "OBSERVADO")
    
    # Step 2: Projection (2024–2035)
    years_future <- 2024:2035
    slope_ept <- coef(lm(EPT ~ ANO, data = df_obs |> filter(ANO %in% 2020:2024)))["ANO"]
    slope_med <- coef(lm(ENSINO_MEDIO ~ ANO, data = df_obs |> filter(ANO %in% 2020:2024)))["ANO"]
    start_ept <- df_obs |> filter(ANO == 2024) |> pull(EPT) |> mean(na.rm = TRUE)
    start_med <- df_obs |> filter(ANO == 2024) |> pull(ENSINO_MEDIO) |> mean(na.rm = TRUE)
    
    df_proj <- tibble(
      ANO = years_future,
      EPT = start_ept + slope_ept * (years_future - 2024),
      ENSINO_MEDIO = start_med + slope_med * (years_future - 2024),
      TIPO = "PROJECAO"
    )
    
    # Step 3: Combine observed and projection
    df_all <- bind_rows(df_obs, df_proj)
    
    # Step 4: Compute PNE Meta 11 target
    base_2013_val <- df_all |> filter(ANO == 2013, TIPO == "OBSERVADO") |> pull(EPT)
    triplo_2013 <- if (!is.na(base_2013_val)) 3 * base_2013_val else NA
    df_all$PNE_META11 <- if (!is.na(triplo_2013)) round(triplo_2013) else NA
    
    # Step 5: Transpose format
    df_transposed <- df_all |>
      select(ANO, TIPO, EPT, ENSINO_MEDIO) |>
      pivot_longer(cols = c(EPT, ENSINO_MEDIO), names_to = "VAR", values_to = "VAL") |>
      unite("ROW", VAR, TIPO, sep = "_") |>
      pivot_wider(names_from = ANO, values_from = VAL)
    
    # Step 6: Format numerics with thousand separators
    df_transposed <- df_transposed |>
      mutate(across(where(is.numeric), ~ format(round(.), big.mark = ".", decimal.mark = ",")))
    
    # Step 7: Display DataTable
    datatable(
      df_transposed,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        pageLength = 50,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "copy", text = "Copiar"),
          list(extend = "csv", filename = "Tabela_EPT", text = "CSV"),
          list(extend = "excel", filename = "Tabela_EPT", text = "Excel"),
          list(
            extend = "pdf",
            filename = "Tabela_EPT",
            text = "PDF",
            orientation = "landscape",
            pageSize = "A4",
            messageTop = "Tabela Transposta de Matrículas"
          )
        ),
        scrollX = TRUE
      ),
      class = "stripe nowrap display"
    )
  })
  
    
  ##############################################################################################################################
  ### OUTPUT  PLOT  TAB 4  META11A  FIRST PART  META11A  FIRST PART  META11A  FIRST PART  META11A  FIRST PART
  ##############################################################################################################################
  
  
    output$meta11a_nova_plot <- renderPlot({
      req(input$meta11a_nova_uf, input$meta11a_nova_definicoes)
      
      df <- meta11a_opcoes %>%
        filter(NM_UF == input$meta11a_nova_uf) %>%
        select(ANO, all_of(input$meta11a_nova_definicoes)) %>%
        pivot_longer(
          cols = -ANO,
          names_to = "Definicao",
          values_to = "Meta11a"
        )
      
      # Criar tooltip text com dados brutos
      df_tooltip <- meta11a_opcoes %>%
        filter(NM_UF == input$meta11a_nova_uf) %>%
        select(ANO, SG_UF, NM_UF, QT_MAT_MED, QT_MAT_PROF_TEC_PROPAG, QT_MAT_CURSO_TEC_CT, QT_MAT_CURSO_TEC_CONC) %>%
        mutate(across(where(is.numeric), ~format(round(.), big.mark = ".", decimal.mark = ","))) %>%
        unite("tooltip_text",
              QT_MAT_PROF_TEC_PROPAG, QT_MAT_CURSO_TEC_CT, QT_MAT_CURSO_TEC_CONC, QT_MAT_MED,
              sep = " | ",
              na.rm = TRUE
        ) %>%
        mutate(
          tooltip_text = paste0("Matrículas: ", tooltip_text),
          ANO = as.numeric(ANO)  # <-- Corrige o tipo
        )
      
      
      df <- df %>%
        left_join(df_tooltip %>% select(ANO, tooltip_text), by = "ANO")
      
      # Color labels
      
      # Definir cores para as opções
      definicao_colors <- c(
        "Meta11a_opcao1" = "blue",
        "Meta11a_opcao2" = "red",
        "Meta11a_opcao3" = "#9c6100"
      )
      
      
      df <- df %>%
        mutate(
          color_label = definicao_colors[Definicao]
        )
 
      df <- df %>%
        mutate(
          color_label = definicao_colors[Definicao],
          vjust_label = case_when(
            Definicao == "Meta11a_opcao3" ~ 1.5,   # place below the line
            TRUE ~ -0.8                            # default (above)
          )
        )
      
      

      # Gráfico
  gg <- ggplot(df, aes(x = ANO, y = Meta11a, color = Definicao)) +
        geom_line(size = 1.2) +
        geom_point(size = 2.5) +
        geom_hline(yintercept = 0.5, color = "darkgreen", linetype = "dashed", linewidth = 1.1) +
        annotate(
          "text", x = 2025, y = 0.5,
          label = "Meta 11a: EPT forma 50% da matricula EM",size=6,
          color = "darkorange", vjust = -1, fontface = "bold"
        ) +
    ggtext::geom_richtext(
      aes(label = paste0("<b>", scales::percent(Meta11a, accuracy = 0.1), "</b>"),
          color = Definicao,
          vjust = vjust_label),
      label.color = df$color_label,
      fill = "white",
      size = 4,
      label.size = 0.25,
      label.r = unit(5, "pt"),
      label.padding = unit(c(3, 5, 3, 5), "pt")
    )+
        scale_color_manual(values = definicao_colors) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 0.6)) +
        scale_x_continuous(breaks = 2007:2035, limits=c(2007,2035)) +
        labs(
          title = paste("Meta 11a – Comparação para:", input$meta11a_nova_uf),
          x = "Ano",
          y = "Atingimento PNE Meta 11a: Porcentagem Matricula de EPT sobre EM",
          color = "Definição"
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none",
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 14, colour = "blue", face= "bold"),
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "blue")  
                  ) +
        
        ggtext::geom_textbox(
          data = data.frame(x = 2012.5, y = 0.5),  # posição mais ao centro e abaixo do topo
          aes(x = x, y = y, label = paste(
            "<b>META 11a (Parte 1): </b> DEFINIÇÕES do Numerador de Matrículas:",
            " ",
            "Opção 1: Técnico Médio (Integrado, Concomitante, Subsequente e EJA) (PROPAG)",
            " ",
            "Opção 2: Técnico Médio (Integrado e Concomitante)",
            " ",
            "Opção 3: Técnico Médio (Somente Integrado)",
            " ",
            "Definição Denominador (comum para as 3 opções): Matrículas no Ensino Médio <br> (Variavel: QT_MAT_MED do Censo Escolar do INEP)",
            sep = "<br>"
          )),
          width = unit(0.4, "npc"),        # ajuste fino (50% da largura do gráfico)
          fill = "#fdf6e3",                # bege claro
          box.color = "gray40",            # borda discreta
          halign = 0,                      # texto à esquerda
          color = "black",
          size = 5,
          fontface = "plain",
          lineheight = 1.1,
          box.padding = unit(c(5, 6, 5, 6), "pt"),
          r = unit(6, "pt")                # cantos arredondados
        )
      
  # PROJEÇÕES PARA CADA DEFINIÇÃO DE META 11a
  # ==== NOVAS PROJEÇÕES POR DEFINIÇÃO ====
  target_year <- as.numeric(input$meta11a_target_year)
  anos_proj <- 2024:target_year
  years_left <- target_year - 2024
  
  # Extrair ponto de partida por definição (valores de 2024)
  df_2024_pct <- df %>%
    filter(ANO == 2024) %>%
    select(Definicao, Meta11a)
  
  
  # === Ensino Médio projection logic ===
  df_em <- meta11a_opcoes %>%
    filter(NM_UF == input$meta11a_nova_uf) %>%
    distinct(ANO, QT_MAT_MED) %>%
    filter(ANO %in% 2020:2024) %>%
    rename(MEDIO = QT_MAT_MED)
  
  lm_med <- lm(MEDIO ~ ANO, data = df_em)
  base_slope <- coef(lm_med)["ANO"]
  adjusted_slope <- if (base_slope < 0) {
    base_slope * (2 - input$ensino_slope_factor)  # Invert factor when slope is negative
  } else {
    base_slope * input$ensino_slope_factor
  }
  current_med <- df_em %>% filter(ANO == 2024) %>% pull(MEDIO)
  projected_med <- current_med + adjusted_slope * (target_year - 2024)
  
  em_text <- paste0(
    "<b>Projeção EM</b><br>",
    format(round(projected_med), big.mark = ".", decimal.mark = ",")
  )
  
  # === Raw EPT numeradores de 2024 por opção ===
  df_2024_raw <- meta11a_opcoes %>%
    filter(NM_UF == input$meta11a_nova_uf, ANO == 2024) %>%
    select(QT_MAT_PROF_TEC_PROPAG, QT_MAT_CURSO_TEC_CT, QT_MAT_CURSO_TEC_CONC, QT_MAT_MED) %>%
    summarise(
      Meta11a_opcao1 = QT_MAT_PROF_TEC_PROPAG,
      Meta11a_opcao2 = coalesce(QT_MAT_CURSO_TEC_CT, 0) + coalesce(QT_MAT_CURSO_TEC_CONC, 0),
      Meta11a_opcao3 = QT_MAT_CURSO_TEC_CT,
      MEDIO = QT_MAT_MED
    )
  
  
  # pop up
  # Cálculo dos textos para cada definição
  popup_info <- df_2024_pct %>%
    mutate(
      raw_EPT = map_dbl(Definicao, ~ df_2024_raw[[.x]]),
      growth_abs = (0.5 * projected_med - raw_EPT) / max(1, years_left),
      growth_pct = (50 - Meta11a * 100) / years_left,
      label_text = paste0(
        "<b>Para atingir 50%</b><br>",
        "em ", target_year, ":<br>",
        "crescimento de<br>",
        "<span style='color:", definicao_colors[Definicao], "'><b>",
        round(growth_pct, 1), "%</b></span> ao ano<br>",
        "(", formatC(round(growth_abs), format = "d", big.mark = "."), " alunos/ano)"
      ),
      label_x = target_year + c(-1.8, 0, 1.8)[match(Definicao, names(definicao_colors))],
      label_y = 0.43 + c(0.08, 0, 0.08)[match(Definicao, names(definicao_colors))],
      label_col = definicao_colors[Definicao]
    )
  
  
  # Criar linhas convergentes para 50% a partir de cada ponto de partida
  proj_lines <- df_2024_pct %>%
    mutate(
      proj = map(Meta11a, ~ {
        tibble(
          ANO = anos_proj,
          Meta11a = .x + ((0.5 - .x) / years_left) * (anos_proj - 2024)
        )
      })
    ) %>%
    select(Definicao, proj) %>%
    unnest(proj)
  
  # Adicionar linhas de projeção
  if (nrow(proj_lines) > 0) {
    gg <- gg +
      geom_line(
        data = proj_lines,
        mapping = aes(x = ANO, y = Meta11a, color = Definicao),
        linetype = "dashed",
        linewidth = 1,
        inherit.aes = FALSE
      )
  }
  
  # target point
  # Target point at 50%
  target_df <- data.frame(
    ANO = target_year,
    Meta11a = 0.5
  )
  
  gg <- gg +
    geom_point(
      data = target_df,
      aes(x = ANO, y = Meta11a),
      color = "red",
      fill = "white",
      size = 4,
      shape = 21,
      stroke = 1.5,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = target_df,
      aes(x = ANO, y = Meta11a),
      color = "red",
      size = 2,
      shape = 16,
      inherit.aes = FALSE
    )
  
  ## Pop-up
  # -- One EM projected label (common to all options) --
  em_text <- paste0(
    "<b>EM: Projetado</b><br>",
    format(round(projected_med), big.mark = ".", decimal.mark = ","), " alunos<br>matriculados"
  )
  
  gg <- gg +
    ggtext::geom_richtext(
      data = data.frame(x = target_year, y = 0.61),
      aes(x = x, y = y, label = em_text),
      fill = "white",
      label.color = "blue",
      color = "blue",
      size = 3.5,
      label.size = 0.5,
      label.r = unit(5, "pt"),
      label.padding = unit(c(4, 6, 4, 6), "pt"),
      vjust = 1,
      hjust = 0.5,
      inherit.aes = FALSE
    )
  
  # -- Multiple effort boxes (1 per selected definition) --
  x_offsets <- c("Meta11a_opcao1" = -1.2,
                 "Meta11a_opcao2" =  0,
                 "Meta11a_opcao3" =  1.2)
  
  y_base <- 0.43
  
  # === POPUPS POR DEFINICAO SELECIONADA ===

  for (i in 1:nrow(popup_info)) {
    gg <- gg +
      ggtext::geom_richtext(
        data = popup_info[i, ],
        aes(x = label_x, y = label_y, label = label_text),
        fill = "white",
        label.color = popup_info$label_col[i],
        color = "black",
        size = 3.5,
        label.size = 0.5,
        label.r = unit(6, "pt"),
        label.padding = unit(c(4, 6, 4, 6), "pt"),
        vjust = 1,
        hjust = 0.5,
        inherit.aes = FALSE
      )
  }
  

  # add EM bubble
  gg <- gg +
    ggtext::geom_richtext(
      data = data.frame(x = target_year, y = 0.52),
      aes(x = x, y = y, label = em_text),
      fill = "#d0ebff",            # light sky blue
      label.color = "steelblue",   # border
      color = "black",
      size = 4,
      label.size = 0.5,
      label.r = unit(5, "pt"),
      label.padding = unit(c(4, 6, 4, 6), "pt"),
      fontface = "plain",
      hjust = 0.5,
      vjust = 0,
      inherit.aes = FALSE
    )
  
    
  
    return(gg)
    
    })
    
    
    ######### TAB5 TAB5 TAB5 TAB5  TAB5 TAB5 TAB5 TAB5  TAB5 TAB5 TAB5 TAB5  TAB5 TAB5 TAB5 TAB5  TAB5 TAB5 TAB5 TAB5  TAB5 TAB5 TAB5 TAB5  
    ##############################################################################################################################
    ### OUTPUT PLOT TAB 5  META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE META11 VIGENTE 
    #####################################
    #########################################################################################
    
    output$meta11a_plot2 <- renderPlot({
      df <- RKT_meta11a_data() 
      
      # Define anos de base para tendência histórica
      years_trend <- 2020:2024
      
      # Último ano observado
      latest_year <- max(df$ANO, na.rm = TRUE)
      
      # Ano-alvo definido pelo usuário
      target_year <- as.numeric(input$meta11a_target_year)
      
      # Quantos anos faltam até o ano-alvo
      years_left <- target_year - 2024
      
      # Valores de 2024
      current_row <- df |> filter(ANO == 2024)
      current_pct <- current_row$PCT_EPT
      current_ept <- current_row$EPT
      current_med <- current_row$MEDIO
      
      latest_pct <- df |> filter(ANO == latest_year) |> pull(PCT_EPT)
      
      # Estimar tendência linear para o Ensino Médio
      lm_med <- lm(MEDIO ~ ANO, data = df |> filter(ANO %in% years_trend))
      base_slope <- coef(lm_med)["ANO"]
      
      # Aplicar ajuste com o fator do slider
      slope_factor <- input$ensino_slope_factor  # entre 0.5 e 1.5
      adjusted_slope <- base_slope * slope_factor
      
      # Projeção do Ensino Médio no ano-alvo
      projected_med <- current_med + adjusted_slope * years_left
      
      # Quantidade de estudantes necessária para atingir 50% da meta
      target_students <- 0.5 * projected_med
      
      # Crescimento absoluto necessário (em matrículas EPT)
      growth_abs <- if (!is.na(current_ept) && years_left > 0) {
        (target_students - current_ept) / years_left
      } else {
        NA_real_
      }
      
      # Crescimento percentual anual necessário (em pontos percentuais)
      required_growth <- if (!is.na(current_pct) && years_left > 0) {
        (50 - current_pct) / years_left
      } else {
        NA_real_
      }
      
      # Texto da projeção do Ensino Médio
      em_text <- paste0(
        "<b>EM: Projetado</b><br>",
        format(round(projected_med), big.mark = ".", decimal.mark = ","), " alunos<br>matriculados"
      )
      
      # Prepare the label
      growth_text <- paste0(
        "<b>Para atingir 50%</b><br>",
        "em ", input$meta11a_target_year, ":<br>",
        "crescimento de<br>",
        "<span style='color:#1f5673'><b>", round(required_growth, 1), "%</b></span> ao ano<br>",
        "(", formatC(growth_abs, format = "d", big.mark = "."), " alunos/ano)"
      )
      
      # with checkbox
      
      # Base plot: line and points
      ggpub <- ggplot(df, aes(x = ANO, y = PCT_EPT)) +
        
      # Meta 11a fixed line
        geom_hline(yintercept = 45, color = "darkgreen", linetype = "dashed", linewidth = 1.1) +
        annotate("text", x = 2008, y = 45, label = "Meta 11a público: 45%", color = "darkgreen", vjust = -1, fontface = "bold")
      
      ggpub <- ggpub +
        scale_x_continuous(breaks = 2007:2026, limits = c(2007, 2026)) +
        scale_y_continuous(limits = c(0, 130), labels = scales::percent_format(scale = 1)) +
        labs(
          title = paste("UF:", input$meta11a_uf),
          subtitle = "Expansão no EPT na rede Pública como % da expansão no EPT",
          x = "Ano",
          y = "% de Matrículas em EPT"
        ) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      
      df_pub_share <- RKT_meta11a_expansao_publica_hist()
      
      ggpub <- ggpub +
        geom_point(
          data = df_pub_share,
          aes(x = ANO, y = 100*SHARE_PUB),
          color = "#FF6600",
          size = 5,
          inherit.aes = FALSE
        )+ 
        ggtext::geom_richtext(
          data = df_pub_share |> filter(!is.na(SHARE_PUB)),
          aes(
            x = ANO,
            y = 100 * SHARE_PUB,
            label = paste0(
              "<b>Expansão ", ANO - 1, "–", ANO, "</b><br>",
              "EPT total: ", format(round(DELTA_TOTAL), big.mark = "."), "<br>",
              "EPT pública: ", format(round(DELTA_PUB), big.mark = "."), "<br>",
              "Público: ", round(100 * SHARE_PUB, 1), "%"
            )
          ),
          fill = "white",
          label.color = "#FF6600",
          color = "black",
          size = 3,
          label.size = 0.4,
          label.r = unit(6, "pt"),
          label.padding = unit(c(3, 5, 3, 5), "pt"),
          vjust = -0.5,
          inherit.aes = FALSE
        )
      
      return(ggpub)
      
    })
    
  
  
      
}  
  

shinyApp(ui = ui, server = server)