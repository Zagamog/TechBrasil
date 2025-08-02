# test6.R
library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(DT)

# ————— 1) Load your four “_wide” tables exactly as before —————
load("df_mat_uf.rda")
load("df_mat_eixo.rda")
load("df_mat_area.rda")
load("df_mat_curso.rda")
load("meta11a_opcoes.rda")
load("df_exarcu.rda")      # << new
load("rais_cbo6_uf23.rda")  
load("rais_cbo6_uf24.rda")



df_mat_uf_wide <- df_mat_uf %>%
  select(CO_UF, NM_UF, SG_UF, ANO, QT_MAT_CURSO_TEC_UF) %>%
  pivot_wider(
    names_from  = ANO,
    values_from = QT_MAT_CURSO_TEC_UF,
    values_fill = list(QT_MAT_CURSO_TEC_UF = 0),
    values_fn   = sum
  ) %>%
  rename(`Matrículas 2023` = `2023`, `Matrículas 2024` = `2024`)

df_mat_eixo_wide <- df_mat_eixo %>%
  select(CO_UF, NM_UF, SG_UF, `Eixo Tecnológico`, ANO, QT_MAT_CURSO_TEC_EIX) %>%
  pivot_wider(
    names_from  = ANO,
    values_from = QT_MAT_CURSO_TEC_EIX,
    values_fill = list(QT_MAT_CURSO_TEC_EIX = 0),
    values_fn   = sum
  ) %>%
  rename(`Matrículas 2023` = `2023`, `Matrículas 2024` = `2024`)

df_mat_area_wide <- df_mat_area %>%
  select(CO_UF, NM_UF, SG_UF, `Área Tecnológica`, ANO, QT_MAT_CURSO_TEC_ARE) %>%
  pivot_wider(
    names_from  = ANO,
    values_from = QT_MAT_CURSO_TEC_ARE,
    values_fill = list(QT_MAT_CURSO_TEC_ARE = 0),
    values_fn   = sum
  ) %>%
  rename(`Matrículas 2023` = `2023`, `Matrículas 2024` = `2024`)

df_mat_curso_wide <- df_mat_curso %>%
  select(CO_UF, NM_UF, SG_UF, IDX_EIXCUR, `Denominação do Curso`, ANO, QT_MAT_CURSO_TEC_CUR) %>%
  pivot_wider(
    names_from  = ANO,
    values_from = QT_MAT_CURSO_TEC_CUR,
    values_fill = list(QT_MAT_CURSO_TEC_CUR = 0),
    values_fn   = sum
  ) %>%
  rename(`Matrículas 2023` = `2023`, `Matrículas 2024` = `2024`)

###
#CBO
# Combine and add year label
df_rais_all <- bind_rows(
  rais_cbo6_uf23 %>% mutate(ANO = 2023),
  rais_cbo6_uf24 %>% mutate(ANO = 2024)
)

sum(is.na(df_rais_all$CodCBO))

# ——— Aggregates by Brazil level ———

# 1. CBO 1-digit (cbo_1dig)
rais_br_cbo1 <- df_rais_all %>% filter(!is.na(cbo_gragru)) %>%
  group_by(ANO, cbo_1dig, cbo_gragru) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
  mutate(NM_UF = "Brasil", SG_UF = "BR")
sum(is.na(rais_br_cbo1$cbo_1dig))


# 2. CBO 4-digit
rais_br_cbo4 <- df_rais_all %>% filter(!is.na(cbo_familia)) %>%
  group_by(ANO, cbo_4dig, cbo_familia) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
  mutate(NM_UF = "Brasil", SG_UF = "BR")

# 3. Full CodCBO level (occupation level)
rais_br_codcbo <- df_rais_all %>% filter(!is.na(`Ocupação`)) %>%
  group_by(ANO, CodCBO, `Ocupação`) %>%
  summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop") %>%
  mutate(NM_UF = "Brasil", SG_UF = "BR")

# ——— Merge back Brasil with UF-level if needed ———
df_rais1dig <- bind_rows(df_rais_all %>% 
                           group_by(ANO, NM_UF, SG_UF, cbo_1dig, cbo_gragru) %>%
                           summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop"),
                         rais_br_cbo1)

df_rais4dig <- bind_rows(df_rais_all %>% 
                           group_by(ANO, NM_UF, SG_UF, cbo_4dig, cbo_familia) %>%
                           summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop"),
                         rais_br_cbo4)

df_raisCodCBO <- bind_rows(df_rais_all %>% 
                             group_by(ANO, NM_UF, SG_UF, CodCBO, `Ocupação`) %>%
                             summarise(vinculos = sum(vinculos, na.rm = TRUE), .groups = "drop"),
                           rais_br_codcbo)


df_rais1dig_wide <- df_rais1dig %>%
  pivot_wider(
    names_from  = ANO,
    values_from = vinculos,
    values_fill = list(vinculos = 0),
    values_fn   = sum
  ) %>%
  rename(`Vínculos 2023` = `2023`, `Vínculos 2024` = `2024`)


df_rais4dig_wide <- df_rais4dig %>%
  pivot_wider(
    names_from  = ANO,
    values_from = vinculos,
    values_fill = list(vinculos = 0),
    values_fn   = sum
  ) %>%
  rename(`Vínculos 2023` = `2023`, `Vínculos 2024` = `2024`)

df_raisCodCBO_wide <- df_raisCodCBO %>%
  pivot_wider(
    names_from  = ANO,
    values_from = vinculos,
    values_fill = list(vinculos = 0),
    values_fn   = sum
  ) %>%
  rename(`Vínculos 2023` = `2023`, `Vínculos 2024` = `2024`)


# For third part of tab both supply and demand together
load("qbq_ocup_cmento1.rda")
load("cnct_qbq_matches.rda")
load("qbq_cnct_matches.rda")



# ————— 2) UI: just UF selector + two DTs —————
# Simplified UI layout based on new design:
# 1. Top row: UF + match direction selector
# 2. Main content: two conditional columns based on match direction

ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/custom.css")),
  
  div(class = "checkbox-dark-panel",
      
      # Top controls row (unchanged)
      fluidRow(
        column(
          width = 3,
          selectizeInput("uf_select", "Selecionar UF ou Brasil:",
                         choices = NULL,
                         options = list(placeholder = "Brasil"))
        ),
        column(
          width = 3,
          radioButtons("match_direction", "Direção da Conexão:",
                       choices = c("Oferta → Demanda", "Demanda → Oferta"),
                       selected = "Oferta → Demanda",
                       inline = TRUE)
        ),
        column(
          width = 3,
          checkboxGroupInput("level_filter", "Níveis de Trabalhador Técnico:",
                             choices = 1:6,
                             selected = 1:3,
                             inline = TRUE)
        ),
        column(
          width = 3,
          sliderInput("score_thresh", "Limite de Proximidade:",
                      min = 0, max = 1, value = 0.2, step = 0.01)
        )
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.match_direction == 'Oferta \u2192 Demanda'",
          column(width = 6,
                 h4("1a - Matrículas por Eixo, Área e Curso"),
                 DTOutput("agg_table"),
                 hr(),
                 h4("1b - Matrículas por Área"),
                 DTOutput("area_table"),
                 hr(),
                 h4("1c - Matrículas por Curso"),
                 DTOutput("curso_table")
          ),  # 👈 CLOSE this column!
          
          column(width = 6,
                 h4("2a – Vínculos por Grande Grupo"),
                 DTOutput("match_summary_table"),
                 hr(),
                 h4("2b – Vínculos por Família"),
                 DTOutput("match_familia_table"),
                 hr(),
                 h4("2c – Vínculos por Ocupação"),
                 DTOutput("cbo6_table")
          )
        ),
        
        conditionalPanel(
          condition = "input.match_direction == 'Demanda \u2192 Oferta'",
          column(width = 6,
                 h4("3a – Vínculos por Grande Grupo"),
                 DTOutput("cbo1_table"),
                 hr(),
                 h4("3b – Vínculos por Família"),
                 DTOutput("cbo4_table"),
                 hr(),
                 h4("3c – Vínculos por Ocupação"),
                 DTOutput("cbo6b_table")  # or remove if not used in this direction
          )
        )
      )
  ) # end div
) # end fluidPage






# ————— 3) Server: UF → Eixo-table → Área-table —————
server <- function(input, output, session) {
  
  # populate UF dropdown
  observe({
    all_ufs <- sort(unique(df_mat_eixo_wide$NM_UF))
    updateSelectizeInput(
      session, "uf_select",
      choices  = c("Brasil", all_ufs),
      selected = "Brasil",
      server   = TRUE
    )
  })
  
  # reactive DF of Eixo rows for the chosen UF
  RKT_agg_df <- reactive({
    req(input$uf_select)
    df_mat_eixo_wide %>%
      filter(NM_UF == input$uf_select) %>%
      select(NM_UF, `Eixo Tecnológico`, `Matrículas 2023`, `Matrículas 2024`) %>%
      arrange(desc(`Matrículas 2024`))
  })
  
  # 1) render the Eixo-table with single selection + total row + formatting
  output$agg_table <- renderDT({
    df0 <- RKT_agg_df()
    
    # tack on the Total row
    total_row <- df0 %>%
      summarise(
        NM_UF               = "Total",
        `Eixo Tecnológico` = "",
        `Matrículas 2023`   = sum(`Matrículas 2023`, na.rm = TRUE),
        `Matrículas 2024`   = sum(`Matrículas 2024`, na.rm = TRUE)
      )
    df_final <- bind_rows(df0, total_row)
    
    datatable(
      df_final,
      selection = "single",
      rownames  = FALSE,
      class     = "compact stripe",
      options   = list(
        pageLength = nrow(df_final),
        autoWidth  = TRUE,
        language   = list(
          url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
        )
      )
    ) %>%
      formatCurrency(
        columns   = c("Matrículas 2023", "Matrículas 2024"),
        currency  = "",
        interval  = 3,
        mark      = ".",
        dec.mark  = ","
      ) %>%
      formatStyle(
        columns     = c("Matrículas 2023", "Matrículas 2024"),
        `text-align` = "right"
      ) %>%
      formatStyle(
        "NM_UF",
        target     = "row",
        fontWeight = styleEqual("Total", "bold"),
        background = styleEqual("Total", "#0d0863")
      )
  })
  
  # grab the clicked Eixo from agg_table
  # Option A: base R [[
  RKT_selected_eixo <- reactive({
    req(input$agg_table_rows_selected)
    df <- RKT_agg_df()
    df[[ "Eixo Tecnológico" ]][ input$agg_table_rows_selected ]
  })
  
  # 2) render the Área-table once an Eixo is clicked
 
  # --- 3) build a reactive DF for Área so selection indices line up ---
  RKT_area_df <- reactive({
    eixo <- RKT_selected_eixo()
    req(eixo, input$uf_select)
    # valid Áreas for this eixo
    df_exarcu %>%
      filter(`Eixo Tecnológico` == eixo) %>%
      distinct(`Área Tecnológica`) %>%
      # join in the UF counts
      left_join(
        df_mat_area_wide %>% filter(NM_UF == input$uf_select),
        by = "Área Tecnológica"
      ) %>%
      select(
        NM_UF,
        `Área Tecnológica`,
        `Matrículas 2023`,
        `Matrículas 2024`
      ) %>%
      arrange(desc(`Matrículas 2024`))
  })
  
  # replace your existing renderDT for area_table with this (enabling single selection)
  output$area_table <- renderDT({
    dat <- RKT_area_df()
    datatable(
      dat,
      selection = "single",
      rownames  = FALSE,
      class     = "compact stripe",
      options   = list(
        pageLength = nrow(dat),
        autoWidth  = TRUE,
        language   = list(
          url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
        )
      )
    ) %>%
      formatCurrency(
        c("Matrículas 2023","Matrículas 2024"),
        currency = "", interval = 3, mark = ".", dec.mark = ","
      ) %>%
      formatStyle(
        c("Matrículas 2023","Matrículas 2024"),
        `text-align` = "right"
      )
  })
  
  # capture the clicked Área
  RKT_selected_area <- reactive({
    req(input$area_table_rows_selected)
    RKT_area_df()$`Área Tecnológica`[ input$area_table_rows_selected ]
  })
  
  # --- 4) cursos for the selected UF ↦ Eixo ↦ Área ---
  RKT_curso_df <- reactive({
    uf   <- input$uf_select
    eixo <- RKT_selected_eixo()
    area <- RKT_selected_area()
    req(uf, eixo, area)
    
    # which cursos belong?
    df_exarcu %>%
      filter(
        `Eixo Tecnológico` == eixo,
        `Área Tecnológica` == area
      ) %>%
      distinct(IDX_EIXCUR, `Denominação do Curso`) %>%
      # join in the UF×curso counts
      left_join(
        df_mat_curso_wide %>% filter(NM_UF == uf),
        by = c("IDX_EIXCUR", "Denominação do Curso")
      ) %>%
      select(
        NM_UF,
        `Denominação do Curso`,
        `Matrículas 2023`,
        `Matrículas 2024`
      ) %>%
      arrange(desc(`Matrículas 2024`))
  })
  
  # 5) render the Curso‐table
  output$curso_table <- renderDT({
    dat <- RKT_curso_df()
    datatable(
      dat,
      rownames = FALSE,
      class    = "compact stripe",
      options  = list(
        pageLength = nrow(dat),
        autoWidth  = TRUE,
        language   = list(
          url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
        )
      )
    ) %>%
      formatCurrency(
        c("Matrículas 2023","Matrículas 2024"),
        currency = "", interval = 3, mark = ".", dec.mark = ","
      ) %>%
      formatStyle(
        c("Matrículas 2023","Matrículas 2024"),
        `text-align` = "right"
      )
  })
  
############################################ CBO CBO
  
############ Level 1 Level  CBO
#################################################
  RKT_cbo1_df <- reactive({
    req(input$uf_select)
    df_rais1dig_wide %>%
      filter(NM_UF == input$uf_select) %>%
      select(NM_UF, cbo_gragru, `Vínculos 2023`, `Vínculos 2024`) %>%
      arrange(desc(`Vínculos 2024`))
  })
  
  output$cbo1_table <- renderDT({
    dat <- RKT_cbo1_df()
    datatable(
      dat,
      selection = "single",
      rownames  = FALSE,
      class     = "compact stripe",
      options   = list(
        pageLength = nrow(dat),
        autoWidth  = TRUE,
        language   = list(
          url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
        )
      )
    ) %>%
      formatCurrency(
        c("Vínculos 2023","Vínculos 2024"),
        currency = "", interval = 3, mark = ".", dec.mark = ","
      ) %>%
      formatStyle(
        c("Vínculos 2023","Vínculos 2024"),
        `text-align` = "right"
      )
  })

  ############ Level 2 Level  Familia
  #################################################
  
  RKT_selected_cbo1 <- reactive({
    req(input$cbo1_table_rows_selected)
    RKT_cbo1_df()$cbo_gragru[ input$cbo1_table_rows_selected ]
  })
  
  
  RKT_cbo4_df <- reactive({
    req(input$uf_select, RKT_selected_cbo1())
    
    selected_1dig <- df_rais1dig %>%
      filter(cbo_gragru == RKT_selected_cbo1()) %>%
      distinct(cbo_1dig) %>%
      pull(cbo_1dig)
    
    df_rais4dig_wide %>%
      filter(
        NM_UF == input$uf_select,
        substr(cbo_4dig, 1, 1) %in% selected_1dig
      ) %>%
      select(NM_UF, cbo_familia, `Vínculos 2023`, `Vínculos 2024`) %>%
      arrange(desc(`Vínculos 2024`))
  })

  output$cbo4_table <- renderDT({
    dat <- RKT_cbo4_df()
    datatable(
      dat,
      selection = "single",
      rownames  = FALSE,
      class     = "compact stripe",
      options   = list(
        pageLength = 10,                          # default to 10 rows
        scrollY    = "300px",                     # optional scroll height
        paging     = TRUE,
        lengthMenu = c(5, 10, 25, 50, 100),       # allow user to choose
        autoWidth  = TRUE,
        language   = list(
          url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
        )
      )
    ) %>%
      formatCurrency(
        c("Vínculos 2023", "Vínculos 2024"),
        currency = "", interval = 3, mark = ".", dec.mark = ","
      ) %>%
      formatStyle(
        c("Vínculos 2023", "Vínculos 2024"),
        `text-align` = "right"
      )
  })
  
  
  ############ Level 3 Level  Ocupação
  #################################################  
  
  RKT_selected_cbo_familia <- reactive({
    req(input$cbo4_table_rows_selected)
    RKT_cbo4_df()$cbo_familia[ input$cbo4_table_rows_selected ]
  })
  

  RKT_cbo6b_table <- reactive({
    req(input$uf_select, RKT_selected_cbo_familia())
    
    # Get the actual 4-digit CBO prefix
    selected_4dig <- df_rais4dig %>%
      filter(cbo_familia == RKT_selected_cbo_familia()) %>%
      distinct(cbo_4dig) %>%
      pull(cbo_4dig)
    
    req(length(selected_4dig) > 0)
    
    df_raisCodCBO_wide %>%
      filter(
        NM_UF == input$uf_select,
        substr(CodCBO, 1, 4) %in% selected_4dig
      ) %>%
      select(NM_UF, Ocupação, `Vínculos 2023`, `Vínculos 2024`) %>%
      arrange(desc(`Vínculos 2024`))
  })
  
  
  output$cbo6b_table <- renderDT({
    dat <- RKT_cbo6b_table()
    datatable(
      dat,
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        pageLength = nrow(dat),
        autoWidth  = TRUE,
        language   = list(
          url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json"
        )
      )
    ) %>%
      formatCurrency(
        c("Vínculos 2023", "Vínculos 2024"),
        currency = "", interval = 3, mark = ".", dec.mark = ","
      ) %>%
      formatStyle(
        c("Vínculos 2023", "Vínculos 2024"),
        `text-align` = "right"
      )
  })
  
######MATCH MATCH MATCH ###########################################################################
######################################## EIXO-AREA   EIXO-AREA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # observeEvent(RKT_selected_eixo(), {
  #   cat("🧲 Forcing renderDT for cbo6_table after eixo selection\n")
  #   output$cbo6_table <- renderDT({  # re-bind the output
  #     req(RKT_selected_eixo())
  #     cat("🚀 renderDT for cbo6_table running...\n")
  #     
  #     df <- RKT_cbo6_df()
  #     validate(need(nrow(df) > 0, "Nenhuma ocupação correspondente encontrada."))
  #     
  #     datatable(
  #       df,
  #       rownames = FALSE,
  #       class = "compact stripe",
  #       options = list(
  #         pageLength = 10,
  #         paging     = TRUE,
  #         autoWidth  = TRUE,
  #         language   = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json")
  #       )
  #     ) %>%
  #       formatCurrency(c("Vínculos 2023", "Vínculos 2024"), currency = "", interval = 3, mark = ".", dec.mark = ",") %>%
  #       formatStyle(c("Vínculos 2023", "Vínculos 2024"), `text-align` = "right")
  #   })
  # })
  # 
  # 
  # Deduplicated df_gg for safe joins
  df_gg_clean <- df_gg %>%
    group_by(cbo_gragru) %>%
    slice(1) %>%
    ungroup()
  
  
  RKT_base_matches <- reactive({
    req(input$uf_select, input$level_filter, input$score_thresh, RKT_selected_eixo())
    
    eixo_sel <- RKT_selected_eixo()
    
    cnct_qbq_matches %>%
      filter(final_score >= input$score_thresh) %>%
      mutate(
        CodCBO     = as.character(CodCBO),
        eixo_code  = substr(IDX_EIXARECUR, 1, 2),
        curso_code = substr(IDX_EIXARECUR, 5, 6),
        IDX_EIXCUR = paste0(eixo_code, sprintf("%03d", as.integer(curso_code)))
      ) %>%
      inner_join(qbq_ocup_cmento1 %>% select(-cbo_1dig), by = "CodCBO") %>%
      filter(NivelOcupacao %in% input$level_filter) %>%
      inner_join(df_exarcu %>% select(IDX_EIXCUR, `Área Tecnológica`, `Eixo Tecnológico`), by = "IDX_EIXCUR") %>%
      filter(`Eixo Tecnológico` == eixo_sel) %>%
      mutate(cbo_1dig = substr(CodCBO, 1, 1)) %>%
      left_join(df_raisCodCBO_wide %>%
                  filter(NM_UF == input$uf_select) %>%
                  select(CodCBO, `Vínculos 2023`, `Vínculos 2024`),
                by = "CodCBO") %>%
      distinct(CodCBO, IDX_EIXCUR, .keep_all = TRUE)  # de-dupe matching combos
  })
  
  
  
  #   observe({
  #   cat("Selected Eixo:", RKT_selected_eixo(), "\n")
  # })
  
  # Oferta → Demanda
  RKT_match_summary_df <- reactive({
    req(input$match_direction == "Oferta \u2192 Demanda")
    req(RKT_selected_eixo())
    
    base <- RKT_base_matches()
    
    # Already filtered by selected eixo in base
    base %>%
      distinct(cbo_1dig, CodCBO, .keep_all = TRUE) %>%
      group_by(cbo_1dig) %>%
      summarise(
        `Vínculos 2023` = sum(`Vínculos 2023`, na.rm = TRUE),
        `Vínculos 2024` = sum(`Vínculos 2024`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(df_gg, by = "cbo_1dig") %>%
      select(`Grande Grupo` = cbo_gragru, `Vínculos 2023`, `Vínculos 2024`) %>%
      arrange(desc(`Vínculos 2024`))
  })
  
  output$match_summary_table <- renderDT({
    df <- RKT_match_summary_df()
    datatable(
      df,
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json")
      )
    ) %>%
      formatCurrency(c("Vínculos 2023", "Vínculos 2024"), currency = "", interval = 3, mark = ".", dec.mark = ",") %>%
      formatStyle(c("Vínculos 2023", "Vínculos 2024"), `text-align` = "right")
  })
  

  
  
  RKT_match_summary_df_reverse <- reactive({
    req(input$match_direction == "Demanda \u2192 Oferta")
    # Insert logic for demand → supply side if you have it
    tibble(
      `Grande Grupo` = character(0),
      `Matrículas 2023` = numeric(0),
      `Matrículas 2024` = numeric(0)
    )
  })
  
  ######MATCH MATCH MATCH ###########################################################################
  ######################################## AREA-FAMILIA   AREA-FAMILIA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  RKT_selected_grande_grupo <- reactive({
    req(input$match_summary_table_rows_selected)
    df <- RKT_match_summary_df()
    df$`Grande Grupo`[input$match_summary_table_rows_selected]
    
  })
  
    
  RKT_match_detail_df <- reactive({
    base <- RKT_base_matches()
  #  cat("📌 RKT_base_matches rows (detail):", nrow(base), "\n")
    
    selected_cbo1 <- if (!is.null(input$cbo1_table_rows_selected)) {
      RKT_selected_grande_grupo() %>%
        left_join(df_gg_clean, by = c("Grande Grupo" = "cbo_gragru")) %>%
        pull(cbo_1dig)
    } else {
      RKT_match_summary_df() %>%
        left_join(df_gg_clean, by = c("Grande Grupo" = "cbo_gragru")) %>%
        pull(cbo_1dig)
    }
    
    selected_cbo1 <- as.character(selected_cbo1)
  #  cat("📌 Selected cbo_1dig (detail):", paste(unique(selected_cbo1), collapse = ", "), "\n")
    
    df_out <- base %>%
      filter(cbo_1dig %in% selected_cbo1) %>%
      distinct(`Área Tecnológica`, CodCBO, .keep_all = TRUE) %>%
      group_by(`Área Tecnológica`) %>%
      summarise(
        `Vínculos 2023` = sum(`Vínculos 2023`, na.rm = TRUE),
        `Vínculos 2024` = sum(`Vínculos 2024`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`Vínculos 2024`))
    
  #  cat("📦 Rows in match_familia_table:", nrow(df_out), "\n")
    return(df_out)
  })
  
  
  
  
  
  
  output$match_familia_table <- renderDT({
    df <- RKT_match_detail_df()
    validate(need(nrow(df) > 0, "Nenhuma família encontrada."))
    
    datatable(
      df,
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        pageLength = 10,
        paging     = TRUE,
        autoWidth  = TRUE,
        language   = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json")
      )
    ) %>%
      formatCurrency(c("Vínculos 2023", "Vínculos 2024"), currency = "", interval = 3, mark = ".", dec.mark = ",") %>%
      formatStyle(c("Vínculos 2023", "Vínculos 2024"), `text-align` = "right")
  })
  
  
  ######MATCH MATCH MATCH ###########################################################################
  ######################################## FAMILIA-OCUPACAO   FAMILIA-OCUPACAO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  RKT_cbo6_df <- reactive({
    req(RKT_selected_eixo())  # 👈 blocks until user picks an eixo
    base <- RKT_base_matches()
  #  cat("🔍 [cbo6] RKT_base_matches rows:", nrow(base), "\n")
    
    selected_cbo1 <- if (!is.null(input$cbo1_table_rows_selected)) {
    #  cat("🔍 [cbo6] Using clicked row from DT1 (cbo1_table_rows_selected)\n")
      RKT_selected_grande_grupo() %>%
        left_join(df_gg_clean, by = c("Grande Grupo" = "cbo_gragru")) %>%
        pull(cbo_1dig)
    } else {
   #   cat("🔍 [cbo6] Using all rows from RKT_match_summary_df()\n")
      RKT_match_summary_df() %>%
        left_join(df_gg_clean, by = c("Grande Grupo" = "cbo_gragru")) %>%
        pull(cbo_1dig)
    }
    
    selected_cbo1 <- as.character(selected_cbo1)
  #  cat("🔍 [cbo6] Selected cbo_1dig values:", paste(unique(selected_cbo1), collapse = ", "), "\n")
    
    filtered_base <- base %>%
      filter(cbo_1dig %in% selected_cbo1)
    
  #  cat("🔍 [cbo6] Rows after filtering base:", nrow(filtered_base), "\n")
    
    df_out <- filtered_base %>%
      distinct(CodCBO, .keep_all = TRUE) %>%
      group_by(CodCBO, cbo_prigru) %>%
      summarise(
        `Vínculos 2023` = sum(`Vínculos 2023`, na.rm = TRUE),
        `Vínculos 2024` = sum(`Vínculos 2024`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`Vínculos 2024`))
    
 #   cat("📦 [cbo6] Final output rows:", nrow(df_out), "\n")
    return(df_out)
  })
  
  
  
  
  output$cbo6_table <- renderDT({
    req(RKT_selected_eixo())  # 👈 blocks until user picks an eixo
#    cat("🚀 renderDT for cbo6_table running...\n")  # <— MUST appear when Eixo is selected
    df <- RKT_cbo6_df()
    validate(need(nrow(df) > 0, "Nenhuma ocupação correspondente encontrada."))
    
    datatable(
      df,
      rownames = FALSE,
      class = "compact stripe",
      options = list(
        pageLength = 10,
        paging     = TRUE,
        autoWidth  = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.21/i18n/Portuguese-Brasil.json")
      )
    ) %>%
      formatCurrency(c("Vínculos 2023", "Vínculos 2024"), currency = "", interval = 3, mark = ".", dec.mark = ",") %>%
      formatStyle(c("Vínculos 2023", "Vínculos 2024"), `text-align` = "right")
  })
  
  
  
  
  
}

shinyApp(ui, server)
