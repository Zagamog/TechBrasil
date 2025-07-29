 
    # 1) Carrega a sua tabela de 145 cenários válidos
    load("dfcen_val.rda")  # assume que dfcen_val tem colunas A, F, I, J
    # agora a coluna F deve guardar "G1","G2","G3"
    
    # 2) Mapeamento completo label → valor, EXATAMENTE como no UI
    all_choices <- list(
      A = c(
        "Sem abatimento"  = "A1",
        "10% abatimento" = "A2",
        "20% abatimento" = "A3"
      ),
      G = c(
        "1%"   = "G1",
        "1.5%" = "G2",
        "2%"   = "G3"
      ),
      I = c(
        "0%"   = "I1",
        "0.5%" = "I2",
        "2%"   = "I3",
        "1%"   = "I4"
      ),
      J = c(
        "0%"          = "J1",
        "1%"          = "J2",
        "2%"          = "J3",
        "4% (Não Adere)" = "J4"
      )
    )
    
    # 3) Função auxiliar: dado um dimensão dim ∈ {A,F,I,J}
    #    e as seleções nos demais sel (lista com 3 elementos),
    #    retorna o vetor de códigos permitidos nessa dim
    valid_codes <- function(dim, sel) {
      df <- dfcen_val
      for(d in names(sel)) {
        df <- df[df[[d]] %in% sel[[d]], , drop = FALSE]
      }
      sort(unique(df[[dim]]))
    }
    
    # 4) Observador único que reage a qualquer mudança nos 4 grupos
    observeEvent(
      list(input$choice_A, input$choice_G, input$choice_I, input$choice_J),
      {
        # 4.1) Agrupa seleções atuais
        sel <- list(
          A = input$choice_A,
          G = input$choice_G,  # note: usamos G aqui porque a coluna ainda se chama F
          I = input$choice_I,
          J = input$choice_J
        )
        
        # 4.2) Para cada dimensão, decide quais opções manter
        for(dim in c("A","G","I","J")) {
          others <- sel[names(sel) != dim]
          if (all(lengths(others) > 0)) {
            ok <- valid_codes(dim, others)
            # filtra o mapeamento completo para essas chaves
            keep <- all_choices[[dim]][ all_choices[[dim]] %in% ok ]
          } else {
            # se algum dos outros ainda não foi selecionado, mostramos tudo
            keep <- all_choices[[dim]]
          }
          
          # 4.3) Atualiza o widget correspondente:
          #      - para A → "choice_A"
          #      - para F → "choice_G"
          #      - para I → "choice_I"
          #      - para J → "choice_J"
          inputId <- switch(dim,
                            A = "choice_A",
                            G = "choice_G",
                            I = "choice_I",
                            J = "choice_J")
          updatePrettyCheckboxGroup(
            session,
            inputId  = inputId,
            choices  = keep,
            selected = intersect(sel[[dim]], keep),
            inline   = TRUE
          )
        }
      },
      ignoreInit = TRUE
    )
    
    # --- aqui você pode adicionar os seus outputs, gráficos, DT etc ---
  }
  
  