simulador.fundeb::simula_fundeb <- function(
    dados_matriculas,           # Main data frame of student enrollments (matrículas)
    dados_complementar,         # Complementary data for socio-fiscal factors
    dados_peso,                 # Weights applied to types of enrollment
    max_nse = 1.05,             # Max allowed value for socio-economic index (NSE)
    min_nse = 0.95,             # Min allowed value for NSE
    max_nf = 1.05,              # Max allowed value for fiscal capacity index (NF)
    min_nf = 0.95,              # Min allowed value for NF
    complementacao_vaaf,        # Total VAAF resources to distribute
    complementacao_vaat,        # Total VAAT resources to distribute
    complementacao_vaar,        # Total VAAR resources to distribute
    checa_valores = FALSE       # Flag to perform input validation
) {
  
  # Optionally validate inputs and parameters
  if (checa_valores) {
    simulador.fundeb:::checa_dados_simulador(
      dados_matriculas = dados_matriculas,
      dados_complementar = dados_complementar,
      dados_peso = dados_peso,
      complementacao_vaaf = complementacao_vaaf,
      complementacao_vaat = complementacao_vaat,
      complementacao_vaar = complementacao_vaar,
      max_nse = max_nse,
      min_nse = min_nse,
      max_nf = max_nf,
      min_nf = min_nf
    )
  }
  
  # Identify entities (e.g., municipalities) ineligible for VAAT complement
  entes_excluidos <- dados_complementar[dados_complementar[["inabilitados_vaat"]] == TRUE, ]$ibge
  
  # Step 1: Apply weights to enrollment data by education stage
  df_matriculas <- pondera_matriculas_etapa(
    dados_matriculas = dados_matriculas,
    dados_peso = dados_peso
  )
  
  # Step 2: Rescale socio-fiscal indexes to stay within predefined bounds
  dados_complementar$nse <- reescala_vetor(dados_complementar$nse, maximo = max_nse, minimo = min_nse)
  dados_complementar$nf  <- reescala_vetor(dados_complementar$nf,  maximo = max_nf,  minimo = min_nf)
  
  # Step 3: Adjust enrollment data based on socio-fiscal factors (NSE and NF)
  df_entes <- pondera_matriculas_sociofiscal(
    dados_matriculas = df_matriculas,
    dados_complementar = dados_complementar
  )
  
  # Step 4: Aggregate values to the state level
  df_estados <- gera_fundo_estadual(df_entes)
  
  # Step 5: Perform equalization of VAAF at state level
  df_fundo_estadual <- equaliza_fundo(
    df_estados,
    complementacao_uniao = complementacao_vaaf,
    var_ordem = "vaaf_estado_inicial",
    var_matriculas = "matriculas_estado_vaaf",
    var_recursos = "recursos_estado_vaaf",
    identificador = "uf",
    entes_excluidos = NULL
  )
  
  # Step 6: Merge equalized VAAF values back into the original entity data
  df_entes <- une_vaaf(df_entes, df_estados, df_fundo_estadual)
  
  # Step 7: Calculate pre-equalization VAAT values (per student)
  df_entes$vaat_pre <- df_entes$recursos_vaat / df_entes$matriculas_vaat
  
  # Step 8: Equalize VAAT across eligible entities (municipal level)
  fundo_vaat <- equaliza_fundo(
    df_entes,
    complementacao_uniao = complementacao_vaat,
    var_ordem = "vaat_pre",
    var_matriculas = "matriculas_vaat",
    var_recursos = "recursos_vaat",
    identificador = "ibge",
    entes_excluidos = entes_excluidos
  )
  
  # Step 9: Merge VAAT equalization results into main data
  df_entes <- une_vaat(df_entes, fundo_vaat)
  
  # Step 10: Apply proportional distribution of VAAR resources
  df_entes$complemento_vaar <- df_entes$peso_vaar * complementacao_vaar
  
  # Step 11: Calculate differences (complements) after equalizations
  df_entes$complemento_vaaf <- df_entes$recursos_vaaf_final - df_entes$recursos_vaaf
  df_entes$complemento_vaat <- df_entes$recursos_vaat_final - df_entes$recursos_vaat
  
  # Step 12: Sum total federal complement for each entity
  df_entes$complemento_uniao <- df_entes$complemento_vaar + df_entes$complemento_vaat + df_entes$complemento_vaaf
  
  # Step 13: Compute total post-complement Fundeb resources
  df_entes$recursos_fundeb <- df_entes$recursos_vaaf + df_entes$complemento_uniao
  
  # Step 14: Return final dataframe with selected columns
  df_entes <- df_entes[, c(
    "ibge", "uf", "nome", "matriculas_vaaf", "matriculas_vaat",
    "recursos_vaaf", "recursos_vaat", "nse", "nf", "inabilitados_vaat",
    "peso_vaar", "recursos_vaaf_final", "vaaf_final", "vaat_pre",
    "recursos_vaat_final", "vaat_final", "complemento_vaaf",
    "complemento_vaat", "complemento_vaar", "complemento_uniao",
    "recursos_fundeb"
  )]
  
  return(df_entes)
}
