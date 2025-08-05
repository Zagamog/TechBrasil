#### DEBUG: Inspect column names of key dataframes ####
observe({
  cat("[DEBUG] cnct_qbq_matches columns:", paste(names(cnct_qbq_matches), collapse=", "), "
")
  cat("[DEBUG] qbq_ocup_cmento1 columns:", paste(names(qbq_ocup_cmento1), collapse=", "), "
")
  cat("[DEBUG] df_exarcu columns:", paste(names(df_exarcu), collapse=", "), "
")
  cat("[DEBUG] df_raisCodCBO_wide columns:", paste(names(df_raisCodCBO_wide), collapse=", "), "
")
})