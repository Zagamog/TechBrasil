# econ_dynam1b.R
# Economic Escalator Index - Core Calculation
# Focus: Consistency × Growth with two-period weighting
# Output: Municipality-level escalator index + estimated population

library(dplyr)
library(data.table)
library(scales)

# Parameters (adjustable)
PERIOD1_WEIGHT <- 1.0    # Weight for 2002-2011 period
PERIOD2_WEIGHT <- 1.5    # Weight for 2012-2021 period
PERIOD1_YEARS <- 2002:2011
PERIOD2_YEARS <- 2012:2021
MIN_YEARS_REQUIRED <- 3  # Minimum years of data required per period

# Column name definitions
setup_column_names <- function() {
  list(
    ano = "Ano",
    codigo_municipio = "Código.do.Município", 
    nome_municipio = "Nome.do.Município",
    nome_uf = "Nome.da.Unidade.da.Federação",
    nome_micro = "Nome.da.Microrregião",
    nome_intermediaria = "Nome.da.Região.Geográfica.Intermediária",
    total_pib = "Produto.Interno.Bruto...a.preços.correntes..R..1.000.",
    pib_per_capita = "Produto.Interno.Bruto.per.capita...a.preços.correntes..R..1.00."
  )
}

# Core function: Calculate Economic Escalator Index
calculate_escalator_index <- function(df, 
                                    period1_weight = PERIOD1_WEIGHT,
                                    period2_weight = PERIOD2_WEIGHT,
                                    period1_years = PERIOD1_YEARS,
                                    period2_years = PERIOD2_YEARS) {
  
  cols <- setup_column_names()
  
  # Step 1: Calculate estimated population and prepare data
  df_prep <- df %>%
    mutate(
      Estimated_Population = ifelse(
        is.na(!!sym(cols$pib_per_capita)) | !!sym(cols$pib_per_capita) == 0,
        NA_real_,
        (!!sym(cols$total_pib) * 1000) / !!sym(cols$pib_per_capita)
      )
    ) %>%
    filter(!is.na(!!sym(cols$pib_per_capita)) & 
           !is.na(Estimated_Population) & 
           Estimated_Population > 0 &
           !!sym(cols$pib_per_capita) > 0) %>%
    arrange(!!sym(cols$codigo_municipio), !!sym(cols$ano))
  
  # Step 2: Calculate year-over-year growth rates
  growth_data <- df_prep %>%
    group_by(!!sym(cols$codigo_municipio)) %>%
    mutate(
      pib_pc_growth_rate = (!!sym(cols$pib_per_capita) / lag(!!sym(cols$pib_per_capita)) - 1) * 100
    ) %>%
    filter(!is.na(pib_pc_growth_rate)) %>%
    ungroup()
  
  # Step 3: Period 1 Analysis (2002-2011)
  period1_data <- growth_data %>%
    filter(!!sym(cols$ano) %in% period1_years) %>%
    group_by(!!sym(cols$codigo_municipio)) %>%
    summarise(
      municipality_name = first(!!sym(cols$nome_municipio)),
      uf_name = first(!!sym(cols$nome_uf)),
      micro_name = first(!!sym(cols$nome_micro)),
      intermediaria_name = first(!!sym(cols$nome_intermediaria)),
      estimated_population_p1 = mean(Estimated_Population, na.rm = TRUE),
      
      # Escalator metrics for period 1
      years_with_data_p1 = n(),
      years_positive_growth_p1 = sum(pib_pc_growth_rate > 0, na.rm = TRUE),
      consistency_p1 = years_positive_growth_p1 / years_with_data_p1, # % of years with positive growth
      avg_positive_growth_p1 = mean(pib_pc_growth_rate[pib_pc_growth_rate > 0], na.rm = TRUE),
      
      # Escalator score = consistency × average positive growth
      escalator_score_p1 = consistency_p1 * ifelse(is.na(avg_positive_growth_p1), 0, avg_positive_growth_p1),
      
      .groups = 'drop'
    ) %>%
    filter(years_with_data_p1 >= MIN_YEARS_REQUIRED)
  
  # Step 4: Period 2 Analysis (2012-2021)
  period2_data <- growth_data %>%
    filter(!!sym(cols$ano) %in% period2_years) %>%
    group_by(!!sym(cols$codigo_municipio)) %>%
    summarise(
      estimated_population_p2 = mean(Estimated_Population, na.rm = TRUE),
      
      # Escalator metrics for period 2
      years_with_data_p2 = n(),
      years_positive_growth_p2 = sum(pib_pc_growth_rate > 0, na.rm = TRUE),
      consistency_p2 = years_positive_growth_p2 / years_with_data_p2,
      avg_positive_growth_p2 = mean(pib_pc_growth_rate[pib_pc_growth_rate > 0], na.rm = TRUE),
      
      # Escalator score = consistency × average positive growth
      escalator_score_p2 = consistency_p2 * ifelse(is.na(avg_positive_growth_p2), 0, avg_positive_growth_p2),
      
      .groups = 'drop'
    ) %>%
    filter(years_with_data_p2 >= MIN_YEARS_REQUIRED)
  
  # Step 5: Combine periods and calculate final escalator index
  results <- period1_data %>%
    inner_join(period2_data, by = cols$codigo_municipio) %>%
    mutate(
      # Average estimated population across both periods
      estimated_population = (estimated_population_p1 + estimated_population_p2) / 2,
      
      # Economic Escalator Index: Weighted combination of escalator scores
      escalator_index = (escalator_score_p1 * period1_weight + 
                        escalator_score_p2 * period2_weight) / 
                       (period1_weight + period2_weight),
      
      # Calculate momentum (is the escalator speeding up or slowing down?)
      momentum = escalator_score_p2 - escalator_score_p1,
      
      # Overall consistency across both periods
      overall_consistency = (years_positive_growth_p1 + years_positive_growth_p2) / 
                           (years_with_data_p1 + years_with_data_p2)
    ) %>%
    filter(!is.na(escalator_index) & !is.infinite(escalator_index))
  
  # Step 6: Calculate deciles and percentiles
  results <- results %>%
    arrange(escalator_index) %>%
    mutate(
      escalator_decile = ntile(escalator_index, 10),
      escalator_percentile = percent_rank(escalator_index) * 100
    ) %>%
    arrange(desc(escalator_index))
  
  # Step 7: Select final columns for output
  final_results <- results %>%
    select(
      !!sym(cols$codigo_municipio),
      municipality_name,
      uf_name,
      micro_name,
      intermediaria_name,
      estimated_population,
      escalator_index,
      escalator_decile,
      escalator_percentile,
      momentum,
      overall_consistency,
      consistency_p1,
      consistency_p2,
      escalator_score_p1,
      escalator_score_p2
    )
  
  return(final_results)
}

# Summary function
display_escalator_summary <- function(escalator_data) {
  
  cat("=== ECONOMIC ESCALATOR INDEX SUMMARY ===\n\n")
  
  cat("Overall Statistics:\n")
  cat(sprintf("Total municipalities: %d\n", nrow(escalator_data)))
  cat(sprintf("Average escalator index: %.2f\n", mean(escalator_data$escalator_index, na.rm = TRUE)))
  cat(sprintf("Median escalator index: %.2f\n", median(escalator_data$escalator_index, na.rm = TRUE)))
  cat(sprintf("Standard deviation: %.2f\n", sd(escalator_data$escalator_index, na.rm = TRUE)))
  cat(sprintf("Index range: %.2f to %.2f\n\n", 
              min(escalator_data$escalator_index, na.rm = TRUE),
              max(escalator_data$escalator_index, na.rm = TRUE)))
  
  cat("TOP 10 MUNICIPALITIES (Highest Escalator Index):\n")
  top_10 <- head(escalator_data, 10)
  for(i in 1:nrow(top_10)) {
    cat(sprintf("%2d. %-25s %-15s Index: %6.2f Pop: %s\n",
                i, 
                top_10$municipality_name[i],
                top_10$uf_name[i],
                top_10$escalator_index[i],
                comma(round(top_10$estimated_population[i]))))
  }
  
  cat("\nBOTTOM 10 MUNICIPALITIES (Lowest Escalator Index):\n")
  bottom_10 <- tail(escalator_data, 10)
  for(i in nrow(bottom_10):1) {
    cat(sprintf("%2d. %-25s %-15s Index: %6.2f Pop: %s\n",
                nrow(escalator_data) - i + 1,
                bottom_10$municipality_name[i],
                bottom_10$uf_name[i], 
                bottom_10$escalator_index[i],
                comma(round(bottom_10$estimated_population[i]))))
  }
  
  cat("\nEscalator Index by Decile:\n")
  decile_summary <- escalator_data %>%
    group_by(escalator_decile) %>%
    summarise(
      municipalities = n(),
      avg_index = mean(escalator_index),
      avg_population = mean(estimated_population),
      .groups = 'drop'
    )
  
  for(i in 1:nrow(decile_summary)) {
    cat(sprintf("Decile %2d: %4d municipalities, avg index: %6.2f, avg pop: %s\n",
                decile_summary$escalator_decile[i],
                decile_summary$municipalities[i],
                decile_summary$avg_index[i],
                comma(round(decile_summary$avg_population[i]))))
  }
}

# Main execution function
run_escalator_analysis <- function(df_pibmunis) {
  
  cat("Calculating Economic Escalator Index...\n")
  cat(sprintf("Period 1 (2002-2011): Weight = %.1f\n", PERIOD1_WEIGHT))
  cat(sprintf("Period 2 (2012-2021): Weight = %.1f\n", PERIOD2_WEIGHT))
  cat("Formula: Escalator Index = Weighted Average of (Consistency × Positive Growth)\n\n")
  
  # Calculate the escalator index
  results <- calculate_escalator_index(df_pibmunis)
  
  # Display summary
  display_escalator_summary(results)
  
  return(results)
}

# Example usage:
cat("Economic Escalator Index Calculator loaded!\n\n")
cat("To run the analysis:\n")
cat("escalator_results <- run_escalator_analysis(df_pibmunis)\n\n")
cat("To export results:\n")
cat("write.csv(escalator_results, 'municipal_escalator_index.csv', row.names = FALSE)\n\n")
cat("The results include:\n")
cat("- municipality_name, uf_name, micro_name, intermediaria_name\n")
cat("- estimated_population (calculated from PIB total / PIB per capita)\n")
cat("- escalator_index (weighted consistency × growth)\n")
cat("- escalator_decile and escalator_percentile\n")
cat("- momentum (period 2 vs period 1 improvement)\n")
cat("- consistency metrics for both periods\n")