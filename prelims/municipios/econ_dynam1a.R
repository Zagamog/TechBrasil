# econ_dynam1a.R

# Economic Dynamism Index Calculator for Brazilian Municipalities
# Based on PIB per capita year-over-year growth rates
# Two periods: 2002-2011 (weight=1.0) and 2012-2021 (weight=1.5)

library(dplyr)
library(data.table)
library(scales)

# Load your data
# load("D:/Country/Brazil/TechBrazil/working/ibge/df_pibmunis.rda")
# 
# If loaded as 'df_pib', rename it:
# if (exists("df_pib")) {
#   df_pibmunis <- df_pib
#   rm(df_pib)
# }

# Parameters (adjustable)
PERIOD1_WEIGHT <- 1.0    # Weight for 2002-2011 period
PERIOD2_WEIGHT <- 1.5    # Weight for 2012-2021 period
PERIOD1_YEARS <- 2002:2011
PERIOD2_YEARS <- 2012:2021

# Function to calculate economic dynamism index
calculate_economic_dynamism <- function(df, 
                                        period1_weight = PERIOD1_WEIGHT,
                                        period2_weight = PERIOD2_WEIGHT,
                                        period1_years = PERIOD1_YEARS,
                                        period2_years = PERIOD2_YEARS) {
  
  # Get column names (using actual column names from your dataframe)
  ano_col <- "Ano"
  nome_municipio_col <- "Nome.do.Município"
  codigo_municipio_col <- "Código.do.Município"
  nome_uf_col <- "Nome.da.Unidade.da.Federação"
  total_pib_col <- "Produto.Interno.Bruto...a.preços.correntes..R..1.000."
  pib_per_capita_col <- "Produto.Interno.Bruto.per.capita...a.preços.correntes..R..1.00."
  
  # Calculate inferred population for weighting
  df_with_pop <- df %>%
    mutate(
      Inferred_Population = ifelse(
        is.na(!!sym(pib_per_capita_col)) | !!sym(pib_per_capita_col) == 0,
        NA_real_,
        (!!sym(total_pib_col) * 1000) / !!sym(pib_per_capita_col)
      )
    ) %>%
    filter(!is.na(!!sym(pib_per_capita_col)) & 
             !is.na(Inferred_Population) & 
             Inferred_Population > 0 &
             !!sym(pib_per_capita_col) > 0)
  
  # Calculate year-over-year growth rates
  growth_data <- df_with_pop %>%
    arrange(!!sym(codigo_municipio_col), !!sym(ano_col)) %>%
    group_by(!!sym(codigo_municipio_col)) %>%
    mutate(
      pib_pc_growth_rate = (!!sym(pib_per_capita_col) / lag(!!sym(pib_per_capita_col)) - 1) * 100
    ) %>%
    filter(!is.na(pib_pc_growth_rate)) %>%
    ungroup()
  
  # Separate into two periods and calculate weighted averages
  period1_data <- growth_data %>%
    filter(!!sym(ano_col) %in% period1_years) %>%
    group_by(!!sym(codigo_municipio_col)) %>%
    summarise(
      municipality_name = first(!!sym(nome_municipio_col)),
      uf_name = first(!!sym(nome_uf_col)),
      avg_population_p1 = mean(Inferred_Population, na.rm = TRUE),
      period1_avg_growth = mean(pib_pc_growth_rate, na.rm = TRUE),
      period1_years_available = n(),
      .groups = 'drop'
    ) %>%
    filter(period1_years_available >= 3)  # Require at least 3 years of data
  
  period2_data <- growth_data %>%
    filter(!!sym(ano_col) %in% period2_years) %>%
    group_by(!!sym(codigo_municipio_col)) %>%
    summarise(
      avg_population_p2 = mean(Inferred_Population, na.rm = TRUE),
      period2_avg_growth = mean(pib_pc_growth_rate, na.rm = TRUE),
      period2_years_available = n(),
      .groups = 'drop'
    ) %>%
    filter(period2_years_available >= 3)  # Require at least 3 years of data
  
  # Combine periods and calculate dynamism index
  combined_data <- period1_data %>%
    inner_join(period2_data, by = codigo_municipio_col) %>%
    mutate(
      # Use average population across both periods for weighting
      avg_population = (avg_population_p1 + avg_population_p2) / 2,
      
      # Calculate weighted dynamism index
      dynamism_index = (period1_avg_growth * period1_weight + 
                          period2_avg_growth * period2_weight) / 
        (period1_weight + period2_weight),
      
      # Population-weighted contribution to overall index
      pop_weighted_contribution = dynamism_index * avg_population
    ) %>%
    filter(!is.na(dynamism_index) & !is.infinite(dynamism_index))
  
  # Calculate deciles
  combined_data <- combined_data %>%
    arrange(dynamism_index) %>%
    mutate(
      dynamism_decile = ntile(dynamism_index, 10),
      dynamism_percentile = percent_rank(dynamism_index) * 100
    ) %>%
    arrange(desc(dynamism_index))
  
  # Summary statistics
  summary_stats <- list(
    total_municipalities = nrow(combined_data),
    period1_weight = period1_weight,
    period2_weight = period2_weight,
    avg_dynamism_index = mean(combined_data$dynamism_index, na.rm = TRUE),
    median_dynamism_index = median(combined_data$dynamism_index, na.rm = TRUE),
    sd_dynamism_index = sd(combined_data$dynamism_index, na.rm = TRUE),
    min_dynamism_index = min(combined_data$dynamism_index, na.rm = TRUE),
    max_dynamism_index = max(combined_data$dynamism_index, na.rm = TRUE)
  )
  
  # Calculate population-weighted national average
  national_weighted_avg <- sum(combined_data$pop_weighted_contribution, na.rm = TRUE) / 
    sum(combined_data$avg_population, na.rm = TRUE)
  summary_stats$pop_weighted_national_avg <- national_weighted_avg
  
  return(list(
    data = combined_data,
    summary = summary_stats
  ))
}

# Function to display top and bottom municipalities by decile
display_decile_summary <- function(results_data, decile_num) {
  decile_data <- results_data %>%
    filter(dynamism_decile == decile_num) %>%
    arrange(desc(dynamism_index))
  
  cat(sprintf("\n=== DECILE %d SUMMARY ===\n", decile_num))
  cat(sprintf("Number of municipalities: %d\n", nrow(decile_data)))
  cat(sprintf("Average dynamism index: %.2f\n", mean(decile_data$dynamism_index)))
  cat(sprintf("Range: %.2f to %.2f\n", 
              min(decile_data$dynamism_index), 
              max(decile_data$dynamism_index)))
  
  cat("\nTop 5 municipalities in this decile:\n")
  top_5 <- head(decile_data, 5)
  for(i in 1:nrow(top_5)) {
    cat(sprintf("%d. %s, %s - Index: %.2f (P1: %.2f%%, P2: %.2f%%)\n",
                i, 
                top_5$municipality_name[i], 
                top_5$uf_name[i],
                top_5$dynamism_index[i],
                top_5$period1_avg_growth[i],
                top_5$period2_avg_growth[i]))
  }
}

# Function to analyze results by state
analyze_by_state <- function(results_data) {
  state_summary <- results_data %>%
    group_by(uf_name) %>%
    summarise(
      municipalities_count = n(),
      avg_dynamism = mean(dynamism_index, na.rm = TRUE),
      median_dynamism = median(dynamism_index, na.rm = TRUE),
      top_decile_count = sum(dynamism_decile == 10),
      bottom_decile_count = sum(dynamism_decile == 1),
      avg_population = mean(avg_population, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(avg_dynamism))
  
  return(state_summary)
}

# Main execution function
run_dynamism_analysis <- function(df_pibmunis) {
  
  cat("=== ECONOMIC DYNAMISM INDEX ANALYSIS ===\n")
  cat("Analyzing PIB per capita growth patterns across Brazilian municipalities\n")
  cat(sprintf("Period 1 (2002-2011): Weight = %.1f\n", PERIOD1_WEIGHT))
  cat(sprintf("Period 2 (2012-2021): Weight = %.1f\n", PERIOD2_WEIGHT))
  cat("\nCalculating...\n")
  
  # Calculate the dynamism index
  results <- calculate_economic_dynamism(df_pibmunis)
  
  # Display summary statistics
  cat("\n=== SUMMARY STATISTICS ===\n")
  cat(sprintf("Total municipalities analyzed: %d\n", results$summary$total_municipalities))
  cat(sprintf("Average dynamism index: %.2f\n", results$summary$avg_dynamism_index))
  cat(sprintf("Median dynamism index: %.2f\n", results$summary$median_dynamism_index))
  cat(sprintf("Standard deviation: %.2f\n", results$summary$sd_dynamism_index))
  cat(sprintf("Range: %.2f to %.2f\n", 
              results$summary$min_dynamism_index, 
              results$summary$max_dynamism_index))
  cat(sprintf("Population-weighted national average: %.2f\n", 
              results$summary$pop_weighted_national_avg))
  
  # Display top and bottom deciles
  display_decile_summary(results$data, 10)  # Top decile
  display_decile_summary(results$data, 1)   # Bottom decile
  
  # State analysis
  cat("\n=== TOP 10 STATES BY AVERAGE DYNAMISM ===\n")
  state_analysis <- analyze_by_state(results$data)
  top_states <- head(state_analysis, 10)
  for(i in 1:nrow(top_states)) {
    cat(sprintf("%d. %s: %.2f (municipalities: %d, top decile: %d)\n",
                i,
                top_states$uf_name[i],
                top_states$avg_dynamism[i],
                top_states$municipalities_count[i],
                top_states$top_decile_count[i]))
  }
  
  return(list(
    results = results,
    state_analysis = state_analysis
  ))
}

# Example usage 
# 

# Check the data structure
cat("Data dimensions:", dim(df_pibmunis), "\n")
cat("Year range:", range(df_pibmunis$Ano, na.rm = TRUE), "\n")
cat("Sample of municipalities:", head(unique(df_pibmunis$Nome.do.Município), 5), "\n")

# Run the analysis
analysis_results <- run_dynamism_analysis(df_pibmunis)

# Access the results
dynamism_data <- analysis_results$results$data
dynamism_data <- dynamism_data %>% arrange(desc(dynamism_index))


state_summary <- analysis_results$state_analysis

# View top 20 most dynamic municipalities
head(dynamism_data, 20)

# Export results if needed
write.csv(dynamism_data, "municipality_dynamism_index.csv", row.names = FALSE)
write.csv(state_summary, "state_dynamism_summary.csv", row.names = FALSE)

cat("Economic Dynamism Index Calculator loaded successfully!\n")
cat("Run: analysis_results <- run_dynamism_analysis(df_pibmunis)\n")