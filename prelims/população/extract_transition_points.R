# extract_transition_points.R
# Simple extraction of demographic transition points

library(dplyr)

# Load the demographic data
load("pop01_70b.rda")

# Create state abbreviation mapping
state_abbrev <- data.frame(
  LOCAL = c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", 
            "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", 
            "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", 
            "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro", 
            "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", 
            "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"),
  UF = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
         "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
         "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
  stringsAsFactors = FALSE
)

# Extract transition points ordered by year
cat("DEMOGRAPHIC TRANSITION POINTS - ORDERED BY YEAR\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Filter for states only and get crossover points
transition_points <- pop01_70b %>%
  filter(LOCAL %in% state_abbrev$LOCAL, 
         Crossover_Flag == 1) %>%
  left_join(state_abbrev, by = "LOCAL") %>%
  select(ANO, UF, LOCAL) %>%
  arrange(ANO, UF)

# Display the results
if(nrow(transition_points) > 0) {
  for(i in 1:nrow(transition_points)) {
    cat(sprintf("%d: %s (%s)\n", 
                transition_points$ANO[i], 
                transition_points$UF[i], 
                transition_points$LOCAL[i]))
  }
} else {
  cat("No transition points found with Crossover_Flag == 1\n")
}

# Also show states without transition points in the 2000-2070 range
all_states <- state_abbrev$LOCAL
states_with_transition <- transition_points$LOCAL
states_without_transition <- setdiff(all_states, states_with_transition)

if(length(states_without_transition) > 0) {
  cat("\nStates without transition point in 2000-2070:\n")
  for(state in states_without_transition) {
    abbrev <- state_abbrev$UF[state_abbrev$LOCAL == state]
    cat(sprintf("%s (%s)\n", abbrev, state))
  }
}