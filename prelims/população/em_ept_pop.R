# em_ept_pop.R
# Extract enrollment and population data for states
# Columns: UF, Pop_2024, Pop_2035, EM_2024, EPT_2024, Pct_EPT_2024

library(dplyr)
library(tidyr)

# Load the data (adjust paths as needed)
load("meta11a_opcoes.rda")
load("pop01_70b.rda")

cat("Extracting EM/EPT enrollment and population data by state...\n")

# Step 1: Prepare population data (15-19 age group)
pop_data <- pop01_70b %>%
  select(ANO, SIGLA, `15-17_T`, `18-21_T`) %>%
  mutate(`15-19_T` = `15-17_T` + (`18-21_T` * 0.5)) %>%
  filter(ANO %in% c(2024, 2035)) %>%
  select(ANO, SIGLA, `15-19_T`) %>%
  pivot_wider(names_from = ANO, values_from = `15-19_T`, 
              names_prefix = "Pop_", values_fill = NA)

# Step 2: Prepare enrollment data - states and Brasil only
enrollment_base <- meta11a_opcoes %>%
  filter(ANO == 2024) %>%
  group_by(SG_UF,NM_UF) %>%
  summarise(
    EM_2024 = sum(QT_MAT_MED, na.rm = TRUE),
    EPT_2024 = sum(QT_MAT_PROF_TEC_PROPAG, na.rm = TRUE),
    .groups = 'drop'
  )

# Separate states from Brasil
enrollment_states <- enrollment_base %>% filter(SG_UF != "BR")
enrollment_brasil <- enrollment_base %>% filter(SG_UF == "BR")

# Step 3: Build all regional aggregates from states
enrollment_regional <- bind_rows(
  # Norte region
  enrollment_states %>%
    filter(SG_UF %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO")) %>%
    summarise(SG_UF = "NO", NM_UF = "Norte", 
              EM_2024 = sum(EM_2024, na.rm = TRUE), 
              EPT_2024 = sum(EPT_2024, na.rm = TRUE)),
  
  # Nordeste region  
  enrollment_states %>%
    filter(SG_UF %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")) %>%
    summarise(SG_UF = "ND", NM_UF = "Nordeste", 
              EM_2024 = sum(EM_2024, na.rm = TRUE), 
              EPT_2024 = sum(EPT_2024, na.rm = TRUE)),
  
  # Sudeste region
  enrollment_states %>%
    filter(SG_UF %in% c("ES", "MG", "RJ", "SP")) %>%
    summarise(SG_UF = "SD", NM_UF = "Sudeste", 
              EM_2024 = sum(EM_2024, na.rm = TRUE), 
              EPT_2024 = sum(EPT_2024, na.rm = TRUE)),
  
  # Sul region
  enrollment_states %>%
    filter(SG_UF %in% c("PR", "RS", "SC")) %>%
    summarise(SG_UF = "SU", NM_UF = "Sul", 
              EM_2024 = sum(EM_2024, na.rm = TRUE), 
              EPT_2024 = sum(EPT_2024, na.rm = TRUE)),
  
  # Centro-Oeste region
  enrollment_states %>%
    filter(SG_UF %in% c("DF", "GO", "MS", "MT")) %>%
    summarise(SG_UF = "CO", NM_UF = "Centro-Oeste", 
              EM_2024 = sum(EM_2024, na.rm = TRUE), 
              EPT_2024 = sum(EPT_2024, na.rm = TRUE)),
  
  # Amazonia Legal
  enrollment_states %>%
    filter(SG_UF %in% c("AC", "AP", "AM", "MA", "MT", "PA", "RO", "RR", "TO")) %>%
    summarise(SG_UF = "AML", NM_UF = "Amazonia_Legal", 
              EM_2024 = sum(EM_2024, na.rm = TRUE), 
              EPT_2024 = sum(EPT_2024, na.rm = TRUE)),
  
  # Nordeste_r
  enrollment_states %>%
    filter(SG_UF %in% c("AL", "BA", "CE", "PB", "PE", "PI", "RN", "SE")) %>%
    summarise(SG_UF = "ND_", NM_UF = "Nordeste_r", 
              EM_2024 = sum(EM_2024, na.rm = TRUE), 
              EPT_2024 = sum(EPT_2024, na.rm = TRUE)),
  
  # Centro-Oeste_r
  enrollment_states %>%
    filter(SG_UF %in% c("DF", "GO", "MS")) %>%
    summarise(SG_UF = "CO_", NM_UF = "Centro-Oeste_r", 
              EM_2024 = sum(EM_2024, na.rm = TRUE), 
              EPT_2024 = sum(EPT_2024, na.rm = TRUE))
)

# Step 4: Combine all enrollment data
enrollment_data <- bind_rows(enrollment_states, enrollment_brasil, enrollment_regional)

# Step 5: Combine datasets
combined_data <- pop_data %>%
  left_join(enrollment_data, by = c("SIGLA" = "SG_UF")) %>%
  # Use NM_UF from enrollment data, fallback to LOCAL if missing
  mutate(
    UF_Name = ifelse(is.na(NM_UF), LOCAL, NM_UF),
    # Calculate percentage EPT of 15-19 population
    Pct_EPT_2024 = ifelse(!is.na(Pop_2024) & Pop_2024 > 0 & !is.na(EPT_2024), 
                          (EPT_2024 / Pop_2024) * 100, 0)
  ) %>%
  # Select and arrange final columns
  select(
    UF = SIGLA,
    UF_Name,
    Pop_2024,
    Pop_2035, 
    EM_2024,
    EPT_2024,
    Pct_EPT_2024
  ) %>%
  # Sort alphabetically by UF (same as app)
  arrange(UF) %>%
  # Format numbers for LaTeX output
  mutate(
    Pop_2024 = format(round(Pop_2024), big.mark = ".", decimal.mark = ","),
    Pop_2035 = format(round(Pop_2035), big.mark = ".", decimal.mark = ","),
    EM_2024 = format(round(EM_2024), big.mark = ".", decimal.mark = ","),
    EPT_2024 = format(round(EPT_2024), big.mark = ".", decimal.mark = ","),
    Pct_EPT_2024 = format(round(Pct_EPT_2024, 2), nsmall = 2, decimal.mark = ",")
  )

# Display the final table
cat("=== COMPLETE STATE TABLE ===\n")
print(combined_data, n = Inf)

cat("\nAnalysis complete!\n")