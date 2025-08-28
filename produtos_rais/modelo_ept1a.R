# modelo_ept1a.R
# EPT Time Series Model - Complete Data Loading and Preparation
# Step 1: Load, clean, and construct panel dataframe for EPT enrollment regression

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(data.table)

# Set working directory path
base_path <- "D:/Country/Brazil/TechBrazil/working"

cat("=== EPT TIME SERIES MODEL - DATA PREPARATION ===\n")

#############################################################################
# STEP 1A: LOAD RAW DATASETS
#############################################################################

cat("Loading datasets...\n")

# Load enrollment data (course level from your RStudio session)
# Assuming you have Tecnico_Forma_Cursos_Garabed1 loaded
# If not loaded, uncomment next line:
# load(file.path(base_path, "mec_inep/Tecnico_Forma_Cursos_Garabed1.rda"))

# Load course classification data
load(file.path(base_path, "mec_inep/df_exarcu.rda"))

# Load PIB data from source .rda
load(file.path(base_path, "ibge/df_pibmunis.rda"))

load(file.path(base_path, "mec_inep/Tecnico_Forma_Cursos_Garabed1.rda"))

cat("Raw data loaded successfully.\n")

#############################################################################
# STEP 1B: COURSE NAME CLEANING AND MAPPING
#############################################################################

cat("Cleaning course names and creating sector mapping...\n")

# Clean course names in enrollment data (remove "Técnico em " prefix)
enrollment_clean <- Tecnico_Forma_Cursos_Garabed1 %>%
  mutate(
    nome_curso_clean = str_remove(NOME_CURSO, "^Técnico em\\s+"),
    nome_curso_clean = str_trim(tolower(nome_curso_clean))
  )

# Clean course names in classification data  
classification_clean <- df_exarcu %>%
  mutate(
    denominacao_clean = str_remove(`Denominação do Curso`, "^Técnico em\\s+"),
    denominacao_clean = str_trim(tolower(denominacao_clean))
  )

# Join to get Área Tecnológica for each course
enrollment_with_area <- enrollment_clean %>%
  left_join(classification_clean, 
            by = c("nome_curso_clean" = "denominacao_clean")) %>%
  filter(!is.na(`Área Tecnológica`))  # Keep only matched courses

#############################################################################
# STEP 1C: ÁREA TECNOLÓGICA → ECONOMIC SECTOR MAPPING
#############################################################################

# Define mapping from Área Tecnológica to 4 economic sectors (using actual data values)



# Primary mapping using Área Tecnológica (for specific areas)

area_to_sector_mapping <- tribble(
  ~area_tecnologica, ~economic_sector,
  
  # AGRICULTURE (matches agro_va from PIB data)
  "Pesca e Aquicultura", "agriculture",
  "Produção Agrícola e Pecuária", "agriculture", 
  "Silvicultura", "agriculture",
  
  # INDUSTRY (matches industry_va from PIB data)
  "Construção de Obras", "industry",
  "Eletrônica e Automação", "industry",
  "Manufatura", "industry",
  "Materiais", "industry",
  "Metalmecânica", "industry",
  "Mineração e Extração", "industry",
  "Química", "industry",
  "Sistemas de Energia", "industry",
  "Tecnologia, Inovação e Práticas Laboratoriais", "industry",
  
  # SERVICES (matches services_va from PIB data)  
  "Acolhimento e Hospedagem", "services",
  "Atividades Turísticas", "services",
  "Comercial", "services",
  "Comunicação Midiática", "services",
  "Design", "services",
  "Gerencial", "services",
  "Gestão e Promoção da Saúde e Bem-Estar", "services",
  "Infraestrutura de Informação e Comunicação", "services",
  "Manifestações Artísticas", "services",
  "Manutenção e Operação", "services",
  "Mensuração Espacial e Volumétrica", "services",
  "Operações de Transporte", "services",
  "Operações Financeiras", "services",
  "Têxtil e Vestuário", "services",
  
  # ADMINISTRATION (matches admin_va from PIB data)
  "Apoio técnico a eventos", "administration",
  "Desenvolvimento de Sistemas", "administration",
  "Gestão Educacional", "administration",
  "Intervenção Social", "administration",
  "Proteção e Reabilitação de Ecossistemas", "administration",
  "Recreação e Sociabilidade", "administration"
)

# Fallback mapping using Eixo Tecnológico (for broad categories)
eixo_to_sector_mapping <- tribble(
  ~eixo_tecnologico, ~economic_sector,
  "Ambiente e Saúde", "services",
  "Recursos Naturais", "agriculture", 
  "Produção Cultural e Design", "services",
  "Controle e Processos Industriais", "industry",
  "Gestão e Negócios", "services",
  "Desenvolvimento Educacional e Social", "administration",
  "Produção Industrial", "industry",
  "Turismo, Hospitalidade e Lazer", "services",
  "Informação e Comunicação", "services",
  "Produção Alimentícia", "agriculture",
  "Infraestrutura", "industry",
  "Segurança", "administration"
)

# Apply mapping with fallback logic
enrollment_with_sectors <- enrollment_with_area %>%
  left_join(area_to_sector_mapping, by = c("Área Tecnológica" = "area_tecnologica")) %>%
  left_join(eixo_to_sector_mapping, by = c("Eixo Tecnológico" = "eixo_tecnologico")) %>%
  mutate(economic_sector = coalesce(economic_sector.x, economic_sector.y)) %>%
  select(-economic_sector.x, -economic_sector.y)
#############################################################################
# STEP 1D: AGGREGATE TO PANEL STRUCTURE
#############################################################################

cat("Creating panel structure: area × state × dependency × year...\n")

# Aggregate to area-state-dependency-year level  
panel_enrollment <- enrollment_with_sectors %>%
  group_by(ANO, SG_UF, TP_DEPENDENCIA, economic_sector) %>%
  summarise(
    QT_MAT_CURSO_TEC = sum(QT_MAT_CURSO_TEC, na.rm = TRUE),
    n_courses = n_distinct(`Área Tecnológica`),
    .groups = "drop"
  ) 

#############################################################################
# STEP 1E: PREPARE PIB DATA (STATE-YEAR LEVEL)
#############################################################################

cat("Preparing PIB and sectoral data from raw df_pibmunis...\n")

# First clean df_pibmunis using column positions (following export_shiny_data.R pattern)

pib_data <- df_pibmunis %>%
  mutate(
    population = ifelse(
      is.na(df_pibmunis[[40]]) | df_pibmunis[[40]] == 0,
      NA_real_,
      (df_pibmunis[[39]] * 1000) / df_pibmunis[[40]]
    )
  ) %>%
  select(
    year = 1, SG_UF= 5, CO_MUN = 7, NM_MUN = 8, NM_UF = 6,
    pib_total = 39, pib_per_capita = 40, population,
    agro_va = 33, industry_va = 34, services_va = 35, admin_va = 36, total_va = 37,
    main_activity = 41, second_activity = 42, third_activity = 43
  )



pib_state_year <- pib_data %>%
  group_by(SG_UF, NM_UF, year) %>%
  summarise(
    pib_total = sum(pib_total, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    agro_va = sum(agro_va, na.rm = TRUE),
    industry_va = sum(industry_va, na.rm = TRUE),
    services_va = sum(services_va, na.rm = TRUE), 
    admin_va = sum(admin_va, na.rm = TRUE),
    total_va = sum(total_va, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Calculate PIB per capita
    pib_per_capita = pib_total / population,
    
    # Calculate sectoral shares
    agro_share = agro_va / total_va,
    industry_share = industry_va / total_va,
    services_share = services_va / total_va,
    admin_share = admin_va / total_va
  ) %>%
  arrange(SG_UF, year) %>%
  group_by(SG_UF) %>%
  mutate(
    # Calculate PIB per capita growth rates
    pib_pc_growth = (pib_per_capita / lag(pib_per_capita, 1) - 1) * 100,
    pib_pc_growth_lag1 = lag(pib_pc_growth, 1)
  ) %>%
  ungroup()

#############################################################################
# STEP 1F: CREATE SECTOR ALIGNMENT VARIABLE  
#############################################################################

# Calculate enrollment shares by state-year
enrollment_shares <- panel_enrollment %>%
  group_by(ANO, SG_UF) %>%
  mutate(
    total_ept_state = sum(QT_MAT_CURSO_TEC, na.rm = TRUE),
    QT_MAT_CURSO_TEC_share = QT_MAT_CURSO_TEC / total_ept_state
  ) %>%
  ungroup()

# Prepare economic shares in long format from the aggregated pib_state_year
economic_shares_long <- pib_state_year %>%
  select(ANO = year, SG_UF, NM_UF, 
         agriculture = agro_share, 
         industry = industry_share, 
         services = services_share, 
         administration = admin_share) %>%
  pivot_longer(cols = c(agriculture, industry, services, administration),
               names_to = "economic_sector", 
               values_to = "econ_sect_va_prop")
cat("STEP 1F - Sector alignment preparation:\n")
cat("Enrollment shares dimensions:", dim(enrollment_shares), "\n")
cat("Economic shares long dimensions:", dim(economic_shares_long), "\n")
cat("Enrollment share range:", round(range(enrollment_shares$QT_MAT_CURSO_TEC_share, na.rm = TRUE), 3), "\n")
cat("Economic share range:", round(range(economic_shares_long$econ_sect_va_prop, na.rm = TRUE), 3), "\n\n")


#############################################################################
# STEP 1G: MERGE ALL DATA AND CREATE MODEL DATAFRAME
#############################################################################
# Merge enrollment with economic data
model_df_base <- enrollment_shares %>%
  left_join(economic_shares_long, 
            by = c("ANO", "SG_UF", "economic_sector")) %>%
  
  # Add PIB variables 
  left_join(pib_state_year %>% select(ANO = year, SG_UF, NM_UF, pib_pc_growth, pib_pc_growth_lag1, pib_per_capita),
            by = c("ANO", "SG_UF", "NM_UF")) %>%
  
  # Create sector alignment variable using log ratio
  # Log ratio handles division by zero and creates symmetric measure around 0:
  # - sector_alignment = 0: perfect alignment (enrollment share = economic share)  
  # - sector_alignment > 0: EPT over-represented relative to economic structure
  # - sector_alignment < 0: EPT under-represented relative to economic structure
  mutate(
    sector_alignment = log(pmax(QT_MAT_CURSO_TEC_share, 0.001) / pmax(econ_sect_va_prop, 0.001)),
    log_QT_MAT_CURSO_TEC = log(QT_MAT_CURSO_TEC)
  ) %>%
  
  # Sort for lag creation
  arrange(economic_sector, SG_UF, TP_DEPENDENCIA, ANO)

cat("STEP 1G - Data merging:\n")
cat("Before merge - enrollment_shares:", nrow(enrollment_shares), "\n")
cat("After economic merge:", nrow(model_df_base), "\n")
cat("Successful economic share matches:", sum(!is.na(model_df_base$econ_sect_va_prop)), "\n")
cat("Successful PIB matches:", sum(!is.na(model_df_base$pib_pc_growth)), "\n")
cat("Final merged dimensions:", dim(model_df_base), "\n")
cat("Key variable NAs - sector_alignment:", sum(is.na(model_df_base$sector_alignment)), "| log_QT_MAT_CURSO_TEC:", sum(is.na(model_df_base$log_QT_MAT_CURSO_TEC)), "\n\n")

model_df_base2 <- model_df_base %>% filter(!is.na(NM_UF)) # 4780 becomes 3754


#############################################################################
# STEP 1H: CREATE LAGGED VARIABLES (DEPENDENT AND INDEPENDENT)
#############################################################################

# Create all lagged variables by panel group
model_df <- model_df_base2 %>%
  group_by(economic_sector, SG_UF, TP_DEPENDENCIA) %>%
  arrange(ANO) %>%
  mutate(
    # Lagged dependent variables
    log_QT_MAT_CURSO_TEC_lag1 = lag(log_QT_MAT_CURSO_TEC, 1),
    log_QT_MAT_CURSO_TEC_lag2 = lag(log_QT_MAT_CURSO_TEC, 2),
    
    # Additional lagged independent variables
    sector_alignment_lag1 = lag(sector_alignment, 1),
    QT_MAT_CURSO_TEC_share_lag1 = lag(QT_MAT_CURSO_TEC_share, 1),
    econ_sect_va_prop_lag1 = lag(econ_sect_va_prop, 1)
    
    # Note: pib_pc_growth and pib_pc_growth_lag1 already created in pib_state_year step
  ) %>%
  ungroup()

cat("STEP 1H - Lagged variables creation:\n")
cat("Panel groups (sector × state × dependency):", n_distinct(paste(model_df$economic_sector, model_df$SG_UF, model_df$TP_DEPENDENCIA)), "\n")
cat("Model_df dimensions:", dim(model_df), "\n")
cat("Non-NA lag1:", sum(!is.na(model_df$log_QT_MAT_CURSO_TEC_lag1)), "| lag2:", sum(!is.na(model_df$log_QT_MAT_CURSO_TEC_lag2)), "\n")
cat("Non-NA sector_alignment_lag1:", sum(!is.na(model_df$sector_alignment_lag1)), "\n\n")


#############################################################################
# STEP 1I: FINAL DATA CLEANING
#############################################################################
#############################################################################
# STEP 1I: FINAL DATA CLEANING
#############################################################################

# Create estimation dataset - remove NAs and prepare for regression
estimation_df <- model_df %>%
  # Remove NAs in key variables
  filter(!is.na(log_QT_MAT_CURSO_TEC),
         !is.na(log_QT_MAT_CURSO_TEC_lag1),
         !is.na(log_QT_MAT_CURSO_TEC_lag2), 
         !is.na(pib_pc_growth),
         !is.na(pib_pc_growth_lag1),
         !is.na(sector_alignment),
         !is.na(econ_sect_va_prop)) %>%
  
  # Create factor variables for fixed effects
  mutate(
    economic_sector_fe = as.factor(economic_sector),
    state_fe = as.factor(SG_UF),
    dependency_fe = as.factor(TP_DEPENDENCIA),
    year_fe = as.factor(ANO),
    state_dependency_fe = as.factor(paste(SG_UF, TP_DEPENDENCIA, sep = "_"))
  ) %>%
  
  # Select final variables
  select(
    # Panel identifiers
    ANO, SG_UF, NM_UF, TP_DEPENDENCIA, economic_sector,
    economic_sector_fe, state_fe, dependency_fe, year_fe, state_dependency_fe,
    
    # Dependent variable and lags
    log_QT_MAT_CURSO_TEC, log_QT_MAT_CURSO_TEC_lag1, log_QT_MAT_CURSO_TEC_lag2, QT_MAT_CURSO_TEC,
    
    # Key explanatory variables  
    pib_pc_growth, pib_pc_growth_lag1, sector_alignment,
    
    # Additional variables
    QT_MAT_CURSO_TEC_share, econ_sect_va_prop, pib_per_capita
  )

cat("STEP 1I - Final data cleaning:\n")
cat("Before filtering:", nrow(model_df), "| After filtering:", nrow(estimation_df), "\n")
cat("Rows lost:", nrow(model_df) - nrow(estimation_df), "| Retention rate:", round(nrow(estimation_df)/nrow(model_df)*100,1), "%\n")
cat("Final dimensions:", dim(estimation_df), "\n")
cat("Column names:", paste(names(estimation_df), collapse = ", "), "\n\n")

#############################################################################
# STEP 1J: DATA SUMMARY AND DIAGNOSTICS
#############################################################################

cat("\n=== FINAL ESTIMATION DATASET SUMMARY ===\n")
cat("Panel dimensions:", dim(estimation_df), "\n")
cat("Years:", min(estimation_df$ANO), "to", max(estimation_df$ANO), "\n")
cat("States:", length(unique(estimation_df$SG_UF)), "\n")
cat("Economic sectors:", length(unique(estimation_df$economic_sector)), "\n")
cat("Dependencies:", length(unique(estimation_df$TP_DEPENDENCIA)), "\n")
cat("Total observations:", nrow(estimation_df), "\n")
cat("Panel groups:", n_distinct(paste(estimation_df$economic_sector, estimation_df$SG_UF, estimation_df$TP_DEPENDENCIA)), "\n")

# Check balance
balance_check <- estimation_df %>%
  group_by(SG_UF, economic_sector, TP_DEPENDENCIA) %>%
  summarise(years_available = n(), .groups = "drop") %>%
  count(years_available) %>%
  arrange(desc(years_available))

cat("\n=== PANEL BALANCE CHECK ===\n")
print(balance_check)

# Check sector distribution
cat("\n=== ENROLLMENT BY ECONOMIC SECTOR ===\n")
sector_summary <- estimation_df %>%
  group_by(economic_sector) %>%
  summarise(
    total_enrollment = sum(QT_MAT_CURSO_TEC, na.rm = TRUE),
    observations = n(),
    .groups = "drop"
  ) %>%
  mutate(pct_enrollment = total_enrollment / sum(total_enrollment) * 100)
print(sector_summary)

# Check data completeness
cat("\n=== DATA COMPLETENESS CHECK ===\n")
completeness <- sapply(estimation_df[c("log_QT_MAT_CURSO_TEC", "log_QT_MAT_CURSO_TEC_lag1", "log_QT_MAT_CURSO_TEC_lag2", 
                                       "pib_pc_growth", "pib_pc_growth_lag1", "sector_alignment")], 
                       function(x) sum(is.na(x)))
print(completeness)

# Preview final dataset
cat("\n=== SAMPLE DATA PREVIEW ===\n")
preview <- estimation_df %>% 
  select(ANO, SG_UF, TP_DEPENDENCIA, economic_sector, QT_MAT_CURSO_TEC, 
         log_QT_MAT_CURSO_TEC, pib_pc_growth, sector_alignment) %>%
  head(10)
print(preview)



cat("\n=== DATA PREPARATION COMPLETE ===\n")
cat("Dataset 'estimation_df' ready for time series estimation.\n")
cat("Next step: Run dynamic panel regression.\n")

df_model_ept1a <- estimation_df

# Save estimation dataset for Step 2 regression
save(df_model_ept1a, file = file.path(base_path, "rais/estimation_df.rda"))
cat("Dataset saved as df_model_ept1a\n")

