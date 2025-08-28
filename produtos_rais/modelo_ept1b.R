# modelo_ept1b.R
# EPT Time Series Model - Complete Data Loading and Preparation
# Step 1: Load, clean, and construct panel dataframe for EPT enrollment regression

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(data.table)
library(fixest)

load("D:/Country/Brazil/TechBrazil/working/rais/df_model_ept1a.rda")


model1a <- feols(log_QT_MAT_CURSO_TEC ~ log_QT_MAT_CURSO_TEC_lag1 + log_QT_MAT_CURSO_TEC_lag2 +
                 pib_pc_growth + pib_pc_growth_lag1 + sector_alignment |
                 economic_sector + SG_UF + TP_DEPENDENCIA + ANO,
               data = df_model_ept1a, cluster = ~SG_UF)

summary(model1a)

# Get all fixed effects
fixed_effects <- fixef(model1a)

# Look at structure
str(fixed_effects)

# Extract specific fixed effect categories
state_fe <- fixed_effects$SG_UF
sector_fe <- fixed_effects$economic_sector  
dependency_fe <- fixed_effects$TP_DEPENDENCIA
year_fe <- fixed_effects$ANO

# View state fixed effects (most policy-relevant)
print(state_fe)

# Sort states by fixed effect magnitude
sort(state_fe, decreasing = TRUE)

# View sector fixed effects
print(sector_fe)

# View year fixed effects (time trend)
print(year_fe)