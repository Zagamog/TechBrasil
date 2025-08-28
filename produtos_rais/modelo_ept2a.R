# modelo_ept2a.R
# EPT Time Series Model - Complete Data Loading and Preparation
# Step 1: Load, clean, and construct panel dataframe for EPT enrollment regression

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(data.table)
library(fixest)

load("D:/Country/Brazil/TechBrazil/working/rais/df_model_ept1a.rda")


library(plm)


# Create unique panel identifier
df_model_ept1a$panel_id <- paste(df_model_ept1a$SG_UF, 
                                 df_model_ept1a$TP_DEPENDENCIA, 
                                 df_model_ept1a$economic_sector, 
                                 sep = "_")

# Check for remaining duplicates
table(paste(df_model_ept1a$panel_id, df_model_ept1a$ANO), useNA = "ifany")

# Create proper panel data frame
pdata_df <- pdata.frame(df_model_ept1a, 
                        index = c("panel_id", "ANO"))

# Then run GMM
model_gmm1a <- pgmm(log_QT_MAT_CURSO_TEC ~ lag(log_QT_MAT_CURSO_TEC, 1:2) + 
                      pib_pc_growth + lag(pib_pc_growth, 1) + sector_alignment | 
                      lag(log_QT_MAT_CURSO_TEC, 2:4),
                    data = pdata_df, 
                    effect = "twoways", model = "twosteps")

summary(model_gmm1a)



# Extract residuals and identify patterns




# Match residuals back to original data
estimation_subset <- df_model_ept1a[complete.cases(df_model_ept1a[c("log_QT_MAT_CURSO_TEC", "log_QT_MAT_CURSO_TEC_lag1", "log_QT_MAT_CURSO_TEC_lag2", "pib_pc_growth", "sector_alignment")]), ]

residuals_df <- estimation_subset[1:length(residuals(model_gmm1a)), ] %>%
  mutate(residual = residuals(model_gmm1a)) %>%
  select(ANO, SG_UF, TP_DEPENDENCIA, economic_sector, QT_MAT_CURSO_TEC, residual)




# Convert each list element to a proper data frame
residuals_df_clean <- residuals_df %>%
  mutate(row_id = row_number()) %>%
  rowwise() %>%
  do({
    res_vec <- unlist(.$residual)
    data.frame(
      ANO = as.numeric(names(res_vec)),
      residual = as.numeric(res_vec),
      SG_UF = .$SG_UF,
      TP_DEPENDENCIA = .$TP_DEPENDENCIA,
      economic_sector = .$economic_sector,
      QT_MAT_CURSO_TEC = .$QT_MAT_CURSO_TEC
    )
  })


# Examine residual distribution by group
residual_summary <- residuals_df_clean %>%
  group_by(SG_UF, economic_sector, TP_DEPENDENCIA) %>%
  summarise(
    avg_residual = mean(residual, na.rm = TRUE),
    positive_years = sum(residual > 0, na.rm = TRUE),
    total_years = n(),
    consistency = positive_years / total_years,
    .groups = "drop"
  )

# Check distributions
summary(residual_summary$avg_residual)
summary(residual_summary$consistency)

# Plot distributions
hist(residual_summary$avg_residual, main = "Average Residuals Distribution")
hist(residual_summary$consistency, main = "Consistency Distribution")


# More realistic thresholds - top quartile performers
winners <- residuals_df_clean %>%
  group_by(SG_UF, economic_sector, TP_DEPENDENCIA) %>%
  summarise(
    avg_residual = mean(residual, na.rm = TRUE),
    positive_years = sum(residual > 0, na.rm = TRUE),
    total_years = n(),
    consistency = positive_years / total_years,
    total_enrollment = mean(QT_MAT_CURSO_TEC),
    .groups = "drop"
  ) %>%
  filter(avg_residual > quantile(avg_residual, 0.75, na.rm = TRUE)) %>%
  arrange(desc(avg_residual))

private_winners <- winners %>%
  filter(TP_DEPENDENCIA == "Privada") %>%
  arrange(desc(avg_residual)) %>%
  select(SG_UF, economic_sector, avg_residual, consistency, total_enrollment, positive_years, total_years)

print(private_winners)

# State (Estadual) over-performers
state_winners <- winners %>%
  filter(TP_DEPENDENCIA == "Estadual") %>%
  arrange(desc(avg_residual)) %>%
  select(SG_UF, economic_sector, avg_residual, consistency, total_enrollment, positive_years, total_years)

# Federal over-performers  
federal_winners <- winners %>%
  filter(TP_DEPENDENCIA == "Federal") %>%
  arrange(desc(avg_residual)) %>%
  select(SG_UF, economic_sector, avg_residual, consistency, total_enrollment, positive_years, total_years)

# Municipal over-performers
municipal_winners <- winners %>%
  filter(TP_DEPENDENCIA == "Municipal") %>%
  arrange(desc(avg_residual)) %>%
  select(SG_UF, economic_sector, avg_residual, consistency, total_enrollment, positive_years, total_years)

print("STATE (ESTADUAL) WINNERS:")
print(state_winners)

print("FEDERAL WINNERS:")
print(federal_winners)

print("MUNICIPAL WINNERS:")  
print(municipal_winners)



save(residuals_df_clean, file = "D:/Country/Brazil/TechBrazil/working/rais/residuals_df_clean.rda")
write.csv(residuals_df_clean, "D:/Country/Brazil/TechBrazil/working/rais/residuals_df_clean.csv", row.names = FALSE)



