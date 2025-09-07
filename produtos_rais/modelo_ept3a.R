# modelo_ept3a.R 

library(fixest)
library(dplyr)
library(data.table)
library(stargazer)

load("D:/Country/Brazil/TechBrazil/working/rais/df_model_ept1a.rda")
load("D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda")

model_ols_simple <- feols(log_QT_MAT_CURSO_TEC ~ 
                            log(pib_per_capita) + sector_alignment |
                            economic_sector + SG_UF + TP_DEPENDENCIA + ANO,
                          data = df_model_ept1a, cluster = ~SG_UF)


summary(model_ols_simple)



# Or to view it first
etable(model_ols_simple, tex = TRUE)



# Extract F-statistic manually
n_obs <- model_ols_simple$nobs
n_params <- length(coef(model_ols_simple))
n_fe <- sum(model_ols_simple$fixef_sizes) - length(model_ols_simple$fixef_sizes)
df1 <- n_params  # degrees of freedom for numerator
df2 <- n_obs - n_params - n_fe  # degrees of freedom for denominator

# Calculate F-statistic from R²
r2 <- r2(model_ols_simple)["r2"]
f_stat <- (r2 / df1) / ((1 - r2) / df2)
p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)

cat("Model F-statistic:", round(f_stat, 3), 
    "with df1 =", df1, "and df2 =", df2,
    "\nP-value:", format(p_value, scientific = TRUE), "\n")


# 1. Get all fixed effects
fe_all <- fixef(model_ols_simple)

# This returns a list with:
# fe_all$economic_sector
# fe_all$SG_UF  
# fe_all$TP_DEPENDENCIA
# fe_all$ANO

# 2. View specific fixed effects
fe_all$SG_UF  # State fixed effects
fe_all$TP_DEPENDENCIA  # Dependency type effects
fe_all$economic_sector  # Sector effects
fe_all$ANO  # Year effects

# 3. Get summary statistics for fixed effects
summary(fixef(model_ols_simple))

# 4. Plot fixed effects
plot(fixef(model_ols_simple))



residuals_ols <- residuals(model_ols_simple)

# Create residuals dataframe
residuals_df_ols <- df_model_ept1a %>%
  mutate(residual = residuals_ols) %>%
  select(ANO, SG_UF, TP_DEPENDENCIA, economic_sector, 
         QT_MAT_CURSO_TEC, residual, sector_alignment, pib_per_capita)


dft_geo_keys <- as.data.table(df_codes_ibge)[
  , .(SG_UF, NM_UF)
]
dft_geo_keys <- unique(dft_geo_keys, by = "SG_UF")

df_residuals_ols <- residuals_df_ols %>%
  left_join(
    dft_geo_keys %>% distinct(SG_UF, NM_UF), 
    by = "SG_UF"
  )

glimpse(df_residuals_ols)


save(df_residuals_ols, file="D:/Country/Brazil/TechBrazil/working/rais/df_residuals_ols.rda")

summary(model_ols_simple)


