# BM_FGV_Propag2a_Painel2.R

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# --- Load all df_censoXX.rda files (2007–2024) ---
anos_validos <- 2007:2024

uf_selected <- "SP"
ept_var <- "QT_MAT_PROF_TEC_PROPAG"
other_var <- "QT_MAT_MED"

# Load and summarize per year only for selected UF
df_all <- purrr::map_dfr(anos_validos, function(ano) {
  short <- substr(as.character(ano), 3, 4)
  file <- paste0("df_censo", short, ".rda")
  
  if (file.exists(file)) {
    load(file)
    df <- get(paste0("df_censo", short))
    
    df_uf <- df %>%
      filter(SG_UF == uf_selected) %>%
      summarise(
        ANO = ano,
        EPT = sum(.data[[ept_var]], na.rm = TRUE),
        OUTRA = sum(.data[[other_var]], na.rm = TRUE)
      )
    
    return(df_uf)
  } else {
    NULL
  }
})


# Ensure ANO is numeric (defensive)
df_all$ANO <- as.numeric(df_all$ANO)

# Triplo da meta 2013
meta_tripla <- 3 * df_all$EPT[df_all$ANO == 2013]

# --- 1. Linear projection from 2020–2024 ---
# Step 1: Fit linear model on selected years (2020–2024 for example)
df_recent <- df_all %>% filter(ANO %in% 2020:2024)
linear_model <- lm(EPT ~ ANO, data = df_recent)

# Step 2: Extract slope only
slope <- coef(linear_model)["ANO"]

# Step 3: Manually construct future values, starting from actual EPT in 2024
start_val <- df_all$EPT[df_all$ANO == 2024]

future_years <- 2024:2035
years_from_start <- future_years - 2024

future_df <- data.frame(
  ANO = future_years,
  EPT = start_val + slope * years_from_start,
  OUTRA = NA,
  VARIAVEL = "PROJECAO"
)

# Step 4: Combine observed and projected
df_long <- df_all %>%
  mutate(VARIAVEL = "OBSERVADO") %>%
  select(ANO, EPT, OUTRA, VARIAVEL)

df_final <- bind_rows(df_long, future_df)


# Pivot to long format for plotting
df_plot <- df_final %>%
  pivot_longer(cols = c("EPT", "OUTRA"), names_to = "TIPO", values_to = "VALOR") %>%
  mutate(GRUPO = ifelse(TIPO == "EPT", VARIAVEL, "OUTRA"))

# --- 2. Plot ---
ggplot(df_plot, aes(x = ANO, y = VALOR, color = GRUPO)) +
  geom_line(size = 1.5) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = meta_tripla, linetype = "dashed", color = "darkorange", linewidth = 1.1) +
  annotate("text", x = 2008, y = meta_tripla, label = paste0("Meta 11 (Triplo 2013): ", format(meta_tripla, big.mark = ".")), color = "darkorange", vjust = -1, fontface = "bold") +
  scale_x_continuous(breaks = 2007:2035, limits = c(2007, 2035)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = paste("UF:", uf_selected),
    subtitle = paste(ept_var, "vs", other_var),
    x = "Ano",
    y = "Total de Matrículas"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
