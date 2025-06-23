# bahia_ideb1a.R

library(dplyr)
library(ggplot2)
library(sf)
library(openxlsx)
library(scales) # for pretty_breaks
library(RColorBrewer)
library(tidyr)

# Load the IDEB municipal level data from D:\Country\Brazil\TechBrazil\rawdata\mec_inep

# Data to map

# divulgacao_ensino_medio_municipios_2023.xlsx
ideb_munis <- read.xlsx("D:/Country/Brazil/TechBrazil/rawdata/mec_inep/divulgacao_ensino_medio_municipios_2023.xlsx",
                        rows=c(11:11732), cols=(c(1:4,41:44)),colNames = TRUE) 

# Columns 5 to 8 rounded off to 1 decimal place after converting string to numerical
ideb_munis_redest <- ideb_munis %>% filter(REDE=="Estadual") %>% 
  mutate(across(c(5:8), ~ round(as.numeric(.), 1))) 


ba_ideb <- ideb_munis_redest %>% 
  filter(SG_UF == "BA")  %>% mutate(CO_MUN = as.character(CO_MUN)) %>% select(-NM_MUN)

###
# Examine contents of sf_regioes.gpkg

gpkg_path <- "D:/Country/Brazil/TechBrazil/working/ibge/mapas/sf_regioes.gpkg"

# List available layers (just like listing tables in a database)
sf::st_layers(gpkg_path)
# indicates currently one layer - sf_regioes_ibge

sf_regioes <- sf::st_read(gpkg_path, layer = "sf_regioes_ibge")

sf_bahia <- sf_regioes %>% filter(CO_UF == 29)  # 29 = Bahia

# Join IDEB to spatial dataframe
sf_bahia_ideb <- sf_bahia %>% 
  left_join(ba_ideb, by = "CO_MUN")



# Use the joined spatial object or the ba_ideb data
ideb_values <- sf_bahia_ideb$IDEB_2017
# Remove NAs
ideb_values <- ideb_values[!is.na(ideb_values)]
# Calculate percentiles
quantile(ideb_values, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)


# COVID-19 mostly NAs in 2021
sf_bahia_ideb_long <- sf_bahia_ideb %>% select(-IDEB_2021) %>% 
  pivot_longer(
    cols = starts_with("IDEB_"),
    names_to = "year",
    names_prefix = "IDEB_",
    values_to = "ideb"
  ) %>%
  mutate(year = as.integer(year))


sf_bahia_ideb_long <- sf_bahia_ideb_long %>%
  mutate(ideb_bin = case_when(
    ideb < 3.3         ~ "< 3.3",
    ideb < 3.5         ~ "3.3–3.5",
    ideb < 3.8         ~ "3.5–3.8",
    ideb < 4.0         ~ "3.8–4.0",
    ideb <= 4.3        ~ "4.0–4.3",
    TRUE               ~ NA_character_
  ),
  ideb_bin = factor(ideb_bin,
                    levels = c("< 3.3", "3.3–3.5", "3.5–3.8", "3.8–4.0", "4.0–4.3"))
  )
# Define bin levels in desired (descending) order
levels_ordered <- c("4.0–4.3", "3.8–4.0", "3.5–3.8", "3.3–3.5", "< 3.3")

# Optional: define custom colors (you can tweak these)
bin_colors <- c(
  "4.0–4.3" = "#d73027",  # deep red (blood red)
  "3.8–4.0" = "#fc4e2a",  # strong orange-red
  "3.5–3.8" = "#fd8d3c",  # orange
  "3.3–3.5" = "#ffd700",  # pale orange / beige
  "< 3.3"   = "#ffffb2"   # white
)

# Apply factor with correct order
sf_bahia_ideb_long <- sf_bahia_ideb_long %>%
  mutate(
    ideb_bin = factor(ideb_bin, levels = levels_ordered)
  )

# Plot with manual color scale
ggplot(data = sf_bahia_ideb_long) +
  geom_sf(aes(fill = ideb_bin), color = "gray40", size = 0.1) +
  scale_fill_manual(
    values = bin_colors,
    na.value = "white",
    name = "IDEB"
  ) +
  facet_wrap(~ year) +
  labs(
    title = "IDEB - Ensino Médio Estadual (Bahia)",
    subtitle = "Distribuição por faixa (2017–2023)",
    caption = "Fonte: INEP | Mapa: IBGE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right"
  )
