# bahia_conect2a.R
library(dplyr)
library(ggplot2)
library(sf)

# Step 1: Create muni-level summary from school_net_wrat
muni_actual_speed <- school_net_wrat %>%
  select(CO_MUN, NM_MUN, muni_weighted_actual) %>%
  mutate(CO_MUN = as.character(CO_MUN)) %>%
  distinct()


# Step 2: Join to Bahia shapefile

###
# Examine contents of sf_regioes.gpkg

gpkg_path <- "D:/Country/Brazil/TechBrazil/working/ibge/mapas/sf_regioes.gpkg"

# List available layers (just like listing tables in a database)
sf::st_layers(gpkg_path)
# indicates currently one layer - sf_regioes_ibge

sf_regioes <- sf::st_read(gpkg_path, layer = "sf_regioes_ibge")

sf_bahia <- sf_regioes %>% filter(CO_UF == 29)  # 29 = Bahia

sf_bahia_speed <- sf_bahia %>%
  left_join(muni_actual_speed, by = "CO_MUN")

# Step 3: Create bins for muni_weighted_actual
sf_bahia_speed <- sf_bahia_speed %>%
  mutate(
    speed_bin = case_when(
      is.na(muni_weighted_actual) ~ NA_character_,
      muni_weighted_actual < 0.25 ~ "< 0.25",
      muni_weighted_actual < 0.5  ~ "0.25–0.5",
      muni_weighted_actual < 1    ~ "0.5–1.0",
      muni_weighted_actual < 2    ~ "1.0–2.0",
      TRUE                        ~ "> 2.0"
    ),
    speed_bin = factor(speed_bin, levels = c("> 2.0", "1.0–2.0", "0.5–1.0", "0.25–0.5", "< 0.25"))
  )

# Step 4: Define custom high-contrast colors (descending intensity)
speed_colors <- c(
  "> 2.0"     = "#67000d",
  "1.0–2.0"   = "#cb181d",
  "0.5–1.0"   = "#ef3b2c",
  "0.25–0.5"  = "#fb6a4a",
  "< 0.25"    = "#fee5d9"
)

# Step 5: Plot
ggplot(data = sf_bahia_speed) +
  geom_sf(aes(fill = speed_bin), color = "gray60", size = 0.1) +
  scale_fill_manual(
    values = speed_colors,
    na.value = "white",
    name = "Mbps por aluno\n(média municipal)"
  ) +
  labs(
    title = "Velocidade Média de Internet por Aluno",
    subtitle = "Escolas Estaduais – Municípios da Bahia",
    caption = "Fonte: Censo Escolar 2024 e SIMET (nic.br)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right"
  )

library(ggplot2)
library(dplyr)

# Categorize schools with and without actual_per_student
school_all <- school_net_wrat %>%
  filter(NM_UF == "Bahia", !is.na(latitude), !is.na(longitude)) %>%
  mutate(speed_cat = case_when(
    is.na(actual_per_student) | actual_per_student == 0 ~ "Sem dados",
    actual_per_student < 0.25 ~ "< 0.25",
    actual_per_student < 0.5  ~ "0.25–0.5",
    actual_per_student < 1    ~ "0.5–1.0",
    actual_per_student < 2    ~ "1.0–2.0",
    TRUE                      ~ "> 2.0"
  )) %>%
  mutate(speed_cat = factor(speed_cat,
                            levels = c("> 2.0", "1.0–2.0", "0.5–1.0", "0.25–0.5", "< 0.25", "Sem dados")))

# Define colors and fills
point_colors <- c(
  "> 2.0"     = "red",
  "1.0–2.0"   = "darkred",
  "0.5–1.0"   = "blue",
  "0.25–0.5"  = "cyan",
  "< 0.25"    = "yellow",
  "Sem dados" = "red"  # border color
)

point_fills <- c(
  "> 2.0"     = "red",
  "1.0–2.0"   = "darkred",
  "0.5–1.0"   = "blue",
  "0.25–0.5"  = "cyan",
  "< 0.25"    = "yellow",
  "Sem dados" = "white"  # fill color
)

# Plot with red-bordered white points for missing data
ggplot() +
  geom_sf(data = sf_bahia, fill = "gray95", color = "gray60", size = 0.1) +
  geom_point(data = school_all,
             aes(x = longitude, y = latitude, fill = speed_cat, color = speed_cat),
             shape = 21, stroke = 0.4, size = 2.3, alpha = 0.8) +
  scale_color_manual(values = point_colors, name = "Mbps por aluno") +
  scale_fill_manual(values = point_fills, name = "Mbps por aluno") +
  labs(
    title = "Velocidade de Internet por Escola Estadual",
    subtitle = "Pontos georreferenciados – Bahia",
    caption = "Fonte: SIMET (nic.br) e Censo Escolar 2024"
  ) +
  coord_sf(xlim = c(-46, -37.5), ylim = c(-18.5, -8.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    legend.position = "right"
  )
