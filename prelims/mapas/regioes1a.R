library(sf)
library(tmap)

# Load shapefile
path <- "D:/Country/Brazil/TechBrazil/rawdata/ibge/RG2017_regioesgeograficas2017_20180911/RG2017_regioesgeograficas2017.shp"

geo_sf <- st_read(path)

sergipe_sf <- geo_sf[geo_sf$UF == "28", ]



# Convert rgint to factor for discrete coloring
sergipe_sf$nome_rgint <- as.factor(sergipe_sf$nome_rgint)
# Static Intermediaria
tmap_mode("plot")
# Plot using tmap v4 syntax
tm_shape(sergipe_sf) +
  tm_polygons(
    fill = "nome_rgint",
    fill.scale = tm_scale(values = "brewer.set3"),
    fill.legend = tm_legend(title = "Região Intermediária")
  ) +
  tm_text(
    text = "nome_rgint",
    size = 0.6,
    col = "black",
    options = opt_tm_text(just = "center")
  ) +
  tm_title("Regiões Intermediárias - Sergipe") +
  tm_layout(
    outer.margins = c(0.12, 0.05, 0.05, 0.01),
    legend.outside = TRUE,
    frame = FALSE
  )

# Convert rgint to factor for discrete coloring
sergipe_sf$nome_rgi <- as.factor(sergipe_sf$nome_rgi)
# Static Intermediaria
tmap_mode("plot")
# Plot using tmap v4 syntax
tm_shape(sergipe_sf) +
  tm_polygons(
    fill = "nome_rgi",
    fill.scale = tm_scale(values = "brewer.set3"),
    fill.legend = tm_legend(title = "Região Imediata")
  ) +
  tm_text(
    text = "nome_rgint",
    size = 0.6,
    col = "black",
    options = opt_tm_text(just = "center")
  ) +
  tm_title("Regiões Imediatas - Sergipe") +
  tm_layout(
    outer.margins = c(0.12, 0.05, 0.05, 0.01),
    legend.outside = TRUE,
    frame = FALSE
  )
