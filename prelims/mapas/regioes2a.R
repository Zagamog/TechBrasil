# regioes2a.R — versão adaptada para tmap v4

# Bibliotecas
library(sf)
library(tmap)
library(dplyr)
library(aws.s3)
library(dotenv)

# --- Carregar credenciais AWS ---
dotenv::load_dot_env()

# Caminhos
bucket_name <- "techbrazildata"
gpkg_local_path <- "D:/Country/Brazil/TechBrazil/working/ibge/mapas/sf_regioes.gpkg"
gpkg_s3_key <- "working/ibge/mapas/sf_regioes.gpkg"

# --- Carregar GeoPackage ---
message("--- Carregando dados espaciais (GeoPackage) ---")
if (file.exists(gpkg_local_path)) {
  message("✅ GeoPackage encontrado localmente. Carregando 'sf_regioes'...")
  sf_regioes <- st_read(dsn = gpkg_local_path, layer = "sf_regioes_ibge")
} else {
  message(paste("GeoPackage não encontrado localmente. Tentando baixar de s3://", bucket_name, "/", gpkg_s3_key, "...", sep = ""))
  tryCatch({
    dir.create(dirname(gpkg_local_path), recursive = TRUE, showWarnings = FALSE)
    save_object(object = gpkg_s3_key, bucket = bucket_name, file = gpkg_local_path)
    message("✅ GeoPackage baixado para ", gpkg_local_path)
    sf_regioes <- st_read(dsn = gpkg_local_path, layer = "sf_regioes_ibge")
  }, error = function(e) {
    stop(paste("❌ Erro ao carregar GeoPackage: ", e$message))
  })
}

# --- Filtrar para Sergipe ---
message("\n--- Filtrando dados para Sergipe ---")
sergipe_sf <- sf_regioes[sf_regioes$CO_UF == "28", ]
message(paste("✅ Dados de Sergipe filtrados. Total de feições:", nrow(sergipe_sf)))

# Set modo de visualização
tmap_mode("plot")

# --- Mapa 1: Regiões Intermediárias ---
message("\n--- Gerando Mapa: Regiões Intermediárias (Sergipe) ---")
sergipe_sf$NM_RGIINTM <- as.factor(sergipe_sf$NM_RGIINTM)

tm_shape(sergipe_sf) +
  tm_polygons(
    fill = "NM_RGIINTM",
    fill.scale = tm_scale_categorical(values = "brewer.set3"),
    fill.legend = tm_legend(title = "Região Intermediária"),
    fill_alpha = 0.8
  ) +
  tm_text(
    text = "NM_RGIINTM",
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


# --- Mapa 2: Regiões Imediatas ---
# --- Mapa 2: Regiões Imediatas de Sergipe ---
message("\n--- Gerando Mapa: Regiões Imediatas (Sergipe) ---")
sergipe_sf$NM_RGIMED <- as.factor(sergipe_sf$NM_RGIMED)

tm_shape(sergipe_sf) +
  tm_polygons(
    fill = "NM_RGIMED",
    fill.scale = tm_scale_categorical(values = "brewer.set3"),
    fill.legend = tm_legend(title = "Região Imediata"),
    fill_alpha = 0.8
  ) +
  tm_text(
    text = "NM_RGIMED",
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
message("✅ Mapa de Regiões Imediatas gerado. Ele será exibido na janela de plots.")

