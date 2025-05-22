# regioes2a.R

# Este script carrega os dados geográficos processados (GeoPackage),
# filtra para o estado de Sergipe e gera mapas temáticos usando tmap.

# Carrega as bibliotecas necessárias
library(sf)         # Para trabalhar com dados espaciais
library(tmap)       # Para criação de mapas temáticos
library(dplyr)      # Para manipulação de dados
library(aws.s3)     # Para interagir com o Amazon S3
library(dotenv)     # Para carregar variáveis de ambiente de forma segura

# --- Passo 1: Carregar Credenciais AWS de forma segura ---
# Garanta que seu arquivo `.env` esteja no diretório correto e contenha:
# AWS_ACCESS_KEY_ID="SEU_ACCESS_KEY_ID"
# AWS_SECRET_ACCESS_KEY="SEU_SECRET_ACCESS_KEY"
# AWS_DEFAULT_REGION="sua-regiao-aws"
dotenv::load_dot_env()

# --- Passo 2: Definir Caminhos para o GeoPackage ---

# Nome do bucket S3 (fixo conforme suas últimas instruções)
bucket_name <- "techbrazildata"

# Caminho local onde o GeoPackage deve estar
gpkg_local_path <- "D:/Country/Brazil/TechBrazil/working/ibge/mapas/sf_regioes.gpkg"

# Caminho (chave) do GeoPackage no S3 (agora hardcoded)
gpkg_s3_key <- "working/ibge/mapas/sf_regioes.gpkg"

# --- Passo 3: Carregar o objeto sf_regioes do GeoPackage (Localmente ou do S3) ---

message("--- Carregando dados espaciais (GeoPackage) ---")
if (file.exists(gpkg_local_path)) {
  message("✅ GeoPackage encontrado localmente. Carregando 'sf_regioes'...")
  sf_regioes <- st_read(dsn = gpkg_local_path, layer = "sf_regioes_ibge")
  message("✅ 'sf_regioes' carregado com sucesso do local.")
} else {
  message(paste("GeoPackage não encontrado localmente. Tentando baixar de s3://", bucket_name, "/", gpkg_s3_key, "...", sep = ""))
  tryCatch({
    # 1. Garante que o diretório local exista para salvar o arquivo baixado
    dir.create(dirname(gpkg_local_path), recursive = TRUE, showWarnings = FALSE)
    
    # 2. Baixa o arquivo GeoPackage do S3 para o caminho local
    save_object(
      object = gpkg_s3_key,
      bucket = bucket_name,
      file = gpkg_local_path
    )
    message(paste("✅ GeoPackage baixado para ", gpkg_local_path, ".", sep = ""))
    
    # 3. Carrega o GeoPackage agora que ele está localmente disponível
    sf_regioes <- st_read(dsn = gpkg_local_path, layer = "sf_regioes_ibge")
    message("✅ 'sf_regioes' carregado com sucesso após download do S3.")
    
  }, error = function(e) {
    stop(paste("❌ Erro ao carregar GeoPackage: ", e$message,
               "\nPor favor, verifique o caminho local, as credenciais AWS e o caminho do S3 (", gpkg_s3_key, ")."))
  })
}

# --- Passo 4: Filtrar dados para o estado de Sergipe (CO_UF == "28") ---
message("\n--- Filtrando dados para Sergipe ---")
sergipe_sf <- sf_regioes[sf_regioes$CO_UF == "28", ]
message(paste("✅ Dados de Sergipe filtrados. Total de feições:", nrow(sergipe_sf)))


# --- Passo 5: Gerar Mapas Temáticos com tmap ---

# Mapa 1: Regiões Intermediárias de Sergipe
message("\n--- Gerando Mapa: Regiões Intermediárias (Sergipe) ---")
# Converte NM_RGIINTM para fator para coloração discreta
sergipe_sf$NM_RGIINTM <- as.factor(sergipe_sf$NM_RGIINTM)

tmap_mode("plot") # Define o modo de plotagem (estático para melhor qualidade de imagem)

tm_shape(sergipe_sf) +
  tm_polygons(
    col = "NM_RGIINTM", 
    palette = "Set3",  
    title = "Região Intermediária", 
    alpha = 0.8 
  ) +
  tm_text(
    text = "NM_RGIINTM",
    size = 0.6,
    col = "black",
    just = "center"
  ) +
  tm_title("Regiões Intermediárias - Sergipe") +
  tm_layout(
    outer.margins = c(0.12, 0.05, 0.05, 0.01),
    legend.outside = TRUE,
    frame = FALSE
  )
message("✅ Mapa de Regiões Intermediárias gerado. Ele será exibido na janela de plots.")

# Mapa 2: Regiões Imediatas de Sergipe
message("\n--- Gerando Mapa: Regiões Imediatas (Sergipe) ---")
# Converte NM_RGIMED para fator para coloração discreta
sergipe_sf$NM_RGIMED <- as.factor(sergipe_sf$NM_RGIMED)

tmap_mode("plot") # Define o modo de plotagem novamente (estático)

tm_shape(sergipe_sf) +
  tm_polygons(
    col = "NM_RGIMED", 
    palette = "Set3",  
    title = "Região Imediata", 
    alpha = 0.8 
  ) +
  tm_text(
    text = "NM_RGIMED", 
    size = 0.6,
    col = "black",
    just = "center"
  ) +
  tm_title("Regiões Imediatas - Sergipe") +
  tm_layout(
    outer.margins = c(0.12, 0.05, 0.05, 0.01),
    legend.outside = TRUE,
    frame = FALSE
  )
message("✅ Mapa de Regiões Imediatas gerado. Ele será exibido na janela de plots.")

message("\n--- Script regioes2a.R concluído ---")