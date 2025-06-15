regioes1a.R

# Este script prepara dados para mapas regionais, carregando códigos geográficos
# e dados espaciais do IBGE, com fallback para S3.

# Creating the database sf_regioes.gpkg - initial layer

# Carrega as bibliotecas necessárias
library(sf)        # Para trabalhar com dados espaciais (shapefiles)
library(tmap)      # Para criação de mapas temáticos
library(dplyr)     # Para manipulação de dadosNM_RG
library(aws.s3)    # Para interagir com o Amazon S3
library(dotenv)    # Para carregar variáveis de ambiente de forma segura
library(sjlabelled) # Para manipulação de rótulos de dados (se necessário)
library(utils)     # Para a função unzip()


# --- Passo 1: Carregar Credenciais AWS de forma segura ---
# Garanta que cada membro da equipe tenha seu próprio arquivo `.env`
# no diretório raiz do projeto ou em um local acessível.
# Exemplo de conteúdo do .env:
# AWS_ACCESS_KEY_ID="SEU_ACCESS_KEY_ID"
# AWS_SECRET_ACCESS_KEY="SEU_SECRET_ACCESS_KEY"
# AWS_DEFAULT_REGION="sua-regiao-aws" # Ex: "us-east-1"
# AWS_S3_BUCKET_NAME="techbrazildata" # Seu nome do bucket S3
# S3_PIBMUNIS_KEY="working/ibge/df_codes_ibge.rda" # Chave para df_codes_ibge.rda no S3
# S3_SHAPEFILE_ZIP_KEY="rawdata/ibge/RG2017_regioesgeograficas2017.zip" # Chave para o zip do shapefile no S3
dotenv::load_dot_env()

# --- Passo 2: Definir Caminhos para os Dados ---

# Nome do bucket S3 (carregado de variável de ambiente para segurança)
bucket_name <- "techbrazildata" # Nome do bucket S3
if (is.null(bucket_name) || bucket_name == "") {
  stop("Erro: A variável de ambiente 'AWS_S3_BUCKET_NAME' não está definida ou está vazia. Por favor, configure seu arquivo .env.")
}

# Caminhos para df_codes_ibge.rda
f1_local_path <- "D:/Country/Brazil/TechBrazil/working/ibge/df_codes_ibge.rda"
f1_s3_key     <- Sys.getenv("S3_PIBMUNIS_KEY") # Carregado de .env

# Caminhos para o shapefile das regiões geográficas
# O nome do arquivo .shp dentro do zip (e na pasta local)
shapefile_base_name <- "RG2017_regioesgeograficas2017"
f2_local_dir <- "D:/Country/Brazil/TechBrazil/rawdata/ibge/RG2017_regioesgeograficas2017_20180911"
f2_local_path <- file.path(f2_local_dir, paste0(shapefile_base_name, ".shp"))

# Caminho para o arquivo ZIP do shapefile no S3
shapefile_s3_zip_key <- Sys.getenv("S3_SHAPEFILE_ZIP_KEY")

# Caminho local temporário para o arquivo ZIP baixado
shapefile_local_zip_path <- file.path(tempdir(), paste0(shapefile_base_name, ".zip"))

# --- Passo 3: Carregar df_codes_ibge.rda (com fallback para S3) ---

# Verifica se o diretório local existe para salvar o arquivo se baixado
dir.create(dirname(f1_local_path), recursive = TRUE, showWarnings = FALSE)

if (file.exists(f1_local_path)) {
  message("✅ df_codes_ibge.rda encontrado localmente. Carregando...")
  load(f1_local_path) # Carrega o objeto df_codes_ibge para o ambiente
} else {
  message(paste("df_codes_ibge.rda não encontrado localmente. Tentando baixar de s3://", bucket_name, "/", f1_s3_key, "...", sep = ""))
  tryCatch({
    # Baixa o arquivo do S3 e o carrega diretamente
    # Assume que o arquivo no S3 foi salvo com saveRDS()
    df_codes_ibge <- s3read_using(
      FUN = readRDS,
      object = f1_s3_key,
      bucket = bucket_name
    )
    message("✅ df_codes_ibge.rda baixado e carregado com sucesso do S3.")
    
    # Opcional: Salva uma cópia local para uso futuro
    saveRDS(df_codes_ibge, file = f1_local_path)
    message("Cópia local de df_codes_ibge.rda salva.")
  }, error = function(e) {
    stop(paste("❌ Erro ao carregar df_codes_ibge.rda: ", e$message,
               "\nPor favor, verifique o caminho local, as credenciais AWS e o caminho do S3 (", f1_s3_key, ")."))
  })
}

# --- Passo 4: Carregar o Shapefile (RG2017_regioesgeograficas2017.shp) com fallback para S3 ---

# Verifica se o diretório de extração local existe e se o .shp está lá
if (file.exists(f2_local_path)) {
  message("✅ Shapefile das regiões geográficas encontrado localmente. Carregando...")
  sf_regioes <- st_read(f2_local_path)
  message("✅ Shapefile carregado com sucesso.")
} else {
  message(paste("Shapefile das regiões geográficas não encontrado localmente em ", f2_local_dir,
                ". Tentando baixar o ZIP de s3://", bucket_name, "/", shapefile_s3_zip_key, "...", sep = ""))
  tryCatch({
    # 1. Cria o diretório local para extração se não existir
    dir.create(f2_local_dir, recursive = TRUE, showWarnings = FALSE)
    
    # 2. Baixa o arquivo ZIP do S3 para um local temporário
    save_object(
      object = shapefile_s3_zip_key,
      bucket = bucket_name,
      file = shapefile_local_zip_path
    )
    message(paste("✅ Arquivo ZIP do shapefile baixado para ", shapefile_local_zip_path, ".", sep = ""))
    
    # 3. Descompacta o arquivo ZIP para o diretório de destino
    unzip(
      zipfile = shapefile_local_zip_path,
      exdir = f2_local_dir # Extrai para o diretório onde o .shp é esperado
    )
    message(paste("✅ Arquivo ZIP descompactado para ", f2_local_dir, ".", sep = ""))
    
    # 4. Carrega o shapefile agora que ele está localmente disponível
    sf_regioes <- st_read(f2_local_path)
    message("✅ Shapefile carregado com sucesso após download e descompactação do S3.")
    
    # Opcional: Limpa o arquivo ZIP temporário
    # file.remove(shapefile_local_zip_path)
    # message("Arquivo ZIP temporário removido.")
    
  }, error = function(e) {
    stop(paste("❌ Erro ao carregar o shapefile: ", e$message,
               "\nPor favor, verifique o caminho local, as credenciais AWS e o caminho do ZIP no S3 (", shapefile_s3_zip_key, ")."))
  })
}

# --- Passo 5: Renomear colunas no objeto sf_regioes ---

message("Renomeando colunas no objeto sf_regioes...")
sf_regioes <- sf_regioes %>%
  rename(
    NM_MUN = NOME,
    CO_MUN = CD_GEOCODI,
    CO_UF = UF,
    CO_RGIMED = rgi,
    NM_RGIMED = nome_rgi,
    CO_RGINTM = rgint,
    NM_RGIINTM = nome_rgint
    # OBJECTID e geometry serão mantidos
  )

message("✅ Colunas renomeadas com sucesso.")
print("Novos nomes das colunas de sf_regioes:")
print(names(sf_regioes))



# --- Passo 6: Salvar o objeto sf_regioes renomeado (como GeoPackage) ---

# Define o caminho para salvar o novo GeoPackage
output_dir <- "D:/Country/Brazil/TechBrazil/working/ibge/mapas" # Diretório para GeoPackage
output_file_name <- "sf_regioes.gpkg" # Nome do arquivo com extensão .gpkg
output_path <- file.path(output_dir, output_file_name)

# Garante que o diretório de saída exista
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message(paste("Salvando o objeto sf como GeoPackage em:", output_path))

tryCatch({
  # Converte o objeto sf para 2D, removendo as coordenadas Z (e M se existirem)
  # A atribuição é feita de volta para 'sf_regioes', modificando-o in-place.
  sf_regioes <- st_zm(sf_regioes, drop = TRUE, what = "ZM")
  message("✅ Objeto sf convertido para 2D (coordenadas Z removidas) in-place.")
  
  # Salva o objeto sf (agora 2D) como um novo GeoPackage
  # O GeoPackage suporta UTF-8 e geometrias 3D nativamente.
  st_write(
    obj = sf_regioes, # Salva o objeto sf modificado (agora 2D)
    dsn = output_path,
    layer = "sf_regioes_ibge", # Nome da camada dentro do GeoPackage (pode ser qualquer nome)
    delete_layer = TRUE, # Permite sobrescrever a camada se ela já existir
    quiet = TRUE # Suprime mensagens de progresso para uma saída mais limpa
  )
  message("✅ Objeto sf salvo como GeoPackage 2D com sucesso.")
}, error = function(e) {
  stop(paste("❌ Erro ao salvar o GeoPackage: ", e$message))
})


# sf_regioes <- st_read(dsn = gpkg_file_path, layer = "sf_regioes_ibge")

# Esta parte só será executada se o salvamento local for bem-sucedido.

gpkg_s3_key <- "working/ibge/mapas/sf_regioes.gpkg" 

message(paste("Fazendo upload do GeoPackage para s3://", bucket_name, "/", gpkg_s3_key, "...", sep = ""))

tryCatch({
  put_object( # Usando put_object para upload de arquivo
    file = output_path, # Caminho do arquivo GeoPackage local (definido no Passo 6)
    object = gpkg_s3_key, # Caminho do GeoPackage no S3
    bucket = bucket_name # Nome do bucket S3 (definido no Passo 2)
  )
  message("✅ GeoPackage carregado para S3 com sucesso.")
  
}, error = function(e) {
  # Este 'stop' será acionado se houver um erro no upload para o S3
  stop(paste("❌ Erro ao fazer upload do GeoPackage para S3: ", e$message))
})


# Examine contents of sf_regioes.gpkg

# Replace with your path
gpkg_path <- "D:/Country/Brazil/TechBrazil/working/ibge/mapas/sf_regioes.gpkg"

# List available layers (just like listing tables in a database)
st_layers(gpkg_path)
# indicates currently one layer - sf_regioes_ibge

sf_regioes <- st_read(gpkg_path, layer = "sf_regioes_ibge")

# That is an sf object and a data.frame
class(sf_regioes)

# Bounding box:  xmin: -50.08704 ymin: -17.89904 xmax: -46.8737 ymax: -14.67194
# Geodetic CRS:  SIRGAS 2000









