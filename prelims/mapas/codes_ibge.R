# codes_ibge.R

# This script creates a dataframe of geographical codes for later use
# Includes all manner of aggregations 

library(dplyr)
library(aws.s3)
library(dotenv)
library(sjlabelled)

# --- Step 1: Load AWS Credentials Securely ---
# Ensure each team member has their own `.env` file in the project directory
dotenv::load_dot_env()


bucket_name <- "techbrazildata" # Nome do bucket S3
pibmunis_local_path <- "D:/Country/Brazil/TechBrazil/working/ibge/df_pibmunis.rda"
pibmunis_s3_key <- "working/ibge/df_pibmunis.rda" # Caminho do arquivo no S3


# Tenta carregar localmente; se não encontrar, baixa do S3
if (file.exists(pibmunis_local_path)) {
  message("✅ df_pibmunis.rda encontrado localmente. Carregando...")
  load(pibmunis_local_path)
} else {
  message(paste("df_pibmunis.rda não encontrado localmente. Tentando baixar de s3://", bucket_name, "/", pibmunis_s3_key, "...", sep = ""))
  tryCatch({
    # Baixa o arquivo do S3 e o carrega diretamente
    df_pibmunis <- s3read_using(
      FUN = readRDS, # Assumindo que o arquivo no S3 foi salvo com saveRDS
      object = pibmunis_s3_key,
      bucket = bucket_name
    )
    message("✅ df_pibmunis.rda baixado e carregado com sucesso do S3.")
    # Opcional: Salve uma cópia local para uso futuro
    # saveRDS(df_pibmunis, file = pibmunis_local_path)
    # message("Cópia local de df_pibmunis.rda salva.")
  }, error = function(e) {
    stop(paste("❌ Erro ao carregar df_pibmunis.rda: ", e$message,
               "\nPor favor, verifique o caminho local, as credenciais AWS e o caminho do S3."))
  })
}



# Define reasonable column names

df_codes_ibge <- df_pibmunis %>%
  rename(
    CO_5RGRANDE = `Código.da.Grande.Região`,
    NM_5RGRANDE = `Nome.da.Grande.Região`,
    
    CO_UF = `Código.da.Unidade.da.Federação`,
    SG_UF = `Sigla.da.Unidade.da.Federação`,
    NM_UF = `Nome.da.Unidade.da.Federação`,
    
    CO_MUN = `Código.do.Município`,
    NM_MUN = `Nome.do.Município`,
    
    NM_REGMET = `Região.Metropolitana`,
    
    CO_MESOREG = `Código.da.Mesorregião`,
    NM_MESOREG = `Nome.da.Mesorregião`,
    
    CO_MICROREG = `Código.da.Microrregião`,
    NM_MICROREG = `Nome.da.Microrregião`,
    
    CO_RGIMED = `Código.da.Região.Geográfica.Imediata`,
    NM_RGIMED = `Nome.da.Região.Geográfica.Imediata`,
    MUN_RGIM_EOP = `Município.da.Região.Geográfica.Imediata`,
    
    
    CO_RGINTM = `Código.da.Região.Geográfica.Intermediária`,
    NM_RGIINTM = `Nome.da.Região.Geográfica.Intermediária`,
    MUN_RGIN_EOP = `Município.da.Região.Geográfica.Intermediária`,
    
    CO_CONC_URBANA = `Código.Concentração.Urbana`,
    NM_CONC_URBANA = `Nome.Concentração.Urbana`,
    TP_GMCONC_URBANA = `Tipo.Concentração.Urbana`,
    HRQ_11URBANA = `Hierarquia.Urbana`,
    HRQ_5URBANA = `Hierarquia.Urbana..principais.categorias.`,
    
    
    CO_ARR_POP = `Código.Arranjo.Populacional`,
    NM_ARR_POP = `Nome.Arranjo.Populacional`,
    
    
    CO_REG_RURAL = `Código.da.Região.Rural`,
    NM_REG_RURAL = `Nome.da.Região.Rural`,
    CDN_6REG_RURAL = `Região.rural..segundo.classificação.do.núcleo.`,
     
    MUN_AMZ_LEG = `Amazônia.Legal`,
    MUN_SEMIARIDO = `Semiárido`,
    MUN_CIDADE_SP = `Cidade.Região.de.São.Paulo`)  


# Convert MUN_AMZ_LEG into a 0 1 variable from Sim and Não
df_codes_ibge$MUN_AMZ_LEG <- ifelse(df_codes_ibge$MUN_AMZ_LEG == "Sim", 1, 0)

# Convert MUN_SEMIARIDO into a 0 1 variable from Sim and Não
df_codes_ibge$MUN_SEMIARIDO <- ifelse(df_codes_ibge$MUN_SEMIARIDO == "Sim", 1, 0)

# Convert MUN_CIDADE_SP into a 0 1 variable from Sim and Não
df_codes_ibge$MUN_CIDADE_SP <- ifelse(df_codes_ibge$MUN_CIDADE_SP == "Sim", 1, 0)


# # Verifica o número de valores únicos de CO_MUN por ano
# unique_mun_per_year <- df_codes_ibge %>%
#   group_by(Ano) %>%
#   summarise(
#     Numero_Municipios_Unicos = n_distinct(CO_MUN)
#   ) %>%
#   arrange(Ano) # Ordena por ano para melhor visualização
# 
# # Imprime o resultado
# print("Número de municípios únicos por ano:")
# print(unique_mun_per_year)
# 
# 
# Ano Numero_Municipios_Unicos
# <dbl>                    <int>
#   1  2002                     5560
# 2  2003                     5560
# 3  2004                     5560
# 4  2005                     5564
# 5  2006                     5564
# 6  2007                     5564
# 7  2008                     5564
# 8  2009                     5565
# 9  2010                     5565
# 10  2011                     5565
# 11  2012                     5565
# 12  2013                     5570
# 13  2014                     5570
# 14  2015                     5570
# 15  2016                     5570
# 16  2017                     5570
# 17  2018                     5570
# 18  2019                     5570
# 19  2020                     5570
# 20  2021                     5570


df_codes_ibge <- df_codes_ibge %>% select(
     Ano,
     CO_5RGRANDE, NM_5RGRANDE,
     CO_UF, SG_UF, NM_UF,
     CO_MUN, NM_MUN,
     NM_REGMET,
     CO_MESOREG, NM_MESOREG,
     CO_MICROREG, NM_MICROREG,
     CO_RGIMED, NM_RGIMED, MUN_RGIM_EOP,
     CO_RGINTM, NM_RGIINTM, MUN_RGIN_EOP,
     CO_CONC_URBANA, NM_CONC_URBANA, TP_GMCONC_URBANA,
     HRQ_11URBANA, HRQ_5URBANA,
     CO_ARR_POP, NM_ARR_POP,
     CO_REG_RURAL, NM_REG_RURAL,CDN_6REG_RURAL, 
     MUN_AMZ_LEG, MUN_SEMIARIDO, MUN_CIDADE_SP
   ) %>%  distinct()


df_codes_ibge <- df_codes_ibge %>%
  set_label(list(
  Ano = "Ano de referência do PIB Municipal",
  
  CO_5RGRANDE = "Código da Grande Região",
  NM_5RGRANDE = "Nome da Grande Região",
  
  CO_UF = "Código da Unidade da Federação",
  SG_UF = "Sigla da Unidade da Federação",
  NM_UF = "Nome da Unidade da Federação",
  
  CO_MUN = "Código do Município (IBGE)",
  NM_MUN = "Nome do Município",
  
  NM_REGMET = "Nome da Região Metropolitana",
  
  CO_MESOREG = "Código da Mesorregião",
  NM_MESOREG = "Nome da Mesorregião",
  
  CO_MICROREG = "Código da Microrregião",
  NM_MICROREG = "Nome da Microrregião",
  
  CO_RGIMED = "Código da Região Geográfica Imediata",
  NM_RGIMED = "Nome da Região Geográfica Imediata",
  MUN_RGIM_EOP = "Município da Região Geográfica Imediata Entorno ou Polo ",
  
  CO_RGINTM = "Código da Região Geográfica Intermediária",
  NM_RGIINTM = "Nome da Região Geográfica Intermediária",
  MUN_RGIN_EOP = "Município da Região Geográfica Intermediária  do Entourno ou Polo",
  
  CO_CONC_URBANA = "Código da Concentração Urbana",
  NM_CONC_URBANA = "Nome da Concentração Urbana",
  TP_GMCONC_URBANA = "Tipo de Concentração Urbana",
  
  HRQ_11URBANA = "Hierarquia Urbana (11 categorias)",
  HRQ_5URBANA = "Hierarquia Urbana (5 categorias principais)",
  
  CO_ARR_POP = "Código do Arranjo Populacional",
  NM_ARR_POP = "Nome do Arranjo Populacional",
  
  CO_REG_RURAL = "Código da Região Rural",
  NM_REG_RURAL = "Nome da Região Rural",
  CDN_6REG_RURAL = "Classificação do Núcleo Rural (6 categorias)",
  
  MUN_AMZ_LEG = "Pertence à Amazônia Legal (0=Não, 1=Sim)",
  MUN_SEMIARIDO = "Pertence ao Semiárido (0=Não, 1=Sim)",
  MUN_CIDADE_SP = "Pertence à Cidade-Região de São Paulo (0=Não, 1=Sim)"
))



# Save the file
save(df_codes_ibge, file = "working/ibge/df_codes_ibge.rda")

# Upload to Amazon S3

tryCatch({
  put_object(file = "working/ibge/df_codes_ibge.rda", object = "working/ibge/df_codes_ibge.rda",
             bucket = bucket_name)
  message("✅ Uploaded to S3: working/ibge/df_codes_ibge.rda")
}, error = function(e) {
  message("❌ Error uploading to S3: ", e$message)
})


get_label(df_codes_ibge)


