#-------------------------------------------------------------------------------
# 		TVET EDUCATION TECHNICAL NOTE 
#-------------------------------------------------------------------------------
#Title: 			    	TVET in Brazil
#Author:			    	Isabella Meyer, World Bank (imirandameyer@worldbank.org)
#Last modification:	Feb 2025 by Isabella Meyer
#Date of creation:	Feb 5th 2025
#
#  
#Description: 			
#
#   OBJECTIVE:    	To pull datasets from Base dos Dados: https://basedosdados.org/
#    
#   PRODUCES:		    Datasets in dta
#    
#   STRUCTURE:		1. PNAD Contínua
#                 2. RAIS (estabelecimentos)
#                 3. RAIS (vínculos)
#                 
#   
#   HOW TO USE IT:	This do-file pulls data from Base dos dados website. To do it
#                   we use querys, so you will need a project billing id from Google
#                   cloud. 
#					
#-------------------------------------------------------------------------------


#-----------------------------------------
#     0. SETTINGS
#-----------------------------------------

# You'll need tidyverse and basedosdados packages. If you do not have them installed,
# install them by using "install.packages("tudyverse")" and 
# "install.packages("basedosdados")" commands
library(tidyverse)
library(basedosdados)

# Set project billing id
set_billing_id("techbrasil")

# state code:
code <- "'11'"

#-----------------------------------------
#     1. PNAD CONTÍNUA
#-----------------------------------------

#query : 
query <- paste0("SELECT ano,trimestre,capital,V2001,VD4019,VD4020,V1027,
V1028,
  FROM `basedosdados.br_ibge_pnadc.microdados`
  WHERE id_uf=", code)

# read database
data <- read_sql(query)

# save data
save(data, "/pnad_cleaned")


#-----------------------------------------
#     2. RAIS (ESTABELECIMENTOS)
#-----------------------------------------

#query : 
query <- paste0("
WITH 
dicionario_natureza_estabelecimento AS (
    SELECT
        chave AS chave_natureza_estabelecimento,
        valor AS descricao_natureza_estabelecimento
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'natureza_estabelecimento'
        AND id_tabela = 'microdados_estabelecimentos'
),
dicionario_tamanho_estabelecimento AS (
    SELECT
        chave AS chave_tamanho_estabelecimento,
        valor AS descricao_tamanho_estabelecimento
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'tamanho_estabelecimento'
        AND id_tabela = 'microdados_estabelecimentos'
),
dicionario_tipo_estabelecimento AS (
    SELECT
        chave AS chave_tipo_estabelecimento,
        valor AS descricao_tipo_estabelecimento
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'tipo_estabelecimento'
        AND id_tabela = 'microdados_estabelecimentos'
),
dicionario_subsetor_ibge AS (
    SELECT
        chave AS chave_subsetor_ibge,
        valor AS descricao_subsetor_ibge
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'subsetor_ibge'
        AND id_tabela = 'microdados_estabelecimentos'
)
SELECT
    *
FROM `basedosdados.br_me_rais.microdados_estabelecimentos` AS dados
WHERE SUBSTR(`id_municipio`, 1, 2)=", 
code)

# read database
data <- read_sql(query)

# save data
save(data, "/rais_estabelecimentos_cleaned")

#-----------------------------------------
#     3. RAIS (VÍNCULOS)
#-----------------------------------------

#query : 
query <- paste0("WITH 
dicionario_tipo_admissao AS (
    SELECT
        chave AS chave_tipo_admissao,
        valor AS descricao_tipo_admissao
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'tipo_admissao'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_motivo_desligamento AS (
    SELECT
        chave AS chave_motivo_desligamento,
        valor AS descricao_motivo_desligamento
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'motivo_desligamento'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_faixa_horas_contratadas AS (
    SELECT
        chave AS chave_faixa_horas_contratadas,
        valor AS descricao_faixa_horas_contratadas
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'faixa_horas_contratadas'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_indicador_cei_vinculado AS (
    SELECT
        chave AS chave_indicador_cei_vinculado,
        valor AS descricao_indicador_cei_vinculado
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'indicador_cei_vinculado'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_indicador_trabalho_parcial AS (
    SELECT
        chave AS chave_indicador_trabalho_parcial,
        valor AS descricao_indicador_trabalho_parcial
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'indicador_trabalho_parcial'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_indicador_trabalho_intermitente AS (
    SELECT
        chave AS chave_indicador_trabalho_intermitente,
        valor AS descricao_indicador_trabalho_intermitente
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'indicador_trabalho_intermitente'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_faixa_remuneracao_media_sm AS (
    SELECT
        chave AS chave_faixa_remuneracao_media_sm,
        valor AS descricao_faixa_remuneracao_media_sm
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'faixa_remuneracao_media_sm'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_tipo_salario AS (
    SELECT
        chave AS chave_tipo_salario,
        valor AS descricao_tipo_salario
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'tipo_salario'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_faixa_etaria AS (
    SELECT
        chave AS chave_faixa_etaria,
        valor AS descricao_faixa_etaria
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'faixa_etaria'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_grau_instrucao_1985_2005 AS (
    SELECT
        chave AS chave_grau_instrucao_1985_2005,
        valor AS descricao_grau_instrucao_1985_2005
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'grau_instrucao_1985_2005'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_grau_instrucao_apos_2005 AS (
    SELECT
        chave AS chave_grau_instrucao_apos_2005,
        valor AS descricao_grau_instrucao_apos_2005
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'grau_instrucao_apos_2005'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_nacionalidade AS (
    SELECT
        chave AS chave_nacionalidade,
        valor AS descricao_nacionalidade
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'nacionalidade'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_sexo AS (
    SELECT
        chave AS chave_sexo,
        valor AS descricao_sexo
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'sexo'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_raca_cor AS (
    SELECT
        chave AS chave_raca_cor,
        valor AS descricao_raca_cor
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'raca_cor'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_tamanho_estabelecimento AS (
    SELECT
        chave AS chave_tamanho_estabelecimento,
        valor AS descricao_tamanho_estabelecimento
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'tamanho_estabelecimento'
        AND id_tabela = 'microdados_vinculos'
),
dicionario_tipo_estabelecimento AS (
    SELECT
        chave AS chave_tipo_estabelecimento,
        valor AS descricao_tipo_estabelecimento
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE
        TRUE
        AND nome_coluna = 'tipo_estabelecimento'
        AND id_tabela = 'microdados_vinculos'
)
SELECT *
  FROM `basedosdados.br_me_rais.microdados_vinculos`
  WHERE SUBSTR(`id_municipio`, 1, 2)=", code)

# read database
data <- read_sql(query)

# save data
save(data, "/rais_vinculos_cleaned")