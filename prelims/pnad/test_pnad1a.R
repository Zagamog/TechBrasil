# test_pnad1a.R

# You'll need tidyverse and basedosdados packages. If you do not have them installed,
# install them by using "install.packages("tidyverse")" and 
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

query <- paste0("SELECT ano,trimestre,capital,V2001,VD4019,VD4020,V1027,
                V1028,
                FROM `basedosdados.br_ibge_pnadc.microdados`
                WHERE id_uf=", code)

# read database
data <- read_sql(query)

sum(!is.na(data$V1028))
# 462,951 in run of April 14, 2025
sum(is.na(data$V1028))
# 0 in run of April 14, 2025
