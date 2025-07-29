rais %>% filter(ano==2024 & cbo_2002=="322230") %>% summarise(vinculos=sum(total_vinculo_ativo_3112))

table(rais$ano)

library(data.table)

# Load extracted data from Norte
df <- fread("D:/Country/Brazil/TechBrazil/rawdata/rais/mintraemp_download/txt_extracted/2024/RAIS_VINC_PUB_NI.txt",
            sep = ";", encoding = "Latin-1", showProgress = TRUE)

# Normalize column names (lowercase + underscores)
names(df) <- tolower(gsub("[^a-zA-Z0-9]", "_", names(df)))



# Confirm the variable exists
stopifnot("v_nculo_ativo_31_12" %in% names(df))

table(df$vinculo_ativo_3112)

# Compute total active vínculos
total_ativos_norte <- sum(df$v_nculo_ativo_31_12 == 1, na.rm = TRUE)

table(df$natureza_jur_dica, useNA = "always")


##

