#censo_escolar_ensino_medio2324.R
library(openxlsx)
library(dplyr)
library(stringr)

# 2019


# 2019
# Define path
file_path <- "D:/Country/Brazil/TechBrazil/rawdata/fundeb/EMR_2019.xlsx"

# Read skipping first 13 rows (i.e., start at line 14)
df_EM2019 <- read.xlsx(
  xlsxFile = file_path,
  startRow = 14,
  colNames = FALSE
)

# Check number of columns
ncol(df_EM2019)

# Assign clean column names for first 25 columns only (if extra columns exist, they'll be unnamed)
colnames(df_EM2019)[1:25] <- c(
  "regiao", "uf", "municipio", "codigo_municipio", 
  "total_EMR", 
  "1serie_total_EMR", "1serie_federal_EMR", "1serie_estadual_EMR", "1serie_municipal_EMR", "1serie_privada_EMR", 
  "2serie_total_EMR", "2serie_federal_EMR", "2serie_estadual_EMR", "2serie_municipal_EMR", "2serie_privada_EMR",
  "3serie_total_EMR", "3serie_federal_EMR", "3serie_estadual_EMR", "3serie_municipal_EMR", "3serie_privada_EMR",
  "ns_total_EMR", "ns_federal_EMR", "ns_estadual_EMR", "ns_municipal_EMR", "ns_privada_EMR"
)

# Filter out rows with missing/blank municipio values
df_EM2019 <- df_EM2019 %>%
  filter(!is.na(municipio)) %>%
  filter(str_trim(municipio) != "")

df_EM2019 <- df_EM2019[,1:25]

# File path
file_path_ept <- "D:/Country/Brazil/TechBrazil/rawdata/fundeb/EPT_2019.xlsx"

# Read from line 14, no column names
df_EPT2019 <- read.xlsx(
  xlsxFile = file_path_ept,
  startRow = 14,
  colNames = FALSE
)


# Assign the first 4 identifiers + placeholder names for all 9 x 5 = 45 columns of EPT breakdown
colnames(df_EPT2019)[1:45] <- c(
  "regiao", "uf", "municipio", "codigo_municipio",
  "total_EPTsnormal",
  
  # Each group has: Total, Federal, Estadual, Municipal, Privada
  paste0("tecnico_integrado_",       c("total", "federal", "estadual", "municipal", "privada")),
  paste0("medio_normal_",            c("total", "federal", "estadual", "municipal", "privada")),
  paste0("tecnico_concomitante_",    c("total", "federal", "estadual", "municipal", "privada")),
  paste0("tecnico_subsequente_",     c("total", "federal", "estadual", "municipal", "privada")),
  paste0("tecnico_eja_integrado_",   c("total", "federal", "estadual", "municipal", "privada")),
  paste0("fic_concomitante_",        c("total", "federal", "estadual", "municipal", "privada")),
  paste0("fic_eja_fundamental_",     c("total", "federal", "estadual", "municipal", "privada")),
  paste0("fic_eja_medio_",           c("total", "federal", "estadual", "municipal", "privada"))
)

# Clean out blank municipio rows
df_EPT2019 <- df_EPT2019 %>%
  filter(!is.na(municipio)) %>%
  filter(str_trim(municipio) != "")



# Now for the aggregates



# Ensure uf field is trimmed for both dataframes
df_EM2019 <- df_EM2019 %>% mutate(uf = str_trim(uf))
df_EPT2019 <- df_EPT2019 %>% mutate(uf = str_trim(uf))

# Summarize EPT and EMR_denom for all UFs
df_ept19 <- df_EM2019 %>%
  group_by(uf) %>%
  summarise(total_EMR19 = sum(total_EMR, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    df_EPT2019 %>%
      group_by(uf) %>%
      summarise(
        total_EPT19 = sum(tecnico_integrado_total + 
                            tecnico_concomitante_total + 
                            tecnico_subsequente_total, na.rm = TRUE),
        total_subconeja19 = sum(tecnico_concomitante_total +
                                  tecnico_subsequente_total +
                                  tecnico_eja_integrado_total +
                                  fic_concomitante_total +
                                  fic_eja_fundamental_total, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "uf"
  ) %>%
  mutate(EMR_denom19 = total_EMR19 - total_subconeja19) %>%
  select(uf, total_EPT19, EMR_denom19)








########
#########



# 2023
# Define path
file_path <- "D:/Country/Brazil/TechBrazil/rawdata/fundeb/EMR_2023.xlsx"

# Read skipping first 13 rows (i.e., start at line 14)
df_EM2023 <- read.xlsx(
  xlsxFile = file_path,
  startRow = 14,
  colNames = FALSE
)

# Check number of columns
ncol(df_EM2023)

# Assign clean column names for first 25 columns only (if extra columns exist, they'll be unnamed)
colnames(df_EM2023)[1:25] <- c(
  "regiao", "uf", "municipio", "codigo_municipio", 
  "total_EMR", 
  "1serie_total_EMR", "1serie_federal_EMR", "1serie_estadual_EMR", "1serie_municipal_EMR", "1serie_privada_EMR", 
  "2serie_total_EMR", "2serie_federal_EMR", "2serie_estadual_EMR", "2serie_municipal_EMR", "2serie_privada_EMR",
  "3serie_total_EMR", "3serie_federal_EMR", "3serie_estadual_EMR", "3serie_municipal_EMR", "3serie_privada_EMR",
  "ns_total_EMR", "ns_federal_EMR", "ns_estadual_EMR", "ns_municipal_EMR", "ns_privada_EMR"
)

# Filter out rows with missing/blank municipio values
df_EM2023 <- df_EM2023 %>%
  filter(!is.na(municipio)) %>%
  filter(str_trim(municipio) != "")

df_EM2023 <- df_EM2023[,1:25]

# File path
file_path_ept <- "D:/Country/Brazil/TechBrazil/rawdata/fundeb/EPT_2023.xlsx"

# Read from line 14, no column names
df_EPT2023 <- read.xlsx(
  xlsxFile = file_path_ept,
  startRow = 14,
  colNames = FALSE
)


# Assign the first 4 identifiers + placeholder names for all 9 x 5 = 45 columns of EPT breakdown
colnames(df_EPT2023)[1:45] <- c(
  "regiao", "uf", "municipio", "codigo_municipio",
  "total_EPTsnormal",
  
  # Each group has: Total, Federal, Estadual, Municipal, Privada
  paste0("tecnico_integrado_",       c("total", "federal", "estadual", "municipal", "privada")),
  paste0("medio_normal_",            c("total", "federal", "estadual", "municipal", "privada")),
  paste0("tecnico_concomitante_",    c("total", "federal", "estadual", "municipal", "privada")),
  paste0("tecnico_subsequente_",     c("total", "federal", "estadual", "municipal", "privada")),
  paste0("tecnico_eja_integrado_",   c("total", "federal", "estadual", "municipal", "privada")),
  paste0("fic_concomitante_",        c("total", "federal", "estadual", "municipal", "privada")),
  paste0("fic_eja_fundamental_",     c("total", "federal", "estadual", "municipal", "privada")),
  paste0("fic_eja_medio_",           c("total", "federal", "estadual", "municipal", "privada"))
)

# Clean out blank municipio rows
df_EPT2023 <- df_EPT2023 %>%
  filter(!is.na(municipio)) %>%
  filter(str_trim(municipio) != "")



# Now for the aggregates



# Ensure uf field is trimmed for both dataframes
df_EM2023 <- df_EM2023 %>% mutate(uf = str_trim(uf))
df_EPT2023 <- df_EPT2023 %>% mutate(uf = str_trim(uf))

# Summarize EPT and EMR_denom for all UFs
df_ept23 <- df_EM2023 %>%
  group_by(uf) %>%
  summarise(total_EMR23 = sum(total_EMR, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    df_EPT2023 %>%
      group_by(uf) %>%
      summarise(
        total_EPT23 = sum(tecnico_integrado_total + 
                          tecnico_concomitante_total + 
                          tecnico_subsequente_total, na.rm = TRUE),
        total_subconeja23 = sum(tecnico_concomitante_total +
                                tecnico_subsequente_total +
                                tecnico_eja_integrado_total +
                                fic_concomitante_total +
                                fic_eja_fundamental_total, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "uf"
  ) %>%
  mutate(EMR_denom23 = total_EMR23 - total_subconeja23) %>%
  select(uf, total_EPT23, EMR_denom23)

# now for 2024
# 2024
# Define path
file_path <- "D:/Country/Brazil/TechBrazil/rawdata/fundeb/EMR_2024.xlsx"

# Read skipping first 13 rows (i.e., start at line 14)
df_EM2024 <- read.xlsx(
  xlsxFile = file_path,
  startRow = 14,
  colNames = FALSE
)

# Check number of columns
ncol(df_EM2024)

# Assign clean column names for first 25 columns only (if extra columns exist, they'll be unnamed)
colnames(df_EM2024)[1:25] <- c(
  "regiao", "uf", "municipio", "codigo_municipio", 
  "total_EMR", 
  "1serie_total_EMR", "1serie_federal_EMR", "1serie_estadual_EMR", "1serie_municipal_EMR", "1serie_privada_EMR", 
  "2serie_total_EMR", "2serie_federal_EMR", "2serie_estadual_EMR", "2serie_municipal_EMR", "2serie_privada_EMR",
  "3serie_total_EMR", "3serie_federal_EMR", "3serie_estadual_EMR", "3serie_municipal_EMR", "3serie_privada_EMR",
  "ns_total_EMR", "ns_federal_EMR", "ns_estadual_EMR", "ns_municipal_EMR", "ns_privada_EMR"
)

# Filter out rows with missing/blank municipio values
df_EM2024 <- df_EM2024 %>%
  filter(!is.na(municipio)) %>%
  filter(str_trim(municipio) != "")

df_EM2024 <- df_EM2024[,1:25]

# File path
file_path_ept <- "D:/Country/Brazil/TechBrazil/rawdata/fundeb/EPT_2024.xlsx"

# Read from line 14, no column names
df_EPT2024 <- read.xlsx(
  xlsxFile = file_path_ept,
  startRow = 14,
  colNames = FALSE
)


# Assign the first 4 identifiers + placeholder names for all 9 x 5 = 45 columns of EPT breakdown
colnames(df_EPT2024)[1:45] <- c(
  "regiao", "uf", "municipio", "codigo_municipio",
  "total_EPTsnormal",
  
  # Each group has: Total, Federal, Estadual, Municipal, Privada
  paste0("tecnico_integrado_",       c("total", "federal", "estadual", "municipal", "privada")),
  paste0("medio_normal_",            c("total", "federal", "estadual", "municipal", "privada")),
  paste0("tecnico_concomitante_",    c("total", "federal", "estadual", "municipal", "privada")),
  paste0("tecnico_subsequente_",     c("total", "federal", "estadual", "municipal", "privada")),
  paste0("tecnico_eja_integrado_",   c("total", "federal", "estadual", "municipal", "privada")),
  paste0("fic_concomitante_",        c("total", "federal", "estadual", "municipal", "privada")),
  paste0("fic_eja_fundamental_",     c("total", "federal", "estadual", "municipal", "privada")),
  paste0("fic_eja_medio_",           c("total", "federal", "estadual", "municipal", "privada"))
)

# Clean out blank municipio rows
df_EPT2024 <- df_EPT2024 %>%
  filter(!is.na(municipio)) %>%
  filter(str_trim(municipio) != "")



# Now for the aggregates



# Ensure uf field is trimmed for both dataframes
df_EM2024 <- df_EM2024 %>% mutate(uf = str_trim(uf))
df_EPT2024 <- df_EPT2024 %>% mutate(uf = str_trim(uf))

# Summarize EPT and EMR_denom for all UFs
df_ept24 <- df_EM2024 %>%
  group_by(uf) %>%
  summarise(total_EMR24 = sum(total_EMR, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    df_EPT2024 %>%
      group_by(uf) %>%
      summarise(
        total_EPT24 = sum(tecnico_integrado_total + 
                            tecnico_concomitante_total + 
                            tecnico_subsequente_total, na.rm = TRUE),
        total_subconeja24 = sum(tecnico_concomitante_total +
                                  tecnico_subsequente_total +
                                  tecnico_eja_integrado_total +
                                  fic_concomitante_total +
                                  fic_eja_fundamental_total, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "uf"
  ) %>%
  mutate(EMR_denom24 = total_EMR24 - total_subconeja24) %>%
  select(uf, total_EPT24, EMR_denom24)



##
# Public data for fundeb computations
compute_emrfun_by_uf <- function(df_em) {
  df_em %>%
    mutate(uf = str_trim(uf)) %>%
    filter(!is.na(municipio), str_trim(municipio) != "") %>%
    mutate(
      total_EMRFUN = rowSums(
        select(., 
               `1serie_estadual_EMR`, `1serie_municipal_EMR`,
               `2serie_estadual_EMR`, `2serie_municipal_EMR`,
               `3serie_estadual_EMR`, `3serie_municipal_EMR`,
               `ns_estadual_EMR`,     `ns_municipal_EMR`
        ),
        na.rm = TRUE
      )
    ) %>%
    group_by(uf) %>%
    summarise(total_EMRFUN = sum(total_EMRFUN, na.rm = TRUE), .groups = "drop")
}

df_emrfun19 <- compute_emrfun_by_uf(df_EM2019) %>% rename(total_EMRFUN19 = total_EMRFUN)
df_emrfun23 <- compute_emrfun_by_uf(df_EM2023) %>% rename(total_EMRFUN23 = total_EMRFUN)
df_emrfun24 <- compute_emrfun_by_uf(df_EM2024) %>% rename(total_EMRFUN24 = total_EMRFUN)

df_emrfun_all <- df_emrfun19 %>%
  full_join(df_emrfun23, by = "uf") %>%
  full_join(df_emrfun24, by = "uf")


# Now usee fundeb figures from fundeb1a.R
# STEP 1: Filter to state-level rows (ibge with 2 digits)
df_estados_teste <- df_teste %>%
  filter(nchar(ibge) == 2)

# STEP 2: Compute per-weighted-matricula R$ values
df_valor_aluno_estado <- df_estados_teste %>%
  mutate(
    valor_vaaf = recursos_vaaf_final / matriculas_vaaf,
    valor_vaat = recursos_vaat_final / matriculas_vaat,
    valor_fundeb_por_matricula = valor_vaaf + valor_vaat
  ) %>%
  select(uf, valor_fundeb_por_matricula, recursos_fundeb)


# Ensure 'uf' in df_eptcenso is trimmed and matches

# State code-to-name mapping (simplified example)
ufs_lookup <- tibble::tibble(
  uf_code = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT",
              "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  uf_name = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará", "Distrito Federal",
              "Espírito Santo", "Goiás", "Maranhão", "Minas Gerais", "Mato Grosso do Sul", "Mato Grosso",
              "Pará", "Paraíba", "Pernambuco", "Piauí", "Paraná", "Rio de Janeiro", "Rio Grande do Norte",
              "Rondônia", "Roraima", "Rio Grande do Sul", "Santa Catarina", "Sergipe", "São Paulo", "Tocantins")
)

# Step 1: Join state code names to df_valor_aluno_estado
df_valor_aluno_estado <- df_valor_aluno_estado %>%
  left_join(ufs_lookup, by = c("uf" = "uf_code"))


# Merge df_ept19 into df_eptcenso
df_eptcenso <- df_ept23 %>%
  left_join(df_ept24, by = "uf") %>%
  left_join(df_ept19, by = "uf")


# Now safely merge into df_eptcenso after it is created
df_eptcenso <- df_eptcenso %>%
  left_join(df_emrfun_all, by = "uf")



# Step 2: Now join by full state name
df_eptcenso <- df_eptcenso %>%
  mutate(uf = str_trim(uf)) %>%
  left_join(
    df_valor_aluno_estado %>% select(uf_name, valor_fundeb_por_matricula, recursos_fundeb),
    by = c("uf" = "uf_name")
  ) %>%
  mutate(
    recursosFUNEMR = total_EMRFUN24 * 1.25 * valor_fundeb_por_matricula,
    recursosFUNEPT = total_EPT24     * 1.30 * valor_fundeb_por_matricula
  )




df_eptcenso <- df_eptcenso %>%
  mutate(
    # Change from 2023 to 2024
    cr2324 = pmax(total_EPT24 - total_EPT23, 0),
    vez_cr2324 = ifelse(EMR_denom24 > 0,
                        floor((0.5 * EMR_denom24) / cr2324),
                        NA),
    
    # Change from 2019 to 2024
    cr1924 = pmax(total_EPT24 - total_EPT19, 0),
    vez_cr1924 = ifelse(EMR_denom24 > 0,
                        floor((0.5 * EMR_denom24) / cr1924),
                        NA)
  )
# Save the final dataframe to an RDS file

saveRDS(df_eptcenso, "D:/Country/Brazil/TechBrazil/working/mec/df_eptcenso.rds")

# To load, use

df_eptcenso <- readRDS("D:/Country/Brazil/TechBrazil/working/mec/df_eptcenso.rds")
