# Avila_Vidal01.R 

library(openxlsx)
library(stringr)  
library(stringi)
library(dplyr)
library(tibble)


# Define path and sheet names
file_path <- "D:/Country/Brazil/TechBrazil/rawdata/fgv/fgv_fin2.xlsx"
sheet_names <- getSheetNames(file_path)

# Define your desired names
target_names <- c("df_2a", "df_2b", "df_2c", "df_3a", "df_3b", "df_3c", "df_4a", "df_4b", "df_nd")

# Read sheets and assign with desired names
df_list <- lapply(seq_along(target_names), function(i) {
  df <- read.xlsx(file_path, sheet = sheet_names[i])
  assign(target_names[i], df, envir = .GlobalEnv)
  df
})

# Set names of the list as well
names(df_2a) <- c("NM_UF", "DIVIDA", "Distr_FEF", "Valor_Abat",sprintf("Saldo%04d", 2025:2054),
                           sprintf("ApoFEF%04d", 2025:2054), sprintf("InvDir%04d", 2025:2054),
                           sprintf("JurPag%04d", 2025:2054))           

# Define your standard column names
new_names <- c(
  "NM_UF", "DIVIDA", "Distr_FEF", "Valor_Abat",
  sprintf("Saldo%04d", 2025:2054),
  sprintf("ApoFEF%04d", 2025:2054),
  sprintf("InvDir%04d", 2025:2054),
  sprintf("JurPag%04d", 2025:2054)
)

# List of your data frame names
dfs_to_rename <- c("df_2a", "df_2b", "df_2c", "df_3a", "df_3b", "df_3c", "df_4a", "df_4b","df_nd")

# Apply renaming
for (df_name in dfs_to_rename) {
  df <- get(df_name)
  if (ncol(df) == length(new_names)) {
    names(df) <- new_names
    assign(df_name, df, envir = .GlobalEnv)
  } else {
    warning(sprintf("Skipped %s: expected %d columns, found %d", df_name, length(new_names), ncol(df)))
  }
}

# List of data frames to update
dfs_to_update <- c("df_2a", "df_2b", "df_2c", "df_3a", "df_3b", "df_3c", "df_4a", "df_4b","df_nd")

# Apply title-casing to NM_UF
for (df_name in dfs_to_update) {
  df <- get(df_name)
  if ("NM_UF" %in% names(df)) {
    df <- df %>%
      mutate(NM_UF = stri_trans_totitle(NM_UF, locale = "pt"))
    assign(df_name, df, envir = .GlobalEnv)
  } else {
    warning(sprintf("Skipped %s: 'NM_UF' column not found.", df_name))
  }
}


normalize_uf_names <- function(df) {
  df %>%
    mutate(
      NM_UF = stri_trans_totitle(NM_UF, locale = "pt"),
      NM_UF = str_replace_all(NM_UF, c(
        "Mato Grosso Do Sul" = "Mato Grosso do Sul",
        "Rio Grande Do Norte" = "Rio Grande do Norte",
        "Rio Grande Do Sul" = "Rio Grande do Sul",
        "Rio De Janeiro" = "Rio de Janeiro"
      ))
    )
}


for (df_name in dfs_to_update) {
  df <- get(df_name)
  if ("NM_UF" %in% names(df)) {
    df <- normalize_uf_names(df)
    assign(df_name, df, envir = .GlobalEnv)
  } else {
    warning(sprintf("Skipped %s: 'NM_UF' column not found.", df_name))
  }
}



names(df_2a)

# [1] "NM_UF"      "DIVIDA"     "Distr_FEF"  "Valor_Abat" "Saldo2025"  "Saldo2026"  "Saldo2027"  "Saldo2028"  "Saldo2029"  "Saldo2030"  "Saldo2031"  "Saldo2032" 
# [13] "Saldo2033"  "Saldo2034"  "Saldo2035"  "Saldo2036"  "Saldo2037"  "Saldo2038"  "Saldo2039"  "Saldo2040"  "Saldo2041"  "Saldo2042"  "Saldo2043"  "Saldo2044" 
# [25] "Saldo2045"  "Saldo2046"  "Saldo2047"  "Saldo2048"  "Saldo2049"  "Saldo2050"  "Saldo2051"  "Saldo2052"  "Saldo2053"  "Saldo2054"  "ApoFEF2025" "ApoFEF2026"
# [37] "ApoFEF2027" "ApoFEF2028" "ApoFEF2029" "ApoFEF2030" "ApoFEF2031" "ApoFEF2032" "ApoFEF2033" "ApoFEF2034" "ApoFEF2035" "ApoFEF2036" "ApoFEF2037" "ApoFEF2038"
# [49] "ApoFEF2039" "ApoFEF2040" "ApoFEF2041" "ApoFEF2042" "ApoFEF2043" "ApoFEF2044" "ApoFEF2045" "ApoFEF2046" "ApoFEF2047" "ApoFEF2048" "ApoFEF2049" "ApoFEF2050"
# [61] "ApoFEF2051" "ApoFEF2052" "ApoFEF2053" "ApoFEF2054" "InvDir2025" "InvDir2026" "InvDir2027" "InvDir2028" "InvDir2029" "InvDir2030" "InvDir2031" "InvDir2032"
# [73] "InvDir2033" "InvDir2034" "InvDir2035" "InvDir2036" "InvDir2037" "InvDir2038" "InvDir2039" "InvDir2040" "InvDir2041" "InvDir2042" "InvDir2043" "InvDir2044"
# [85] "InvDir2045" "InvDir2046" "InvDir2047" "InvDir2048" "InvDir2049" "InvDir2050" "InvDir2051" "InvDir2052" "InvDir2053" "InvDir2054" "JurPag2025" "JurPag2026"
# [97] "JurPag2027" "JurPag2028" "JurPag2029" "JurPag2030" "JurPag2031" "JurPag2032" "JurPag2033" "JurPag2034" "JurPag2035" "JurPag2036" "JurPag2037" "JurPag2038"
# [109] "JurPag2039" "JurPag2040" "JurPag2041" "JurPag2042" "JurPag2043" "JurPag2044" "JurPag2045" "JurPag2046" "JurPag2047" "JurPag2048" "JurPag2049" "JurPag2050"
# [121] "JurPag2051" "JurPag2052" "JurPag2053" "JurPag2054"


# Let's save the rda data to D:/Country/Brazil/TechBrazil/working/fgv
save(df_2a, file = "D:/Country/Brazil/TechBrazil/working/fgv/df_2a.rda")
save(df_2b, file = "D:/Country/Brazil/TechBrazil/working/fgv/df_2b.rda")
save(df_2c, file = "D:/Country/Brazil/TechBrazil/working/fgv/df_2c.rda")

save(df_3a, file = "D:/Country/Brazil/TechBrazil/working/fgv/df_3a.rda")
save(df_3b, file = "D:/Country/Brazil/TechBrazil/working/fgv/df_3b.rda")
save(df_3c, file = "D:/Country/Brazil/TechBrazil/working/fgv/df_3c.rda")

save(df_4a, file = "D:/Country/Brazil/TechBrazil/working/fgv/df_4a.rda")
save(df_4b, file = "D:/Country/Brazil/TechBrazil/working/fgv/df_4b.rda")
save(df_nd, file = "D:/Country/Brazil/TechBrazil/working/fgv/df_nd.rda")

glimpse(df_2a)


# Avila_Vidal codes

## 0. Mapas código ↔ rótulo  (use os mesmos no UI!)
A.map <- c("A1"="Sem abatimento", "A2"="10% abatimento", "A3"="20% abatimento")
G.map <- c("G1"="1%", "G2"="1.5%", "G3"="2%")
I.map <- c("I1"="0%", "I2"="0.5%", "I3"="1%", "I4"="1.5%", "I5"="2%")  # <-- agora 5 opções
J.map <- c("J1"="0%", "J2"="1%", "J3"="2%", "J4"="4% (Não Adere)")

# 1. Todos os códigos “normais” (J4 fora)
df_all <- expand.grid(
  A = names(A.map),
  G = names(G.map),
  I = names(I.map),
  J = names(J.map)[1:3],   # J1..J3
  stringsAsFactors = FALSE
)

# 2. Linha especial “Não Adere” (J4 sozinho)
nd_row <- data.frame(A="ND1", G="ND1", I="ND1", J="J4", stringsAsFactors = FALSE)

# 3. Conjunto completo bruto
dfcen_val <- rbind(df_all, nd_row)




# 4. Marque as combinações realmente válidas (8 + 1)

valid_tbl <- tribble(
  ~A,  ~G,  ~I,  ~J,
  # Juros 2%
  "A2","G2","I2","J3",   # 10% amort | 0,5% ID | 1,5% FEF
  "A1","G1","I3","J3",   # 0% amort  | 1%   ID | 1%   FEF
  
  # Juros 1%
  "A2","G2","I2","J2",   # 10% amort | 0,5% ID | 1,5% FEF
  "A3","G1","I1","J2",   # 20% amort | 0%   ID | 1%   FEF
  "A1","G3","I3","J2",   # 0% amort  | 1%   ID | 2%   FEF
  
  # Juros 0%
  "A2","G2","I4","J1",   # 10% amort | 1,5% ID | 1,5% FEF
  "A3","G1","I3","J1",   # 20% amort | 1%   ID | 1%   FEF
  "A1","G3","I5","J1",   # 0% amort  | 2%   ID | 2%   FEF
  
  # Não Adere
  "ND1","ND1","ND1","J4"
)

# 5. Flag 'valid'
dfcen_val$valid <- with(dfcen_val,
                        paste(A,G,I,J) %in% paste(valid_tbl$A, valid_tbl$G, valid_tbl$I, valid_tbl$J)
) 



dfcen_val <-dfcen_val %>% arrange(desc(valid))
dfcen_val


dfcen_val[7, ]   <- NA
dfcen_val[95, ]  <- NA
dfcen_val[8, ]   <- NA
dfcen_val[106, ] <- NA


# Perform the swaps
# Assign correct values from original spreadsheet logic
dfcen_val[7, ] <- data.frame(
  A = "A2", G = "G1", I = "I1", J = "J3", valid = TRUE, stringsAsFactors = FALSE
)

dfcen_val[8, ] <- data.frame(
  A = "A1", G = "G2", I = "I2", J = "J3", valid = TRUE, stringsAsFactors = FALSE
)

dfcen_val[95, ] <- data.frame(
  A = "A2", G = "G2", I = "I2", J = "J3", valid = FALSE, stringsAsFactors = FALSE
)

dfcen_val[106, ] <- data.frame(
  A = "A1", G = "G1", I = "I3", J = "J3", valid = FALSE, stringsAsFactors = FALSE
)


opcao <- c("II-A", "II-B", "II-C","III-A","III-B","III-C", "IV-A", "IV-B","ND",rep("NA", 127))
dfcen_val$opcao <- opcao
dfcen_val




save(dfcen_val, file = "D:/Country/Brazil/TechBrazil/working/fgv/dfcen_val.rda")
write.csv(dfcen_val, file = "D:/Country/Brazil/TechBrazil/working/fgv/dfcen_val.csv", row.names = FALSE)
openxlsx:: write.xlsx(dfcen_val, file = "D:/Country/Brazil/TechBrazil/working/fgv/avila_vidal_ocpoes.xlsx", rowNames = FALSE)






dfcen_val <-dfcen_val %>% arrange(desc(valid))


















