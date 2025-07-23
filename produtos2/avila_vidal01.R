# Avila_Vidal01.R 

library(openxlsx)
library(stringi)


# Define path and sheet names
file_path <- "D:/Country/Brazil/TechBrazil/rawdata/fgv/fgv_fin.xlsx"
sheet_names <- getSheetNames(file_path)

# Define your desired names
target_names <- c("df_2a", "df_2b", "df_2c", "df_3a", "df_3b", "df_3c", "df_4a", "df_4b")

# Read sheets and assign with desired names
df_list <- lapply(seq_along(target_names), function(i) {
  df <- read.xlsx(file_path, sheet = sheet_names[i])
  assign(target_names[i], df, envir = .GlobalEnv)
  df
})

# Set names of the list as well
names(df_2a) <- c("NM_UF", "DIVIDA", "Distr_FEF", "Valor_Abat",sprintf("Saldo%04d", 2025:2054),
                           sprintf("ApoFEF%04d", 2025:2054), sprintf("InvDir2%04d", 2025:2054))

# Define your standard column names
new_names <- c(
  "NM_UF", "DIVIDA", "Distr_FEF", "Valor_Abat",
  sprintf("Saldo%04d", 2025:2054),
  sprintf("ApoFEF%04d", 2025:2054),
  sprintf("InvDir2%04d", 2025:2054)
)

# List of your data frame names
dfs_to_rename <- c("df_2a", "df_2b", "df_2c", "df_3a", "df_3b", "df_3c", "df_4a", "df_4b")

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
dfs_to_update <- c("df_2a", "df_2b", "df_2c", "df_3a", "df_3b", "df_3c", "df_4a", "df_4b")

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

