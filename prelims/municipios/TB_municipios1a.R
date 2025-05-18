# Load required package
library(DBI)

# Define path to your SQLite database
db_path <- "D:/Country/Brazil/TechBrazil/working/sqlite/TB_municipios.sqlite"

# Connect to the database
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# List all tables in the database
dbListTables(con)

# Preview the first few rows of your PIB table
dbGetQuery(con, "SELECT * FROM PIB_dos_Municipios LIMIT 10")

# Quick summary of columns
dbListFields(con, "PIB_dos_Municipios")


df_pib <- dbReadTable(con, "PIB_dos_Municipios")

# Optional: Check year range
dbGetQuery(con, "SELECT MIN(Ano) AS min_year, MAX(Ano) AS max_year FROM PIB_dos_Municipios")

# Always disconnect when done
dbDisconnect(con)

table(df_pib$`Nome.da.Grande.Região`)




