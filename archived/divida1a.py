
import pandas as pd

# Raw data extracted manually from the image and structured as (Estado, Saldo Devedor)
data = [
    ["SÃO PAULO", "291.684.192.718,19"],
    ["RIO DE JANEIRO", "178.485.878.129,97"],
    ["MINAS GERAIS", "164.072.322.152,05"],
    ["RIO GRANDE DO SUL", "101.642.375.981,12"],
    ["GOIÁS", "19.039.529.108,97"],
    ["PARANÁ", "12.512.559.235,68"],
    ["SANTA CATARINA", "11.428.037.582,88"],
    ["ALAGOAS", "8.990.378.025,69"],
    ["MATO GROSSO DO SUL", "7.355.125.617,76"],
    ["BAHIA", "5.808.094.633,51"],
    ["PERNAMBUCO", "4.295.502.477,28"],
    ["RONDÔNIA", "2.867.331.838,13"],
    ["MARANHÃO", "1.938.409.232,89"],
    ["ESPÍRITO SANTO", "1.691.077.107,86"],
    ["CEARÁ", "1.236.595.874,28"],
    ["SERGIPE", "1.201.372.532,28"],
    ["PARÁ", "1.198.518.957,35"],
    ["PARAÍBA", "963.096.161,66"],
    ["DISTRITO FEDERAL", "852.998.835,17"],
    ["MATO GROSSO", "754.141.024,33"],
    ["RIO GRANDE DO NORTE", "667.008.481,56"],
    ["AMAPÁ", "520.847.287,56"],
    ["PIAUÍ", "500.796.641,13"],
    ["ACRE", "426.996.338,22"],
    ["AMAZONAS", "272.634.327,20"],
    ["RORAIMA", "41.205.843,07"]
]

# Create DataFrame
df = pd.DataFrame(data, columns=["Estado", "Saldo_Devedor_BR"])

# Convert Brazilian format (1.234.567,89) to float (1234567.89)
df["Saldo_Devedor"] = df["Saldo_Devedor_BR"].str.replace(".", "", regex=False).str.replace(",", ".", regex=False).astype(float)

# Format the float column without scientific notation
df["Saldo_Devedor"] = df["Saldo_Devedor"].map("{:,.2f}".format)

# Add Tocantins with 0 debt
df.loc[len(df.index)] = ["TOCANTINS", "0,00", "0.00"]

# Manually transcribed total debt by state from the new image (July 2024)
debt_july24 = [
    ["AC", "412.817.174"],
    ["AL", "8.396.922.777"],
    ["AM", "342.093.742"],
    ["AP", "504.209.054"],
    ["BA", "5.530.980.342"],
    ["CE", "1.177.807.221"],
    ["DF", "988.954.368"],
    ["ES", "1.603.832.362"],
    ["GO", "16.887.724.651"],
    ["MA", "1.118.700.859"],
    ["MG", "142.615.023.561"],
    ["MS", "6.996.204.395"],
    ["MT", "1.041.778.159"],
    ["PA", "1.140.531.490"],
    ["PB", "916.499.062"],
    ["PE", "3.821.467.155"],
    ["PI", "0"],
    ["PR", "11.907.169.047"],
    ["RJ", "156.796.832.309"],
    ["RN", "660.219.339"],
    ["RO", "2.738.548.896"],
    ["RR", "51.451.426"],
    ["RS", "92.871.280.232"],
    ["SC", "10.875.119.375"],
    ["SE", "1.144.052.960"],
    ["SP", "277.625.902.004"],
    ["TO", "0"]
]

# Create DataFrame and convert to numeric
df_july24 = pd.DataFrame(debt_july24, columns=["UF", "Saldo_julho24_BR"])
df_july24["Saldo_julho24"] = df_july24["Saldo_julho24_BR"].str.replace(".", "", regex=False).str.replace(",", ".", regex=False).astype(float)


# Format the numeric column to display with commas and no scientific notation
df_july24["Saldo_julho24"] = df_july24["Saldo_julho24"].map("{:,.2f}".format)

# First, ensure both dataframes are available and create a 2-letter UF code mapping for the first dataframe
uf_map = {
    "ACRE": "AC", "ALAGOAS": "AL", "AMAPÁ": "AP", "AMAZONAS": "AM", "BAHIA": "BA",
    "CEARÁ": "CE", "DISTRITO FEDERAL": "DF", "ESPÍRITO SANTO": "ES", "GOIÁS": "GO",
    "MARANHÃO": "MA", "MATO GROSSO": "MT", "MATO GROSSO DO SUL": "MS", "MINAS GERAIS": "MG",
    "PARÁ": "PA", "PARAÍBA": "PB", "PARANÁ": "PR", "PERNAMBUCO": "PE", "PIAUÍ": "PI",
    "RIO DE JANEIRO": "RJ", "RIO GRANDE DO NORTE": "RN", "RIO GRANDE DO SUL": "RS",
    "RONDÔNIA": "RO", "RORAIMA": "RR", "SANTA CATARINA": "SC", "SÃO PAULO": "SP",
    "SERGIPE": "SE", "TOCANTINS": "TO"
}

# Add UF code to the first dataframe
df["UF"] = df["Estado"].map(uf_map)

# Convert Saldo_Devedor column back to float for proper merging
df["saldo_mar25"] = df["Saldo_Devedor"].str.replace(",", "", regex=False).astype(float)

# Merge with July 2024 dataframe on UF
merged_df = pd.merge(df[["UF", "saldo_mar25"]], df_july24[["UF", "Saldo_julho24"]], on="UF", how="outer")

# Format both monetary columns with commas and two decimal places (no scientific notation)
merged_df["saldo_mar25"] = merged_df["saldo_mar25"].map("{:,.2f}".format)



# Convert saldo_mar25 and Saldo_julho24 back to float (remove formatting)
merged_df["saldo_mar25"] = merged_df["saldo_mar25"].str.replace(",", "", regex=False).astype(float)
merged_df["Saldo_julho24"] = merged_df["Saldo_julho24"].str.replace(",", "", regex=False).astype(float)

# Calculate the percentage of July 2024 compared to March 2025, avoiding division by zero
merged_df["perc_jul24_vs_mar25"] = merged_df.apply(
    lambda row: (row["Saldo_julho24"] / row["saldo_mar25"]) * 100 if row["saldo_mar25"] != 0 else None,
    axis=1
)

# Format all columns for display
merged_df["saldo_mar25"] = merged_df["saldo_mar25"].map("{:,.2f}".format)
merged_df["Saldo_julho24"] = merged_df["Saldo_julho24"].map("{:,.2f}".format)
merged_df["perc_jul24_vs_mar25"] = merged_df["perc_jul24_vs_mar25"].map("{:.2f}%".format)

