# divida1b.py

import pandas as pd
import pyreadr

# Recreate full version preserving the UF name from original df_mar25
# Redefine both datasets again to preserve context

# Rebuild March 2025 data
# From the document "FAQ - Perguntas e Respostas - Programa de Pleno Pagamento de Dívidas dos Estados Anexo

data_mar25 = [
    ["SÃO PAULO", "291.684.192.718,19"], ["RIO DE JANEIRO", "178.485.878.129,97"],
    ["MINAS GERAIS", "164.072.322.152,05"], ["RIO GRANDE DO SUL", "101.642.375.981,12"],
    ["GOIÁS", "19.039.529.108,97"], ["PARANÁ", "12.512.559.235,68"],
    ["SANTA CATARINA", "11.428.037.582,88"], ["ALAGOAS", "8.990.378.025,69"],
    ["MATO GROSSO DO SUL", "7.355.125.617,76"], ["BAHIA", "5.808.094.633,51"],
    ["PERNAMBUCO", "4.295.502.477,28"], ["RONDÔNIA", "2.867.331.838,13"],
    ["MARANHÃO", "1.938.409.232,89"], ["ESPÍRITO SANTO", "1.691.077.107,86"],
    ["CEARÁ", "1.236.595.874,28"], ["SERGIPE", "1.201.372.532,28"],
    ["PARÁ", "1.198.518.957,35"], ["PARAÍBA", "963.096.161,66"],
    ["DISTRITO FEDERAL", "852.998.835,17"], ["MATO GROSSO", "754.141.024,33"],
    ["RIO GRANDE DO NORTE", "667.008.481,56"], ["AMAPÁ", "520.847.287,56"],
    ["PIAUÍ", "500.796.641,13"], ["ACRE", "426.996.338,22"],
    ["AMAZONAS", "272.634.327,20"], ["RORAIMA", "41.205.843,07"], ["TOCANTINS", "0,00"]
]
df_mar25 = pd.DataFrame(data_mar25, columns=["Estado", "Saldo_Devedor_BR"])
df_mar25["Saldo_Devedor"] = df_mar25["Saldo_Devedor_BR"].str.replace(".", "", regex=False).str.replace(",", ".", regex=False).astype(float)

# Add UF codes
uf_map = {
    "ACRE": "AC", "ALAGOAS": "AL", "AMAPÁ": "AP", "AMAZONAS": "AM", "BAHIA": "BA",
    "CEARÁ": "CE", "DISTRITO FEDERAL": "DF", "ESPÍRITO SANTO": "ES", "GOIÁS": "GO",
    "MARANHÃO": "MA", "MATO GROSSO": "MT", "MATO GROSSO DO SUL": "MS", "MINAS GERAIS": "MG",
    "PARÁ": "PA", "PARAÍBA": "PB", "PARANÁ": "PR", "PERNAMBUCO": "PE", "PIAUÍ": "PI",
    "RIO DE JANEIRO": "RJ", "RIO GRANDE DO NORTE": "RN", "RIO GRANDE DO SUL": "RS",
    "RONDÔNIA": "RO", "RORAIMA": "RR", "SANTA CATARINA": "SC", "SÃO PAULO": "SP",
    "SERGIPE": "SE", "TOCANTINS": "TO"
}
df_mar25["UF"] = df_mar25["Estado"].map(uf_map)
df_mar25["saldo_mar25"] = df_mar25["Saldo_Devedor"]

# Rebuild July 2024 data

# From the  webpage of FGV-IBRE 
# https://observatorio-politica-fiscal.ibre.fgv.br/federalismo-fiscal/historico-de-renegociacao-de-divida/renegociacao-das-dividas-estaduais

data_july24 = [
    ["AC", "412.817.174"], ["AL", "8.396.922.777"], ["AM", "342.093.742"],
    ["AP", "504.209.054"], ["BA", "5.530.980.342"], ["CE", "1.177.807.221"],
    ["DF", "988.954.368"], ["ES", "1.603.832.362"], ["GO", "16.887.724.651"],
    ["MA", "1.118.700.859"], ["MG", "142.615.023.561"], ["MS", "6.996.204.395"],
    ["MT", "1.041.778.159"], ["PA", "1.140.531.490"], ["PB", "916.499.062"],
    ["PE", "3.821.467.155"], ["PI", "0"], ["PR", "11.907.169.047"],
    ["RJ", "156.796.832.309"], ["RN", "660.219.339"], ["RO", "2.738.548.896"],
    ["RR", "51.451.426"], ["RS", "92.871.280.232"], ["SC", "10.875.119.375"],
    ["SE", "1.144.052.960"], ["SP", "277.625.902.004"], ["TO", "0"]
]
df_july24 = pd.DataFrame(data_july24, columns=["UF", "Saldo_julho24_BR"])
df_july24["Saldo_julho24"] = df_july24["Saldo_julho24_BR"].str.replace(".", "", regex=False).str.replace(",", ".", regex=False).astype(float)

# Merge with original df_mar25 to retain Estado
merged_df = pd.merge(df_mar25[["UF", "Estado", "saldo_mar25"]], df_july24[["UF", "Saldo_julho24"]], on="UF", how="outer")


# Format
merged_df["saldo_mar25"] = merged_df["saldo_mar25"].map("{:,.2f}".format)
merged_df["Saldo_julho24"] = merged_df["Saldo_julho24"].map("{:,.2f}".format)

# Remove the Saldo_julho24 column from merged_df
merged_df = merged_df.drop(columns=["Saldo_julho24"])

# Manually input amortization values in millions R$ (from the table)

# From the document Quadro 2- Estimativa de Impacto da Lei Complementar nº 212/2025 Nota Técnica Tesouro Nacional, January 2025
amort_dict = {
    "AC": 85.79, "AL": 1745.81, "AM": 52.29, "AP": 104.38, "BA": 1166.72,
    "CE": 248.40, "DF": 166.05, "ES": 334.18, "GO": 3831.94, "MA": 174.82,
    "MT": 141.78, "MS": 1477.64, "MG": 33112.50, "PA": 240.78, "PB": 193.49,
    "PR": 2513.76, "PE": 797.46, "RJ": 34972.01, "RN": 132.58, "RS": 20438.23,
    "RO": 576.03, "RR": 7.88, "SC": 2295.88, "SP": 57049.58, "SE": 241.33, "TO": 0.00, "PI": 0.00
}

# Convert to DataFrame
df_amort = pd.DataFrame(list(amort_dict.items()), columns=["UF", "amort_extr_mil"])

# Merge with your merged_df (convert to R$ full)
df_amort["amort_extr"] = df_amort["amort_extr_mil"] * 1_000_000
merged_df = pd.merge(merged_df, df_amort[["UF", "amort_extr"]], on="UF", how="left")

# Format amort_extr without scientific notation
merged_df["amort_extr"] = merged_df["amort_extr"].map("{:,.2f}".format)


#  FEF and investment contributions:
    # First convert formatted columns back to float
merged_df["saldo_mar25_float"] = merged_df["saldo_mar25"].str.replace(",", "", regex=False).astype(float)
merged_df["amort_extr_float"] = merged_df["amort_extr"].str.replace(",", "", regex=False).astype(float)

# Calculate refinanced base
merged_df["refinanced_base"] = merged_df["saldo_mar25_float"] - merged_df["amort_extr_float"]


# Compute FEF_1ano (1%) and EPT_1ano (0.6%) of the refinanced base
merged_df["FEF_1ano_cen01"] = merged_df["refinanced_base"] * 0.01
merged_df["EPT_1ano_cen01"] = merged_df["refinanced_base"] * 0.006


# Format new columns for display
merged_df["FEF_1ano_cen01"] = merged_df["FEF_1ano_cen01"].map("{:,.2f}".format)
merged_df["EPT_1ano_cen01"] = merged_df["EPT_1ano_cen01"].map("{:,.2f}".format)

##

# From STN presentation on Propag, April 2025 with decree promulgation
# Tesoro Nacional apresentacao-da-regulamentacao-propag abril 2025.pdf


fef_shares = {
    "AC": 4.3, "AL": 4.0, "AP": 2.9, "AM": 4.5, "BA": 7.5,
    "CE": 5.9, "DF": 1.2, "ES": 2.5, "GO": 2.3, "MA": 6.7,
    "MT": 4.4, "MS": 1.8, "MG": 3.7, "PA": 6.3, "PB": 4.3,
    "PR": 2.9, "PE": 6.2, "PI": 3.6, "RJ": 1.6, "RN": 4.1,
    "RS": 1.6, "RO": 3.1, "RR": 4.2, "SC": 1.8, "SP": 1.1,
    "SE": 4.0, "TO": 3.3
}

# Total FEF (previously calculated from sum of FEF_1ano_float)
total_fef_cen01 = 6583457161.46

# Convert FEF shares to DataFrame
df_fef_shares = pd.DataFrame(list(fef_shares.items()), columns=["UF", "fef_share_pct"])
df_fef_shares["fef_received"] = df_fef_shares["fef_share_pct"] / 100 * total_fef_cen01


# Merge with merged_df to bring fef_share_pct
merged_df = pd.merge(merged_df, df_fef_shares[["UF", "fef_share_pct"]], on="UF", how="left")

# Merge with merged_df to bring in received values
merged_df = pd.merge(merged_df, df_fef_shares[["UF", "fef_received"]], on="UF", how="left")

# Reconvert FEF_1ano_cen01 to float for subtraction
merged_df["FEF_1ano_float"] = merged_df["FEF_1ano_cen01"].str.replace(",", "", regex=False).astype(float)


# Compute FEF net benefit = received - contributed
merged_df["FEF_1ano_liq_cen01"] = merged_df["fef_received"] - merged_df["FEF_1ano_float"]
merged_df["FEF_1ano_liq_cen01"] = merged_df["FEF_1ano_liq_cen01"].map("{:,.2f}".format)


# Cenario II

# Compute FEF_1ano_cen02 and EPT_1ano_cen02 based on refinanced base
merged_df["FEF_1ano_cen02"] = merged_df["refinanced_base"] * 0.02
merged_df["EPT_1ano_cen02"] = merged_df["refinanced_base"] * 0.012

# Format for display
merged_df["FEF_1ano_cen02"] = merged_df["FEF_1ano_cen02"].map("{:,.2f}".format)
merged_df["EPT_1ano_cen02"] = merged_df["EPT_1ano_cen02"].map("{:,.2f}".format)

# Reconvert formatted FEF_1ano_cen02 for arithmetic
merged_df["FEF_1ano_cen02_float"] = merged_df["FEF_1ano_cen02"].str.replace(",", "", regex=False).astype(float)

# Total FEF for Cenário II
total_fef_cen02 = merged_df["FEF_1ano_cen02_float"].sum()

# Build share allocation table
df_fef_shares_cen02 = pd.DataFrame(list(fef_shares.items()), columns=["UF", "fef_share_pct"])
df_fef_shares_cen02["fef_received_cen02"] = df_fef_shares_cen02["fef_share_pct"] / 100 * total_fef_cen02

# Merge received amounts into main dataframe
merged_df = pd.merge(merged_df, df_fef_shares_cen02[["UF", "fef_received_cen02"]], on="UF", how="left")

# Compute net benefit
merged_df["FEF_1ano_liq_cen02"] = merged_df["fef_received_cen02"] - merged_df["FEF_1ano_cen02_float"]
merged_df["FEF_1ano_liq_cen02"] = merged_df["FEF_1ano_liq_cen02"].map("{:,.2f}".format)


# Convert formatted FEF and EPT 1-year columns back to float
merged_df["FEF_1ano_cen01_float"] = merged_df["FEF_1ano_cen01"].str.replace(",", "", regex=False).astype(float)
merged_df["EPT_1ano_cen01_float"] = merged_df["EPT_1ano_cen01"].str.replace(",", "", regex=False).astype(float)
merged_df["FEF_1ano_cen02_float"] = merged_df["FEF_1ano_cen02"].str.replace(",", "", regex=False).astype(float)
merged_df["EPT_1ano_cen02_float"] = merged_df["EPT_1ano_cen02"].str.replace(",", "", regex=False).astype(float)

# Multiply by 5 to get 5-year totals
merged_df["EPT_5ano_cen01"] = (merged_df["EPT_1ano_cen01_float"] * 5).map("{:,.2f}".format)
merged_df["EPT_5ano_cen02"] = (merged_df["EPT_1ano_cen02_float"] * 5).map("{:,.2f}".format)
# Now compute 5-year totals for the net values
# Convert 1-year net FEF values from string to float
merged_df["FEF_1ano_liq_cen01"] = merged_df["FEF_1ano_liq_cen01"].str.replace(",", "", regex=False).astype(float)
merged_df["FEF_1ano_liq_cen02"] = merged_df["FEF_1ano_liq_cen02"].str.replace(",", "", regex=False).astype(float)

# Now compute 5-year totals for the net values and format
merged_df["FEF_5ano_liq_cen01"] = (merged_df["FEF_1ano_liq_cen01"] * 5).map("{:,.2f}".format)
merged_df["FEF_5ano_liq_cen02"] = (merged_df["FEF_1ano_liq_cen02"] * 5).map("{:,.2f}".format)


# Drop intermediate float columns to clean up the DataFrame
cols_to_drop = [
    "refinanced_base",
    "saldo_mar25_float", "amort_extr_float", "FEF_1ano_float",
    "FEF_1ano_cen01_float", "FEF_1ano_cen02_float",
    "EPT_1ano_cen01_float", "EPT_1ano_cen02_float",
    "fef_received", "fef_received_cen02"
]
merged_df = merged_df.drop(columns=cols_to_drop)



# Save the DataFrame as a pickle file for efficient Python use
pickle_path = "D:/Country/Brazil/TechBrazil/working/mec/propag_ept_financeiro.pkl"
merged_df.to_pickle(pickle_path)

# Load the DataFrame back from the pickle file
propag_ept_financeiro = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/mec/propag_ept_financeiro.pkl")

# Save the merged_df as .rds (correct way)
pyreadr.write_rds("D:/Country/Brazil/TechBrazil/working/mec/propag_ept_financeiro.rds", merged_df)
# Save as CSV (no index column)
propag_ept_financeiro.to_csv("D:/Country/Brazil/TechBrazil/working/mec/propag_ept_financeiro.csv", index=False)



