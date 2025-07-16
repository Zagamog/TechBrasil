# qbq_cnct1b.py

# Just unpickles the cnct/censo and qbq pickle files 

import os
import pandas as pd
from openai import OpenAI
import boto3
from botocore.exceptions import NoCredentialsError, ClientError
from dotenv import load_dotenv
import openpyxl
import seaborn as sns
import matplotlib.pyplot as plt

## Load Openai keys
dotenv_path = "D:/AdvancedR/knowbankedu/openai/.env"
#load_dotenv()
load_dotenv(dotenv_path)
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
os.environ["OPENAI_API_KEY"] = OPENAI_API_KEY
client = OpenAI()


local_path = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.pkl"
df_qbq_ocup_cmento1 = pd.read_pickle(local_path)

local_path = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbq.pkl"
df_censo_supl_tec_4qbq = pd.read_pickle(local_path)




## Now examine the CBOs provided by MEC
df_censo_cbo = (
    df_censo_supl_tec_4qbq
    .explode("cbo_list")                                # one row per CBO code
    .rename(columns={"cbo_list": "CBO_code"})           # rename for clarity
    .dropna(subset=["CBO_code"])                        # drop nulls if any
    .assign(CBO_code=lambda df: df["CBO_code"].astype(str).str.strip())  # ensure string type
)

df_censo_cbo = df_censo_cbo.sort_values(by="CBO_code").reset_index(drop=True)

# check from qbq

# Ensure CodCBO is string, padded to 6 digits, and deduplicated
# Fix CBO_code column: remove decimal and ensure string
df_qbq_cbo = (
    df_qbq_ocup_cmento1[["CodCBO"]]
    .dropna()
    .assign(CBO_code=lambda df: df["CodCBO"].astype(int).astype(str).str.zfill(6))
    .drop_duplicates()
    .sort_values(by="CBO_code")
    .reset_index(drop=True)
)

# First, ensure both are string CBOs (done for df_censo_cbo already)
df_qbq_ocup_cmento1["CBO_code"] = df_qbq_ocup_cmento1["CodCBO"].astype(int).astype(str).str.zfill(6)

# Now perform left join using CBO_code
df_censo_cbo_matched = df_censo_cbo.merge(
    df_qbq_ocup_cmento1,
    how="left",
    on="CBO_code"
)

# Keep only unmatched rows (those with NaN in the joined QBQ data)
df_unmatched_cbo_censo = df_censo_cbo_matched[df_censo_cbo_matched["CodCBO"].isna()].copy()
# 14 unmatched cbos censo_cnct codes not in qbq 

df_censo_cbo_matched = df_censo_cbo_matched.dropna(subset=["Ocupação"]).reset_index(drop=True)
# 477 rows

# Examine the data
df_censo_cbo_matched.info()
df_censo_cbo_matched.iloc[50,4]       
df_censo_cbo_matched.iloc[50,11]       
df_censo_cbo_matched.head(3)
# Save a version in MS-Excel

# Define path to save
output_path = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_cbo_matched.xlsx"

# Export to Excel
df_censo_cbo_matched.to_excel(output_path, index=False)



# Ensure CBO_code is string with leading zeros as needed
df_censo_cbo_matched["CBO_code"] = df_censo_cbo_matched["CBO_code"].astype(str).str.zfill(6)

# Extract hierarchical CBO components
df_censo_cbo_matched["GG"] = df_censo_cbo_matched["CBO_code"].str[0]       # Grande Grupo
df_censo_cbo_matched["SGP"] = df_censo_cbo_matched["CBO_code"].str[:2]     # Subgrupo Principal
df_censo_cbo_matched["SG"] = df_censo_cbo_matched["CBO_code"].str[:3]      # Subgrupo

# Count the distribution across hierarchies
distribution = {
    "Grande_Grupo": df_censo_cbo_matched["GG"].value_counts().sort_index(),
    "Subgrupo_Principal": df_censo_cbo_matched["SGP"].value_counts().sort_index(),
    "Subgrupo": df_censo_cbo_matched["SG"].value_counts().sort_index(),
}

# Convert to DataFrame for display
df_distribution = pd.DataFrame(distribution)


# Extract the 1st digit of the CBO code as 'Grande Grupo'
df_censo_cbo_matched["CBO_code_str"] = df_censo_cbo_matched["CBO_code"].astype(str).str.zfill(6)
df_censo_cbo_matched["Grande_Grupo"] = df_censo_cbo_matched["CBO_code_str"].str[0]

# Frequency distribution by Grande Grupo
freq_grande_grupo = df_censo_cbo_matched["Grande_Grupo"].value_counts().sort_index().reset_index()
freq_grande_grupo.columns = ["Grande_Grupo", "Frequência"]



# Plot the frequency distribution
plt.figure(figsize=(10, 6))
sns.barplot(data=freq_grande_grupo, x="Grande_Grupo", y="Frequência", palette="viridis")
plt.title("Distribuição de CBOs por Grande Grupo")
plt.xlabel("Grande Grupo (1º dígito do CBO)")
plt.ylabel("Frequência")
plt.xticks(rotation=0)
plt.tight_layout()
plt.show()




