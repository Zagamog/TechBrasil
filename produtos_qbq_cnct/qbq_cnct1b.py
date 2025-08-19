# qbq_cnct1b.py
# Cleaned-up script: unpickles CNCT/CENSO and QBQ data, aligns CBOs, enriches, and exports

import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from dotenv import load_dotenv

# --- Load Environment ---
dotenv_path = "D:/AdvancedR/knowbankedu/openai/.env"
load_dotenv(dotenv_path)

# --- Load Pickle Files ---
df_qbq = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.pkl")
df_cnct = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbq.pkl")

# --- Explode and clean CBOs from CNCT ---
df_censo_cbo = (
    df_cnct.explode("cbo_list")
    .rename(columns={"cbo_list": "CBO_code"})
    .dropna(subset=["CBO_code"])
    .assign(CBO_code=lambda df: df["CBO_code"].astype(str).str.strip())
    .sort_values("CBO_code").reset_index(drop=True)
)

# --- Prepare QBQ CBOs ---
df_qbq["CBO_code"] = df_qbq["CodCBO"].astype(int).astype(str).str.zfill(6)
df_qbq_clean = df_qbq.dropna(subset=["CodCBO"])[["CBO_code"]].drop_duplicates().sort_values("CBO_code")

# --- Merge and identify unmatched ---
df_censo_cbo_matched = df_censo_cbo.merge(df_qbq, how="left", on="CBO_code")
df_unmatched = df_censo_cbo_matched[df_censo_cbo_matched["CodCBO"].isna()].copy()
df_censo_cbo_matched = df_censo_cbo_matched.dropna(subset=["Ocupação"]).reset_index(drop=True)

# --- Add hierarchical labels from CBO CSVs ---
base = "D:/Country/Brazil/TechBrazil/rawdata/cbo/"

# Grande Grupo
df_gg = pd.read_csv(base + "CBO2002 - Grande Grupo.csv", encoding="latin1", sep=";")
df_gg.columns = df_gg.columns.str.strip().str.lower()
df_gg = df_gg.rename(columns={"codigo": "cbo_1dig", "titulo": "cbo_gragru"})
df_gg["cbo_1dig"] = df_gg["cbo_1dig"].astype(str)
df_censo_cbo_matched["cbo_1dig"] = df_censo_cbo_matched["CBO_code"].str[0]
df_censo_cbo_matched = df_censo_cbo_matched.merge(df_gg, on="cbo_1dig", how="left")

# SubGrupo Principal
df_sgp = pd.read_csv(base + "CBO2002 - SubGrupo Principal.csv", encoding="latin1", sep=";")
df_sgp.columns = df_sgp.columns.str.strip().str.lower()
df_sgp = df_sgp.rename(columns={"codigo": "cbo_2dig", "titulo": "cbo_prigru"})
df_sgp["cbo_2dig"] = df_sgp["cbo_2dig"].astype(str).str.zfill(2)
df_censo_cbo_matched["cbo_2dig"] = df_censo_cbo_matched["CBO_code"].str[:2]
df_censo_cbo_matched = df_censo_cbo_matched.merge(df_sgp, on="cbo_2dig", how="left")

# SubGrupo
df_sg = pd.read_csv(base + "CBO2002 - SubGrupo.csv", encoding="latin1", sep=";")
df_sg.columns = df_sg.columns.str.strip().str.lower()
df_sg = df_sg.rename(columns={"codigo": "cbo_3dig", "titulo": "cbo_subgru"})
df_sg["cbo_3dig"] = df_sg["cbo_3dig"].astype(str).str.zfill(3)
df_censo_cbo_matched["cbo_3dig"] = df_censo_cbo_matched["CBO_code"].str[:3]
df_censo_cbo_matched = df_censo_cbo_matched.merge(df_sg, on="cbo_3dig", how="left")

# Familia
df_fam = pd.read_csv(base + "CBO2002 - Familia.csv", encoding="latin1", sep=";")
df_fam.columns = df_fam.columns.str.strip().str.lower()
df_fam = df_fam.rename(columns={"codigo": "cbo_4dig", "titulo": "cbo_familia"})
df_fam["cbo_4dig"] = df_fam["cbo_4dig"].astype(str).str.zfill(4)
df_censo_cbo_matched["cbo_4dig"] = df_censo_cbo_matched["CBO_code"].str[:4]
df_censo_cbo_matched = df_censo_cbo_matched.merge(df_fam, on="cbo_4dig", how="left")

# --- Finalize and export ---
df_censo_cbo_matched = df_censo_cbo_matched.rename(columns={"CBO_code": "cbo_6dig"})
df_censo_cbo_matched.drop(columns=["CodCBO"], inplace=True, errors="ignore")

# Export Excel
output_path = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_cbo_matched.xlsx"
df_censo_cbo_matched.to_excel(output_path, index=False)

# Save as pickle file
pickle_path = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_cbo_matched.pkl"
df_censo_cbo_matched.to_pickle(pickle_path)

# --- Plot Distribution by Grande Grupo ---
df_censo_cbo_matched["GG"] = df_censo_cbo_matched["cbo_6dig"].str[0]
freq = df_censo_cbo_matched["GG"].value_counts().sort_index().reset_index()
freq.columns = ["Grande_Grupo", "Frequencia"]

plt.figure(figsize=(10, 6))
sns.barplot(data=freq, x="Grande_Grupo", y="Frequencia", palette="viridis")
plt.title("Distribuição de CBOs por Grande Grupo")
plt.xlabel("Grande Grupo (1º dígito do CBO)")
plt.ylabel("Frequência")
plt.tight_layout()
plt.show()

# --- Plot Distribution by Nivel Ocupacional ---
df_censo_cbo_matched["NO"] = df_censo_cbo_matched["cbo_6dig"].str[0]
freq = df_censo_cbo_matched["NO"].value_counts().sort_index().reset_index()
freq.columns = ["NivelOcupacional", "Frequencia"]

plt.figure(figsize=(10, 6))
sns.barplot(data=freq, x="NivelOcupacional", y="Frequencia", palette="viridis")
plt.title("Distribuição de CBOs por NivelOcupacional")
plt.xlabel("NivelOcupacional")
plt.ylabel("Frequência")
plt.tight_layout()
plt.show()





