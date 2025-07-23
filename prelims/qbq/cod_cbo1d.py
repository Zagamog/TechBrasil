# cod_cbo1d.py

import os
import json
import pandas as pd
import requests
from dotenv import load_dotenv
import re

# Load API key
load_dotenv("D:/AdvancedR/knowbankedu/openai/.env")
api_key = os.getenv("OPENAI_API_KEY")

# Load data
df_cod3 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/df_cod3.pkl")
df_qbq3 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/df_qbq3.pkl")
df_match2 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result2.pkl")  # 2-digit match only

# --- Build the instruction ---
instruction = (
    "You are given two Portuguese-language lists:\n"
    "1. df_cod3: a list of occupation subgroup titles labeled 'COD_NOME'. These are associated with 3-digit codes called 'COD_3'.\n"
    "2. df_qbq3: a list of occupation subgroup titles labeled 'cbo_subgru', each associated with a 3-digit 'cbo_3dig' code. Each 'cbo_3dig' also belongs to a broader 2-digit 'cbo_2dig' and 1-digit 'cbo_1dig' category.\n"
    "\n"
    "Your task is to match each 'COD_NOME' from df_cod3 to the most semantically and hierarchically aligned 'cbo_subgru'.\n"
    "\n"
    "Prioritize:\n"
    "- Strong **thematic similarity** between the COD_NOME and cbo_subgru (e.g., 'marketing' → 'marketing').\n"
    "Example of an incorrect match to avoid:\n"
    "- COD_3 = 122: 'DIRIGENTES DE VENDAS, COMERCIALIZAÇÃO, MARKETING E DESENVOLVIMENTO'\n"
    "- Incorrect: cbo_3dig = 253 ('PROFISSIONAIS DE RELAÇÕES PÚBLICAS, PUBLICIDADE E COMUNICAÇÃO') → this is a media/communications **professional** group, not a managerial one.\n"
    "- Correct: cbo_3dig = 141 ('GERENTES DE COMERCIALIZAÇÃO E MARKETING') → includes marketing and business development **managers**, which fits the intent of COD_3 = 122.\n"
    "\n"
    "Note:\n"
    "- There are more cbo_subgru than COD_NOME, so some mappings will reuse the same cbo_subgru.\n"
    "- You may also use the previously matched 2-digit codes (COD_2 ↔ cbo_2dig) for alignment hints when helpful.\n"
    "\n"
    "Return only a valid JSON list with:\n"
    "  - COD_NOME\n"
    "  - matched_cbo_subgru\n"
    "  - cbo_3dig\n"
    "Do not include any explanatory text or markdown wrappers before or after the list."
)

# --- Build the payload ---
df_cod3["COD_2"] = df_cod3["COD_3"].str.slice(0, 2)
df_qbq3["cbo_2dig"] = df_qbq3["cbo_3dig"].str.slice(0, 2)


df_cod_payload = df_cod3[["COD_3", "COD_NOME", "COD_2"]].to_dict(orient="records")
df_qbq_payload = df_qbq3[["cbo_3dig", "cbo_subgru", "cbo_2dig"]].to_dict(orient="records")
df_match2_payload = df_match2.to_dict(orient="records")

data_payload = {
    "df_cod3": df_cod_payload,
    "df_qbq3": df_qbq_payload,
    "match2": df_match2_payload
}

messages = [
    {"role": "system", "content": instruction},
    {"role": "user", "content": json.dumps(data_payload, ensure_ascii=False)}
]

# --- API Call ---
response = requests.post(
    "https://api.openai.com/v1/chat/completions",
    headers={
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}"
    },
    json={
        "model": "gpt-4o",
        "messages": messages,
        "temperature": 0.0
    }
)

# --- Parse and clean ---
result_json = response.json()
matches = result_json["choices"][0]["message"]["content"]
cleaned = re.sub(r"```[\w]*\s*", "", matches).strip()

# Convert to dataframe
records = json.loads(cleaned)
df_match3 = pd.DataFrame(records)

# Reattach COD_3 from original df_cod3
df_cod3_short = df_cod3[["COD_3", "COD_NOME"]]
df_match3 = (
    df_match3
      .merge(df_cod3_short, on="COD_NOME", how="left")
      .rename(columns={"matched_cbo_subgru": "cbo_subgru"})
      .loc[:, ["COD_3", "COD_NOME", "cbo_3dig", "cbo_subgru"]]
)


# Save outputs
df_match3.to_pickle("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result3.pkl")
df_match3.to_csv("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result3.csv", index=False)

print("[✓] 3-digit mapping complete. Output saved as pickle and CSV.")
