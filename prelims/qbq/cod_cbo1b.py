#cod_cbo1b.py

import os
import json
import pandas as pd
import requests
from dotenv import load_dotenv
import re 

# Load API key
load_dotenv("D:/AdvancedR/knowbankedu/openai/.env")
api_key = os.getenv("OPENAI_API_KEY")

# Load input dataframes
df_cod1 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/df_cod1.pkl")
df_qbq1 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/df_qbq1.pkl")

# Build the prompt
instruction = (
    "You are given two lists:\n"
    "1. df_cod1: a list of 'COD_NOME' occupation group titles.\n"
    "2. df_qbq1: a list of 'cbo_gragru' titles with associated 1-digit 'cbo_1dig' codes.\n"
    "Your task is to match each 'COD_NOME' to the best matching 'cbo_gragru'.\n"
    "Do not leave any unmatched 'cbo_gragru' or unmatched 'COD_NOME'.\n"
    "Return a JSON list with: COD_NOME, matched_cbo_gragru, cbo_1dig."
)

data_payload = {
    "df_cod1": df_cod1["COD_NOME"].tolist(),
    "df_qbq1": df_qbq1[["cbo_1dig", "cbo_gragru"]].to_dict(orient="records")
}

messages = [
    {"role": "system", "content": instruction},
    {"role": "user", "content": json.dumps(data_payload, ensure_ascii=False)}
]

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

# Parse and save the result
result_json = response.json()
matches = result_json["choices"][0]["message"]["content"]


# ---- 1.  Clean the raw string --------------------------------------------
#  `matches` is what you already captured from the API:
#     matches = result_json["choices"][0]["message"]["content"]

cleaned = re.sub(r"```[\w]*\s*", "", matches).strip()   # drops ```json / ``` fences

# ---- 2.  Parse the JSON list ---------------------------------------------
records = json.loads(cleaned)          # <-- no more JSONDecodeError
df_match = pd.DataFrame(records)       # columns: COD_NOME, matched_cbo_gragru, cbo_1dig

# ---- 3.  Add COD_1 (from the original df_cod1) and tidy names ------------
df_cod1 = pd.read_pickle(
    "D:/Country/Brazil/TechBrazil/working/ibge/df_cod1.pkl"
)[["COD_1", "COD_NOME"]]                # keep only what we need

df_match = (
    df_match
      .merge(df_cod1, on="COD_NOME", how="left")        # adds COD_1
      .rename(columns={"matched_cbo_gragru": "cbo_gragru"})
      .loc[:, ["COD_1", "COD_NOME", "cbo_1dig", "cbo_gragru"]]  # desired order
)


# Save as pickle
df_match.to_pickle("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result.pkl")

# Save as CSV (with UTF-8 encoding to preserve accents)
df_match.to_csv("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result.csv", index=False, encoding="utf-8-sig")

print("[✓] Matching complete. Files saved as .json, .pkl, and .csv.")


