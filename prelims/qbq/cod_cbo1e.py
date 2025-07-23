# cod_cbo1e.py

import os
import json
import pandas as pd
import requests
from dotenv import load_dotenv
import re
import time

# Load API key
load_dotenv("D:/AdvancedR/knowbankedu/openai/.env")
api_key = os.getenv("OPENAI_API_KEY")

# Load data
df_cod4 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/df_cod4.pkl")
df_qbq4 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/df_qbq4.pkl")
df_match3 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result3.pkl")

# Instruction
instruction = (
    "You are given two Portuguese-language lists:\n"
    "1. df_cod4: a list of occupation subgroup titles labeled 'COD_NOME'. These are associated with 4-digit codes called 'COD_4'.\n"
    "2. df_qbq4: a list of occupation subgroup titles labeled 'cbo_familia', each associated with a 4-digit 'cbo_4dig' code. Each 'cbo_4dig' also belongs to a broader 3-digit 'cbo_3dig' category.\n"
    "\n"
    "Your task is to match each 'COD_NOME' from df_cod4 to the most semantically and hierarchically aligned 'cbo_familia'.\n"
    "\n"
    "Prioritize:\n"
    "- Strong **thematic similarity** between the COD_NOME and cbo_familia (e.g., 'marketing' → 'marketing').\n"
    "- Correct hierarchical alignment wherever possible using the provided 3-digit matches.\n"
    "\n"
    "Note:\n"
    "- There are more cbo_familia than COD_NOME, so some mappings will reuse the same cbo_familia.\n"
    "- You may also use the previously matched 3-digit codes (COD_3 ↔ cbo_3dig) for alignment hints when helpful.\n"
    "\n"
    "Return only a valid JSON list with:\n"
    "  - COD_NOME\n"
    "  - matched_cbo_familia\n"
    "  - cbo_4dig\n"
    "Do not include any explanatory text or markdown wrappers before or after the list."
)

# Prepare fields
df_cod4 = df_cod4[~df_cod4["COD_NOME"].str.contains("SEXO", case=False, na=False)]
# json truncated with this one 
df_cod4["COD_3"] = df_cod4["COD_4"].str.slice(0, 3)
df_qbq4["cbo_3dig"] = df_qbq4["cbo_4dig"].str.slice(0, 3)

df_qbq_payload = df_qbq4[["cbo_4dig", "cbo_familia", "cbo_3dig"]].to_dict(orient="records")
df_match3_payload = df_match3.to_dict(orient="records")

# Parameters
batch_size = 100
results = []

# Batch loop
for i in range(0, len(df_cod4), batch_size):
    print(f"Processing batch {i // batch_size + 1}...")
    batch_df = df_cod4.iloc[i:i+batch_size].copy()
    batch_payload = batch_df[["COD_4", "COD_NOME", "COD_3"]].to_dict(orient="records")

    data_payload = {
        "df_cod4": batch_payload,
        "df_qbq4": df_qbq_payload,
        "match3": df_match3_payload
    }

    messages = [
        {"role": "system", "content": instruction},
        {"role": "user", "content": json.dumps(data_payload, ensure_ascii=False)}
    ]

    try:
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
        response.raise_for_status()
        content = response.json()["choices"][0]["message"]["content"]
        cleaned = re.sub(r"```[\w]*\s*", "", content).strip()
        records = json.loads(cleaned)
        results.extend(records)
    except Exception as e:
        print(f"[!] Error in batch {i // batch_size + 1}: {e}")
        time.sleep(5)  # brief pause before next batch

# Final DataFrame
df_match4 = pd.DataFrame(results)

# Reattach COD_4 from original df_cod4
df_cod4_short = df_cod4[["COD_4", "COD_NOME"]]
df_match4 = (
    df_match4
      .merge(df_cod4_short, on="COD_NOME", how="left")
      .rename(columns={"matched_cbo_familia": "cbo_familia"})
      .loc[:, ["COD_4", "COD_NOME", "cbo_4dig", "cbo_familia"]]
)

df_match4.to_pickle("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result4.pkl")
df_match4.to_csv("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result4.csv", index=False)

print(f"[✓] 4-digit mapping complete for {len(df_match4)} records. Output saved.")
