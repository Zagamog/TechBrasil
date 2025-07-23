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
df_cod2 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/df_cod2.pkl")
df_qbq2 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/df_qbq2.pkl")
df_match1 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result.pkl")  # 1-digit match

# --- Build the instruction ---
instruction = (
    "You are given two portuguese language lists:\n"
    "1. df_cod2: a list of occupation group titles labeled 'COD_NOME'. These are associated with 2-digit codes called 'COD_2'.\n"
    "2. df_qbq2: a list of occupation group titles labeled 'cbo_prigru', each associated with a 2-digit 'cbo_2dig' code. Each 'cbo_2dig' also belongs to a broader 1-digit group 'cbo_1dig'.\n"
    "Your task is to match each 'COD_NOME' from df_cod2 to the most semantically aligned 'cbo_prigru' from df_qbq2.\n"
    "\n"
    "You must prioritize **meaningful thematic alignment** (e.g., 'sales' with 'sales', 'admin' with 'admin').\n"
    "Do NOT enforce numeric similarity between COD_2 and cbo_2dig unless it also reflects real thematic similarity.\n"
    "\n"
    "Note:\n"
    "- There are 39 unique 'COD_NOME' and 42 unique 'cbo_prigru'. So some matches may reuse the same 'cbo_prigru'.\n"
    "- Use the available 'cbo_1dig' field for contextual alignment when helpful, based on prior mapping between COD_1 and cbo_1dig.\n"
    "\n"
    "Return the result as a pure JSON list (do not wrap it in markdown). Each entry must include:\n"
    "  - COD_NOME\n"
    "  - matched_cbo_prigru\n"
    "  - cbo_2dig\n"
    "No explanatory text should be included before or after the JSON."
)


# --- Build the payload ---
# Extract COD_1 from COD_2 (first digit)
df_cod2["COD_1"] = df_cod2["COD_2"].str.slice(0, 1)
df_qbq2["cbo_1dig"] = df_qbq2["cbo_2dig"].str.slice(0, 1)

df_cod_payload = df_cod2[["COD_2", "COD_NOME", "COD_1"]].to_dict(orient="records")
df_qbq_payload = df_qbq2[["cbo_2dig", "cbo_prigru", "cbo_1dig"]].to_dict(orient="records")
df_match1_payload = df_match1.to_dict(orient="records")

data_payload = {
    "df_cod2": df_cod_payload,
    "df_qbq2": df_qbq_payload,
    "match1": df_match1_payload
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
df_match2 = pd.DataFrame(records)

# Reattach COD_2 values from the original dataframe
df_cod2_short = df_cod2[["COD_2", "COD_NOME"]]  # keep only necessary columns
df_match2 = (
    df_match2
      .merge(df_cod2_short, on="COD_NOME", how="left")  # adds back COD_2
      .rename(columns={"matched_cbo_prigru": "cbo_prigru"})  # optional: rename to match 1-digit style
      .loc[:, ["COD_2", "COD_NOME", "cbo_2dig", "cbo_prigru"]]  # reorder columns
)

# Save as pickle and CSV
df_match2.to_pickle("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result2.pkl")
df_match2.to_csv("D:/Country/Brazil/TechBrazil/working/ibge/cbo_matching_result2.csv", index=False)

print("[✓] 2-digit mapping complete. Output saved as pickle and CSV.")
