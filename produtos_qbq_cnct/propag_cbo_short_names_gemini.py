"""
cbo_short_names_gemini.py

PROPAG / Juros por Educação — Geração de Nomes Curtos CBO Família
Usa Gemini Flash via WB Azure gateway para gerar nomes curtos (3-4 palavras)
para as famílias CBO usadas na Aba D2 (APLs).

USO:
  1. Rodar o script R auxiliar primeiro para exportar cbo_familias.csv:
     > load("qbq_ocup_cmento1.rda")
     > cbo_fam <- unique(qbq_ocup_cmento1[, c("cbo_4dig", "cbo_familia")])
     > write.csv(cbo_fam, "cbo_familias.csv", row.names = FALSE)

  2. Rodar este script:
     $ python cbo_short_names_gemini.py

  3. Output: cbo_short_names.csv e cbo_short_names.rda (via R helper)

DEPENDÊNCIAS: requests, pandas, dotenv (mesmas do knowbankedu)
"""

import json
import time
import os
import logging
import pandas as pd
import requests
from azure.identity import DeviceCodeCredential, get_bearer_token_provider

# Set working directory to TechBrazil root
os.chdir("D:/Country/Brazil/TechBrazil")
print(f"Working directory: {os.getcwd()}")

# ============================================================================
# Configuration — same as knowbankedu
# ============================================================================
GEMINI_ENDPOINT = "https://azapimdev.worldbank.org/conversationalai/platform/gemini/chat/"
AZURE_TENANT_ID = "31a2fec0-266b-4c67-b56e-2796d8f59c36"
AZURE_CLIENT_ID = "00c104af-b0ae-4557-9787-6e6cfced741e"

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# ============================================================================
# Auth
# ============================================================================
def device_code_prompt(verification_uri, user_code, expires_on):
    print("\n" + "="*60)
    print("AZURE LOGIN REQUIRED")
    print(f"Go to: {verification_uri}")
    print(f"Enter code: {user_code}")
    print("="*60 + "\n")

token_provider = get_bearer_token_provider(
    DeviceCodeCredential(
        tenant_id=AZURE_TENANT_ID,
        client_id=AZURE_CLIENT_ID,
        prompt_callback=device_code_prompt
    ),
    "https://cognitiveservices.azure.com/.default"
)

def call_gemini(prompt, system_prompt="", model="gemini-2.0-flash",
                max_tokens=4000, temperature=0.2, timeout=90):
    token = token_provider()
    messages = []
    if system_prompt:
        messages.append({"role": "system", "content": system_prompt})
    messages.append({"role": "human", "content": prompt})

    response = requests.post(
        GEMINI_ENDPOINT,
        headers={"Authorization": f"Bearer {token}", "Content-Type": "application/json"},
        json={
            "model_name": model, "messages": messages,
            "temperature": temperature, "max_tokens": max_tokens,
            "timeout": timeout, "max_retries": 3
        },
        timeout=timeout + 30
    )
    if response.status_code != 200:
        raise Exception(f"Gemini API error: {response.status_code} - {response.text}")

    result = response.json()
    content = None
    if "choices" in result and len(result["choices"]) > 0:
        msg = result["choices"][0].get("message", {})
        content = msg.get("content") if isinstance(msg, dict) else msg
    elif "content" in result:
        content = result["content"]
    elif "response" in result:
        content = result["response"]

    if content is None:
        return str(result)
    elif isinstance(content, list):
        return "\n".join(
            item.get("text", str(item)) if isinstance(item, dict) else str(item)
            for item in content
        )
    return str(content)

# ============================================================================
# Examples for few-shot prompt (30 diverse examples across sectors)
# ============================================================================
EXAMPLES = ""  # Not needed — Gemini handles this from instruction alone

# ============================================================================
# Main
# ============================================================================
def main():
    # Load input
    df = pd.read_csv("working/qbq/cbo_familias.csv")
    df = df.dropna(subset=["cbo_4dig", "cbo_familia"]).drop_duplicates(subset=["cbo_4dig"])
    logger.info(f"Total unique CBO families: {len(df)}")

    # Process in batches of 50
    batch_size = 50
    all_results = []

    system_prompt = (
        "You shorten Portuguese text labels into concise versions of maximum 4 words. "
        "You only use words that appear in the original text — never add new information. "
        "Return ONLY valid JSON — a list of objects with keys: cbo_4dig, short_name. "
        "No markdown, no ```json blocks, no explanations."
    )

    for i in range(0, len(df), batch_size):
        batch = df.iloc[i:i+batch_size]
        batch_list = "\n".join(
            f"{row['cbo_4dig']}|{row['cbo_familia']}"
            for _, row in batch.iterrows()
        )

        prompt = f"""Shorten each label to maximum 4 words in Portuguese.

Rules:
- ONLY use words that appear in the original text — never invent or add words
- If the name is already 1-2 words, keep it exactly as-is
- Remove generic prefixes like "Trabalhadores de/nos/na", "Operadores de" when possible
- Keep the most distinctive words that identify the activity
- No abbreviations

Return JSON array: [{{"cbo_4dig": "XXXX", "short_name": "Nome Curto"}}]

INPUT (format: cbo_4dig|original_label):
{batch_list}
"""

        logger.info(f"Processing batch {i//batch_size + 1} ({len(batch)} families)...")

        try:
            raw = call_gemini(
                prompt=prompt,
                system_prompt=system_prompt,
                model="gemini-2.0-flash",
                max_tokens=4000,
                temperature=0.2,
                timeout=90
            )
            raw = str(raw).strip()
            # Clean markdown fences if present
            if raw.startswith("```"):
                raw = raw.split("\n", 1)[-1]
            if raw.endswith("```"):
                raw = raw.rsplit("```", 1)[0]
            raw = raw.strip()

            results = json.loads(raw)
            all_results.extend(results)
            logger.info(f"  Got {len(results)} results")
        except Exception as e:
            logger.error(f"  Batch failed: {e}")
            # Fallback: use original names
            for _, row in batch.iterrows():
                all_results.append({
                    "cbo_4dig": str(row["cbo_4dig"]),
                    "short_name": row["cbo_familia"]
                })

        time.sleep(2)  # Rate limit

    # Merge and save
    df_short = pd.DataFrame(all_results)
    df_short["cbo_4dig"] = df_short["cbo_4dig"].astype(str)
    df["cbo_4dig"] = df["cbo_4dig"].astype(str)

    df_merged = df.merge(df_short, on="cbo_4dig", how="left")
    df_merged["short_name"] = df_merged["short_name"].fillna(df_merged["cbo_familia"])

    # Save CSV
    df_merged.to_csv("working/qbq/cbo_short_names.csv", index=False)
    logger.info(f"Saved working/qbq/cbo_short_names.csv ({len(df_merged)} rows)")
    logger.info("Next: run qbq_03a.R to convert to .rda and upload to S3")

if __name__ == "__main__":
    main()