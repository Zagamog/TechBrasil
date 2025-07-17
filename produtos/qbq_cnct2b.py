# qbq_cnct2b.py

# Check query and rerank test

from pinecone import Pinecone
from sentence_transformers import SentenceTransformer
from dotenv import load_dotenv
from transformers import AutoTokenizer, AutoModelForSequenceClassification
from torch.nn.functional import softmax
import torch
import pandas as pd
import os
import unicodedata
import re
from collections import Counter
import pprint
import random

# --- Load Pinecone ---

# --- 1. Setup Pinecone ---
load_dotenv("D:/AdvancedR/knowbankedu/openai/.env")
pc = Pinecone(api_key=os.getenv("PINECONE_API_KEY"))
index = pc.Index("cnct-qbq")

# --- Load models ---
embed_model = SentenceTransformer("intfloat/multilingual-e5-large", device="cuda")



# --- Load df ---
df = pd.read_excel("D:/Country/Brazil/TechBrazil/working/qbq/df_censo_cbo_matched.xlsx")


# --- Pick an example egresso ---
def normalize_namespace(text):
    text_ascii = unicodedata.normalize("NFKD", text).encode("ASCII", "ignore").decode()
    text_ascii = text_ascii.lower()
    text_hyphenated = re.sub(r"[^a-z0-9]+", "-", text_ascii)
    return re.sub(r"-+", "-", text_hyphenated).strip("-")

# 🔍 Describe index once to get valid namespaces
valid_namespaces = set(index.describe_index_stats().namespaces.keys())

# 📦 Pick a valid row
row = df.iloc[19]  # 0-based index, so 19 = 20th row
if row is None:
    raise ValueError("❌ No matching row found for cbo_1dig priorities")

egresso_id = str(row["IDX_EIXCUR"])
egresso_text = f"{row['Denominacao_Curso_CNCT']} | {row['Perfil_CNCT']} | {row['Campo_CNCT']}"
eixo_norm = normalize_namespace(row["Eixo_Tecnologico_CNCT"])
egresso_namespace = f"egresso-{eixo_norm}"

# ✅ Confirm namespace exists
if egresso_namespace not in valid_namespaces:
    raise ValueError(f"❌ Namespace '{egresso_namespace}' not found in Pinecone index")

# ✅ Confirm ID exists
response = index.fetch(ids=[egresso_id], namespace=egresso_namespace)
if egresso_id not in response.vectors:
    raise ValueError(f"❌ Vector ID '{egresso_id}' not found in namespace '{egresso_namespace}'")

# --- Embed query ---
###########################################
query_vec = embed_model.encode(egresso_text).tolist()


# Define target to query
###############################################
# Normalize namespace values for ocupacao:: (cbo_gragru)
# These are the namespaces we will query from ocupações

# Just grab existing occupation-related namespaces directly from Pinecone
target_ns_list = [
    ns for ns in valid_namespaces
    if ns.startswith("ocupacao-")
]

if not target_ns_list:
    raise ValueError("❌ No valid 'ocupacao' namespaces found in index")


## Now execute the query with payload to pinecone
######################################
#  Prepare an empty list to collect matches
results_all = []

# Loop through each ocupacao-* namespace
for ns in target_ns_list:
    try:
        res = index.query(vector=query_vec, namespace=ns, top_k=50, include_metadata=True)
        for match in res.matches:
            results_all.append({
                "cbo_6dig": match.id,
                "score_cosine": match.score,
                "ocup_text": " | ".join([match.metadata[k] for k in ["Ocupacao", "Sintese", "Perfil"] if k in match.metadata])
            })
    except Exception as e:
        print(f"⚠️ Skipping namespace {ns}: {e}")

if not results_all:
    print("⚠️ No results returned from vector query")


###########################################################
# --- Convert final list to DataFrame and sort by cosine score ---
df_results = pd.DataFrame(results_all)
df_results = df_results.sort_values(by="score_cosine", ascending=False).reset_index(drop=True)

# --- Add egresso (course) metadata ---
df_results["IDX_EIXCUR"] = egresso_id
df_results["Denominacao_Curso_CNCT"] = row["Denominacao_Curso_CNCT"]

# --- Ensure matching types for merge ---
df_results["cbo_6dig"] = df_results["cbo_6dig"].astype(str)
df["cbo_6dig"] = df["cbo_6dig"].astype(str)

# --- Merge detailed occupation metadata from df (deduplicated) ---
df_dedup = df.drop_duplicates(subset="cbo_6dig")[["cbo_6dig", "Ocupação", "Síntese", "PerfilOcupacional"]]
df_results = df_results.merge(df_dedup, how="left", on="cbo_6dig")

# --- Final display columns ---
df_results = df_results[[
    "IDX_EIXCUR", "Denominacao_Curso_CNCT", "cbo_6dig", "Ocupação",
    "score_cosine"
]]

# --- Display the result ---
pd.set_option("display.max_colwidth", 100)
print(df_results)

