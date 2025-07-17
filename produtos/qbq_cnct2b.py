# qbq_cnct2b.py

# Check query and rerank test

from pinecone import Pinecone
from sentence_transformers import SentenceTransformer
from transformers import AutoTokenizer, AutoModelForSequenceClassification
from torch.nn.functional import softmax
import torch
import pandas as pd
import os
import unicodedata
import re
from collections import Counter
import pprint

# --- Load Pinecone ---
pc = Pinecone(api_key=os.getenv("PINECONE_API_KEY"))
index = pc.Index("cnct-qbq")

# --- Load models ---
embed_model = SentenceTransformer("intfloat/e5-large-v2", device="cuda")
tokenizer = AutoTokenizer.from_pretrained("cross-encoder/ms-marco-MiniLM-L12-v2")
reranker = AutoModelForSequenceClassification.from_pretrained("cross-encoder/ms-marco-MiniLM-L12-v2").cuda()

# --- Load df ---
df = pd.read_excel("D:/Country/Brazil/TechBrazil/working/qbq/df_censo_cbo_matched.xlsx")





# --- Pick an example egresso ---
def normalize_namespace(text):
    text_ascii = unicodedata.normalize("NFKD", text).encode("ASCII", "ignore").decode()
    text_ascii = text_ascii.lower()
    text_hyphenated = re.sub(r"[^a-z0-9]+", "-", text_ascii)
    return re.sub(r"-+", "-", text_hyphenated).strip("-")

def find_first_available_row(df, priorities=[3, 4, 5, 6, 7, 8, 9]):
    for grp in priorities:
        subset = df[df["cbo_1dig"] == grp]
        if not subset.empty:
            return subset.iloc[0]
    return None

# 🔍 Describe index once to get valid namespaces
valid_namespaces = set(index.describe_index_stats().namespaces.keys())

# 📦 Pick a valid row
row = find_first_available_row(df)
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
query_vec = embed_model.encode(egresso_text).tolist()

# --- Query ocupacao namespaces starting with técnicos (cbo_1dig = 3) ---
# Normalize namespace values for ocupacao:: (cbo_gragru)
# --- Normalize and validate ocupacao namespaces ---
target_ns_list_raw = df[df.cbo_1dig == 3]["cbo_gragru"].dropna().unique()

target_ns_list = []
for val in target_ns_list_raw:
    val_ascii = unicodedata.normalize("NFKD", val).encode("ascii", "ignore").decode().lower()
    val_norm = re.sub(r"[^a-z0-9]+", "-", val_ascii).strip("-")
    ns = f"ocupacao-{val_norm}"
    if ns in valid_namespaces:
        target_ns_list.append(ns)
    else:
        print(f"⚠️ Skipping namespace '{ns}' — not present in index")

if not target_ns_list:
    raise ValueError("❌ No valid 'ocupacao' namespaces found in index for cbo_1dig == 3")


results_all = []

for ns in target_ns_list:
    try:
        res = index.query(vector=query_vec, namespace=ns, top_k=10, include_metadata=True)
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


# --- Rerank with CrossEncoder ---
pairs = [(egresso_text, r["ocup_text"]) for r in results_all]

if not pairs:
    raise ValueError("❌ No candidate pairs to rerank.")

encodings = tokenizer(pairs, padding=True, truncation=True, return_tensors="pt").to("cuda")
with torch.no_grad():
    logits = reranker(**encodings).logits.squeeze().tolist()

# Ensure logits is a list
if isinstance(logits, float):
    logits = [logits]

# Attach raw reranked scores to results_all
for i, score in enumerate(logits):
    results_all[i]["score_reranked"] = score

# --- Normalize reranked scores over all results ---
min_score = min(logits)
max_score = max(logits)
for r in results_all:
    r["score_reranked_norm"] = (r["score_reranked"] - min_score) / (max_score - min_score + 1e-8)

# --- Deduplicate by cbo_6dig: keep highest scoring entry per code ---
seen = {}
for r in results_all:
    code = r["cbo_6dig"]
    if code not in seen or r["score_reranked"] > seen[code]["score_reranked"]:
        seen[code] = r

# --- Convert to DataFrame and sort ---
df_results = pd.DataFrame(seen.values())
df_results = df_results.sort_values(by="score_reranked_norm", ascending=False).reset_index(drop=True)

# --- Add egresso metadata ---
df_results["IDX_EIXCUR"] = egresso_id
df_results["Denominacao_Curso_CNCT"] = row["Denominacao_Curso_CNCT"]

# --- Merge occupation info from df ---
df_results["cbo_6dig"] = df_results["cbo_6dig"].astype(str)
df["cbo_6dig"] = df["cbo_6dig"].astype(str)

df_results = df_results.merge(
    df[["cbo_6dig", "Ocupação", "Síntese", "PerfilOcupacional"]],
    how="left",
    on="cbo_6dig"
)

# --- Final column order ---
df_results = df_results[[
    "IDX_EIXCUR", "Denominacao_Curso_CNCT", "cbo_6dig", "Ocupação",
    "score_cosine", "score_reranked", "score_reranked_norm"
]]

# --- Display or export ---
pd.set_option("display.max_colwidth", 100)
print(df_results)
