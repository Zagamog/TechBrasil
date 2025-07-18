# qbq_cnct3a.py

import pandas as pd
from tqdm import tqdm
import unicodedata
import re
from pinecone import Pinecone
from sentence_transformers import SentenceTransformer
from dotenv import load_dotenv
import os

# --- Load Pinecone ---

# --- 1. Setup Pinecone ---
load_dotenv("D:/AdvancedR/knowbankedu/openai/.env")
pc = Pinecone(api_key=os.getenv("PINECONE_API_KEY"))
index = pc.Index("cnct-qbq")

# --- Load models ---
model = SentenceTransformer("intfloat/multilingual-e5-large", device="cuda")


# --- 1. Utility: normalize namespace strings ---
def normalize_namespace(text):
    text_ascii = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode().lower()
    return re.sub(r"[^a-z0-9]+", "-", text_ascii).strip("-")

# --- 2. Load data and prep ---
df = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/qbq/df_censo_cbo_matched.pkl")
df["cbo_6dig"] = df["cbo_6dig"].astype(str)

# --- 3. Load Pinecone index + model beforehand ---
# Assume: index = pc.Index("...") and model = SentenceTransformer(...) already exist

valid_namespaces = set(index.describe_index_stats().namespaces.keys())

# --- 4. Egressos 
egressos = df["IDX_EIXCUR"].unique()
df = df[df["IDX_EIXCUR"].isin(egressos)].copy()
all_results = []

for idx in tqdm(egressos, desc="🔁 Processing egressos (sample)"):
    row = df[df["IDX_EIXCUR"] == idx].iloc[0]

    egresso_text = f"{row['Denominacao_Curso_CNCT']} | {row['Perfil_CNCT']} | {row['Campo_CNCT']}"
    eixo_norm = normalize_namespace(row["Eixo_Tecnologico_CNCT"])
    egresso_vec = model.encode(egresso_text).tolist()

    results = []
    for ns in [ns for ns in valid_namespaces if ns.startswith("ocupacao-")]:
        try:
            res = index.query(vector=egresso_vec, namespace=ns, top_k=20, include_metadata=True)
            for match in res.matches:
                results.append({
                    "IDX_EIXCUR": idx,
                    "Denominacao_Curso_CNCT": row["Denominacao_Curso_CNCT"],
                    "cbo_6dig": match.id,
                    "score_cosine": match.score,
                    "ocup_text": " | ".join([match.metadata.get(k, "") for k in ["Ocupacao", "Sintese", "Perfil"]])
                })
        except Exception as e:
            print(f"⚠️ Skipping namespace {ns}: {e}")
    all_results.extend(results)

# --- 5. Combine all results into a DataFrame ---
df_cosine_top10 = pd.DataFrame(all_results)
df_cosine_top10["cbo_6dig"] = df_cosine_top10["cbo_6dig"].astype(str)

# --- 6. Merge with ground-truth matches ---
df_eval = df_cosine_top10.merge(
    df[["IDX_EIXCUR", "cbo_6dig"]],
    how="left",
    on=["IDX_EIXCUR", "cbo_6dig"],
    indicator=True
)

df_eval["is_match"] = df_eval["_merge"] == "both"

# --- 7. Compute mean cosine score for correctly matched items only ---
mean_cosine_for_true_matches = df_eval[df_eval["is_match"]]["score_cosine"].mean()

# --- 8. Prepare ground-truth cbo_list per IDX_EIXCUR ---
df_truth = df.groupby("IDX_EIXCUR").agg({
    "Denominacao_Curso_CNCT": "first",
    "cbo_6dig": lambda x: list(x)
}).rename(columns={"cbo_6dig": "cbo_list"}).reset_index()

# --- 9. Group top-10 predictions ---
df_top10_grouped = df_cosine_top10.groupby("IDX_EIXCUR").agg({
    "cbo_6dig": list,
    "score_cosine": list
}).rename(columns={"cbo_6dig": "predicted_cbos", "score_cosine": "predicted_scores"}).reset_index()

# --- 10. Merge truth with predictions ---
df_summary = df_truth.merge(df_top10_grouped, on="IDX_EIXCUR", how="left")
# --- Ensure empty lists for missing predictions ---
df_summary["predicted_cbos"] = df_summary["predicted_cbos"].apply(lambda x: x if isinstance(x, list) else [])
df_summary["predicted_scores"] = df_summary["predicted_scores"].apply(lambda x: x if isinstance(x, list) else [])

# --- Compute evaluation columns ---
def evaluate(row):
    cbo_list = set(row["cbo_list"])
    predicted = row["predicted_cbos"]
    predicted_scores = row["predicted_scores"]

    predicted_map = dict(zip(predicted, predicted_scores))
    found = cbo_list & set(predicted)

    in_top10 = len(found)
    out_top10 = len(cbo_list - found)
    total = len(cbo_list)

    if found:
        mean_score = sum(predicted_map[c] for c in found) / in_top10
    else:
        mean_score = 0.0

    return pd.Series({
        "mean_cosine": round(mean_score, 6),
        "InTop10": in_top10,
        "OutTop10": out_top10,
        "FraIN_Top10": round(in_top10 / total, 2) if total else 0,
        "FraOUT_Top10": round(out_top10 / total, 2) if total else 0,
        "matched_cbos": sorted(found)
    })

# --- Apply and append results ---
df_summary = df_summary.join(df_summary.apply(evaluate, axis=1))

# --- Reorder display columns ---
df_summary = df_summary[[
    "IDX_EIXCUR", "Denominacao_Curso_CNCT", "cbo_list", "matched_cbos",
    "mean_cosine", "InTop10", "OutTop10", "FraIN_Top10", "FraOUT_Top10"
]]
# sort
df_summary = df_summary.sort_values(by="mean_cosine", ascending=False).reset_index(drop=True)

df_summary.to_pickle("D:/Country/Brazil/TechBrazil/working/qbq/df_cnct_cbo_atual.pkl")

# --- Export full summary to Excel ---
excel_path = "D:/Country/Brazil/TechBrazil/working/qbq/df_cnct_cbo_resultado_completo.xlsx"
df_summary.to_excel(excel_path, index=False)
print(f"✅ Full results written to {excel_path}")
