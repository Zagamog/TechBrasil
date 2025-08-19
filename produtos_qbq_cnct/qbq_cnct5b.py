# qbq_cnct5b.py

import os
import numpy as np
import pandas as pd
from tqdm import tqdm
from dotenv import load_dotenv
from sklearn.metrics.pairwise import cosine_similarity
from pinecone import Pinecone

# --- 1. Setup ---
load_dotenv("D:/AdvancedR/knowbankedu/openai/.env")
pc = Pinecone(api_key=os.getenv("PINECONE_API_KEY"))
index = pc.Index("cnct-qbq2")

# --- 2. Load TF-IDF vectors ---
tfidf_cnct = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/qbq/tfidf_vectors_cnct.pkl")
tfidf_qbq = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/qbq/tfidf_vectors_qbq.pkl")

dict_cnct = dict(zip(tfidf_cnct.IDX_EIXARECUR.astype(str), tfidf_cnct.tfidf_vector))
dict_qbq = dict(zip(tfidf_qbq.CodCBO.astype(str), tfidf_qbq.tfidf_vector))
ocup_ids = list(dict_qbq.keys())

# --- 3. Cosine similarity helper ---
def cosine_similarity_named_dicts(d1, d2):
    keys = list(set(d1.keys()) | set(d2.keys()))
    v1 = np.array([d1.get(k, 0.0) for k in keys])
    v2 = np.array([d2.get(k, 0.0) for k in keys])
    if np.linalg.norm(v1) == 0 or np.linalg.norm(v2) == 0:
        return 0.0
    return float(cosine_similarity([v1], [v2])[0][0])

# --- 4. Matching loop ---
results = []

for curso_id, curso_vec in tqdm(dict_cnct.items(), desc="🔍 Matching CNCT → QBQ"):
    tfidf_scores = {
        ocup_id: cosine_similarity_named_dicts(curso_vec, dict_qbq[ocup_id])
        for ocup_id in ocup_ids
    }

    # Fetch curso embedding
    try:
        curso_embed = index.fetch(ids=[curso_id], namespace="curso").vectors[curso_id].values
    except KeyError:
        continue

    # Query semantic similarity
    try:
        sem_result = index.query(vector=curso_embed, namespace="ocupacao", top_k=2000, include_values=False)
        sem_scores = {match["id"]: match["score"] for match in sem_result["matches"]}
    except Exception as e:
        print(f"❌ Pinecone query error for {curso_id}: {e}")
        continue

    # Multiply scores and keep top 50
    hybrid_scores = {
        ocup_id: tfidf_scores.get(ocup_id, 0.0) * sem_scores.get(ocup_id, 0.0)
        for ocup_id in ocup_ids
    }

    top_matches = sorted(hybrid_scores.items(), key=lambda x: -x[1])[:50]

    for ocup_id, final_score in top_matches:
        results.append({
            "IDX_EIXARECUR": curso_id,
            "CodCBO": ocup_id,
            "final_score": final_score,
            "semantic": sem_scores.get(ocup_id, 0.0),
            "tfidf": tfidf_scores.get(ocup_id, 0.0)
        })

# --- 5. Save results ---
df_results = pd.DataFrame(results)
output_base = "D:/Country/Brazil/TechBrazil/working/qbq/cnct_qbq_matches"
df_results.to_pickle(output_base + ".pkl")
df_results.to_csv(output_base + ".csv", index=False)

print("✅ Saved top 50 matches per course to .pkl and .csv.")
