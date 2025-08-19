# qbq_cnct5c.py

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
tfidf_qbq  = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/qbq/tfidf_vectors_qbq.pkl")

dict_cnct = dict(zip(tfidf_cnct.IDX_EIXARECUR.astype(str), tfidf_cnct.tfidf_vector))
dict_qbq  = dict(zip(tfidf_qbq.CodCBO.astype(str), tfidf_qbq.tfidf_vector))
curso_ids = list(dict_cnct.keys())

# --- 3. Cosine similarity helper ---
def cosine_similarity_named_dicts(d1, d2):
    keys = list(set(d1.keys()) | set(d2.keys()))
    v1 = np.array([d1.get(k, 0.0) for k in keys])
    v2 = np.array([d2.get(k, 0.0) for k in keys])
    if np.linalg.norm(v1) == 0 or np.linalg.norm(v2) == 0:
        return 0.0
    return float(cosine_similarity([v1], [v2])[0][0])

# --- 4. Reverse Matching: QBQ → CNCT ---
results = []

for ocup_id, ocup_vec in tqdm(dict_qbq.items(), desc="🔁 Matching QBQ → CNCT"):
    tfidf_scores = {
        curso_id: cosine_similarity_named_dicts(ocup_vec, dict_cnct[curso_id])
        for curso_id in curso_ids
    }

    # Fetch occupation embedding
    try:
        ocup_embed = index.fetch(ids=[ocup_id], namespace="ocupacao").vectors[ocup_id].values
    except KeyError:
        continue

    # Query semantic similarity to CNCT
    try:
        sem_result = index.query(vector=ocup_embed, namespace="curso", top_k=2000, include_values=False)
        sem_scores = {match["id"]: match["score"] for match in sem_result["matches"]}
    except Exception as e:
        print(f"❌ Pinecone query error for {ocup_id}: {e}")
        continue

    # Combine scores
    hybrid_scores = {
        curso_id: tfidf_scores.get(curso_id, 0.0) * sem_scores.get(curso_id, 0.0)
        for curso_id in curso_ids
    }

    top_matches = sorted(hybrid_scores.items(), key=lambda x: -x[1])[:20]

    for curso_id, final_score in top_matches:
        results.append({
            "CodCBO": ocup_id,
            "IDX_EIXARECUR": curso_id,
            "final_score": final_score,
            "semantic": sem_scores.get(curso_id, 0.0),
            "tfidf": tfidf_scores.get(curso_id, 0.0)
        })

# --- 5. Save results ---
df_results = pd.DataFrame(results)
output_base = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_cnct_matches"
df_results.to_pickle(output_base + ".pkl")
df_results.to_csv(output_base + ".csv", index=False)

print("✅ Saved top 20 matches per occupation to .pkl and .csv.")
