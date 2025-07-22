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

# --- 2. Load TF-IDF dict-based vectors ---
tfidf_cnct = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/qbq/tfidf_vectors_cnct.pkl")
tfidf_qbq = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/qbq/tfidf_vectors_qbq.pkl")

dict_cnct = dict(zip(tfidf_cnct.IDX_EIXARECUR.astype(str), tfidf_cnct.tfidf_vector))
dict_qbq = dict(zip(tfidf_qbq.CodCBO.astype(str), tfidf_qbq.tfidf_vector))
ocup_ids = list(dict_qbq.keys())

# --- 3. Cosine helper for named dicts ---
def cosine_similarity_named_dicts(d1, d2):
    keys = list(set(d1.keys()) | set(d2.keys()))
    v1 = np.array([d1.get(k, 0.0) for k in keys])
    v2 = np.array([d2.get(k, 0.0) for k in keys])
    if np.linalg.norm(v1) == 0 or np.linalg.norm(v2) == 0:
        return 0.0
    return float(cosine_similarity([v1], [v2])[0][0])

# --- 4. Elbow + fallback cutoff ---
def get_cutoff(scores, max_matches=20):
    scores = sorted(scores, reverse=True)
    if len(scores) < 2:
        return 0
    diffs = np.diff(scores)
    elbow_pos = np.argmax(diffs) + 1
    if diffs[elbow_pos - 1] > 0.1:
        return scores[elbow_pos]
    else:
        return scores[min(len(scores) - 1, max_matches)]

# --- 5. Matching loop ---
results = []

for curso_id, curso_vec in tqdm(dict_cnct.items(), desc="🔍 Matching CNCT → QBQ"):
    # TF-IDF cosine
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

    # Multiply hybrid scores
    hybrid_scores = {
        ocup_id: tfidf_scores.get(ocup_id, 0.0) * sem_scores.get(ocup_id, 0.0)
        for ocup_id in ocup_ids
    }

    # Select matches above cutoff
    score_values = list(hybrid_scores.values())
    cutoff = get_cutoff(score_values)
    top_matches = [(ocup, score) for ocup, score in hybrid_scores.items() if score >= cutoff]
    top_matches = sorted(top_matches, key=lambda x: -x[1])

    for ocup_id, final_score in top_matches:
        results.append({
            "IDX_EIXARECUR": curso_id,
            "CodCBO": ocup_id,
            "final_score": final_score,
            "semantic": sem_scores.get(ocup_id, 0.0),
            "tfidf": tfidf_scores.get(ocup_id, 0.0)
        })

# --- 6. Save output ---
df_results = pd.DataFrame(results)
df_results.to_pickle("D:/Country/Brazil/TechBrazil/working/qbq/cnct_qbq_matches.pkl")

print("✅ Hybrid matching completed and results saved.")
