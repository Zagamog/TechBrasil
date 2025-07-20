# qbq_cnct5a.py
# Upsert CNCT curso_blob embeddings into Pinecone under namespace "curso"

import os
import time
import pandas as pd
from dotenv import load_dotenv
from sentence_transformers import SentenceTransformer
from pinecone import Pinecone, ServerlessSpec
from tqdm import tqdm

# --- 1. Setup Pinecone ---
load_dotenv("D:/AdvancedR/knowbankedu/openai/.env")
pc = Pinecone(api_key=os.getenv("PINECONE_API_KEY"))
spec = ServerlessSpec(cloud="aws", region="us-east-1")

index_name = "cnct-qbq2"
pc.create_index(name=index_name, dimension=1024, metric="cosine", spec=spec)
time.sleep(3)
index = pc.Index(index_name)


# --- 2. Load embedding model (only once) ---
model = SentenceTransformer("intfloat/multilingual-e5-large", device="cuda")

# --- 3. Helper: upsert function ---
def upsert_blob(df, id_col, text_col, namespace, id_label):
    batch_size = 100
    for start in tqdm(range(0, len(df), batch_size), desc=f"🔄 Upserting to '{namespace}'"):
        end = min(start + batch_size, len(df))
        batch = df.iloc[start:end]

        upserts = []
        for _, row in batch.iterrows():
            uid = str(row[id_col])
            text = row[text_col]
            vec = model.encode(text).tolist()

            upserts.append((
                uid,
                vec,
                {
                    id_label: uid,
                    text_col: text
                }
            ))
        index.upsert(vectors=upserts, namespace=namespace)

# --- 4. CNCT (curso) ---
df_cnct = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/qbq/df_cnct2025b_blob.pkl")
upsert_blob(df_cnct, id_col="IDX_EIXARECUR", text_col="curso_blob", namespace="curso", id_label="IDX_EIXARECUR")
print("✅ CNCT curso_blob vectors upserted to namespace 'curso'.")

# --- 5. QBQ (ocupacao) ---
df_qbq = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1_blob.pkl")
upsert_blob(df_qbq, id_col="CodCBO", text_col="ocup_blob", namespace="ocupacao", id_label="CodCBO")
print("✅ QBQ ocup_blob vectors upserted to namespace 'ocupacao'.")

