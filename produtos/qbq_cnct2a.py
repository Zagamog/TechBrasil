# qbq_cnct2a.py

# Create 1024 dimensioned index and upsert df_censo_cb_matched columns

import os
import time
import pandas as pd
from dotenv import load_dotenv
from sentence_transformers import SentenceTransformer
from pinecone import Pinecone, ServerlessSpec
from tqdm import tqdm
import unicodedata
import re

def normalize_namespace(text):
    # 1. Remove accents
    text_ascii = unicodedata.normalize("NFKD", text).encode("ASCII", "ignore").decode()
    # 2. Lowercase and replace invalid characters with hyphen
    text_ascii = text_ascii.lower()
    text_hyphenated = re.sub(r"[^a-z0-9]+", "-", text_ascii)
    # 3. Collapse repeated hyphens and strip
    return re.sub(r"-+", "-", text_hyphenated).strip("-")


# --- 1. Setup Pinecone ---
load_dotenv("D:/AdvancedR/knowbankedu/openai/.env")
pc = Pinecone(api_key=os.getenv("PINECONE_API_KEY"))
spec = ServerlessSpec(cloud="aws", region="us-east-1")

# create index
index_name = "cnct-qbq"
pc.create_index(name=index_name, dimension=1024, metric="cosine", spec=spec)
time.sleep(3)
index = pc.Index(index_name)



# --- 2. Load Excel ---
df = pd.read_excel("D:/Country/Brazil/TechBrazil/working/qbq/df_censo_cbo_matched.xlsx")

# --- 3. Load Embedding Model ---
model = SentenceTransformer("intfloat/multilingual-e5-large", device="cuda")



# --- Batch upsert to Pinecone ---



batch_size = 100

# Total rows
n_rows = df.shape[0]

# Loop over DataFrame in batches
for start in tqdm(range(0, n_rows, batch_size), desc="🔄 Upserting batches"):
    end = min(start + batch_size, n_rows)
    batch = df.iloc[start:end]

    egresso_upserts = {}
    ocup_upserts = {}

    for _, row in batch.iterrows():
        # Normalize namespaces
        eixo_norm = normalize_namespace(row["Eixo_Tecnologico_CNCT"])
        grupo_norm = normalize_namespace(row["cbo_gragru"])
        egresso_ns = f"egresso-{eixo_norm}"
        ocup_ns = f"ocupacao-{grupo_norm}"




        # Unique IDs
        egresso_id = str(row["IDX_EIXCUR"])
        ocup_id = str(row["cbo_6dig"])

        # --- EGRESSO ---
        egresso_text = f"{row['Denominacao_Curso_CNCT']} | {row['Perfil_CNCT']} | {row['Campo_CNCT']}"
        egresso_vec = model.encode(egresso_text).tolist()

        # Append to correct namespace
        egresso_upserts.setdefault(egresso_ns, []).append(
            (
                egresso_id,
                egresso_vec,
                {
                    "IDX_EIXCUR": egresso_id,
                    "Curso": row["Denominacao_Curso_CNCT"],
                    "Perfil": row["Perfil_CNCT"],
                    "Campo": row["Campo_CNCT"],
                },
            )
        )

        # --- OCUPAÇÃO ---
        ocup_text = f"{row['Ocupação']} | {row['Síntese']} | {row['PerfilOcupacional']}"
        ocup_vec = model.encode(ocup_text).tolist()

        ocup_upserts.setdefault(ocup_ns, []).append(
            (
                ocup_id,
                ocup_vec,
                {
                    "CBO": ocup_id,
                    "Ocupacao": row["Ocupação"],
                    "Sintese": row["Síntese"],
                    "Perfil": row["PerfilOcupacional"],
                },
            )
        )

    # --- UPSERT ALL for this batch ---
    for ns, vectors in egresso_upserts.items():
        index.upsert(vectors=vectors, namespace=ns)
    for ns, vectors in ocup_upserts.items():
        index.upsert(vectors=vectors, namespace=ns)

print("✅ All batches upserted successfully.")


# # To confirm the vector exists
# response = index.fetch(ids=["2001"], namespace="egresso::controle_e_processos_industriais")
# print(response)
