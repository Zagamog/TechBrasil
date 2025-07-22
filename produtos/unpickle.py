import pandas as pd

path = "D:/Country/Brazil/TechBrazil/working/qbq/tfidf_vectors_qbq.pkl"
df_check = pd.read_pickle(path)

print("📄 Type of loaded object:", type(df_check))
print("🧱 Columns:", df_check.columns.tolist())

# Inspect one row
first_vec = df_check.tfidf_vector.iloc[0]
print("🔍 Type of first tfidf_vector element:", type(first_vec))
print("🔍 Content (first 5 items):", list(first_vec.items())[:5] if hasattr(first_vec, "items") else first_vec[:5])
