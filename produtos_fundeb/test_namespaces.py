# test_namespaces.py
import pandas as pd
import unicodedata
import re

def normalize_namespace(text):
    text_ascii = unicodedata.normalize("NFKD", text).encode("ASCII", "ignore").decode()
    text_ascii = text_ascii.lower()
    text_hyphenated = re.sub(r"[^a-z0-9]+", "-", text_ascii)
    return re.sub(r"-+", "-", text_hyphenated).strip("-")

def is_valid_namespace(text):
    return re.fullmatch(r"[a-z0-9\-]+", text) is not None

# Load DataFrame
df = pd.read_excel("D:/Country/Brazil/TechBrazil/working/qbq/df_censo_cbo_matched.xlsx")

# Unique values to be used as namespace components
eixo_values = df["Eixo_Tecnologico_CNCT"].dropna().unique()
gragru_values = df["cbo_gragru"].dropna().unique()

# Collect results
bad_eixos = []
bad_gragrus = []
valid_egresso_ns = []
valid_ocup_ns = []

# Check EIXO_TECNOLÓGICO_CNCT
for val in eixo_values:
    norm = normalize_namespace(val)
    if is_valid_namespace(norm):
        valid_egresso_ns.append(f"egresso-{norm}")
    else:
        bad_eixos.append((val, norm))

# Check cbo_gragru
for val in gragru_values:
    norm = normalize_namespace(val)
    if is_valid_namespace(norm):
        valid_ocup_ns.append(f"ocupacao-{norm}")
    else:
        bad_gragrus.append((val, norm))

# Report
print("\n🔎 Validation Report:")
if not bad_eixos and not bad_gragrus:
    print("✅ All namespaces are valid for Pinecone.")
else:
    if bad_eixos:
        print("\n❌ Invalid EIXO_TECNOLÓGICO_CNCT entries:")
        for original, norm in bad_eixos:
            print(f"  '{original}' → '{norm}'")
    if bad_gragrus:
        print("\n❌ Invalid cbo_gragru entries:")
        for original, norm in bad_gragrus:
            print(f"  '{original}' → '{norm}'")

# Print valid results
print(f"\n✅ Total valid egresso namespaces: {len(valid_egresso_ns)}")
print("\n".join(valid_egresso_ns))

print(f"\n✅ Total valid ocupacao namespaces: {len(valid_ocup_ns)}")
print("\n".join(valid_ocup_ns))
