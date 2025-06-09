# pronatec_cursos1b.py


# Load TOC with curso_id, course_name, page_number
import pandas as pd
import fitz  # PyMuPDF
import re

# Load existing TOC list (preserves correct curso_id + names)
df_detailed_pronatec2016 = pd.read_csv("D:/Country/Brazil/TechBrazil/working/mec/df_list_pronatec2016.csv")

# Open the detailed course catalog PDF
with open("D:/Country/Brazil/TechBrazil/rawdata/mec/catalogo_cursos_pronatec_fic_2016.pdf", "rb") as f:
    catalog_doc = fitz.open("pdf", f.read())

# Define field extraction patterns
field_patterns = {
    "codigo_curso": r"C[oó]digo do Curso:\s*(\d+)",
    "eixo_tecnologico": r"Eixo Tecnol[oó]gico:\s*(.*?)\n",
    "escolaridade_minima": r"Escolaridade M[ií]nima:\s*(.*?)\n",
    "perfil_profissional": r"Perfil Profissional:\s*(.*?)\s*(?:Idade:|Outros pr[eé]-requisitos:|Ocupações Associadas|Observa[cç][aã]o:)",
    "idade": r"Idade:\s*(.*?)\s*(?:Outros pr[eé]-requisitos:|Ocupações Associadas|Observa[cç][aã]o:)",
    "outros_pre_requisitos": r"Outros pr[eé]-requisitos:\s*(.*?)\s*(?:Ocupações Associadas|Observa[cç][aã]o:)",
    "ocupacoes_cbo": r"Ocupações Associadas \(CBO\):\s*(.*?)\s*(?:Observa[cç][aã]o:)",
    "observacao": r"Observa[cç][aã]o:\s*(.*?)(?:\n|$)",
    "carga_horaria": r"(\d{2,4})\s*Horas"
}

# Create empty columns for each target field
target_fields = list(field_patterns.keys())
for col in target_fields:
    df_detailed_pronatec2016[col] = None

# Process each course
for idx, row in df_detailed_pronatec2016.iterrows():
    pg_num = int(row["page_number"]) - 1
    text = ""
    for offset in [0, 1]:
        if pg_num + offset < len(catalog_doc):
            text += catalog_doc[pg_num + offset].get_text()

    for field, pattern in field_patterns.items():
        match = re.search(pattern, text, re.DOTALL)
        value = match.group(1).strip() if match else None

        # Always clear observacao unless in hardcoded list
        if field == "observacao" and row["curso_id"] not in {252, 307, 345, 346, 348, 529, 530, 642}:
            value = None

        df_detailed_pronatec2016.at[idx, field] = value

# Hardcode full observacao text for the known 8 cases by curso_id
# These strings are from verified entries, stripped of asterisks
hardcoded_obs = {
    252: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
    307: "O curso só poderá ser ofertado por instituições credenciadas pelo DETRAN.",
    345: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
    346: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
    348: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
    529: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
    530: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
    642: "O curso só poderá ser ofertado por unidade autorizada pelo Ministério da Justiça, por intermédio do Departamento de Polícia Federal."
}

for cid, obs in hardcoded_obs.items():
    df_detailed_pronatec2016.loc[df_detailed_pronatec2016["curso_id"] == cid, "observacao"] = obs















# Save updated DataFrame
df_detailed_pronatec2016.to_csv("D:/Country/Brazil/TechBrazil/working/mec/df_detailed_pronatec2016.csv", index=False)