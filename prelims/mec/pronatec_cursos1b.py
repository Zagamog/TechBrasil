# pronatec_cursos1b.py


# Load TOC with curso_id, course_name, page_number
import pandas as pd
import fitz  # PyMuPDF
import re

# Load the TOC file (previously saved)
df_list_pronatec2016 = pd.read_csv("D:/Country/Brazil/TechBrazil/working/mec/df_list_pronatec2016.csv")

# Open the detailed course catalog PDF
with open("D:/Country/Brazil/TechBrazil/rawdata/mec/catalogo_cursos_pronatec_fic_2016.pdf", "rb") as f:
    catalog_doc = fitz.open("pdf", f.read())

# Prepare storage for extracted fields
detail_rows = []


# Define field extraction patterns
field_patterns = {
    "codigo_curso": r"^Código do Curso:\s*(\d+)",
    "eixo_tecnologico": r"^Eixo Tecnológico:\s*(.+)",
    "escolaridade_minima": r"^Escolaridade Mínima:\s*(.+)",
    "perfil_profissional": r"^Perfil Profissional:\s*(.*?)^(Idade:|Outros pré-requisitos:|Ocupações Associadas|Observação:)",
    "idade": r"^Idade:\s*(.*?)^(Outros pré-requisitos:|Ocupações Associadas|Observação:)",
    "outros_pre_requisitos": r"^Outros pré-requisitos:\s*(.*?)^(Ocupações Associadas|Observação:)",
    "ocupacoes_cbo": r"^Ocupações Associadas \(CBO\):\s*(.*?)^(Observação:)",
    "observacao": r"^Observação:\s*(.*?)^(\\d{2,4})\s*Horas|$",
    "carga_horaria": r"(\d{2,4})\s*Horas"
}


# Iterate through TOC entries and extract course detail pages
for _, row in df_list_pronatec2016.iterrows():
    pg_num = int(row["page_number"]) - 1
    text = ""
    for offset in [0, 1]:
        if pg_num + offset < len(catalog_doc):
            text += catalog_doc[pg_num + offset].get_text()

    parsed = {
        "curso_id": row["curso_id"],
        "course_name": row["course_name"],
        "page_number": row["page_number"]
    }
    for field, pattern in field_patterns.items():
        match = re.search(pattern, text, re.DOTALL | re.MULTILINE)
        parsed[field] = match.group(1).strip() if match and match.group(1) else "NA"



    detail_rows.append(parsed)

# Create DataFrame
df_pronatec_detail = pd.DataFrame(detail_rows)

# Save for reuse
df_pronatec_detail.to_csv("D:/Country/Brazil/TechBrazil/working/mec/df_pronatec_detail.csv", index=False)
