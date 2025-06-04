# pronatec_cursos1a.py

import re
import pandas as pd
import fitz  # PyMuPDF
import pyreadr

    
#  Pronatec Course TOC
# Step 1: Extract text from PDF
with open("D:/Country/Brazil/TechBrazil/rawdata/mec/listagem_cursos_pronatec_fic_2016.pdf", "rb") as f:
    doc = fitz.open("pdf", f.read())

text = "\n".join(page.get_text() for page in doc)

# Step 2: Parse course entries like "46. Ajustador Mecânico .................... 33"
pattern = re.compile(r"^\d+\.\s+(.*?)\.{3,}\s*(\d{1,3})$", re.MULTILINE)
matches = pattern.findall(text)

# Step 3: Create DataFrame
df_list_pronatec2016 = pd.DataFrame(matches, columns=["course_name", "page_number"])
df_list_pronatec2016["page_number"] = df_list_pronatec2016["page_number"].astype(int)
df_list_pronatec2016.insert(0, "curso_id", range(1, len(df_list_pronatec2016) + 1))
df_list_pronatec2016.reset_index(drop=True, inplace=True)

# Step 4: Save in multiple formats
base_path = "D:/Country/Brazil/TechBrazil/working/mec/df_list_pronatec2016"
df_list_pronatec2016.to_pickle(base_path + ".pkl")
pyreadr.write_rds(base_path + ".rds", df_list_pronatec2016)
df_list_pronatec2016.to_csv(base_path + ".csv", index=False)


# Load the DataFrame back from the pickle file
df_list_pronatec2016 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/mec/df_list_pronatec2016.pkl")  









# Convert to DataFrame
df_fic = pd.DataFrame(data)

# Save CSV and RDS
csv_path = "D:/Country/Brazil/TechBrazil/working/mec/pronatec_fic_2016.csv"
rds_path = "D:/Country/Brazil/TechBrazil/working/mec//pronatec_fic_2016.rds"
df_fic.to_csv(csv_path, index=False)

pyreadr.write_rds(rds_path, df_fic)


