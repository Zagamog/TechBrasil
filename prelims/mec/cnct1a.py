# cnct1a.py
import pandas as pd
import pyreadr

# Load the newly uploaded CSV file
csv_path = "D:/Country/Brazil/TechBrazil/rawdata/mec/catalogo_cnct.csv"

# Retry reading with ISO-8859-1 encoding
df_full = pd.read_csv(csv_path, delimiter=";", encoding="ISO-8859-1")

# Extract summary information and a sample for inspection
df_summary = {
    "n_rows": df_full.shape[0],
    "n_columns": df_full.shape[1],
    "columns": list(df_full.columns),
    "sample": df_full.head(2).to_dict(orient="records")
}

df_summary

# Create unique indices for Eixo, then Área within Eixo, then Curso within Área
df_full = df_full.copy()

# Map Eixo to index
eixo_map = {eixo: f"{i+1:02d}" for i, eixo in enumerate(df_full['Eixo Tecnológico'].unique())}
df_full['eixo_code'] = df_full['Eixo Tecnológico'].map(eixo_map)

# Create AA: Área index within Eixo
df_full['eixo_area_key'] = df_full['eixo_code'] + "||" + df_full['Área Tecnológica']
area_codes = (
    df_full[['eixo_area_key']]
    .drop_duplicates()
    .reset_index(drop=True)
    .assign(area_index=lambda d: d.groupby('eixo_area_key').ngroup() + 1)
)
area_codes['area_code'] = area_codes['area_index'].apply(lambda x: f"{x:02d}")
df_full = df_full.merge(area_codes[['eixo_area_key', 'area_code']], on='eixo_area_key', how='left')

# Create ZZ: Curso index within each Área
df_full['eixo_area_course_key'] = df_full['eixo_area_key'] + "||" + df_full['Denominação do Curso']
curso_codes = (
    df_full[['eixo_area_key', 'Denominação do Curso']]
    .drop_duplicates()
    .groupby('eixo_area_key')
    .cumcount() + 1
)
df_full['curso_code'] = curso_codes.apply(lambda x: f"{x:02d}")

# Combine into full ID
df_full['course_id'] = df_full['eixo_code'] + df_full['area_code'] + df_full['curso_code']

# Sample output
df_full[['course_id', 'Eixo Tecnológico', 'Área Tecnológica', 'Denominação do Curso']].head(10)


# Reorder columns to place 'course_id' first, followed by the original 12
original_columns = [
    'Eixo Tecnológico', 'Área Tecnológica', 'Denominação do Curso',
    'Perfil Profissional de Conclusão', 'Carga Horária Mínima',
    'Descrição Carga Horária Mínima', 'Pré-Requisitos para Ingresso',
    'Itinerários Formativos', 'Campo de Atuação', 'Ocupações CBO Associadas',
    'Infraestrutura Mínima', 'Legislação Profissional'
]

df_ordered = df_full[['course_id'] + original_columns]


# Re-append the three auxiliary codes to the end of the cleaned DataFrame
df_final = df_ordered.copy()
df_final['eixo_code'] = df_full['eixo_code']
df_final['area_code'] = df_full['area_code']
df_final['curso_code'] = df_full['curso_code']


# Save the DataFrame as a pickle file for efficient Python use
pickle_path = "D:/Country/Brazil/TechBrazil/working/mec/df_cnct2025a.pkl"
df_final.to_pickle(pickle_path)

# Load the DataFrame back from the pickle file
df_cnct2025a = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/mec/df_cnct2025a.pkl")



# Save as .rds with a single object (e.g., a DataFrame)
pyreadr.write_rds("D:/Country/Brazil/TechBrazil/working/mec/df_cnct2025a.rds", df_cnct2025a)

# Save as CSV (no index column)
df_cnct2025a.to_csv("D:/Country/Brazil/TechBrazil/working/mec/df_cnct2025a.csv", index=False)
