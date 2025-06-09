# pronatec_cursos1a.py

import pandas as pd
import re
import pyreadr


# Text file version of Pronatec catalog

# Try reading the file using 'latin1' encoding instead of 'utf-8'
with open("D:/Country/Brazil/TechBrazil/rawdata/mec/catalogo_cursos_pronatec_fic_2016.txt", "r", encoding="latin1") as file:
    text = file.read()


# Split text into individual course entries using a numbered course pattern
entries = re.split(r"\n(?=\d+\.\s)", text)

# Define extraction regex patterns
patterns = {
    "curso_nome": r"^\d+\.\s+(.*?)\s+\d+\s+Horas",
    "carga_horaria": r"(\d{2,4})\s+Horas",
    "codigo_curso": r"C[oó]digo do Curso:\s*(\d+)",
    "eixo_tecnologico": r"Eixo Tecnol[oó]gico:\s*(.*?)\s*Escolaridade M[ií]nima:",
    "escolaridade_minima": r"Escolaridade M[ií]nima:\s*(.*?)\s*Perfil Profissional:",
    "perfil_profissional": r"Perfil Profissional:\s*(.*?)\s*(?:Idade:|Outros pr[eé]-requisitos:|Ocupações Associadas|Observa[cç][aã]o:)",
    "idade": r"Idade:\s*(.*?)\s*(?:Outros pr[eé]-requisitos:|Ocupações Associadas|Observa[cç][aã]o:)",
    "outros_pre_requisitos": r"Outros pr[eé]-requisitos:\s*(.*?)\s*(?:Ocupações Associadas|Observa[cç][aã]o:)",
    "ocupacoes_cbo": r"Ocupações Associadas \(CBO\):\s*(.*?)\s*(?:Observa[cç][aã]o:|$)",
    "observacao": r"Observa[cç][aã]o:\s*(.*)"
}

# Parse each course entry
parsed_courses = []
for entry in entries:
    course_data = {}
    for field, pattern in patterns.items():
        match = re.search(pattern, entry, re.DOTALL)
        course_data[field] = match.group(1).strip() if match else None
    parsed_courses.append(course_data)

# Create DataFrame
df_detailed_pronatec2016 = pd.DataFrame(parsed_courses)

# Insert curso_id as a running index
df_detailed_pronatec2016.insert(0, "curso_id", range(1, len(df_detailed_pronatec2016) + 1))


# Apply manual fixes to curso_nome for specific curso_id values
for curso_id, new_name in {
    7: "Administrador de Empreendimentos Florestais de Base Comunitária",
    19: "Agente de Inclusão Digital em Centros Públicos de Acesso à Internet",
    87: "Assistente de Planejamento, Programação e Controle de Produção",
    120: "Auxiliar de Transporte, Movimentação e Distribuição de Cargas",
    167: "Condutor de Turismo em Unidades de Conservação Ambiental Local",
    296: "Instalador e Reparador de Equipamentos de Transmissão em Telefonia",
    301: "Instalador e Reparador de Redes, Cabos e Equipamentos Telefônicos",
    309: "Introdução à Interpretação em Língua Brasileira de Sinais (Libras)",
    322: "Língua Portuguesa e Cultura Brasileira para Estrangeiros – Básico",
    323: "Língua Portuguesa e Cultura Brasileira para Estrangeiros – Intermediário",
    324: "Língua Portuguesa e Cultura Brasileira para Surdos – Básico",
    325: "Língua Portuguesa e Cultura Brasileira para Surdos – Intermediário",
    375: "Mecânico de Sistemas de Freios, Suspensão e Direção de Veículos Rodoviários Pesados",
    453: "Operador de Fresadora com Comando Numérico Computadorizado",
    463: "Operador de Máquinas com Comando Numérico Computadorizado para Madeiras e Derivados",
    465: "Operador de Máquinas de Linha de Abertura, Cardas e Preparação de Fiação",
    489: "Operador de Processos de Acabamento em Mármores e Granitos",
    498: "Operador de Produção em Unidade de Tratamento de Resíduos",
    519: "Operador e Programador de Sistemas Automatizados de Soldagem",
    555: "Produtor de Frutas e Hortaliças Processadas com Uso de Acidificação",
    556: "Produtor de Frutas e Hortaliças Processadas com Uso do Frio",
    557: "Produtor de Frutas e Hortaliças Processadas pelo Uso de Calor",
    558: "Produtor de Frutas, Hortaliças e Plantas Aromáticas Processadas por Secagem e Desidratação",
    559: "Produtor de Hortaliças e Plantas Aromáticas Processadas com Uso de Sal",
    597: "Revitalizador de Estruturas, Elementos e Construções em Metal",
    607: "Soldador de Estruturas e Tubulação em Aço Carbono no Processo TIG",
    612: "Soldador no Processo Eletrodo Revestido Aço Carbono e Aço Baixa Liga"
}.items():
    df_detailed_pronatec2016.loc[df_detailed_pronatec2016["curso_id"] == curso_id, "curso_nome"] = new_name

# Return affected rows for review
manually_fixed_rows = df_detailed_pronatec2016[df_detailed_pronatec2016["curso_id"].isin([7, 19, 87, 120, 167, 296, 301, 309, 322, 323, 324, 325, 375, 453, 463, 465, 489, 498, 519, 555, 556, 557, 558, 559, 597, 607, 612])]

manual_missing_fixes = {
    51: ("Algicultor", 180),
    62: ("Armador de Estruturas Pesadas", 180),
    63: ("Armador de Ferragem", 180),
    75: ("Assistente de Controle de Qualidade", 180),
    90: ("Assistente de Secretaria Escolar", 180),
    114: ("Auxiliar de Manutenção Predial", 180),
    161: ("Colorista Automotivo", 180),
    193: ("Cravejador de Joias", 180),
    200: ("Cumim", 180),
    241: ("Espanhol Aplicado a Serviços Turísticos", 180),
    257: ("Francês Aplicado a Serviços Turísticos", 180),
    282: ("Inglês Aplicado a Serviços Turísticos", 180),
    307: ("Instrutor de Trânsito", 180),
    326: ("Lixador-Esmerilhador", 180),
    329: ("Maçariqueiro", 180),
    351: ("Matrizeiro de Solados", 180),
    377: ("Mecânico de Transmissão Automática Automotiva", 180),
    378: ("Mecânico de Transmissão de Veículos Rodoviários Pesados", 180),
    379: ("Mecânico de Transmissão Manual Automotiva", 180),
    427: ("Operador de Abastecimento de Aeronaves", 180),
    487: ("Operador de Processos Cerâmicos", 180),
    500: ("Operador de Rampa de Aeronaves", 180),
    521: ("Organizador de Eventos", 180),  # confirmed in image
    533: ("Pintor de Obras Imobiliárias", 180),  # confirmed in image
    539: ("Polidor Automotivo", 180)  # confirmed in image
}


# Apply the fixes to the DataFrame
for cid, (name, hours) in manual_missing_fixes.items():
    df_detailed_pronatec2016.loc[df_detailed_pronatec2016["curso_id"] == cid, "curso_nome"] = name
    df_detailed_pronatec2016.loc[df_detailed_pronatec2016["curso_id"] == cid, "carga_horaria"] = str(hours)

# Get curso_id values for rows where curso_nome or carga_horaria is None
# none_rows_ids = df_detailed_pronatec2016[
#     df_detailed_pronatec2016["curso_nome"].isna() | df_detailed_pronatec2016["carga_horaria"].isna()
# ]["curso_id"].tolist()

# none_rows_ids
# Capitalize first letter of curso_nome if lowercase
df_detailed_pronatec2016["curso_nome"] = df_detailed_pronatec2016["curso_nome"].apply(
    lambda x: x[0].upper() + x[1:] if isinstance(x, str) and x[0].islower() else x
)



# Save as .pkl
df_detailed_pronatec2016.to_pickle("D:/Country/Brazil/TechBrazil/working/mec/df_detailed_pronatec2016.pkl")

# Save updated DataFrame
df_detailed_pronatec2016.to_csv("D:/Country/Brazil/TechBrazil/working/mec/df_detailed_pronatec2016.csv", index=False)

# Load the DataFrame back from the pickle file
df_detailed_pronatec2016 = pd.read_pickle("D:/Country/Brazil/TechBrazil/working/mec/df_detailed_pronatec2016.pkl")



# Save as .rds with a single object (e.g., a DataFrame)
pyreadr.write_rds("D:/Country/Brazil/TechBrazil/working/mec/df_detailed_pronatec2016.rds", df_detailed_pronatec2016)

# Test

# Select the specific curso_ids requested
selected_ids = [213, 65, 453,557]
selected_rows = df_detailed_pronatec2016[df_detailed_pronatec2016["curso_id"].isin(selected_ids)]

# Convert to JSON
selected_json = selected_rows.to_dict(orient="records")
selected_json

# hardcoded_obs = {
#     252: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
#     307: "O curso só poderá ser ofertado por instituições credenciadas pelo DETRAN.",
#     345: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
#     346: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
#     348: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
#     529: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
#     530: "O curso só poderá ser ofertado por unidade acreditada pela Marinha do Brasil, por intermédio da Diretoria de Portos e Costas.",
#     642: "O curso só poderá ser ofertado por unidade autorizada pelo Ministério da Justiça, por intermédio do Departamento de Polícia Federal."
# }
