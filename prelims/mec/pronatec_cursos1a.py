# pronatec_cursos1a.py

# ronatex course list made into dataframe

import os
import re
import pandas as pd
import pyreadr
import boto3
from botocore.exceptions import NoCredentialsError, ClientError
from dotenv import load_dotenv

# --- Step 1: Load AWS credentials ---
load_dotenv()

aws_access_key = os.getenv("AWS_ACCESS_KEY_ID")
aws_secret_key = os.getenv("AWS_SECRET_ACCESS_KEY")
aws_region = os.getenv("AWS_DEFAULT_REGION", "us-east-1")

bucket_name = "techbrazildata"
s3_key = "rawdata/mec_outros/catalogo_cursos_pronatec_fic_2016.txt"
local_path = "D:/Country/Brazil/TechBrazil/rawdata/mec_outros/catalogo_cursos_pronatec_fic_2016.txt"

# --- Step 2: Download from S3 if not found locally ---
if not os.path.exists(local_path):
    print(f"⚠️ Local file not found. Attempting download from S3...")

    try:
        s3 = boto3.client(
            "s3",
            aws_access_key_id=aws_access_key,
            aws_secret_access_key=aws_secret_key,
            region_name=aws_region,
        )
        os.makedirs(os.path.dirname(local_path), exist_ok=True)
        s3.download_file(bucket_name, s3_key, local_path)
        print(f"✅ Downloaded from S3: {local_path}")
    except (NoCredentialsError, ClientError) as e:
        print(f"❌ Failed to download from S3: {e}")
        raise
else:
    print(f"✅ Using local file: {local_path}")

# --- Step 3: Load and parse catalog text ---
with open(local_path, "r", encoding="latin1") as file:
    text = file.read()

entries = re.split(r"\n(?=\d+\.\s)", text)

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

parsed_courses = []
for entry in entries:
    course_data = {}
    for field, pattern in patterns.items():
        match = re.search(pattern, entry, re.DOTALL)
        course_data[field] = match.group(1).strip() if match else None
    parsed_courses.append(course_data)

df_detailed_pronatec2016 = pd.DataFrame(parsed_courses)
df_detailed_pronatec2016.insert(0, "curso_id", range(1, len(df_detailed_pronatec2016) + 1))

# --- Step 4: Manual fixes ---
manual_names = {
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
}

for cid, name in manual_names.items():
    df_detailed_pronatec2016.loc[df_detailed_pronatec2016["curso_id"] == cid, "curso_nome"] = name

# Apply missing names + carga horaria
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
    521: ("Organizador de Eventos", 180),
    533: ("Pintor de Obras Imobiliárias", 180),
    539: ("Polidor Automotivo", 180)
}

for cid, (name, hours) in manual_missing_fixes.items():
    df_detailed_pronatec2016.loc[df_detailed_pronatec2016["curso_id"] == cid, "curso_nome"] = name
    df_detailed_pronatec2016.loc[df_detailed_pronatec2016["curso_id"] == cid, "carga_horaria"] = str(hours)

# Capitalize curso_nome if lowercase
df_detailed_pronatec2016["curso_nome"] = df_detailed_pronatec2016["curso_nome"].apply(
    lambda x: x[0].upper() + x[1:] if isinstance(x, str) and x[0].islower() else x
)

# --- Step 5: Save cleaned files ---
output_base = "D:/Country/Brazil/TechBrazil/working/mec_outros/df_detailed_pronatec2016"
df_detailed_pronatec2016.to_pickle(output_base + ".pkl")
df_detailed_pronatec2016.to_csv(output_base + ".csv", index=False)
pyreadr.write_rds(output_base + ".rds", df_detailed_pronatec2016)

print("✅ Saved .pkl, .csv, and .rds outputs.")
