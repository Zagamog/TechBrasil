# cnct1a.py

# Creates dataframe and csv of CNCT technical courses catalogue

import os
import pandas as pd
import pyreadr
import boto3
from botocore.exceptions import NoCredentialsError, ClientError
from dotenv import load_dotenv

# --- Step 1: Load AWS credentials securely from .env ---
load_dotenv()

aws_access_key = os.getenv("AWS_ACCESS_KEY_ID")
aws_secret_key = os.getenv("AWS_SECRET_ACCESS_KEY")
aws_region = os.getenv("AWS_DEFAULT_REGION", "us-east-1")

bucket_name = "techbrazildata"
s3_key = "rawdata/mec_outros/catalogo_cnct.csv"
local_path = "D:/Country/Brazil/TechBrazil/rawdata/mec_outros/catalogo_cnct.csv"

# --- Step 2: Download from S3 if file not found locally ---
if not os.path.exists(local_path):
    print(f"⚠️ File not found locally: {local_path}\n→ Attempting download from S3...")

    try:
        s3 = boto3.client(
            "s3",
            aws_access_key_id=aws_access_key,
            aws_secret_access_key=aws_secret_key,
            region_name=aws_region,
        )

        os.makedirs(os.path.dirname(local_path), exist_ok=True)
        s3.download_file(bucket_name, s3_key, local_path)
        print(f"✅ Downloaded from S3 to: {local_path}")

    except (NoCredentialsError, ClientError) as e:
        print(f"❌ Failed to download from S3: {e}")
        raise

else:
    print(f"✅ Using local file: {local_path}")

# --- Step 3: Load CSV and inspect ---
df_full = pd.read_csv(local_path, delimiter=";", encoding="ISO-8859-1")

# --- Step 4: Create course_id using hierarchical codes ---
df_full = df_full.copy()
eixo_map = {eixo: f"{i+1:02d}" for i, eixo in enumerate(df_full['Eixo Tecnológico'].unique())}
df_full['eixo_code'] = df_full['Eixo Tecnológico'].map(eixo_map)

df_full['eixo_area_key'] = df_full['eixo_code'] + "||" + df_full['Área Tecnológica']
area_codes = (
    df_full[['eixo_area_key']]
    .drop_duplicates()
    .reset_index(drop=True)
    .assign(area_index=lambda d: d.groupby('eixo_area_key').ngroup() + 1)
)
area_codes['area_code'] = area_codes['area_index'].apply(lambda x: f"{x:02d}")
df_full = df_full.merge(area_codes[['eixo_area_key', 'area_code']], on='eixo_area_key', how='left')

df_full['eixo_area_course_key'] = df_full['eixo_area_key'] + "||" + df_full['Denominação do Curso']
curso_codes = (
    df_full[['eixo_area_key', 'Denominação do Curso']]
    .drop_duplicates()
    .groupby('eixo_area_key')
    .cumcount() + 1
)
df_full['curso_code'] = curso_codes.apply(lambda x: f"{x:02d}")
df_full['course_id'] = df_full['eixo_code'] + df_full['area_code'] + df_full['curso_code']

# --- Step 5: Reorder and finalize output ---
original_columns = [
    'Eixo Tecnológico', 'Área Tecnológica', 'Denominação do Curso',
    'Perfil Profissional de Conclusão', 'Carga Horária Mínima',
    'Descrição Carga Horária Mínima', 'Pré-Requisitos para Ingresso',
    'Itinerários Formativos', 'Campo de Atuação', 'Ocupações CBO Associadas',
    'Infraestrutura Mínima', 'Legislação Profissional'
]

df_ordered = df_full[['course_id'] + original_columns]
df_ordered['eixo_code'] = df_full['eixo_code']
df_ordered['area_code'] = df_full['area_code']
df_ordered['curso_code'] = df_full['curso_code']

# --- Step 6: Save outputs ---
output_base = "D:/Country/Brazil/TechBrazil/working/mec_outros/df_cnct2025a"
df_ordered.to_pickle(output_base + ".pkl")
pyreadr.write_rds(output_base + ".rds", df_ordered)
df_ordered.to_csv(output_base + ".csv", index=False)

print("✅ Saved as .pkl, .rds, and .csv")
