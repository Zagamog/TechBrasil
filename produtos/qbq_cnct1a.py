# qbq_cnct1a.py

# Just unpickles the cnct/censo and qbq pickle files 

import os
import pandas as pd
from openai import OpenAI
import boto3
from botocore.exceptions import NoCredentialsError, ClientError
from dotenv import load_dotenv


# --- Step 1: Load AWS credentials from .env ---
load_dotenv()

aws_access_key = os.getenv("AWS_ACCESS_KEY_ID")
aws_secret_key = os.getenv("AWS_SECRET_ACCESS_KEY")
aws_region = os.getenv("AWS_DEFAULT_REGION", "us-east-1")

bucket_name = "techbrazildata"
s3_key = "working/qbq/qbq_ocup_cmento1.pkl"
local_path = "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.pkl"


## Load Openai keys
dotenv_path = "D:/AdvancedR/knowbankedu/openai/.env"
#load_dotenv()
load_dotenv(dotenv_path)
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
os.environ["OPENAI_API_KEY"] = OPENAI_API_KEY
client = OpenAI()




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

# --- Step 3: Load pickle into DataFrame ---
try:
    df_qbq_ocup_cmento1 = pd.read_pickle(local_path)
    print(f"📦 Loaded DataFrame with shape: {df_qbq_ocup_cmento1.shape}")
except Exception as e:
    print(f"❌ Failed to load pickle file: {e}")
    raise
    
    
s3_key = "working/qbq/df_censo_supl_tec_4qbq.pkl"
local_path = "D:/Country/Brazil/TechBrazil/working/qbq/df_censo_supl_tec_4qbq.pkl"

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

df_censo_supl_tec_4qbq = pd.read_pickle(local_path)
   