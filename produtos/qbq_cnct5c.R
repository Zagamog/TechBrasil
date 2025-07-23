# qbq_cnct5c.R

# Matching courses and occupations


# From AWS S3 bucket

library(aws.s3)
library(dotenv)


# Load credentials
dotenv::load_dot_env()
bucket_name <- "techbrazildata"


# In AWS
put_object(file = "D:/Country/Brazil/TechBrazil/working/qbq/cnct_qbq_matches.csv",
           object = "working/qbq/cnct_qbq_matches.csv",
           bucket = bucket_name)

