# techbrasil_awsui.R

library(aws.s3)
library(dotenv)

# Load environment variables (ensure this is called once per session)
dotenv::load_dot_env()

bucket_name <- "techbrazildata"

# Function: download file from S3 if not already downloaded
update_data_from_s3 <- function(local_path, s3_path, bucket) {
  if (!file.exists(local_path)) {
    tryCatch({
      save_object(object = s3_path, bucket = bucket, file = local_path)
      message("✅ File downloaded from S3: ", local_path)
    }, error = function(e) {
      message("❌ Error downloading from S3: ", e$message)
    })
  } else {
    message("✅ Using local version: ", local_path)
  }
}

# Function: upload processed file to S3
upload_processed_data <- function(local_path, s3_path, bucket) {
  tryCatch({
    put_object(file = local_path, object = s3_path, bucket = bucket)
    message("✅ Uploaded to S3: ", s3_path)
  }, error = function(e) {
    message("❌ Error uploading to S3: ", e$message)
  })
}
