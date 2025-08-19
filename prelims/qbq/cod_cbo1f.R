# cod_cbo1f.R

# importing match cod - cbo
library(aws.s3)
library(readr)
cbo_matching_result4 <- read_csv("working/qbq/cbo_matching_result4.csv")
View(cbo_matching_result4)

cbo_cod_match <- cbo_matching_result4
save(cbo_cod_match, file="working/qbq/cbo_cod_match.rda")
# In AWS
put_object(file = "D:/Country/Brazil/TechBrazil/working/qbq/cbo_cod_match.rda",
           object = "working/qbq/cbo_cod_match.rda",
           bucket = bucket_name)

