# qbq_cnct6a.R

## FOR SPARKLINES ON MATCHES

library(tidyverse)
library(readr)
library(DT)

# --- Step 1: Load match results and course/occupation metadata ---
df_matches <- read_csv("D:/Country/Brazil/TechBrazil/working/qbq/cnct_qbq_matches.csv")

df_cnct2025a <- read_rds("D:/Country/Brazil/TechBrazil/working/mec_outros/df_cnct2025a.rds")         # df_cnct2025a
load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.rda")            # qbq_ocup_cmento1

# --- Step 2: Ensure types match ---
df_matches <- df_matches %>%
  mutate(
    IDX_EIXARECUR = as.character(IDX_EIXARECUR),
    CodCBO = as.character(CodCBO)
  )

# --- Step 3: Create rank and sparkline vectors ---
spark_df <- df_matches %>%
  group_by(IDX_EIXARECUR) %>%
  arrange(desc(final_score)) %>%
  mutate(
    rank = row_number(),
    spark = list(final_score)  # will be duplicated per group
  ) %>%
  ungroup()

# --- Step 4: Merge with course and occupation names ---
df_spark1a <- spark_df %>%
  left_join(df_cnct2025a %>%
              select(IDX_EIXARECUR, `Denominação do Curso`),
            by = "IDX_EIXARECUR") %>%
  left_join(qbq_ocup_cmento1 %>%
              select(CodCBO, Ocupação),
            by = "CodCBO") %>%
  relocate(IDX_EIXARECUR, `Denominação do Curso`, CodCBO, Ocupação, final_score, rank, spark) %>%
  arrange(IDX_EIXARECUR, rank) 

# --- Step 5: Save as .rda ---
save(df_spark1a, file = "D:/Country/Brazil/TechBrazil/working/qbq/df_spark1a.rda")


library(dplyr)
library(gt)
library(gtExtras)

# --- Load your saved df_spark1a ---
load("D:/Country/Brazil/TechBrazil/working/qbq/df_spark1a.rda")

df_one <- df_spark1a %>%
  filter(IDX_EIXARECUR == "010101") %>%
  arrange(rank) %>%
  mutate(
    spark_vec = lapply(spark, function(x) head(as.numeric(x), 10))  # keep first 10
  )

gt_tbl <- df_one %>%
  select(rank, CodCBO, Ocupação, final_score, spark_vec) %>%
  gt() %>%
  gt_plt_sparkline(
    column = spark_vec,
    type = "ref_last",                       # <- adds red dot at final point
    fig_dim = c(5, 30),
    palette = c("black", "red", "transparent", "transparent", "transparent"),
    same_limit = TRUE,
    label = TRUE
  )

gt_tbl