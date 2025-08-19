# qbq_cnct6a.R

library(tidyverse)
library(readr)
library(DT)

load("D:/Country/Brazil/TechBrazil/working/qbq/cnct_qbq_matches.rda")
load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_cnct_matches.rda")


load("D:/Country/Brazil/TechBrazil/working/mec_inep/df_exarcu.rda")

cnct_qbq_matches2 <- cnct_qbq_matches
qbq_cnct_matches2 <- qbq_cnct_matches

cnct_qbq_matches2$IDX_EIXCUR = paste0(
  substr(cnct_qbq_matches2$IDX_EIXARECUR, 1, 2), 
  sprintf("%03d", as.numeric(substr(cnct_qbq_matches2$IDX_EIXARECUR, 5, 6)))
)

cnct_qbq_matches2 <- left_join(cnct_qbq_matches2,df_exarcu,by="IDX_EIXCUR") 
cnct_qbq_matches2 <- cnct_qbq_matches2 %>% filter(!is.na(`Eixo Tecnológico`))
cnct_qbq_matches2 %>% select(IDX_EIXARECUR) %>% n_distinct() # 171 courses out of 184 in df_exarcu, 218 in matches 

# 171 * 50 or 8550 rows from original 218 * 50 or 10900 rows

qbq_cnct_matches2$IDX_EIXCUR = paste0(
  substr(qbq_cnct_matches2$IDX_EIXARECUR, 1, 2), 
  sprintf("%03d", as.numeric(substr(qbq_cnct_matches2$IDX_EIXARECUR, 5, 6)))
)

qbq_cnct_matches2 <- left_join(qbq_cnct_matches2,df_exarcu,by="IDX_EIXCUR") 
qbq_cnct_matches2 <- qbq_cnct_matches2 %>% filter(!is.na(`Eixo Tecnológico`))
qbq_cnct_matches2 %>% select(IDX_EIXARECUR) %>% n_distinct() # 171 courses out of 184 in df_exarcu, 218 in matches
qbq_cnct_matches2 %>% select(CodCBO) %>% n_distinct() # 1899 CodCBOs; but some CodCBOs have less than 20 matched courses due to


cnct_qbq_matches2$CodCBO <- as.character(cnct_qbq_matches2$CodCBO)
qbq_cnct_matches2$CodCBO <- as.character(qbq_cnct_matches2$CodCBO)

save(cnct_qbq_matches2, file="D:/Country/Brazil/TechBrazil/working/qbq/cnct_qbq_matches2.rda")
save(qbq_cnct_matches2, file="D:/Country/Brazil/TechBrazil/working/qbq/qbq_cnct_matches2.rda")


















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

library(dplyr)
library(gt)
library(gtExtras)

# Get all 50 rows for one course
df_one <- df_spark1a %>%
  filter(IDX_EIXARECUR == "010101") %>%
  arrange(rank)

# Generate one common sparkline
spark_vec <- df_one$spark[[1]] |> as.numeric()

# Add highlight index column
df_one <- df_one %>%
  mutate(
    spark_vec = list(spark_vec),  # all rows share same vector
    highlight_index = rank        # each CBO has different point highlighted
  )

# Select one row per display (or keep all for listing)
df_one_display <- df_one %>%
  select(rank, CodCBO, Ocupação, final_score, spark_vec, highlight_index)

# Create gt table with red dot at the correct position
gt_tbl <- df_one_display %>%
  gt() %>%
  gt_plt_sparkline(
    column = spark_vec,
    type = "points",
    fig_dim = c(5, 30),
    palette = c("black", rep("transparent", 3), "red"),
    same_limit = TRUE,
    label = FALSE
  )

gt_tbl
