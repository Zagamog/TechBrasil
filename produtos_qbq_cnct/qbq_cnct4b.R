# qbq_cnct4b.R

library(aws.s3)
library(dotenv)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(janitor)
library(stringr)
library(stringi)
library(purrr)
library(tidyr)
library(reticulate)
library(tidytext)
library(stopwords)


# Load the course file
load("D:/Country/Brazil/TechBrazil/working/mec_outros/df_cnct2025b.rda")

# Load qbq file
load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.rda")

# Prepare curso blob
df_cnct2025b <- df_cnct2025b %>%
  mutate(
    curso_blob = paste(
      Eixo_Tecnologico_CNCT_cleaned,
      Area_Tecnologica_CNCT_cleaned,
      Denominacao_Curso_CNCT,
      Perfil_Profissional_CNCT,
      Campo_de_Atuacao_CNCT,
      sep = " "
    )
  ) %>%
  select(IDX_EIXARECUR, curso_blob)

# Now save the blob
py_save_object(df_cnct2025b, "D:/Country/Brazil/TechBrazil/working/qbq/df_cnct2025b_blob.pkl")


# Now TF-IDF

# Tokenize and clean
tfidf_cnct <- df_cnct2025b %>%
  unnest_tokens(word, curso_blob, token = "words") %>%        # Split text into words
  filter(str_detect(word, "[a-zA-Z]")) %>%                     # Keep only words (exclude numbers/punct)
  count(IDX_EIXARECUR, word, sort = TRUE) %>%                  # Count word frequency per course
  bind_tf_idf(word, IDX_EIXARECUR, n) %>%                      # Compute TF-IDF
  arrange(desc(tf_idf))

# Stop words

# Get Portuguese stopwords
pt_stopwords <- stopwords("pt")

# Remove stopwords from tokenized table
tfidf_cnct_nostop <- df_cnct2025b %>%
  unnest_tokens(word, curso_blob, token = "words") %>%
  filter(str_detect(word, "[a-zA-Z]")) %>%
  filter(!word %in% pt_stopwords) %>%
  count(IDX_EIXARECUR, word, sort = TRUE) %>%
  bind_tf_idf(word, IDX_EIXARECUR, n) %>%
  arrange(desc(tf_idf))


# # For each course, keep top 100 or all if fewer
# top_words_per_course <- tfidf_cnct_nostop %>%
#   group_by(IDX_EIXARECUR) %>%
#   slice_max(tf_idf, n = 100, with_ties = FALSE) %>%
#   ungroup() %>% arrange(IDX_EIXARECUR, desc(tf_idf)) %>% relocate (tf_idf, .after = word)



# Step 2: For each course, take top 100 TF-IDF words and build named vector
# tfidf_vectors_cnct <- tfidf_cnct_nostop %>%
#   group_by(IDX_EIXARECUR) %>%
#   slice_max(tf_idf, n = 100, with_ties = FALSE) %>%
#   summarise(tfidf_vector = list(setNames(tf_idf, word))) %>%
#   ungroup()

# Check the length of each vector
# map_int(tfidf_vectors_cnct$tfidf_vector, length)

tfidf_vectors_cnct <- df_cnct2025b %>%
  unnest_tokens(word, curso_blob, token = "words") %>%
  filter(str_detect(word, "[a-zA-Z]")) %>%
  filter(!word %in% pt_stopwords) %>%
  count(IDX_EIXARECUR, word, sort = TRUE) %>%
  bind_tf_idf(word, IDX_EIXARECUR, n) %>%
  group_by(IDX_EIXARECUR) %>%
  slice_max(tf_idf, n = 100, with_ties = FALSE) %>%
  summarise(tfidf_vector = list(set_names(as.list(tf_idf), word))) %>%
  ungroup()



# Now save the tfidf_vector in pickle
# 2. Save for Python
reticulate::py_save_object(tfidf_vectors_cnct, "D:/Country/Brazil/TechBrazil/working/qbq/tfidf_vectors_cnct.pkl")



## Now qbq side

# Prepare occupation blob
qbq_ocup_cmento1 <- qbq_ocup_cmento1 %>%
  mutate(
    ocup_blob = paste(
      cbo_familia,
      Ocupação,
      PerfilOcupacional,
      sep = " "
    )
  ) %>%
  select(CodCBO, ocup_blob)

# Save as Python-compatible pickle
py_save_object(qbq_ocup_cmento1, "D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1_blob.pkl")


# Remove stopwords from tokenized table
tfidf_qbq_nostop <- qbq_ocup_cmento1 %>%
  unnest_tokens(word, ocup_blob, token = "words") %>%
  filter(str_detect(word, "[a-zA-Z]")) %>%
  filter(!word %in% pt_stopwords) %>%
  count(CodCBO, word, sort = TRUE) %>%
  bind_tf_idf(word, CodCBO, n) %>%
  arrange(desc(tf_idf))


# Tokenize and clean QBQ
tfidf_vectors_qbq <- qbq_ocup_cmento1 %>%
  unnest_tokens(word, ocup_blob, token = "words") %>%
  filter(str_detect(word, "[a-zA-Z]")) %>%
  filter(!word %in% pt_stopwords) %>%
  count(CodCBO, word, sort = TRUE) %>%
  bind_tf_idf(word, CodCBO, n) %>%
  group_by(CodCBO) %>%
  slice_max(tf_idf, n = 100, with_ties = FALSE) %>%
  summarise(tfidf_vector = list(set_names(as.list(tf_idf), word))) %>%
  ungroup()


# Save as pickle for use in Python
py_save_object(tfidf_vectors_qbq, "D:/Country/Brazil/TechBrazil/working/qbq/tfidf_vectors_qbq.pkl")





