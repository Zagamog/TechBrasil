###############################################################################
# qbq_cnct_06a.R
#
# PROPAG / Juros por Educação — Preparação de Dados
# Enriquecimento Final do Matching CBO ↔ CNCT
#
# USO NAS ABAS: D2 (APLs via rais_apo_03*.R), E1 (Oferta-Demanda)
#
# OBJETIVO:
#   Enriquecer os resultados brutos do matching semântico (gerados pelo
#   pipeline Python qbq_cnct_*.py — ver qbq_cnct_PIPELINE.md) com:
#     - Labels de Eixo Tecnológico e Área Tecnológica (de df_exarcu.rda)
#     - Filtro para manter apenas cursos válidos no catálogo atual
#     - Padronização de CodCBO como character
#
#   O matching bruto contém ~218 cursos × 50 ocupações (cnct→qbq) e
#   ~1,899 ocupações × 20 cursos (qbq→cnct). Após filtro por cursos
#   válidos no CNCT atual, restam ~171 cursos.
#
#   VER: qbq_cnct_PIPELINE.md para documentação completa do pipeline
#   de matching semântico (Python + Pinecone + embeddings).
#
# INSUMOS:
#   working/qbq/cnct_qbq_matches.rda  (de qbq_cnct5b.py → load_matches1a.R)
#   working/qbq/qbq_cnct_matches.rda  (de qbq_cnct5c.py → load_matches1a.R)
#   working/mec_inep/df_exarcu.rda  (lookup Eixo × Área × Curso)
#
# SAÍDAS (S3):
#   working/qbq/cnct_qbq_matches2.rda  (curso → ocupações, enriquecido)
#   working/qbq/qbq_cnct_matches2.rda  (ocupação → cursos, enriquecido)
#
# DEPENDÊNCIAS: tidyverse
###############################################################################

library(tidyverse)

###############################################################################
# CARREGAR DADOS
###############################################################################

message("=== Enriquecimento do Matching CBO ↔ CNCT ===")

# Matching bruto (gerado pelo pipeline Python)
load("working/qbq/cnct_qbq_matches.rda")  # → cnct_qbq_matches
load("working/qbq/qbq_cnct_matches.rda")  # → qbq_cnct_matches

# Lookup de Eixo Tecnológico × Área × Curso (IDX_EIXCUR → labels)
load("working/mec_inep/df_exarcu.rda")     # → df_exarcu

message(">> cnct_qbq_matches: ", nrow(cnct_qbq_matches), " linhas")
message(">> qbq_cnct_matches: ", nrow(qbq_cnct_matches), " linhas")
message(">> df_exarcu: ", nrow(df_exarcu), " cursos")

###############################################################################
# FUNÇÃO: DERIVAR IDX_EIXCUR DE IDX_EIXARECUR
###############################################################################
#
# IDX_EIXARECUR é o código hierárquico de 6 dígitos (EECCNN) do curso.
# IDX_EIXCUR é o código de 5 dígitos (EENNN) usado no lookup df_exarcu.
# Conversão: primeiros 2 dígitos + últimos 2 como número de 3 dígitos.
# Exemplo: IDX_EIXARECUR = "010103" → IDX_EIXCUR = "01003"
###############################################################################

derivar_idx_eixcur <- function(idx_eixarecur) {
  paste0(
    substr(idx_eixarecur, 1, 2),
    sprintf("%03d", as.numeric(substr(idx_eixarecur, 5, 6)))
  )
}

###############################################################################
# ENRIQUECER cnct_qbq_matches (Curso → Ocupações)
###############################################################################

message(">> Enriquecendo cnct → qbq...")

cnct_qbq_matches2 <- cnct_qbq_matches
cnct_qbq_matches2$IDX_EIXCUR <- derivar_idx_eixcur(cnct_qbq_matches2$IDX_EIXARECUR)

cnct_qbq_matches2 <- left_join(cnct_qbq_matches2, df_exarcu, by = "IDX_EIXCUR")
cnct_qbq_matches2 <- cnct_qbq_matches2 %>%
  filter(!is.na(`Eixo Tecnológico`))

n_cursos_cnct <- cnct_qbq_matches2 %>% select(IDX_EIXARECUR) %>% n_distinct()
message(">> Cursos válidos (cnct→qbq): ", n_cursos_cnct, " de 218")

###############################################################################
# ENRIQUECER qbq_cnct_matches (Ocupação → Cursos)
###############################################################################

message(">> Enriquecendo qbq → cnct...")

qbq_cnct_matches2 <- qbq_cnct_matches
qbq_cnct_matches2$IDX_EIXCUR <- derivar_idx_eixcur(qbq_cnct_matches2$IDX_EIXARECUR)

qbq_cnct_matches2 <- left_join(qbq_cnct_matches2, df_exarcu, by = "IDX_EIXCUR")
qbq_cnct_matches2 <- qbq_cnct_matches2 %>%
  filter(!is.na(`Eixo Tecnológico`))

n_cursos_qbq <- qbq_cnct_matches2 %>% select(IDX_EIXARECUR) %>% n_distinct()
n_cbos_qbq <- qbq_cnct_matches2 %>% select(CodCBO) %>% n_distinct()
message(">> Cursos válidos (qbq→cnct): ", n_cursos_qbq)
message(">> CBOs com matches: ", n_cbos_qbq)

###############################################################################
# PADRONIZAR CodCBO COMO CHARACTER
###############################################################################

cnct_qbq_matches2$CodCBO <- as.character(cnct_qbq_matches2$CodCBO)
qbq_cnct_matches2$CodCBO <- as.character(qbq_cnct_matches2$CodCBO)

###############################################################################
# SALVAR
###############################################################################

save(cnct_qbq_matches2, file = "working/qbq/cnct_qbq_matches2.rda")
save(qbq_cnct_matches2, file = "working/qbq/qbq_cnct_matches2.rda")

message(">> Salvo: working/qbq/cnct_qbq_matches2.rda (",
        nrow(cnct_qbq_matches2), " linhas)")
message(">> Salvo: working/qbq/qbq_cnct_matches2.rda (",
        nrow(qbq_cnct_matches2), " linhas)")
message("=== qbq_cnct_06a.R concluído ===")


## Load to S3

library(aws.s3)
dotenv::load_dot_env()
put_object(file = "working/qbq/cnct_qbq_matches2.rda", 
           object = "working/qbq/cnct_qbq_matches2.rda", bucket = "techbrazildata")
put_object(file = "working/qbq/qbq_cnct_matches2.rda", 
           object = "working/qbq/qbq_cnct_matches2.rda", bucket = "techbrazildata")
put_object(file = "working/mec_inep/df_exarcu.rda", 
           object = "working/mec_inep/df_exarcu.rda", bucket = "techbrazildata")

