

# Join CNCT metadata to CENSO courses using official ID
df_censo_matched <- df_censo_supl_tec_4qbq %>%
  left_join(
    df_cnct2025a %>%
      select(IDX_EIXARECUR, `Eixo Tecnológico`, `Área Tecnológica`,
             `Denominação do Curso`, `Perfil Profissional de Conclusão`,
             `Campo de Atuação`, `Ocupações CBO Associadas`, `Infraestrutura Mínima`),
    by = "IDX_EIXARECUR"
  )
