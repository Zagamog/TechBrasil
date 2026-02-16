# five_sectors_analysis1f.R
#
# Total Employment from PNAD-C Raw Microdata
# Using the downloaded .rds files from pnad_ocups1a.R
#
# This calculates ALL employed persons (not just EPT graduates)
# to get the true Brasil employment total

library(tidyverse)
library(data.table)

cat("=============================================================================\n")
cat("  PNAD-C TOTAL EMPLOYMENT (All Workers - Raw Microdata)                     \n")
cat("=============================================================================\n\n")

# =============================================================================
# STEP 1: LOAD RAW PNAD-C MICRODATA
# =============================================================================

cat("=== Loading raw PNAD-C microdata ===\n")

raw_dir <- "D:/Country/Brazil/TechBrazil/rawdata/pnad"
files <- list.files(raw_dir, pattern = "\\.rds$", full.names = TRUE)

cat(sprintf("Found %d files:\n", length(files)))
print(basename(files))

# Load all years
raw <- map_dfr(files, readRDS)

cat(sprintf("\nTotal rows: %s\n", format(nrow(raw), big.mark = ",")))
cat(sprintf("Years: %s\n", paste(unique(raw$Ano), collapse = ", ")))

# =============================================================================
# STEP 2: PREPARE VARIABLES
# =============================================================================

cat("\n=== Preparing variables ===\n")

raw2 <- raw %>% 
  mutate(
    V1028 = as.numeric(V1028),  # Weight
    VD4002_num = case_when(
      VD4002 == "Pessoas ocupadas" ~ 1,
      VD4002 == 1 ~ 1,
      TRUE ~ 0
    ),
    emprego_formal = case_when(
      VD4009 %in% c("Empregado no setor privado com carteira de trabalho assinada",
                    "Trabalhador doméstico com carteira de trabalho assinada",
                    "Empregado no setor público com carteira de trabalho assinada",
                    "Militar e servidor estatutário") ~ 1,
      VD4009 == "Empregador" & V4019 == 1 ~ 1,
      VD4009 == "Conta-própria" & VD4013 == 1 ~ 1,
      TRUE ~ 0
    )
  )

# =============================================================================
# STEP 3: CALCULATE BRASIL TOTALS BY YEAR (ALL WORKERS)
# =============================================================================

cat("\n=== Calculating Brasil totals (ALL workers) ===\n")

brasil_by_year <- raw2 %>%
  group_by(Ano) %>%
  summarise(
    Ocupado = sum(VD4002_num * V1028, na.rm = TRUE),
    Ocupado_Formal = sum(VD4002_num * emprego_formal * V1028, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Ocupado_Informal = Ocupado - Ocupado_Formal,
    Taxa_Formalidade = Ocupado_Formal / Ocupado,
    Taxa_Formalidade_Pct = round(Taxa_Formalidade * 100, 1)
  )

cat("\nBRASIL TOTAL EMPLOYMENT BY YEAR (All Workers):\n")
print(brasil_by_year %>% 
        mutate(
          `Total (M)` = round(Ocupado / 1e6, 1),
          `Formal (M)` = round(Ocupado_Formal / 1e6, 1),
          `Informal (M)` = round(Ocupado_Informal / 1e6, 1)
        ) %>%
        select(Ano, `Total (M)`, `Formal (M)`, `Informal (M)`, Taxa_Formalidade_Pct))

# =============================================================================
# STEP 4: BY UF (LATEST YEAR)
# =============================================================================

cat("\n=== By UF (Latest Year) ===\n")

latest_year <- max(raw2$Ano)

by_uf <- raw2 %>%
  filter(Ano == latest_year) %>%
  group_by(UF) %>%
  summarise(
    Ocupado = sum(VD4002_num * V1028, na.rm = TRUE),
    Ocupado_Formal = sum(VD4002_num * emprego_formal * V1028, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Ocupado_Informal = Ocupado - Ocupado_Formal,
    Taxa_Formalidade_Pct = round(Ocupado_Formal / Ocupado * 100, 1)
  ) %>%
  arrange(desc(Ocupado))

cat(sprintf("\nTop 10 UFs by Employment (%s):\n", latest_year))
print(by_uf %>% 
        head(10) %>%
        mutate(
          `Total (M)` = round(Ocupado / 1e6, 2),
          `Formal (M)` = round(Ocupado_Formal / 1e6, 2),
          `Informal (M)` = round(Ocupado_Informal / 1e6, 2)
        ) %>%
        select(UF, `Total (M)`, `Formal (M)`, `Informal (M)`, Taxa_Formalidade_Pct))

# =============================================================================
# STEP 5: COMPARISON - ALL vs EPT GRADUATES
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  COMPARISON: ALL WORKERS vs EPT GRADUATES                                  \n")
cat("=============================================================================\n\n")

# EPT graduates only (same filter as original script)
ept_only <- raw2 %>%
  mutate(
    concluiu_curso_tec_prof = case_when(
      V3023A == "Sim" | V3032 == "Sim" ~ 1, 
      TRUE ~ 0
    )
  ) %>%
  filter(VD3004 != "Superior completo", concluiu_curso_tec_prof == 1) %>%
  group_by(Ano) %>%
  summarise(
    Ocupado_EPT = sum(VD4002_num * V1028, na.rm = TRUE),
    .groups = "drop"
  )

comparison <- brasil_by_year %>%
  left_join(ept_only, by = "Ano") %>%
  mutate(
    Pct_EPT = round(Ocupado_EPT / Ocupado * 100, 1)
  )

cat("ALL WORKERS vs EPT GRADUATES:\n")
print(comparison %>%
        mutate(
          `All Workers (M)` = round(Ocupado / 1e6, 1),
          `EPT Graduates (M)` = round(Ocupado_EPT / 1e6, 1)
        ) %>%
        select(Ano, `All Workers (M)`, `EPT Graduates (M)`, Pct_EPT))

# =============================================================================
# STEP 6: FINAL SUMMARY
# =============================================================================

cat("=============================================================================\n")
cat(sprintf("  FINAL SUMMARY (Latest Year: %s)                                           \n", latest_year))
cat("=============================================================================\n\n")

latest <- brasil_by_year %>% filter(Ano == latest_year)
latest_ept <- ept_only %>% filter(Ano == latest_year)

cat(sprintf("BRASIL TOTAL EMPLOYMENT (PNAD-C Q2 %s):\n\n", latest_year))
cat(sprintf("  ALL WORKERS:\n"))
cat(sprintf("    Total:    %.1f M\n", latest$Ocupado / 1e6))
cat(sprintf("    Formal:   %.1f M (%.1f%%)\n", latest$Ocupado_Formal / 1e6, latest$Taxa_Formalidade_Pct))
cat(sprintf("    Informal: %.1f M (%.1f%%)\n", latest$Ocupado_Informal / 1e6, 100 - latest$Taxa_Formalidade_Pct))
cat(sprintf("\n  EPT GRADUATES (excl. higher ed):\n"))
cat(sprintf("    Total:    %.1f M (%.1f%% of all workers)\n", 
            latest_ept$Ocupado_EPT / 1e6, 
            latest_ept$Ocupado_EPT / latest$Ocupado * 100))

cat("\n=== Done ===\n")