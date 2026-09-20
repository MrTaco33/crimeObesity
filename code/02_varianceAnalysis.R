# ── 02_analisisVarianza.R ─────────────────────────────────────────────────────
# Auditoría de categorías ENIGH con alta varianza en densidad calórica
# Objetivo: identificar si la SD refleja heterogeneidad real o error de mapeo
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(readr)
library(knitr)
library(kableExtra)
library(glue)

# ── 1. Leer datos ──────────────────────────────────────────────────────────────
smae     <- read_csv("data/processed/smae.csv")
catalogo <- read_csv("data/processed/catalogoCruzadoENIGH.csv",
                     locale = locale(encoding = "latin1"))

# ── 2. Reconstruir catalogoConKcal (mismo pipeline que 01_) ───────────────────
smaeEstandar <- smae %>%
  mutate(kcal_por_kg = (energia_kcal / peso_neto_g) * 1000)

catalogoExpandido <- catalogo %>%
  separate_rows(smae_id, sep = ",\\s*") %>%
  mutate(smae_id = as.integer(smae_id))

catalogoConKcal <- catalogoExpandido %>%
  left_join(
    smaeEstandar %>% select(clave_smae = clave, alimento, kcal_por_kg),
    by = c("smae_id" = "clave_smae")
  )

# ── 3. Identificar top N categorías por SD ────────────────────────────────────
N_TOP <- 30

resumenKcal <- catalogoConKcal %>%
  filter(!is.na(claveAntes)) %>%   # <-- agregar esta línea
  group_by(clave, concepto) %>%
  summarise(
    n_items_smae = sum(!is.na(kcal_por_kg)),
    kcal_media   = mean(kcal_por_kg, na.rm = TRUE),
    kcal_sd      = sd(kcal_por_kg,   na.rm = TRUE),
    .groups = "drop"
  )

top_claves <- resumenKcal %>%
  filter(n_items_smae >= 2) %>%
  arrange(desc(kcal_sd)) %>%
  slice_head(n = N_TOP) %>%
  pull(clave)









# ── 4. Desglose: un renglón por ítem SMAE en cada categoría top ───────────────
desglose <- catalogoConKcal %>%
  filter(clave %in% top_claves) %>%
  select(clave, concepto, smae_id, alimento, kcal_por_kg) %>%
  left_join(
    resumenKcal %>% select(clave, kcal_media, kcal_sd, n_items_smae),
    by = "clave"
  ) %>%
  arrange(desc(kcal_sd), clave, desc(kcal_por_kg)) %>%
  mutate(
    desviacion_vs_media = round(kcal_por_kg - kcal_media, 1),
    kcal_por_kg         = round(kcal_por_kg, 1),
    kcal_media          = round(kcal_media, 1),
    kcal_sd             = round(kcal_sd, 1)
  )

# ── 5. Imprimir en consola (lectura rápida) ────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  AUDITORÍA DE MAPEOS — TOP", N_TOP, "CATEGORÍAS POR SD\n")
cat("══════════════════════════════════════════════════════════════\n\n")

for (cl in top_claves) {
  meta <- resumenKcal %>% filter(clave == cl)
  rows <- desglose    %>% filter(clave == cl)

  cat(glue("▸ [{cl}] {meta$concepto}"),       "\n")
  cat(glue("  n={meta$n_items_smae}  ",
           "media={meta$kcal_media} kcal/kg  ",
           "SD={meta$kcal_sd}\n"))
  cat("  ─────────────────────────────────────────────────────\n")

  rows %>%
    select(smae_id, alimento, kcal_por_kg, desviacion_vs_media) %>%
    { cat(format(capture.output(print(as.data.frame(.), row.names = FALSE)),
                 width = 80), sep = "\n") }

  cat("\n")
}

# ── 6. Guardar CSV de auditoría ────────────────────────────────────────────────
write_csv(desglose, "output/auditoria_alta_varianza.csv")
cat("──────────────────────────────────────────────────────────────\n")
cat("CSV guardado en: output/auditoria_alta_varianza.csv\n")
