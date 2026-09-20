# Librerías
library(tidyverse)
library(readr)
library(knitr)
library(kableExtra)
library(glue)

# ── 1. Leer datos ──────────────────────────────────────────────────────────────
smae     <- read_csv("data/processed/smae.csv")
catalogo <- read_csv("data/processed/catalogoCruzadoENIGH.csv")

# ── 2. Estandarizar SMAE a kcal/kg ────────────────────────────────────────────
smaeEstandar <- smae %>%
  mutate(
    kcal_por_kg = (energia_kcal / peso_neto_g) * 1000
  )

# ── 3. Expandir smae_id (separados por coma) ──────────────────────────────────
catalogoExpandido <- catalogo %>%
  separate_rows(smae_id, sep = ",\\s*") %>%
  mutate(smae_id = as.integer(smae_id))

# ── 4. Join con SMAE estandarizado ────────────────────────────────────────────
catalogoConKcal <- catalogoExpandido %>%
  left_join(
    smaeEstandar %>% select(clave_smae = clave, alimento, kcal_por_kg),
    by = c("smae_id" = "clave_smae")
  )

# ── 5. Resumen X_barra por categoría ENIGH (en kcal/kg) ───────────────────────
resumenKcal <- catalogoConKcal %>%
  group_by(clave, concepto) %>%
  summarise(
    n_items_smae = sum(!is.na(kcal_por_kg)),
    kcal_media   = mean(kcal_por_kg, na.rm = TRUE),
    kcal_sd      = sd(kcal_por_kg,   na.rm = TRUE),
    kcal_se      = if_else(n_items_smae >= 2, kcal_sd / sqrt(n_items_smae), NA_real_),
    kcal_ic_low  = if_else(n_items_smae >= 2, kcal_media - 1.96 * kcal_se,  NA_real_),
    kcal_ic_high = if_else(n_items_smae >= 2, kcal_media + 1.96 * kcal_se,  NA_real_),
    n_flag       = case_when(
      n_items_smae == 0 ~ "no_match",
      n_items_smae == 1 ~ "single_item",
      n_items_smae <  5 ~ "low_n",
      TRUE              ~ "ok"
    ),
    .groups = "drop"
  )

# ── 6. Guardar resumen completo ────────────────────────────────────────────────
write_csv(resumenKcal, "data/processed/resumenXbarra.csv")

# ── 7. Estadísticos para el encabezado ────────────────────────────────────────
n_total    <- nrow(resumenKcal)
n_ok       <- resumenKcal %>% filter(n_flag == "ok")          %>% nrow()
n_low      <- resumenKcal %>% filter(n_flag == "low_n")       %>% nrow()
n_single   <- resumenKcal %>% filter(n_flag == "single_item") %>% nrow()
n_no_match <- resumenKcal %>% filter(n_flag == "no_match")    %>% nrow()

# ── 8. Top 20 por varianza ─────────────────────────────────────────────────────
top20_varianza <- resumenKcal %>%
  filter(n_items_smae >= 2) %>%
  arrange(desc(kcal_sd)) %>%
  slice_head(n = 20) %>%
  select(clave, concepto, n_items_smae, kcal_media, kcal_sd, kcal_ic_low, kcal_ic_high, n_flag) %>%
  mutate(across(c(kcal_media, kcal_sd, kcal_ic_low, kcal_ic_high), ~ round(., 1)))

# ── 9. Tabla HTML ──────────────────────────────────────────────────────────────
tabla_html <- top20_varianza %>%
  kbl(
    format    = "html",
    col.names = c("Code", "Category", "n SMAE", "Mean (kcal/kg)",
                  "SD", "95% CI Low", "95% CI High", "Flag"),
    caption   = glue(
      "<div style='text-align:left; font-family: Arial, sans-serif;'>",
      "<span style='font-size:15px; font-weight:bold; color:#1a3a5c;'>",
      "Top 20 ENIGH Categories by Caloric Density Variance</span><br>",
      "<span style='font-size:12px; color:#555;'>",
      "Caloric density standardized to kcal/kg &nbsp;·&nbsp; ",
      "Total categories: <b>{n_total}</b> &nbsp;·&nbsp; ",
      "OK (n ≥ 5): <b>{n_ok}</b> &nbsp;·&nbsp; ",
      "Low n (2–4): <b>{n_low}</b> &nbsp;·&nbsp; ",
      "Single match: <b>{n_single}</b> &nbsp;·&nbsp; ",
      "No match: <b>{n_no_match}</b>",
      "</span></div>"
    ),
    escape    = FALSE,
    align     = c("l", "l", "c", "r", "r", "r", "r", "c")
  ) %>%
  kable_styling(
    bootstrap_options = c("hover", "condensed", "bordered"),
    full_width        = TRUE,
    font_size         = 13,
    html_font         = "Arial, sans-serif"
  ) %>%
  row_spec(0,
    bold       = TRUE,
    background = "#1a3a5c",
    color      = "white",
    font_size  = 13
  ) %>%
  row_spec(
    which(top20_varianza$n_flag == "low_n"),
    background = "#fff8e1"
  ) %>%
  column_spec(2, width = "25em") %>%
  column_spec(8,
    color = if_else(top20_varianza$n_flag == "ok", "#2e7d32", "#e65100")
  ) %>%
  footnote(
    general           = "Rows highlighted in yellow have fewer than 5 SMAE matches — SD and CI should be interpreted with caution. Categories with a single SMAE match are excluded from this table (no SD computable).",
    general_title     = "Note: ",
    footnote_as_chunk = TRUE
  )

# ── 10. Guardar tabla ──────────────────────────────────────────────────────────
writeLines(as.character(tabla_html), "outputs/top20_kcal_variance.html")

cat("─────────────────────────────────────────\n")
cat("Tabla guardada en:  output/top20_kcal_variance.html\n")
cat("Resumen guardado en: data/processed/resumenXbarra.csv\n")
cat("─────────────────────────────────────────\n")
cat("Total categorías:      ", n_total,    "\n")
cat("Con n >= 5 (ok):       ", n_ok,       "\n")
cat("Con n = 2-4 (low_n):   ", n_low,      "\n")
cat("Con n = 1 (sin SD):    ", n_single,   "\n")
cat("Sin match:             ", n_no_match, "\n")
cat("─────────────────────────────────────────\n")

