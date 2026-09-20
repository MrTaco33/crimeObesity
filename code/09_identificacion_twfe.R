# ==============================================================================
# 09_identificacion_twfe.R
# Diagnóstico de identificación en la especificación two-way FE
#
# Pregunta: ¿sobre qué municipios está identificado el coeficiente de
# log_tasa_hom en mm2/m5 (| municipio_id + time_id)?
#
# En TWFE, β se estima sobre la variación within-municipio within-tiempo
# (doble demeaning). Municipios que aparecen en un solo período tienen
# X̃_it = 0 y no contribuyen nada. La pregunta es si el coeficiente lo
# impulsan los ~196 municipios con los 6 períodos ENIGH, o si hay
# variación identificante suficiente en los de 2-3 períodos.
#
# Outputs: output/chequeos/id_*.tex  +  resumen en consola
# ==============================================================================

library(tidyverse)
library(fixest)
library(xtable)

dir.create("output/chequeos", recursive = TRUE, showWarnings = FALSE)

tex_out <- function(df, fname, caption, digits = NULL) {
  xt <- xtable(df, caption = caption,
               label   = paste0("tab:", tools::file_path_sans_ext(fname)),
               digits  = digits)
  sink(file.path("output/chequeos", fname))
  print(xt, floating = TRUE, include.rownames = FALSE, booktabs = TRUE,
        caption.placement = "top", comment = FALSE,
        sanitize.text.function = identity,
        sanitize.colnames.function = identity)
  sink()
  invisible(df)
}

# ==============================================================================
# 0. PREPARAR DATOS (igual que 06_regresiones.R)
# ==============================================================================

cat("[0] Preparando datos...\n")

base <- read_csv("data/processed/base_completa.csv", show_col_types = FALSE) %>%
  filter(!is.na(kcal_per_capita), kcal_per_capita > 0,
         !is.na(tasa_homicidios), tasa_homicidios >= 0,
         !is.na(tot_integ), tot_integ > 0) %>%
  mutate(log_kcal_pc  = log(kcal_per_capita),
         log_tasa_hom = log(tasa_homicidios + 1),
         municipio_id = paste(entidad, municipio, sep = "_"),
         time_id      = paste(year, mes, sep = "_")) %>%
  rename(peso = factor)

baseMod <- base %>% filter(year < 2024)

cat("   baseMod: ", nrow(baseMod), "hogares,",
    n_distinct(baseMod$municipio_id), "municipios\n\n")


# ==============================================================================
# ID1: Estructura del panel — cuántos períodos distintos por municipio
# "Período" = año ENIGH (no time_id, para claridad conceptual)
# ==============================================================================

cat("[ID1] Estructura del panel por municipio...\n")

panel_muni <- baseMod %>%
  group_by(municipio_id) %>%
  summarise(
    n_anios     = n_distinct(year),
    n_time_ids  = n_distinct(time_id),
    n_hogares   = n(),
    sd_log_tasa = round(sd(log_tasa_hom), 4),
    rango_tasa  = round(max(log_tasa_hom) - min(log_tasa_hom), 4),
    .groups = "drop"
  )

id1_dist <- panel_muni %>%
  count(n_anios, name = "N_munis") %>%
  mutate(
    pct_munis   = round(N_munis / sum(N_munis) * 100, 1),
    N_hogares   = map_int(n_anios, ~ sum(panel_muni$n_hogares[panel_muni$n_anios == .x])),
    pct_hogares = round(N_hogares / sum(N_hogares) * 100, 1),
    identifican = ifelse(n_anios > 1, "Sí", "No — singleton temporal")
  ) %>%
  arrange(n_anios) %>%
  rename(`N anios ENIGH` = n_anios, `N municipios` = N_munis,
         `pct municipios` = pct_munis, `N hogares` = N_hogares,
         `pct hogares` = pct_hogares, `Contribuye a ID` = identifican)

tex_out(id1_dist, "id_ID1_panel_balance.tex",
        "Distribución de municipios según número de años ENIGH en baseMod (year < 2024)")
print(id1_dist)


# ==============================================================================
# ID2: Variación within-municipio en el tratamiento
# Para municipios con ≥ 2 años: cuánta variación en log_tasa_hom queda
# después de descontar el efecto temporal común (demeaning por time_id)
#
# X̃_it = log_tasa_hom_it − X̄_·t   (demeaning por tiempo)
# Luego: SD de X̃ dentro de cada municipio = variación identificante neta
# ==============================================================================

cat("[ID2] Variacion within-municipio en tratamiento (post-demeaning temporal)...\n")

# Paso 1: descontar media de tiempo (X̄_·t)
baseMod_dm <- baseMod %>%
  group_by(time_id) %>%
  mutate(log_tasa_dm_t = log_tasa_hom - mean(log_tasa_hom)) %>%
  ungroup()

# Paso 2: para cada municipio, SD de la variable parcializada
within_var <- baseMod_dm %>%
  group_by(municipio_id) %>%
  summarise(
    n_anios       = n_distinct(year),
    n_time_ids    = n_distinct(time_id),
    n_hogares     = n(),
    # variación bruta within-municipio (ignora el FE de tiempo)
    sd_bruta      = round(sd(log_tasa_hom), 4),
    # variación identificante: within-municipio DESPUÉS de parcializar tiempo
    sd_neta       = round(sd(log_tasa_dm_t), 4),
    # contribución cuadrática al estimador (∝ Σ X̃²)
    sum_sq_neta   = round(sum(log_tasa_dm_t^2), 4),
    .groups = "drop"
  ) %>%
  # municipios que de facto no identifican (sd_neta ≈ 0)
  mutate(efectivamente_identifica = sd_neta > 1e-8)

id2_por_grupo <- within_var %>%
  group_by(n_anios) %>%
  summarise(
    N_munis              = n(),
    N_munis_sd_pos       = sum(efectivamente_identifica),
    sd_neta_media        = round(mean(sd_neta[efectivamente_identifica]), 4),
    sd_neta_mediana      = round(median(sd_neta[efectivamente_identifica]), 4),
    pct_sum_sq           = round(sum(sum_sq_neta) / sum(within_var$sum_sq_neta) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(n_anios) %>%
  rename(`N anios` = n_anios, `N munis` = N_munis,
         `N munis con var.` = N_munis_sd_pos,
         `SD neta media` = sd_neta_media,
         `SD neta mediana` = sd_neta_mediana,
         `% peso en ID` = pct_sum_sq)

tex_out(id2_por_grupo, "id_ID2_variacion_within_tratamiento.tex",
        "Variación within-municipio en tratamiento (post-demeaning temporal) y peso en la identificación, por número de años ENIGH")
print(id2_por_grupo)


# ==============================================================================
# ID3: Re-estimación de mm2 restringiendo a municipios con ≥ k años
# Cada fila responde: ¿qué pasa con β cuando excluyo a los singletons?
# ¿Y cuando me quedo sólo con los municipios bien representados?
# ==============================================================================

cat("[ID3] Re-estimacion de mm2 por umbral de anos minimos...\n")

# referencia: modelo completo
mm2_full <- feols(log_kcal_pc ~ log_tasa_hom | municipio_id + time_id,
                  weights = ~peso, data = baseMod, cluster = ~municipio_id)

estimar_con_umbral <- function(k) {
  munis_k <- panel_muni %>% filter(n_anios >= k) %>% pull(municipio_id)
  dat     <- baseMod %>% filter(municipio_id %in% munis_k)
  fit     <- feols(log_kcal_pc ~ log_tasa_hom | municipio_id + time_id,
                   weights = ~peso, data = dat, cluster = ~municipio_id)
  tibble(
    k_min_anios  = k,
    N_munis      = n_distinct(dat$municipio_id),
    N_hogares    = nrow(dat),
    beta         = round(coef(fit)["log_tasa_hom"], 4),
    se           = round(se(fit)["log_tasa_hom"], 4),
    pval         = round(pvalue(fit)["log_tasa_hom"], 3),
    ci_lo        = round(confint(fit)["log_tasa_hom", 1], 4),
    ci_hi        = round(confint(fit)["log_tasa_hom", 2], 4)
  )
}

id3 <- map_dfr(1:6, estimar_con_umbral) %>%
  rename(`Min. años` = k_min_anios, `N munis` = N_munis,
         `N hogares` = N_hogares, Beta = beta, SE = se,
         `p-val` = pval, `IC 95% lo` = ci_lo, `IC 95% hi` = ci_hi)

tex_out(id3, "id_ID3_beta_por_umbral_anios.tex",
        "Estimación de $\\hat{\\beta}$ en mm2 restringiendo a municipios con mínimo $k$ años ENIGH en baseMod")
print(id3)


# ==============================================================================
# ID4: Coeficiente SÓLO en cada grupo (municipios con exactamente k años)
# Revela si los singletons temporales arrastran el coeficiente cuando están
# incluidos, aunque no contribuyan al estimador within
# ==============================================================================

cat("[ID4] Estimacion mm2 en cada grupo por separado...\n")

estimar_solo_grupo <- function(k) {
  munis_k <- panel_muni %>% filter(n_anios == k) %>% pull(municipio_id)
  dat     <- baseMod %>% filter(municipio_id %in% munis_k)
  if (n_distinct(dat$municipio_id) < 5) return(NULL)
  fit <- tryCatch(
    feols(log_kcal_pc ~ log_tasa_hom | municipio_id + time_id,
          weights = ~peso, data = dat, cluster = ~municipio_id),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  tibble(
    n_anios   = k,
    N_munis   = n_distinct(dat$municipio_id),
    N_hogares = nrow(dat),
    beta      = round(coef(fit)["log_tasa_hom"], 4),
    se        = round(se(fit)["log_tasa_hom"], 4),
    pval      = round(pvalue(fit)["log_tasa_hom"], 3)
  )
}

id4 <- map_dfr(1:6, estimar_solo_grupo) %>%
  rename(`N años` = n_anios, `N munis` = N_munis, `N hogares` = N_hogares,
         Beta = beta, SE = se, `p-val` = pval)

tex_out(id4, "id_ID4_beta_por_grupo_exacto.tex",
        "Estimación de mm2 en cada subgrupo de municipios por número exacto de años ENIGH")
print(id4)


# ==============================================================================
# ID5: Top municipios por contribución cuadrática a la identificación
# Municipios con mayor Σ X̃² son los que más pesan en β̂
# ==============================================================================

cat("[ID5] Top municipios por peso en la identificacion...\n")

total_sum_sq <- sum(within_var$sum_sq_neta)

id5 <- within_var %>%
  filter(efectivamente_identifica) %>%
  arrange(desc(sum_sq_neta)) %>%
  slice_head(n = 30) %>%
  mutate(
    pct_individual = round(sum_sq_neta / total_sum_sq * 100, 2),
    pct_acumulado  = round(cumsum(sum_sq_neta) / total_sum_sq * 100, 1)
  ) %>%
  separate(municipio_id, into = c("entidad", "municipio"), sep = "_",
           convert = TRUE, remove = FALSE) %>%
  select(municipio_id, entidad, municipio, n_anios, n_hogares,
         sd_neta, sum_sq_neta, pct_individual, pct_acumulado) %>%
  rename(`ID municipio` = municipio_id, Entidad = entidad, Municipio = municipio,
         `N años` = n_anios, `N hogares` = n_hogares,
         `SD neta` = sd_neta, `Sum X̃²` = sum_sq_neta,
         `% peso` = pct_individual, `% acum.` = pct_acumulado)

tex_out(id5, "id_ID5_top30_municipios_peso.tex",
        "Top 30 municipios por contribución cuadrática a la identificación de $\\hat{\\beta}$ en mm2 ($\\sum \\tilde{X}^2$)")
print(id5)

cat("\n  % del peso total en los top 10 municipios:",
    round(sum(within_var %>% arrange(desc(sum_sq_neta)) %>%
                slice_head(n = 10) %>% pull(sum_sq_neta)) / total_sum_sq * 100, 1), "%\n")
cat("  % del peso total en los top 30 municipios:",
    round(sum(within_var %>% arrange(desc(sum_sq_neta)) %>%
                slice_head(n = 30) %>% pull(sum_sq_neta)) / total_sum_sq * 100, 1), "%\n\n")


# ==============================================================================
# ID6: Leave-one-municipality-out — ¿algún municipio domina el coeficiente?
# Para los top 20 municipios por peso, re-estima mm2 excluyendo cada uno
# ==============================================================================

cat("[ID6] Leave-one-out para municipios de alto peso...\n")

top20_munis <- within_var %>%
  arrange(desc(sum_sq_neta)) %>%
  slice_head(n = 20) %>%
  pull(municipio_id)

beta_full <- coef(mm2_full)["log_tasa_hom"]

loo_results <- map_dfr(top20_munis, function(m) {
  dat <- baseMod %>% filter(municipio_id != m)
  fit <- tryCatch(
    feols(log_kcal_pc ~ log_tasa_hom | municipio_id + time_id,
          weights = ~peso, data = dat, cluster = ~municipio_id),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  tibble(
    municipio_excluido = m,
    beta_sin           = round(coef(fit)["log_tasa_hom"], 4),
    cambio_abs         = round(coef(fit)["log_tasa_hom"] - beta_full, 4),
    cambio_pct         = round((coef(fit)["log_tasa_hom"] - beta_full) / abs(beta_full) * 100, 1)
  )
})

id6 <- loo_results %>%
  left_join(within_var %>% select(municipio_id, n_anios, n_hogares, pct = sum_sq_neta) %>%
              mutate(pct = round(pct / total_sum_sq * 100, 2)),
            by = c("municipio_excluido" = "municipio_id")) %>%
  arrange(desc(abs(cambio_abs))) %>%
  rename(`Municipio excluido` = municipio_excluido, `Beta sin muni` = beta_sin,
         `Cambio abs.` = cambio_abs, `Cambio %` = cambio_pct,
         `N años` = n_anios, `N hogares` = n_hogares, `% peso ID` = pct)

tex_out(id6, "id_ID6_leave_one_out.tex",
        paste0("Leave-one-out: efecto de excluir cada uno de los 20 municipios de mayor peso sobre $\\hat{\\beta}$ (base: ",
               round(beta_full, 4), ")"))
print(id6)


# ==============================================================================
# RESUMEN EN CONSOLA
# ==============================================================================

cat("\n")
cat("=================================================================\n")
cat(" DIAGNÓSTICO DE IDENTIFICACIÓN TWFE (mm2: | municipio_id + time_id)\n")
cat("=================================================================\n")

n_munis_total   <- n_distinct(baseMod$municipio_id)
n_munis_1anio   <- sum(panel_muni$n_anios == 1)
n_munis_6anios  <- sum(panel_muni$n_anios == 6)
n_munis_id      <- sum(within_var$efectivamente_identifica)
pct_peso_top10  <- round(sum(within_var %>% arrange(desc(sum_sq_neta)) %>%
                               slice_head(n = 10) %>% pull(sum_sq_neta)) / total_sum_sq * 100, 1)

cat(sprintf("\n  Total municipios en baseMod:          %d (100%%)\n", n_munis_total))
cat(sprintf("  Municipios con 1 solo año ENIGH:      %d (%.1f%%) → NO identifican\n",
            n_munis_1anio, n_munis_1anio / n_munis_total * 100))
cat(sprintf("  Municipios con los 6 años ENIGH:      %d (%.1f%%)\n",
            n_munis_6anios, n_munis_6anios / n_munis_total * 100))
cat(sprintf("  Municipios que efectivamente ID:      %d\n", n_munis_id))
cat(sprintf("  Peso acumulado top 10 municipios:     %.1f%% del poder identificante\n", pct_peso_top10))

cat("\n  Beta mm2 completo:", round(beta_full, 4), "\n")
cat("  Beta restringido a >= 2 años:", id3$Beta[id3$`Min. años` == 2], "\n")
cat("  Beta restringido a >= 6 años:", id3$Beta[id3$`Min. años` == 6], "\n")

cat("\n  ARCHIVOS GENERADOS en output/chequeos/:\n")
cat(paste0("    ", list.files("output/chequeos/", pattern = "id_.*\\.tex$")), sep = "\n")
cat("\n=================================================================\n")
