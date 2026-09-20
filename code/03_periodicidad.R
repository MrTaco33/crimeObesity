# =============================================================================
# crimeObesity — cantidad_tri con checkeos diagnósticos
# Corre para 2024, 2022 y 2020 secuencialmente
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# CONFIGURACIÓN
# -----------------------------------------------------------------------------

anios <- c(2024)


rutas_gastoshogar <- list(
  "2024" = "data/raw/gastoshogar2024.csv",
  
)

ruta_xbar <- "data/processed/resumenXbarra.csv"   # ajusta si el nombre es distinto

PREFIJOS_ALIMENTOS <- c("011", "012", "013", "014", "181")
SEMANAS_TRIMESTRE  <- 13
MESES_TRIMESTRE    <- 3
UMBRAL_KG_TRI      <- 100   # umbral para detectar valores implausibles

# Referencia de sanity check en CHECK 4
PERSONAS_PROMEDIO  <- 3.7
KCAL_DIA_PERSONA   <- 2000
DIAS_TRIMESTRE     <- 91

# -----------------------------------------------------------------------------
# Cargar X-bar una sola vez (crosswalk ENIGH × SMAE)
# -----------------------------------------------------------------------------

xbar <- read_csv(ruta_xbar, show_col_types = FALSE)

# Detectar nombre de columna de kcal/kg
xbar_col <- "kcal_media"
cat("Columna X-bar detectada:", xbar_col, "\n\n")

claves_en_xbar <- unique(xbar$clave)

# Contenedor de resultados finales
resultados_por_anio <- list()

# =============================================================================
# LOOP POR AÑO
# =============================================================================

for (anio in anios) {

  anio_chr <- as.character(anio)
  cat("\n", strrep("#", 60), "\n")
  cat("### AÑO:", anio, "\n")
  cat(strrep("#", 60), "\n\n")

  # ---------------------------------------------------------------------------
  # LECTURA
  # ---------------------------------------------------------------------------

  ruta <- rutas_gastoshogar[[anio_chr]]

  if (!file.exists(ruta)) {
    cat("⚠️  Archivo no encontrado:", ruta, "— saltando año", anio, "\n")
    next
  }

  gastoshogar <- read_csv(ruta, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    mutate(across(c(cantidad, gasto, gasto_tri), as.numeric))

  # ==========================================================================
  # CHECK 0 — Estructura básica
  # ==========================================================================
  cat(strrep("=", 60), "\n")
  cat("CHECK 0: Estructura de gastoshogar", anio, "\n")
  cat(strrep("=", 60), "\n")

  cat("Filas:          ", nrow(gastoshogar), "\n")
  cat("Hogares únicos: ", n_distinct(paste(gastoshogar$folioviv, gastoshogar$foliohog)), "\n")
  cat("Claves únicas:  ", n_distinct(gastoshogar$clave), "\n")

  cat("\nDistribución de tipo_gasto:\n")
  print(count(gastoshogar, tipo_gasto))

  cat("\nDistribución de mes_dia — top 15 valores más frecuentes:\n")
  gastoshogar %>% count(mes_dia, sort = TRUE) %>% head(15) %>% print()

  n_diarios <- sum(gastoshogar$mes_dia != "0000", na.rm = TRUE)
  n_nodiar  <- sum(gastoshogar$mes_dia == "0000", na.rm = TRUE)
  cat("\nRegistros del cuadernillo diario (mes_dia ≠ 0000):", n_diarios,
      sprintf("(%.1f%%)", 100 * n_diarios / nrow(gastoshogar)), "\n")
  cat("Registros cuestionario mensual  (mes_dia = 0000): ", n_nodiar,
      sprintf("(%.1f%%)", 100 * n_nodiar  / nrow(gastoshogar)), "\n")

  # ==========================================================================
  # PASO 1 — Clasificar período de referencia
  # ==========================================================================
  cat("\n", strrep("=", 60), "\n")
  cat("PASO 1: Clasificar período de referencia\n")
  cat(strrep("=", 60), "\n")

  gastoshogar <- gastoshogar %>%
    mutate(
      es_alimento = str_sub(clave, 1, 3) %in% PREFIJOS_ALIMENTOS,
      es_diario   = mes_dia != "0000",
      periodo_ref = case_when(
        es_diario                ~ "diario",
        !es_diario & es_alimento ~ "mensual",
        TRUE                     ~ "otro"
      )
    )

  cat("\nRegistros por período de referencia:\n")
  gastoshogar %>%
    count(periodo_ref, es_alimento) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    print()

  n_alim_mensual <- sum(gastoshogar$periodo_ref == "mensual")
  cat("\nRegistros alimentarios con mes_dia='0000' (estimación mensual):", n_alim_mensual, "\n")
  if (n_alim_mensual > 0 & n_alim_mensual < 30) {
    gastoshogar %>% filter(periodo_ref == "mensual") %>% count(clave) %>% print()
  }

  # ==========================================================================
  # PASO 2 — Agregar días del cuadernillo por hogar × clave
  # ==========================================================================
  cat("\n", strrep("=", 60), "\n")
  cat("PASO 2: Agregar días por hogar × clave\n")
  cat(strrep("=", 60), "\n")

  alimentos_diario <- gastoshogar %>%
    filter(periodo_ref == "diario", es_alimento, tipo_gasto == "G1")

  cat("Filas de alimentos diarios (G1):", nrow(alimentos_diario), "\n")

  dias_obs <- alimentos_diario %>%
    count(folioviv, foliohog, clave, name = "n_dias")

  cat("\nDías observados por hogar×clave:\n")
  dias_obs %>%
    count(n_dias) %>%
    mutate(
      pct       = round(n / sum(n) * 100, 1),
      acumulado = cumsum(pct)
    ) %>%
    print()

  cat("\n→ La mayoría debería tener entre 1 y 7 días.\n")
  cat("  Valores > 7 indicarían registros duplicados — revisar.\n")

  if (any(dias_obs$n_dias > 7)) {
    cat("\n⚠️  Registros con más de 7 días observados:\n")
    dias_obs %>% filter(n_dias > 7) %>% arrange(desc(n_dias)) %>% head(10) %>% print()
  }

  alimentos_semana <- alimentos_diario %>%
    group_by(folioviv, foliohog, clave, tipo_gasto) %>%
    summarise(
      kg_semana_obs   = sum(cantidad,              na.rm = TRUE),
      gasto_semana    = sum(gasto,                 na.rm = TRUE),
      gasto_tri_inegi = sum(as.numeric(gasto_tri), na.rm = TRUE),
      n_dias_obs      = n(),
      .groups         = "drop"
    )

  cat("\nResumen de kg_semana_obs:\n")
  summary(alimentos_semana$kg_semana_obs) %>% print()

  # ==========================================================================
  # PASO 3 — Construir cantidad_tri
  # ==========================================================================
  cat("\n", strrep("=", 60), "\n")
  cat("PASO 3: Construir cantidad_tri\n")
  cat(strrep("=", 60), "\n")

  cant_tri_diario <- alimentos_semana %>%
    mutate(
      cantidad_tri = kg_semana_obs * SEMANAS_TRIMESTRE,
      fuente       = "diario"
    )

  cant_tri_mensual <- gastoshogar %>%
    filter(periodo_ref == "mensual", tipo_gasto == "G1") %>%
    group_by(folioviv, foliohog, clave, tipo_gasto) %>%
    summarise(
      cantidad_tri    = sum(cantidad * MESES_TRIMESTRE, na.rm = TRUE),
      gasto_tri_inegi = sum(as.numeric(gasto_tri),     na.rm = TRUE),
      n_dias_obs      = 0L,
      fuente          = "mensual",
      .groups         = "drop"
    )

  cantidad_tri <- bind_rows(
    cant_tri_diario  %>% select(folioviv, foliohog, clave, tipo_gasto,
                                 cantidad_tri, gasto_tri_inegi, n_dias_obs, fuente),
    cant_tri_mensual %>% select(folioviv, foliohog, clave, tipo_gasto,
                                 cantidad_tri, gasto_tri_inegi, n_dias_obs, fuente)
  )

  cat("Registros en cantidad_tri:\n")
  count(cantidad_tri, fuente) %>% print()

  cat("\nkg/trimestre por fuente:\n")
  cantidad_tri %>%
    group_by(fuente) %>%
    summarise(
      media = round(mean(cantidad_tri, na.rm = TRUE), 2),
      p50   = round(median(cantidad_tri, na.rm = TRUE), 2),
      p95   = round(quantile(cantidad_tri, .95, na.rm = TRUE), 2),
      max   = round(max(cantidad_tri, na.rm = TRUE), 2)
    ) %>%
    print()

  # ==========================================================================
  # CHECK 1 — Correlación gasto_tri reconstruido vs INEGI
  # gasto_semana × 13 debe ≈ gasto_tri_inegi — esperamos > 0.95
  # ==========================================================================
  cat("\n", strrep("=", 60), "\n")
  cat("CHECK 1: Correlación gasto_tri reconstruido vs INEGI\n")
  cat(strrep("=", 60), "\n")

  check1 <- cant_tri_diario %>%
    mutate(gasto_tri_rec = gasto_semana * SEMANAS_TRIMESTRE) %>%
    filter(gasto_tri_inegi > 0, !is.na(gasto_tri_inegi), !is.na(gasto_tri_rec))

  if (nrow(check1) > 10) {
    cor_val <- cor(check1$gasto_tri_rec, check1$gasto_tri_inegi, use = "complete.obs")
    cat("Correlación:", round(cor_val, 4), "\n")
    if (cor_val > 0.95) {
      cat("✅ Alta — lógica de trimestrialización correcta\n")
    } else if (cor_val > 0.80) {
      cat("⚠️  Moderada — revisar registros con discrepancias grandes\n")
    } else {
      cat("❌ Baja — probable error en clasificación de períodos\n")
    }

    cat("\nTop 5 casos con mayor discrepancia relativa:\n")
    check1 %>%
      mutate(
        diff_abs = abs(gasto_tri_rec - gasto_tri_inegi),
        diff_pct = round(diff_abs / (abs(gasto_tri_inegi) + 1) * 100, 1)
      ) %>%
      arrange(desc(diff_pct)) %>%
      select(folioviv, foliohog, clave, n_dias_obs,
             gasto_tri_rec, gasto_tri_inegi, diff_pct) %>%
      head(5) %>%
      print()
  } else {
    cat("⚠️  Muy pocos registros para calcular correlación\n")
  }

  # ==========================================================================
  # CHECK 2 — Valores implausibles
  # ==========================================================================
  cat("\n", strrep("=", 60), "\n")
  cat("CHECK 2: Valores implausibles de cantidad_tri\n")
  cat(strrep("=", 60), "\n")

  n_imp  <- sum(cantidad_tri$cantidad_tri > UMBRAL_KG_TRI, na.rm = TRUE)
  n_cero <- sum(cantidad_tri$cantidad_tri == 0 | is.na(cantidad_tri$cantidad_tri))

  cat("Registros con cantidad_tri >", UMBRAL_KG_TRI, "kg:", n_imp, "\n")
  cat("Registros con cantidad_tri = 0 o NA:           ", n_cero, "\n")

  if (n_imp > 0) {
    cat("\nTop 10 valores más altos:\n")
    cantidad_tri %>%
      arrange(desc(cantidad_tri)) %>%
      select(folioviv, foliohog, clave, cantidad_tri, n_dias_obs, fuente) %>%
      head(10) %>%
      print()
    cat("⚠️  Claves 011/012 con >100 kg/tri → revisar unidades en SMAE\n")
    cat("   Bebidas (agua, refresco) → cantidades altas son esperadas\n")
  } else {
    cat("✅ Sin valores implausibles\n")
  }

  # ==========================================================================
  # CHECK 3 — Cobertura del crosswalk
  # ==========================================================================
  cat("\n", strrep("=", 60), "\n")
  cat("CHECK 3: Cobertura del crosswalk X-bar\n")
  cat(strrep("=", 60), "\n")

  claves_en_datos <- unique(cantidad_tri$clave)
  n_map    <- sum(claves_en_datos %in% claves_en_xbar)
  n_no_map <- length(claves_en_datos) - n_map

  cat("Claves únicas en datos:   ", length(claves_en_datos), "\n")
  cat("Claves con X-bar:         ", n_map, "\n")
  cat("Claves sin X-bar:         ", n_no_map,
      sprintf("(%.1f%%)", 100 * n_no_map / length(claves_en_datos)), "\n")

  pct_kg <- cantidad_tri %>%
    mutate(mapeada = clave %in% claves_en_xbar) %>%
    summarise(
      kg_mapeado    = sum(cantidad_tri[mapeada],  na.rm = TRUE),
      kg_no_mapeado = sum(cantidad_tri[!mapeada], na.rm = TRUE)
    ) %>%
    mutate(pct = round(kg_mapeado / (kg_mapeado + kg_no_mapeado) * 100, 1))

  cat("% de kg_tri cubiertos:    ", pct_kg$pct, "%\n")

  if (pct_kg$pct < 90) {
    cat("⚠️  Cobertura baja — claves sin mapear:\n")
    claves_en_datos[!claves_en_datos %in% claves_en_xbar] %>% sort() %>% print()
  } else {
    cat("✅ Cobertura suficiente\n")
  }

  # ==========================================================================
  # PASO 4 — Calcular kcal_tri por hogar
  # ==========================================================================
  cat("\n", strrep("=", 60), "\n")
  cat("PASO 4: Calcular kcal_tri por hogar\n")
  cat(strrep("=", 60), "\n")

  kcal_hogar <- cantidad_tri %>%
    left_join(
      xbar %>% rename(xbar_val = !!xbar_col) %>% select(clave, xbar_val),
      by = "clave"
    ) %>%
    mutate(kcal_tri = cantidad_tri * xbar_val) %>%
    group_by(folioviv, foliohog) %>%
    summarise(
      kcal_tri_total = sum(kcal_tri,     na.rm = TRUE),
      kg_tri_total   = sum(cantidad_tri, na.rm = TRUE),
      n_claves       = n(),
      n_mapeadas     = sum(!is.na(xbar_val)),
      pct_mapeado    = round(n_mapeadas / n_claves * 100, 1),
      .groups        = "drop"
    ) %>%
    mutate(anio = anio)

  cat("Hogares con estimación:", nrow(kcal_hogar), "\n")

  # ==========================================================================
  # CHECK 4 — Distribución de kcal_tri_total
  # Referencia: 3.7 personas × 2000 kcal/día × 91 días ≈ 673,400 kcal
  # La mediana observada será más baja porque el cuadernillo solo captura
  # alimentos dentro del hogar (no comidas fuera, claves 111xxx)
  # ==========================================================================
  cat("\n", strrep("=", 60), "\n")
  cat("CHECK 4: Distribución kcal_tri_total por hogar\n")
  cat(strrep("=", 60), "\n")

  cat("Estadísticos:\n")
  summary(kcal_hogar$kcal_tri_total) %>% print()

  kcal_ref    <- PERSONAS_PROMEDIO * KCAL_DIA_PERSONA * DIAS_TRIMESTRE
  mediana_obs <- median(kcal_hogar$kcal_tri_total, na.rm = TRUE)
  ratio_ref   <- mediana_obs / kcal_ref

  cat(sprintf("\nReferencia teórica (%.1f pers × %d kcal/día × %d días): %s kcal\n",
              PERSONAS_PROMEDIO, KCAL_DIA_PERSONA, DIAS_TRIMESTRE,
              format(round(kcal_ref), big.mark = ",")))
  cat("Mediana observada:", format(round(mediana_obs), big.mark = ","), "kcal\n")
  cat("Ratio mediana/referencia:", round(ratio_ref, 2), "\n")

  if (ratio_ref < 0.15) {
    cat("❌ Muy baja — posible error en unidades o en el factor de escala\n")
  } else if (ratio_ref < 0.5) {
    cat("⚠️  Baja pero esperable: cuadernillo captura solo dentro del hogar.\n")
    cat("   Las comidas fuera (claves 111xxx) no están incluidas aquí.\n")
  } else if (ratio_ref > 3) {
    cat("❌ Muy alta — revisar doble conteo o errores en gasto_tri_inegi\n")
  } else {
    cat("✅ Rango razonable\n")
  }

  cat("\nHogares con < 80% de claves mapeadas:",
      sum(kcal_hogar$pct_mapeado < 80, na.rm = TRUE), "\n")

  # ==========================================================================
  # CHECK 5 — Top 10 claves por kg_tri
  # Deberías ver tortillas, leche, pollo, refrescos, agua entre los primeros
  # ==========================================================================
  cat("\n", strrep("=", 60), "\n")
  cat("CHECK 5: Top 10 claves por kg_tri total\n")
  cat(strrep("=", 60), "\n")

  cantidad_tri %>%
    group_by(clave) %>%
    summarise(
      kg_tri_total   = sum(cantidad_tri, na.rm = TRUE),
      n_hogares      = n_distinct(paste(folioviv, foliohog)),
      kg_tri_mediana = round(median(cantidad_tri, na.rm = TRUE), 2),
      .groups        = "drop"
    ) %>%
    arrange(desc(kg_tri_total)) %>%
    head(10) %>%
    left_join(
      xbar %>% rename(xbar_val = !!xbar_col) %>% select(clave, xbar_val),
      by = "clave"
    ) %>%
    print()

  cat("\n→ Espera: tortillas, agua, leche, refresco, pollo, huevo\n")
  cat("  Si ves claves no alimentarias arriba → revisar PREFIJOS_ALIMENTOS\n")

  # ==========================================================================
  # GUARDAR año actual
  # ==========================================================================
  dir.create("output", showWarnings = FALSE)

  write_csv(cantidad_tri, sprintf("output/cantidad_tri_%d.csv",    anio))
  write_csv(kcal_hogar,   sprintf("output/kcal_hogar_tri_%d.csv",  anio))

  cat(sprintf("\n✅ output/cantidad_tri_%d.csv   (%d filas)\n",   anio, nrow(cantidad_tri)))
  cat(sprintf("✅ output/kcal_hogar_tri_%d.csv (%d hogares)\n\n", anio, nrow(kcal_hogar)))

  resultados_por_anio[[anio_chr]] <- kcal_hogar

} # fin loop años

# =============================================================================
# RESUMEN COMPARATIVO ENTRE AÑOS
# =============================================================================
cat("\n", strrep("#", 60), "\n")
cat("### COMPARATIVO ENTRE AÑOS\n")
cat(strrep("#", 60), "\n\n")

if (length(resultados_por_anio) > 1) {
  bind_rows(resultados_por_anio) %>%
    group_by(anio) %>%
    summarise(
      hogares           = n(),
      kcal_p10          = round(quantile(kcal_tri_total, .10, na.rm = TRUE)),
      kcal_mediana      = round(median(kcal_tri_total,        na.rm = TRUE)),
      kcal_p90          = round(quantile(kcal_tri_total, .90, na.rm = TRUE)),
      kg_mediana        = round(median(kg_tri_total,          na.rm = TRUE), 1),
      pct_bien_mapeado  = round(mean(pct_mapeado >= 80) * 100, 1)
    ) %>%
    print()

  todos <- bind_rows(resultados_por_anio)
  write_csv(todos, "output/kcal_hogar_tri_panel.csv")
  cat("\n✅ output/kcal_hogar_tri_panel.csv guardado\n")
} else {
  cat("Solo se procesó un año — no hay comparativo\n")
}




######################################
gastoshogar %>%
  filter(mes_dia == "0000", es_alimento, tipo_gasto == "G1") %>%
  select(clave, cantidad) %>%
  head(30) %>%
  unique() %>%
  print()

# Y comparar la ratio gasto/gasto_tri para inferir el factor implícito
gastoshogar %>%
  filter(mes_dia == "0000", es_alimento, tipo_gasto == "G1",
         !is.na(cantidad), cantidad > 0,
         !is.na(gasto_tri), gasto_tri > 0,
         !is.na(gasto), gasto > 0) %>%
  mutate(factor_implicito = gasto_tri / gasto) %>%
  count(round(factor_implicito, 1)) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  print()







gastoshogar %>%
  filter(mes_dia == "0000", es_alimento, tipo_gasto == "G1",
         !is.na(cantidad), cantidad > 0)


