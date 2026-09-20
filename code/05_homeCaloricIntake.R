library(tidyverse)
library(haven)

# ==============================================================================
# 05_homeCaloricIntake.R
# Calcula calorías per cápita por hogar-año y hace merge con tasas de homicidios
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. CARGAR DENSIDAD CALÓRICA
# ------------------------------------------------------------------------------

xbarra <- read_csv("data/processed/resumenXbarra.csv",
                   locale = locale(encoding = "latin1"),
                   show_col_types = FALSE) %>%
  select(clave, kcal_media)

# catálogo de equivalencias entre claves nuevas (6 dígitos, 2024) y viejas (4 chars, 2012-2022)
catalogo_cruzado <- read_csv("data/processed/catalogoCruzadoENIGH.csv",
                             show_col_types = FALSE)

# xbarra para claves viejas: promedio de kcal_media de las claves nuevas equivalentes
xbarra_viejo <- catalogo_cruzado %>%
  filter(!is.na(claveAntes)) %>%
  left_join(xbarra, by = "clave") %>%
  group_by(claveAntes) %>%
  summarise(kcal_media = mean(kcal_media, na.rm = TRUE), .groups = "drop") %>%
  rename(clave = claveAntes)

# lookup unificado: cubre sistema nuevo (6 dígitos) y viejo (4 chars)
xbarra_total <- bind_rows(xbarra, xbarra_viejo)


# ------------------------------------------------------------------------------
# 2. CARGAR Y APILAR GASTOSHOGAR
# ------------------------------------------------------------------------------

leer_gastoshogar <- function(anio) {
  read_csv(
    paste0("data/raw/basesENIGH/gastoshogar", anio, ".csv"),
    locale    = locale(encoding = "latin1"),
    col_types = cols(
      folioviv  = col_character(),
      foliohog  = col_character(),
      clave     = col_character(),
      cantidad  = col_double(),
      gasto     = col_double(),
      gasto_tri = col_double(),
      mes_dia   = col_character(),
      .default  = col_skip()
    )
  ) %>%
    mutate(year = anio)
}

anios <- c(2012, 2014, 2016, 2018, 2020, 2022, 2024)
gastoshogar <- map_dfr(anios, leer_gastoshogar)

# ------------------------------------------------------------------------------
# 3. CARGAR Y APILAR CONCENTRADOHOGAR
# ------------------------------------------------------------------------------

leer_concentrado <- function(anio) {
  df_raw <- read_csv(paste0("data/raw/basesConcentrados/concentradohogar", anio, ".csv"),
                     locale = locale(encoding = "latin1"),
                     show_col_types = FALSE, n_max = 0)
  factor_col <- if ("factor_hog" %in% names(df_raw)) "factor_hog" else "factor"

  col_spec <- cols(
    folioviv  = col_character(),
    foliohog  = col_character(),
    ubica_geo = col_character(),
    tot_integ = col_double(),
    .default  = col_skip()
  )
  col_spec$cols[[factor_col]] <- col_double()

  df <- read_csv(paste0("data/raw/basesConcentrados/concentradohogar", anio, ".csv"),
                 locale = locale(encoding = "latin1"),
                 col_types = col_spec)

  if ("factor_hog" %in% names(df)) df <- df %>% rename(factor = factor_hog)
  df %>% mutate(year = anio)
}

concentrado <- map_dfr(anios, leer_concentrado)

concentrado <- concentrado %>%
  mutate(
    entidad   = as.numeric(substr(ubica_geo, 1, 2)),
    municipio = as.numeric(substr(ubica_geo, 3, 5))
  ) %>%
  select(-ubica_geo)


# ------------------------------------------------------------------------------
# 4. CALCULAR MES DEL LEVANTAMIENTO POR HOGAR (desde mes_dia)
# mes_dia viene en formato MMDD (ej. 0919 = 19 sep); 0000 = sin fecha
# Se toma el mes modal de las transacciones con fecha válida por hogar
# ------------------------------------------------------------------------------

mode_mes <- function(x) {
  x <- x[!is.na(x) & x > 0 & x <= 12]
  if (length(x) == 0) return(NA_integer_)
  as.integer(names(sort(table(x), decreasing = TRUE))[1])
}

mes_hogar <- gastoshogar %>%
  mutate(mes = as.integer(substr(formatC(as.integer(mes_dia), width = 4, flag = "0"), 1, 2))) %>%
  group_by(folioviv, foliohog, year) %>%
  summarise(mes = mode_mes(mes), .groups = "drop")


# ------------------------------------------------------------------------------
# 5. CALCULAR CALORÍAS POR HOGAR-AÑO
# ------------------------------------------------------------------------------

calorias_hogar <- gastoshogar %>%
  filter(!is.na(cantidad), cantidad > 0, !is.na(gasto), gasto > 0) %>%
  left_join(xbarra_total, by = "clave") %>%
  mutate(cantidad_tri = cantidad * (gasto_tri / gasto),
         kcal = kcal_media * cantidad_tri) %>%
  group_by(folioviv, foliohog, year) %>%
  summarise(
    kcal_total  = sum(kcal, na.rm = TRUE),
    n_productos = n(),
    n_match     = sum(!is.na(kcal_media)),
    pct_match   = n_match / n_productos,
    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# 6. PEGAR CONCENTRADO, MES Y CALCULAR PER CÁPITA
# ------------------------------------------------------------------------------

calorias_hogar <- calorias_hogar %>%
  left_join(concentrado, by = c("folioviv", "foliohog", "year")) %>%
  left_join(mes_hogar,   by = c("folioviv", "foliohog", "year")) %>%
  mutate(kcal_per_capita = kcal_total / tot_integ)


# ------------------------------------------------------------------------------
# 7. PEGAR TASAS DE HOMICIDIOS
# ------------------------------------------------------------------------------

tasas_homicidios <- read_dta("data/processed/tasas_homicidios.dta")

base_completa <- calorias_hogar %>%
  left_join(tasas_homicidios,
            by = c("entidad"   = "entidad_hom",
                   "municipio" = "municipio_hom",
                   "year"      = "anio_hom",
                   "mes"       = "mes_hom"))


# ------------------------------------------------------------------------------
# 8. GUARDAR
# ------------------------------------------------------------------------------

write_csv(base_completa, "data/processed/base_completa.csv")
write_dta(base_completa, "data/processed/base_completa.dta")


# ==============================================================================
# 9. CHEQUEOS DE CALIDAD
# ==============================================================================

cat("\n--- 1. Hogares sin mes (mes_dia todo cero) ---\n")
mes_hogar %>%
  left_join(concentrado %>% select(folioviv, foliohog, year), by = c("folioviv", "foliohog", "year")) %>%
  group_by(year) %>%
  summarise(
    hogares    = n(),
    sin_mes    = sum(is.na(mes)),
    pct_sin_mes = round(mean(is.na(mes)) * 100, 1)
  ) %>%
  print()

cat("\n--- 2. Cobertura calórica por año (pct_match promedio) ---\n")
calorias_hogar %>%
  group_by(year) %>%
  summarise(
    hogares         = n(),
    pct_match_medio = round(mean(pct_match, na.rm = TRUE), 3),
    kcal_pc_media   = round(mean(kcal_per_capita, na.rm = TRUE), 1),
    kcal_pc_p50     = round(median(kcal_per_capita, na.rm = TRUE), 1),
    hogares_kcal_0  = sum(kcal_total == 0 | is.na(kcal_per_capita))
  ) %>%
  print()

cat("\n--- 3. Join con tasas de homicidios ---\n")
base_completa %>%
  group_by(year) %>%
  summarise(
    hogares       = n(),
    con_tasa      = sum(!is.na(tasa_homicidios)),
    pct_con_tasa  = round(mean(!is.na(tasa_homicidios)) * 100, 1),
    na_mes        = sum(is.na(mes))
  ) %>%
  print()

cat("\n--- 4. Resumen final de base_completa ---\n")
cat("Filas totales:", nrow(base_completa), "\n")
cat("Años presentes:", paste(sort(unique(base_completa$year)), collapse = ", "), "\n")
cat("Columnas:", paste(names(base_completa), collapse = ", "), "\n")
