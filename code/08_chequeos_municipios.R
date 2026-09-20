# ==============================================================================
# 08_chequeos_municipios.R
# Diagnóstico de cobertura municipal: ENIGH vs. tasas de homicidios
#
# Pregunta central: cuando tasa_homicidios = NA, ¿el municipio no reportó
# (ausente del SESNSP) o sólo falta mes_dia? Y cuando tasa = 0, ¿son ceros
# verdaderos o municipios que no aparecen en el registro?
#
# Outputs: output/chequeos/muni_*.tex  +  resumen en consola
# ==============================================================================

library(tidyverse)
library(haven)
library(xtable)

dir.create("output/chequeos", recursive = TRUE, showWarnings = FALSE)

# ── helper TeX ────────────────────────────────────────────────────────────────
tex_out <- function(df, fname, caption, digits = NULL) {
  xt <- xtable(df, caption = caption,
               label = paste0("tab:", tools::file_path_sans_ext(fname)),
               digits = digits)
  sink(file.path("output/chequeos", fname))
  print(xt, floating = TRUE, include.rownames = FALSE, booktabs = TRUE,
        caption.placement = "top", comment = FALSE,
        sanitize.text.function = identity,
        sanitize.colnames.function = identity)
  sink()
  invisible(df)
}

# ==============================================================================
# 0. CARGAR DATOS
# ==============================================================================

cat("[0] Cargando datos...\n")

anios <- c(2012, 2014, 2016, 2018, 2020, 2022, 2024)

leer_concentrado <- function(anio) {
  df_raw <- read_csv(paste0("data/raw/basesConcentrados/concentradohogar", anio, ".csv"),
                     locale = locale(encoding = "latin1"),
                     show_col_types = FALSE, n_max = 0)
  factor_col <- if ("factor_hog" %in% names(df_raw)) "factor_hog" else "factor"
  col_spec <- cols(folioviv = col_character(), foliohog = col_character(),
                   ubica_geo = col_character(), tot_integ = col_double(),
                   .default = col_skip())
  col_spec$cols[[factor_col]] <- col_double()
  df <- read_csv(paste0("data/raw/basesConcentrados/concentradohogar", anio, ".csv"),
                 locale = locale(encoding = "latin1"), col_types = col_spec)
  if ("factor_hog" %in% names(df)) df <- df %>% rename(factor = factor_hog)
  df %>% mutate(year = anio)
}

concentrado <- map_dfr(anios, leer_concentrado) %>%
  mutate(entidad   = as.integer(substr(ubica_geo, 1, 2)),
         municipio = as.integer(substr(ubica_geo, 3, 5))) %>%
  select(-ubica_geo)

tasas_hom <- read_dta("data/processed/tasas_homicidios.dta") %>%
  rename(entidad   = entidad_hom,
         municipio = municipio_hom,
         year      = anio_hom,
         mes       = mes_hom)

base <- read_csv("data/processed/base_completa.csv",
                 show_col_types = FALSE) %>%
  mutate(municipio_id = paste(entidad, municipio, sep = "_"))

cat("   OK. Hogares en base_completa:", nrow(base), "\n\n")

# ==============================================================================
# CHEQUEO M1: Universo de municipios por fuente y año
# Cuántos municipios únicos ve cada fuente, y cuánto solapan
# ==============================================================================

cat("[M1] Universo de municipios por fuente...\n")

munis_enigh <- concentrado %>%
  group_by(year) %>%
  summarise(N_munis_enigh = n_distinct(paste(entidad, municipio)),
            N_hogares     = n(),
            .groups = "drop")

munis_hom_anio <- tasas_hom %>%
  group_by(year) %>%
  summarise(N_munis_hom  = n_distinct(paste(entidad, municipio)),
            N_obs_hom    = n(),
            N_meses_med  = round(n() / n_distinct(paste(entidad, municipio)), 1),
            .groups = "drop")

# municipios que están en ENIGH Y en tasas (por año)
match_anio <- concentrado %>%
  distinct(entidad, municipio, year) %>%
  left_join(tasas_hom %>% distinct(entidad, municipio, year) %>% mutate(en_hom = 1L),
            by = c("entidad", "municipio", "year")) %>%
  group_by(year) %>%
  summarise(con_hom = sum(!is.na(en_hom)),
            sin_hom = sum(is.na(en_hom)),
            .groups = "drop")

m1 <- munis_enigh %>%
  left_join(munis_hom_anio, by = "year") %>%
  left_join(match_anio,     by = "year") %>%
  mutate(pct_cubierto = round(con_hom / N_munis_enigh * 100, 1)) %>%
  rename(Anio = year, `Munis ENIGH` = N_munis_enigh, `Hogares ENIGH` = N_hogares,
         `Munis en hom.` = N_munis_hom, `Obs hom.` = N_obs_hom,
         `Meses/muni` = N_meses_med,
         `Con datos hom.` = con_hom, `Sin datos hom.` = sin_hom,
         `pct cubierto` = pct_cubierto)

tex_out(m1, "muni_M1_universo_por_anio.tex",
        "Municipios únicos en ENIGH y en tasas de homicidios, y solapamiento, por año")
print(m1)


# ==============================================================================
# CHEQUEO M2: Causas de NA en tasa_homicidios a nivel hogar
# Distingue: (A) mes_dia=NA → no se puede hacer el join temporal
#            (B) municipio ausente del registro de homicidios ese año-mes
# ==============================================================================

cat("[M2] Causas de NA en tasa_homicidios (desglose)...\n")

# para cada hogar sin tasa, ¿el municipio existe ALGUNA VEZ en tasas_hom?
munis_en_hom_alguna_vez <- tasas_hom %>%
  distinct(entidad, municipio) %>%
  mutate(muni_existe_en_hom = TRUE)

base_diag <- base %>%
  left_join(munis_en_hom_alguna_vez, by = c("entidad", "municipio")) %>%
  mutate(
    causa_na = case_when(
      !is.na(tasa_homicidios)              ~ "Con tasa",
      is.na(mes)                           ~ "Sin mes (mes_dia=0)",
      is.na(muni_existe_en_hom)            ~ "Municipio nunca en SESNSP",
      TRUE                                 ~ "Municipio en SESNSP pero sin ese año-mes"
    )
  )

m2 <- base_diag %>%
  group_by(year, causa_na) %>%
  summarise(N_hogares = n(), .groups = "drop") %>%
  pivot_wider(names_from = causa_na, values_from = N_hogares, values_fill = 0L) %>%
  arrange(year) %>%
  rename(Anio = year)

tex_out(m2, "muni_M2_causas_na_tasa.tex",
        "Causas de tasa\\_homicidios = NA a nivel hogar, por año")
print(m2)


# ==============================================================================
# CHEQUEO M3: Municipios que NUNCA aparecen en tasas_homicidios
# Con cuántos hogares y qué tamaño tienen (por población)
# ==============================================================================

cat("[M3] Municipios ENIGH que nunca aparecen en SESNSP...\n")

munis_nunca_hom <- concentrado %>%
  distinct(entidad, municipio) %>%
  anti_join(tasas_hom %>% distinct(entidad, municipio),
            by = c("entidad", "municipio"))

# hogares afectados por municipio (sobre todos los años)
hogares_x_muni_nunca <- base %>%
  semi_join(munis_nunca_hom, by = c("entidad", "municipio")) %>%
  group_by(entidad, municipio) %>%
  summarise(N_hogares  = n(),
            anos       = paste(sort(unique(year)), collapse = ","),
            .groups    = "drop")

m3_por_entidad <- hogares_x_muni_nunca %>%
  group_by(entidad) %>%
  summarise(N_munis    = n(),
            N_hogares  = sum(N_hogares),
            municipios = paste(sort(municipio), collapse = ", "),
            .groups = "drop") %>%
  arrange(desc(N_hogares)) %>%
  rename(Entidad = entidad, `N munis` = N_munis, `N hogares` = N_hogares,
         Municipios = municipios)

tex_out(m3_por_entidad, "muni_M3_munis_nunca_en_sesnsp.tex",
        "Municipios de ENIGH que nunca aparecen en datos de homicidios (SESNSP), por entidad")
print(m3_por_entidad)

cat("  Total municipios ENIGH sin ningun registro SESNSP:",
    nrow(munis_nunca_hom), "\n")
cat("  Hogares afectados:",
    sum(hogares_x_muni_nunca$N_hogares), "\n\n")


# ==============================================================================
# CHEQUEO M4: Municipios con datos SESNSP en algunos años pero no en otros
# Puede indicar: municipio creado/fusionado, cambio de clave, o silencio selectivo
# ==============================================================================

cat("[M4] Municipios con cobertura SESNSP parcial entre anios...\n")

# Para cada municipio de ENIGH (que sí existe en SESNSP alguna vez),
# ¿en cuántos años de ENIGH tiene tasa disponible?
munis_cobertura_anual <- base %>%
  semi_join(tasas_hom %>% distinct(entidad, municipio), by = c("entidad", "municipio")) %>%
  filter(!is.na(mes)) %>%  # sólo hogares donde el mes sí es conocido
  group_by(entidad, municipio, year) %>%
  summarise(pct_con_tasa = mean(!is.na(tasa_homicidios)),
            N_hogares    = n(),
            .groups = "drop")

m4 <- munis_cobertura_anual %>%
  group_by(entidad, municipio) %>%
  summarise(
    n_anios_enigh    = n(),
    n_anios_con_tasa = sum(pct_con_tasa > 0.5),
    n_anios_sin_tasa = sum(pct_con_tasa <= 0.5),
    .groups = "drop"
  ) %>%
  filter(n_anios_enigh > 1, n_anios_sin_tasa > 0) %>%  # sólo los inconsistentes
  arrange(desc(n_anios_sin_tasa)) %>%
  slice_head(n = 30) %>%
  rename(Entidad = entidad, Municipio = municipio,
         `Años ENIGH` = n_anios_enigh,
         `Años con tasa` = n_anios_con_tasa,
         `Años sin tasa` = n_anios_sin_tasa)

tex_out(m4, "muni_M4_cobertura_parcial_anios.tex",
        "Municipios con cobertura de tasa de homicidios inconsistente entre años (top 30)")
print(m4)


# ==============================================================================
# CHEQUEO M5: Distribución de ceros en tasa_homicidios
# cero REAL (municipio reportó 0 homicidios) vs. ausencia
# Para SESNSP: un municipio en el dataset con hom=0 sí reportó activamente
# Un municipio ausente del dataset es silencio (puede ser 0 o no-reporte)
# ==============================================================================

cat("[M5] Distribucion de ceros vs. positivos en tasa_homicidios...\n")

m5_tasas <- tasas_hom %>%
  mutate(cat_tasa = case_when(
    homicidios == 0                  ~ "Cero (reportó 0)",
    homicidios > 0 & homicidios <= 1 ~ "1",
    homicidios > 1 & homicidios <= 5 ~ "2-5",
    homicidios > 5 & homicidios <= 20 ~ "6-20",
    homicidios > 20                  ~ ">20"
  )) %>%
  group_by(year, cat_tasa) %>%
  summarise(N_obs = n(), .groups = "drop") %>%
  pivot_wider(names_from = cat_tasa, values_from = N_obs, values_fill = 0L) %>%
  arrange(year) %>%
  rename(Anio = year)

# asegurar orden de columnas
col_orden <- c("Anio", "Cero (reportó 0)", "1", "2-5", "6-20", ">20")
col_orden <- col_orden[col_orden %in% names(m5_tasas)]
m5_tasas <- m5_tasas %>% select(all_of(col_orden))

tex_out(m5_tasas, "muni_M5_distribucion_ceros_hom.tex",
        "Distribución de observaciones municipio-mes en tasas\\_homicidios por número de homicidios")
print(m5_tasas)

# Fracción de municipio-mes con tasa = 0 en la base final (hogares)
m5_base <- base %>%
  filter(!is.na(tasa_homicidios)) %>%
  group_by(year) %>%
  summarise(
    N_hogares   = n(),
    N_tasa_cero = sum(tasa_homicidios == 0),
    N_tasa_pos  = sum(tasa_homicidios > 0),
    pct_cero    = round(mean(tasa_homicidios == 0) * 100, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `N hogares` = N_hogares, `Tasa=0` = N_tasa_cero,
         `Tasa>0` = N_tasa_pos, `pct tasa=0` = pct_cero)

tex_out(m5_base, "muni_M5b_tasa_cero_en_base.tex",
        "Hogares con tasa de homicidios = 0 (municipio reportó activamente cero) vs. positiva")
print(m5_base)


# ==============================================================================
# CHEQUEO M6: Municipios grandes con mala cobertura
# Un municipio grande con NA tiene mayor impacto en la muestra de regresión
# ==============================================================================

cat("[M6] Municipios grandes con NA en tasa (impacto en muestra)...\n")

m6 <- base %>%
  filter(!is.na(mes)) %>%
  group_by(entidad, municipio) %>%
  summarise(
    N_hogares      = n(),
    pct_con_tasa   = round(mean(!is.na(tasa_homicidios)) * 100, 1),
    pob_mediana    = median(poblacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(pct_con_tasa < 80, N_hogares >= 10) %>%
  arrange(desc(N_hogares)) %>%
  slice_head(n = 25) %>%
  rename(Entidad = entidad, Municipio = municipio, `N hogares` = N_hogares,
         `pct con tasa` = pct_con_tasa, `Pob. mediana` = pob_mediana)

tex_out(m6, "muni_M6_grandes_sin_tasa.tex",
        "Municipios con muchos hogares en muestra pero cobertura de tasa < 80\\% (top 25)")
print(m6)


# ==============================================================================
# CHEQUEO M7: Cobertura de meses por municipio en tasas_homicidios
# Un municipio bien cubierto debería tener 12 meses/año
# Municipios con menos de 12 pueden tener meses faltantes (gaps de reporte)
# ==============================================================================

cat("[M7] Meses cubiertos por municipio en tasas_homicidios...\n")

meses_x_muni_anio <- tasas_hom %>%
  group_by(entidad, municipio, year) %>%
  summarise(n_meses = n_distinct(mes), .groups = "drop")

m7 <- meses_x_muni_anio %>%
  mutate(cat_meses = case_when(
    n_meses == 12  ~ "12 (completo)",
    n_meses >= 9   ~ "9-11",
    n_meses >= 6   ~ "6-8",
    n_meses < 6    ~ "< 6"
  )) %>%
  group_by(year, cat_meses) %>%
  summarise(N_munis = n(), .groups = "drop") %>%
  pivot_wider(names_from = cat_meses, values_from = N_munis, values_fill = 0L) %>%
  arrange(year) %>%
  rename(Anio = year)

col_meses <- c("Anio", "12 (completo)", "9-11", "6-8", "< 6")
col_meses <- col_meses[col_meses %in% names(m7)]
m7 <- m7 %>% select(all_of(col_meses))

tex_out(m7, "muni_M7_meses_por_muni_anio.tex",
        "Municipios en tasas\\_homicidios según número de meses cubiertos por año")
print(m7)


# ==============================================================================
# CHEQUEO M8: Impacto total en la muestra de regresión
# ¿Cuántos hogares caen fuera de la muestra específicamente por el problema
# de cobertura municipal (no por kcal=0 ni por mes_dia)?
# ==============================================================================

cat("[M8] Impacto neto en muestra de regresion por problema municipal...\n")

# muestra base de regresión (filtros del 06)
base_reg <- base %>%
  filter(!is.na(kcal_per_capita), kcal_per_capita > 0,
         !is.na(tot_integ), tot_integ > 0)

m8 <- base_reg %>%
  mutate(
    tiene_mes      = !is.na(mes),
    tiene_tasa     = !is.na(tasa_homicidios),
    en_muestra_reg = tiene_mes & tiene_tasa
  ) %>%
  group_by(year) %>%
  summarise(
    N_hogares_kcal  = n(),
    N_con_mes       = sum(tiene_mes),
    N_en_muestra    = sum(en_muestra_reg),
    N_perdidos_mes  = sum(!tiene_mes),
    N_perdidos_muni = sum(tiene_mes & !tiene_tasa),
    pct_retenido    = round(mean(en_muestra_reg) * 100, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `Hogares con kcal` = N_hogares_kcal,
         `Con mes` = N_con_mes, `En muestra reg.` = N_en_muestra,
         `Perdidos por mes` = N_perdidos_mes,
         `Perdidos por muni` = N_perdidos_muni,
         `pct retenido` = pct_retenido)

tex_out(m8, "muni_M8_impacto_muestra_regresion.tex",
        "Hogares perdidos de la muestra de regresión específicamente por problema de cobertura municipal")
print(m8)


# ==============================================================================
# RESUMEN EN CONSOLA
# ==============================================================================

cat("\n")
cat("=================================================================\n")
cat(" RESUMEN DE COBERTURA MUNICIPAL\n")
cat("=================================================================\n")

cat("\n[Municipios ENIGH que NUNCA aparecen en SESNSP]\n")
cat("  N municipios:", nrow(munis_nunca_hom), "\n")
cat("  Hogares afectados:", sum(hogares_x_muni_nunca$N_hogares), "\n")

cat("\n[Causas de NA en tasa_homicidios en base_completa]\n")
base_diag %>%
  count(causa_na, name = "N_hogares") %>%
  mutate(pct = round(N_hogares / sum(N_hogares) * 100, 1)) %>%
  arrange(desc(N_hogares)) %>%
  print()

cat("\n[Fracción de hogares EN MUESTRA con tasa = 0 (cero verdadero SESNSP)]\n")
base %>%
  filter(!is.na(tasa_homicidios)) %>%
  summarise(pct_cero = round(mean(tasa_homicidios == 0) * 100, 1),
            N        = n()) %>%
  print()

cat("\n[Archivos generados en output/chequeos/]\n")
cat(paste0("  ", list.files("output/chequeos/", pattern = "muni_.*\\.tex$")), sep = "\n")
cat("=================================================================\n")
