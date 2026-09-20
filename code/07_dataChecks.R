# ==============================================================================
# 07_dataChecks.R
# Diagnóstico de pérdida de información a lo largo del pipeline de datos
# Detecta dónde se pierden hogares/registros entre gastoshogar → regresión
# Outputs: output/chequeos/*.tex
# ==============================================================================

library(tidyverse)
library(haven)
library(xtable)

dir.create("output/chequeos", recursive = TRUE, showWarnings = FALSE)

anios <- c(2012, 2014, 2016, 2018, 2020, 2022, 2024)

# ── Helper: guarda data frame como tabla .tex ──────────────────────────────────
tex_out <- function(df, fname, caption, digits = NULL) {
  xt <- xtable(df,
               caption = caption,
               label   = paste0("tab:", tools::file_path_sans_ext(fname)),
               digits  = digits)
  sink(file.path("output/chequeos", fname))
  print(xt,
        floating              = TRUE,
        include.rownames      = FALSE,
        booktabs              = TRUE,
        caption.placement     = "top",
        comment               = FALSE,
        sanitize.text.function = identity,
        sanitize.colnames.function = function(x) x)
  sink()
  invisible(df)
}

# ==============================================================================
# 0. CARGAR INSUMOS
# ==============================================================================

cat("[0] Cargando insumos base...\n")

xbarra <- read_csv("data/processed/resumenXbarra.csv",
                   locale = locale(encoding = "latin1"),
                   show_col_types = FALSE) %>%
  select(clave, kcal_media)

catalogo_cruzado <- read_csv("data/processed/catalogoCruzadoENIGH.csv",
                              show_col_types = FALSE)

xbarra_viejo <- catalogo_cruzado %>%
  filter(!is.na(claveAntes)) %>%
  left_join(xbarra, by = "clave") %>%
  group_by(claveAntes) %>%
  summarise(kcal_media = mean(kcal_media, na.rm = TRUE), .groups = "drop") %>%
  rename(clave = claveAntes)

xbarra_total <- bind_rows(xbarra, xbarra_viejo)

leer_gastoshogar <- function(anio) {
  df <- read_csv(
    paste0("data/raw/basesENIGH/gastoshogar", anio, ".csv"),
    locale    = locale(encoding = "latin1"),
    col_types = cols(
      folioviv   = col_character(),
      foliohog   = col_character(),
      clave      = col_character(),
      tipo_gasto = col_character(),
      cantidad   = col_double(),
      gasto      = col_double(),
      mes_dia    = col_character(),
      .default   = col_skip()
    )
  )
  n_prob <- sum(is.na(df$cantidad))
  if (n_prob > 0)
    message(sprintf("  gastoshogar%d: %d NA en `cantidad` tras parseo", anio, n_prob))
  df %>% mutate(year = anio)
}

gastoshogar <- map_dfr(anios, leer_gastoshogar)

leer_concentrado <- function(anio) {
  # leer sin tipos fijos primero para detectar el nombre del factor
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

  df <- read_csv(
    paste0("data/raw/basesConcentrados/concentradohogar", anio, ".csv"),
    locale    = locale(encoding = "latin1"),
    col_types = col_spec
  )
  if ("factor_hog" %in% names(df)) df <- df %>% rename(factor = factor_hog)
  df %>% mutate(year = anio)
}

concentrado <- map_dfr(anios, leer_concentrado) %>%
  mutate(
    entidad   = as.numeric(substr(ubica_geo, 1, 2)),
    municipio = as.numeric(substr(ubica_geo, 3, 5))
  ) %>%
  select(-ubica_geo)

tasas_homicidios <- read_dta("data/processed/tasas_homicidios.dta")

mode_mes <- function(x) {
  x <- x[!is.na(x) & x > 0 & x <= 12]
  if (length(x) == 0) return(NA_integer_)
  as.integer(names(sort(table(x), decreasing = TRUE))[1])
}

mes_hogar <- gastoshogar %>%
  mutate(mes = as.integer(substr(
    formatC(as.integer(mes_dia), width = 4, flag = "0"), 1, 2))) %>%
  group_by(folioviv, foliohog, year) %>%
  summarise(mes = mode_mes(mes), .groups = "drop")

base_completa <- read_csv("data/processed/base_completa.csv", show_col_types = FALSE)

cat("   Insumos cargados.\n\n")


# ==============================================================================
# CHEQUEO 1: Pérdida por `cantidad` en gastoshogar
# ¿Cuántos registros de gasto se pierden porque cantidad es NA o cero?
# ==============================================================================

cat("[1] Pérdida por `cantidad` en gastoshogar...\n")

c1 <- gastoshogar %>%
  group_by(year) %>%
  summarise(
    N_registros = n(),
    cant_NA     = sum(is.na(cantidad)),
    cant_cero   = sum(!is.na(cantidad) & cantidad <= 0),
    cant_util   = sum(!is.na(cantidad) & cantidad > 0),
    pct_NA      = round(cant_NA   / N_registros * 100, 1),
    pct_cero    = round(cant_cero / N_registros * 100, 1),
    pct_util    = round(cant_util / N_registros * 100, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, N = N_registros,
         `NA` = cant_NA, `Cero o neg` = cant_cero, Utiles = cant_util,
         `pct NA` = pct_NA, `pct cero` = pct_cero, `pct util` = pct_util)

tex_out(c1, "01_perdida_cantidad.tex",
        "P\\'erdida de registros por \\texttt{cantidad} NA o cero en gastoshogar por a\\~{n}o")


# ==============================================================================
# CHEQUEO 2: Distribución de `cantidad` — detectar outliers extremos
# Un valor de cantidad muy grande (p. ej. > 500) inflaría mucho las calorías
# ==============================================================================

cat("[2] Distribucion de cantidad (positivos)...\n")

c2 <- gastoshogar %>%
  filter(!is.na(cantidad), cantidad > 0) %>%
  group_by(year) %>%
  summarise(
    N      = n(),
    media  = round(mean(cantidad), 1),
    p50    = round(median(cantidad), 1),
    p75    = round(quantile(cantidad, 0.75), 1),
    p95    = round(quantile(cantidad, 0.95), 1),
    p99    = round(quantile(cantidad, 0.99), 1),
    max    = round(max(cantidad), 1),
    N_gt100  = sum(cantidad > 100),
    N_gt500  = sum(cantidad > 500),
    pct_gt500 = round(mean(cantidad > 500) * 100, 2),
    .groups = "drop"
  ) %>%
  rename(Anio = year, Media = media, P50 = p50, P75 = p75,
         P95 = p95, P99 = p99, Max = max,
         `N>100` = N_gt100, `N>500` = N_gt500, `pct>500` = pct_gt500)

tex_out(c2, "02_distribucion_cantidad.tex",
        "Distribuci\\'on de \\texttt{cantidad} (solo positivos) por a\\~{n}o")


# ==============================================================================
# CHEQUEO 3: Cobertura calórica — match clave → xbarra_total
# ¿Qué fracción de registros de gasto tiene densidad calórica asignada?
# Desglose: claves nuevas (2024) vs. viejas (2012–2022)
# ==============================================================================

cat("[3] Cobertura calorica (match xbarra)...\n")

gh_jx <- gastoshogar %>%
  filter(!is.na(cantidad), cantidad > 0) %>%
  left_join(xbarra_total, by = "clave") %>%
  mutate(
    tipo_clave = case_when(
      year == 2024 & clave %in% xbarra$clave    ~ "nueva (6 dig)",
      year <  2024 & clave %in% xbarra_viejo$clave ~ "vieja (4 car)",
      TRUE ~ "sin match"
    )
  )

c3a <- gh_jx %>%
  group_by(year) %>%
  summarise(
    N           = n(),
    con_kcal    = sum(!is.na(kcal_media)),
    sin_kcal    = sum(is.na(kcal_media)),
    pct_con     = round(con_kcal / N * 100, 1),
    claves_uniq = n_distinct(clave),
    claves_sin  = n_distinct(clave[is.na(kcal_media)]),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `Con kcal` = con_kcal, `Sin kcal` = sin_kcal,
         `pct con` = pct_con, `Claves uniq` = claves_uniq,
         `Claves sin match` = claves_sin)

tex_out(c3a, "03a_cobertura_kcal_anio.tex",
        "Cobertura cal\\'orica: registros con densidad cal\\'orica disponible por a\\~{n}o")

# Top 20 claves sin match (por frecuencia total)
c3b <- gh_jx %>%
  filter(is.na(kcal_media)) %>%
  count(clave, year) %>%
  group_by(clave) %>%
  summarise(
    N_total = sum(n),
    anios   = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(N_total)) %>%
  slice_head(n = 20) %>%
  rename(Clave = clave, `N registros` = N_total, `Anos presentes` = anios)

tex_out(c3b, "03b_top20_claves_sin_match.tex",
        "Top 20 claves m\\'as frecuentes sin densidad cal\\'orica en \\texttt{xbarra\\_total}")

rm(gh_jx)


# ==============================================================================
# CHEQUEO 4: pct_match por hogar — ¿qué fracción del gasto de cada hogar
# tiene densidad calórica asignada? Hogares con pct_match bajo tienen
# estimación de calorías muy imprecisa.
# ==============================================================================

cat("[4] pct_match por hogar...\n")

calorias_hogar <- gastoshogar %>%
  filter(!is.na(cantidad), cantidad > 0) %>%
  left_join(xbarra_total, by = "clave") %>%
  mutate(kcal = kcal_media * cantidad / 1000) %>%
  group_by(folioviv, foliohog, year) %>%
  summarise(
    kcal_total = sum(kcal, na.rm = TRUE),
    n_prods    = n(),
    n_match    = sum(!is.na(kcal_media)),
    pct_match  = n_match / n_prods,
    .groups = "drop"
  )

c4 <- calorias_hogar %>%
  group_by(year) %>%
  summarise(
    N_hogares   = n(),
    media       = round(mean(pct_match), 3),
    p10         = round(quantile(pct_match, 0.10), 3),
    p25         = round(quantile(pct_match, 0.25), 3),
    p50         = round(median(pct_match), 3),
    p75         = round(quantile(pct_match, 0.75), 3),
    N_lt50pct   = sum(pct_match < 0.5),
    pct_lt50    = round(mean(pct_match < 0.5) * 100, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `N hogares` = N_hogares, Media = media,
         P10 = p10, P25 = p25, P50 = p50, P75 = p75,
         `N<50pct` = N_lt50pct, `pct<50pct` = pct_lt50)

tex_out(c4, "04_pct_match_hogares.tex",
        "Cobertura cal\\'orica a nivel hogar: distribuci\\'on de \\texttt{pct\\_match} por a\\~{n}o")


# ==============================================================================
# CHEQUEO 5: Hogares con kcal_total = 0
# Ocurre cuando ninguno de los productos del hogar tiene densidad calórica
# ==============================================================================

cat("[5] Hogares con kcal_total = 0...\n")

c5 <- calorias_hogar %>%
  left_join(concentrado %>% select(folioviv, foliohog, year, tot_integ),
            by = c("folioviv", "foliohog", "year")) %>%
  mutate(kcal_pc = kcal_total / tot_integ) %>%
  group_by(year) %>%
  summarise(
    N_hogares  = n(),
    kcal_cero  = sum(kcal_total == 0),
    pct_cero   = round(mean(kcal_total == 0) * 100, 1),
    kcal_pc_NA = sum(is.na(kcal_pc)),
    kcal_pc_cero = sum(!is.na(kcal_pc) & kcal_pc == 0),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `N hogares` = N_hogares,
         `kcal=0` = kcal_cero, `pct kcal=0` = pct_cero,
         `kcal_pc NA` = kcal_pc_NA, `kcal_pc=0` = kcal_pc_cero)

tex_out(c5, "05_hogares_kcal_cero.tex",
        "Hogares con calor\\'ias totales o per c\\'apita iguales a cero por a\\~{n}o")


# ==============================================================================
# CHEQUEO 6: Hogares con muy pocos productos reportados
# Si n_prods es muy bajo, el kcal calculado es un proxy muy pobre del consumo real
# ==============================================================================

cat("[6] Hogares con pocos productos...\n")

c6 <- calorias_hogar %>%
  group_by(year) %>%
  summarise(
    N_hogares   = n(),
    media_prods = round(mean(n_prods), 1),
    p10_prods   = as.integer(quantile(n_prods, 0.10)),
    p50_prods   = as.integer(median(n_prods)),
    N_1prod     = sum(n_prods == 1),
    N_le3prod   = sum(n_prods <= 3),
    pct_le3     = round(mean(n_prods <= 3) * 100, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `N hogares` = N_hogares, `Media prods` = media_prods,
         `P10 prods` = p10_prods, `P50 prods` = p50_prods,
         `N 1 prod` = N_1prod, `N<=3 prods` = N_le3prod, `pct<=3` = pct_le3)

tex_out(c6, "06_hogares_pocos_productos.tex",
        "Hogares con muy pocos productos reportados en gastoshogar por a\\~{n}o")


# ==============================================================================
# CHEQUEO 7: Cobertura temporal — mes_dia → mes
# Si mes_dia es 0 para todos los registros del hogar, no podemos hacer el join
# con tasas de homicidios (que está a nivel mes-municipio)
# ==============================================================================

cat("[7] Cobertura temporal (mes_dia)...\n")

c7a <- mes_hogar %>%
  group_by(year) %>%
  summarise(
    N_hogares  = n(),
    sin_mes    = sum(is.na(mes)),
    pct_sin    = round(mean(is.na(mes)) * 100, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `N hogares` = N_hogares,
         `Sin mes` = sin_mes, `pct sin mes` = pct_sin)

tex_out(c7a, "07a_cobertura_mes.tex",
        "Hogares sin mes de levantamiento asignable (\\texttt{mes\\_dia} = 0) por a\\~{n}o")

# Distribución de meses por año (¿el levantamiento cubre meses uniformemente?)
c7b <- mes_hogar %>%
  filter(!is.na(mes)) %>%
  group_by(year, mes) %>%
  summarise(hogares = n(), .groups = "drop") %>%
  pivot_wider(names_from = mes, values_from = hogares,
              values_fill = 0L, names_prefix = "m") %>%
  arrange(year) %>%
  rename(Anio = year)

tex_out(c7b, "07b_distribucion_mes.tex",
        "Distribuci\\'on de hogares por mes de levantamiento y a\\~{n}o (solo con mes asignable)")


# ==============================================================================
# CHEQUEO 8: Municipios de ENIGH vs. tasas_homicidios
# ¿Hay municipios que aparecen en la encuesta pero no tienen datos de homicidios?
# Pueden ser municipios de nueva creación (post-censo) o con código distinto
# ==============================================================================

cat("[8] Cobertura municipal ENIGH vs. homicidios...\n")

munis_enigh <- concentrado %>% distinct(entidad, municipio, year)

munis_hom <- tasas_homicidios %>%
  rename(year = anio_hom, entidad = entidad_hom, municipio = municipio_hom) %>%
  distinct(entidad, municipio, year)

c8a <- munis_enigh %>%
  left_join(munis_hom %>% mutate(en_hom = TRUE), by = c("entidad", "municipio", "year")) %>%
  group_by(year) %>%
  summarise(
    N_munis_enigh  = n(),
    con_datos_hom  = sum(!is.na(en_hom)),
    sin_datos_hom  = sum(is.na(en_hom)),
    pct_sin        = round(mean(is.na(en_hom)) * 100, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `Munis ENIGH` = N_munis_enigh,
         `Con hom.` = con_datos_hom, `Sin hom.` = sin_datos_hom,
         `pct sin hom.` = pct_sin)

tex_out(c8a, "08a_munis_enigh_vs_hom.tex",
        "Municipios de ENIGH con y sin datos de tasas de homicidios por a\\~{n}o")

# Municipios que nunca aparecen en tasas_homicidios (en ningún año)
c8b <- munis_enigh %>%
  anti_join(munis_hom, by = c("entidad", "municipio")) %>%
  distinct(entidad, municipio) %>%
  group_by(entidad) %>%
  summarise(
    N_munis   = n(),
    municipios = paste(sort(municipio), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(N_munis)) %>%
  slice_head(n = 15) %>%
  rename(Entidad = entidad, `N munis sin hom` = N_munis, Municipios = municipios)

tex_out(c8b, "08b_munis_sin_hom_nunca.tex",
        "Municipios en ENIGH sin datos de homicidios en ning\\'un a\\~{n}o (top 15 entidades)")


# ==============================================================================
# CHEQUEO 9: Desglose de pérdida en el join con tasas_homicidios
# Causa 1: mes = NA (sin mes_dia válido)
# Causa 2: municipio no aparece en tasas_homicidios ese año-mes
# ==============================================================================

cat("[9] Perdida en join con tasas de homicidios (desglose de causas)...\n")

cal_full <- calorias_hogar %>%
  left_join(concentrado, by = c("folioviv", "foliohog", "year")) %>%
  left_join(mes_hogar,   by = c("folioviv", "foliohog", "year")) %>%
  mutate(kcal_per_capita = kcal_total / tot_integ)

base_recon <- cal_full %>%
  left_join(tasas_homicidios,
            by = c("entidad"   = "entidad_hom",
                   "municipio" = "municipio_hom",
                   "year"      = "anio_hom",
                   "mes"       = "mes_hom"))

c9 <- base_recon %>%
  group_by(year) %>%
  summarise(
    N_hogares       = n(),
    con_tasa        = sum(!is.na(tasa_homicidios)),
    sin_tasa_total  = sum(is.na(tasa_homicidios)),
    causa_mes_NA    = sum(is.na(tasa_homicidios) & is.na(mes)),
    causa_muni_out  = sum(is.na(tasa_homicidios) & !is.na(mes)),
    pct_sin_tasa    = round(mean(is.na(tasa_homicidios)) * 100, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `N hogares` = N_hogares, `Con tasa` = con_tasa,
         `Sin tasa` = sin_tasa_total, `Por mes NA` = causa_mes_NA,
         `Por muni ausente` = causa_muni_out, `pct sin tasa` = pct_sin_tasa)

tex_out(c9, "09_perdida_join_tasa.tex",
        "P\\'erdida de hogares en el join con tasas de homicidios: desglose por causa y a\\~{n}o")


# ==============================================================================
# CHEQUEO 10: Distribución de kcal_per_capita — outliers
# Calorías muy bajas (< 200 kcal/día) o muy altas (> 10,000) son sospechosas
# ==============================================================================

cat("[10] Distribucion de kcal_per_capita...\n")

c10 <- base_completa %>%
  group_by(year) %>%
  summarise(
    N     = n(),
    media = round(mean(kcal_per_capita, na.rm = TRUE)),
    p10   = round(quantile(kcal_per_capita, 0.10, na.rm = TRUE)),
    p25   = round(quantile(kcal_per_capita, 0.25, na.rm = TRUE)),
    p50   = round(median(kcal_per_capita, na.rm = TRUE)),
    p75   = round(quantile(kcal_per_capita, 0.75, na.rm = TRUE)),
    p90   = round(quantile(kcal_per_capita, 0.90, na.rm = TRUE)),
    p99   = round(quantile(kcal_per_capita, 0.99, na.rm = TRUE)),
    N_lt200   = sum(!is.na(kcal_per_capita) & kcal_per_capita < 200),
    N_gt10000 = sum(!is.na(kcal_per_capita) & kcal_per_capita > 10000),
    .groups = "drop"
  ) %>%
  rename(Anio = year, Media = media, P10 = p10, P25 = p25, P50 = p50,
         P75 = p75, P90 = p90, P99 = p99, `N<200` = N_lt200, `N>10000` = N_gt10000)

tex_out(c10, "10_kcal_pc_distribucion.tex",
        "Distribuci\\'on de calor\\'ias per c\\'apita (kcal/trimestre) en \\texttt{base\\_completa} por a\\~{n}o",
        digits = 0)


# ==============================================================================
# CHEQUEO 11: Distribución de tasa_homicidios en la muestra de regresión
# Detecta si hay valores extremos que puedan dominar la estimación
# ==============================================================================

cat("[11] Distribucion de tasa_homicidios en muestra de regresion...\n")

base_reg <- base_completa %>%
  filter(!is.na(kcal_per_capita), kcal_per_capita > 0,
         !is.na(tasa_homicidios), tasa_homicidios >= 0,
         !is.na(tot_integ), tot_integ > 0) %>%
  mutate(municipio_id = paste(entidad, municipio, sep = "_"))

c11 <- base_reg %>%
  group_by(year) %>%
  summarise(
    N          = n(),
    pct_cero   = round(mean(tasa_homicidios == 0) * 100, 1),
    media      = round(mean(tasa_homicidios), 3),
    p50        = round(median(tasa_homicidios), 3),
    p75        = round(quantile(tasa_homicidios, 0.75), 3),
    p90        = round(quantile(tasa_homicidios, 0.90), 3),
    p95        = round(quantile(tasa_homicidios, 0.95), 3),
    p99        = round(quantile(tasa_homicidios, 0.99), 3),
    max        = round(max(tasa_homicidios), 3),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `pct tasa=0` = pct_cero, Media = media,
         P50 = p50, P75 = p75, P90 = p90, P95 = p95, P99 = p99, Max = max)

tex_out(c11, "11_tasa_hom_distribucion.tex",
        "Distribuci\\'on de la tasa de homicidios (por 10{,}000 hab.) en la muestra de regresi\\'on por a\\~{n}o")


# ==============================================================================
# CHEQUEO 12: Flowchart completo de pérdidas hasta la muestra de regresión
# Cuantifica exactamente cuántos hogares se pierden en cada filtro del código 06
# ==============================================================================

cat("[12] Flowchart de perdidas hasta muestra de regresion...\n")

n0 <- nrow(base_completa)
n1 <- sum(!is.na(base_completa$kcal_per_capita) & base_completa$kcal_per_capita > 0)
n2 <- base_completa %>%
        filter(!is.na(kcal_per_capita), kcal_per_capita > 0,
               !is.na(tasa_homicidios), tasa_homicidios >= 0) %>% nrow()
n3 <- base_completa %>%
        filter(!is.na(kcal_per_capita), kcal_per_capita > 0,
               !is.na(tasa_homicidios), tasa_homicidios >= 0,
               !is.na(tot_integ), tot_integ > 0) %>% nrow()

c12_total <- tibble(
  Etapa = c(
    "base\\_completa (total)",
    "Filtro 1: kcal\\_pc no-NA y > 0",
    "Filtro 2: tasa\\_homicidios no-NA",
    "Filtro 3: tot\\_integ no-NA y > 0 [MUESTRA FINAL]"
  ),
  N     = c(n0, n1, n2, n3),
  Perdidos_paso  = c(NA_integer_, n0-n1, n1-n2, n2-n3),
  pct_perdido_paso = c(NA_real_, round((n0-n1)/n0*100,1),
                       round((n1-n2)/n0*100,1), round((n2-n3)/n0*100,1)),
  pct_retenido   = round(c(n0,n1,n2,n3)/n0*100, 1)
) %>%
  rename(`Perdidos (paso)` = Perdidos_paso,
         `pct perdido paso` = pct_perdido_paso,
         `pct retenido` = pct_retenido)

tex_out(c12_total, "12a_flowchart_perdidas_total.tex",
        "P\\'erdida total de observaciones en cada filtro de la muestra de regresi\\'on (c\\'odigo 06)")

c12_anio <- base_completa %>%
  group_by(year) %>%
  summarise(
    N_total   = n(),
    paso1     = sum(!is.na(kcal_per_capita) & kcal_per_capita > 0),
    paso2     = sum(!is.na(kcal_per_capita) & kcal_per_capita > 0 &
                      !is.na(tasa_homicidios) & tasa_homicidios >= 0),
    paso3     = sum(!is.na(kcal_per_capita) & kcal_per_capita > 0 &
                      !is.na(tasa_homicidios) & tasa_homicidios >= 0 &
                      !is.na(tot_integ) & tot_integ > 0),
    pct_ret   = round(paso3 / N_total * 100, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `N total` = N_total, `Paso 1` = paso1,
         `Paso 2` = paso2, `Paso 3 (final)` = paso3, `pct retenido` = pct_ret)

tex_out(c12_anio, "12b_flowchart_perdidas_anio.tex",
        "P\\'erdida de observaciones en cada filtro de la muestra de regresi\\'on, por a\\~{n}o")


# ==============================================================================
# CHEQUEO 13: Diagnóstico de lags — ¿son realmente consecutivos en el tiempo?
# PROBLEMA POTENCIAL: ENIGH es bienal. El "lag 1" en lags_muni puede ser
# 2 años atrás (p. ej. 2012→2014), no el mes anterior. Esto rompe la
# interpretación de lag como efecto dinámico de corto plazo.
# ==============================================================================

cat("[13] Diagnostico temporal de lags (ENIGH bienal)...\n")

lags_muni_diag <- base_reg %>%
  filter(!is.na(mes)) %>%
  mutate(log_tasa_hom = log(tasa_homicidios + 1)) %>%
  distinct(municipio_id, year, mes, log_tasa_hom, tasa_homicidios) %>%
  arrange(municipio_id, year, mes) %>%
  group_by(municipio_id) %>%
  mutate(
    fecha      = as.Date(paste(year, mes, "01", sep = "-")),
    fecha_prev = lag(fecha),
    gap_dias   = as.integer(fecha - fecha_prev),
    gap_meses  = round(gap_dias / 30.44, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(gap_meses))

c13a <- lags_muni_diag %>%
  summarise(
    N_pares       = n(),
    gap_media     = round(mean(gap_meses), 1),
    gap_p25       = round(quantile(gap_meses, 0.25), 1),
    gap_p50       = round(median(gap_meses), 1),
    gap_p75       = round(quantile(gap_meses, 0.75), 1),
    N_lt3m        = sum(gap_meses < 3),
    N_3a6m        = sum(gap_meses >= 3 & gap_meses < 6),
    N_6a18m       = sum(gap_meses >= 6 & gap_meses < 18),
    N_gt18m       = sum(gap_meses > 18),
    pct_gt18      = round(mean(gap_meses > 18) * 100, 1)
  ) %>%
  rename(`N pares` = N_pares, `Media (m)` = gap_media,
         `P25 (m)` = gap_p25, `P50 (m)` = gap_p50, `P75 (m)` = gap_p75,
         `N<3m` = N_lt3m, `N 3-6m` = N_3a6m, `N 6-18m` = N_6a18m,
         `N>18m` = N_gt18m, `pct>18m` = pct_gt18)

tex_out(c13a, "13a_lags_gap_temporal.tex",
        "Gap temporal (meses) entre observaciones consecutivas en \\texttt{lags\\_muni}: ¿son lags mensuales reales?")

c13b <- lags_muni_diag %>%
  mutate(
    tipo_gap = case_when(
      gap_meses < 3   ~ "Menos de 3 meses",
      gap_meses < 6   ~ "3 a 6 meses",
      gap_meses < 18  ~ "6 a 18 meses",
      gap_meses < 30  ~ "18 a 30 meses (bienal ENIGH)",
      TRUE            ~ "Mas de 30 meses"
    )
  ) %>%
  count(tipo_gap, name = "N") %>%
  mutate(pct = round(N / sum(N) * 100, 1)) %>%
  arrange(desc(N)) %>%
  rename(`Tipo de gap` = tipo_gap, `N pares` = N, `pct del total` = pct)

tex_out(c13b, "13b_lags_tipo_gap.tex",
        "Clasificaci\\'on de gaps temporales entre observaciones consecutivas de \\texttt{lags\\_muni}")


# ==============================================================================
# CHEQUEO 14: Balance del panel en la muestra de regresión
# ¿Cuántos municipios aparecen en todos los años vs. en pocos?
# ==============================================================================

cat("[14] Balance del panel...\n")

c14a <- base_reg %>%
  group_by(year) %>%
  summarise(
    N_hogares   = n(),
    N_munis     = n_distinct(municipio_id),
    N_entidades = n_distinct(entidad),
    hogares_muni = round(n() / n_distinct(municipio_id), 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `N hogares` = N_hogares, `N munis` = N_munis,
         `N entidades` = N_entidades, `Hogares/muni` = hogares_muni)

tex_out(c14a, "14a_panel_hogares_munis.tex",
        "Hogares y municipios únicos en la muestra de regresi\\'on por a\\~{n}o")

n_anios_reg <- base_reg %>% filter(year < 2024) %>% pull(year) %>% n_distinct()

c14b <- base_reg %>%
  filter(year < 2024) %>%
  group_by(municipio_id) %>%
  summarise(n_anios = n_distinct(year), .groups = "drop") %>%
  count(n_anios, name = "N_munis") %>%
  mutate(
    pct = round(N_munis / sum(N_munis) * 100, 1),
    panel_completo = ifelse(n_anios == n_anios_reg, "Si", "No")
  ) %>%
  arrange(desc(n_anios)) %>%
  rename(`N anos` = n_anios, `N municipios` = N_munis,
         `pct municipios` = pct, `Panel completo` = panel_completo)

tex_out(c14b, "14b_balance_panel.tex",
        paste0("Balance del panel: municipios seg\\'un cu\\'antos a\\~{n}os aparecen en muestra (sin 2024; total = ",
               n_anios_reg, " a\\~{n}os)"))


# ==============================================================================
# CHEQUEO 15: Cobertura de tasa_homicidios por entidad
# ¿Hay estados donde sistemáticamente se pierde el join con homicidios?
# ==============================================================================

cat("[15] Cobertura de tasa_homicidios por entidad...\n")

c15 <- base_completa %>%
  group_by(entidad, year) %>%
  summarise(
    N_hogares    = n(),
    pct_con_tasa = round(mean(!is.na(tasa_homicidios)) * 100, 1),
    .groups = "drop"
  ) %>%
  group_by(entidad) %>%
  summarise(
    N_anios      = n(),
    N_hogares    = sum(N_hogares),
    pct_min      = round(min(pct_con_tasa), 1),
    pct_media    = round(mean(pct_con_tasa), 1),
    pct_max      = round(max(pct_con_tasa), 1),
    .groups = "drop"
  ) %>%
  arrange(pct_media) %>%
  rename(Entidad = entidad, `N anos` = N_anios, `N hogares` = N_hogares,
         `pct min` = pct_min, `pct media` = pct_media, `pct max` = pct_max)

tex_out(c15, "15_cobertura_tasa_por_entidad.tex",
        "Cobertura de tasa de homicidios por entidad, ordenado de menor a mayor cobertura media")


# ==============================================================================
# CHEQUEO 16: Consistencia del join gastoshogar–concentrado
# ¿Hay hogares que aparecen en gastoshogar pero no en concentrado (y viceversa)?
# ==============================================================================

cat("[16] Consistencia join gastoshogar-concentrado...\n")

hogs_gh   <- gastoshogar %>% distinct(folioviv, foliohog, year)
hogs_conc <- concentrado %>% distinct(folioviv, foliohog, year)

en_gh_no_conc <- hogs_gh   %>% anti_join(hogs_conc, by = c("folioviv","foliohog","year"))
en_conc_no_gh <- hogs_conc %>% anti_join(hogs_gh,   by = c("folioviv","foliohog","year"))

c16 <- bind_rows(
  en_gh_no_conc %>% mutate(caso = "En gastoshogar, NO en concentrado"),
  en_conc_no_gh %>% mutate(caso = "En concentrado, NO en gastoshogar")
) %>%
  count(year, caso) %>%
  pivot_wider(names_from = caso, values_from = n, values_fill = 0L) %>%
  arrange(year) %>%
  rename(Anio = year)

tex_out(c16, "16_consistencia_gh_concentrado.tex",
        "Hogares presentes en un archivo pero no en el otro: gastoshogar vs.~concentrado")


# ==============================================================================
# RESUMEN EJECUTIVO — Tabla 00 con los números clave del pipeline
# ==============================================================================

cat("[RESUMEN] Generando resumen ejecutivo...\n")

# registros de gasto
n_gh_total  <- nrow(gastoshogar)
n_gh_util   <- sum(!is.na(gastoshogar$cantidad) & gastoshogar$cantidad > 0)
n_gh_kcal   <- gastoshogar %>%
  filter(!is.na(cantidad), cantidad > 0) %>%
  left_join(xbarra_total %>% select(clave), by = "clave") %>%
  summarise(n = n()) %>% pull(n)  # con clave en xbarra_total (aproximado)

# en realidad n_gh_kcal es cuántos registros tienen match (kcal_media no NA)
n_gh_kcal <- gastoshogar %>%
  filter(!is.na(cantidad), cantidad > 0) %>%
  left_join(xbarra_total, by = "clave") %>%
  summarise(n = sum(!is.na(kcal_media))) %>% pull(n)

# hogares
n_hogs_conc  <- nrow(concentrado)
n_hogs_cal   <- nrow(calorias_hogar)
n_hogs_bc    <- nrow(base_completa)
n_hogs_tasa  <- sum(!is.na(base_completa$tasa_homicidios))
n_hogs_reg   <- n3

resumen_ej <- tibble(
  Nivel = c(
    "REGISTROS DE GASTO (gastoshogar)",
    "  1. Con cantidad valida (> 0)",
    "  2. Con densidad calorica (match xbarra)",
    "HOGARES (concentrado hogar)",
    "  3. Con calorias calculadas",
    "  4. En base\\_completa (tras merge)",
    "  5. Con tasa de homicidios disponible",
    "  6. Muestra final de regresion"
  ),
  N = c(n_gh_total, n_gh_util, n_gh_kcal,
        n_hogs_conc, n_hogs_cal, n_hogs_bc, n_hogs_tasa, n_hogs_reg),
  `pct vs inicio` = c(
    100.0,
    round(n_gh_util / n_gh_total * 100, 1),
    round(n_gh_kcal / n_gh_total * 100, 1),
    100.0,
    round(n_hogs_cal  / n_hogs_conc * 100, 1),
    round(n_hogs_bc   / n_hogs_conc * 100, 1),
    round(n_hogs_tasa / n_hogs_conc * 100, 1),
    round(n_hogs_reg  / n_hogs_conc * 100, 1)
  )
)

tex_out(resumen_ej, "00_RESUMEN_EJECUTIVO.tex",
        "Resumen ejecutivo: p\\'erdida de informaci\\'on a lo largo de todo el pipeline de datos")


# ==============================================================================
# ══════════════════════════════════════════════════════════════════════════════
# BLOQUE B: CHEQUEOS DE UNIDADES Y PRECIOS
# Verifica si `cantidad` en gastoshogar tiene sentido como kg/litro
# ══════════════════════════════════════════════════════════════════════════════
# ==============================================================================

# Catálogo de conceptos para etiquetar claves
catalogo_labels <- read_csv("data/processed/catalogoCruzadoENIGH.csv",
                             show_col_types = FALSE) %>%
  select(clave, claveAntes, concepto) %>%
  # incluir también las claves viejas con el mismo concepto
  bind_rows(
    read_csv("data/processed/catalogoCruzadoENIGH.csv",
             show_col_types = FALSE) %>%
      filter(!is.na(claveAntes)) %>%
      transmute(clave = claveAntes, claveAntes = NA_character_, concepto)
  ) %>%
  distinct(clave, .keep_all = TRUE)


# ==============================================================================
# CHEQUEO 17: G1 (alimentos) vs. no-G1 — ¿qué tipo de gasto tiene cantidad?
# El código 05 usa TODOS los registros con cantidad > 0, no sólo alimentos.
# Si hay claves no-G1 que hacen match con xbarra_total, se estarían contando
# calorías de gastos no alimentarios.
# ==============================================================================

cat("[17] G1 vs no-G1 con cantidad valida...\n")

c17a <- gastoshogar %>%
  mutate(
    es_G1         = tipo_gasto == "G1",
    cant_util     = !is.na(cantidad) & cantidad > 0,
    en_xbarra     = clave %in% xbarra_total$clave
  ) %>%
  group_by(year, tipo_gasto) %>%
  summarise(
    N_registros  = n(),
    con_cantidad = sum(cant_util),
    con_kcal     = sum(cant_util & en_xbarra),
    pct_con_cant = round(mean(cant_util) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(year, desc(con_cantidad)) %>%
  rename(Anio = year, Tipo = tipo_gasto, N = N_registros,
         `Con cant.` = con_cantidad, `Con kcal` = con_kcal,
         `pct con cant.` = pct_con_cant)

tex_out(c17a, "17a_tipo_gasto_cobertura.tex",
        "Registros por tipo de gasto: cobertura de \\texttt{cantidad} y match cal\\'orico")

# Claves NO-G1 que SÍ tienen match en xbarra_total (potencial contaminación)
claves_no_g1_con_kcal <- gastoshogar %>%
  filter(tipo_gasto != "G1", !is.na(cantidad), cantidad > 0,
         clave %in% xbarra_total$clave) %>%
  count(clave, tipo_gasto, year, name = "n") %>%
  group_by(clave, tipo_gasto) %>%
  summarise(N_total = sum(n),
            anios = paste(sort(unique(year)), collapse = ", "),
            .groups = "drop") %>%
  arrange(desc(N_total)) %>%
  left_join(catalogo_labels %>% select(clave, concepto), by = "clave")

if (nrow(claves_no_g1_con_kcal) > 0) {
  tex_out(claves_no_g1_con_kcal %>%
            rename(Clave = clave, Tipo = tipo_gasto, `N total` = N_total,
                   Anos = anios, Concepto = concepto),
          "17b_claves_no_G1_con_kcal.tex",
          "Claves fuera de G1 (alimentos) que tienen match en \\texttt{xbarra\\_total}")
} else {
  cat("  -> No hay claves no-G1 con match en xbarra_total\n")
}


# ==============================================================================
# CHEQUEO 18: G1 con cantidad NA — gasto alimentario excluido de kcal
# Los registros G1 con cantidad=NA tienen gasto registrado pero NO contribuyen
# a kcal_total porque no se puede calcular la cantidad consumida.
# ==============================================================================

cat("[18] Gasto alimentario G1 excluido por cantidad NA...\n")

c18 <- gastoshogar %>%
  filter(tipo_gasto == "G1") %>%
  group_by(year) %>%
  summarise(
    N_G1            = n(),
    N_cant_NA       = sum(is.na(cantidad)),
    N_cant_cero     = sum(!is.na(cantidad) & cantidad <= 0),
    N_cant_util     = sum(!is.na(cantidad) & cantidad > 0),
    pct_excluido    = round((N_cant_NA + N_cant_cero) / N_G1 * 100, 1),
    gasto_total_G1  = round(sum(gasto, na.rm = TRUE)),
    gasto_excluido  = round(sum(gasto[is.na(cantidad) | cantidad <= 0], na.rm = TRUE)),
    pct_gasto_excl  = round(gasto_excluido / gasto_total_G1 * 100, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `N G1` = N_G1, `NA cant.` = N_cant_NA,
         `Cero cant.` = N_cant_cero, `Util` = N_cant_util,
         `pct excl.` = pct_excluido, `Gasto G1` = gasto_total_G1,
         `Gasto excl.` = gasto_excluido, `pct gasto excl.` = pct_gasto_excl)

tex_out(c18, "18_G1_cantidad_NA_gasto.tex",
        "Registros de alimentos (G1) excluidos por \\texttt{cantidad} NA o cero, con gasto correspondiente")


# ==============================================================================
# CHEQUEO 19: Precio implícito por categoría — ¿tiene sentido como pesos/kg?
# precio = gasto / cantidad. Si cantidad está en kg/L, precio = pesos/kg.
# Categorías con precio muy bajo (< 10) o muy alto (> 600) son sospechosas.
# Umbrales orientativos para México 2022: básicos ~15-80 pesos/kg,
# carnes ~80-350, lácteos ~12-25/L, abarrotes procesados ~30-250.
# ==============================================================================

cat("[19] Precio implicito por categoria...\n")

# Precio por unidad reportada (gasto/cantidad) para G1 con cantidad válida
precio_cat <- gastoshogar %>%
  filter(tipo_gasto == "G1", !is.na(cantidad), cantidad > 0,
         !is.na(gasto), gasto > 0) %>%
  mutate(precio_unit = gasto / cantidad) %>%
  left_join(catalogo_labels %>% select(clave, concepto), by = "clave") %>%
  group_by(clave, concepto) %>%
  summarise(
    N_obs      = n(),
    med_cant   = round(median(cantidad), 2),
    med_precio = round(median(precio_unit), 1),
    p10_precio = round(quantile(precio_unit, 0.10), 1),
    p90_precio = round(quantile(precio_unit, 0.90), 1),
    sd_precio  = round(sd(precio_unit), 1),
    .groups = "drop"
  ) %>%
  mutate(
    flag_unidad = case_when(
      med_precio <  10  ~ "MUY BAJO: posible unidad mayor (100g, pieza chica?)",
      med_precio >  600 ~ "MUY ALTO: posible unidad = pieza cara",
      med_precio >  300 ~ "ALTO: revisar (pieza o producto premium)",
      TRUE              ~ "OK"
    )
  ) %>%
  arrange(med_precio)

# Tabla completa de precios por categoría
tex_out(precio_cat %>%
          rename(Clave = clave, Concepto = concepto, N = N_obs,
                 `Med cant.` = med_cant, `Med precio` = med_precio,
                 `P10 precio` = p10_precio, `P90 precio` = p90_precio,
                 `SD precio` = sd_precio, Flag = flag_unidad),
        "19a_precio_implicito_todas_categorias.tex",
        "Precio impl\\'icito por unidad (gasto/cantidad) para alimentos G1 — todas las categor\\'ias")

# Tabla de categorías con flags (posibles problemas de unidad)
c19b <- precio_cat %>%
  filter(flag_unidad != "OK") %>%
  arrange(desc(N_obs)) %>%
  rename(Clave = clave, Concepto = concepto, N = N_obs,
         `Med cant.` = med_cant, `Med precio` = med_precio,
         `P10` = p10_precio, `P90` = p90_precio, Flag = flag_unidad) %>%
  select(-sd_precio)

if (nrow(c19b) > 0) {
  tex_out(c19b, "19b_categorias_precio_sospechoso.tex",
          "Categor\\'ias con precio impl\\'icito sospechoso (posible unidad distinta a kg/L)")
} else {
  cat("  -> Ninguna categoria con precio sospechoso\n")
}


# ==============================================================================
# CHEQUEO 20: Consistencia de precio implícito por categoría entre años
# Si la unidad de `cantidad` cambia entre rondas del ENIGH, el precio implícito
# dará un salto que no corresponde a inflación (inflación México ~5-8% anual).
# Un salto > 50% en un solo bienio es señal de alerta.
# ==============================================================================

cat("[20] Consistencia del precio implicito entre anos...\n")

precio_anio <- gastoshogar %>%
  filter(tipo_gasto == "G1", !is.na(cantidad), cantidad > 0,
         !is.na(gasto), gasto > 0) %>%
  mutate(precio_unit = gasto / cantidad) %>%
  left_join(catalogo_labels %>% select(clave, concepto), by = "clave") %>%
  group_by(clave, concepto, year) %>%
  summarise(
    N          = n(),
    med_precio = round(median(precio_unit), 2),
    med_cant   = round(median(cantidad), 3),
    .groups = "drop"
  )

# Calcular cambio bienal en precio y cantidad por categoría
precio_cambio <- precio_anio %>%
  arrange(clave, year) %>%
  group_by(clave) %>%
  mutate(
    precio_prev  = lag(med_precio),
    cant_prev    = lag(med_cant),
    year_prev    = lag(year),
    cambio_precio = round((med_precio - precio_prev) / precio_prev * 100, 1),
    cambio_cant   = round((med_cant   - cant_prev)   / cant_prev   * 100, 1)
  ) %>%
  filter(!is.na(precio_prev)) %>%
  ungroup()

# Tabla con top categorías por mayor cambio bienal en precio implícito
c20a <- precio_cambio %>%
  mutate(abs_cambio = abs(cambio_precio)) %>%
  arrange(desc(abs_cambio)) %>%
  slice_head(n = 30) %>%
  left_join(catalogo_labels %>% select(clave, concepto), by = "clave") %>%
  mutate(
    concepto = coalesce(concepto.y, concepto.x),
    alerta = ifelse(abs_cambio > 50, "ALERTA", "")
  ) %>%
  select(clave, concepto, year_prev, year, cambio_precio, cambio_cant,
         med_precio, precio_prev, alerta) %>%
  rename(Clave = clave, Concepto = concepto, `Anio prev.` = year_prev,
         `Anio` = year, `Cambio precio (pct)` = cambio_precio,
         `Cambio cant. (pct)` = cambio_cant, `Precio` = med_precio,
         `Precio prev.` = precio_prev, Alerta = alerta)

tex_out(c20a, "20a_cambio_precio_bienal.tex",
        "Top 30 categor\\'ias por mayor cambio bienal en precio impl\\'icito (gasto/cantidad)")

# Tabla pivot: precio por año para categorías clave (tortillas, arroz, frijol, pollo)
claves_clave <- c("A001", "A003", "A004", "A019", "011111", "011131", "011112", "011193")
c20b <- precio_anio %>%
  filter(clave %in% claves_clave | clave %in%
           (catalogo_labels %>%
              filter(grepl("tortilla|arroz|maiz|frijol|pollo|leche|aceite",
                           concepto, ignore.case = TRUE)) %>% pull(clave))) %>%
  select(clave, concepto, year, med_precio, med_cant, N) %>%
  arrange(clave, year)

if (nrow(c20b) > 0) {
  tex_out(c20b %>%
            rename(Clave = clave, Concepto = concepto, Anio = year,
                   `Precio med.` = med_precio, `Cant. med.` = med_cant),
          "20b_precio_categorias_clave.tex",
          "Precio impl\\'icito (pesos/unidad) para categor\\'ias clave a trav\\'es del tiempo")
}


# ==============================================================================
# CHEQUEO 21: kcal_total implícita con fórmula correcta vs. actual
# La fórmula actual: kcal = kcal_media * cantidad / 1000
# Si kcal_media está en kcal/kg y cantidad en kg, la fórmula correcta es:
#   kcal = kcal_media * cantidad  (sin división)
# Este chequeo compara ambas y muestra la distribución de kcal_pc resultante
# ==============================================================================

cat("[21] Comparacion formula kcal: actual vs. corregida...\n")

kcal_comparacion <- gastoshogar %>%
  filter(tipo_gasto == "G1", !is.na(cantidad), cantidad > 0) %>%
  left_join(xbarra_total, by = "clave") %>%
  filter(!is.na(kcal_media)) %>%
  mutate(
    kcal_actual    = kcal_media * cantidad / 1000,   # fórmula usada en cód. 05
    kcal_corregida = kcal_media * cantidad            # si cantidad ya está en kg
  ) %>%
  left_join(concentrado %>% select(folioviv, foliohog, year, tot_integ),
            by = c("folioviv", "foliohog", "year")) %>%
  group_by(folioviv, foliohog, year, tot_integ) %>%
  summarise(
    kcal_pc_actual    = sum(kcal_actual,    na.rm = TRUE) / first(tot_integ),
    kcal_pc_corregida = sum(kcal_corregida, na.rm = TRUE) / first(tot_integ),
    .groups = "drop"
  ) %>%
  filter(!is.na(tot_integ), tot_integ > 0)

c21 <- kcal_comparacion %>%
  mutate(year = as.integer(year)) %>%
  group_by(year) %>%
  summarise(
    N               = n(),
    # Fórmula actual (/ 1000)
    pc_actual_med   = round(median(kcal_pc_actual,    na.rm = TRUE), 1),
    pc_actual_p90   = round(quantile(kcal_pc_actual,  0.90, na.rm = TRUE), 1),
    # Fórmula corregida (sin / 1000)
    pc_correg_med   = round(median(kcal_pc_corregida, na.rm = TRUE), 1),
    pc_correg_p90   = round(quantile(kcal_pc_corregida, 0.90, na.rm = TRUE), 1),
    pc_correg_p10   = round(quantile(kcal_pc_corregida, 0.10, na.rm = TRUE), 1),
    # kcal/día implícitas (dividir entre 91 días del trimestre)
    kcal_dia_actual  = round(median(kcal_pc_actual,    na.rm = TRUE) / 91, 1),
    kcal_dia_correg  = round(median(kcal_pc_corregida, na.rm = TRUE) / 91, 1),
    .groups = "drop"
  ) %>%
  rename(Anio = year, `N hogares` = N,
         `P50 actual` = pc_actual_med, `P90 actual` = pc_actual_p90,
         `P50 correg.` = pc_correg_med, `P10 correg.` = pc_correg_p10,
         `P90 correg.` = pc_correg_p90,
         `kcal/dia actual` = kcal_dia_actual, `kcal/dia correg.` = kcal_dia_correg)

tex_out(c21, "21_kcal_formula_actual_vs_corregida.tex",
        "Comparaci\\'on de calor\\'ias per c\\'apita: f\\'ormula actual (\\texttt{/1000}) vs. corregida (solo G1)")


# ==============================================================================
# CHEQUEO 22: Categorías con cantidad mediana > 5 kg por compra
# Compras de más de 5 kg en una transacción son inusuales para la mayoría
# de los alimentos. Pueden indicar: a) unidades distintas a kg (cientos de g,
# piezas), b) compras al mayoreo, o c) errores de captura.
# ==============================================================================

cat("[22] Categorias con cantidad mediana alta (posible unidad != kg)...\n")

c22 <- gastoshogar %>%
  filter(tipo_gasto == "G1", !is.na(cantidad), cantidad > 0) %>%
  left_join(catalogo_labels %>% select(clave, concepto), by = "clave") %>%
  group_by(clave, concepto) %>%
  summarise(
    N          = n(),
    med_cant   = round(median(cantidad), 2),
    p75_cant   = round(quantile(cantidad, 0.75), 2),
    p95_cant   = round(quantile(cantidad, 0.95), 2),
    max_cant   = round(max(cantidad), 1),
    N_gt5kg    = sum(cantidad > 5),
    pct_gt5kg  = round(mean(cantidad > 5) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(med_cant > 2 | pct_gt5kg > 10) %>%
  arrange(desc(med_cant)) %>%
  rename(Clave = clave, Concepto = concepto, `Med cant.` = med_cant,
         `P75` = p75_cant, `P95` = p95_cant, `Max` = max_cant,
         `N>5kg` = N_gt5kg, `pct>5kg` = pct_gt5kg)

tex_out(c22, "22_categorias_cantidad_alta.tex",
        "Categor\\'ias alimentarias con cantidad mediana alta (posible unidad distinta a kg)")


# ==============================================================================
# CHEQUEO 23: Homologación entre años — ¿cambia la cantidad mediana entre rondas?
# Si INEGI cambia la unidad de reporte de una categoría (ej. de piezas a kg),
# la cantidad mediana cambiará drásticamente. Esto rompe la comparabilidad.
# ==============================================================================

cat("[23] Cambio en cantidad mediana entre anos (homologacion)...\n")

cant_anio <- gastoshogar %>%
  filter(tipo_gasto == "G1", !is.na(cantidad), cantidad > 0) %>%
  left_join(catalogo_labels %>% select(clave, concepto), by = "clave") %>%
  group_by(clave, concepto, year) %>%
  summarise(
    N        = n(),
    med_cant = round(median(cantidad), 3),
    .groups = "drop"
  )

# Cambio bienal en cantidad mediana por categoría
cant_cambio <- cant_anio %>%
  arrange(clave, year) %>%
  group_by(clave, concepto) %>%
  mutate(
    cant_prev      = lag(med_cant),
    year_prev      = lag(year),
    cambio_cant_pct = round((med_cant - cant_prev) / cant_prev * 100, 1)
  ) %>%
  filter(!is.na(cant_prev), abs(cambio_cant_pct) > 40) %>%  # sólo cambios > 40%
  ungroup() %>%
  arrange(desc(abs(cambio_cant_pct))) %>%
  slice_head(n = 30) %>%
  rename(Clave = clave, Concepto = concepto, `Anio prev.` = year_prev,
         Anio = year, `Cant. prev.` = cant_prev, `Cant.` = med_cant,
         `Cambio (pct)` = cambio_cant_pct)

if (nrow(cant_cambio) > 0) {
  tex_out(cant_cambio,
          "23_cambio_cantidad_mediana_bienal.tex",
          "Top 30 categor\\'ias con mayor cambio bienal en \\texttt{cantidad} mediana (posible cambio de unidad)")
} else {
  cat("  -> No se detectaron cambios de cantidad > 40% entre anios\n")
}


# ==============================================================================
# REPORTE FINAL EN CONSOLA (actualizado)
# ==============================================================================

cat("\n")
cat("=============================================================\n")
cat(" CHEQUEOS COMPLETADOS (16 originales + 7 de unidades)\n")
cat("=============================================================\n")
cat(" Archivos generados en output/chequeos/:\n\n")
archivos <- list.files("output/chequeos/", pattern = "\\.tex$")
cat(paste0("  ", archivos), sep = "\n")
cat("\n")
cat(" HALLAZGOS PRINCIPALES:\n")
cat("  CRITICO - Chequeo 10/21: kcal_per_capita ~10 kcal/trimestre\n")
cat("    -> Verificar si formula debe ser kcal_media*cantidad (sin /1000)\n")
cat("    -> Ver tabla 21 para comparacion de magnitudes\n")
cat("  CRITICO - Chequeo 13: lags temporales = bienales (no mensuales)\n")
cat("  REVISAR  - Chequeo 18: fraccion de gasto G1 sin cantidad\n")
cat("  REVISAR  - Chequeo 19: categorias con precio implicito sospechoso\n")
cat("  REVISAR  - Chequeo 22: categorias con cantidad >> 5 kg por compra\n")
cat("  REVISAR  - Chequeo 23: cambios de cantidad entre anos (homologacion)\n")
cat("=============================================================\n")
