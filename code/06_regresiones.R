# ==============================================================================
# 06_regresiones.R
# ==============================================================================

library(tidyverse)
library(haven)
library(fixest)
library(modelsummary)

dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. CARGAR Y PREPARAR DATOS
# ------------------------------------------------------------------------------



base <- read_csv("data/processed/base_completa.csv",
                 show_col_types = FALSE) %>%
  filter(
    !is.na(kcal_per_capita), kcal_per_capita > 0,
    !is.na(tasa_homicidios), tasa_homicidios >= 0,
    !is.na(tot_integ),       tot_integ > 0
  ) %>%
  mutate(
    log_kcal_pc  = log(kcal_per_capita),
    log_tasa_hom = log(tasa_homicidios + 1),
    municipio_id = paste(entidad, municipio, sep = "_"),
    time_id      = paste(year, mes, sep = "_")
  ) %>%
  rename(peso = factor)





# lags a nivel municipio-período (no a nivel hogar)
lags_muni <- base %>%
  distinct(municipio_id, year, mes, log_tasa_hom, tasa_homicidios) %>%
  arrange(municipio_id, year, mes) %>%
  group_by(municipio_id) %>%
  mutate(
    lag1_hom_log = lag(log_tasa_hom, 1),
    lag2_hom_log = lag(log_tasa_hom, 2),
    lag1_hom_raw = lag(tasa_homicidios, 1),
    lag2_hom_raw = lag(tasa_homicidios, 2)
  ) %>%
  ungroup() %>%
  select(municipio_id, year, mes, lag1_hom_log, lag2_hom_log, lag1_hom_raw, lag2_hom_raw)

base <- base %>%
  left_join(lags_muni, by = c("municipio_id", "year", "mes"))

###
baseMod <- base %>%
  filter(year < 2024)


# ------------------------------------------------------------------------------
# 2. MODELOS LOG-LOG
# Variable dependiente: log(kcal per cápita)
# Variable independiente: log(tasa homicidios + 1)
# Interpretación del coeficiente: elasticidad — un 1% de aumento en homicidios
# se asocia con un X% de cambio en calorías per cápita
# ------------------------------------------------------------------------------

# sin FE ni controles — correlación bruta
m1 <- feols(log_kcal_pc ~ log_tasa_hom,
            weights = ~peso, data = base, cluster = ~municipio_id)

mod1 <- feols(log_kcal_pc ~ log_tasa_hom,
            weights = ~peso, data = baseMod, cluster = ~municipio_id)

mod2 <- feols(log_kcal_pc ~ log_tasa_hom | municipio_id,
            weights = ~peso, data = baseMod, cluster = ~municipio_id)

# + controles de hogar
m2 <- feols(log_kcal_pc ~ log_tasa_hom + tot_integ + pct_match,
            weights = ~peso, data = base, cluster = ~municipio_id)

unique(baseMod$year)

# + FE de año
m3 <- feols(log_kcal_pc ~ log_tasa_hom + tot_integ + pct_match | year,
            weights = ~peso, data = base, cluster = ~municipio_id)

# + FE de estado y año
m4 <- feols(log_kcal_pc ~ log_tasa_hom + tot_integ + pct_match | entidad + year,
            weights = ~peso, data = base, cluster = ~municipio_id)

# two-way FE municipio + tiempo — especificación principal
m5 <- feols(log_kcal_pc ~ log_tasa_hom + tot_integ + pct_match |
              municipio_id + time_id,
            weights = ~peso, data = base, cluster = ~municipio_id)

# two-way FE + lag 1
m6 <- feols(log_kcal_pc ~ log_tasa_hom + lag1_hom_log + tot_integ + pct_match |
              municipio_id + time_id,
            weights = ~peso, data = base, cluster = ~municipio_id)

# two-way FE + lags 1 y 2
m7 <- feols(log_kcal_pc ~ log_tasa_hom + lag1_hom_log + lag2_hom_log +
              tot_integ + pct_match | municipio_id + time_id,
            weights = ~peso, data = base, cluster = ~municipio_id)


mm1 <- feols(log_kcal_pc ~ log_tasa_hom | municipio_id,
             weights = ~peso, data = base, cluster = ~municipio_id)

mm2 <- feols(log_kcal_pc ~ log_tasa_hom | municipio_id + time_id,
             weights = ~peso, data = base, cluster = ~municipio_id)


mm3 <- feols(log_kcal_pc ~ log_tasa_hom + tot_integ | municipio_id + time_id,
             weights = ~peso, data = base, cluster = ~municipio_id)

# lista de modelos con nombres descriptivos
modelos_mm <- list(
  "Solo FE mun."         = mm1,
  "Two-way FE"           = mm2,
  "Two-way FE + integ."  = mm3,
  "Two-way FE + controles" = m5
)

coef_map_mm <- c(
  "log_tasa_hom" = "Log(tasa homicidios $+$ 1)",
  "tot_integ"    = "Integrantes del hogar",
  "pct_match"    = "Cobertura cat\\'{a}logo"
)

fe_rows_mm <- tribble(
  ~term,           ~`Solo FE mun.`, ~`Two-way FE`, ~`Two-way FE + integ.`, ~`Two-way FE + controles`,
  "FE municipio",  "Sí",            "Sí",           "Sí",                   "Sí",
  "FE tiempo",     "No",            "Sí",           "Sí",                   "Sí"
)
attr(fe_rows_mm, "position") <- c(5, 6)

# ver en consola
modelsummary(
  modelos_mm,
  coef_map = coef_map_mm,
  add_rows = fe_rows_mm,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit = "AIC|BIC|Log|RMSE"
)

# guardar en LaTeX
modelsummary(
  modelos_mm,
  output   = "output/tablas/tab_mm.tex",
  coef_map = coef_map_mm,
  add_rows = fe_rows_mm,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit = "AIC|BIC|Log|RMSE",
  title    = "Especificaciones con efectos fijos",
  notes    = "Errores est\\'{a}ndar clusterizados a nivel municipio en par\\'{e}ntesis."
)


# ------------------------------------------------------------------------------
# 3. MODELOS LOG-NIVEL (robustness)
# Variable dependiente: log(kcal per cápita)
# Variable independiente: tasa homicidios (sin log)
# Interpretación: un aumento de 1 homicidio por 10,000 hab se asocia con
# un cambio de X*100% en calorías per cápita
# ------------------------------------------------------------------------------

n1 <- feols(log_kcal_pc ~ tasa_homicidios,
            weights = ~peso, data = base, cluster = ~municipio_id)

n2 <- feols(log_kcal_pc ~ tasa_homicidios + tot_integ + pct_match,
            weights = ~peso, data = base, cluster = ~municipio_id)

n3 <- feols(log_kcal_pc ~ tasa_homicidios + tot_integ + pct_match | year,
            weights = ~peso, data = base, cluster = ~municipio_id)

n4 <- feols(log_kcal_pc ~ tasa_homicidios + tot_integ + pct_match | entidad + year,
            weights = ~peso, data = base, cluster = ~municipio_id)

n5 <- feols(log_kcal_pc ~ tasa_homicidios + tot_integ + pct_match |
              municipio_id + time_id,
            weights = ~peso, data = base, cluster = ~municipio_id)

n6 <- feols(log_kcal_pc ~ tasa_homicidios + lag1_hom_raw + lag2_hom_raw +
              tot_integ + pct_match | municipio_id + time_id,
            weights = ~peso, data = base, cluster = ~municipio_id)



check6 <- feols(log_kcal_pc ~ tasa_homicidios + lag1_hom_raw + lag2_hom_raw +
              tot_integ + pct_match | municipio_id + time_id,
            weights = ~peso, data = baseMod, cluster = ~municipio_id)


basePeña <- base %>%
  filter(year <= 2018) %>%
  filter(year >= 2012)


check66 <- feols(log_kcal_pc ~ tasa_homicidios + lag1_hom_raw + lag2_hom_raw +
              tot_integ + pct_match | municipio_id + time_id,
            weights = ~peso, data = basePeña, cluster = ~municipio_id)
            
# ------------------------------------------------------------------------------
# 4. EXPORTAR CON MODELSUMMARY
# ------------------------------------------------------------------------------

# etiquetas compartidas para controles
coef_map_loglog <- c(
  "log_tasa_hom"  = "Log(tasa homicidios $+$ 1)",
  "lag1_hom_log"  = "Log(tasa homicidios) $t-1$",
  "lag2_hom_log"  = "Log(tasa homicidios) $t-2$",
  "tot_integ"     = "Integrantes del hogar",
  "pct_match"     = "Cobertura cat\\'{a}logo"
)

coef_map_lognivel <- c(
  "tasa_homicidios" = "Tasa homicidios",
  "lag1_hom_raw"    = "Tasa homicidios $t-1$",
  "lag2_hom_raw"    = "Tasa homicidios $t-2$",
  "tot_integ"       = "Integrantes del hogar",
  "pct_match"       = "Cobertura cat\\'{a}logo"
)

# filas adicionales con indicadores de FE
fe_rows_loglog <- tribble(
  ~term,            ~m1,   ~m2,   ~m3,          ~m4,          ~m5,          ~m6,          ~m7,
  "FE año",         "No",  "No",  "Sí",         "Sí",         "No",         "No",         "No",
  "FE entidad",     "No",  "No",  "No",         "Sí",         "No",         "No",         "No",
  "FE municipio",   "No",  "No",  "No",         "No",         "Sí",         "Sí",         "Sí",
  "FE tiempo",      "No",  "No",  "No",         "No",         "Sí",         "Sí",         "Sí"
)
attr(fe_rows_loglog, "position") <- c(9, 10, 11, 12)

fe_rows_lognivel <- tribble(
  ~term,            ~n1,   ~n2,   ~n3,          ~n4,          ~n5,          ~n6,
  "FE año",         "No",  "No",  "Sí",         "Sí",         "No",         "No",
  "FE entidad",     "No",  "No",  "No",         "Sí",         "No",         "No",
  "FE municipio",   "No",  "No",  "No",         "No",         "Sí",         "Sí",
  "FE tiempo",      "No",  "No",  "No",         "No",         "Sí",         "Sí"
)
attr(fe_rows_lognivel, "position") <- c(9, 10, 11, 12)

# tabla log-log
modelsummary(
  list(m1, m2, m3, m4, m5, m6, m7),
  output      = "output/tablas/tab_loglog.tex",
  coef_map    = coef_map_loglog,
  add_rows    = fe_rows_loglog,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit    = "AIC|BIC|Log|RMSE",
  title       = "Ingesta calórica per cápita y tasa de homicidios (log-log)",
  notes       = "Errores estándar clusterizados a nivel municipio en paréntesis."
)

# tabla log-nivel
modelsummary(
  list(n1, n2, n3, n4, n5, n6),
  output      = "output/tablas/tab_lognivel.tex",
  coef_map    = coef_map_lognivel,
  add_rows    = fe_rows_lognivel,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit    = "AIC|BIC|Log|RMSE",
  title       = "Ingesta calórica per cápita y tasa de homicidios (log-nivel)",
  notes       = "Errores estándar clusterizados a nivel municipio en paréntesis."
)

message("Tablas guardadas en output/tablas/")



# ------------------------------------------------------------------------------
# 4. GRÁFICA: CALORÍAS PER CÁPITA PROMEDIO POR AÑO
# ------------------------------------------------------------------------------

calorias_tiempo <- base %>%
  group_by(year) %>%
  summarise(
    kcal_promedio = weighted.mean(kcal_per_capita, w = peso, na.rm = TRUE),
    kcal_p25 = as.numeric(quantile(kcal_per_capita, 0.25, na.rm = TRUE)),
    kcal_p75 = as.numeric(quantile(kcal_per_capita, 0.75, na.rm = TRUE))
  )



p_calorias <- ggplot(calorias_tiempo, aes(x = year, y = kcal_promedio)) +
  geom_ribbon(aes(ymin = kcal_p25, ymax = kcal_p75),
              alpha = 0.2, fill = "darkred") +
  geom_line(color = "darkred", linewidth = 1) +
  scale_x_continuous(breaks = unique(calorias_tiempo$year)) +
  xlab("") +
  ylab("Calorías per cápita (promedio trimestral)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dir.create("figs/graphs", recursive = TRUE, showWarnings = FALSE)
ggsave("figs/graphs/calorias_tiempo.png", plot = p_calorias, width = 10, height = 6, dpi = 300)


base %>%
  group_by(year) %>%
  summarise(
    n_hogares = n(),
    n_peso_na = sum(is.na(peso)),
    n_kcal_na = sum(is.na(kcal_per_capita))
  ) %>%
  print()


  # ------------------------------------------------------------------------------
  base_raw <- read_csv("data/processed/base_completa.csv", show_col_types = FALSE)
table(base_raw$year)


base_raw %>%
  group_by(year) %>%
  summarise(
    n = n(),
    kcal_pos = sum(kcal_per_capita > 0, na.rm = TRUE),
    tasa_ok  = sum(tasa_homicidios >= 0, na.rm = TRUE),
    integ_ok = sum(tot_integ > 0, na.rm = TRUE)
  )

# =============================================
# Ver los modelos 
# =============================================
# --- VER TABLA LOG-LOG EN CONSOLA ---
modelsummary(
  list(m1, m2, m3, m4, m5, m6, m7),
  output   = "markdown",  
  coef_map = coef_map_loglog,
  add_rows = fe_rows_loglog,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit = "AIC|BIC|Log|RMSE",
  title    = "Resultados Log-Log (Consola)"
)

# --- VER TABLA LOG-NIVEL EN CONSOLA ---
modelsummary(
  list(n1, n2, n3, n4, n5, n6),
  output   = "markdown", 
  coef_map = coef_map_lognivel,
  add_rows = fe_rows_lognivel,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit = "AIC|BIC|Log|RMSE",
  title    = "Resultados Log-Nivel (Consola)"
)



# --- VER TABLA MM EN CONSOLA ---
modelsummary(
  modelos_mm,
  output   = "markdown",
  coef_map = coef_map_mm,
  add_rows = fe_rows_mm,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_omit = "AIC|BIC|Log|RMSE",
  title    = "Especificaciones con efectos fijos (Consola)"
)
