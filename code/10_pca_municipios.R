# ==============================================================================
# 10_pca_municipios.R
# PCA de patrones de gasto alimentario por municipio — ENIGH 2024

# Esta base sale de un proyecto en el que trabajamos en economía para analizar consumo de calorías a lo largo de diferentes municipios. 

# Grupos SMAE/ENIGH:
#   cereales    = 011
#   frutas      = 081, 082
#   verduras    = 083
#   leguminosas = 091-096
#   proteina    = 031-032, 041-046, 051-056, 098   (carne + mar + huevo)
#   lacteos     = 061-064
#   grasas      = 071-074
#   azucar      = 101-105
#   preparados  = 111, 122, 131-133, 139, 171-174
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)

dir.create("output/pca", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 0. MAPEO DE PREFIJOS ENIGH → GRUPO SMAE
# ==============================================================================

grupo_map <- tribble(
  ~pref3, ~grupo,
  # Cereales y derivados (tortilla, pan, pasta, arroz, etc.)
  "011", "cereales",
  # Frutas
  "081", "frutas",
  "082", "frutas",
  # Verduras y hortalizas
  "083", "verduras",
  # Leguminosas y semillas
  "091", "leguminosas",
  "092", "leguminosas",
  "093", "leguminosas",
  "094", "leguminosas",
  "095", "leguminosas",
  "096", "leguminosas",
  # Proteína animal: carnes, aves, mariscos, huevo
  "031", "proteina",
  "032", "proteina",
  "041", "proteina",
  "043", "proteina",
  "044", "proteina",
  "045", "proteina",
  "046", "proteina",
  "051", "proteina",
  "052", "proteina",
  "053", "proteina",
  "054", "proteina",
  "055", "proteina",
  "056", "proteina",
  "098", "proteina",
  # Lácteos
  "061", "lacteos",
  "062", "lacteos",
  "063", "lacteos",
  "064", "lacteos",
  # Aceites y grasas
  "071", "grasas",
  "072", "grasas",
  "073", "grasas",
  "074", "grasas",
  # Azúcares, dulces y postres
  "101", "azucar",
  "102", "azucar",
  "103", "azucar",
  "104", "azucar",
  "105", "azucar",
  # Alimentos preparados y comida fuera del hogar
  "111", "preparados",
  "122", "preparados",
  "131", "preparados",
  "132", "preparados",
  "133", "preparados",
  "139", "preparados",
  "171", "preparados",
  "172", "preparados",
  "173", "preparados",
  "174", "preparados"
)

# ==============================================================================
# 1.  PROCESAR DATOS
# hay que usar gastoshogar 2024 (solo un año, quedarnos con las variables del gasto en alimento que se encuentran en G1)
# Luego usar los datos de municipio del hogar que están en concentradohogar 2024, 
# ==============================================================================

cat("[1] Cargando gastoshogar2024...\n")

gh <- read_csv("data/raw/basesENIGH/gastoshogar2024.csv",
               locale    = locale(encoding = "latin1"),
               col_types = cols(
                 folioviv  = col_character(),
                 foliohog  = col_character(),
                 clave     = col_character(),
                 tipo_gasto = col_character(),
                 gasto_tri = col_double(),
                 factor    = col_double(),
                 entidad   = col_double(),
                 upm       = col_character(),
                 .default  = col_skip()
               )) %>%
  filter(tipo_gasto == "G1", !is.na(gasto_tri), gasto_tri > 0)





conc_raw <- read_csv("data/raw/basesConcentrados/concentradohogar2024.csv",
                     locale = locale(encoding = "latin1"),
                     show_col_types = FALSE, n_max = 0)
factor_col <- if ("factor_hog" %in% names(conc_raw)) "factor_hog" else "factor"

conc_spec <- cols(folioviv = col_character(), foliohog = col_character(),
                  ubica_geo = col_character(), .default = col_skip())
conc_spec$cols[[factor_col]] <- col_double()

conc <- read_csv("data/raw/basesConcentrados/concentradohogar2024.csv",
                 locale = locale(encoding = "latin1"),
                 col_types = conc_spec) %>%
  { if ("factor_hog" %in% names(.)) rename(., factor_hog2 = factor_hog) else . } %>%
  mutate(entidad   = as.integer(substr(ubica_geo, 1, 2)),
         municipio = as.integer(substr(ubica_geo, 3, 5))) %>%
  select(folioviv, foliohog, entidad, municipio, factor_conc = any_of(c("factor_hog2", "factor")))

if (!"factor_conc" %in% names(conc)) {
  conc <- conc %>% rename(factor_conc = last_col())
}


# ==============================================================================
# 2. ASIGNAR GRUPO, CALCULAR SHARES POR HOGAR
# ==============================================================================


gh <- gh %>%
  mutate(pref3 = substr(formatC(as.integer(clave), width = 6, flag = "0"), 1, 3)) %>%
  left_join(grupo_map, by = "pref3")

# gasto_tri total G1 y por grupo, por hogar
gasto_por_hogar <- gh %>%
  group_by(folioviv, foliohog) %>%
  mutate(gasto_total_g1 = sum(gasto_tri)) %>%
  ungroup() %>%
  filter(!is.na(grupo)) %>%  # solo los grupos definidos
  group_by(folioviv, foliohog, grupo, gasto_total_g1) %>%
  summarise(gasto_grupo = sum(gasto_tri), .groups = "drop") %>%
  mutate(share = gasto_grupo / gasto_total_g1)

n_hogs_sin_grupo <- n_distinct(paste(gh$folioviv, gh$foliohog)) -
  n_distinct(paste(gasto_por_hogar$folioviv, gasto_por_hogar$foliohog))
cat("  Hogares con al menos un gasto en grupos definidos:",
    n_distinct(paste(gasto_por_hogar$folioviv, gasto_por_hogar$foliohog)), "\n")

# Pivotear a wide: hogar × grupo
shares_wide <- gasto_por_hogar %>%
  select(folioviv, foliohog, grupo, share, gasto_total_g1) %>%
  pivot_wider(names_from = grupo, values_from = share, values_fill = 0) %>%
  left_join(conc, by = c("folioviv", "foliohog"))



# ==============================================================================
# 3. AGREGAR A NIVEL MUNICIPIO (media ponderada por factor)
# ==============================================================================

# Agregando a nivel municipio

grupos_cols <- grupos_validos

muni_shares <- shares_wide %>%
  filter(!is.na(entidad), !is.na(municipio), !is.na(factor_conc)) %>%
  group_by(entidad, municipio) %>%
  summarise(
    across(all_of(grupos_cols),
           ~ weighted.mean(.x, w = factor_conc, na.rm = TRUE)),
    n_hogares    = n(),
    gasto_median = median(gasto_total_g1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_hogares >= 3)  # al menos 3 hogares por municipio



# ==============================================================================
# 4. PCA
# ==============================================================================

cat("Ejecutando PCA...\n")

mat_pca <- muni_shares %>% select(all_of(grupos_cols)) %>% as.matrix()
rownames(mat_pca) <- paste(muni_shares$entidad, muni_shares$municipio, sep = "_")

# escalar (correlación, no covarianza) porque las shares están en distintas magnitudes
pca_fit <- prcomp(mat_pca, center = TRUE, scale. = TRUE)

# varianza explicada
var_exp <- summary(pca_fit)$importance
cat("Varianza explicada por componente:\n")
print(round(var_exp[, 1:min(9, ncol(var_exp))], 3))

# ==============================================================================
# 5. RESULTADOS
# ==============================================================================

# Tabla de cargas (loadings) para PC1-PC4
loadings_df <- as.data.frame(pca_fit$rotation[, 1:min(6, ncol(pca_fit$rotation))]) %>%
  rownames_to_column("Grupo") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# Scores de municipios (coordenadas en el espacio PCA)
scores_df <- as.data.frame(pca_fit$x[, 1:min(6, ncol(pca_fit$x))]) %>%
  rownames_to_column("municipio_id") %>%
  separate(municipio_id, into = c("entidad", "municipio"), sep = "_", convert = TRUE) %>%
  left_join(muni_shares %>% select(entidad, municipio, n_hogares, gasto_median),
            by = c("entidad", "municipio"))

write_csv(scores_df, "output/pca/scores_municipios.csv")

# ==============================================================================
# 6. GRÁFICAS
# ==============================================================================



# ── Cuánto aporta cada componente ──────────────────────────────────────────────────────────────
var_df <- tibble(
  PC    = paste0("PC", seq_along(pca_fit$sdev)),
  var_prop  = pca_fit$sdev^2 / sum(pca_fit$sdev^2),
  var_acum  = cumsum(pca_fit$sdev^2 / sum(pca_fit$sdev^2))
)

p_scree <- ggplot(var_df, aes(x = PC)) +
  geom_col(aes(y = var_prop), fill = "steelblue", alpha = 0.8) +
  geom_line(aes(y = var_acum, group = 1), color = "darkred", linewidth = 1) +
  geom_point(aes(y = var_acum), color = "darkred", size = 2) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.02)) +
  labs(title = "Scree plot — PCA de patrones alimentarios municipales (ENIGH 2024)",
       x = NULL, y = "Varianza explicada") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/pca/scree.png", p_scree, width = 8, height = 5, dpi = 150)

# ── B. Biplot PC1 vs PC2 ───────────────────────────────────────────────────────
# escalar flechas para que quepan en el mismo plano que los scores
escala <- max(abs(scores_df[, c("PC1","PC2")])) /
  max(abs(pca_fit$rotation[, c("PC1","PC2")])) * 0.45

arrows_df <- as.data.frame(pca_fit$rotation[, c("PC1","PC2")]) %>%
  rownames_to_column("grupo") %>%
  mutate(PC1_s = PC1 * escala, PC2_s = PC2 * escala)

pct1 <- round(var_df$var_prop[1] * 100, 1)
pct2 <- round(var_df$var_prop[2] * 100, 1)

p_biplot <- ggplot() +
  # puntos de municipios (tamaño proporcional a n_hogares)
  geom_point(data = scores_df,
             aes(x = PC1, y = PC2, size = n_hogares),
             alpha = 0.4, color = "steelblue") +
  # flechas de grupos
  geom_segment(data = arrows_df,
               aes(x = 0, y = 0, xend = PC1_s, yend = PC2_s),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               color = "darkred", linewidth = 0.8) +
  geom_label(data = arrows_df,
             aes(x = PC1_s * 1.12, y = PC2_s * 1.12, label = grupo),
             size = 3.2, color = "darkred",
             label.padding = unit(0.15, "lines"), label.size = 0) +
  scale_size_continuous(name = "Hogares", range = c(1, 6)) +
  labs(title = "Biplot PC1 vs PC2 — Patrones de gasto alimentario por municipio (ENIGH 2024)",
       x = paste0("PC1 (", pct1, "%)"),
       y = paste0("PC2 (", pct2, "%)")) +
  theme_minimal(base_size = 12)

ggsave("output/pca/biplot_pc1_pc2.png", p_biplot, width = 10, height = 8, dpi = 150)



# ── C. Top/últimos municipios en PC1 ──────────────────────────────────────────
top_bottom <- bind_rows(
  scores_df %>% arrange(desc(PC1)) %>% slice_head(n = 10) %>% mutate(extremo = "PC1 alto"),
  scores_df %>% arrange(PC1)       %>% slice_head(n = 10) %>% mutate(extremo = "PC1 bajo")
)

p_top <- ggplot(top_bottom,
               aes(x = reorder(paste(entidad, municipio, sep = "-"), PC1),
                   y = PC1, fill = extremo)) +
  geom_col() +
  scale_fill_manual(values = c("PC1 alto" = "#d6604d", "PC1 bajo" = "#2166ac"),
                    name = NULL) +
  coord_flip() +
  labs(title = "Municipios en los extremos del PC1",
       x = "Entidad-Municipio", y = "Score PC1") +
  theme_minimal(base_size = 11)

ggsave("output/pca/top_bottom_pc1.png", p_top, width = 9, height = 6, dpi = 150)

