library(tidyverse)
library(knitr)
library(kableExtra)
library(dplyr)

# ── Load data ──────────────────────────────────────────────────────────────────
catalogoFull <- read_csv("data/processed/catalogoCruzadoENIGH.csv")

# ── Helper: count elements in smae_id ─────────────────────────────────────────
count_smae <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(0L)
  length(str_split(x, ",")[[1]])
}

# ── Prepare base ───────────────────────────────────────────────────────────────
catalogoFull <- catalogoFull %>%
  mutate(
    has_smae   = !is.na(smae_id) & str_trim(smae_id) != "",
    n_smae_ids = map_int(smae_id, count_smae)
  )

# One row per 2024 key — used as base for all ENIGH counts
unique_keys <- catalogoFull %>% distinct(clave, .keep_all = TRUE)

# Pivot by claveAntes: group 2024 keys under their previous equivalent
by_group <- catalogoFull %>%
  mutate(group = if_else(!is.na(claveAntes) & claveAntes != "", claveAntes, clave)) %>%
  group_by(group) %>%
  summarise(
    prev_key      = first(claveAntes),
    concept       = first(concepto),
    n_keys_2024   = n_distinct(clave),
    n_smae_total  = sum(n_smae_ids),
    source        = if_else(any(has_smae), "SMAE", "Kantar"),
    .groups = "drop"
  )

# ── Compute statistics ─────────────────────────────────────────────────────────

# ENIGH coverage
n_2024_total    <- nrow(unique_keys)
n_prev_unique   <- n_distinct(unique_keys$claveAntes[!is.na(unique_keys$claveAntes) & unique_keys$claveAntes != ""])
n_with_match    <- unique_keys %>% filter(!is.na(claveAntes) & claveAntes != "") %>% nrow()
n_no_match      <- unique_keys %>% filter(is.na(claveAntes)  | claveAntes == "") %>% nrow()
pct_no_match    <- round(n_no_match / n_2024_total * 100, 1)

# Expanded categories
expanded        <- by_group %>% filter(!is.na(prev_key) & prev_key != "", n_keys_2024 > 1)
n_expanded      <- nrow(expanded)
pct_expanded    <- round(n_expanded / n_prev_unique * 100, 1)

# Nutritional sources
n_groups_total  <- nrow(by_group)
n_smae_groups   <- sum(by_group$source == "SMAE")
n_kantar_groups <- sum(by_group$source == "Kantar")
pct_smae        <- round(n_smae_groups   / n_groups_total * 100, 1)
pct_kantar      <- round(n_kantar_groups / n_groups_total * 100, 1)
n_smae_ids_all  <- sum(by_group$n_smae_total)

# SMAE IDs distribution
smae_dist <- by_group %>%
  filter(source == "SMAE") %>%
  summarise(
    median = median(n_smae_total),
    mean   = round(mean(n_smae_total), 1),
    max    = max(n_smae_total),
    p90    = round(quantile(n_smae_total, 0.9), 0)
  )

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 1 · Summary statistics
# ══════════════════════════════════════════════════════════════════════════════
summary_tbl <- tibble(
  Indicator = c(
    # ENIGH coverage (2 rows)
    "Unique categories ENIGH 2024",
    "Unique categories ENIGH previous editions",
    # No match (1 row)
    "Categories without previous equivalent (%)",
    # Expansions (2 rows)
    "Previous categories expanded in 2024",
    "Expanded categories as share of previous edition (%)",
    # Sources (5 rows)
    "Category groups covered by SMAE",
    "Share covered by SMAE (%)",
    "Category groups covered by Kantar",
    "Share covered by Kantar (%)",
    "Total SMAE IDs assigned",
    # Distribution (4 rows)
    "Median SMAE IDs per category",
    "Mean SMAE IDs per category",
    "Maximum SMAE IDs (broadest category)",
    "90th percentile of SMAE IDs"
  ),
  Value = c(
    n_2024_total, n_prev_unique,
    pct_no_match,
    n_expanded, pct_expanded,
    n_smae_groups, pct_smae,
    n_kantar_groups, pct_kantar,
    n_smae_ids_all,
    smae_dist$median, smae_dist$mean, smae_dist$max, smae_dist$p90
  ),
  Note = c(
    "Unit of analysis in ENIGH 2024",
    "Keys from the 2022 catalog present in the crosswalk",
    paste0(n_no_match, " of ", n_2024_total, " keys have no prior equivalent"),
    "One previous key maps to 2+ keys in 2024",
    paste0(n_expanded, " of ", n_prev_unique, " previous categories"),
    "At least one SMAE ID assigned",
    paste0(n_smae_groups, " / ", n_groups_total, " groups"),
    "No SMAE match: Kantar database will be used",
    paste0(n_kantar_groups, " / ", n_groups_total, " groups"),
    "Sum of all SMAE IDs across the catalog",
    "SMAE-covered categories only",
    "SMAE-covered categories only",
    "SMAE-covered categories only",
    "90% of categories have this number of IDs or fewer"
  )
)

table1_html <- summary_tbl %>%
  kbl(
    format  = "html",
    caption = "Table 1. Descriptive statistics — ENIGH crosswalk catalog",
    align   = c("l", "r", "l"),
    escape  = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = FALSE,
    font_size         = 13,
    html_font         = "\"Segoe UI\", Arial, sans-serif"
  ) %>%
  pack_rows("ENIGH Coverage",              1,  2, bold = TRUE, color = "#1E3A5F", background = "#EEF2FF") %>%
  pack_rows("New Categories",              3,  3, bold = TRUE, color = "#7B0000", background = "#FFF0F0") %>%
  pack_rows("Category Expansions",         4,  5, bold = TRUE, color = "#7A4000", background = "#FFF8EE") %>%
  pack_rows("Nutritional Sources",         6, 10, bold = TRUE, color = "#0A4D1F", background = "#F0FFF4") %>%
  pack_rows("SMAE IDs Distribution",      11, 14, bold = TRUE, color = "#2C006B", background = "#F8F0FF") %>%
  row_spec(1:2,   color = "#1E3A5F", bold = TRUE) %>%
  row_spec(3,     color = "#C0392B", bold = TRUE) %>%
  row_spec(4:5,   color = "#E67E22", bold = TRUE) %>%
  row_spec(6:10,  color = "#1A6B3C", bold = TRUE) %>%
  row_spec(11:14, color = "#6C3483", bold = TRUE) %>%
  column_spec(1, width = "24em") %>%
  column_spec(2, width = "5em",  bold = TRUE, border_left = TRUE, border_right = TRUE) %>%
  column_spec(3, width = "22em", color = "grey45", italic = TRUE) %>%
  footnote(
    general           = "Reference edition for previous keys: ENIGH 2022. Source: INEGI / SMAE 5th ed.",
    general_title     = "Note: ",
    footnote_as_chunk = TRUE
  )

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 2 · Expanded categories detail
# ══════════════════════════════════════════════════════════════════════════════
table2_html <- expanded %>%
  arrange(desc(n_keys_2024)) %>%
  select(
    `Previous key`    = prev_key,
    `Concept (ref.)`  = concept,
    `2024 subcat.`    = n_keys_2024,
    `SMAE IDs`        = n_smae_total,
    `Source`          = source
  ) %>%
  kbl(
    format  = "html",
    caption = "Table 2. Previous categories expanded in ENIGH 2024",
    align   = c("c", "l", "c", "c", "c"),
    escape  = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = FALSE,
    font_size         = 13,
    html_font         = "\"Segoe UI\", Arial, sans-serif"
  ) %>%
  column_spec(1, bold = TRUE, color = "#1E3A5F", width = "8em") %>%
  column_spec(2, width = "22em") %>%
  column_spec(3, bold = TRUE, color = "#E67E22", width = "7em") %>%
  column_spec(4, bold = TRUE, color = "#1A6B3C", width = "7em") %>%
  column_spec(5, width = "6em")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE 3 · Top 20 categories by SMAE IDs
# ══════════════════════════════════════════════════════════════════════════════
table3_html <- by_group %>%
  filter(source == "SMAE") %>%
  arrange(desc(n_smae_total)) %>%
  slice_head(n = 20) %>%
  select(
    `Group`       = group,
    `Concept`     = concept,
    `2024 subcat.`= n_keys_2024,
    `SMAE IDs`    = n_smae_total
  ) %>%
  kbl(
    format  = "html",
    caption = "Table 3. Top 20 categories by number of SMAE IDs assigned",
    align   = c("c", "l", "c", "c"),
    escape  = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = FALSE,
    font_size         = 13,
    html_font         = "\"Segoe UI\", Arial, sans-serif"
  ) %>%
  column_spec(1, bold = TRUE, color = "#1E3A5F", width = "7em") %>%
  column_spec(2, width = "25em") %>%
  column_spec(3, width = "7em") %>%
  column_spec(4, bold = TRUE, color = "#1A6B3C", width = "6em") %>%
  row_spec(1, background = "#FFFDE7", bold = TRUE)

# ── TABLE 4 · Categories covered by Kantar ────────────────────────────────────
table4_html <- by_group %>%
  filter(source == "Kantar") %>%
  arrange(group) %>%
  select(
    `Group`        = group,
    `Concept`      = concept,
    `2024 subcat.` = n_keys_2024,
    `Prev. key`    = prev_key
  ) %>%
  mutate(`Prev. key` = if_else(is.na(`Prev. key`) | `Prev. key` == "", "— new —", `Prev. key`)) %>%
  kbl(
    format  = "html",
    caption = paste0("Table 4. Categories to be covered by Kantar (n = ", n_kantar_groups, ")"),
    align   = c("c", "l", "c", "c"),
    escape  = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "bordered"),
    full_width        = FALSE,
    font_size         = 13,
    html_font         = "\"Segoe UI\", Arial, sans-serif"
  ) %>%
  column_spec(1, bold = TRUE, color = "#1E3A5F", width = "7em") %>%
  column_spec(2, width = "25em") %>%
  column_spec(3, width = "7em") %>%
  column_spec(4, color = "#C0392B", width = "7em")


# ── Export HTML ────────────────────────────────────────────────────────────────
dir.create("outputs", showWarnings = FALSE)

full_html <- paste0(
  "<!DOCTYPE html><html lang='en'><head>",
  "<meta charset='UTF-8'>",
  "<title>ENIGH Catalog Statistics</title>",
  "<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css'>",
  "<style>
    body  { font-family: 'Segoe UI', Arial, sans-serif; padding: 40px; background: #F9FAFB; }
    h1    { font-size: 1.5rem; font-weight: 700; color: #1E3A5F; margin-bottom: 4px; }
    p.sub { font-size: 0.9rem; color: #6B7280; margin-bottom: 32px; }
    .block { background: white; border-radius: 10px; padding: 28px;
             box-shadow: 0 1px 6px rgba(0,0,0,0.08); margin-bottom: 40px; }
  </style></head><body>",
  "<h1>Descriptive Statistics &mdash; ENIGH Crosswalk Catalog</h1>",
  "<p class='sub'>Pivot: claveAntes &nbsp;&middot;&nbsp; Sources: SMAE and Kantar &nbsp;&middot;&nbsp; ENIGH 2024 vs 2022</p>",
  "<div class='block'>", as.character(table1_html), "</div>",
  "<div class='block'>", as.character(table2_html), "</div>",
  "<div class='block'>", as.character(table3_html), "</div>",
  "<div class='block'>", as.character(table4_html), "</div>",
  "</body></html>"
)

writeLines(full_html, "outputs/tables_descriptive.html")
message("Done: outputs/tables_descriptive.html")

# ── Chart ──────────────────────────────────────────────────────────────────────
plot_data <- by_group %>%
  arrange(desc(n_smae_total)) %>%
  mutate(
    axis_label = if_else(
      !is.na(prev_key) & prev_key != "",
      prev_key,
      str_sub(concept, 1, 10)
    ),
    axis_label = fct_inorder(axis_label)
  )

p <- ggplot(plot_data, aes(x = axis_label, y = n_smae_total, fill = source)) +
  geom_col(width = 0.75) +
  scale_fill_manual(
    values = c("SMAE" = "#4ECDC4", "Kantar" = "#FF6B6B"),
    name   = "Nutritional source"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08)),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  labs(
    title    = "SMAE IDs assigned per ENIGH category",
    subtitle = paste0(
      n_smae_groups, " categories covered by SMAE (", pct_smae, "%)  \u00b7  ",
      n_kantar_groups, " by Kantar (", pct_kantar, "%)"
    ),
    x       = NULL,
    y       = "Number of SMAE IDs assigned",
    caption = "Source: INEGI ENIGH 2024 \u00b7 SMAE 5th ed. \u00b7 Own elaboration"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "grey40", size = 9.5, margin = margin(b = 12)),
    plot.caption       = element_text(color = "grey55", size = 8, margin = margin(t = 10)),
    plot.background    = element_rect(fill = "#FAFAFA", color = NA),
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", size = 0.4),
    axis.text.x        = element_text(angle = 50, hjust = 1, vjust = 1,
                                      size = 6.5, color = "grey30"),
    axis.text.y        = element_text(size = 9, color = "grey30"),
    legend.position    = "top",
    legend.title       = element_text(size = 9, face = "bold"),
    legend.text        = element_text(size = 9),
    plot.margin        = margin(t = 16, r = 20, b = 10, l = 10)
  )

plot_width <- max(1400, nrow(plot_data) * 14)
ggsave("outputs/chart_smae.png", plot = p,
       width = plot_width / 100, height = 7, dpi = 150, bg = "#FAFAFA")

message("Done: outputs/chart_smae.png")
