########
# NOtas

# catálogo cruzado trae para cada cateogría de ENIGH los id's de SMAE



# LLibrerías
library(tidyverse)
library(haven)
library(readr)
library(readxl)


# Leer datos
smae <- read_csv("data/processed/smae.csv")

catalogo <- read_csv("data/processed/catalogoCruzadoENIGH.csv")


#########################################
# Para cada elemento en el catálogo cruzado, obtener una medida estándar 1 kilo, 100 gramos o 1 L 
# esto es con lo que voy a sacar un promedio por categoría 
# Con eso multiplico por la columna de cantidad de cada gasto

smaeEstandar <- smae %>%
     mutate(kcal_por_100g = (energia_kcal / peso_neto_g) * 100)


####
# Expandir smae_id (están separados por coma en una sola celda)
catalogoExpandido <- catalogo %>%
  separate_rows(smae_id, sep = ",\\s*") %>%
  mutate(smae_id = as.integer(smae_id))

# Join con smae estandarizado
catalogoConKcal <- catalogoExpandido %>%
  left_join(smaeEstandar %>% select(clave_smae = clave, alimento, kcal_por_100g),
            by = c("smae_id" = "clave_smae"))

# Promedio y SD por clave ENIGH
resumenKcal <- catalogoConKcal %>%
  group_by(clave, concepto) %>%
  summarise(
    n_items_smae = sum(!is.na(kcal_por_100g)),
    kcal_media   = mean(kcal_por_100g, na.rm = TRUE),
    kcal_sd      = sd(kcal_por_100g, na.rm = TRUE),
    kcal_se      = kcal_sd / sqrt(n_items_smae),
    kcal_ic_low  = kcal_media - 1.96 * kcal_se,
    kcal_ic_high = kcal_media + 1.96 * kcal_se,
    .groups = "drop"
  )
head(resumenKcal)
glimpse()