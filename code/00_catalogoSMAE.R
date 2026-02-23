# Cargar librerías necesarias
library(tidyverse)  # Para manipulación de datos
library(haven)      # Para leer archivos .dta si es STATA
library(readr)      # Para leer CSV
library(readxl)

# -------------------------------------------------
# Leer datos y generar claves únicas por categorías
# -------------------------------------------------


# Cereales 

cereales <- read_xlsx("data/raw/basesSMAE/smae_cereales.xlsx") %>%
  mutate(clave = as.integer(paste0("1", str_pad(row_number(), 3, pad = "0"))))

# ------------------------------------------------------------

# Alimentos Origen animal
alimentos_bajo <- read_csv("data/raw/basesSMAE/smae_alimentos_animales_bajo.csv") 
  
alimentos_alto <- read_csv("data/raw/basesSMAE/smae_alimentos_animalesModAlto.csv") 
  

# Ambas bases de origen animal
alimentosAnimal <- bind_rows(alimentos_bajo, alimentos_alto) %>%
  mutate(clave = as.integer(paste0("2", str_pad(row_number(), 3, pad = "0"))))

# ---------------------------------------------------------------


# Leche
leche <- read_csv("data/raw/basesSMAE/smae_leche.csv") %>%
  mutate(clave = as.integer(paste0("3", str_pad(row_number(), 3, pad = "0"))))

  # ---------------------------------------------------------------


# Frutas
frutas <- read_csv("data/raw/basesSMAE/smae_frutas.csv") %>%
  mutate(clave = as.integer(paste0("4", str_pad(row_number(), 3, pad = "0"))))



# Legumbres
leguminosas <- read_csv("data/raw/basesSMAE/smae_leguminosas.csv") %>%
  mutate(clave = as.integer(paste0("5", str_pad(row_number(), 3, pad = "0"))))

  # ---------------------------------------------------------------

# Verduras
verduras <- read_csv("data/raw/basesSMAE/smae_verduras.csv") %>%
  mutate(clave = as.integer(paste0("6", str_pad(row_number(), 3, pad = "0"))))



  # ---------------------------------------------------------------

# Alcohol
alcohol <- read_csv("data/raw/basesSMAE/smae_alcohol.csv") %>%
  mutate(clave = as.integer(paste0("7", str_pad(row_number(), 3, pad = "0"))))


# grasas
grasas <- read_csv("data/raw/basesSMAE/smae_grasas.csv") %>%
  mutate(clave = as.integer(paste0("8", str_pad(row_number(), 3, pad = "0"))))

# platillos
platillos <- read_csv("data/raw/basesSMAE/smae_platillos.csv") %>%
  mutate(clave = as.integer(paste0("8", str_pad(row_number(), 3, pad = "0")))) %>%
  mutate(clave = clave + 300) %>%
  mutate(alimento = generico)

# eliminar verdura, fruta, cereal_sin_grasa, cereal_con_grasa, leguminosa, muy_bajo_aporte_grasa, moderado_aporte_grasa, alto_aporte_grasa,
# leche_descremada, leche semidescremada, leche_entera, leche_con_azucar, grasa_sin_proteina, grasa_con_proteina,
# etanol, alimentos_libres_energia, 
platillos <- platillos %>%
  select(-c(verdura, fruta, cereal_sin_grasa, cereal_con_grasa, leguminosa, muy_bajo_aporte_grasa, moderado_aporte_grasa, alto_aporte_grasa,
            leche_descremada, leche_semidescremada, leche_entera, leche_con_azucar, grasa_sin_proteina, grasa_con_proteina,
            etanol, alimentos_libres_energia, azucar, azucar_con_grasa, bajo_aporte_grasa))


# Azúzar
azucar <- read_csv("data/raw/basesSMAE/smae_azucar.csv") %>%
  mutate(clave = as.integer(paste0("9", str_pad(row_number(), 3, pad = "0"))))

# Libres
libres <- read_csv("data/raw/basesSMAE/smae_libres.csv") %>%
  mutate(clave = as.integer(paste0("9", str_pad(row_number(), 3, pad = "0")))) %>%
  mutate(clave = clave + 500)

# fastFood
fastFood <- read_csv("data/raw/basesSMAE/smae_fastFood.csv") %>%
  mutate(clave = as.integer(paste0("9", str_pad(row_number(), 3, pad = "0")))) %>%
  mutate(clave = clave + 800) %>%
  mutate(alimento = producto)




# ver el máximo
platillos %>%
  summarise(max_clave = min(clave))


  str(platillos)
str(grasas)







# ----------------------------------------
# Corrigiendo formatos de variables ------
# ----------------------------------------

##### Convertir fracciones mixtas a decimales


convertir_fraccion <- function(x) {
  x <- trimws(x)
  sapply(x, function(val) {
    if (grepl("^\\d+/\\d+$", val)) {
      p <- as.numeric(strsplit(val, "/")[[1]])
      p[1] / p[2]
    } else if (grepl("^\\d+ \\d+/\\d+$", val)) {
      partes <- strsplit(val, " ")[[1]]
      entero <- as.numeric(partes[1])
      frac <- as.numeric(strsplit(partes[2], "/")[[1]])
      entero + frac[1] / frac[2]
    } else {
      as.numeric(val)
    }
  }, USE.NAMES = FALSE)
}

leche <- leche %>% mutate(cantidad_sugerida = convertir_fraccion(cantidad_sugerida))
alimentosAnimal <- alimentosAnimal %>% mutate(cantidad_sugerida = convertir_fraccion(cantidad_sugerida))
azucar <- azucar %>% mutate(cantidad_sugerida = convertir_fraccion(cantidad_sugerida))
platillos <- platillos %>% mutate(cantidad_sugerida = convertir_fraccion(cantidad))

alimentosAnimal <- alimentosAnimal %>%
  mutate(across(c(colesterol_mg, vitamina_a_ug_re, calcio_mg, hierro_mg, sodio_mg, selenio_ug), 
                ~ as.numeric(.x)))




leche <- leche %>%
  mutate(across(c(colesterol_mg, vitamina_a_ug_re, calcio_mg, sodio_mg, , azucar_por_equivalente_g), 
                ~ as.numeric(.x)))


frutas <- frutas %>%
  mutate(across(c( vitamina_a_ug_re, fibra_g, azucar_por_equivalente_g,acido_ascorbico_mg, acido_folico_ug, potasio_mg, indice_glucemico, carga_glucemica), 
                ~ as.numeric(.x)))


leguminosas <- leguminosas %>%
  mutate(across(c( fibra_g, hierro_no_hem_mg, selenio_ug, sodio_mg, fosforo_mg, azucar_por_equivalente_g, potasio_mg, 
                  indice_glicemico, carga_glicemica), 
                ~ as.numeric(.x)))


verduras <- verduras %>%
  mutate(across(c( fibra_g, vitamina_a_ug_re, acido_ascorbico_mg, acido_folico_ug, hierro_no_hem_mg, potasio_mg, 
                  indice_glucemico, carga_glucemica), 
                ~ as.numeric(.x)))

str(grasas)
grasas <- grasas %>%
  mutate(across(c( ag_saturados_g, ag_monoinsaturados_g, ag_poliinsaturados_g, colesterol_mg), 
                ~ as.numeric(.x)))

str(azucar)
azucar <- azucar %>%
  mutate(across(c( lipidos_g, sodio_mg, azucar_por_equivalente_g, indice_glucemico, carga_glucemica), 
                ~ as.numeric(.x)))



# ----------------------------------------------------------------
### Uniendo las bases ###

smae <- bind_rows(
  cereales,
  alimentosAnimal,
  leche,
  frutas,
  leguminosas,
  verduras,
  alcohol,
  grasas,
  platillos,
  azucar,
  libres,
  fastFood
)



# -----------------------------------------
# Catálogo ENIGH 2024 ---------------------
# -----------------------------------------

catalogo2024 <- read_csv("data/raw/catalogo_enigh_2024.csv") 


glimpse(catalogo2024)



# Gurdar SMAE clave y concepto 

smaeClave <- smae %>%
  select(clave, alimento)

write_csv(smaeClave, "data/processed/smae_clave_concepto.csv")


str(fastFood)


smaeClave %>%
  filter(clave > 9800)


catalogo2024 %>%
  filter(clave == "011139")

glimpse(smae)

#########################3
## ENIGH ## esto alch lo voy a quitar fue solo para explorar
##########################


enigh24 <- read_csv("data/raw/gastoshogar2024.csv")
glimpse(enigh24)


enigh24 %>% 
  filter(clave == "011411") %>% 
  summarise(
    cantidad_min = min(cantidad, na.rm = TRUE),
    cantidad_median = median(cantidad, na.rm = TRUE),
    cantidad_max = max(cantidad, na.rm = TRUE),
    gasto_median = median(gasto, na.rm = TRUE),
    n = n()
  )



enigh24 %>% 
  filter(clave == "011231") %>%  # jamón
  summarise(
    cantidad_min = min(cantidad, na.rm = TRUE),
    cantidad_median = median(cantidad, na.rm = TRUE),
    cantidad_max = max(cantidad, na.rm = TRUE),
    gasto_median = median(gasto, na.rm = TRUE),
    precio_unitario_median = median(gasto/cantidad, na.rm = TRUE),
    n = n()
  )





########################################################################
# Guardar la base del SMAE en procesados 


write_csv(smae, "data/processed/smae.csv")


##### Guardar también la base para cruzar con enigh
write_csv(catalogo_enigh_2024)