



# Leer datos


gastoshogar20 <- read_csv("data/raw/gastoshogar2020.csv")  # Ajusta el nombre del archivo y la ruta según sea necesario

head(gastoshog)  # Ver las primeras filas del dataset para entender su estructura



# Alimentos
alimentos <- read_csv("data/raw/Nutricion_alimentos.csv")  

catalogo2024 <- read_csv("data/raw/catalogo_enigh_2024.csv") 

## Ver qué pasó con las comas

catalogo2024 %>%
  filter(str_detect(concepto, ",")) %>%
  select(concepto) %>%
  head(20)


catalogo2024 %>%
    filter(clave == "011616")
