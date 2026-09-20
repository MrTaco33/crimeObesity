

#----------------------------------------------------------------------------------------------------------

# script que construye una base de datos con la tasa de homicidios por mes para cada municipio 2000-2019

# autor: jorge emilio - julio, 2022

# proyecto: tesis de maestría

#--------------------------------------------------------------------------------------------------------




# paquetes
library(foreign) # Para leer datos dbf
library(tidyverse)
library(lubridate)
library(HistogramTools)
library(ggplot2)
library(haven)
library(stringr)




#-----------------------------------------------------------------------------------------------------------------------------


# Rutas
ruta_defun <- "data/raw/defun"


# abro las bases de defunciones, van de 2000 al 2024
def_2000 <- read.dbf(paste0(ruta_defun, "/DEFUN00.dbf"))
def_2001 <- read.dbf(paste0(ruta_defun, "/DEFUN01.dbf"))
def_2002 <- read.dbf(paste0(ruta_defun, "/DEFUN02.dbf"))
def_2003 <- read.dbf(paste0(ruta_defun, "/DEFUN03.dbf"))
def_2004 <- read.dbf(paste0(ruta_defun, "/DEFUN04.dbf"))
def_2005 <- read.dbf(paste0(ruta_defun, "/DEFUN05.dbf"))
def_2006 <- read.dbf(paste0(ruta_defun, "/DEFUN06.dbf"))
def_2007 <- read.dbf(paste0(ruta_defun, "/DEFUN07.dbf"))
def_2008 <- read.dbf(paste0(ruta_defun, "/DEFUN08.dbf"))
def_2009 <- read.dbf(paste0(ruta_defun, "/DEFUN09.dbf"))
def_2010 <- read.dbf(paste0(ruta_defun, "/DEFUN10.dbf"))
def_2011 <- read.dbf(paste0(ruta_defun, "/DEFUN11.dbf"))
def_2012 <- read.dbf(paste0(ruta_defun, "/DEFUN12.dbf"))
def_2013 <- read.dbf(paste0(ruta_defun, "/DEFUN13.dbf"))
def_2014 <- read.dbf(paste0(ruta_defun, "/DEFUN14.dbf"))
def_2015 <- read.dbf(paste0(ruta_defun, "/DEFUN15.dbf"))
def_2016 <- read.dbf(paste0(ruta_defun, "/DEFUN16.dbf"))
def_2017 <- read.dbf(paste0(ruta_defun, "/DEFUN17.dbf"))
def_2018 <- read.dbf(paste0(ruta_defun, "/DEFUN18.dbf"))
def_2019 <- read.dbf(paste0(ruta_defun, "/DEFUN19.dbf"))
def_2020 <- read.dbf(paste0(ruta_defun, "/defun20.dbf"))
def_2021 <- read.dbf(paste0(ruta_defun, "/defun21.dbf"))
def_2022 <- read.dbf(paste0(ruta_defun, "/DEFUN22.dbf"))
def_2023 <- read.dbf(paste0(ruta_defun, "/DEFUN23.dbf"))
def_2024 <- read.dbf(paste0(ruta_defun, "/DEFUN24.dbf"))
#-----------------------------------------------------------------------------------------------------------------------------



# agrupo todas las bases de defunciones en un lista
lista <- list(def_2000, def_2001, def_2002, def_2003, def_2004, def_2005, def_2006, 
              def_2007, def_2008, def_2009, def_2010, def_2011, def_2012, def_2013, 
              def_2014, def_2015, def_2016, def_2017, def_2018, def_2019, def_2020,
              def_2021, def_2022, def_2023, def_2024)


# Función que limpia bases de datos
limpiar_bases <- function(x){
  
  # filtrar homicidios según la variable disponible
  if ("PRESUNTO" %in% names(x)) {
    x <- x %>% filter(PRESUNTO == 2)
  } else if ("TIPO_DEFUN" %in% names(x)) {
    x <- x %>% filter(TIPO_DEFUN == 2)
  } else {
    stop("No se encontró variable de tipo de defunción")
  }
  
  # seleccionar solo columnas presentes en todos los años
  x <- x %>% select(ENT_OCURR, MUN_OCURR, LISTA_MEX, SEXO, EDAD,
                    DIA_OCURR, MES_OCURR, ANIO_OCUR, NACIONALID)
}

lista <- lapply(lista, limpiar_bases)


# extraemos bases de datos
def_2000 <- lista[[1]]
def_2001 <- lista[[2]]
def_2002 <- lista[[3]]
def_2003 <- lista[[4]]
def_2004 <- lista[[5]]
def_2005 <- lista[[6]]
def_2006 <- lista[[7]]
def_2007 <- lista[[8]]
def_2008 <- lista[[9]]
def_2009 <- lista[[10]]
def_2010 <- lista[[11]]
def_2011 <- lista[[12]]
def_2012 <- lista[[13]]
def_2013 <- lista[[14]]
def_2014 <- lista[[15]]
def_2015 <- lista[[16]]
def_2016 <- lista[[17]]
def_2017 <- lista[[18]]
def_2018 <- lista[[19]]
def_2019 <- lista[[20]]
def_2020 <- lista[[21]]
def_2021 <- lista[[22]]
def_2022 <- lista[[23]]
def_2023 <- lista[[24]]
def_2024 <- lista[[25]]

# pegamos las bases de datos
homicidios <- rbind(def_2000, def_2001, def_2002, def_2003, def_2004, def_2005, def_2006, 
                    def_2007, def_2008, def_2009, def_2010, def_2011, def_2012, def_2013, 
                    def_2014, def_2015, def_2016, def_2017, def_2018, def_2019, def_2020,
                    def_2021, def_2022, def_2023, def_2024)


# limpiamos environment
rm(lista)
rm(def_2000, def_2001, def_2002, def_2003, def_2004, def_2005, def_2006, 
   def_2007, def_2008, def_2009, def_2010, def_2011, def_2012, def_2013, 
   def_2014, def_2015, def_2016, def_2017, def_2018, def_2019, def_2020, def_2021, def_2022, def_2023, def_2024)


# nos quedamos con homicidios desde 2000
homicidios <- homicidios %>% filter(ANIO_OCUR >= 2000)


# renombramos variables
homicidios <- homicidios %>% rename(entidad_hom = ENT_OCURR) %>%
                             rename(municipio_hom = MUN_OCURR) %>%
                             rename(mujer = SEXO) %>%
                             rename(edad = EDAD) %>%
                             rename(dia_hom = DIA_OCURR) %>%
                             rename(mes_hom = MES_OCURR) %>%
                             rename(anio_hom = ANIO_OCUR) %>%
                             rename(extranjero = NACIONALID)


# variables de interés
homicidios <- homicidios %>% select(dia_hom, mes_hom, anio_hom, entidad_hom, 
                                    municipio_hom, edad, mujer, extranjero)


# pasamos a variables numéricas
homicidios <- homicidios %>% mutate(entidad_hom = as.numeric(entidad_hom)) %>%
                             mutate(municipio_hom = as.numeric(municipio_hom))


# tiramos homicidios con fechas no especificadas en la base
homicidios <- homicidios %>% filter(mes_hom != 99 & anio_hom != 9999)

#tiramos homicidios que tengan lugar no especificado en la base de datos
homicidios <- homicidios %>% filter(municipio_hom != 999)
homicidios <- homicidios %>% filter(entidad_hom <= 32)



# calculamos la edad (para saber si son mayores de edad)
homicidios <- homicidios %>% mutate(edad = edad - 4000) 
homicidios <- homicidios %>% mutate(edad = replace(edad, edad <= 0, 0)) # personas de 0 años 
homicidios <- homicidios %>% mutate(edad = replace(edad, edad == 998, NA)) # edades no especificadas

homicidios <- homicidios %>% mutate(mayor_edad = as.numeric(edad >= 18))


# cambiamos valores de dummy de mujeres
homicidios <- homicidios %>% mutate(mujer = replace(mujer, mujer == 1, 0))
homicidios <- homicidios %>% mutate(mujer = replace(mujer, mujer == 2, 1))
homicidios <- homicidios %>% mutate(mujer = replace(mujer, mujer == 9, NA))


# cambiamos valores de dummy de extranjeros
homicidios <- homicidios %>% mutate(extranjero = replace(extranjero, extranjero == 1, 0))
homicidios <- homicidios %>% mutate(extranjero = replace(extranjero, extranjero == 2, 1))
homicidios <- homicidios %>% mutate(extranjero = replace(extranjero, extranjero == 9, NA))



#-----------------------------------------------------------------------------------------------------------------------------



# agrupamos por mes y municipio para obtener el número de homicidios 
# también caluclamos proporciones de mujeres, mayoría de edad y extranjerxs
tasas_homicidios <- homicidios %>% group_by(mes_hom, anio_hom, entidad_hom, municipio_hom) %>%
                                  mutate(homicidios = n()) %>%
                                  mutate(proporcion_mujeres = mean(mujer, na.rm = TRUE)) %>%
                                  mutate(proporcion_mayor_edad = mean(mayor_edad, na.rm = TRUE)) %>%
                                  mutate(proporcion_extranjeros = mean(extranjero, na.rm = TRUE))

homicidios <- homicidios %>%
  mutate(extranjero = as.numeric(as.character(extranjero)))

tasas_homicidios <- homicidios %>% 
  group_by(mes_hom, anio_hom, entidad_hom, municipio_hom) %>%
  summarise(
    homicidios             = n(),
    proporcion_mujeres     = mean(mujer, na.rm = TRUE),
    proporcion_mayor_edad  = mean(mayor_edad, na.rm = TRUE),
    proporcion_extranjeros = mean(extranjero, na.rm = TRUE),
    .groups = "drop"
  )
# duplicates drop
tasas_homicidios <- tasas_homicidios %>% distinct(mes_hom, anio_hom, entidad_hom, municipio_hom, .keep_all = TRUE)

# variables de interés
tasas_homicidios <- tasas_homicidios %>% select(mes_hom, anio_hom, entidad_hom, municipio_hom, 
                                                homicidios, proporcion_mujeres, proporcion_mayor_edad,
                                                proporcion_extranjeros)


hom_anual <- homicidios %>% group_by(anio_hom, mes_hom) %>% mutate(homicidios = n())
hom_anual <- hom_anual %>% distinct(anio_hom, mes_hom, homicidios)
hom_anual <- hom_anual %>% mutate(fecha = make_date(year = anio_hom, month = mes_hom))
ggplot(hom_anual, aes(fecha, homicidios)) + # Ejes
                  geom_line(color = "darkred", linewidth = 1) + # Tipo de línea
                  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Frecuencia de datos
                  ggtitle("") +
                  xlab("") +
                  ylab("Homicides") + 
                  theme(axis.line = element_line(colour = "black")) + 
                  theme_bw() + # Quitar el fondo gris 
                  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
                  ylim(250, 4000) +
                  geom_vline(xintercept = as.Date("2006-12-01"), color = "black", linewidth = 0.5,
                             linetype = "dashed") +
                  geom_vline(xintercept = as.Date("2012-12-01"), color = "black", linewidth = 0.5,
                             linetype = "dashed") +
                  geom_vline(xintercept = as.Date("2018-12-01"), color = "black", linewidth = 0.5,
                             linetype = "dashed") +
                  geom_vline(xintercept = as.Date("2000-12-01"), color = "black", linewidth = 0.5,
                             linetype = "dashed") +
                  annotate(geom = "text", y = 3750, x = as.Date("2010-01-01"), label = "Sexenio de FCH") +
                  annotate(geom = "text", y = 3750, x = as.Date("2016-01-01"), label = "Sexenio de EPN") +
                  annotate(geom = "text", y = 3750, x = as.Date("2020-01-01"), label = "Sexenio de AMLO") +
                  annotate(geom = "text", y = 3750, x = as.Date("2004-01-01"), label = "Sexenio de VFQ") 


ggsave("figs/graphs/homicidiosTiempo.png", 
       width = 10, height = 6, dpi = 300)

# datos por año, estado o municipio
año <- tasas_homicidios %>% group_by(anio_hom) %>% summarise(sum(homicidios))
entidad <- tasas_homicidios %>% group_by(entidad_hom) %>% summarise(sum(homicidios))
municipio <- tasas_homicidios %>% group_by(entidad_hom, municipio_hom) %>% summarise(sum(homicidios))

rm(año, entidad, municipio)



#-----------------------------------------------------------------------------------------------------------------------

# (exploración de columnas — comentado para no releer los archivos en cada corrida)
# iter_2020_01 <- read_csv("data/raw/censos/iter_2020/ITER_01CSV20.csv", locale = locale(encoding = "latin1"))
# names(iter_2020_01)
# iter_2010_01 <- read.dbf("data/raw/censos/iter_2010/ITER_01DBF10.dbf")
# names(iter_2010_01)
# iter_2000_01 <- read.dbf("data/raw/censos/iter_2000/ITER_01DBF00.dbf")
# names(iter_2000_01)







# =======================================================================================================
# ── NUEVO: leer los ITER ──────────────────────────────────────────────────────
limpiar_iter <- function(archivo, tipo = "csv") {
  if (tipo == "csv") {
    x <- read_csv(archivo, locale = locale(encoding = "latin1"), 
                  show_col_types = FALSE)
  } else {
    x <- read.dbf(archivo)
  }
  x %>%
    filter(LOC == "0000" & MUN != "000") %>%
    mutate(geo = paste0(str_pad(ENTIDAD, 2, pad = "0"),
                        str_pad(MUN, 3, pad = "0"))) %>%
    select(geo, tot_pop = POBTOT)
}

archivos_2020 <- list.files("data/raw/censos/iter_2020/", full.names = TRUE)
archivos_2010 <- list.files("data/raw/censos/iter_2010/", full.names = TRUE)
archivos_2000 <- list.files("data/raw/censos/iter_2000/", full.names = TRUE)

censo_2020_raw <- map_dfr(archivos_2020, limpiar_iter, tipo = "csv")
censo_2010_raw <- map_dfr(archivos_2010, limpiar_iter, tipo = "dbf")
censo_2000_raw <- map_dfr(archivos_2000, limpiar_iter, tipo = "dbf")

# ── DEL ORIGINAL: extraer entidad/municipio y renombrar ──────────────────────
limpiar_censos <- function(x){
  x %>%
    mutate(entidad  = as.numeric(str_sub(geo, 1, 2)),
           municipio = as.numeric(str_sub(geo, 3, 5))) %>%
    select(entidad, municipio, tot_pop)
}

censo_2000 <- limpiar_censos(censo_2000_raw) %>% rename(poblacion_2000 = tot_pop)
censo_2010 <- limpiar_censos(censo_2010_raw) %>% rename(poblacion_2010 = tot_pop)
censo_2020 <- limpiar_censos(censo_2020_raw) %>% rename(poblacion_2020 = tot_pop)

# ── DEL ORIGINAL: merge y cálculo de crecimientos (sin cambios) ──────────────
censos <- inner_join(censo_2000, censo_2010, by = c("entidad", "municipio"))
censos <- inner_join(censos, censo_2020, by = c("entidad", "municipio"))



censos <- censos %>%
  mutate(
    poblacion_2000 = as.numeric(as.character(poblacion_2000)),
    poblacion_2010 = as.numeric(as.character(poblacion_2010)),
    poblacion_2020 = as.numeric(as.character(poblacion_2020))
  )

# =======================================================================================================================



# diferencias entre poblaciones
censos <- censos %>% mutate(diferencia_0_a_10 = poblacion_2010 - poblacion_2000)
censos <- censos %>% mutate(diferencia_10_a_20 = poblacion_2020 - poblacion_2010)

# asumimos crecimiento lineal de la población.
# para esto vamos a cer cuánto creció la población entre 2000 y 2010 y asumimos que el cambio
# total entre esos años crece de la misma forma mes con mes
censos <- censos %>% mutate(div_0_a_10 = diferencia_0_a_10/120)
censos <- censos %>% mutate(div_10_a_20 = diferencia_10_a_20/120)

# municipios con los que trabajamos
municipios <- censos %>% select(entidad, municipio)

# vamos a crear una base de datos con las fechas
anio <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
          2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,
          2021, 2022, 2023, 2024)
mes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
fechas <- expand_grid(anio, mes)

# combinamos fechas y municipios
municipios_fechas <- merge(fechas, municipios)

# limpiamos environment
rm(censo_2000, censo_2010, censo_2020, fechas, municipios, anio, mes)

# ordenamos variables
municipios_fechas <- municipios_fechas %>% arrange(entidad, municipio, anio, mes)

# separamos por año
municipios_fechas_1 <- municipios_fechas %>% filter(anio < 2010)
municipios_fechas_2 <- municipios_fechas %>% filter(anio >= 2010 & anio < 2020)
municipios_fechas_3 <- municipios_fechas %>% filter(anio >= 2020)

# escribimos el factor con el cual crece la población
n_munis <- nrow(distinct(municipios_fechas_1, entidad, municipio))
factor <- tibble(value = rep(seq(0, 119, by = 1), n_munis))

# pegamos el factor a las bases
municipios_fechas_1 <- cbind(municipios_fechas_1, factor)
municipios_fechas_2 <- cbind(municipios_fechas_2, factor)

# vamos a calcular el crecimiento lineal de la población de 2000 a 2010
censo_0_10 <- censos %>% select(entidad, municipio, poblacion_2000, div_0_a_10, poblacion_2010)
municipios_fechas_1 <- inner_join(municipios_fechas_1, censo_0_10, by = c("entidad", "municipio"))
municipios_fechas_1 <- municipios_fechas_1 %>% mutate(poblacion = poblacion_2000 + value*div_0_a_10)
municipios_fechas_1 <- municipios_fechas_1 %>% select(anio, mes, entidad, municipio, poblacion)

# vamos a hacer el crecimiento lineal de la población de 2010 a 2020
censo_10_20 <- censos %>% select(entidad, municipio, poblacion_2010, div_10_a_20, poblacion_2020)
municipios_fechas_2 <- inner_join(municipios_fechas_2, censo_10_20, by = c("entidad", "municipio"))
municipios_fechas_2 <- municipios_fechas_2 %>% mutate(poblacion = poblacion_2010 + value*div_10_a_20)
municipios_fechas_2 <- municipios_fechas_2 %>% select(anio, mes, entidad, municipio, poblacion)

# de 2020 a 2021 vamos a asumir la misma población (no crece) porque no tenemos datos
censo_20_21 <- censos %>% select(entidad, municipio, poblacion_2020)
municipios_fechas_3 <- inner_join(municipios_fechas_3, censo_20_21, by = c("entidad", "municipio"))
municipios_fechas_3 <- municipios_fechas_3 %>% rename(poblacion = poblacion_2020)

# pegamos nuestras bases de datos
municipios_fechas <- rbind(municipios_fechas_1, municipios_fechas_2, municipios_fechas_3)

# limpiamos environment
rm(censo_0_10, censo_10_20, censo_20_21, censos, factor, municipios_fechas_1, municipios_fechas_2, municipios_fechas_3)



#-------------------------------------------------------------------------------------------------------------------------



# ahora sí construímos la base final

# renombramos algunas variables para el merge
municipios_fechas <- municipios_fechas %>% rename(mes_hom = mes) %>%
                                           rename(anio_hom = anio) %>%
                                           rename(entidad_hom = entidad) %>%
                                           rename(municipio_hom = municipio)


# acomodamos variables para el merge
tasas_homicidios <- tasas_homicidios %>% arrange(entidad_hom, municipio_hom, anio_hom, mes_hom)

# pegamos con la base de tasa de homicidios
tasas_homicidios <- right_join(tasas_homicidios, municipios_fechas,
                              by = c("anio_hom", "mes_hom", "entidad_hom", "municipio_hom"))

# volvemos a acomodar
tasas_homicidios <- tasas_homicidios %>% arrange(entidad_hom, municipio_hom, anio_hom, mes_hom)

# valores donde hay o homicidios
tasas_homicidios$homicidios[is.na(tasas_homicidios$homicidios)] <- 0

# calculamos la tasa de homicidios por 10,000 habitantes
tasas_homicidios <- tasas_homicidios %>% mutate(tasa_homicidios = (homicidios/poblacion)*10000)

# nos quedamos con variables de interés (después le podemos agregar más variables)
tasas_homicidios <- tasas_homicidios %>% select(entidad_hom, municipio_hom, anio_hom, mes_hom,
                                                poblacion, homicidios, tasa_homicidios)


# guardamos la base de datos
write_dta(tasas_homicidios, "data/processed/tasas_homicidios.dta")


#----------------------------------------------------------------------------------------------------------------



# algunas gráficas para analizar datos

# primero obtenemos el número de homicidios a nivel anual
hom_anual <- homicidios %>% group_by(anio_hom, mes_hom) %>% mutate(homicidios = n())
hom_anual <- hom_anual %>% distinct(anio_hom, mes_hom, homicidios)
hom_anual <- hom_anual %>% mutate(fecha = make_date(year = anio_hom, month = mes_hom))

# calculamos población total por mes
pob_anual <- tasas_homicidios %>% group_by(mes_hom, anio_hom) %>% summarise(poblacion = sum(poblacion))

# pegamos la base de poblacion total por mes con homicidos totales por mes (para cada municipio)
hom_anual <- inner_join(hom_anual, pob_anual, by = c("mes_hom", "anio_hom"))

# tasa de homicidios por cada 100,000 habitantes
hom_anual <- hom_anual %>% mutate(tasa_homicidios = (homicidios/poblacion)*100000)



# serie de tiempo de tasas de homicidios a nivel nacional por mes (por cada 100,000 habitantes)
# OJO: sale igual al paper de andrea velázquez
ggplot(hom_anual, aes(fecha, tasa_homicidios)) + # Ejes
       geom_line(color = "darkred", linewidth = 1) + # Tipo de línea
       scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Frecuencia de datos
       ggtitle("") +
       xlab("") +
       ylab("Monthly Homicide Rate (per 100,000)") + 
       theme(axis.line = element_line(colour = "black")) + 
       theme_bw() + # Quitar el fondo gris 
       theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
       ylim(0, 3) +
       annotate("rect",
                 xmin = as.Date("2005-01-01"), xmax = as.Date("2006-12-31"),
                 ymin = -Inf, ymax = Inf,
                 fill = "blue", alpha = 0.15) +
       annotate("rect",
                 xmin = as.Date("2009-01-01"), xmax = as.Date("2012-12-31"),
                 ymin = -Inf, ymax = Inf,
                 fill = "blue", alpha = 0.15) +
       geom_text(data = data.frame(x = as.Date(c("2005-03-10", "2009-03-10")), y = c(3250, 3250), label = c("MxFLS-2", "MxFLS-3")),
                  aes(x = x, y = y, label = label), angle = 90, vjust = 0.5, hjust = 0.5)
      


# serie de tiempo de homicidios a nivel nacional por mes 
ggplot(hom_anual, aes(fecha, homicidios)) + # Ejes
       geom_line(color = "darkred", linewidth = 1) + # Tipo de línea
       scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Frecuencia de datos
       ggtitle("") +
       xlab("") +
       ylab("Homicides") + 
       theme(axis.line = element_line(colour = "black")) + 
       theme_bw() + # Quitar el fondo gris 
       theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
       ylim(250, 3500) +
       annotate("rect",
                xmin = as.Date("2005-01-01"), xmax = as.Date("2006-12-31"),
                ymin = -Inf, ymax = Inf,
                fill = "blue", alpha = 0.15) +
       annotate("rect",
                xmin = as.Date("2009-01-01"), xmax = as.Date("2012-12-31"),
                ymin = -Inf, ymax = Inf,
                fill = "blue", alpha = 0.15) +
       geom_text(data = data.frame(x = as.Date(c("2005-03-10", "2009-03-10")), y = c(3250, 3250), label = c("MxFLS-2", "MxFLS-3")),
                 aes(x = x, y = y, label = label), angle = 90, vjust = 0.5, hjust = 0.5)
  


# Histograma de la tasa de homicidios a nivel nacional por mes
PlotRelativeFrequency(hist(hom_anual$tasa_homicidios, breaks = 30), col = "darkred",
                      xlab = "Monthly Homicide Rate (per 100,000)", ylab = "Relative Frequency", main = "")


# Histograma de los homicidios a nivel nacional por mes
PlotRelativeFrequency(hist(hom_anual$homicidios, breaks = 40), col = "darkred",
                      xlab = "Monthly Homicides", ylab = "Relative Frequency", main = "")


# histograma de edades que aparecen en la base de todos los homicidios
PlotRelativeFrequency(hist(homicidios$edad, breaks = 80), col = "darkred",
                      xlab = "Age", ylab = "Relative Frequency", main = "",
                      xaxp = c(0, 120, 12))

summary(homicidios$edad) # descriptivos edad

mean(homicidios$mujer, na.rm = TRUE) # proporción de mujeres que aparecen en la base

mean(homicidios$extranjero, na.rm = TRUE) # tampoco es tan relevante

mean(homicidios$mayor_edad, na.rm = TRUE) # casi todxs son mayores de edad



# datos por año, estado o municipio (para ver algunas cifras)
año <- tasas_homicidios %>% group_by(anio_hom) %>% summarise(sum(homicidios))
entidad <- tasas_homicidios %>% group_by(entidad_hom) %>% summarise(sum(homicidios))
municipio <- tasas_homicidios %>% group_by(entidad_hom, municipio_hom) %>% summarise(sum(homicidios))


# ------------------------------------------------------------------------------
# VERIFICACIÓN: población interpolada para una muestra de municipios
# ------------------------------------------------------------------------------

set.seed(42)
munis_muestra <- tasas_homicidios %>%
  distinct(entidad_hom, municipio_hom) %>%
  slice_sample(n = 6)

pob_muestra <- tasas_homicidios %>%
  inner_join(munis_muestra, by = c("entidad_hom", "municipio_hom")) %>%
  mutate(
    fecha      = make_date(anio_hom, mes_hom),
    muni_label = paste0("Ent ", entidad_hom, " - Mun ", municipio_hom)
  )

p_pob <- ggplot(pob_muestra, aes(x = fecha, y = poblacion, color = muni_label)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.Date(c("2000-01-01", "2010-01-01", "2020-01-01")),
             linetype = "dashed", color = "gray40") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(x = "", y = "Población", color = "Municipio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dir.create("figs/graphs", recursive = TRUE, showWarnings = FALSE)
ggsave("figs/graphs/verificacion_poblacion.png", plot = p_pob,
       width = 10, height = 6, dpi = 300)

message("Verificación guardada en figs/graphs/verificacion_poblacion.png")






