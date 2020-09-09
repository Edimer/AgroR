library(tidyverse)
library(readxl)

# Vector de nombres
nombres <- c("numero", "sexo", "fecha_nac", "reproduccion", "peso_nto", "raza",
             "num_madre", "num_padre", "raza_madre", "raza_padre", "fecha_dest",
             "edad_dest", "peso_dest", "edad_destM", "ganancia_dia")

# Leer los datos con cambio de nombres
datos <- read_xls("Datos ganadero PD Cotove.xls", skip = 11, col_names = nombres)


# Estructura de los datos
glimpse(datos)

# Seleccionar variables de interés
datos2 <- datos %>% # CTRL + SHIFT + M
  select(-c(numero, num_madre, num_padre, edad_destM))

# Editar variables
library(openxlsx)
datos3 <- datos2 %>% 
  mutate(fecha_dest = convertToDate(fecha_dest),
         fecha_nac = as.Date(fecha_nac)) %>% 
  mutate_if(is.character, as.factor)

glimpse(datos3)  

# Creando nuevas variables
library(lubridate)
datos4 <- datos3 %>% 
  mutate(año_nac = year(fecha_nac),
         mes_nac = month(fecha_nac),
         año_dest = year(fecha_dest),
         mes_dest = month(fecha_dest))
glimpse(datos4)

# Exportar a formato .Rdata
#dataFinal <- datos4
save(datos4, file = "cotoveFinal.Rdata")

# Prueba de lectura
load("cotoveFinal.Rdata")
