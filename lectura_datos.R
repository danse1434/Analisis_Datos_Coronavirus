##------------------------------------------------------------------------#
## Nombre del Script: Lectura de datos de repositorio GitHUB  -
##  
## Propósito del Script: realizar la lectura de archivos de repositorio de 
##  gitHub establecido para trazar en Covid19. Se adaptó el método de Ben 
##  Phillips de https://github.com/benflips/nCovForecast/blob/master/getDataNew.R#L27
##  
## Autor: Daniel S. Parra Gonzalez 
## Fecha de creación:  27-03-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
# Carga de paquete
require(tidyverse)
require(lubridate)

# Directorio de casos confirmados
tsConf <- file.path('https:/','raw.githubusercontent.com','CSSEGISandData',
          'COVID-19',
          'master', 'csse_covid_19_data', 'csse_covid_19_time_series', 
          'time_series_covid19_confirmed_global.csv')

# Directorio de muertes confirmados
tsDeath <- file.path('https:/', 'raw.githubusercontent.com', 'CSSEGISandData', 
          'COVID-19', 
          'master', 'csse_covid_19_data', 'csse_covid_19_time_series', 
          'time_series_covid19_deaths_global.csv')

##########################################################################-
# Lectura de archivos de datos de casos y muertes confirmadas
tsI <- read_csv(file = tsConf)
tsD <- read_csv(file = tsDeath)

##########################################################################-
# Modificación de archivo de datos
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Cambio de datos
##  2 Eliminar variables latitud y longitud
##  3 Renombrar columna a Location
##  4 Renombrar columna a Province
##  5 Colapsar datos en columnas con fechas
##  6 Renombrar la fecha
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

tsI1 <- tsI %>%
  select(-Lat, -Long) %>%
  rename(Location = `Country/Region`) %>%
  rename(Province = `Province/State`) %>%
  gather(key = 'Date', value = 'Casos_Totales', matches("\\d{1,2}")) %>%
  mutate(Date = mdy(Date))


tsD1 <- tsD %>%
  select(-Lat, -Long) %>%
  rename(Location = `Country/Region`) %>%
  rename(Province = `Province/State`) %>%
  gather(key = 'Date', value = 'Muertes_Totales', matches("\\d{1,2}")) %>%
  mutate(Date = mdy(Date))

##########################################################################-
# Unión de archivos de datos
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 selección de ts1
##  2 Adición por izquierda de tsD1
##  3 Agrupar por locación y fecha
##  4 Resumir por casos totales en Locación y fecha
##  5 Resumir por muertes totales en Locación y fecha
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data <- tsI1 %>%
  left_join(tsD1, by = c('Province', 'Location', 'Date')) %>%
  group_by(Location, Date) %>%
  summarise(
    Casos_Totales = sum(Casos_Totales),
    Muertes_Totales = sum(Muertes_Totales)
  )

##########################################################################-
# Remover objetos no utilizados
rm('tsI1', 'tsD1', 'tsI', 'tsD', 'tsConf', 'tsDeath')






