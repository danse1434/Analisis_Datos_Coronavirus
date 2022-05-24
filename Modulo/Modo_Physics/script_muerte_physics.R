##------------------------------------------------------------------------#
## Nombre del Script: Análisis de fallecimiento exponencial de COVID-19  ----
##  
## Propósito del Script: realizar una copia del gráfico propuesto por Minute 
##  of Physics 
##  Created by Aatish Bhatia in collaboration with Minute Physics · World 
##  data provided by Johns Hopkins University · US state data provided by 
##  NYTimes · Shortcuts: +/- for daily changes, space to play/pause · Credits 
##  & Source · Stay safe!  
##  
## Autor: Daniel S. Parra Gonzalez 
## Fecha de creacion: 04-04-2020 
##
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
# Carga de paquetes
require(ggplot2)
require(tidyverse)
require(RcppRoll)
require(scales)
require(gganimate)
require(directlabels) # Rectas al final de gráficos
require(lubridate)

##########################################################################-
# Lectura de datos
data_mp <- source('Modulo/lectura_datos.R')

# Fecha actualizacion de datos para gráficos
a <- glue::glue("Actualizado {now()}")

##########################################################################-
# Modificación de archivo de datos ----------------------------------------
##########################################################################-
# Creación de tabla modificada data1
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Crear una variable con la semana epidemiológica
##  2 Ordenar la tabla por locación y fecha
##  3 Agrupar por locación
##  4 Eliminar registros con fallecimientos igual a cero
##  5 Calcular *dd* que es diferencia en tiempo desde la fecha mínima en 
##  días.
##  6 Desagrupar la tabla
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data1 <- data %>%
  mutate(Epiwk = epiweek(Date)) %>%
  arrange(Location, Date) %>% 
  group_by(Location) %>%
  filter(Muertes_Totales != 0) %>% 
  mutate(dd = difftime(Date, min(Date), units = "days") %>% as.numeric(.)) %>% 
  ungroup(.)

##########################################################################-
# Cálculo días de lag
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Agrupar por Locación
##  2 Calcular los datos de fallecimientos de casos por día
##  3 Calcular muertes acumulados de los 7 días anteriores *Muert_lw*
##  4 Convertir valores de *Muert_lw* para que nunca sean cero
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data4 <- data1 %>%
  group_by(Location) %>%
  mutate(Muert_Incid = Muertes_Totales - lag(Muertes_Totales)) %>%
  mutate(Muert_lw = RcppRoll::roll_sum(Muert_Incid, 7, fill = NA, align = "right")) %>% 
  mutate(Muert_lw = if_else(Muert_lw == 0 | is.na(Muert_lw), 1E-16, Muert_lw))

##########################################################################-
# Función de conversión fechas --------------------------------------------
##########################################################################-

#' Conversión de fechas a formato dd-mm-yy con abreviatura en mes
#'
#' @param date fecha en formato date
#' @return (carácter) fecha en forma de carácter
#' @examples
#' conv_date(data4$Date)
#' 
conv_date <- function(date) {
  x <- date %>% ymd()
  
  a = x %>% day()
  b = x %>% month(label = TRUE, abbr = TRUE)
  c = x %>% year()
  
  return(paste(a, b, c, sep = '-'))
}

#-------------------------------------------------------------------------#
# Función de selección número de fallecimientos
conv_casos <- function(x) {
  isEmpty <- function(y) {return(length(y) == 0)}
  z = filter(data5, Date == as.character(x),
           Location == 'Colombia') %>% pull(Casos_Totales)
  if (isEmpty(z)) {return(0)} else {return(z)}
}

conv_muertes <- function(x) {
  isEmpty <- function(y) {return(length(y) == 0)}
  z = filter(data5, Date == as.character(x),
             Location == 'Colombia') %>% pull(Muertes_Totales)
  if (isEmpty(z)) {return(0)} else {return(z)}
}

# conv_muertes("2020-03-21")

##########################################################################-
# Gráfico de comparación países de referencia -----------------------------
##########################################################################-
# Vector con países de referencia
c5 <- c('US', 'Germany', 'Argentina', 'Italy', 'Peru',
        'United Kingdom', 'Australia', 'Chile', 'Ecuador', 
        'Brazil', 'Mexico', 'Colombia')

# Seleccionar sólo las locaciones en c5
data5 <- data4 %>%
  filter(Location %in% c5)

# Preparación de gráfico base
G_mp <- data5 %>% 
  ggplot(aes(x = Muertes_Totales, y = Muert_lw, group = Location)) +
  # Datos de c5 
  geom_line(data = data5 %>% filter(Location != 'Colombia'),
            alpha = 0.1) +
  geom_point(data = data5 %>% filter(Location != 'Colombia'),
             alpha = 0.1) +
  # Datos de Colombia 
  geom_line(data = data5 %>% filter(Location == 'Colombia'), 
            col = 'red') +
  geom_point(data = data5 %>% filter(Location == 'Colombia'),
             col = 'red', size = 3) +
  # Parámetros estéticos
  xlab('Fallecimiento asociados totales') + ylab('Fallecimientos 7d anteriores') +
  # Aplicación de la función conv_date en la expresión "glue"
  labs(title = 'Trayectoria de Fallecimientos asociados a COVID-19',
       subtitle = paste0('Fecha: ', "{conv_date(frame_along)}",
                         ";     Casos COL: ", "{conv_casos(frame_along)}",
                         "; Fallecimientos COL: ", "{conv_muertes(frame_along)}"), 
      caption = paste0("Adaptado a partir de Aatish Bhatia en colaboración con Minute Physics: https://aatishb.com/covidtrends \n",
                       "Datos tomados de: ", "https://github.com/CSSEGISandData/COVID-19. ",
                       a)) +
  coord_cartesian(ylim = c(10, 10 ^ (7)), xlim = c(10, 10 ^ (7))) +
  scale_y_log10(breaks = 10 ^ (0:7),
                labels = trans_format("log10", math_format(10 ^ .x))) +
  scale_x_log10(breaks = 10 ^ (0:7),
                labels = trans_format("log10", math_format(10 ^ .x))) +
  annotation_logticks(sides = 'bl') +
  theme_bw() +
  # Mundo
  geom_dl(data = data5 %>% filter(Location != 'Colombia'),
          aes(label = Location),
          method = list(dl.trans(x = x * 1.05), "last.points", cex = 0.8)) +
  # Colombia
  geom_dl(data = data5 %>% filter(Location == 'Colombia'),
          aes(label = Location), col = 'red',
          method = list(dl.trans(x = x * 1.05), "last.points", cex = 0.8)) +
  theme(panel.grid = element_line(colour = 'white'))

# Visualización previa de gráfico base
G_mp

##########################################################################-
# Elaboración de GIF ------------------------------------------------------
##########################################################################-
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Transición de tipo reveal para la variable *Date*
##  2 Velocidad de transición cúbica
##  3 Animar el objeto *G_manim* con resolución específica, terminar la 
##  figura con 10 repeticiones y rebobinar en reversa.
##  4 Almacenar en formato .gif
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

G_manim <- G_mp +
  transition_reveal(Date) +
  ease_aes(default = "cubic-out")

# Almacenamiento de objeto en gif
animate(G_manim, end_pause = 10, rewind = FALSE, 
        height = 680, width = 800, res = 140, duration = 30) 
anim_save(file.path('Figuras', paste0(today(), "R6", ".gif")))










