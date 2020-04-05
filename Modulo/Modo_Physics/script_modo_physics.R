##------------------------------------------------------------------------#
## Nombre del Script: Análisis de crecimiento exponencial de COVID-19  ----
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

##########################################################################-
# Lectura de datos
data_mp <- source('lectura_datos.R')

# Fecha actualizacion de datos para gráficos
a <- paste0("Actualizado ", now())

##########################################################################-
# Modificación de archivo de datos ----------------------------------------
##########################################################################-
# Creación de tabla modificada data1
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Crear una variable con la semana epidemiológica
##  2 Ordenar la tabla por locación y fecha
##  3 Agrupar por locación
##  4 Eliminar registros con casos totales igual a cero
##  5 Calcular *dd* que es diferencia en tiempo desde la fecha mínima en 
##  días.
##  6 Desagrupar la tabla
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data1 <- data %>%
  mutate(Epiwk = epiweek(Date)) %>%
  arrange(Location, Date) %>% 
  group_by(Location) %>%
  filter(Casos_Totales != 0) %>% 
  mutate(dd = difftime(Date, min(Date), units = "days") %>% as.numeric(.)) %>% 
  ungroup(.)

##########################################################################-
# Cálculo días de lag
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Agrupar por Locación
##  2 Calcular los datos de incidencia de casos por día
##  3 Calcular casos acumulados de los 7 días anteriores *Casos_lw*
##  4 Convertir valores de *Casos_lw* para que nunca sean cero
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data4 <- data1 %>%
  group_by(Location) %>%
  mutate(Casos_Incid = Casos_Totales - lag(Casos_Totales)) %>%
  mutate(Casos_lw = RcppRoll::roll_sum(Casos_Incid, 7, fill = NA, align = "right")) %>% 
  mutate(Casos_lw = if_else(Casos_lw == 0 | is.na(Casos_lw), 1E-16, Casos_lw))

##########################################################################-
# Gráfico de comparación países de referencia -----------------------------
##########################################################################-
# Vector con países de referencia
c5 <- c('US', 'Spain', 'Germany', 'France', 'Iran', 'Italy', 'China', 
        'United Kingdom', 'Canada', 'Australia', 'India', 'Japan', 
        'Korea, South', 'Brazil', 'Colombia')

# Seleccionar sólo las locaciones en c5
data5 <- data4 %>%
  filter(Location %in% c5)

# Gráfico Base
G_mp <- data5 %>% 
  ggplot(aes(x = Casos_Totales, y = Casos_lw, group = Location)) +
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
  xlab('Casos Confirmados Totales') + ylab('Casos Nuevos 7d anteriores') +
  labs(title = 'Trayectoria de Casos Confirmados de COVID-19',
      subtitle = 'Fecha: {frame_along}',
      caption = paste0("Estos números no dicen nada sobre el número de personas infectadas, sólo el número de personas quienes han sido positivas. \n",
                       "Este no es un gráfico oficial, sólo informativo. Datos tomados de: ", "https://github.com/CSSEGISandData/COVID-19 \n",
                       "Adaptado a partir de Aatish Bhatia en colaboración con Minute Physics: https://aatishb.com/covidtrends \n",
                       a)) +
  coord_cartesian(ylim = c(10, 10 ^ (6)), xlim = c(10, 10 ^ (7))) +
  scale_y_log10(breaks = 10 ^ (0:6),
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
          method = list(dl.trans(x = x * 1.05), "last.points", cex = 0.8)) 

# Visualización previa de gráfico base
# G_mp

##########################################################################-
# Elaboración de GIF

G_manim <- G_mp +
  transition_reveal(Date) +
  ease_aes(default = "cubic-out")

# Almacenamiento de objeto en gif
anim_save('Figuras/G_manim1.gif', animation = G_manim)

# G_anim <- animate(G_manim, fps = 2)
# anim_save('Figuras/G_manim2.gif', animation = G_anim)





