##------------------------------------------------------------------------#
## Nombre del Script: Análisis inicial de datos de expansión de COVID-19  -
##  
## Propósito del Script: análisis inicial de los datos de expansión de escala 
##  logarítmica de COVID-19.
##  
## Autor: Daniel S. Parra Gonzalez 
## Fecha de creación:  20-03-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
# Carga de paquetes
library(magrittr)     #
library(lubridate)    #
library(rlang)        # Evaluación tardía
library(tidyverse)    #
library(grid)         #
library(patchwork)    #
library(gtable)       #  
library(ggrepel)      # Ampliación de espacios entre puntos
library(readxl)       # Lectura de archivos de Excel
library(httr)         # Lectura de vínculos https//
library(directlabels) # Rectas al final de gráficos

#-------------------------------------------------------------------------------#
# Carga de funciones definidas por usuario
source("Modulo/funciones.R", encoding = "UTF-8")

##########################################################################-
# Lectura de archivo de datos ---------------------------------------------
##########################################################################-
source("Modulo/lectura_datos.R", encoding = "UTF-8")
data

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
# Gráficos iniciales por países -------------------------------------------
##########################################################################-
# Selección de tema para gráficos
theme_set(theme_classic() +
            theme(panel.border = element_rect(fill = NA, colour = 'black')))

# Gráficos de referencia escala original

# Vector con países de referencia internacional
c1 = list(
  # Original
  orig = c('Colombia', 'Spain', 'France', 'Italy', 
           'Japan', 'Korea, South', 'US', 'Germany'),
  # Traducción para mostrar en gráfico
  trad = c('Colombia', 'España', 'Francia', 'Italia', 
           'Japón', 'Corea del Sur', 'EEUU', 'Alemania')
)

# Gráfico 1 - Vector 1 referencia internacional 
G1 <- 
  data1 %>%
  filter(Location %in% c1$orig) %>% 
  mutate(Location = factor(Location, levels = c1$orig)) %>% 
  ggplot(aes(x = dd, y = Casos_Totales, 
             group = Location, colour = Location, 
             linetype = Location, shape = Location)) +
  geom_line() + geom_point() + 
  aux_param(c1)

# Vector con países de referencia Latinoamérica
c2 = list(
  # Original
  orig = c('Colombia', 'Brazil', 'Argentina', 'Chile', 'Peru', 'Ecuador'),
  # Traducción para mostrar en gráfico
  trad = c('Colombia', 'Brasil', 'Argentina', 'Chile', 'Perú', 'Ecuador')
)

# Gráfico 2 - Vector 2 referencia internacional 
G2 <- data1 %>%
  filter(Location %in% c2$orig) %>% # Filtrar países por c2
  mutate(Location = factor(Location, levels = c2$orig)) %>% # Ordenar
  ggplot(aes(x = dd, y = Casos_Totales, 
             group = Location, colour = Location, 
             linetype = Location, shape = Location)) +
  geom_line() + geom_point() + 
  aux_param(c2)
  
##########################################################################-
# Almacenamiento de gráficos en PDF
G1_comp <- (G1 + G2) +
  plot_annotation(
    title = 'Reporte de casos acumulados COVID-19',
    subtitle = '275 días desde primer reporte en países de referencia',
    caption = paste0(
      "Estos números no reflejan la totalidad de personas infectadas, ",
      "sólo el número de personas quienes han sido positivas. \n",
      "Este no es un gráfico oficial, sólo informativo. ",
      "Datos tomados de: https://github.com/CSSEGISandData/COVID-19 \n",
      a))

# Almacenamiento de objeto G1_comp en formato de *pdf* y *png*
ggsave(file.path('Figuras', paste0(today(), "R1", ".pdf")), G1_comp, 
       width = 8.5, height = 5, device = 'pdf')

ggsave(file.path('Figuras', paste0(today(), "R1", ".png")), G1_comp, 
       width = 8.5, height = 5, device = 'png', dpi = 300)
 
##########################################################################-
# Gráfico de comportamiento China 
# Este gráfico no se imprime y sólo tiene fines informativos
G_China <- data1 %>% 
  filter(Location == "China") %>% 
  ggplot(aes(x = dd, y = Casos_Totales)) +
  geom_line(col = 'blue4') + 
  geom_line(aes(y = Muertes_Totales), col = 'red3') +
  scale_y_log10(breaks = 10^(1:6),
                labels = 
                  scales::trans_format("log10", 
                                       scales::math_format(10 ^ .x)) ) +
  annotation_logticks(sides = 'l') +
  xlab('Días desde inicio') + ylab('Casos reportados') + 
  labs(title = 'Reporte de Casos China Covid-19',
       subtitle = "Tomado CSSEGISandData COVID-19")

##########################################################################-
# Modelamiento de tiempo de duplicación -----------------------------------
##########################################################################-
# Creación de listas con tablas de datas separadas por país
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Tomar la tabla de datos *data1*
##  2 Calcular una variable *dd1* que le adicione un día a los días desde el 
##  inicio de epidemia, esto evita los casos donde se tenga dd=0
##  3 Agrupar la tabla por locación.
##  4 Filtrar aquellos países que lleven por lo menos 5 días desde el 
##  inicio de la epidemia.
##  5 Filtrar por país eliminando a China.
##  6 Agrupar a los datos en una columna-lista "data", excepto "Location".
##  7 Aplicar la función *nls_function* con los datos en la columna-lista
##  8 Aplicar la función summary.nls en la columna "model"
##  9 Aplicar la función tidy para obtener parámetros estimados>-> "pr"
##  10  Aplicar la función glance para obtener medidas de GOF  >-> "gl"
##  11  Desanidar a "pr" y "gl" quedan como filas
##  12  Calcular el día del año donde se dio inicio de reportes   >-> "firstd"
##  13  Calcular la semana del año donde se dio inicio de reportes>-> "firstw"
##  14  Calcular el número de filas en cada elemento de columna-lista
##  15  Calcular el coef. de variación de estimación de thalf
##  16  Calcular error de IC en distribución t.
##  17  Renombrar a las columnas estimate >-> thalf_mn
##  18  Renombrar a las columnas estimate >-> thalf_sd
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

datalist <- data1 %>%
  mutate(dd1 = dd + 1) %>%
  group_by(Location) %>%
  filter(n() > 5) %>%
  filter(Location != 'China') %>% 
  nest() %>% 
  mutate(model = map(data, ~nls_function(.x)),
         summa = map(model, ~summary(.x)) ) %>%
  mutate(pr = map(model, ~broom::tidy(.x)),
         gl = map(model, ~broom::glance(.x))) %>% 
  unnest(cols = c('pr', 'gl')) %>% 
  mutate(firstd   = map_dbl(data, ~ yday(min(.x$Date))),
         firstw   = map_dbl(data, ~ epiweek(min(.x$Date))),
         N        = map_dbl(data, ~dim(.x)[[1]]),
         RSD      = std.error/estimate,
         error    = qt(0.975, df=N-1) * std.error/sqrt(N)) %>%
  rename(thalf_mn = estimate,
         thalf_sd = std.error) 
  
##########################################################################-
# Creación de gráfico de tiempo de duplicación vs día del año

set.seed(245)

G_THALF <- datalist %>% 
  ggplot(aes(x = firstd, y = thalf_mn, col = RSD)) +
  geom_errorbar(aes(ymin = thalf_mn - error,
                    ymax = thalf_mn + error)) +
  geom_point() +
  stat_smooth(method = 'loess', formula = y ~ x) +
  ylab('T. duplicación casos aparente (días)') +
  xlab('Día del año (detección primer caso)') +
  labs(title = 'Modelamiento tiempo de duplicación aparente por país', 
       subtitle = expression(T[1/2]~'duplicación de casos aparente por regresión no lineal vs día del año detección primer caso')) + 
  coord_cartesian(ylim = c(0, 30), xlim = c(0,130)) +
  geom_point(data = filter(datalist, Location=='Colombia'),
             col = 'red') +
  geom_text_repel(aes(label = Location),
                  data = filter(datalist, 
                                Location%in%c(c1$orig, c2$orig)), 
                  box.padding = unit(0.95, "lines")) +
  theme(legend.position = c(0.5, 0.1)) +
  guides(colour = guide_colorbar(direction = "horizontal"))

# Almacenamiento de objeto G_THALF en formato de *pdf* y *png*
ggsave(file.path('Figuras', paste0(today(), "R2", ".pdf")), G_THALF, 
       width = 6, height = 5.0, device = 'pdf')

ggsave(file.path('Figuras', paste0(today(), "R2", ".png")), G_THALF, 
       width = 6, height = 5.0, device = 'png', dpi = 300)

##########################################################################-
# Curva epidémica con logs de crecimiento ---------------------------------
##########################################################################-
# Gráfico en escala logarítmica con guías de tiempo de duplicación

# Vector de selección de países de Suramérica para comparación
# Se agregaron países adicionales de América del Sur
c3 <- list(
  orig = c(c2$orig, 'Panama', 'Mexico', 'Bolivia', 'Uruguay', 
           'Paraguay', 'Venezuela'),
  trad = c(c2$trad, 'Panama', 'México', 'Bolivia', 'Uruguay', 
           'Paraguay', 'Venezuela')
  )

##########################################################################-
# Creación de objeto G3 en escala logarítmica
min_brks <- rep(1:9, 10)*(10^rep(0:9, each=9))

G3 <- data1 %>%
  # Filtrar países de interés
  filter(Location %in% c3$orig) %>% 
  mutate(Location = factor(Location, levels = c3$orig)) %>%
  ggplot(aes(x = dd, y = Casos_Totales, 
             group = Location, colour = Location)) +
  # Guías de tiempo de duplicación
  line_t(1) + line_t(2) + line_t(3) + line_t(7) + line_t(30) +
  geom_line() + 
  geom_point(data = . %>%
               group_by(., Location) %>%
               slice(which.max(dd))) + 
  scale_y_log10(breaks = 10^(1:7), minor_breaks = min_brks,
                labels = scales::trans_format("log10", scales::math_format(10^.x)) ) +
  aux_param(ls = c3, cond = 2) +
  coord_cartesian(xlim = c(0, 270), ylim = c(1E0,1E7)) + 
  labs(title = 'Curvas epidémicas de COVID19 en países seleccionados', 
       subtitle = "Incluye casos relacionados, importados, y en estudio. Escala logarítmica / desde caso índice.", 
       caption = paste0("Estos números reflejan el total de personas infectadas, sólo el número de personas quienes han sido positivas. \n",
                        "Este no es un gráfico oficial, sólo informativo. Datos tomados de: ", "https://github.com/CSSEGISandData/COVID-19 \n",
                        a))+
  geom_dl(aes(label = Location),
          method = list(dl.trans(x = x + 0.12), "last.bumpup", cex = 0.8)) +
  annotation_logticks(sides = 'l') +
  theme(panel.grid.major.x = element_line(colour = 'gray90'), 
        panel.grid.minor.y = element_line(colour = 'gray98'),
        legend.position = "none",
        panel.grid.major = element_line(colour = "gray96"),
        plot.caption = element_text(hjust = 0))

##########################################################################-
# Adicionar leyendas en guías de duplicación de datos

legend_dup <- tribble(
      ~r,     ~t,             ~label, ~rot,
  22.7, 50.000,   "Duplica diario",  81L,
  35.8, 28.489,  "Duplica cada 2d",  73L,
  60.0, 20,  "Duplica cada 3d",   68L,
  45.0,  7.000,  "Duplica cada 7d",   48L,
  200,  1.185, "Duplica cada mes",   0L
  ) %>% 
  mutate(x = r*cos(t*pi/180),
         y = 2^(r*sin(t*pi/180)))

# Creación de objeto G3 con leyendas en las guías de duplicación
G4 <- G3 + 
  geom_text(data = legend_dup,
            mapping = aes(x = x, y = y, label = label, angle = rot), 
            stat = "identity", position = "identity", 
            col = "black", inherit.aes = FALSE)
G4 
# Almacenamiento de objeto G4 en formato de *pdf* y *png*
ggsave(file.path('Figuras', paste0(today(), "R3", ".pdf")), G4, width = 8, 
       height = 6, device = 'pdf')

ggsave(file.path('Figuras', paste0(today(), "R3", ".png")), 
       G4, width = 8, height = 6, device = 'png', dpi = 300)

##########################################################################-
# Curva epidémica con logs de crecimiento (caso 100) ----------------------
##########################################################################-
# Modificación de tabla de datos 1 (data1)
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 data1.
##  2 Filtrar registros con casos totales mayores a 100.
##  3 Agrupar por locación.
##  4 Calcular variable dd2 que el número de días desde que se superaron 
##  los 100 casos.
##  5 Filtrar por las locaciones contenidas en el vector c3.
##  6 Desagrupar.
##  7 Convertir locación en un vector ordenado de acuerdo a c3.
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data3 <- data1 %>% 
  filter(Casos_Totales >= 100) %>% 
  group_by(Location) %>% 
  mutate(dd2 = difftime(Date, min(Date), units = "days") %>% 
           as.numeric(.)) %>% 
  filter(Location %in% c3$orig) %>% 
  ungroup(.) %>% 
  mutate(Location = factor(Location, levels = c3$orig))
  
##########################################################################-
# Creación de gráfico G5 con escala logarítmica y corrección por días de 
# llegada a 100 casos.
  
G5 <- data3 %>% 
  ggplot(aes(x = dd2, y = Casos_Totales, 
             group = Location, colour = Location)) +
  line_t1(1) + line_t1(2) + line_t1(3) + line_t1(7) + line_t1(30) +
  geom_line() + 
  # Seleccionar último punto
  geom_point(data = . %>% group_by(Location) %>% slice(which.max(dd))) + 
  scale_y_log10(breaks = 10^(1:6), minor_breaks = min_brks,
                labels = scales::trans_format("log10", scales::math_format(10^.x)) ) +
  aux_param(c3, cond=2) +
  coord_cartesian(xlim = c(0, 270), ylim = c(1E2, 1E7)) + 
  labs(title = 'Curvas epidémicas de COVID-19 en países seleccionados', 
       subtitle = "Incluye casos relacionados, importados, y en estudio. Escala logarítmica / desde caso 100.", 
       caption = paste0("Estos números no reflejan el total de personas infectadas, sólo el número de personas quienes han sido positivas. \n",
                        "Este no es un gráfico oficial, sólo informativo. Datos tomados de: ", "https://github.com/CSSEGISandData/COVID-19 \n",
                        a)) +
  geom_dl(aes(label = Location),
          method = list(dl.trans(x = x + 0.13), "last.bumpup", cex = 0.8)) +
  xlab('Días desde el caso 100') +
  annotation_logticks(sides = 'l') +
  theme(panel.grid.major.x = element_line(colour = 'gray90'), 
        panel.grid.minor.y = element_line(colour = 'gray98'),
        legend.position = "none",
        plot.caption = element_text(hjust = 0))

##########################################################################-
# Adicionar leyendas en guías de duplicación de datos

legend_dup1 <- tribble(
      ~r,     ~t,             ~label, ~rot,
      16.7, 50.000,   "Duplica diario",  81L,
      28.8, 28.489,  "Duplica cada 2d",  75L,
      40.0, 20,  "Duplica cada 3d",   75L,
      45.0,  7.000,  "Duplica cada 7d",   48L,
      200,  1.500, "Duplica cada mes",   0L
  ) %>% 
  mutate(x = r * cos(t * pi / 180), 
         y = 1E2 * 2 ^ (r * sin(t * pi / 180)))

# Creación de objeto G3 con leyendas en las guías de duplicación
G6 <- G5 + 
  geom_text(data = legend_dup1,
            mapping = aes(x = x, y = y, label = label, angle = rot), 
            stat = "identity", position = "identity", 
            col = "black", inherit.aes = FALSE)
G6

# Almacenamiento de objeto G4 en formato de *pdf* y *png*
ggsave(file.path('Figuras', paste0(today(), "R4", ".png")), 
       G6, width = 8, height = 6, device = 'png', dpi = 300)

ggsave(file.path('Figuras', paste0(today(), "R4", ".pdf")), G6, width = 8, 
       height = 6, device = 'pdf')

