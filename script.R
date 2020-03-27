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
library(magrittr)
library(lubridate)
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(ggrepel)
library(readxl) # Lectura de archivos de Excel
library(httr) # Lectura de vínculos https//
library(directlabels) # Rectas al final de gráficos

##########################################################################-
# Lectura de archivo de datos ---------------------------------------------
##########################################################################-
# Apertura de datos automática
# Datos adaptados de https://ourworldindata.org/coronavirus-source-data
url <- paste(
    "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
    format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

# Elección de URL, configuración específica para la página, se asigna el link de la tabla 
# a la variable *tf*.
httr::GET(url,
          authenticate(":", ":", type = "ntlm"),
          write_disk(tf <- tempfile(fileext = ".xlsx")))

##########################################################################-
# Modificación de tabla de datos *tf*
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Leer archivo de excel
##  2 Renombrar variables date (fecha de reporte); location (lugar de 
##  reporte); new_cases (casos nuevos); total_deaths (muertes totales).
##  3 Cambiar variable *date* a formato /date/
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data <- read_excel(tf) %>%
  rename(
    date = DateRep,
    location = `Countries and territories`,
    new_cases = Cases,
    total_deaths = Deaths
  ) %>%
  mutate(date = as.Date(date))

# Estos datos dan resultados muy desactualizados para su análisis por el momemento
# data <-
#   read_csv(
#     curl::curl("https://covid.ourworldindata.org/data/full_data.csv")
#   )

##########################################################################-
# Modificación de archivo de datos ----------------------------------------
##########################################################################-
# Creación de tabla modificada data1
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Cambiar data a un formato "ymd"
##  2 Crear una variable con la semana epidemiológica
##  3 Ordenar la tabla por locación y fecha
##  4 Agrupar por locación
##  5 Calcular casos totales como suma acumulada de nuevos casos
##  6 Eliminar registros con casos totales igual a cero
##  7 Calcular *dd* que es diferencia en tiempo desde la fecha mínima en 
##  días.
##  8 Desagrupar la tabla
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data1 <- data %>%
  mutate(date = ymd(date)) %>%
  mutate(epiwk = epiweek(date)) %>%
  arrange(location, date) %>% 
  group_by(location) %>%
  mutate(total_cases = cumsum(new_cases)) %>% 
  filter(total_cases != 0) %>% 
  mutate(dd = difftime(date, min(date), units = "days") %>% as.numeric(.)) %>% 
  ungroup(.)

##########################################################################-
# Gráficos iniciales por países -------------------------------------------
##########################################################################-
# Selección de tema para gráficos
theme_set(theme_classic() +
            theme(panel.border = element_rect(fill = NA, colour = 'black')))

#' Función de parámetros auxiliares para añadir a gráficos
#' @param x opción de paleta de colores para gráfico  "magma" ("A"), 
#' "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", predeterminado) 
#' y "cividis" (or "E").
#' @export lista de objetos *ggproto* con especificaciones de formato gráfico
#' @examples
#' ggplot() + aux_param("D") 
#' 
aux_param <- function(x) {
  list(coord_cartesian(xlim = c(0, 30), ylim = c(0, 1000)),
       scale_color_viridis_d(option = x, name = ""), 
       scale_linetype_discrete(name = ""),
       scale_shape_discrete(name = ""),
       xlab("Días desde primer reporte"), ylab("Casos reportados"), 
       theme_bw(), 
       theme(legend.position = c(0.3, 0.75),
             legend.title = element_blank()))
}


##########################################################################-
# Gráficos de referencia escala original

# Vector con países de referencia internacional
c1 = c('Colombia', 'Spain', 'France', 'Italy', 
       'Japan', 'South_Korea', 'United_States_of_America')

# Gráfico 1 - Vector 1 referencia internacional 
G1 <- data1 %>%
  filter(location %in% c1) %>% 
  mutate(location = factor(location, levels = c1)) %>% 
  ggplot(aes(x = dd, y = total_cases, 
             group = location, colour = location, 
             linetype = location, shape = location)) +
  geom_line() + geom_point() + 
  aux_param("D") 
  
# Vector con países de referencia Latinoamérica
c2 = c('Colombia', 'Brazil', 'Argentina', 'Chile', 'Peru', 'Ecuador')

# Gráfico 2 - Vector 2 referencia internacional 
G2 <- data1 %>%
  filter(location %in% c2) %>% # Filtrar países por c2
  mutate(location = factor(location, levels = c2)) %>% # Ordenar
  ggplot(aes(x = dd, y = total_cases, 
             group = location, colour = location, 
             linetype = location, shape = location)) +
  geom_line() + geom_point() + 
  aux_param("C")
  
##########################################################################-
# Almacenamiento de gráficos en PDF

pdf(file = file.path('Figuras',paste0(today(),"R1",".pdf")), 
    width = 8.5, height = 5);{
  grid.arrange(
    G1, G2, nrow = 1,
    top = "Número de casos COVID-19 primeros 30 días desde reporte",
    bottom = textGrob(
      paste0("Datos tomados de: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide \n",
             a), 
      gp = gpar(fontface = 3, fontsize = 9),
      hjust = 1, x = 1 ))
    }; dev.off()

##########################################################################-
# Gráfico de comportamiento China 
# Este gráfico no se imprime y sólo tiene fines informativos
G_China <- data1 %>% 
  filter(location == "China") %>% 
  mutate(muertes_total = cumsum(total_deaths)) %>% 
  ggplot(aes(x = dd, y = total_cases)) +
  geom_line(col = 'blue4') + 
  geom_line(aes(y = muertes_total), col = 'red3') +
  scale_y_log10(breaks = 10^(1:6),
                labels = 
                  scales::trans_format("log10", 
                                       scales::math_format(10 ^ .x)) ) +
  annotation_logticks(sides = 'l') +
  xlab('Días desde inicio') + ylab('Casos reportados') + 
  labs(title = 'Reporte de Casos China Covid-19',
       subtitle = "Tomado ECDC Europa")


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
##  6 Desagrupar en listas por locación.
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

datalist <- data1 %>%
  mutate(dd1 = dd + 1) %>%
  group_by(location) %>%
  filter(n() > 5) %>%
  filter(location != 'China') %>%
  split(., .$location)

##########################################################################-
#' Función de tipo exponencial parametrizada en tiempos de vida media
#' @param x Archivo de datos que contiene datos de dd1 (días desde inicio 
#' de epidemia), y total_cases (casos totales)
#' @return objeto de tipo *nls* con resultados de regresión no lineal. 
#'
#' @examples
#' nls_function(data1)
#' datalist[['Chile']] %>% nls_function(.)
#' 
nls_function <- function(x) {
  out <- tryCatch({
    nls(total_cases ~ exp(log(2) * dd1 / thalf),
        data = x,
        start = list(thalf = 5))
  },
  error = function(cond) {
    message(cond)
    return(NA)
  },
  warning = function(cond) {
    message(cond)
    return(NULL)
  })
  return(out)
}

##########################################################################-
# Aplicación de la función nls_function a todos los elementos de la lista 
# *datalist*, se descartan aquellos elementos que sólo sean vectores 
# (arrojaron error).
dl1 <- datalist %>%
  map( ~ nls_function(.x)) %>% 
  purrr::discard(~ is.vector(.x))

##########################################################################-
# Creación de objeto transformado a partir de *dl1*
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Aplicar a función resumen a cada objeto de tipo _nls_
##  2 Seleccionar parámetros dentro de la función resumen
##  3 Crear data.frame con identidad de locación, resultados de parámetro.
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

dl2 <- dl1 %>%
  map( ~ summary(.x)) %>% 
  map( ~ magrittr::use_series(.x, 'parameters')) %>% 
  map_dfr( ~ as.data.frame(.x), .id = 'location')

##########################################################################-
# Creación de tabla *data2* con parámetros de tiempo de duplicación calculados 
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Tomar data1
##  2 Adicionar la tabla dl2 con los parámetros calculados mediante la 
##  variable locación.
##  3 Filtrar locaciones donde no se haya obtenido un estimado
##  4 Agrupar por locación
##  5 Resumir por locación primer día de inicio (firstd), primera semana de 
##  epidemia (firstd), promedio de días estimados thalf (thalf_mn), sd de 
##  días estimados (thalf_sd), número de datos usados para la estimación (n)
##  6 Calcular el error para obtener IC95% basados en la prueba t
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data2 <- data1 %>%
  left_join(dl2, by = 'location') %>%
  filter(!is.na(Estimate)) %>%
  group_by(location) %>%
  summarise(
    firstd = yday(min(date)),
    firstw = epiweek(min(date)),
    thalf_mn = mean(Estimate),
    thalf_sd = mean(`Std. Error`),
    n = n() ) %>%
  mutate(error = qt(0.975, df = n - 1) * thalf_sd / sqrt(n))

##########################################################################-
# Creación de gráfico de tiempo de duplicación vs día del año
pdf(file = file.path('Figuras',paste0(today(),"R2",".pdf")), 
    width = 5.5, height = 5);{
  data2 %>%
    ggplot(aes(x = firstd, y = thalf_mn)) +
    geom_errorbar(aes(ymin = thalf_mn - error,
                      ymax = thalf_mn + error)) +
    geom_point() +
    stat_smooth(method = 'loess', formula = y ~ x) +
    ylab('T. duplicación casos aparente (días)') +
    xlab('Día del año (detección primer caso)') +
    labs(title = 'Modelamiento tiempo de duplicación aparente por país', 
         subtitle = 'T half por regresión no lineal para cada país') + 
    coord_cartesian(ylim = c(0, 10)) +
    geom_point(data = data2 %>% filter(location == 'Colombia'),
               col = 'red') +
    geom_text_repel(
      aes(label = location),
      data = data2 %>% filter(location == 'Colombia'),
      box.padding = unit(0.45, "lines")
    )}; dev.off()

##########################################################################-
# Curva epidémica con logs de crecimiento ---------------------------------
##########################################################################-
# Fecha actualizacion de datos para gráficos
a <- paste0("Actualizado ", today(), "; ", "00:00 CET")

#' Función de adición de líneas guía de tiempo de duplicación
#' @param t tiempo de duplicación a graficar
#' @return lista con objeto stat_function que depende del eje x
#' @examples
#' ggplot() + ... + line_t(1)
#' 
line_t <- function(t) {
  list(stat_function(fun = function(x) {2 ^ (x / t)},
    inherit.aes = F, lty = 'dotted', colour = 'gray1'))
  }


##########################################################################-
# Gráfico en escala logarítmica con guías de tiempo de duplicación

# Vector de selección de países de Suramérica para comparación
# No ponemos Bolivia, Uruguay, Paraguay o Venezuela
c3 <- c(c2, 'Panama', 'Mexico')

##########################################################################-
# Creación de objeto G3 en escala logarítmica
G3 <- data1 %>%
  # Filtrar países de interés
  filter(location %in% c3) %>% 
  mutate(location = factor(location, levels = c3)) %>% 
  ggplot(aes(x = dd, y = total_cases, 
             group = location, colour = location)) +
  # Guías de tiempo de duplicación
  line_t(1) + line_t(2) + line_t(3) + line_t(7) + line_t(30) +
  geom_line() + 
  geom_point(data = . %>%
               group_by(location) %>%
               slice(which.max(dd))) + 
  scale_y_log10(breaks = 10^(1:4),
                labels = scales::trans_format("log10", scales::math_format(10^.x)) ) +
  aux_param("D") +
  coord_cartesian(xlim = c(0, 30), ylim = c(1,10000)) + 
  labs(title = 'Curvas epidémicas de COVID19 en países seleccionados', 
       subtitle = "Incluye casos relacionados, importados, y en estudio. Escala logarítmica / desde caso índice.", 
       caption = paste0("Estos números no dicen nada sobre el número de personas infectadas, sólo el número de personas quienes han sido positivas. \n",
                        "Este no es un gráfico oficial, sólo informativo. Datos tomados de: \n",
                        "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide \n",
                        a))+
  geom_dl(aes(label = location),
          method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8)) +
  annotation_logticks(sides = 'l') +
  theme(panel.grid = element_line(colour = NA), 
        legend.position = "none",
        plot.caption = element_text(hjust = 0))

##########################################################################-
# Adicionar leyendas en guías de duplicación de datos

legend_dup <- tribble(
  ~ x, ~ y, ~ label, ~ rot,
  8, 0.5 * 10 ^ 3, "Duplica diario", 60,
  25, 5 * 10 ^ 3, "Duplica cada 2d", 35,
  28, 3 * 10 ^ 2, "Duplica cada 3d", 0,
  28, 1 * 10 ^ 1, "Duplica cada 7d", 0,
  27, 1.5, "Duplica cada mes", 0)


# Creación de objeto G3 con leyendas en las guías de duplicación
G4 <- G3 + 
  geom_text(data = legend_dup,
            mapping = aes(x = x, y = y, label = label, angle = rot), 
            stat = "identity", position = "identity", 
            col = "black", inherit.aes = FALSE)

# Almacenamiento de objeto G4 en formato de *pdf* y *png*
ggsave(file.path('Figuras', paste0(today(), "R3", ".pdf")), G4, width = 8, 
       height = 6, device = 'pdf')

ggsave(file.path('Figuras', paste0(today(), "R3", ".png")), 
       G4, width = 8, height = 6, device = 'png', dpi = 300)


##########################################################################-
# Curva epidémica con logs de crecimiento (caso 100) ----------------------
##########################################################################-
#' Función de adición de líneas guía de tiempo de duplicación
#' @param t tiempo de duplicación a graficar
#' @return lista con objeto stat_function que depende del eje x, está 
#' multiplicado por 100 como corrección.
#' @examples
#' ggplot() + ... + line_t1(1)
#' 
line_t1 <- function(t) {
  list(stat_function(fun = function(x) {1E2*2 ^ ((x) / t)},
                     inherit.aes = F, lty = 'dotted', colour = 'gray1'))
}

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
  filter(total_cases >= 100) %>% 
  group_by(location) %>% 
  mutate(dd2 = difftime(date, min(date), units = "days") %>% 
           as.numeric(.)) %>% 
  filter(location %in% c3) %>% 
  ungroup(.) %>% 
  mutate(location = factor(location, levels = c3))
  
##########################################################################-
# Creación de gráfico G5 con escala logarítmica y corrección por días de 
# llegada a 100 casos.
  
G5 <- data3 %>% 
  ggplot(aes(x = dd2, y = total_cases, 
             group = location, colour = location)) +
  line_t1(1) + line_t1(2) + line_t1(3) + line_t1(7) + line_t1(30) +
  geom_line() + 
  # Seleccionar último punto
  geom_point(data = . %>% group_by(location) %>% slice(which.max(dd))) + 
  scale_y_log10(breaks = 10^(1:5),
                labels = scales::trans_format("log10", scales::math_format(10^.x)) ) +
  aux_param("D") +
  coord_cartesian(xlim = c(0, 10), ylim = c(1E2, 1E4)) + 
  labs(title = 'Curvas epidémicas de COVID19 en países seleccionados', 
       subtitle = "Incluye casos relacionados, importados, y en estudio. Escala logarítmica / desde caso 100.", 
       caption = paste0("Estos números no dicen nada sobre el número de personas infectadas, sólo el número de personas quienes han sido positivas. \n",
                        "Este no es un gráfico oficial, sólo informativo. Datos tomados de: \n",
                        "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide \n",
                        a))+
  geom_dl(aes(label = location),
          method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8)) +
  xlab('Días desde el caso 100') +
  annotation_logticks(sides = 'l') +
  theme(panel.grid = element_line(colour = NA), 
        legend.position = "none",
        plot.caption = element_text(hjust = 0))

##########################################################################-
# Adicionar leyendas en guías de duplicación de datos

legend_dup1 <- tribble(
  ~ x, ~ y, ~ label, ~ rot,
  5.0, 4 * 10 ^ 3, "Duplica diario", 46, 
  7.8, 2 * 10 ^ 3, "Duplica cada 2d", 30,
  8.5, 5 * 10 ^ 2, "Duplica cada 3d", 0,
  9.0, 2 * 10 ^ 2, "Duplica cada 7d", 0,
  9.0, 1 * 10 ^ 2, "Duplica cada mes", 0)

# Creación de objeto G3 con leyendas en las guías de duplicación
G6 <- G5 + 
  geom_text(data = legend_dup1,
            mapping = aes(x = x, y = y, label = label, angle = rot), 
            stat = "identity", position = "identity", 
            col = "black", inherit.aes = FALSE)


# Almacenamiento de objeto G4 en formato de *pdf* y *png*
ggsave(file.path('Figuras', paste0(today(), "R4", ".pdf")), G6, width = 8, 
       height = 6, device = 'pdf')

ggsave(file.path('Figuras', paste0(today(), "R4", ".png")), 
       G6, width = 8, height = 6, device = 'png', dpi = 300)

