##------------------------------------------------------------------------------#
## Nombre del Script: Ejercicio de 10-05-2020 sobre seguimiento a pruebas -------
## diagnósticas 
##  
## Propósito del Script: ejercicio de 10-05-2020 sobre seguimiento a pruebas 
## diagnósticas sugerido en Twitter.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  10-05-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(lubridate)
require(tidyverse)
require(directlabels)
require(patchwork)
require(gganimate)

#-------------------------------------------------------------------------------#
# Introducción -----------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Lectura de archivo
# Se descarga archivo de compilación por la colaboración Our World in Data (OWID) 
# con pruebas diagnósticas
data <- read_csv("Modulo/7_Seguimiento_Pruebas/data/raw-owid-covid-data.csv")

# Se carga una lista con países de interés para comparación, se crea una lista:
# "orig": nombres originales de países en el set de datos
# "trad": nombres traducidos a español (para presentación gráfica)

c1 <- list(
  orig = c("Colombia", "Chile", "Ecuador", "Brazil", "Argentina", "Peru", 
          "Bolivia", "Mexico", "Uruguay", "Paraguay", "Panama"),
  trad = c("Colombia", "Chile", "Ecuador", "Brasil", "Argentina", "Perú", 
           "Bolivia", "México", "Uruguay", "Paraguay", "Panamá")
)

#-------------------------------------------------------------------------------#
# Modificación de archivo de datos
#................................................................................
# 1 Selección de objeto *data*
# 2 Filtrar por locación, teniendo en cuenta nombres originales
# 3 Agrupar por locación
# 4 Calcular columna "dd" que representa la diferencia en días respecto al primer 
# día de reporte.
# 5 Calcular una variable que realize la correción de nombre de países colocando 
# tildes cuando no están.
# 6 Cálculo de estadística de test totales (acumulados a la fecha) por 1000 hab, 
# en lugar de la escala original.
#................................................................................

data1 <- data %>% 
  filter(location %in% c1$orig) %>% 
  group_by(location) %>% 
  mutate(dd  = difftime(date, min(date), units = "days"),
         loctra = case_when(
           location=="Mexico" ~ "México",
           location=="Peru" ~ "Perú",
           location=="Panama" ~ "Panamá",
           TRUE ~ location
         ),
         ttt_100000 = total_tests_per_thousand*1000) 


#-------------------------------------------------------------------------------#
# Creación de gráfico con casos totales por día de reporte ----------------------
#-------------------------------------------------------------------------------#
# Configuración de tema 
theme_set(theme_bw() + theme(
  panel.border = element_rect(fill = NA, colour = 'black')
))

# Posiciones de breaks de líneas en escala log para el eje Y
mb <- c(seq(1,9,1), seq(10,90,10), seq(100,900,100), 
        seq(1E3, 9E3, 1E3), seq(1E4, 9E4, 1E4))

# Gráfica de días desde el primer reporte vs N. de test por 1E5 habitantes, con 
# colores para cada día

p1 <- data1 %>% 
  ggplot(aes(dd,ttt_100000, 
             group = location, col = location)) + 
  geom_point(size = 1) + 
  geom_line() +
  xlab('Días desde el primer reporte') + 
  ylab('N. test por 100.000 hab.') + 
  geom_dl(aes(label = loctra), 
          method = list(dl.trans(x = x + 0.13), "last.points", cex = 0.8)) +
  theme(legend.position = "none") + 
  coord_cartesian(xlim = c(0,150)) +
  scale_y_log10(breaks = 10^(1:6), 
                minor_breaks = mb,
                labels = scales::trans_format("log10", scales::math_format(10^.x)) ) +
  annotation_logticks(sides = 'l') 

#-------------------------------------------------------------------------------#
# Creación de tabla complementaria con información de N. de test en escala normal
#................................................................................
# 1 Seleccionar objeto *data*
# 2 Eliminar casos de test sin reportes.
# 3 Seleccionar la última fila (por grupo heredado de data1)
# 4 Desagrupar
# 5 Seleccionar tres columnas de interés de presentación
# 6 Renombrar algunas columnas para mejorar la presentación gráfica
#................................................................................
  
data2 <- data1 %>%
  filter(!is.na(ttt_100000)) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  select(loctra, date,ttt_100000) %>% 
  rename(País = loctra, Fecha = date, "Test totales por 1E5 hab" = ttt_100000)

#-------------------------------------------------------------------------------#
# Creación de gráfico-tabla compuesto

p_tot <- (p1 + gridExtra::tableGrob(data2, rows = c(), 
                  theme = gridExtra::ttheme_minimal(base_size = 9))) +
  plot_annotation(
    title = 'Pruebas totales realizadas por 100.000 habitantes',
    subtitle = '',
    caption = "Adaptado de: Max Roser, Hannah Ritchie, Esteban Ortiz-Ospina and Joe Hasell (2020) - 'Coronavirus Pandemic (COVID-19)' \n. Publicado en OurWorldInData.org. Obtenido de: 'https://ourworldindata.org/coronavirus' [Online]"
  ) +
  plot_layout(widths = c(2,1.5))

# Almacenamiento de gráficos en PDF, y PNG
ggsave("Modulo/7_Seguimiento_Pruebas/figures/Rplot.pdf", p_tot, 
       width = 9, height = 6, device = 'pdf')

ggsave("Modulo/7_Seguimiento_Pruebas/figures/Rplot.png", p_tot, 
       width = 9, height = 6, device = 'png', 
       units = "in", dpi = 3000)


#-------------------------------------------------------------------------------#
# Creación de gráfico con N. de tests y consideración de reporte de fallecidos --
#-------------------------------------------------------------------------------#

Gmp <- data1 %>%  
  mutate(ttt_cor = 
           if_else(row_number()==1&is.na(ttt_100000), 0, ttt_100000),
         col.indic = if_else(is.na(ttt_100000), 0, 1) %>% factor()
         ) %>% 
  fill(ttt_cor, .direction = "down") %>% 
  mutate(total_deaths_per_1E5 = total_deaths_per_million/10,
         DD1 = round(as.double(dd))) %>% 
  # Grafico
  ggplot(aes(x = ttt_cor, y = total_deaths_per_1E5, 
           col = loctra)) + 
  geom_point(aes(shape = col.indic, group = seq_along(DD1))) + 
  geom_line() + 
  scale_shape_manual(breaks = c('0', '1'), values = c(NA, 16)) +
  scale_color_viridis_d() +
  scale_y_continuous(breaks = seq(0,10,2)) + 
  scale_x_continuous(breaks = seq(0, 2E4, 5E3)) +
  coord_cartesian(xlim=c(0,2E4)) + 
  theme(legend.position = "none") +
  geom_dl(aes(label = loctra), 
          method = list(dl.trans(x = x + 0.13), "last.points", cex = 0.8)) +
  xlab('N. test por 100.000 habitantes') + 
  ylab('N. fallecimientos por 100.000 habitantes') + 
  labs(title = 'Pruebas totales realizadas vs fallecimientos por 100.000 hab',
       subtitle = paste0('Días desde primer reporte: ', "{round(as.double(frame_along))}"),
       caption = "Adaptado de: Max Roser, Hannah Ritchie, Esteban Ortiz-Ospina and Joe Hasell (2020) - 'Coronavirus Pandemic (COVID-19)' \n. Publicado en OurWorldInData.org. Obtenido de: 'https://ourworldindata.org/coronavirus' [Online]")

Gmp_anim <- Gmp + 
  transition_reveal(DD1)

animate(Gmp_anim, end_pause = 10, rewind = TRUE, 
        height = 680, width = 800, res = 140, duration = 30) 

anim_save(paste('Figuras', paste0(today(), "R5", ".gif")))














