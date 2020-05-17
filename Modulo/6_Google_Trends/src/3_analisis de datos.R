##------------------------------------------------------------------------------#
## Nombre del Script: Análisis de datos de tendencias de Google -----------------
##  
## Propósito del Script: realizar un análisis de tendencias de Google
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 16-05-2020 
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
library(tidyverse)
library(lubridate)
library(gganimate)
library(directlabels)

#-------------------------------------------------------------------------------#
# Lectura de datos --------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Carga de datos de tendencia mundiales

data <- read_csv(
  file = glue::glue("Modulo/6_Google_Trends/data/google_{today()}.csv"),
  locale = locale(),
  col_types = cols(date = col_datetime())
)

# Carga de datos de tendencias para Colombia

dataCOL <- read_csv(
  file = glue::glue("Modulo/6_Google_Trends/data/google_COL_{today()}.csv"),
  locale = locale(),
  col_types = cols(date = col_datetime())
)

#-------------------------------------------------------------------------------#
# Modificación de archivo *data*
#................................................................................
# 1 Apertura de *data*
# 2 Agrupar por 'geo', 'gprop', 'keyword1', 'date'
# 3 Resumir por suma de hits en cada grupo - esto permite agregar por 'keyword1'
# 4 Desagrupar
# 5 Convertir "date" desde Date.Time a Date
# 6 Eliminar el 'Cloro' de "keyword1"
#................................................................................

data1 <- data %>% 
  group_by(geo, gprop, keyword1, date) %>% 
  summarise(hits = sum(hits)) %>% 
  ungroup() %>% 
  mutate(date = as.Date(date, "%Y-%m-%d", tz = 'America/Bogota'))


data1 <- data1 %>% 
  filter(keyword1 != 'Cloro OR MMS OR Dióxido de cloro')

#-------------------------------------------------------------------------------#
# Selección de medicamentos específicos
#................................................................................
# 1 Selección de data1
# 2 Eliminación de nombres de keyword1 con acentos.
# 3 Eliminación de algunos nombres de medicamentos no necesitados
#................................................................................

data2 <- data1 %>% 
  mutate(keyword1 = case_when(
    keyword1 == "Interferón" ~ 'Interferon', 
    TRUE ~ keyword1)) %>% 
  filter(!(
    keyword1 %in% c('Colchicina', 'Dapagliflozin', 'Lenzilumab', 'Sarilumab')
  ))

#-------------------------------------------------------------------------------#
# Gráfica 1 - Totales -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Configuración de tema
theme_set(theme_bw() + 
            theme(panel.border = element_rect(fill = NA, colour = 'black'),
                  legend.position = "none"))

# Configuración de caption
cpt <- glue::glue(
  "Fuente: https://trends.google.com/trends/?geo=US",
  "{today()}. \n Hecho por: Daniel Parra")

# Gráfico 1 con todos
G0 <- data2 %>% 
  ggplot(aes(x = date, y = hits, col = keyword1)) +
  geom_line() +
  xlab("Fecha") + ylab("Hits") +
  labs(title = 'Tendencias Mundial Google', 
       subtitle = "Medicamentos 'candidatos' en tratamiento de COVID-19", 
       caption = cpt)

G1 <- G0 + geom_dl(
    aes(label = keyword1),
    method = list(dl.trans(x = x * 1.05), "last.points", cex = 0.8)
  ) + 
  coord_cartesian(xlim = c(as.Date("2020-02-15"), as.Date("2020-07-01")))

# Almacenamiento de gráfico
ggsave(G1, 
  filename = glue::glue("Modulo/6_Google_Trends/figures/G1_{today()}.pdf") %>% 
         as.character(),
       device = 'pdf', width = 8, height = 6)

#-------------------------------------------------------------------------------#
# Gráfico 2 con facets ----------------------------------------------------------
#-------------------------------------------------------------------------------#

G2 <- G0 +
  facet_wrap(. ~ keyword1) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    date_minor_breaks = "1 week"
  ) +
  coord_cartesian(xlim = c(as.Date("2020-02-15"), today())) +
  theme(
    panel.grid.major = element_line(colour = 'gray90'),
    panel.grid.minor = element_line(colour = 'gray98')
  )

# Almacenamiento de gráfico
ggsave(G2, 
       filename = glue::glue("Modulo/6_Google_Trends/figures/G2_{today()}.pdf") %>% 
         as.character(),
       device = 'pdf', width = 8, height = 6)


G1_anim <- G1 +
  transition_reveal(date) +
  ease_aes(default = "linear")


#-------------------------------------------------------------------------------#
# animate(plot = G1_anim, nframes = 100, fps = 10, duration = 40, detail = 1, 
#   device = 'png', ref_frame = 1, start_pause = 0, end_pause = 10,  rewind = TRUE, 
#   height = 680, width = 1000, res = 200) 
# 
# anim_save(file.path('Modulo', '6_Google_Trends', 'figures', 
#                     paste0('G1_anim', today(), ".gif")))
 

#-------------------------------------------------------------------------------#
# Gráfica 3 - Facets para Colombia ----------------------------------------------
#-------------------------------------------------------------------------------#
dataCOL1 <- dataCOL %>%
  group_by(geo, gprop, keyword1, date) %>%
  summarise(hits = sum(hits)) %>%
  ungroup() %>%
  mutate(date = as.Date(date, "%Y-%m-%d", tz = 'America/Bogota')) %>%
  filter(keyword1 != 'Cloro OR MMS OR Dióxido de cloro') %>%
  mutate(keyword1 = case_when(keyword1 == "Interferón" ~ 'Interferon',
                              TRUE ~ keyword1)) %>%
  filter(!(
    keyword1 %in% c('Colchicina', 'Dapagliflozin', 'Lenzilumab', 'Sarilumab')
  ))

G1_COL <- dataCOL1 %>% 
  ggplot(aes(x = date, y = hits, col = keyword1)) +
  geom_line() +
  xlab("Fecha") + ylab("Hits") +
  labs(title = 'Tendencias en Google para Colombia', 
       subtitle = "Medicamentos 'candidatos' en tratamiento de COVID-19", 
       caption = cpt) + 
  facet_wrap(. ~ keyword1) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    date_minor_breaks = "1 week"
  ) +
  coord_cartesian(xlim = c(as.Date("2020-02-15"), today())) +
  theme(
    panel.grid.major = element_line(colour = 'gray90'),
    panel.grid.minor = element_line(colour = 'gray98')
  )

# Almacenamiento de gráfico
ggsave(G1_COL, 
       filename = glue::glue("Modulo/6_Google_Trends/figures/G1_COL_{today()}.pdf") %>% 
         as.character(),
       device = 'pdf', width = 8, height = 6)
