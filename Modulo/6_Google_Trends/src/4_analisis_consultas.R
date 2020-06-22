##------------------------------------------------------------------------------#
## Nombre del Script: Análisis de datos de consultas de Google -----------------
##  
## Propósito del Script: realizar un análisis de consultas de Google
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
library(wordcloud)

# Lectura de datos
data <- read_csv(
  file = glue::glue("Modulo/6_Google_Trends/data/queries_{today()}.csv"),
  locale = locale(encoding = "latin1", asciify = TRUE)
)

data1 <- read_csv(
  file = glue::glue("Modulo/6_Google_Trends/data/queries_COL_{today()}.csv"),
  locale = locale(encoding = "latin1", asciify = TRUE)
)

#-------------------------------------------------------------------------------#
# Modificación de tabla "data"
#................................................................................
# 1 Selección de búsquedas por top
# 2 Conversión de columna "subject" a números
# 3 Calculo de frecuencia relativa con acumulado de hits por subject
# 4 Eliminación de medicamentos que no son de interés
#................................................................................

datat <- data %>%
  filter(related_queries == 'top') %>%
  mutate(subject  = as.double(subject),
         rel_frec = subject * hits_t) %>%
  filter(!(keyword1 %in% c(
    'Colchicina',
    'Dapagliflozin',
    'Lenzilumab',
    'Sarilumab',
    'Cloro OR MMS OR DiÃ³xido de cloro'
  )))

# datat %>% view()

datat1 <- data1 %>%
  filter(related_queries == 'top') %>%
  mutate(subject  = as.double(subject),
         rel_frec = subject * hits_t) %>%
  filter(!(keyword1 %in% c(
    'Colchicina',
    'Dapagliflozin',
    'Lenzilumab',
    'Sarilumab',
    'Cloro OR MMS OR DiÃ³xido de cloro'
  )))


#-------------------------------------------------------------------------------#
# Creación de nube de palabras 1 ------------------------------------------------
#-------------------------------------------------------------------------------#
#................................................................................
#   Nubes de palabras con consultas frecuentes en relación a los medicamentos
#................................................................................

set.seed(1234)

pdf(glue::glue("Modulo/6_Google_Trends/figures/WC_{today()}.pdf"),
    width = 8, height = 6)
wordcloud(words = datat$value, freq = datat$rel_frec, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()


pdf(glue::glue("Modulo/6_Google_Trends/figures/WC_COL_{today()}.pdf"),
    width = 8, height = 6)
wordcloud(words = datat1$value, freq = datat1$rel_frec, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

data %>% 
  filter(related_queries == 'rising') %>% 
  rownames_to_column(var = 'ID') %>% 
  filter(!(keyword1 %in% c(
    'Colchicina',
    'Dapagliflozin',
    'Lenzilumab',
    'Sarilumab',
    'Cloro OR MMS OR DiÃ³xido de cloro'
  ))) %>% 
  mutate(ID = as.double(ID), 
         hits_f = hits_t*ID/sum(hits_t*ID)) %>%
  select(value, hits_f) %$% 
  wordcloud(words = .$value, freq = .$hits_f, min.freq = 1E-8,
            max.words=500, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
