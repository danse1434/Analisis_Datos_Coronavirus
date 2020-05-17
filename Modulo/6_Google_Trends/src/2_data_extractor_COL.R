##------------------------------------------------------------------------------#
## Nombre del Script: Extractor de datos de Google Trends -----------------------
##  
## Propósito del Script: Realizar una extracción de datos de Google Trends como 
## herramienta para conocer el interés de la población general en ciertas palabras 
## clave a través del tiempo. 
## Para esto se utiliza el paquete "gtrendsR" de R, que permite extraer la tendencia 
## de 5 palabras clave escogida. 
##  
## SE EXTRAEN DATOS ESPECÍFICOS DE COLOMBIA
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 16-05-2020 
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Apertura de paquetes
library(gtrendsR)
library(tidyverse)
library(directlabels)

#-------------------------------------------------------------------------------#
# Especificación de Query al servidor -------------------------------------------
#-------------------------------------------------------------------------------#
#................................................................................
#   1 Especificación de palabras clave para el servidor, se escoje una serie de 
#   medicamentos y sus nombres en inglés en "palab.clave".
#   2 Especificación de las áreas geográficas de interés "paises"
#   3 Especificación de rango de tiempo de interés "tiempo": 
#   conversión de objeto "glue" en "carácter" - este actualiza la fecha de búsqueda
#   4 Especificación de canales de interés "canales".
#................................................................................
palab.clave = 
  c("Cloroquina", "Chloroquine", 
    "Hidroxicloroquina", "Hydroxychloroquine",
    "Azithromycin", "Azitromicina",
    "Colchicine", "Colchicina",
    "Ivermectin", "Ivermectina",
    "Favipiravir", "Remdesivir", "Sarilumab", "Tocilizumab", "Lenzilumab",
    "Dapagliflozin",
    "Interferon", "Interferón",
    "Lopinavir/ritonavir", "Ritonavir",
    "Lopinavir",
    "MMS", "Dióxido de cloro", "Chlorine dioxide", "Chlorine",
    "Cloro",
    "Vaccine", "Vacuna", "Vacunas")

paises = c('CO')

tiempo = glue::glue("2020-01-01 {today()}") %>% as.character()
# Canales
canales='web'

#-------------------------------------------------------------------------------#
# Almacenamiento de tablas de consulta en lista ---------------------------------
#-------------------------------------------------------------------------------#
#................................................................................
#   1.  Creación de lista *tend*
#   2.  Realización de consultas y almacenamientos, sólo se pueden 5 palabras clave 
#   a la vez, se ocupa un elemento de la lista por consulta.
#................................................................................

tend <- list()

tend[[1]] <- gtrendsR::gtrends(palab.clave[1:5], gprop = canales, 
                               geo = paises, time = tiempo)

tend[[2]] <- gtrendsR::gtrends(palab.clave[6:10], gprop = canales, 
                               geo = paises, time = tiempo)

tend[[3]] <- gtrendsR::gtrends(palab.clave[11:15], gprop = canales, 
                               geo = paises, time = tiempo)

tend[[4]] <- gtrendsR::gtrends(palab.clave[16:20], gprop = canales, 
                               geo = paises, time = tiempo)

tend[[5]] <- gtrendsR::gtrends(palab.clave[21:25], gprop = canales, 
                               geo = paises, time = tiempo)

tend[[6]] <- gtrendsR::gtrends(palab.clave[26:29], gprop = canales, 
                               geo = paises, time = tiempo)

#-------------------------------------------------------------------------------#
# Modificación a data.frame -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Se debe realizar una modificación de los objetos a un df.
#................................................................................
#   1 Extracción de tabla de "interés a través del tiempo". 
#   2 Conversión a tibble
#   3 Conversión de tabla hits a caracteres para permitir unión
#   4 Unión de todas las tablas
#   5 Creación de columna "keyword1" con nombres estandarizados de medicamentos 
#   para análisis.
#   6 Conversión de "keyword1" en factor
#   7 Conversión de hits a una variable numérica, si se reporta como <1, se toma 
#   como 0.5.
#................................................................................

tend1 <- tend %>% 
  map( ~ magrittr::use_series(.x, "interest_over_time")) %>% 
  map( ~ as_tibble(.x)) %>% 
  map( ~ mutate(.x, hits = as.character(hits))) %>% 
  map_dfr( ~ .x) %>% 
  mutate(
    keyword1 = case_when(
      keyword %in% c("Cloroquina", "Chloroquine") ~ "Cloroquina",
      keyword %in% c("Hidroxicloroquina", "Hydroxychloroquine") ~ "Hidroxicloroquina",
      keyword %in% c("Azithromycin", "Azitromicina") ~ "Azitromicina",
      keyword %in% c("Colchicine", "Colchicina") ~ "Colchicina",
      keyword %in% c("Ivermectin", "Ivermectina") ~ "Ivermectina",
      keyword %in% c("Favipiravir") ~ "Favipiravir",
      keyword %in% c("Remdesivir") ~ "Remdesivir",
      keyword %in% c("Sarilumab") ~ "Sarilumab",
      keyword %in% c("Tocilizumab") ~ "Tocilizumab",
      keyword %in% c("Lenzilumab") ~ "Lenzilumab",
      keyword %in% c("Dapagliflozin") ~ "Dapagliflozin",
      keyword %in% c("Interferon", "Interferón") ~ "Interferón",
      keyword %in% c("Lopinavir/ritonavir", "Ritonavir",
                     "Lopinavir") ~ "Lopinavir OR Ritonavir",
      keyword %in% c(
        "MMS",
        "Dióxido de cloro",
        "Chlorine dioxide",
        "Chlorine",
        "Cloro"
      ) ~ "Cloro OR MMS OR Dióxido de cloro",
      keyword %in% c("Vaccine", "Vacuna", "Vacunas") ~ "Vacuna"),
    keyword1 = factor(keyword1),
    hits = if_else(hits == '<1', '0.5', hits),
    hits = as.double(hits)
  )

# Almacenamiento en *.csv
write_csv(
  x = tend1, 
  path = glue::glue("Modulo/6_Google_Trends/data/google_COL_{today()}.csv")
)

#-------------------------------------------------------------------------------#
# Consultas relacionadas -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Es de utilidad conocer la frecuencia de las consultas realizadas en el buscador
# para los diferentes medicamentos
#................................................................................
# 1 Extracción de tabla de "consultas relacionadas". 
# 2 Conversión a tibble
# 3 Unión de todas las tablas
# 4 Creación de columna "keyword1" con nombres estandarizados de medicamentos 
# para análisis.
# 5 Conversión de "keyword1" en factor
#................................................................................

tend2 <- tend %>% 
  map( ~ magrittr::use_series(.x, "related_queries")) %>% 
  map( ~ as_tibble(.x)) %>% 
  map_dfr( ~ .x) %>% 
  mutate(
    keyword1 = case_when(
      keyword %in% c("Cloroquina", "Chloroquine") ~ "Cloroquina",
      keyword %in% c("Hidroxicloroquina", "Hydroxychloroquine") ~ "Hidroxicloroquina",
      keyword %in% c("Azithromycin", "Azitromicina") ~ "Azitromicina",
      keyword %in% c("Colchicine", "Colchicina") ~ "Colchicina",
      keyword %in% c("Ivermectin", "Ivermectina") ~ "Ivermectina",
      keyword %in% c("Favipiravir") ~ "Favipiravir",
      keyword %in% c("Remdesivir") ~ "Remdesivir",
      keyword %in% c("Sarilumab") ~ "Sarilumab",
      keyword %in% c("Tocilizumab") ~ "Tocilizumab",
      keyword %in% c("Lenzilumab") ~ "Lenzilumab",
      keyword %in% c("Dapagliflozin") ~ "Dapagliflozin",
      keyword %in% c("Interferon", "Interferón") ~ "Interferón",
      keyword %in% c("Lopinavir/ritonavir", "Ritonavir",
                     "Lopinavir") ~ "Lopinavir OR Ritonavir",
      keyword %in% c(
        "MMS",
        "Dióxido de cloro",
        "Chlorine dioxide",
        "Chlorine",
        "Cloro"
      ) ~ "Cloro OR MMS OR Dióxido de cloro",
      keyword %in% c("Vaccine", "Vacuna", "Vacunas") ~ "Vacuna"),
    keyword1 = factor(keyword1)
  )

#-------------------------------------------------------------------------------#
# La columna "subject" muestra la aparición relativa de cada consulta. La 
# puntuación se basa en una escala relativa en la que 100 es la consulta más 
# buscada, 50 es una consulta que se busca con la mitad de frecuencia, etc.

tend1_sum <- tend1 %>% 
  group_by(keyword1) %>% 
  summarise(hits_t = sum(hits))

#-------------------------------------------------------------------------------#
# Adición de una columna que indica la frec. relativa de cada consulta

tend2 <- tend2 %>% 
  left_join(tend1_sum, by = 'keyword1') 

# Almacenamiento en *.csv
write_csv(
  x = tend2, 
  path = glue::glue("Modulo/6_Google_Trends/data/queries_COL_{today()}.csv")
)
