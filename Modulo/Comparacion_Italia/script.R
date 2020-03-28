##------------------------------------------------------------------------#
## Nombre del Script: Comparación demografía de Colombia y locaciones de 
##  referencia ----------------------------------------------------
##  
## Propósito del Script: comparación de demografía por medio de pirámide 
##  poblacional entre Colombia y otros países.
##  
## Autor: Daniel S. Parra Gonzalez 
## Fecha de creacion: 26-03-2020 
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
# Carga de paquetes
require(tidyverse)
require(scales)
require(readxl)

##########################################################################-
# Introducción ------------------------------------------------------------
##########################################################################-
# Selección de directorio principal
setwd(file.path('F:', 'Documentos', 'Estudio - Documentos', 'Estadistica_2013-2', 
                'Regresion', '2_Regresión Nolineal', 'Casos', 
                'Analisis_Datos_Coronavirus', 'Modulo', 'Comparacion_Italia'))

# Carga de datos demográficos de Italia
DATA_IT <- read_csv("DATA/DCIS_POPRES1_25032020220746738.csv")
# I.Stat - Instituto Nazionale di Statistica
# http://dati.istat.it/Index.aspx?QueryId=18462#

# Carga de datos demográficos de Colombia
DATA_CO <- read_excel(file.path('DATA', 
      'anexos-proyecciones-pob-dptos-area-grupos-de-edad-2018-2023.xlsx'),
                   sheet = "PPO_GQEdad_DPTO", skip = 8, 
                   n_max = 23)

##########################################################################-
# Modificación de tablas de datos -----------------------------------------
##########################################################################-
# Vector de datos de rango de edad
edad_br_car <- c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                 "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                 "90-94", "95-99", "+100")

##########################################################################-
# Modificación de datos de Colombia
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Selección de archivo de datos de Colombia, eliminación de filas 1 y 2, 
##  selección de columnas c(2, 7, 8).
##  2 Renombrar variable a Edad
##  3 Renombrar variable a Hombre
##  4 Renombrar variable a Mujeres
##  5 Colapsar tabla de datos con *gather*
##  6 Cambiar Edad_break sólo en '100 AÑOS Y MÁS' que cambia a '+100', el 
##  resto igual.
##  7 Cambiar Edad_break a factor ordenado
##  8 Cambiar sexo a una variable numérica con valores 1 ("Hombre") o 2 
##  ("Mujeres).
##  9 Cambiar sexo a un factor ordenado
##  10  Calcular la proporción (prop_Valor) con el número de personas por 
##  grupos (Valor).
##  11  Agrupar por *Edad_break* y *Sexo*
##  12  Resumir por la suma de prop_Valor en cada subgrupo
##  13  Adicionar columna que indica la Locación
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

DATA_CO_1 <- DATA_CO[-c(1:2), c(2, 7:8)] %>%
  rename('Edad' = `Grupos de edad`) %>%
  rename('Hombre' = 'Hombres...7') %>%
  rename('Mujeres' = 'Mujeres...8') %>%
  gather('Hombre', 'Mujeres', key = 'Sexo', value = 'Valor') %>%
  mutate(
    Edad_break = if_else(Edad == "100 AÑOS Y MÁS", '+100', Edad), 
    Edad_break = factor(Edad_break, levels = edad_br_car),
    Sexo = if_else(Sexo == "Hombre", 1, 2),
    Sexo = factor(Sexo, levels = c(1, 2)),
    prop_Valor = Valor / sum(Valor)
  ) %>% 
  group_by(Edad_break, Sexo) %>% 
  summarise(Val = sum(prop_Valor)) %>% 
  add_column(Locacion = 'Colombia', .before = 'Edad_break')

##########################################################################-
# Modificación de datos en Italia
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Selección de tabla de datos *DATA_IT*
##  2 Filtrar por el Territorio de Lombardía
##  3 Filtrar sólo fecha 2019
##  4 Filtrar edad no puede ser "totales"
##  5 Separar la variable Età en el número de edad y la unidad
##  6 Filtrar por estado civil total
##  7 Filtrar sexo, eliminando "totale"
##  8 Cambiar SEXISTAT1 por una variable factor
##  9 Renombrar variable SEXISTAT1 por Sexo
##  10 Renombrar Value por Valor
##  11 Calcular Valor por prop_Valor
##  12 Calcular variable de edad agrupada por quinquenios
##  13 Agrupar por *Edad_break* y *Sexo*
##  14 Resumir por *Val* suma de proporción
##  15 Adicionar columna que indica la Locación
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

DATA_IT_1 <- DATA_IT %>% 
  filter(Territorio == 'Lombardia') %>% 
  filter(TIME == 2019) %>%
  filter(Età != "totale") %>%
  separate(Età, into = c('Edad', 'Unidad'), sep = "(?<=\\d{1,3})\\s", 
           convert = TRUE) %>% 
  filter(`Stato civile` == 'totale') %>%   
  filter(Sesso != 'totale') %>% 
  mutate(SEXISTAT1 = factor(SEXISTAT1, levels = c(1,2))) %>% 
  rename(Sexo = SEXISTAT1) %>% 
  rename(Valor = Value) %>% 
  mutate(prop_Valor = Valor/sum(Valor)) %>% 
  mutate(
    Edad_break = case_when(
      Edad >= 00 & Edad < 05 ~ '00-04',
      Edad >= 05 & Edad < 10 ~ '05-09',
      Edad >= 10 & Edad < 15 ~ '10-14',
      Edad >= 15 & Edad < 20 ~ '15-19',
      Edad >= 20 & Edad < 25 ~ '20-24',
      Edad >= 25 & Edad < 30 ~ '25-29',
      Edad >= 30 & Edad < 35 ~ '30-34',
      Edad >= 35 & Edad < 40 ~ '35-39',
      Edad >= 40 & Edad < 45 ~ '40-44',
      Edad >= 45 & Edad < 50 ~ '45-49',
      Edad >= 50 & Edad < 55 ~ '50-54',
      Edad >= 55 & Edad < 60 ~ '55-59',
      Edad >= 60 & Edad < 65 ~ '60-64',
      Edad >= 65 & Edad < 70 ~ '65-69',
      Edad >= 70 & Edad < 75 ~ '70-74',
      Edad >= 75 & Edad < 80 ~ '75-79',
      Edad >= 80 & Edad < 85 ~ '80-84',
      Edad >= 85 & Edad < 90 ~ '85-89',
      Edad >= 90 & Edad < 95 ~ '90-94',
      Edad >= 95 & Edad < 100 ~ '95-99',
      Edad >= 100  ~ '+100'
    ),
    Edad_break = factor(Edad_break, levels = edad_br_car)) %>%  
  group_by(Edad_break, Sexo) %>% 
  summarise(Val = sum(prop_Valor)) %>% 
  add_column(Locacion = 'Lombardia', .before = 'Edad_break')
  
##########################################################################-
# Unir tablas DATA_IT_1 y DATA_CO_1

DATATOT <- DATA_IT_1 %>% 
  bind_rows(DATA_CO_1)

##########################################################################-
# Pirámide poblacional (gráfico) ------------------------------------------
##########################################################################-
# Colores por sexo
col_plot <- c(Masculino = 'blue1', 
              Femenino = 'red1')

# Pirámide poblacional

DATATOT %>%
  ggplot(aes(x = Edad_break)) +
  geom_bar(data = subset(DATATOT, Sexo == 2), 
           stat = "identity", alpha = 0.5, 
           aes(y = Val, fill = 'Femenino')) +
  
  geom_bar(data = subset(DATATOT, Sexo == 1), 
           stat = "identity", alpha = 0.5,
           aes(y = -Val, fill = 'Masculino')) +
  scale_y_continuous(breaks = seq(-1, 1, 0.02),
                     labels = round(abs(seq(-1, 1, 0.02)), 3)) +
  theme_bw() + coord_flip() +
  facet_wrap(. ~ Locacion) +
  xlab('Proporción') + ylab('Rango Edad') + 
  scale_fill_manual(values = col_plot, name = 'Sexo') +
  labs(title = 'Comparación demografía Colombia - Lombardía',
       subtitle = 'Pirámide poblacional',
       caption = 'Fuentes: Departamento Administrativo Nacional de Estadística; I.Stat - Instituto Nazionale di Statistica')