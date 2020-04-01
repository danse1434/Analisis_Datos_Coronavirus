##------------------------------------------------------------------------#
## Nombre del Script: Modelamiento de propagación COVID-19 Colombia  ------
##  
## Propósito del Script: evaluar un modelo SEIR con especificación de 
##  comportamiento.   
##  
## Autor: Daniel S. Parra Gonzalez 
## Fecha de creacion: 28-03-2020  
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
# Carga de paquetes
require(tidyverse)
require(lubridate)
require(deSolve)

##########################################################################-
# Introducción ------------------------------------------------------------
##########################################################################-
# Lectura de archivo de casos reportados
# Ir a página https://www.ins.gov.co/Noticias/Paginas/Coronavirus.aspx
# Descargar archivo de datos actualizado y guardar en la carpeta 
# ./Modulo/Predicciones/data

epidCol <- read_csv("Modulo/Predicciones/data/Casos1.csv") %>% 
  rename(FD = `Fecha de diagnóstico`,
         Tipo = `Tipo*`) %>% 
  mutate(FD = dmy(FD)) %>% 
  add_column(Caso_Nuevo = 1) %>% 
  mutate(Casos_Totales = cumsum(Caso_Nuevo))

##########################################################################-
# Subestudio de casos importados
epidCol_I <- epidCol %>%
  filter(Tipo == 'Importado') %>%
  group_by(FD) %>%
  summarise(Conteo = n()) %>% 
  mutate(Casos_Totales = cumsum(Conteo),
         dd = difftime(FD, min(FD), units = "days") %>% as.numeric(.)) 


epidCol_I %>% 
  ggplot(aes(x = dd, y = Casos_Totales)) +
  geom_point() +
  theme_bw() +
  stat_function(fun = ~exp(0.2842 * .x), geom = 'line') +
  xlab('Fecha de Reporte') + ylab('Casos de infección documentados')  +
  labs(title = 'Reporte de casos importados - Colombia', 
       subtitle = 'Tomado de INS - MinSalud') 


nls(Casos_Totales ~ exp(k * dd),
    data = epidCol_I,
    start = list(k = 1))
  




Z = c(S = 45E6, E = 0, I = 1, R = 0)
p = c(beta = 1, sigma = 1/7, gamma = 1/5)


SEIH = function(t, Z, p){
  S = Z[1]; E = Z[2]; I = Z[3]; R = Z[4]; N = sum(Z)
  
  sigma = p["sigma"]; gamma = p["gamma"]
  
  N = S + E + I + R
  
  if (t < 20) {
    beta <- p["beta"]
  } else if (t >= 20) {
    beta <- p["beta"] * (1 - 0.2) * (1 - 0.05 * I / N) ^ 100
  }
  
  dS = -(beta * S * I) / N
  dE = (beta * S * I) / N - (sigma * E)
  dI = (sigma * E) - (gamma * I)
  dR = gamma * I
  dZ = c(dS, dE, dI, dR)
  list(dZ)}


times = seq(0, 150, by = .1)

resol = ode(y = Z,
            times = times,
            func = SEIH,
            parms = p
            )


##########################################################################-
# 
col_vec <- c('S' = 'green3', 'E' = 'yellow4', 'I' = 'red', 'R' = 'blue4')

resol %>% 
  as_tibble(.) %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = S, col = 'S')) +
  geom_line(aes(y = E, col = 'E')) +
  geom_line(aes(y = I, col = 'I')) +
  geom_line(aes(y = R, col = 'R')) +
  theme_classic() +
  scale_color_manual(values = col_vec, name = 'Tipo') +
  xlab('Tiempo') + ylab('Fracción')







