##------------------------------------------------------------------------------#
## Nombre del Script: Simular el comportamiento de ivermectina en el organismo----
## humano basado en modelos de farmacocinética poblacional descritos en bibliografía. 
##  
## Propósito del Script: obtener gráficas que muestren el comportamiento del fármaco 
## ivermectina en dosis habituales. Se utilizan dos modelos: 
## 
## 1. Duthaler U, Suenderhauf C, Karlsson MO, Hussner J, Meyer zu Schwabedissen H, 
## Krähenbühl S, et al. Population pharmacokinetics of oral ivermectin in venous 
## plasma and dried blood spots in healthy volunteers. Br J Clin Pharmacol. 2019; 
## 85(3):626–33. y 
## 
## 2. El-Tahtawy A, Glue P, Andrews EN, Mardekian J, Amsden GW, Knirsch CA. The 
## effect of azithromycin on ivermectin pharmacokinetics - A population 
## pharmacokinetic model analysis. PLoS Negl Trop Dis. 2008;2(5). 
## 
## Autor: Daniel S. Parra González 
## Fecha de creación: 10 -04 - 2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(tidyverse)
require(mlxR)

# Selección de directorio principal
# setwd("C:/Users/Daniel/OneDrive/Documents/Proyecto_Ivermectina/modulos")

#-------------------------------------------------------------------------------#
# Modelo 1  ---------------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Definición de componentes del modelo
#................................................................................
#   Definición de parámetros *p1* (e. fijos) y *p2* (e. aleatorios)
#   Definición de observaciones *Cc* 
#   Definición de regimen de administración *adm*
#   Definición de grupos *g*
#................................................................................

p1a  <- c(V1_pop = 89.13, V2_pop = 234.30, Cl_pop = 7.67, Q_pop = 19.0, 
         Ktr = (6 + 1) / 1.05, Mtt_pop = 1.05, ka = 0.55)
p2a  <- c(omega_V1 = 0.10, omega_V2 = 0.20, omega_Cl = 0.25, omega_Q = 0.41, 
         omega_Mtt = 0.60)
Cca  <- list(name = 'Cc', time = seq(0, 80, 0.1))
adm1 <- list(time = seq(0, 24 * 5, length.out = 16), 
             amount = c(600, rep(400, 15)))
ga   <- list(size = 1000, level = 'individual')

# Módulo de Simulación
resa <- simulx(model     = 'modulos/Modelo_PK/modelo_1.mlxtran',
               parameter = c(p1a, p2a),
               output    = Cca,
               treatment = adm1,
               group     = ga)

#-------------------------------------------------------------------------------#
# Modelo 2 ----------------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Definición de componentes del modelo
#................................................................................
#   Definición de parámetros *p1* (e. fijos) y *p2* (e. aleatorios)
#   Definición de observaciones *Cc* 
#   Definición de regimen de administración *adm*
#   Definición de grupos *g*
#................................................................................
p1b  <- c(V1_pop = 195, V2 = 882, Cl_pop = 11.8, Q = 18.9, 
         ka = 0.24, p_pop = 1.00)
p2b  <- c(omega_V1 = sqrt(0.063), omega_Cl = +sqrt(0.023),
          omega_p  = sqrt(0.061), r_ClV    = -sqrt(0.011))
Ccb  <- list(name = 'Cc', time = seq(0, 80, 0.1))
adm2 <- list(time = seq(0, 24 * 5, length.out = 16), 
             amount = c(600, rep(400, 15)))
gb   <- list(size = 1000, level = 'individual')

# Módulo de Simulación
resb <- simulx(model     = 'modulos/Modelo_PK/modelo_2.mlxtran',
               parameter = c(p1b, p2b), 
               output    = Ccb, 
               treatment = adm2, 
               group     = gb)

#-------------------------------------------------------------------------------#
# Manipulación de resultados PK -------------------------------------------------
#-------------------------------------------------------------------------------#
# Obtención de percentiles para perfiles plasmáticos
#................................................................................
#   1 Selección de vector 'Cc' conc. plasmática; cálculo en ng/mL (mcg/L)
#   2 Cálculo de percentiles 10:90 (*dfy_[ab]*)
#   3 Convertir en tibble *DFY[a,b]*
#   4 Eliminar la columna de percentil 50% repetido
#   5 Adicionar una columna que indique el modelo generador de los datos
#   6 Unir tablas *DFYa* y *DFYb* por filas para generar *DFY*
#................................................................................

dfy_a <- resa$Cc %>% mutate(y1 = Cc * 1000)
dfy_b <- resb$Cc %>% mutate(y1 = Cc * 1000)

dfy_a <- prctilemlx(dfy_a, plot = FALSE)
dfy_b <- prctilemlx(dfy_b, plot = FALSE) 


DFYa <- dfy_a[['y']] %>%
  as_tibble(., .name_repair = 'unique') %>%
  select(-`50%...6`) %>% 
  add_column(Modelo = "M1", .before = "time")

DFYb <- dfy_b[['y']] %>%
  as_tibble(., .name_repair = 'unique') %>%
  select(-`50%...6`) %>% 
  add_column(Modelo = "M2", .before = "time")

DFY <- bind_rows(DFYa, DFYb) %>% 
  rename(p50 = `50%...7`,
         p10 = `10%`,
         p20 = `20%`,
         p30 = `30%`,
         p40 = `40%`,
         p60 = `60%`,
         p70 = `70%`,
         p80 = `80%`,
         p90 = `90%`)

#-------------------------------------------------------------------------------#
# Gráfico 1   -------------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Configurar tema
theme_set(theme_classic() + 
            theme(panel.border = element_rect(fill = NA, colour = 'black')))

# Leyenda de gráfico
cpt <- paste(
'Marc Lavielle (2019). mlxR: Simulation of Longitudinal Data. R package version 4.1.0.',
'https://CRAN.R-project.org/package=mlxR \n',

'[M1] Duthaler U, et al. Population pharmacokinetics of oral ivermectin in venous plasma and dried',
'blood spots in healthy \n volunteers. Br J Clin Pharmacol. 2019; 85(3):626–33.',
'[M2] El-Tahtawy A et al. The effect of azithromycin on ivermectin \n pharmacokinetics - A population ',
'pharmacokinetic model analysis. PLoS Negl Trop Dis. 2008; 2 (5)'
)

labv <- c('M1: Duthaler U et al.',
          'M2: El-Tahtawy A et al.')

# Gráfico conjugado
{G1 <- DFY %>% 
  ggplot(aes(x = time, group = Modelo, col = Modelo, fill = Modelo)) +
  geom_line(mapping = aes(y = p50)) + 
  geom_ribbon(mapping = aes(ymin = p40, ymax = p60), alpha = 0.4, colour = NA) + 
  geom_ribbon(mapping = aes(ymin = p30, ymax = p70), alpha = 0.3, colour = NA) + 
  geom_ribbon(mapping = aes(ymin = p20, ymax = p80), alpha = 0.2, colour = NA) + 
  geom_ribbon(mapping = aes(ymin = p10, ymax = p90), alpha = 0.1, colour = NA) +
  geom_hline(yintercept = 1750.2, lty = 'dotted') +
  xlab('Tiempo (horas)') + 
  ylab(expression(paste("Conc. plasmástica (",mu,"g ",L^{-1},")"))) + 
  labs(title = 'Simulación farmacocinética Ivermectina', 
       subtitle = 'Ivermectina PO Dosis inicial 600 mg + 400mg q8h',
       caption = cpt) + 
  scale_y_continuous(
    sec.axis = sec_axis( ~ . / 875.10, 
                         name = expression(paste("Conc. plasmástica (", mu, "M)")))) +
  theme(legend.position = c(0.2, 0.7),
        panel.grid = element_line(colour = NA), 
        plot.caption = element_text(hjust = 0, size = 8)) +
  coord_cartesian(ylim = c(0, 8000), expand = FALSE) + 
  scale_color_manual(values = c('green4', 'purple3'), labels = labv) + 
  scale_fill_manual(values = c('green4', 'purple3'), labels = labv) 
}

G1


#write_rds(G1, "modulos/Modelo_PK/Modelos_Dosis/PK2_opcional7.rds")

# Almacenamiento de gráfico
ggsave(filename = 'PK2_opcional7.pdf', plot = G1, device = 'pdf', 
       width = 8, height = 6, units = 'in') 

# G1 +
#   coord_cartesian(ylim = c(0, 2000), expand = FALSE) + 
#   geom_hline(yintercept = 1750.2 ) 
