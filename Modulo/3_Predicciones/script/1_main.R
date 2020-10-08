##------------------------------------------------------------------------------#
## Nombre del Script:  ----------------------------------------------------------
##  
## Propósito del Script:  
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 27/07/2020 
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquete
require(squire) # Paquete de 
require(tidyverse)
require(patchwork)

# Paralelizar el modelo
future::plan(future::multiprocess())


# 1 Introducción ------------------------------------------------------------

# Cargar datos desde API si no existe el archivo
if (!file.exists(paste0('data/raw/dataCOL', Sys.Date(), '.csv'))) {
  source('script/2_almacenar_datos.R')
} else {
  dataCOL <- read_csv(file = paste0('data/raw/dataCOL', Sys.Date(), '.csv'))
}


dataCOL1 <- dataCOL %>% 
  mutate(id_de_caso = as.numeric(id_de_caso), 
         across(matches("fecha|fis"), ~as.Date(.x)) )


m <- seq.Date(from = min(dataCOL1$fis, na.rm = TRUE),
              to   = Sys.Date(), 
              by   = "day")

dataCOL2 <- dataCOL1 %>% 
  group_by(fecha_de_muerte) %>% 
  count() %>%
  filter(!is.na(fecha_de_muerte)) %>% 
  ungroup() %>% 
  tidyr::complete(fecha_de_muerte = m) %>% 
  rownames_to_column('X') %>% 
  rename(date = fecha_de_muerte,
         deaths = n) %>% 
  mutate(deaths = ifelse(is.na(deaths), 0, deaths)) %>% 
  arrange(date)

dataCOL2 


# 2 Calibrar modelo -------------------------------------------------------
int_unique <- read_delim('data/raw/intervenciones_gobierno_COL.txt', 
           delim = ',', skip = 2, trim_ws = TRUE) %>% 
  rename(date_change = Fecha,
         change      = Efectividad)

# 
# int_unique1 <- int_unique %>% 
#   mutate(date_change1 = as.Date(date_change, origin = "2020-01-01"),
#          change = R_0 / max(R_0)) %>% 
#   distinct(date_change1, .keep_all = TRUE) %>% 
#   select(-date_change, -R_0) %>% 
#   arrange(date_change1)
# 
# int_unique2 <- int_unique1 %>% 
#   add_column(rep = rep(1:2, times = 87)) %>% 
#   filter(rep != 1)
  
#-------------------------------------------------------------------------------#
# Quitar fechas de muertes recientes por problemas
dataCOL2b <-  dataCOL2 %>% 
  filter(date < Sys.Date() - 7)

set.seed(12346)

int_unique3 <- int_unique2 %>% 
  mutate(fc = case_when(
    date_change1 > as.Date("2020-07-01") ~ 1.08,
    date_change1 > as.Date("2020-06-15") ~ 1.02,
    date_change1 > as.Date("2020-06-01") ~ 1.00,
    date_change1 > as.Date("2020-05-15") ~ 1.28,
    TRUE ~ 1.00),
    change2 = change * fc)  %>% 
  add_row(date_change1 = as.Date("2020-07-16"), change2 = 0.3866667)


g1 <- int_unique3 %>% 
  ggplot(aes(x = date_change1, y = fc)) + 
  geom_line()

g2 <- int_unique3 %>% 
  ggplot(aes(x = date_change1)) + 
  geom_line(aes(y = change*3), col = 'black') +
  geom_line(aes(y = change2*3), lty = 'dashed',col = 'red') +
  geom_hline(yintercept = 1)

# g1 + g2

# ptm = Sys.time()
# Ajustar el modelo
out3 <- calibrate(
  data = dataCOL2b,
  R0_min = 0.65, R0_max = 3,
  R0_step = 0.05, #R0_step = 0.05,
  first_start_date = "2020-02-13",
  last_start_date =  "2020-02-15",
  day_step = 1,
  replicates = 80,
  n_particles = 20,
  R0_change = int_unique$change, 
  forecast = 90,
  date_R0_change = int_unique$date_change,
  country = "Colombia")

plot(out3, var_select = c("deaths"), x_var = "date", particle_fit = TRUE) +
  coord_cartesian(ylim = c(0, 500)) +
  theme(panel.grid.major.x = element_line(colour = 'gray60'),
        panel.grid.major.y = element_line(colour = 'gray80')) + 
  scale_x_date(date_minor_breaks = "1 weeks", date_breaks = "1 month", 
               date_labels = "%b %d") + 
  labs(title = 'Pronóstico de 3 Meses - COVID-19 en Colombia',
       subtitle = '', 
       caption = 
  glue::glue('Modelo SEIR estructurado por edad con capacidad de sistema de salud y \\
             severidad de la enfermedad
             Estimado con Squire R disponible en: https://github.com/mrc-ide/squire')) +
  xlab('Fecha') +
  ylab('Muertes diarias')

# print(ptm-Sys.time())

#-------------------------------------------------------------------------------#
# Crear proyecciones
p_c0 <- projections(r = out3, R0_change = c(1.0), tt_R0 = c(0),)
p_c1 <- projections(r = out3, R0_change = c(0.8), tt_R0 = c(0))
p_c2 <- projections(r = out3, R0_change = c(0.6), tt_R0 = c(0))
p_c3 <- projections(r = out3, R0_change = c(0.4), tt_R0 = c(0))
p_c4 <- projections(r = out3, R0_change = c(0.2), tt_R0 = c(0))


ggproj <- projection_plotting(r_list = list(p_c0, p_c1, p_c2, p_c3, p_c4),
                              scenarios = c("pC0","pC1","pC2","pC3","pC4"),
                              var_select = c("deaths"),
                              add_parms_to_scenarios = TRUE,replicates = TRUE,
                              ci = TRUE, x_var = 'date',
                              summarise = TRUE) + 
  facet_wrap(~ Scenario) +
  scale_x_date(date_minor_breaks = "1 weeks", date_breaks = "1 month", 
               date_labels = "%b %d") + 
  xlab('Fecha') + 
  ylab('Ocupación de UCI') +
  theme(panel.grid.major.x = element_line(colour = 'gray60'),
        panel.grid.major.y = element_line(colour = 'gray80')) +
  labs(title = 'Pronóstico de 3 Meses - COVID-19 en Colombia',
       subtitle = '', 
       caption = 
         glue::glue('Modelo SEIR estructurado por edad con capacidad de sistema de salud y \\
             severidad de la enfermedad
             Estimado con Squire R disponible en: https://github.com/mrc-ide/squire')) +
  xlab('Fecha') +
  ylab('Ocupación UCI') + 
  theme(legend.position = 'none', axis.text.x = element_text(angle = 30, vjust = 0.2))

ggproj

ggproj1 <- projection_plotting(r_list = list(p_un, p_c1, p_c2, p_c3, p_c4),
                    scenarios = c("pUN","pC1","pC2","pC3","pC4"),
                    var_select = c("ICU_demand"),
                    add_parms_to_scenarios = TRUE,
                    ci = TRUE, x_var = "date",
                    summarise = TRUE)



ggproj1 + 
  geom_hline()



ggproj2 <- projection_plotting(r_list = list(p_un, p_c1, p_c2, p_c3, p_c4),
                               scenarios = c("pUN","pC1","pC2","pC3","pC4"),
                               var_select = c("ICU_occupancy"),
                               add_parms_to_scenarios = TRUE,
                               ci = TRUE, x_var = "date",
                               summarise = TRUE)

ggproj2


# https://salutia.org/pronosticos-comportamiento-covid-19


projection_plotting(r_list = list(out3),
                              scenarios = c("Normal"),
                              var_select = c("ICU_demand"),
                              add_parms_to_scenarios = TRUE,
                              ci = TRUE, x_var = "date",
                              summarise = TRUE) + 
  scale_x_date(date_minor_breaks = "1 weeks", date_breaks = "1 month", 
               date_labels = "%b %d") + 
  xlab('Fecha') + 
  ylab('Demanda de UCI') +
  theme(panel.grid.major.x = element_line(colour = 'gray60'),
        panel.grid.major.y = element_line(colour = 'gray80'))


projection_plotting(r_list = list(out3),
                    scenarios = c("Normal"),
                    var_select = c("ICU_occupancy"),
                    add_parms_to_scenarios = TRUE,
                    ci = TRUE, x_var = "date",
                    summarise = TRUE) + 
  scale_x_date(date_minor_breaks = "1 weeks", date_breaks = "1 month", 
               date_labels = "%b %d") + 
  xlab('Fecha') + 
  ylab('Ocupación de UCI') +
  theme(panel.grid.major.x = element_line(colour = 'gray60'),
        panel.grid.major.y = element_line(colour = 'gray80'))




