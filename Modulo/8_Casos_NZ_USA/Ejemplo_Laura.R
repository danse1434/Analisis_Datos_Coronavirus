##------------------------------------------------------------------------#
## Nombre del Script: Lectura de datos de repositorio GitHUB  -
##  
## Propósito del Script: realizar la lectura de archivos de repositorio de 
##  gitHub establecido para trazar en Covid19. Se adaptó el método de Ben 
##  Phillips de https://github.com/benflips/nCovForecast/blob/master/getDataNew.R#L27
##  
## Autor: Daniel S. Parra Gonzalez 
## Fecha de creación:  27-03-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
# Carga de paquete
require(tidyverse)
require(lubridate)
require(ggrepel)

#-------------------------------------------------------------------------------#
# Casos en Nueva Zelanda --------------------------------------------------------
#-------------------------------------------------------------------------------#
# Directorio de casos confirmados
tsConf <- file.path('https:/','raw.githubusercontent.com','CSSEGISandData',
                    'COVID-19',
                    'master', 'csse_covid_19_data', 'csse_covid_19_time_series', 
                    'time_series_covid19_confirmed_global.csv')

# Directorio de muertes confirmados
tsDeath <- file.path('https:/', 'raw.githubusercontent.com', 'CSSEGISandData', 
                     'COVID-19', 
                     'master', 'csse_covid_19_data', 'csse_covid_19_time_series', 
                     'time_series_covid19_deaths_global.csv')

##########################################################################-
# Lectura de archivos de datos de casos y muertes confirmadas
tsI <- read_csv(file = tsConf)
tsD <- read_csv(file = tsDeath)

#-------------------------------------------------------------------------------#
# Creación de gráficos-----------------------------------------------------
#-------------------------------------------------------------------------------#
# Configuración de gráficos
theme_set(theme_bw() +
            theme(panel.border = element_rect(fill = NULL, color = 'black')))

#-------------------------------------------------------------------------------#
# Función de creación de polígonos en GGplto2
poly_fun <- function(xmin,xmax,alpha) {
  list(geom_rect(aes(xmin = as.Date(xmin),
                     xmax = as.Date(xmax), 
                     ymin = -10, ymax=500), col = NA, 
                 fill = alpha("#ffae4c", alpha)))
}

#-------------------------------------------------------------------------------#
# Intervenciones resaltadas

labdf = tribble(
  ~x, ~y, ~label,
  as.Date("2020-03-30"), 90, "Lockdown begins \n to take effect \n (30-March)"
)

#-------------------------------------------------------------------------------#
# Intervenciones de Salud Pública
textdf = tribble(
  ~x, ~y, ~label,
  "2020-03-19", 125, "Borders closed for non-residents",
  "2020-04-10", 095, "New Zealanders returning, going \n to supervised quarantine",
  
  "2020-03-22", 125, "Lockdown Level 2",
  "2020-03-24", 125, "Lockdown Level 3",
  "2020-03-27", 125, "Lockdown Level 4",
  "2020-04-29", 125, "Lockdown Level 3",
  "2020-05-15", 125, "Lockdown Level 2"
)

# Caption
cpt <- glue::glue(paste0(
  "This is not an official graph. ",
  "Adapted from: https://github.com/CSSEGISandData/COVID-19 ",
  "{today()}. \n Made by: Sofia Parra"))

#-------------------------------------------------------------------------------#
# Creación de gráfico sin asignación a un objeto
# G1 <- 
df1 %>% 
  filter(Country == 'New Zealand') %>%  
  mutate(Val_New = if_else(Val_New<0, 0, Val_New)) %>% 
  ggplot(aes(x = Date, y = Val_New)) +
  
  poly_fun("2020-03-21", "2020-03-23", 0.009) +
  poly_fun("2020-03-23", "2020-03-26", 0.01) +
  
  poly_fun("2020-03-26", "2020-04-28", 0.03) +
  poly_fun("2020-04-28", "2020-05-14", 0.01) +
  poly_fun("2020-05-14", "2020-05-16", 0.009) +
  
  xlab('Date') + ylab('New Reported Cases per day') +
  labs(title = 'New Cases per Day - New Zealand', 
       caption = cpt) +

  geom_bar(stat = 'identity', fill = '#878787', size=0.01,
           width = 0.8, col ="gray10") +
  
  geom_label_repel(
    data = labdf,
    mapping = aes(x=x, y=y, label=label),
    arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "first"),
    force = 10, 
    ylim  = c(125, NA), xlim = c(as.Date("2020-04-01"), NA)) +
  
  geom_text(aes(label = ifelse((Val_New > 0), Val_New, NA)), 
            size = 3, nudge_y = 4, 
            col="#878787", angle=90) +
  
  geom_text(data = textdf, aes(x=as.Date(x),y=y,label=label), angle=90,
            size=3, col = "gray40") + 
  
  scale_x_date(date_breaks = "1 week", 
               date_minor_breaks = "1 day",
               date_labels = "%m-%d") + 
  coord_cartesian(xlim=c(as.Date("2020-02-24"), as.Date("2020-05-15")), 
                  ylim=c(0, 150)) 


ggsave("NZ.pdf", width = 8*1.2, height = 6*1.2, device = 'pdf') 
ggsave("NZ.png", width = 8*1.2, height = 6*1.2, device = 'png', dpi = 700) 


#-------------------------------------------------------------------------------#
# Reporte de Casos en USA -------------------------------------------------------
#-------------------------------------------------------------------------------#

# Directorio de casos confirmados
tsUCI <- file.path('https:/','raw.githubusercontent.com','CSSEGISandData',
                   'COVID-19',
                   'master', 'csse_covid_19_data', 'csse_covid_19_time_series',
                   'time_series_covid19_confirmed_US.csv')

# Reporte de Casos por estado

df2 <- read_csv(file = tsUCI) 

df2b <- df2 %>% 
  select(-UID, -code3, -FIPS, -Admin2) %>% 
  pivot_longer(cols = -c(Province_State, Country_Region, Lat, Long_, 
                         iso2, iso3, Combined_Key), 
               names_to = 'Date', values_to = 'Value') %>% 
  mutate(Date = mdy(Date)) %>% 
  group_by(Country_Region, Province_State) %>% 
  filter(iso3 == "USA") %>% 
  group_by(Date, add = TRUE) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Val_New = Value - lag(Value))


G2 <- df2b %>% 
  mutate(Val_New = if_else(Val_New<0, 0, Val_New)) %>% 
  ggplot(aes(x = Date, y = Val_New)) +
  geom_bar(stat = 'identity', fill = '#878787', size=0.005,
           width = 0.8, col ="gray10") +
  
  geom_text(aes(label = ifelse((Val_New > 0), Val_New, NA),
                y = Val_New * 1.5), 
            size = 0.5, vjust=1, 
            col="#878787", angle=90) +
  
  facet_wrap(~ Province_State, ncol = 7) + 
  xlab('Date') + ylab('New Reported Cases per day') +
  labs(title = 'New Cases per Day - United States', 
       caption = cpt) +
  scale_x_date(date_breaks = "1 month", 
               date_minor_breaks = "15 days",
               date_labels = "%m-%d") + 
  coord_cartesian(xlim=c(as.Date("2020-02-24"), as.Date("2020-05-15")), 
                  ylim=c(0, 10000)) +
  theme(
    panel.grid.minor = element_line(size=0.01, colour = "gray99"),
    panel.grid.major = element_line(size=0.01, colour = "gray95"))

ggsave("USA.pdf", plot = G2, width = 8*1.5, height = 6*1.5, device = 'pdf') 
ggsave("USA.png", plot = G2, width = 8*1.5, height = 6*1.5, device = 'png', dpi = 700) 


#-------------------------------------------------------------------------------#
# Casos a nivel país

# Intervenciones comunes de gobierno federal
textdf1 = tribble(
  ~x, ~y, ~label,
  "2020-03-13", 2.5E4, "National Emergency",
  "2020-03-19", 2.5E4, "Avoid all international travel"
)

# Gráfico totalizado

df2b %>% 
  ggplot(aes(x = Date, y = Val_New)) +
  geom_bar(stat = 'identity', fill = '#878787', size=0.005,
           width = 0.8) +
  
  geom_text(aes(label = ifelse((Val_New > 0), Val_New, NA),
                y = Val_New * 1.5), 
            size = 0.5, vjust=1, 
            col="#878787", angle=90) +
  xlab('Date') + ylab('New Reported Cases per day') +
  labs(title = 'New Cases per Day - United States', 
       caption = cpt) +
  scale_x_date(date_breaks = "1 week", 
               date_minor_breaks = "1 day",
               date_labels = "%m-%d") + 
  
  geom_text(data = textdf1, aes(x=as.Date(x),y=y,label=label), angle=90,
            size=3, col = "gray40") + 
  
  coord_cartesian(xlim=c(as.Date("2020-02-24"), as.Date("2020-05-15")), 
                  ylim=c(0, 4E4)) +
  theme(
    panel.grid.minor = element_line(size=0.01, colour = "gray99"),
    panel.grid.major = element_line(size=0.01, colour = "gray95"))


ggsave("USA1.pdf", width = 8, height = 6, device = 'pdf') 
ggsave("USA1.png", width = 8, height = 6, device = 'png', dpi = 700) 
