library("tidyverse")
library("lubridate")
library("sf")
library('glue')

source("Modulo/5_Bogota/src/1_mapa_manejo.R")

#-------------------------------------------------------------------------------#

data <- read_delim(file = glue::glue("http://saludata.saludcapital.gov.co/osb/\\
                              datos_abiertos_osb/enf-transmisibles/OSB_EnfTransm\\
                              -COVID-19.csv"), ";", skip = 4, 
                   locale = locale('es', encoding="ISO-8859-1", asciify=TRUE))

data <- data[1:(nrow(data)-2),]

write_csv(data, glue("Modulo/5_Bogota/data/raw/rawdata_{today()}.csv"))


data1 <- data %>% 
  rename_with(~word(., 1)) %>% 
  filter(!is.na(ID)) %>% 
  mutate(Fecha = dmy(Fecha),
         Sexo = factor(Sexo))

write_csv(data1, glue("Modulo/5_Bogota/data/processed/ \\
                      prodata_{today()}.csv"))

#-------------------------------------------------------------------------------#
theme_set(theme_bw())

data1 %>% 
  group_by(Fecha) %>% 
  summarise(n = n()) %>% 
  mutate(ntot = cumsum(n)) %>% 
  ggplot(aes(x = Fecha, y = ntot)) +
  geom_bar(stat = "identity")


#-------------------------------------------------------------------------------#
# Función de creación de polígonos en GGplto2
poly_fun <- function(xmin,xmax,alpha) {
  list(geom_rect(aes(xmin = as.Date(xmin),
                     xmax = as.Date(xmax), 
                     ymin = 0, ymax=200), col = NA, 
                 fill = alpha("#84898C", alpha)))
}

textdf = tribble(
  ~x, ~y, ~label, ~col,
  "2020-03-26", 115, "Cuarentena nacional", 'black',
  "2020-04-29", 115, "Inicio reactivación", 'red3'
)

G1 <- data1 %>% 
  group_by(Localidad, Fecha) %>% 
  summarise(n = n()) %>% 
  mutate(ntot = cumsum(n)) %>% 
  filter(!(Localidad %in% c('Sumapaz', 'Sin Dato', 'Fuera de Bogotá'))) %>% 
  ggplot(aes(x = Fecha, y = n, group = Localidad)) + 
  facet_wrap(~Localidad) + 
  geom_vline(xintercept = as.Date('2020-03-20'), lty = 'dotted') +
  geom_vline(xintercept = as.Date('2020-03-24'), lty = 'solid') +
  geom_vline(xintercept = as.Date('2020-04-13'), lty = 'dashed') +
  geom_vline(xintercept = as.Date('2020-04-21'), lty = 'dashed') +
  
  geom_vline(xintercept = as.Date('2020-04-27'), lty = 'dashed', col = 'red3') +
  
  geom_vline(xintercept = as.Date('2020-06-01'), lty = 'dashed') +
  ylab('Número de reportes de casos Covid-19 diarios') +
  # Línea
  geom_line(colour = '#658dc6') + 
  geom_text(data = textdf, aes(x=as.Date(x),y=y,label=label, col=col), angle=90,
            size=2.5, inherit.aes = FALSE) + 
  scale_fill_viridis_d() +
  scale_color_manual(values = setNames(c('black', 'red3'), c('black', 'red3'))) +
  theme(legend.position = 'none', 
        axis.title.x = element_blank(), 
        panel.grid = element_line(colour = 'gray99')) +
  coord_cartesian(ylim = c(0,200), xlim = as.Date(c('2020-03-15','2020-06-21')))

G1
ggsave(glue('Modulo/5_Bogota/Figuras/Bogota_{today()}.png'), G1, 
       width = 10, height = 6, device = 'png', dpi = 300)



#-------------------------------------------------------------------------------#
# Mapa de Bogotá

data2 <- data1 %>% 
  add_column(indic = 1) %>% 
  group_by(Localidad, Fecha) %>% 
  summarise(cumindic = sum(indic)) %>% 
  ungroup() %>%
  pivot_wider(names_from = Fecha, values_from = cumindic) %>% 
  pivot_longer(cols = -Localidad, names_to = "Fecha", 
               values_to = "Reportes") %>% 
  mutate(Fecha = ymd(Fecha),
         Reportes = ifelse(is.na(Reportes), 0, Reportes),
         Localidad = toupper(Localidad),
         Localidad = iconv(Localidad, 
                           from='UTF-8', to='ASCII//TRANSLIT')) %>% 
  arrange(Localidad, Fecha)

data_BOG <- data_BOG %>% 
  mutate(Localidad = toupper(LocNombre),
         Localidad = iconv(Localidad, 
                           from='UTF-8', to='ASCII//TRANSLIT')) %>% 
  st_as_sf()



data3 <- data2 %>% 
  group_by(Localidad) %>% 
  mutate(RepAcu = cumsum(Reportes)) %>% 
  left_join(data_BOG, by = "Localidad")



# Creación de mapa :D
theme_set(theme_bw() + 
            theme(panel.grid      = element_blank(),
                  panel.border    = element_rect(),
                  legend.position = c(0.8, 0.85),
                  axis.title      = element_blank()))

data3 %>% 
  filter(Fecha == "2020-05-12") %>% 
  ggplot(aes(geometry = geometry)) + 
  geom_sf(aes(fill = RepAcu))

require(tmap)

tmap_mode("view")

data3 %>% 
  filter(Fecha == "2020-05-12") %>% 
  tm_shape(., name = 'Localidad') +
  tm_fill("RepAcu")



