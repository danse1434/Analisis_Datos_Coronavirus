library("tidyverse")
library("lubridate")
library("sf")

# source("Modulo/5_Bogota/src/1_mapa_manejo.R")

#-------------------------------------------------------------------------------#

data <- read_delim(file = glue::glue("http://saludata.saludcapital.gov.co/osb/\\
                              datos_abiertos_osb/enf-transmisibles/OSB_EnfTransm\\
                              -COVID-19.csv"), ";", skip = 4, 
                   locale = locale('es', encoding="ISO-8859-1", asciify=TRUE))

write_csv(data, glue::glue("Modulo/5_Bogota/data/raw/rawdata_{today()}.csv"))


data1 <- data %>% 
  rename_with(~word(., 1)) %>% 
  filter(!is.na(ID)) %>% 
  mutate(Fecha = dmy(Fecha),
         Sexo = factor(Sexo))
  
write_csv(data1, 
          glue::glue("Modulo/5_Bogota/data/processed/prodata_{today()}.csv"))



data1 %>% 
  group_by(Fecha) %>% 
  summarise(n = n()) %>% 
  mutate(ntot = cumsum(n)) %>% 
  ggplot(aes(x = Fecha, y = ntot)) +
  geom_bar(stat = "identity")


data1 %>% 
  group_by(Localidad, Fecha) %>% 
  summarise(n = n()) %>% 
  mutate(ntot = cumsum(n)) %>% 
  ggplot(aes(x = Fecha, y = n)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~Localidad)






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
  ggplot(aes(geometry)) + 
  geom_sf(aes(fill = RepAcu))

require(tmap)

tmap_mode("view")

data3 %>% 
  filter(Fecha == "2020-05-12") %>% 
  tm_shape(., name = 'Localidad') +
  tm_fill("RepAcu")



