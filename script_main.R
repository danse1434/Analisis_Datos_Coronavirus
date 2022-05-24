require(tidyverse)
require(lubridate)

data <- read_csv("Casos_positivos_de_COVID-19_en_Colombia.csv") %>% 
  as_tibble(.name_repair = "universal")


data %>% 
  mutate(FIS1 = if_else(FIS == "Asintomático", 
                        FIS, 'Sintomático')) %>% 
  count(FIS1)

data1 <- data %>% 
  filter(FIS != "Asintomático") %>% 
  mutate(FIS = ymd_hms(FIS),
         DDx = difftime(Fecha.diagnostico, FIS, units = c("days")))

data1 %>% 
  ggplot(aes(x = Departamento.o.Distrito, 
             y = DDx/(60*60*24))) + 
  geom_bar(stat = 'identity') + 
  coord_flip()


data$FIS
  mutate(DDx = Fecha.diagnostico - FIS)
data$FIS %>% unique()
