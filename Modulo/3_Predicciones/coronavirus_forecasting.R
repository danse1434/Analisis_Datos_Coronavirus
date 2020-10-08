require(tidyverse)
require(squire)
require(patchwork)

# r <- run_explicit_SEEIR_model(country = "Colombia")
# plot(r)
# plot(r, var_select = "S")
# 
# output <- format_output(r, var_select = "E")
# head(output)
# 
# output <- format_output(r, var_select = "E", reduce_age = FALSE)
# head(output)



# Población
pop <- get_population("Colombia")
population <- pop$n

# Traer la matriz de mezcla
# contact_matrix <- get_mixing_matrix("Colombia")
# 
# contact_matrix %>% 
#   as_tibble(., rownames = 'M1') %>% 
#   pivot_longer(cols = -M1, names_to = 'M2', values_to = 'V') %>% 
#   ggplot(aes(x = M1, y=M2)) + 
#   geom_raster(aes(fill = V)) +
#   scale_fill_gradient(low='white', high = 'red4')
#  

library(jsonlite)


dataCOL <- fromJSON('https://www.datos.gov.co/resource/gt2j-8ykr.json?$limit=300000')

dataCOL1 <- dataCOL %<>% 
  as_tibble() %>% 
  mutate(id_de_caso = as.numeric(id_de_caso), 
         across(matches("fecha|fis"), ~as.Date(.x)) )

m <- seq.Date(min(dataCOL1$fis, na.rm = TRUE),
              Sys.Date(), 
              by = "day")

dataCOL2 <- dataCOL %>% 
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

#-------------------------------------------------------------------------------#
# Ajustar a modelo-----------------------------------------------------
#-------------------------------------------------------------------------------#
# Paralelizar el modelo
future::plan(future::multiprocess())

# Ajustar el modelo
out <- calibrate(
  data = dataCOL2,
  R0_min = 0.65, R0_max = 3,
  R0_step = 0.05,
  first_start_date = "2020-02-13",
  last_start_date =  "2020-02-25",
  day_step = 1,
  replicates = 10,
  n_particles = 20,
  # R0_change = int_unique$change, date_R0_change = int_unique$date_change,
  country = "Colombia")

# Intervenciones
# int_unique = tribble(
#   ~date_change, ~change, ~descrip,
#   '2020-03-13', 0.808, 'Simulacro Cuarentena',
#   '2020-03-17', 0.505, 'Cuarentena nacional'
# )

int_unique <- tibble::tribble(
  ~date_change,    ~R_0,
   60.3901, 2.9542,
   61.4238, 2.9491,
   62.4574, 2.9389,
    63.491, 2.9235,
   64.5246, 2.9061,
   65.5583, 2.8836,
   66.5919, 2.8569,
   67.6255, 2.8252,
   68.6591, 2.7893,
   69.6928, 2.7504,
   70.7264, 2.7064,
     71.76, 2.6562,
   72.7936,  2.605,
   73.8273, 2.5466,
   74.8609, 2.4841,
   75.8945, 2.4165,
    76.842, 2.3497,
   77.7895, 2.2772,
    78.737, 2.2059,
   79.5984, 2.1334,
   80.3736, 2.0621,
   81.0627, 1.9945,
   81.8379, 1.9195,
   82.6992, 1.8396,
   83.5606, 1.7634,
   84.4219, 1.6909,
   85.3694, 1.6135,
   86.3169, 1.5422,
   87.2644, 1.4732,
    88.298, 1.4056,
   89.3317, 1.3431,
   90.3653, 1.2888,
   91.3989, 1.2448,
   92.4325, 1.2079,
   93.4662, 1.1741,
   94.4998, 1.1444,
   95.5334, 1.1178,
    96.567, 1.0962,
   97.6007, 1.0799,
   98.6343, 1.0696,
   99.6679, 1.0614,
  100.7016, 1.0635,
  101.7352, 1.0696,
  102.7688, 1.0727,
  103.8024, 1.0819,
  104.8361,  1.086,
  105.8697, 1.0952,
  106.9033, 1.1014,
  107.9369, 1.1178,
  108.9706, 1.1454,
  110.0042,  1.172,
  111.0378, 1.1915,
  112.0714, 1.1598,
  113.1051, 1.1485,
  114.1387,  1.169,
  115.1723, 1.1915,
  116.2059,  1.213,
  117.2396, 1.2314,
  118.2732, 1.2448,
  119.3068, 1.2571,
  120.1682, 1.2801,
  120.8573,   1.36,
  121.6669, 1.4414,
  122.2641, 1.4496,
  122.8384, 1.3779,
  123.4413, 1.3062,
  124.3027, 1.2642,
  125.3363, 1.3052,
  126.3699, 1.3083,
  127.2313, 1.2494,
  128.0926,  1.169,
  129.1263, 1.1372,
  130.1599, 1.1669,
  131.1935, 1.2171,
  132.2271, 1.1577,
  133.2608, 1.1731,
  134.2944, 1.1833,
   135.328, 1.1966,
  136.3616, 1.2099,
  137.3953, 1.1895,
  138.4289, 1.2017,
  139.4625, 1.2263,
  140.3239, 1.2601,
  141.1278, 1.2919,
   141.702, 1.1966,
  142.5634,  1.127,
  143.3386, 1.0543,
  144.1138, 0.9744,
  145.1475, 0.9887,
  146.1811, 1.0624,
  147.2147, 1.0481,
  148.2483, 1.0747,
   149.282,  1.126,
  150.3156, 1.1065,
   151.177, 1.0496,
  152.0168, 0.9874,
  152.3312, 1.1194,
  152.5034, 1.2386,
  152.8627,  1.365,
  153.0432, 1.4885,
  153.5887, 1.3923,
  153.6318, 1.3047,
  153.8041, 1.2186,
  154.2778, 1.1413,
  155.3115, 1.1085,
  156.1728, 1.0747,
  156.6724, 1.1796,
  157.2064, 1.2396,
  157.9817, 1.1526,
  158.2401, 1.0834,
  158.7052, 1.0125,
  159.0799, 0.9529,
  159.6059, 1.1052,
   160.092, 1.2471,
  161.2056,   1.25,
  162.0587, 1.3185,
   162.633, 1.1895,
   163.199, 1.1017,
  164.1834, 1.1292,
  164.7002, 1.0432,
  165.6477, 0.9662,
  166.3885,  0.994,
  166.8536, 1.1024,
  167.3704, 1.1987,
  168.0226, 1.2948,
  169.0931, 1.2612,
  170.1268, 1.2642,
  171.1604, 1.2622,
  172.0045, 1.2866,
  172.2371, 1.1987,
  172.4524, 1.1198,
  172.6247, 1.0379,
  172.9692, 0.9452,
   173.176, 0.8945,
  174.0029, 0.9774,
  174.3474, 1.0681,
  174.9719,  1.171,
  175.0365, 1.2386,
  175.8978,  1.126,
  176.0701, 1.0957,
  177.1899, 1.1895,
  178.0512, 1.1464,
  178.6542, 1.0665,
  179.3433, 0.9903,
  180.2907, 0.9272,
  181.1521, 0.9395,
  181.4966, 1.0563,
  181.7551,  1.171,
  182.1685, 1.2693,
  182.7148, 1.3501,
  183.0299, 1.4205,
  183.4433, 1.2915,
    183.65, 1.1936,
  184.0376, 1.0888,
  185.1143, 1.0737,
  185.9757, 1.0112,
   186.837, 0.9354,
    187.44, 0.9293,
  187.8707,      1,
  188.1865, 1.0256,
  188.8674, 0.9155,
   189.335, 0.9129,
  190.2825, 0.9764,
  191.3161,  1.003,
  192.1602, 1.0026,
  192.4359, 0.9313,
  192.8493, 0.8465,
  193.3403, 0.7716,
  194.0897, 0.7396,
  194.5031, 0.8453,
   195.063, 0.9582,
  196.0535, 0.8945,
  196.8288,  0.833,
  153.1026, 1.6359
)


int_unique1 <- int_unique %>% 
  mutate(date_change1 = as.Date(date_change, origin = "2020-01-01"),
         change = R_0 / max(R_0)) %>% 
  distinct(date_change1, .keep_all = TRUE) %>% 
  select(-date_change, -R_0) %>% 
  arrange(date_change1)

int_unique2 <- int_unique1 %>% 
  add_column(rep = rep(1:2, times = 87)) %>% 
  filter(rep != 1)
  
  
# # Ajustar el modelo
# out1 <- calibrate(
#   data = dataCOL2,
#   R0_min = 0.65, R0_max = 3,
#   R0_step = 0.05,
#   first_start_date = "2020-02-13",
#   last_start_date =  "2020-02-25",
#   day_step = 1,
#   replicates = 10,
#   n_particles = 20,
#   R0_change = int_unique2$change, 
#   date_R0_change = int_unique2$date_change1,
#   country = "Colombia")

# plot(out1, "deaths", x_var = "date", particle_fit = TRUE)
# 
# # plot(out, "deaths", date_0 = max(dataCOL1$date), x_var = "date")
# plot(out, particle_fit = TRUE)
# 

# plot(out$scan_results)
# 
# s.Date.POSIXct()


dataCOL2 %>% 
  ggplot(aes(x= date, y = deaths)) +
  geom_point()
#-------------------------------------------------------------------------------#
# Quitar fechas de muertes recientes por problemas
dataCOL2b <-  dataCOL2 %>% 
  filter(date < Sys.Date() - 7)

# Ajustar el modelo
# out2 <- calibrate(
#   data = dataCOL2b,
#   R0_min = 0.65, R0_max = 3,
#   R0_step = 0.05,
#   first_start_date = "2020-02-13",
#   last_start_date =  "2020-02-25",
#   day_step = 1,
#   replicates = 10,
#   n_particles = 20,
#   R0_change = int_unique2$change, 
#   date_R0_change = int_unique2$date_change1,
#   country = "Colombia")
# 
# plot(out2, var_select = c("deaths","E","IMild"), x_var = "date", particle_fit = TRUE)
# 
# plot(out2, var_select = c("deaths","E","R"), 
#      x_var = "date")

#-------------------------------------------------------------------------------#
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

ptm = Sys.time()
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
  R0_change = int_unique3$change2, 
  forecast = 90,
  date_R0_change = int_unique3$date_change1,
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

print(ptm-Sys.time())

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




