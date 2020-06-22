# Carga de paquetes
require(tidyverse)
require(rlang)
require(lubridate)
require(directlabels)
require(forecast)


yearfun <- function(year) {
  x <- read_delim(file.path('Modulo', '9_EV_DANE', 'data', paste0('nofetal', year, '.txt')), 
             "\t", escape_double = FALSE, trim_ws = TRUE)
  assign(paste0("nofetal", year), x, envir = parent.frame())
}

for (i in 2000:2018) {
yearfun(i)
}  

for (i in 2000:2018) {
  expr(!!paste0("nofetal", 2000)) %>%
    parse_expr() %>%
    eval_bare() %>%
    colnames() %>% print()
  print('\\n')
}


func_nofe <- function(year) {
  x <- paste0("nofetal", year)
  y <- parse_expr(x) %>% eval_bare()
  
  z <- y %>% 
    rename_with(toupper) %>% 
    group_by(ANO, MES) %>% 
    summarise(n = n()) %>% 
    ungroup()
  
  assign(paste0("res", year), z, envir = parent.frame())
}


for (i in 2000:2018) {
  func_nofe(i)
}

#-------------------------------------------------------------------------------#
# Datos de 2019 ----------------------------------------------------------------
#-------------------------------------------------------------------------------#












#-------------------------------------------------------------------------------#
# Unión de todos los gráficos ---------------------------------------------------
#-------------------------------------------------------------------------------#


RES <- bind_rows(res2000, res2001, res2002, res2003, res2004, 
                 res2005, res2006, res2007, res2008, res2009,
                 res2010, res2011, res2012, res2013, res2014,
                 res2015, res2016, res2017, res2018) %>% 
  mutate(MES = as.double(MES), 
         Mes = month(MES, label = TRUE),
         Fecha = ymd(paste(ANO, MES, 01)))


RES


#-------------------------------------------------------------------------------#
theme_set(theme_classic() + theme(
  panel.border = element_rect(fill = NA, colour = 'black')))

cpt <- paste0('Adaptado de: Departamento Administrativo Nacional de ',
              'Estadística (DANE). Sistema de Información y Consulta de \n',
              ' Estadísticas Vitales. Estadísticas Vitales (EEVV), http:',
              '//systema74.dane.gov.co/bincol/rpwebengine.exe/portal?',
              'lang=esp \n (2019, consultado el 27 de abril de 2020).')

RES %>% 
  ggplot(aes(x = Mes, y = n, group = ANO, col = ANO)) +
  geom_line() + geom_point() +
  xlab('Mes') + ylab('Fallecimientos por todas las causas por mes') +
  labs(title = 'Fallecimientos por todas las causas por mes en Colombia', 
       subtitle = '2000 - 2018',
       caption = cpt) + 
  scale_color_continuous(type = 'viridis', name = 'Año') +
  coord_cartesian(ylim = c(1.4E4, 2.2E4))
  
ggsave('Muertes_Totales_Mes.pdf', width = 8, height = 6)

#-------------------------------------------------------------------------------#
# Serie de tiempo

RES.ts <- ts(data = RES$n, frequency = 12, start = c(2000,01,01))
# RES.mo <- HoltWinters(RES.ts)
# RES.pr <- predict(RES.mo, 24, prediction.interval = TRUE)

RES.mo <- auto.arima(y = RES.ts)
RES.pr <- 
  forecast(RES.mo, h = 24) %>% 
  as_tibble(.name_repair = 'universal') %>% 
  rename(n = Point.Forecast) %>% 
  add_column(Fecha = seq(as.Date('2019/1/1'), as.Date('2020/12/1'), by = 'month')) %>% 
  mutate(ANO = year(Fecha))

# ts.plot(RES.ts, RES.pr, col = c(4,2,2,10), lty = c(1,2,2,2),
#         gpars = list(xlab = 'Fecha', 
#                      ylab = 'Fallecimientos por todas las causas por mes'))

RES %>% 
  ggplot(aes(Fecha, n, col = ANO)) +
  geom_line() +
  xlab('Fecha') + ylab('Fallecimientos por todas las causas por mes') +
  labs(title = 'Fallecimientos por todas las causas por mes en Colombia', 
       subtitle = 'Datos: 2000 - 2018; Pred. ARIMA(1,1,1)(0,1,1)[12]: 2019-2020',
       caption = cpt) +
  scale_color_continuous(type = 'viridis', name = 'Año') +
  geom_line(data = RES.pr.tb, col = 'red') +
  geom_ribbon(data = RES.pr.tb,
              aes(x = Fecha, ymin = lwr, ymax = upr),
              fill = alpha('red', 0.1), inherit.aes = FALSE)

# Para Colombia se esperaban 21.066 (IC95% 19.939-22.193) fallecimientos (por
# todas las causas) para el mes de mayo de 2020, esto es un valor esperado diario
# de 702 (IC95% 665-740) fallecimientos.

ggsave('Muertes_Totales_Año.pdf', width = 8, height = 6)







#-------------------------------------------------------------------------------#
# Unión de todas las tablas -----------------------------------------------------
#-------------------------------------------------------------------------------#
func_nofe_1 <- function(year) {
  x <- paste0("nofetal", year)
  y <- parse_expr(x) %>% eval_bare()
  
  z <- y %>% 
    rename_with(toupper) %>% 
    select(ANO, MES, C_BAS1) %>% 
    ungroup()
  return(z)
  # assign(paste0("res1", year), z, envir = parent.frame())
}

ls = list()

for (i in 2000:2018) {
  ls[[i-2000+1]] <- func_nofe_1(i)
}

df_total <- bind_rows(ls)

G1 <- df_total %>% 
  filter(C_BAS1 == 'J180') %>% 
  group_by(ANO, MES) %>% 
  count(C_BAS1) %>% 
  ggplot(aes(x = MES, y = n, group = ANO, col = ANO)) +
  geom_line() + geom_point() +
  xlab('Mes') + ylab('Fallecimientos por mes') +
  labs(title = 'Fallecimientos en Colombia 2000 - 2018', 
       subtitle = 'J18.0: Bronconeumonía, microorganismo no especificado',
       caption = cpt) + 
  scale_color_continuous(type = 'viridis', name = 'Año') +
  coord_cartesian(ylim = c(0, 200))
G1
ggsave('Rplot1.pdf', width = 8, height = 6)

G2 <-df_total %>% 
  filter(C_BAS1 == 'J181') %>% 
  group_by(ANO, MES) %>% 
  count(C_BAS1) %>% 
  ggplot(aes(x = MES, y = n, group = ANO, col = ANO)) +
  geom_line() + geom_point() +
  xlab('Mes') + ylab('Fallecimientos por mes') +
  labs(title = 'Fallecimientos en Colombia 2000 - 2018', 
       subtitle = 'J18.1: Neumonía lobar, microorganismo no especificado',
       caption = cpt) + 
  scale_color_continuous(type = 'viridis', name = 'Año') +
  coord_cartesian(ylim = c(0, 200))
G2
ggsave('Rplot2.pdf', width = 8, height = 6)

G3 <- df_total %>% 
  filter(C_BAS1 == 'J182') %>% 
  group_by(ANO, MES) %>% 
  count(C_BAS1) %>% 
  ggplot(aes(x = MES, y = n, group = ANO, col = ANO)) +
  geom_line() + geom_point() +
  xlab('Mes') + ylab('Fallecimientos por mes') +
  labs(title = 'Fallecimientos en Colombia 2000 - 2018', 
       subtitle = 'J18.2: Neumonía hipostática, microorganismo no especificado',
       caption = cpt) + 
  scale_color_continuous(type = 'viridis', name = 'Año')  +
  coord_cartesian(ylim = c(0, 20))
G3
ggsave('Rplot3.pdf', width = 8, height = 6)

G4 <- df_total %>% 
  filter(C_BAS1 == 'J188') %>% 
  group_by(ANO, MES) %>% 
  count(C_BAS1) %>% 
  ggplot(aes(x = MES, y = n, group = ANO, col = ANO)) +
  geom_line() + geom_point() +
  xlab('Mes') + ylab('Fallecimientos por mes') +
  labs(title = 'Fallecimientos en Colombia 2000 - 2018', 
       subtitle = 'J18.8: Otros tipos de neumonía, microorganismo no especificado',
       caption = cpt) + 
  scale_color_continuous(type = 'viridis', name = 'Año')  +
  coord_cartesian(ylim = c(0, 20))
G4
ggsave('Rplot4.pdf', width = 8, height = 6)


G5 <- df_total %>% 
  filter(C_BAS1 == 'J189') %>% 
  group_by(ANO, MES) %>% 
  count(C_BAS1) %>% 
  ggplot(aes(x = MES, y = n, group = ANO, col = ANO)) +
  geom_line() + geom_point() +
  xlab('Mes') + ylab('Fallecimientos por mes') +
  labs(title = 'Fallecimientos en Colombia 2000 - 2018', 
       subtitle = 'J18.9: Neumonía, microorganismo no especificado',
       caption = cpt) + 
  scale_color_continuous(type = 'viridis', name = 'Año')  +
  coord_cartesian(ylim = c(0, 1000))
G5
ggsave('Rplot5.pdf', width = 8, height = 6)


G6 <- df_total %>% 
  filter(str_detect(C_BAS1, 'J18')) %>% 
  group_by(ANO, MES) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = MES, y = n, group = ANO, col = ANO)) +
  geom_line() + geom_point() +
  xlab('Mes') + ylab('Fallecimientos por mes') +
  labs(title = 'Fallecimientos en Colombia 2000 - 2018', 
       subtitle = 'J18.X: Todas las neumonías',
       caption = cpt) + 
  scale_color_continuous(type = 'viridis', name = 'Año')  +
  coord_cartesian(ylim = c(0, 1000))
G6
ggsave('Neumonías.pdf', width = 8, height = 6)



data_neumom <- df_total %>% 
  filter(str_detect(C_BAS1, 'J12|J13|J14|J15|J16|J17|J18')) %>% 
  group_by(ANO, MES) %>% 
  summarise(n = n()) %>% 
  mutate(Fecha = ymd(paste(ANO, MES, 01)))

data_neum_1 <- ts(data = data_neumom$n, start = c(2000,1), end = c(2018,12), frequency = 12)

neum_model1 <- auto.arima(y = data_neum_1)

data.neum.1.pr <- 
  forecast(neum_model1, h = 24) %>% 
  as_tibble(.name_repair = 'universal') %>% 
  rename(n = Point.Forecast) %>% 
  add_column(Fecha = seq(as.Date('2019/1/1'), as.Date('2020/12/1'), by = 'month')) %>% 
  mutate(ANO = year(Fecha))
# Se esperaban 3 299 muertes para losmeses febrero, marzo, abril, mayo

data_neumom %>% 
  ggplot(aes(Fecha, n, col = ANO)) +
  geom_line() +
  xlab('Fecha') + ylab('Fallecimientos (neumonía) por mes') +
  labs(title = 'Fallecimientos por neumonía J12 a J18 en Colombia', 
       subtitle = 'Datos: 2000 - 2018; Predicción: ARIMA(2,1,2)(2,0,0)[12]: 2019-2020',
       caption = cpt) +
  theme_bw()+
  scale_color_continuous(type = 'viridis', name = 'Año') +
  geom_ribbon(data = data.neum.1.pr,
              aes(x = Fecha, ymin = Lo.80, ymax = Hi.80),
              fill = alpha('red', 0.3), inherit.aes = FALSE) +
  geom_ribbon(data = data.neum.1.pr,
              aes(x = Fecha, ymin = Lo.95, ymax = Hi.95),
              fill = alpha('red', 0.1), inherit.aes = FALSE) +
  geom_line(data = data.neum.1.pr, col = 'red4') 

ggsave('Neumonías_PRED.pdf', width = 8, height = 6)


data.neum.1.pr %>% 
  filter(Fecha > as.Date('2020-01-01') & Fecha < as.Date('2020-06-01')) %>% 
  summarise(mean = sum(n))








