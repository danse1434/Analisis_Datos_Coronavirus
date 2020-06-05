library(EpiModel)
library(tidyverse)

##########################################################################-
# Parámetros
# Lambda - tiempo de infección
l = 1/7
# R0 
R = 2.56
# Fuerza de infección
lambda <- l*R
# Tasa de contacto por persona
c = 10
# Probabilidad de infección
beta <- lambda/c

# Tasa de natalidad
a = 16.3/(1000*365)
# Tasa de mortalidad
m = 5.50/(1000*365)
# Tasa de mortalidad
m_c = 3/100

##########################################################################-
# Población Colombiana 
P = 49070000
PE = P * 0.55 * 0.7979706

init <- init.dcm(s.num = PE, i.num = 58, r.num = 1)

control <- control.dcm(type = "SIR", nsteps = 500, dt = 0.5)



param <- param.dcm(inf.prob = beta, act.rate = c, rec.rate = l, 
                   a.rate = a, ds.rate = m, di.rate = m_c, dr.rate = m)
mod0 <- dcm(param, init, control)



param1 <- param.dcm(inf.prob = beta, act.rate = c, rec.rate = l, 
                    a.rate = a, ds.rate = m, di.rate = m_c, dr.rate = m, 
                    inter.start = 15, inter.eff = 1 - 0.8)

mod1 <- dcm(param1, init, control)


param2 <- param.dcm(inf.prob = beta, act.rate = c, rec.rate = l, 
                    a.rate = a, ds.rate = m, di.rate = m_c, dr.rate = m, 
                    inter.start = 15, inter.eff = 1 - 0.6)

mod2 <- dcm(param2, init, control)


##########################################################################-
# Rearreglo de tabla

data_EPI <- function(m) {
  Svec = m$epi$s.num
  Ivec = m$epi$i.num$run1
  Rvec = m$epi$r.num$run1
  
  
  data_table <- Svec %>%
    rownames_to_column(var = 'Tiempo') %>%
    rename(S = run1) %>%
    as_tibble(.) %>%
    add_column(I = Ivec) %>%
    add_column(R = Rvec) %>%
    gather(S, I, R, key = 'Tipo', value = 'No') %>%
    mutate(Tiempo = as.double(Tiempo))
  
  return(data_table)
}

dataCol <- data1 %>% 
  filter(location == 'Colombia')

finit <- as.Date("2020/03/06", "%Y/%m/%d")

df1 <- data_EPI(mod0) %>%
  mutate(Fecha = Tiempo + finit - 15) %>% 
  filter(Tipo == 'I')
df2 <- data_EPI(mod1) %>%  
  mutate(Fecha = Tiempo + finit - 15) %>% 
  filter(Tipo == 'I')
df3 <- data_EPI(mod2) %>%  
  mutate(Fecha = Tiempo + finit - 15) %>% 
  filter(Tipo == 'I')





GEPI1 <- ggplot(df1, aes(x = Tiempo, y = No, 
                group = Tipo, colour = Tipo)) + 
  geom_line(col = 'red') +
  geom_line(data = df2, col = 'blue3') +
  geom_line(data = df3, col = 'green3') +
  geom_point(data = dataCol,
             aes(x = dd + 15, y = total_cases),
             inherit.aes = FALSE) +
  ylab('No. de infectados') + 
  labs(caption = 'Creado DSPG')

GEPI2 <- GEPI1 + coord_cartesian(xlim = c(0, 100),
                                 ylim = c(0, 2.50E+4))+
  theme(legend.position="none", axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(color="red1", fill="white"),
        plot.margin = unit(c(0,0,-6,-6),"mm"), plot.caption = element_blank())


t <- ggplotGrob(GEPI2)



GEPI1 + 
  annotation_custom(grob = t, 
                    xmin = 500, xmax = 1000,
                    ymin = 2.2E6, ymax = 4E6)

ggplot(df1, aes(x = Fecha, y = No, 
                group = Tipo, colour = Tipo)) + 
  geom_line(col = 'red') +
  geom_line(data = df2, col = 'blue3') +
  geom_line(data = df3, col = 'green3') +
  scale_color_discrete() +
  ylab('No. de infectados') + 
  labs(caption = 'Creado DSPG')





##########################################################################-
# Simulación estocástica --------------------------------------------------
##########################################################################-
# initS <- init.icm(s.num = PE, i.num = 58, r.num = 1)
# controlS <- control.icm(type = "SIR", nsteps = 500, nsims = 1)
# paramS <- param.icm(inf.prob = beta, act.rate = c, rec.rate = l, 
#                    a.rate = a, ds.rate = m, di.rate = m_c, dr.rate = m)
# 
# modS <- icm(paramS, initS, controlS)
# 
# 
