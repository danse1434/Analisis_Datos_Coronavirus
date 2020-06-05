library(EpiModel)
library(tidyverse)

##########################################################################-
# Parámetros
# Lambda - Tiempo de infección
l <- 1/7
# R0 - Número reproductivo básico
R <- 2.56
# Fuerza de infección
lambda <- R * l
# Tasa de contacto por persona
c = 10
# Probabilidad de infección
beta <- lambda/c

##########################################################################-
# Población Colombiana 
P = 49070000
PE = P * 0.55 * 0.7979706

init <- init.dcm(s.num = PE, i.num = 58, r.num = 1)
control <- control.dcm(type = "SIR", nsteps = 500, dt = 0.5)
param <- param.dcm(inf.prob = beta, act.rate = c, rec.rate = l)

mod0 <- dcm(param, init, control)






# microbenchmark::microbenchmark(mod0 <- dcm(param, init, control))
# Unit: milliseconds
#                              expr      min      lq    mean   median       uq      max neval
# mod0 <- dcm(param, init, control) 287.8134 299.282 309.373 303.0545 309.8393 643.8075   100


plot(mod0)
