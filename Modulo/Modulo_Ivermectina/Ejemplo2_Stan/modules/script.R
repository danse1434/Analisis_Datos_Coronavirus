require(tidyverse)
require(rstan)
require(shinystan)

##########################################################################-
# Datos de rata

rat_data <- read_table2("modules/rats.txt")

# Matriz
y <- as.matrix(rat_data)

# Vector de parámetros
x <- c(8, 15, 22, 29, 36)

# Parámetro media
xbar <- mean(x)

# Dimensiones
N <- nrow(y)
T <- ncol(y)

data <- list(y = y, x = x, xbar = xbar, N = N, T = T)

rats_fit <- stan('modules/rats.stan', data = data)

##########################################################################-
# Resultados
rats_fit


pairs(rats_fit, pars = c("mu_alpha", "mu_beta", "sigmasq_y", "sigmasq_alpha", "sigmasq_beta"))



shinystan::launch_shinystan(rats_fit)
