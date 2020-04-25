require(deSolve)
require(tidyverse)

# Simulación de datos requiere los parámetros
# Condiciones iniciales
V = c(Ep0 = 25-2.59, Ei0 = 2.59, V0 = 0.061)
# Parámetros de transmisión
p = c(dep = 1E-3, dei = 0.11, 
      beta = 0.55, pi = 0.24, dv = 5.36)

# Modelo SIR
VIR <- function(t, y, params) {
  Ep = V["Ep0"]; Ei = V['Ei0']; V = V['V0']
  beta = p['beta']; dep = p['dep']; dei = p['dei'];
  dv = p['dv']; pi = p['pi'];
  
  dEp  = dep*(Ep - y[1]) - beta*y[1]*y[3]
  dEi  = beta*y[1]*y[3] - dei*y[2]
  dV   = pi*y[2] - dv*y[3]
  
  res <- c(dEp, dEi, dV)
  list(res)
  
}

out <- ode(y = V, times = seq(0, 25, 0.01), func = VIR, parms = p, method = 'ode45')

out %>% 
  as_tibble() %>% str(.)
  pivot_longer(cols = c('Ep0', 'Ei0', 'V0'), 
               names_to = 'Parámnetro',
               values_to = 'Valores')


matplot(x = out[,1], y = out[,2:4], type = 'l')





















