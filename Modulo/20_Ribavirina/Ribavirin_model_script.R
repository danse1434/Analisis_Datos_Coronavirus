##########################################################################-

require(mlxR)
require(tidyverse)

# Jin, R., Fossler, M. J., McHutchison, J. G., Howell, C. D., & Dowling, T. 
# C. (2012). Population Pharmacokinetics and Pharmacodynamics of Ribavirin 
# in Patients with Chronic Hepatitis C Genotype 1 Infection. The AAPS 
# Journal, 14(3), 571–580. doi:10.1208/s12248-012-9368-z 

model1 <- inlineModel("
[LONGITUDINAL]
input = {ka, Cl, V1, Q, V2}
                      
EQUATION:
V = V1 
k = Cl/V1 
k12 = Q/V1 
k21 = Q/V2

Cc = pkmodel(ka, V, k, k12, k21)

OUTPUT:
output = Cc

[INDIVIDUAL]
input = {V1_pop, omega_V1, gamma_V1, Cl_pop, omega_Cl, gamma_Cl, V2_pop, omega_V2}

DEFINITION:
Cl = {distribution = lognormal, prediction = Cl_pop, varlevel={id, id*occ}, sd = {omega_Cl, gamma_Cl}}
V1 = {distribution = lognormal, prediction = V1_pop, varlevel={id, id*occ}, sd = {omega_V1, gamma_V1}}
V2 = {distribution = lognormal, prediction = V2_pop, sd = omega_V2}
")

convCV <- function(CV){
  sqrt(log((CV / 100) ^ 2 + 1)) 
}

param <- c(ka = 0.86, Q = 38.6,
           Cl_pop = 19.00, omega_Cl = convCV(37.42), gamma_Cl = convCV(7.54),
           V1_pop = 1130, omega_V1 = convCV(31.56), gamma_V1 = convCV(64.34),
           V2_pop = 4020, omega_V2 = convCV(67.90))

trt <- list(time = seq(0, 24 * 7, 12), amount = c(rep(c(400, 600), 7), 400))
out.cc <- list(name = 'Cc', time = seq(0, 27 * 8, by = 0.1))
out.param <- list(name = c('ka', 'V1', 'Cl'))
occ <- list(name = "occ", time = seq(0, 24 * 7, 12))

res <- simulx(
  model     = model1,
  parameter = param,
  treatment = trt,
  varlevel  = occ,
  output    = list(out.cc),
  group     = list(size = 1000, level = 'individual'),
  settings  = list(seed = 123123)
)

G1 <- res$Cc %>% 
  ggplot(aes(x = time, y = Cc, group = id, colour = id)) +
  geom_line(size = 0.3) +
  theme_bw() +
  xlab('Tiempo') + ylab(expression(C[P]~(mg/L))) + 
  labs(title = 'Perfil plasmático Ribavirina', 
       subtitle = 'Tratamiento Hepatitis C - Regímen de dosis 400mg QAM PO y 600mg QPM PO', 
       caption = 'Jin R et al. The AAPS Journal 14(3) 571-584') +
  theme(legend.position = "none")

ggsave(G1, filename = "plot2.pdf", device = 'pdf', width = 6, height = 4)


p1 <- mlxR::prctilemlx(res$Cc)

print(p1) +
  xlab('Tiempo') + ylab(expression(C[P]~(mg/L))) + 
  labs(title = 'Perfil plasmático Ribavirina', 
       subtitle = 'Tratamiento Hepatitis C', 
       caption = 'Regímen de dosis 400mg QAM PO y 600mg QPM PO; modelo adaptado de Jin R et al. The AAPS Journal 14(3) 571-584') +
  theme_bw()







model2 <- inlineModel("
[LONGITUDINAL]
input = {Cl, V1, Q, V2}
                      
EQUATION:
V = V1 
k = Cl/V1 
k12 = Q/V1 
k21 = Q/V2

Cc = pkmodel(V, k, k12, k21)

OUTPUT:
output = Cc

[INDIVIDUAL]
input = {V1_pop, omega_V1, gamma_V1, Cl_pop, omega_Cl, gamma_Cl, V2_pop, omega_V2}

DEFINITION:
Cl = {distribution = lognormal, prediction = Cl_pop, varlevel={id, id*occ}, sd = {omega_Cl, gamma_Cl}}
V1 = {distribution = lognormal, prediction = V1_pop, varlevel={id, id*occ}, sd = {omega_V1, gamma_V1}}
V2 = {distribution = lognormal, prediction = V2_pop, sd = omega_V2}
")



trt1 <- list(time = c(0, seq(6, 96, 6), seq(104, 174, 8)), 
             tinf = 1,  
             amount = c(2000, rep(1000, 4 * 4), rep(500, 3 * 3)))

out.cc1 <- list(name = 'Cc', time = seq(0, max(trt$time), 0.1))
occ1 <- list(name = "occ", time = trt1$time)

param2 <- c(ka = 0.86, Q = 38.6,
           Cl_pop = 19.00, omega_Cl = convCV(37.42), gamma_Cl = convCV(7.54),
           V1_pop = 1130, omega_V1 = convCV(31.56), gamma_V1 = convCV(64.34),
           V2_pop = 4020, omega_V2 = convCV(67.90))


res1 <- simulx(
  model     = model2,
  parameter = param2,
  treatment = trt1,
  varlevel  = occ1,
  output    = list(out.cc1),
  group     = list(size = 1000, level = 'individual'),
  settings  = list(seed = 123123)
)


p2 <- mlxR::prctilemlx(res1$Cc)

print(p2) +
  xlab('Tiempo') + ylab(expression(C[P]~(mg/L))) + 
  labs(title = 'Perfil plasmático Ribavirina', 
       subtitle = 'Tratamiento Virus SARS - I', 
       caption = 'Regímen de dosis 2000mg carga; 1000mg q6h x 4 días; 5000mg q8h x 3 días IV 
       Modelo adaptado de Jin R et al. The AAPS Journal 14(3) 571-584') +
  theme_bw()



g1 <- list(model = model1,
           parameter = param,
           treatment = trt,
           varlevel = occ,
           size = 1000,
           level = 'individual')


g2 <- list(model = model2,
           parameter = param2,
           treatment = trt1,
           varlevel = occ1,
           size = 1000,
           level = 'individual')


E <- exposure(group = list(g1, g2), 
              output = list(name = c("Cc"), time = seq(72, 100, 0.01)))

