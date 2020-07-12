# Carga de paquetes
require(tidyverse)
require(mlxR)
require(directlabels)
require(scales)
require(ggrepel)
#-------------------------------------------------------------------------------#
# Modelo 1  ---------------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Definición de componentes del modelo
#................................................................................
#   Definición de parámetros *p1* (e. fijos) y *p2* (e. aleatorios)
#   Definición de observaciones *Cc* 
#   Definición de regimen de administración *adm*
#   Definición de grupos *g*
#................................................................................

p1a  <- c(V1_pop = 89.13, V2_pop = 234.30, Cl_pop = 7.67, Q_pop = 19.0, 
          Ktr = (6 + 1) / 1.05, Mtt_pop = 1.05, ka = 0.55, pb = 0.93)
p2a  <- c(omega_V1 = 0.10, omega_V2 = 0.20, omega_Cl = 0.25, omega_Q = 0.41, 
          omega_Mtt = 0.60)
pRa  <- c(Imax = 1.207, IC50 = 2.275, hill = 3.914)
pCV  <- c(beta = 6.77E-5, gamma = 4.55, delta = 1.59)
pCV  <- pCV/24 # Consistencia dimensional de 1/días a 1/horas

time_scale <- seq(0, 24*20, 0.1)
Cca        <- list(name = 'u_Cc', time = time_scale)
Ra         <- list(name = 'R', time = time_scale)
CinVir1    <- list(name = 'f', time = time_scale)
CinVir2    <- list(name = 'v', time = time_scale)


adm_ivm <- function(x) {
  list(time = seq(0, 24 * 7, length.out = (3*7)+1), 
       amount = c(12*x, rep(12*x, 3*7)))
}

adm1 <- adm_ivm(1)
adm2 <- adm_ivm(10)
adm3 <- adm_ivm(50)
adm4 <- adm_ivm(100)
adm5 <- adm_ivm(200)

# 600,400
g1   <- list(size = 10, level = 'individual', treatment = adm1)
g2   <- list(size = 10, level = 'individual', treatment = adm2)
g3   <- list(size = 10, level = 'individual', treatment = adm3)
g4   <- list(size = 10, level = 'individual', treatment = adm4)
g5   <- list(size = 10, level = 'individual', treatment = adm5)


# Módulo de Simulación
resa <- simulx(model     = 'modelo_PD_2/modelo_combinado.txt',
               parameter = c(p1a, p2a, pRa,pCV),
               output    = list(Cca, Ra, CinVir1, CinVir2),
               group     = list(g1,g2,g3,g4,g5))


Cc_df <- prctilemlx(resa$u_Cc, number = 2, plot = FALSE)[['y']] %>%
  as_tibble(.name_repair = 'universal')

R_df <- prctilemlx(resa$R, number = 2, plot = FALSE)[['y']] %>%
  as_tibble(.name_repair = 'universal') 

CV_v_df <- prctilemlx(resa$v, number = 2, plot = FALSE)[['y']] %>%
  as_tibble(.name_repair = 'universal') 

CV_f_df <- prctilemlx(resa$f, number = 2, plot = FALSE)[['y']] %>%
  as_tibble(.name_repair = 'universal')


theme_set(theme_bw())

legend_df <- tibble::tribble(
  ~ group, ~ l1,
  '1', '12mg q8h',
  '2', '120mg q8h',
  '3', '600mg q8h',
  '4', '1200mg q8h',
  '5', '2400mg q8h'
)


Cc_df1 <- Cc_df %>% 
  group_by(group) %>% 
  filter(`..90.` == max(..90.)) %>% 
  left_join(legend_df, by = 'group') 

G1 <- Cc_df %>% 
  ggplot(aes(x = time/24, y = ..50.)) +
  geom_ribbon(aes(ymin = ..10., ymax = ..90., fill = group, alpha = 0.2)) +
  geom_line(aes(col = group)) + 
  xlab('Tiempo (días)') +
  coord_cartesian(xlim = c(0,22)) +
  scale_y_continuous(
    sec.axis = sec_axis( ~ .*1000 / 875.10, 
                         name = expression(paste("Conc. libre IVM (", mu, "M)")))) +
  xlab('Tiempo (días)') +
  geom_text_repel(data = Cc_df1, 
                  aes(x = time/24, y = ..90., col = group, label = l1),
                  xlim = c(15, 20)) +
  ylab('Concentración libre \n IVM (mg/L)') +
  theme(legend.position = "none")  +
  labs(
    title = 'Farmacocinética de Ivermectina',
    subtitle = 'Esquemas de tratamiento de 7 días',
    caption =
      glue::glue(
        "PK: Duthaler U, Suenderhauf C, Karlsson MO, Hussner J, Meyer zu Schwabedissen H, Krähenbühl S, et al. 
      Population pharmacokinetics of oral ivermectin in venous plasma and dried blood spots in healthy 
      volunteers. Br J Clin Pharmacol. 2019;85(3):626–33." 
      )
  )+ 
  scale_color_viridis_d(end = 0.90) +
  scale_fill_viridis_d(end = 0.90)

R_df1 <- R_df %>% 
  group_by(group) %>% 
  filter(`..50.` == min(..50.)) %>% 
  left_join(legend_df, by = 'group') 


G2 <- R_df %>% 
  ggplot(aes(x = time/24, y = ..50.)) +
  geom_ribbon(aes(ymin = ..10., ymax = ..90., fill = group, alpha = 0.5)) +
  geom_line(aes(col = group)) + 
  coord_cartesian()+
  xlab('Tiempo (días)') +
  ylab('Efecto RNA viral relativo (%)') +
  geom_text_repel(data = R_df1, 
                  aes(x = time/24, y = ..50., col = group, label = l1),
                  xlim = c(15, 20)) +
  theme(legend.position = "none")  +
  labs(
    title = 'Reducción relativa viral vs dosis de ivermectina',
    subtitle = 'RNA Gen E - Cultivo Celular',
    caption =
    glue::glue(
      "
      PD: Caly L, Druce JD, Catton MG, Jans DA, Wagstaff KM. The FDA-approved Drug Ivermectin
      inhibits the replication of SARS-CoV-2 in vitro. Antiviral Res. 2020;104787."
    )
  ) +
  scale_color_viridis_d(end = 0.90) +
  scale_fill_viridis_d(end = 0.90)

#-------------------------------------------------------------------------------#
# Gráifco 1 -----------------------------------------------------
#-------------------------------------------------------------------------------#

breaks <- 10^(-1:7)
minor_breaks <- rep(1:9, 9)*(10^rep(-1:7, each=9))


CV_v_df1 <- CV_v_df %>% 
  group_by(group) %>% 
  filter(`..50.` == max(..50.)) %>% 
  left_join(legend_df, by = 'group') 


G3 <- CV_v_df %>% 
  left_join(legend_df, by = 'group') %>%
  ggplot(aes(x = time/24, y = ..50., col = group, lty = group)) +
  geom_line(size = 1.2) + 
  scale_y_log10(breaks=breaks,
                minor_breaks = minor_breaks) +
  xlab('Tiempo (días)') + 
  ylab('Carga viral RNA (copias/mL)') +
  coord_cartesian(x = c(0, 20))  +
  annotation_logticks(sides = 'rl') +
  theme(legend.position = "none") +
  geom_text_repel(data = CV_v_df1, 
                  aes(x = time/24, y = ..50., col = group, label = l1),
                  xlim = c(15, 20)) +
  scale_colour_viridis_d(end = 0.95) + 
  labs(
    title = 'Curso de la infección SARS-COV-2 vs dosis de ivermectina',
    subtitle = 'Carga viral determinada en hisopado de garganta',
    caption = 
      glue::glue("
      Farmacodinamia: Caly L, Druce JD, Catton MG, Jans DA, Wagstaff KM. The FDA-approved Drug Ivermectin 
      inhibits the replication of SARS-CoV-2 in vitro. Antiviral Res. 2020;104787. 
      Cinética viral: Kim KS, Ejima K, Ito Y, Iwanami S, Ohashi H, Koizumi Y, et al. Modelling SARS-CoV-2 
      Dynamics: Implications for Therapy. medRxiv. 2020;2020.03.23.20040493. "))

G4 <- CV_f_df %>%
  left_join(legend_df, by = 'group') %>% 
  ggplot(aes(x = time/24, y = ..50., col = group, lty = group)) +
  geom_line(size = 1) +
  xlab('Tiempo (días)') + ylab('Fracción de células no infectadas') +
  coord_cartesian(xlim = c(0, 22), ylim = c(0, 1)) +
  geom_dl(mapping = aes(label = l1),
          method = list(dl.trans(x = x + 0.13), "last.bumpup", cex = 0.8)) +
  theme(legend.position = "none") +
  labs(title = 'Curso de la infección SARS-COV-2 vs dosis de ivermectina',
       subtitle = 'Fracción: células no infectadas vs células susceptibles iniciales',
       caption = 
         glue::glue("
      Farmacodinamia: Caly L, Druce JD, Catton MG, Jans DA, Wagstaff KM. The FDA-approved Drug Ivermectin 
      inhibits the replication of SARS-CoV-2 in vitro. Antiviral Res. 2020;104787. 
      Cinética viral: Kim KS, Ejima K, Ito Y, Iwanami S, Ohashi H, Koizumi Y, et al. Modelling SARS-CoV-2 
      Dynamics: Implications for Therapy. medRxiv. 2020;2020.03.23.20040493. ")) +
  scale_colour_viridis_d(end = 0.95) 
  
ggsave('modelo_PD_2/figuras/G1.pdf', G1, 'pdf', width = 8, height = 6)
ggsave('modelo_PD_2/figuras/G2.pdf', G2, 'pdf', width = 8, height = 6)
ggsave('modelo_PD_2/figuras/G3.pdf', G3, 'pdf', width = 8, height = 6)
ggsave('modelo_PD_2/figuras/G4.pdf', G4, 'pdf', width = 8, height = 6)
