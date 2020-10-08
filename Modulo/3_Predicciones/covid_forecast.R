infectados_1 <- dataCOL %>% 
  group_by(fis) %>% 
  count() %>%
  filter(!is.na(fis)) %>% 
  ungroup() %>% 
  tidyr::complete(fis = m) 
  
infectados_2 <- dataCOL %>% 
  group_by(fecha_recuperado) %>% 
  count() %>%
  filter(!is.na(fecha_recuperado)) %>% 
  ungroup() %>% 
  tidyr::complete(fecha_recuperado = m) 

dataCOL_fis <- infectados_1 %>% 
  left_join(infectados_2, by = c('fis' = 'fecha_recuperado')) %>%
  left_join(dataCOL2b, by = c('fis' = 'date')) %>%
  select(-X) %>% 
  mutate(across(c(n.x, n.y, deaths), 
                .fns = ~ifelse(is.na(.x), 0, .x))) %>% 
  mutate(R = cumsum(n.y), M = cumsum(deaths), I = cumsum(n.x)) %>% 
  mutate(Infectados = I - R - M,
         Infectados_Nuevo = lead(Infectados) - Infectados)



out3S <- projections(r = out3)


projection_plotting(r_list = list(out3S),
                    # scenarios = c("pUN","pC1","pC2","pC3","pC4"),
                    scenarios = c("I"),
                    var_select = c("infections"),
                    add_parms_to_scenarios = TRUE,
                    ci = TRUE, x_var = "date",
                    summarise = TRUE) +
  geom_point(data = dataCOL_fis, 
             mapping = aes(x = fis, y = Infectados)) +
  coord_cartesian(ylim = c(0, 100000))   +
  ylab('Infecciones')


