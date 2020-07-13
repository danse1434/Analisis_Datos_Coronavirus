require(tidyverse)
require(treemapify)
require(LBE)
require(PhViD)

data <- read_csv('ivermectin_ADR_profile.csv')


data1 <- data %>%
  mutate(
    main_text = str_replace(Main_group, "\\s\\(\\d{1,4}\\)", ""),
    main_numb = str_replace(Main_group, ".+\\s\\((?=\\d{1,4}\\)$)", ""),
    main_numb = as.double(str_replace(main_numb, "(?<=\\d{1,4})\\)", "")),
    
    secu_text = str_replace(Sec_group, "\\s\\(\\d{1,4}\\)", ""),
    secu_numb = str_replace(Sec_group, ".+\\s\\((?=\\d{1,4}\\)$)", ""),
    secu_numb = as.double(str_replace(secu_numb, "(?<=\\d{1,4})\\)", ""))
  ) %>% 
  select(main_text, main_numb, secu_text, secu_numb)


data2 <-
  data1 %>%
  mutate(
    main_text2 =
      case_when(
        main_text == "Blood and lymphatic system disorders" ~ "Blood and \n lymphatic system",
        main_text == "Skin and subcutaneous tissue disorders" ~ "Skin and \n subcutaneous tissue",
        main_text == "General disorders and administration site conditions" ~ "General disorders and \n administration site",
        main_text == "Gastrointestinal disorders" ~ "Gastrointestinal \n disorders",
        main_text == "Nervous system disorders" ~ "Nervous \n system",
        main_text == "Injury, poisoning and procedural complications" ~ "Injury, \n poisoning and \n procedural",
        main_text == "Infections and infestations" ~ "Infections and \n infestations",
        main_text == "Psychiatric disorders" ~ "Psychiatric \n disorders",
        main_text == "Eye disorders" ~ "Eye \n disorders",
        main_text == "Musculoskeletal and connective tissue disorders" ~ "Musculoskeletal and \n connective tissue",
        main_text == "Vascular disorders" ~ "Vascular \n disorders",
        main_text == "Renal and urinary disorders" ~ "Renal and \n urinary",
        main_text == "Metabolism and nutrition disorders" ~ "Metabolism and \n nutrition",
        main_text == "Cardiac disorders" ~ "Cardiac \n disorders",
        main_text == "Immune system disorders" ~ "Immune system",
        main_text == "Reproductive system and breast disorders" ~ "Reproductive system \n and breast",
        main_text == "Hepatobiliary disorders" ~ "Hepatobiliary \n disorders",
        main_text == "Psychiatric disorders" ~ "Psychiatric \n disorders",
        main_text == "Respiratory, thoracic and mediastinal disorders" ~ "Respiratory, \n thoracic and \n mediastinal",
        TRUE ~ main_text
      )
  )
  



G1 <- 
ggplot(data2, aes(area=secu_numb, label=secu_text, 
                  subgroup=main_text2, fill=secu_numb)) +
  geom_treemap() +
  scale_fill_viridis_c(alpha = 0.8) +
  geom_treemap_subgroup_border(colour = 'white') +
  geom_treemap_subgroup_text(place = "centre", grow = T, colour = "white", 
                             min.size = 0, fontface='bold', size=8) +
  geom_treemap_text(colour = "gray90", place = "bottomright",
                    grow = TRUE, size = 6, alpha = 0.6) +
  theme(legend.position = 'none') +
  labs(title = 'Treemap of Adverse Drug Reactions (ADR) with Ivermectin', 
       subtitle = 'ADR total count: 8613; The area of each square represents the number of reported cases.', 
       caption='Adapted from: Uppsala Monitoring Center. Vigiaccess. Available at: http: //www.vigiaccess.org/ [Accessed: July 2020] \n Made by DSPG'
       )
  

ggsave('Heat_Map_IVM.pdf', plot = G1, device = 'pdf', height = 6, width = 8)





#-------------------------------------------------------------------------------#
# Eventos adversos Colombia -----------------------------------------------------
#-------------------------------------------------------------------------------#
df1 <- read_csv('EVENTOS_ADVERSOS_DE_MEDICAMENTOS.csv') 

df2 <- df1 %>% 
  as_tibble(.name_repair = 'universal') %>% 
  group_by(Descripción.ATCMedicamento, DescripcionWhoart) %>% 
  summarise(N = as.double(n()))

df3 <- df2 %>% 
  as.data.frame() %>% 
  as.PhViD()

df_ROR <- ROR(df3)

PhViD.search(df_ROR, DRUG = 'IVERMECTINA')



