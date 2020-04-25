require(ggplot2)
require(scales)

setwd(file.path('F:/Documentos/Estudio - Documentos/Estadistica_2013-2/Regresion/2_Regresión Nolineal/Casos/Analisis_Datos_Coronavirus/Modulo/Modulo_Ivermectina/modelo_pkpd/data'))

theme_set(theme_bw() + 
            theme(panel.background = element_rect(fill = NULL, colour = 'black')))

g1 <- readRDS('PK2_opcional7.rds')

G1 <- g1 +
  ylab(expression("Conc. plasmática (ng"*mL^{-1}*")")) +
  scale_y_continuous(breaks = seq(0, 8000, 2000),
                     sec.axis = sec_axis(~ . / 875.10, name = derive())
                     )

remove_geom <- function(ggplot2_object, geom_type) {
  # Delete layers that match the requested type.
  layers <- lapply(ggplot2_object$layers, function(x) {
    if (class(x$geom)[1] == geom_type) {
      NULL
    } else {
      x
    }
  })
  # Delete the unwanted layers.
  layers <- layers[!sapply(layers, is.null)]
  ggplot2_object$layers <- layers
  ggplot2_object
}

G2 <- remove_geom(G1, "GeomHline")

G2 <- G2 +
  geom_hline(yintercept = 2187, lty = 'dashed')

ggsave('figura_ejemplo_twitter.png', plot = G2, 'png', width = 8, height = 7)
