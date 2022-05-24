
require(tidyverse)
require(sf)
require(plotly)
require(rjson)

colombiaMunicipios <-
  fromJSON(
    file = file.path(
      'Modulo',
      '10_mapa',
      'co_2018_MGN_MPIO_POLITICO.geojson.txt'
    )
  )

# sf::read_sf(file.path(
#   'Modulo',
#   '10_mapa',
#   'co_2018_MGN_MPIO_POLITICO.geojson.txt'
# )) %>% View()

#'-------------------------------------------------------------------------------
# 1. Lectura de datos ------------------
#'-------------------------------------------------------------------------------
dataCOL <- fromJSON('http://www.datos.gov.co/resource/gt2j-8ykr.json?$limit=1')


#'-------------------------------------------------------------------------------
# 2. Mapa ------------------
#'-------------------------------------------------------------------------------

fig <- plot_ly()

fig <- fig %>% add_trace(
  type          = "choroplethmapbox",
  geojson       = colombiaMunicipios,
  # locations     = formatC(MOR1$CodDepartamento, width=2, flag='0'),
  z             = I('black'),
  colorscale    = "Viridis",
  featureidkey  = 'properties.DPTO',
  # text          = MOR1$hover,
  marker        = list(line = list(width = 1, color = I('black'))),
  hovertemplate = '%{text}'
  # frame = MOR1$Año
) %>% 
  layout(mapbox = list(
    style  = "carto-positron",
    zoom   = 4,
    center = list(lon = -74.2973328, lat = 4.570868)
  ), title = paste0('COVID')
  # scene = list(zaxis = list(range = c(0, max(MOR1$CantUC))))
  ) %>%
  config(displaylogo = FALSE)

  

fig
