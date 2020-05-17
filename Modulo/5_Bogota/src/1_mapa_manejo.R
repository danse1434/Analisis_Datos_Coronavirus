#-------------------------------------------------------------------------------#
# Colombia
# En este mapa se pueden encontrar datos geoestadísticos adoptados por el DANE
# https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-\\
# marco-geoestadistico-nacional/

# Bogotá
# En la siguientes webs se pueden encontrar datos geoestastícos de Bogotá D.C.
# Datos kmz por localidad
# https://sites.google.com/site/seriescol/kml---kmz
# glue::glue(https://datosabiertos.bogota.gov.co/dataset?organization=secretaria\\
# -distrital-de-planeacion&groups=ordenamiento-territorial&res_format=WMS)

data_BOG <- read_sf("Modulo/5_Bogota/data/raw/Loca.shp")

# plot(data_BOG)
