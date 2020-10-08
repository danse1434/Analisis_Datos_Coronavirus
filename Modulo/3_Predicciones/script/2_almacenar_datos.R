require(jsonlite)

# Lectura desde API INS - Datos de Covid
dataCOL <- fromJSON('https://www.datos.gov.co/resource/gt2j-8ykr.json?$limit=300000')

readr::write_csv(x = dataCOL,
                 path = paste0('data/raw/','dataCOL', Sys.Date(), '.csv'))

