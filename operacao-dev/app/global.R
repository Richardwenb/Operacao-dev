packages <- c('shinyWidgets', 'foreach', 'doParallel', 'RMariaDB', 'tidyr', 'shinyalert', 'readxl', 'stringi', 'yaml', 'varhandle', 'janitor', 'DBI', 'lubridate', 'stringr', 'foreach', 'DT',
              'shiny', 'dplyr', 'RSAP', 'reshape', 'devtools')
lapply(packages, library, character.only = TRUE)

options(warn=-1)
producao_planejado <<- NULL
demanda_parcial <<- NULL
lastUpdate <<- Sys.time() - (3*3600)
periodo <<- Sys.time()  
