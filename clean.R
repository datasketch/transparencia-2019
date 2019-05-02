library(tidyverse)
library(jsonlite)


casosUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=casos"
notasUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=notas"
actoresUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=actores"

casos <- fromJSON(casosUrl)

inf <- c("id_tipo_actor_individual",         
         "id_tipo_actor_individual_nivel_2", 
         "id_tipo_actor_individual_nivel_3", 
         "id_tipo_actor_colectivo",          
         "id_tipo_actor_colectivo_nivel_2",  
         "id_tipo_actor_colectivo_nivel_3",  
         "tipo_actor_colectivo_nivel_3_cual",
         "tipo_actor_individual",           
         "tipo_actor_individual_nivel_2",    
         "tipo_actor_individual_nivel_3",    
         "tipo_actor_colectivo",             
         "tipo_actor_colectivo_nivel_2",     
         "tipo_actor_colectivo_nivel_3")
capture.output(a, file = "s.txt")
actores <- fromJSON(actoresUrl)


notas <- fromJSON(notasUrl)

a <- map(actores[inf], function(x) unique(x))

library(xlsx)
data <- read_excel("data/original/reporte_casos_20190409.xls")
