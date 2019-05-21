library(tidyverse)
library(openxlsx)
library(jsonlite)

casosUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=casos"
casos <- fromJSON(casosUrl)
casos <- map_df(casos, trimws)
casos <- casos %>% plyr::rename(c("sector" = "sector_afectado",
                                  "id_rango_dinero" = "rango_dinero",
                                  "id_derecho_vulnerado" = "derecho_vulnerado"))


casos$departamento[casos$departamento == "BOGOTÁ, DISTRITO CAPITAL"] <- "BOGOTA, D.C."
casos$departamento[casos$departamento == "GUAJIRA"] <- "LA GUAJIRA"
casos$departamento[casos$departamento == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
casos$departamento[casos$departamento == "VALLE"] <- "VALLE DEL CAUCA"

# Territorios de concentración/consolidación

territorio <- casos %>% 
                filter(caso_emblematico == "Territorios de concentración/consolidación")

# Informe II 2016-2018

info <- casos %>% 
          filter(caso_emblematico == "informe II 2016-2018")


info$derecho_vulnerado <- plyr::revalue(info$derecho_vulnerado,
                                           c("1" = "Derechos sociales, económicos y culturales",
                                             "2" = "Derechos fundamentales, civiles y políticos",
                                             "3" = "Derechos colectivos y del medio ambiente", 
                                             "4" = "No Aplica"))

info$autoridad_politica <- plyr::revalue(info$autoridad_politica,
                                         c("1" = "Sí",
                                           "0" = "No"))


write_csv(info, "app/data/clean/casos_all_data.csv")

func_paste <- function(x) paste(unique(x), collapse = '. ')

info <- info %>%
  group_by(id_caso) %>%
  summarise_each(funs(func_paste))



write_csv(info, "app/data/clean/casos_agregadas_data.csv")


# notasUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=notas"
# actoresUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=actores"
