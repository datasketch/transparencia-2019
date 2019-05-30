library(tidyverse)
library(openxlsx)
library(jsonlite)

casosUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=casos"
casos <- fromJSON(casosUrl)
casos <- map_df(casos, trimws)
casos <- casos %>% plyr::rename(c("sector" = "sector_afectado",
                                  "id_rango_dinero" = "rango_dinero",
                                  "id_derecho_vulnerado" = "derecho_vulnerado"))


casos$rango_dinero <- plyr::revalue(casos$rango_dinero,
                                         c("1" = "De 0 a 100 millones de pesos",
                                           "2" = "De 101 a 500 millones de pesos",
                                           "3" = "De 501 a 1000 millones de pesos", 
                                           "4" = "De 1001 a 5000 millones de pesos",
                                           "5" = "De 5001 a 10.000 millones de pesos",
                                           "6" = "Más de 10.000",
                                           "7" = "No aplica"))

casos$derecho_vulnerado <- plyr::revalue(casos$derecho_vulnerado,
                                        c("1" = "Derechos sociales, económicos y culturales",
                                          "2" = "Derechos fundamentales, civiles y políticos",
                                          "3" = "Derechos colectivos y del medio ambiente", 
                                          "4" = "No Aplica"))

casos$autoridad_politica <- plyr::revalue(casos$autoridad_politica,
                                         c("1" = "Sí",
                                           "0" = "No"))


casos$nombre_actor <- ifelse(casos$situacion_judicial == "Condenado penalmente" |
                               casos$situacion_judicial == "Inhabilitado disciplinariamente" |
                               casos$situacion_judicial == "Responsable fiscalmente" |
                               casos$situacion_judicial == "Sanción Fiscal" |
                               casos$situacion_judicial == "Suspendido disciplinariamente" |
                               casos$situacion_judicial == "Sancionado disciplinariamente", paste0(casos$nombre_actor, " (", casos$cargo, ")"), paste0("(", casos$cargo, ")"))


casos$departamento[casos$departamento == "BOGOTÁ, DISTRITO CAPITAL"] <- "BOGOTA"
casos$departamento[casos$departamento == "GUAJIRA"] <- "LA GUAJIRA"
casos$departamento[casos$departamento == "NORTE SANTANDER"] <- "NORTE DE SANTANDER"
casos$departamento[casos$departamento == "VALLE"] <- "VALLE DEL CAUCA"
casos$departamento[is.na(casos$departamento)] <- "NACIONAL"
# Territorios de concentración/consolidación

casos_app <- casos %>% 
                filter(caso_emblematico %in% c("Territorios de concentración/consolidación", "informe II 2016-2018"))




write_csv(casos_app, "app/data/clean/casos_all_data.csv")

casos_app$nombre_actor[is.na(casos_app$nombre_actor)] <- "-"

func_paste <- function(x) paste(unique(x), collapse = '. ')

casos_app <- casos_app %>%
  group_by(id_caso) %>%
  summarise_each(funs(func_paste))

casos_app$nombre_actor <- gsub("-.|-", "", casos_app$nombre_actor)

write_csv(casos_app, "app/data/clean/casos_agregadas_data.csv")


# notasUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=notas"
# actoresUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=actores"
