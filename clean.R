library(tidyverse)
library(openxlsx)
library(jsonlite)

#casosUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=casos"
# notasUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=notas"
# actoresUrl <- "http://50.31.146.35/~wwwmonit/monitorCorrupcion/admin/api/reporte/vistasApi.php?flag=actores"
# casos <- fromJSON(casosUrl)
# actores <- fromJSON(actoresUrl)
casos <- read.xlsx("data/original/Base de datos limpia Hechos con actores.xlsx")
casos <- casos[c(-1,-2),]
names(casos) <- trimws(casos[1,])
casos <- casos[-1,]

names(casos) <- gsub("#", "Número de ", names(casos))
names(casos)[names(casos) == "Sector_2"] <- "Sector Afectado"                     
                  

labelCasos <- iconv(tolower(gsub("[[:space:]]", "_",names(casos))), to='ASCII//TRANSLIT')
labelCasos[labelCasos == "dinero_recuperado_y/o_multa_impuesta"] <- "dinero_recuperado"



dic_casos <- data.frame(label = names(casos),
                        id = labelCasos)

names(casos) <- labelCasos
casos <- Filter(function(z) !all(is.na(z)), casos)
varInf <- data.frame(id  = names(casos))
dic_casos <- varInf %>% left_join(dic_casos)

library(datafringe)
dic_casos$ctype <- getCtypes(casos)
casos <- map_df(casos, trimws)


casos <- casos %>% 
  map(function(z) {
    d <- gsub("No aplica|no aplica", "No Aplica", z)
    gsub("No disponible|no disponible", "No Disponible", d)
  }) %>% bind_rows()


write_csv(casos, "app/data/clean/casos_all_data.csv", na = "")
write_csv(dic_casos, "app/data/clean/casos_all_dic.csv",  na = "")

func_paste <- function(x) paste(unique(x), collapse = '. ')

casos <- casos %>%
           group_by(id_caso) %>%
             summarise_each(funs(func_paste))

write_csv(casos, "app/data/clean/casos_agregadas_data.csv", na = "")


# Notas 

notas <- read.xlsx("data/original/notas.xlsx")

casos_notas <- casos %>% select(id_caso)
notas$id_caso <- as.character(notas$id_caso)
notas_casos <- casos_notas %>% left_join(notas)

notas_casos <- notas_casos %>% select(id_caso, medio)
write_csv(notas_casos, "app/data/clean/notas_all_data.csv")

notas <- notas_casos %>%
          group_by(id_caso) %>%
            summarise_each(funs(func_paste))


# casos <- casos %>% 
#   map(function(z) {
#     d <- gsub("\\. NA|NA\\.|. NA|NA\\. |. NA|. NA", "", z)
#     d <- gsub(" No disponible|no disponible|No disponible|No aplica|\\. no aplica|no aplica|No Disponible|No Aplica|No Aplica\\.|No disponible\\.", "", d)
#     d <- gsub("\\.\\.|\\. \\.", ".", d)
#     d <- gsub("^\\.", "", d)
#     d <- trimws(gsub("NA", "", d))
#     d
#   }) %>% bind_rows()
# 
# write_csv(casos, "app/data/clean/casos_agregadas_data.csv", na = "")
# 
# d <- read_csv("app/data/clean/casos_agregadas_data.csv", na = c('', NA, '.','ND'))
# write_csv(d, "app/data/clean/casos_agregadas_data.csv", na = "")
# 
# 
# # write_csv(dic_casos, "app/data/clean/casos_dic.csv", na = "")
# 
# 
# # Limpieza casos ----------------------------------------------------------
# # se toma la de la URL, verificar
# # en esa al filtrar por territios de consolidación no hay id_derecho_vulnerado
# # en la de Angela al filtrar NA en tipo de corrupcion quedan solo 327 filas
# 
# casos <- casos %>% 
#             drop_na(tipo_corrupcion, nombrePublico)
# 
# 
# unique(casos$caso_emblematico)
# 
# casos <- casos %>% filter(caso_emblematico == "informe II 2016-2018")
# # casos <- casos %>%
# #            dplyr::filter(caso_emblematico == "Territorios de concentración/consolidación")
# 
# var_all <- names(casos)
# casos <- Filter(function(z) !all(is.na(z)), casos)
# var_diff <- setdiff(var_all, names(casos))
# # "dinero_recuperado"    "id_derecho_vulnerado" "forma_corrupcion"
# 
# unique(casos$nombre_actor)
# 
# casos$nombre_actor <- ifelse(casos$tipo_participacion == "Actor involucrado", casos$nombre_actor, NA)
# length(casos$cargo)
# casos$cargo <- ifelse(casos$tipo_participacion == "Actor involucrado", casos$cargo, NA)
# casos$institucion <- ifelse(casos$tipo_participacion == "Actor involucrado", casos$institucion, '')
# 
# func_paste <- function(x) paste(unique(x), collapse = '. ')
# 
# casos <- casos %>%
#           group_by(id_caso) %>%
#            summarise_each(funs(func_paste))
# 
# casos <- casos %>% 
#              map(function(z) {
#               d <- gsub("\\. NA|NA\\.|. NA|NA\\. |. NA|. NA", "", z)
#               d <- gsub(" No disponible|no disponible|No disponible|No aplica|\\. no aplica|no aplica|No Disponible|No Aplica|No Aplica\\.|No disponible\\.", "", d)
#               d <- gsub("\\.\\.|\\. \\.", ".", d)
#               d <- gsub("^\\.", "", d)
#               d <- trimws(gsub("NA", "", d))
#               d
#              }) %>% bind_rows()
# 
# write_csv(casos, "app/data/clean/casos_data.csv", na = "")
# casos <- read_csv("app/data/clean/casos_data.csv", na = c('', NA, '.','ND'))
# write_csv(casos, "app/data/clean/casos_data.csv", na = "")
# aa <- read_csv("app/data/clean/casos_data.csv")
#write_csv(dic_casos, "app/data/clean/casos_dic.csv", na = "")
# write_csv(casos, "data/clean/casos_data.csv", na = "")
# write_csv(dic_casos, "data/clean/casos_dic.csv", na = "")