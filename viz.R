library(hgchmagic)


hechos_data <- read_csv("data/clean/casos_data.csv")
hechos_dicc <- read_csv("data/clean/casos_dic.csv")

func_paste <- function(x) paste(unique(x), collapse = '. ')
casos <- hechos_data %>%
           group_by(id_caso) %>%
             summarise_each(funs(func_paste))
# ¿Cuántos hechos de corrupción han sido investigados y 
#reportados por la prensa?

num_hechos <- casos %>% 
                group_by(tipo_de_investigacion) %>% 
                  summarise(cont = n())


# Dos mapas
# 1. coropleta

library(leaflet)


# 2. puntos lat lon coloreados por categoria