library(lfltmagic)
library(hgchmagic)
library(tidyverse)
library(dsAppWidgets)

actores <- read_csv("data/clean/casos_all_data.csv")
casos <- read_csv("data/clean/casos_agregadas_data.csv")
notas <- read_csv("data/clean/notas_all_data.csv")
dic_casos <- read_csv("data/clean/casos_all_dic.csv")

basicos <- read_csv("data/aux/basicos.csv")
avanzados <- read_csv("data/aux/avanzadas.csv")
cruces <- read_csv("data/aux/cruces.csv")