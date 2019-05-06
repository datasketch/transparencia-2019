library(lfltmagic)
library(hgchmagic)
library(tidyverse)
library(dsAppWidgets)
library(geomagic)
library(lfltmagic)

actores <- read_csv("data/clean/casos_all_data.csv")
casos <- read_csv("data/clean/casos_agregadas_data.csv")

notas <- read_csv("data/clean/notas_all_data.csv")
dic_casos <- read_csv("data/clean/casos_all_dic.csv")

basicos <- read_csv("data/aux/basicos.csv")
avanzados <- read_csv("data/aux/avanzadas.csv")
cruces <- read_csv("data/aux/cruces.csv")


caso <- function(id){
  casos %>% dplyr::filter(id_caso == id) 
}


# función de mapas
map_c <- function(id){
  
  caso <- caso(id)
  data2 <- data.frame(id = caso$departamento, num = 100)
  
  
  p1 <- suppressWarnings(suppressMessages(  gg_choropleth_GnmNum(data = data2, mapName = "col_departments") +
                                              scale_fill_gradient(low = '#0095D4', high = '#0095D4') +scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
                                              guides(fill = FALSE))) + theme(plot.margin=unit(c(0,0,0,0),"mm"))  
  
 p1
}



getFicha <- function(id){
  
  message(id)
  if(is.null(id)) return()
  caso <- casos %>% dplyr::filter(id_caso == id) %>% as.list()
  
  template <- list(
    fluidRow(
      div(id="headsty",class="text-center",
          #column(3,
          #       "uiOutput('img_d')"),
          HTML(
            paste0('<div style="display: block; margin-top: -15%;margin-right: -10%;text-align: right;">',img(src = 'logo.jpg', style = "width: 180px;"),'</div>')
          ),
          column(12, 
                 HTML('<div>',  paste0(div(id = "tdesg",(toupper(caso$`nombre_hecho_de_corrupcion_(publico)`))), 
                                       div(id = "subdesg", (caso$`subtitulo_hecho_de_corrupcion_(publico)`)),'</div>','<br><br/>'
                 ))
          ))),
    fluidRow(
      div(id = "styhecho", class = "text-justify",
          column(12, 
                 HTML(
                   caso$hecho_de_corrupcion)
          )
      )),
    div(id = "bodysty",
        fluidRow(
          column(12,
                 HTML(paste0( '<center>',
                              plotOutput("map_d", height = "250px", width = "250px"),
                              '</center>'
                 ))
          )
        )),
    div(id = "bodysty",
    fluidRow(
      div(class='col-sm-12', style = 'margin-left: 14%;margin-left: 14%;padding: 0%;display: block;
          width: 70%;',
          HTML(paste0('<div class="container vertical-divider">
                      <div class="column one-third">
                      <table class="TFtable">',
                      '<td style="width: 50%;">',
                      div(id="colone",'LUGAR DEL HECHO:'),'</td>',
                      '<td style="width: 50%">', div(id="coltwo", toupper(caso$departamento)),
                      '</td>',
                      '</tr>',
                      '<tr>',
                      '<td style="text-align: center;">',
                      div(id ="colone",'FECHA DE INICIO:' ),
                      '</td>',
                      '<td>', 
                      div(id ="coltwo", ifelse(is.na(caso$ano_inicial_hecho), 'no disponible', caso$ano_inicial_hecho)),'</td>',
                      '</tr>',
                      '<tr>',
                      '<td>',div(id="colone",'ACTOR O ENTIDAD INVOLUCRADO:'), 
                      '</td>',
                      '<td>', div(id="coltwo",ifelse(is.na(caso$new), 'No disponible', caso$new)), '</td>',
                      '</tr>',
                      '<td>', div(id="colone",'TIPO DE CORRUPCIÓN:'),
                      '</td>',
                      '<td>', div(id="coltwo",ifelse(is.na(caso$tipo_corrupcion), 'No disponible', caso$tipo_corrupcion)),
                      '</td>',
                      '</tr>',
                      '</table>
                      </div>
                      <div class="column two-thirds"> ',
                      
                      '<table class="TFtable">',
                      
                      ifelse(is.na(caso$delito), 
                             HTML(paste0('<td style="width: 50%">',
                                         div(id="colone",'SECTOR AFECTADO:'),
                                         '</td>', 
                                         '<td>', 
                                         div(id="colone", style = "font-size: 14pt;", ifelse(is.na(caso$sector_afectado), 'No disponible', toupper(caso$sector_afectado))),
                                         '</td>',
                                         '</table>')),
                             HTML(paste0(    
                               '<td style="width: 50%">',
                               div(id="colone",'DELITO:'),'</td>',
                               '<td>', div(id = "coltwo", ifelse(is.na(caso$delito), 'No disponible', caso$delito)),
                               '</tr>',
                               '<td>',
                               div(id="colone", HTML('SECTOR AFECTADO:')),
                               '</td>', 
                               '<td>', 
                               div(id="colone", style = "font-size: 14pt;", ifelse(is.na(caso$sector_afectado), 'No disponible', toupper(caso$sector_afectado))),
                               '</td>',
                               '</table>'))),
                      '<table class="Fictable">',
                      '<tr>',
                      '<td style="width: 50%;border-top: none;">', 
                      '<p id="colend"> ENTIDAD DE <br/> CONOCIMIENTO: </p>',
                      '<p id = "colfib">', ifelse(is.na(caso$nombre_actor_autoridad), '', caso$nombre_actor_autoridad) , '</p>', '</td>',
                      '<td style="width: 50%;border-top: none;">',
                      '<p id="colend"> ESTADO JUDICIAL: </p>',
                      '<p id = "colfib">', ifelse(is.na(caso$situacion_judicial), '', caso$situacion_judicial), '</p>', 
                      '</td>',
                      '</tr>',
                      '</table>
                      </div>
                      </div>
                      ')))),
    fluidRow(
      div(id = "styhecho", align = 'right',
          ifelse(is.na(caso$fecha_del_historial), '',
                 HTML(paste0('Última actualización ', caso$fecha_del_historial))
          )
      )),
    fluidRow(
      br(),
      div(align = 'center',
          downloadButton('descarga_ficha', 'Descargar PDF')  
      )
    )#)
            )
    #     )
    #   )
          )
  
  template
}