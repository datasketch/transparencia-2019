library(hgchmagic)
library(dsAppWidgets)
library(shinycustomloader)
library(visNetwork)

actores <- read_csv("data/clean/casos_all_data.csv")
casos <- read_csv("data/clean/casos_agregadas_data.csv")

notas <- read_csv("data/clean/notas_all_data.csv")
dic_casos <- read_csv("data/clean/casos_all_dic.csv")

basicos <- read_csv("data/aux/basicos.csv")
avanzados <- read_csv("data/aux/avanzadas.csv")
cruces <- read_csv("data/aux/cruces.csv")
red <- read_csv("data/aux/red.csv")

caso <- function(id){
  casos %>% dplyr::filter(id_caso == id) 
}


map_c <- function(id){

  opts_map = list(
    border_color = "#CCCCCC",
    border_width = 1,
    null_color = "#dddddd",
  legend_show = F,
  map_navigation = F,
  showText = F,
  tooltip = list(headerFormat = NULL, pointFormat = "{point.name}"),
  colors = c('#c250c2', '#c250c2')
  )
  caso <- caso(id)
  data2 <- data.frame(id = caso$departamento, num = 100)


  p1 <- suppressWarnings(suppressMessages( 
    hgch_map_choropleth_GnmNum(data = data2,opts = opts_map)
  ))

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
            paste0(img(src = 'logo.png', style = "width: 150px;float: right;"))
          ),
          column(12, 
                 HTML( paste0(div(style = "font-size: 21px;font-weight: 700;margin-bottom: 3px;",(toupper(caso$nombre_publico))), 
                              div(style = "font-size: 19px;font-weight: 700;color: #F48323;", (caso$subtitulo_publico))
                 ))
          ))),
    fluidRow(
      div(style = "font-size: 15px;padding: 3%;height: 230px; overflow: auto;", 
                   caso$hecho)
      ),
    highchartOutput("map_d", height = 210),
    div(id = "bodysty",
    fluidRow(
      div(class='col-sm-12', 
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
                      div(id ="colone",'FECHA DEL HECHO:' ),
                      '</td>',
                      '<td>', 
                      div(id ="coltwo", ifelse(is.na(caso$ano_hecho), 'no disponible', caso$ano_hecho)),'</td>',
                      '</tr>',
                      '<tr>',
                      '<td>',div(id="colone",'ACTOR O ENTIDAD INVOLUCRADO:'), 
                      '</td>',
                      '<td>', div(id="coltwo",ifelse(is.na(caso$nombre_actor), 'No disponible', caso$nombre_actor)), '</td>',
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
                               '<td style="width: 34%;">',
                               div(id="colone",'DERECHO VULNERADO:'),'</td>',
                               '<td>', div(id = "coltwo", ifelse(is.na(caso$derecho_vulnerado), 'No disponible', caso$derecho_vulnerado)),
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
                      '<p id="colend" style = "font-weight: 600;"> ENTIDAD DE <br/> CONOCIMIENTO: </p>',
                      '<p id = "colfib" >', ifelse(is.na(caso$institucion), '', caso$institucion) , '</p>', '</td>',
                      '<td style="width: 50%;border-top: none;">',
                      '<p id="colend" style = "font-weight: 600;"> ESTADO JUDICIAL: </p>',
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