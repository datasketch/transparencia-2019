shinyServer(function(input, output, session) {
  
  # Datos para graficar
  output$base_opts <- renderUI({
    data_dis <- setNames(c("Territorios de concentración/consolidación", "informe II 2016-2018"), c("Territorios de paz", "Hechos 2016-2018"))
    checkboxGroupInput("base_cliente", HTML("<div style='font-weight: 700;'>BUSCAR POR</div>"), choices = data_dis, inline = T, selected = "informe II 2016-2018")
  })
  
  
  # bases
  casos_data <- reactive({
    tema <- input$base_cliente
    d <- casos %>% dplyr::filter(caso_emblematico %in% tema)
    d
  })
  
  
  actores_data <- reactive({
    tema <- input$base_cliente
    d <- actores %>% dplyr::filter(caso_emblematico %in% tema)
    d <- d %>% distinct(id_actor, .keep_all = T)
    d
  })
  
  
  #valor absoluto o relativo
  output$operacion <- renderUI({
    checkboxInput("tipo_agg", "Valor Total/Porcentaje")
  })
  
  # Graficos para opt básicas y avanzadas
  output$vizOptions <- renderUI({
    charts <- c("map", "barrash", "barras" ,"bubbles", "pie", "lines")
    buttonImage(id = "last_chart", charts, charts, file = "icons/", format = "svg", classImg = "imgStyle")
  })
  
  # Opciones Básicas
  
  output$basicos <- renderUI({
    
    l <- purrr::map(1:nrow(basicos), function(z){
      actionButton(inputId = basicos[z,]$id, label = basicos[z,]$preguntas, class = "needed")
    })
    l[[1]] <- gsub("needed", "needed basic_active", l[[1]])
    l[[1]] <- HTML(paste0(paste(l[[1]], collapse = '')))
    
    div(class = "opciones_basicas",
        tags$button(id = "Basico", class = "click_option",  HTML("<div id='opts' class = 'active_opt'>&gt;</div> OPCIONES BÁSICAS")),
        div(id = "cont_basicas",
            HTML("<div style='font-size:15px;margin-top: 9px;'>Visualizar</div>"),
            div(class = "preguntas_basicas",
                l
            )))
  })
  
  # variables según base
  #variables_avanzadas <- reactiveValues(var = NULL)
  
  outVar <- reactive({
    data_sel <- input$base_sel
    if (is.null(data_sel)) return()
    
    chart <- input$last_chart
    
    df <- avanzados %>% 
      dplyr::filter(base == data_sel)
    
    if(chart != "lines") {
      df <- df 
    } else {
      df <- df %>% 
        dplyr::filter(variables_id != c('ano_final_hecho', 'ano_hecho'))
    }   
    
    setNames(df$variables_id, df$variables_label)
  })  
  
  observe({
    updateSelectizeInput(session, "var_principal",
                         choices = outVar())
    })
 
  
  outCross <- reactive({
    viz_sel <- input$last_chart
    if (is.null(viz_sel)) return()
    var_cho <- input$var_principal
    if (is.null(var_cho)) return()
    
    sometimes <- data.frame(org = c('departamento'),
                            var_label = c('Departamento'))
    
    if (viz_sel == "barras" | viz_sel == "barrash" | viz_sel == "bubbles"){
      df <- cruces %>% dplyr::filter(var_sel == var_cho)
      if (nrow(df) == 0) {
        l <- sometimes
      } else {
        l <- df %>% drop_na()
      }
    } else if (viz_sel == "map") {
      l <- sometimes
    } else if (viz_sel == "lines") {
      l <- cruces %>% dplyr::filter(plot == "lineas")
    } else {
      l <- NULL
    }
    setNames(as.character(l$org), as.character(l$var_label))
  })
  
  
  observe({
    updateSelectizeInput(session, "var_cruce",
                         choices = outCross())
    updateSelectizeInput(session, "var_principal",
                         selected = input$var_principal)
    })
  
  # Opciones avanzadas
  
  output$avanzadas <- renderUI({
    div(class = "opciones_avanzadas",
        tags$button(id = "Avanzada", class = "click_option",  HTML("<div id='opts_a'>&gt;</div> OPCIONES AVANZADAS")),
        div(id = "cont_avanzada", class = "hideOptions",
            radioButtons(inputId = "base_sel", label = "Información de", c('Hechos', 'Actores'), inline = T),
            selectizeInput(inputId = 'var_principal', label = "Visualizar", choices = NULL, options = list(
              #placeholder = 'Selecciona una variable para visualizar',
              #onInitialize = I('function() { this.setValue(""); }'),
              plugins= list('remove_button')
            )),
            selectizeInput(inputId = 'var_cruce', label = "organizado por", choices = NULL,options = list(
              placeholder = 'Selecciona una variable para cruzar',
              onInitialize = I('function() { this.setValue(""); }'),
              plugins= list('remove_button')
            ))
        )
    )
  })
  
  
  output$red_var <- renderUI({
    varRed <- setNames(red$id, red$label)
    selectizeInput("var_red", "Selecciona variable para filtros", varRed,
                   options = list(
                     placeholder = 'Sin filtros',
                     onInitialize = I('function() { this.setValue(""); }'),
                     plugins= list('remove_button')))
  })
  
  output$red_cat <- renderUI({
    
    variable_red <- input$var_red
    if (variable_red != "") {
      dt <- actores_data()[[variable_red]]
      categorias_red <- as.character(unique(dt))
    } else {
      categorias_red <- NULL 
    }
    selectizeInput("cat_red", "Selecciona categoria(s) para filtrar", choices = categorias_red, 
                   #multiple = TRUE,
                   options = list(
                     placeholder = 'Todas',
                     onInitialize = I('function() { this.setValue(""); }'),
                     plugins= list('remove_button'))
    )
  })
  
  output$red <- renderUI({
    div(class = "opciones_red",
        tags$button(id = "Red", class = "click_option",  HTML("<div id='opts_r'>&gt;</div> LA RED DE LA CORRUPCIÓN")),
        div(id = "cont_red", class = "hideOptions",
            uiOutput("red_var"),
            uiOutput("red_cat")
        ))
  })
  
  
  data_red <- reactive({
    var_red_sel <- input$var_red
    var_cat_sel <- input$cat_red
    if (var_cat_sel == "" | is.null(var_cat_sel)) {
      dt <- actores_data()
    } else {
      dt <-  actores_data()[actores_data()[, var_red_sel] == c(var_cat_sel), ]
    }
    
    casosAll <- dt
    situacion <- c("Sancionado disciplinariamente","Condenado penalmente", "Sanción Fiscal")
    
    casosAll <- map_df(situacion, function(x) casosAll[grepl(x, casosAll$situacion_judicial),] )
    
    func_paste <- function(x) paste(unique(x), collapse = ', ')
    casosRed <- casosAll %>%
      group_by(id_caso) %>%
      dplyr::summarise_each(funs(func_paste))
    
    net <- casosAll[c("id_caso","nombre_publico","id_actor","nombre_actor")]
    net$cid <- paste0("c",net$id_caso)
    net$aid <- paste0("a",net$id_actor)
    net
  })
  
  edges <- reactive({
    net <- data_red()
    edges <- net[c("cid","aid")]
    names(edges) <- c("from","to")
    edges
  })
  
  nodes <- reactive({
    net <- data_red()
    nodesCaso <- net[c("cid","nombre_publico")]
    names(nodesCaso) <- c("id","label")
    nodesCaso$type <- "Caso"
    nodesActor <- net[c("aid","nombre_actor")]
    names(nodesActor) <- c("id","label")
    nodesActor$type <- "Actor"
    
    nodes <- bind_rows(
      nodesActor,
      nodesCaso
    ) 
    nodes$color <- "#fa8223"
    nodes$color[nodes$type == "Caso"] <- "#f03a47"
    
    nodes$size <- 30
    nodes$size[nodes$type == "Caso"] <- 50
    
    nodes <- nodes %>% distinct(id,.keep_all = TRUE)
    nodes
  })
  
  
  output$netViz <- renderVisNetwork({
    viz <- visNetwork(edges = edges(), nodes = nodes(), width = "100%") %>% 
      visInteraction(navigationButtons = TRUE) %>% 
      visEvents(
        click = "function(nodes) {
        console.info('click')
        console.info(nodes)
        Shiny.onInputChange('clickRed', {nodes : nodes.nodes[0]});
        ;}"
      ) %>% 
      visPhysics(stabilization= FALSE)  
    viz
  })
  

  data_viz <- reactive({
    l_o <- input$last_option 
    if (is.null(l_o)) l_o <- "Basico"
    
    if (l_o == "Basico") {
      q_sel <- input$last_click
      if (is.null(q_sel)) q_sel <- 'q1'
      
      dt_bs <- basicos %>% dplyr::filter(id == q_sel)
      
      var_sel <- dt_bs$variable
      
      var_sel <- c("id_caso", var_sel)
      
      
      if (dt_bs$base == 'notas') {
        dt <- notas
      } else if (dt_bs$base == 'casos') {
        dt <- casos_data()[var_sel]
        if (dt_bs$variable == 'delito') {
          dt <- separate_rows(dt, delito, convert = TRUE, sep = ",")
          dt$delito <- trimws(dt$delito)
        }
      } else {
        dt <- actores_data() %>% dplyr::filter(tipo_participacion == 'Actor involucrado')
        dt <- dt[var_sel]
      }
    } else if (l_o == "Avanzada"){
      base <- input$base_sel
      if (is.null(base)) return()
      
      var_prim <- input$var_principal
      if (is.null(var_prim)) return()
      var_cruce <- input$var_cruce
      if (is.null(var_cruce)) return()
      click_chart <- input$last_chart
      
      if (click_chart == "pie" | click_chart == "map") var_cruce <- ""
      
      
      if (base == "Hechos") {
        
        if (var_cruce == ""){
          dt <- casos_data() %>% dplyr::select_('id_caso', var_prim)
        } else {
          dt <- casos_data() %>% dplyr::select_('id_caso', var_prim, var_cruce)
        }
        
      } else {
        if (var_cruce == ""){
          dt <- actores_data() %>% dplyr::select_('id_caso', var_prim)
        } else {
          dt <- actores_data() %>% dplyr::select_('id_caso', var_prim, var_cruce)
        }
      }
      
      if (var_prim == 'delito') {
        dt <- separate_rows(dt, delito, convert = TRUE, sep = ",")
        dt$delito <- trimws(dt$delito)
      }
      
    } else {
      dt <- actores_data()
    }
    dt
  })
  
  
  # Titulos
  
  data_titles <- reactive({
    l_o <- input$last_option 
    if (is.null(l_o)) l_o <- "Basico"
    base_sel <- input$base_cliente
    
    if (l_o == "Basico") {
      q_sel <- input$last_click
      if (is.null(q_sel)) q_sel <- 'q1'
      if (length(base_sel) == 1) {
        if (base_sel == "Territorios de concentración/consolidación") {
          title <- basicos$titulos_dos[basicos$id == q_sel]
        } else {
          title <- basicos$titulos_uno[basicos$id == q_sel]
        }} else {
          title <- basicos$titulos_tres[basicos$id == q_sel]
        }
      
    } else if (l_o == "Avanzada") {
      var_prim <- input$var_principal
      var_cruce <- input$var_cruce
      if (!is.null(var_prim) & (is.null(var_cruce) | var_cruce == "")) {
        var_avz <- dic_casos$label[dic_casos$id == var_prim]
      } else {
        var_avz <- paste0(dic_casos$label[dic_casos$id == var_prim], ' y ', dic_casos$label[dic_casos$id == var_cruce])
      }
      base_avz <- input$base_sel
      if (base_avz == 'Hechos') {
        if (length(base_sel) == 1) {
          if (base_sel == "Territorios de concentración/consolidación") {
            title <- paste0('Hechos de corrupción investigados y reportados por la prensa en  territorios de paz según ', var_avz, ' (2016-2018).')
          } else {
            title <- paste0('Hechos de corrupción investigados y reportados por la prensa en Colombia según ', var_avz, ' (2010-2016).')
          }} else {
            title <- paste0('Hechos de corrupción investigados y reportados por la prensa en Colombia según ', var_avz, ' (2010-2018).')
          }
      } else {
        if (length(base_sel) == 1) {
          if (base_sel == "Territorios de concentración/consolidación") {
            title <- paste0('Actores involucrados en hechos de corrupción investigados y reportados por la prensa en territorios de paz según ', var_avz, ' (2016-2018).')
          } else {
            title <- paste0('Actores involucrados en hechos de corrupción investigados y reportados por la prensa en Colombia según ', var_avz, ' (2010-2016).')
          }} else {
            title <- paste0('Actores involucrados en hechos de corrupción investigados y reportados por la prensa en Colombia según ', var_avz, ' (2010-2018).')
          }
      }
    } else {
      title <- ' '
    }
    title
    
  })
  
  
  capt <- reactive({
    base_sel <- input$base_cliente
    
    if (length(base_sel == 1)) {
      if (base_sel == "Territorios de concentración/consolidación") {
        cp <- "<span style='font-size: 11px;margin-top: 2px; color:#000000'>Fuente: Monitor Ciudadano de la Corrupción 2017</span>"
      } else {
        cp <- "<span style='font-size: 11px;margin-top: 2px; color:#000000'>Fuente: Monitor Ciudadano de la Corrupción 2019</span>" 
      }} else {
        cp <-  "<span style='font-size: 11px;margin-top: 2px; color:#000000'>Fuente: Monitor Ciudadano de la Corrupción 2017-2019</span>"
      }
    cp 
  })
  
  nom_base <- reactive({
    l_o <- input$last_option 
    if (is.null(l_o)) l_o <- "Basico"
    if (l_o == "Basico") {
      q_sel <- input$last_click
      if (is.null(q_sel)) q_sel <- 'q1'
      tx <- gsub("casos", "hechos", basicos$base[basicos$id == q_sel])
    } else if (l_o == "Avanzada") {
      base_avz <- input$base_sel
      if (base_avz == 'Hechos') {
        tx <- "hechos"
      } else {
        tx <- "actores"
      }
    } else {
      return()
    }
    
    tx
  })
  
  # Graficos (basicos y avanzados)
  
  output$viz_hgch <- renderHighchart({
    
    click_chart <- input$last_chart
    
    if (is.null(click_chart)) return()
    
    click_chart <- gsub('lines', 'line',  click_chart)
    
    type_agg <- input$tipo_agg
    add_p <- "Número"
    if (type_agg) add_p <- "Porcentaje"
    
    orientacion <- 'ver'
    horLabel <- NULL
    verLabel <- paste0(add_p, " de ", nom_base())
    
    
    
    if (click_chart == 'barrash'){
      orientacion <- 'hor'
      horLabel <- paste0(add_p, " de ", nom_base())
      verLabel <- NULL
    } 
    
    click_chart <- gsub('barras|barrash', 'bar', click_chart)
    
    
    
    df <- data_viz() %>% select(-id_caso)
    
    
    if (grepl("actor_individual",names(df))) {
      df$tipo_actor_individual[df$tipo_actor_individual == "No Aplica"] <- NA
      df <- df %>% drop_na()
    }
    
    if (grepl("actor_colectivo",names(df))) {
      df$tipo_actor_colectivo[df$tipo_actor_colectivo == "No Aplica"] <- NA
      df <- df %>% drop_na()
    }
    
    if(click_chart == 'map' & length(names(df)) == 1 & grepl("departamento", names(df))) {
      viz <- 'hgch_map_choropleth_GnmNum'
      df <- df %>% dplyr::group_by_all() %>% dplyr::summarise(Total = n())
    } else if (click_chart == 'map') {
      viz <- 'hgch_map_bubbles_CatGltGlnNum'
      dt_cor <- casos_data() %>% select(id_caso, latitud, longitud)
      #dt_cor$latitud <- dt_cor$latitud + runif(nrow(dt_cor), -1, 1)
      df <- left_join(data_viz(), dt_cor, by = "id_caso")
      df <- df[,c(-1)]
      df <- df %>% group_by_all() %>% dplyr::summarise(Total = n())
    } else {
      typeDt <- 'Cat'
      if (ncol(df) != 1) typeDt <- 'CatCat'
      viz <- paste0('hgch_', click_chart, '_', typeDt)
      df <- df
    }
    
    line_width <- 2
    if (click_chart == 'map' | click_chart == 'bubbles') {
      line_width <- 0
    }
    
    colors <- "#fdb731"
    colSc <- "no"
    
   
   if (click_chart == 'pie' | click_chart == 'map') {
      colSc <- "discrete"
      colors <- c("#fdb731","#0a446b", "#137fc0", "#c250c2", "#fa8223", "#64c6f2", "#f49bf9", "#fc7e5b", "#ffe566", "#64f4c8", "#137fc0", "#c250c2", "#f03a47", "#fdb731", "#36c16f", "#022f40", "#691f6b", "#931c4d", "#fa8223", "#2b6d46")
    }
    
    dic <- data.frame(id = as.character(names(df)))
    dic <- dplyr::left_join(dic, dic_casos)
    
    show_text <- TRUE
    
    if (click_chart != "map") {
      df <- datafringe::fringe(df, dic)
    } else {
      df <- df
      show_text = FALSE
    }
    
    myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}")
    
    if (click_chart == 'line') myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
   
    if (sum(grep('Cat', getCtypes(df))) >= 3 & click_chart != 'line') {
      colSc <- "discrete"
      colors <- c("#fdb731","#0a446b", "#137fc0", "#c250c2", "#fa8223", "#64c6f2", "#f49bf9", "#fc7e5b", "#ffe566", "#64f4c8", "#137fc0", "#c250c2", "#f03a47", "#fdb731", "#36c16f", "#022f40", "#691f6b", "#931c4d", "#fa8223", "#2b6d46")
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, cat:this.name, timestamp: new Date().getTime()});}")
    }
     
    nDg <- 0
    if (type_agg) nDg <- 2
    
    opts_viz <- list(
      title = HTML(paste0(as.character(data_titles())), collapse = ''),
      subtitle = NULL,
      caption = capt(),
      horLabel = horLabel,
      verLabel = verLabel,
      orientation = orientacion,
      colors = colors,
      color_scale = colSc,
      percentage = type_agg,
      marks = c(".", ","),
      nDigits = nDg,
      dropNa = FALSE,
      sort = "desc",
      clickFunction = myFunc,
      labelWrap = 150,
      bubble_min = '1%',
      bubble_max = '2%',
      showText = show_text,
      allow_point = TRUE,
      cursor =  'pointer',
      color_hover = "#fa8223",
      color_click  = "#fa8223",
      labelWrapV = c(30, 30),
      legend_position  = "center",
      startAtZero = TRUE,
      spline = FALSE,
      fill_opacity = 0.9,
      agg_text = " ",
      export =  TRUE,
      border_color = '#000000',
      theme = tma(custom = list(stylesX_lineWidth = 0, 
                                height = 570,
                                showText = show_text,
                                colors = colors,
                                font_family = "Raleway",
                                font_size = '11px',
                                font_color = '#000000',
                                line_width = line_width,
                                stylesTitleY_fontWeight = 'bold',
                                stylesTitleX_fontWeight = 'bold'))
    )
    
    do.call(viz, c(list(data = df, opts = opts_viz)))
    
  })
  
  
  
  data_ficha <- reactive({
    df <- data_viz() 
    
    if (is.null(df) | nrow(df) == 0) return()  
    l_o <- input$last_option 
    if (is.null(l_o)) l_o <- "Basico"
    if (l_o != "Red") {
      var1 <- input$hcClicked$id
      
      if (is.null(var1)) return()
      var1 <- gsub('<br/>', ' ', var1)
      if (sum(grepl("departamento", names(df))) == 1) {
        var1 <- toupper(iconv(input$hcClicked$id, to = "ASCII//TRANSLIT"))
        var1 <- ifelse(var1 == 'NARINO', 'NARIÑO', var1)
      }
      
      var2 <- input$hcClicked$cat
      
      
      if (is.null(var2)) {
        var_sel <- names(df)[2]
        dta <- df[df[var_sel] == var1,]
      } else {
        var2 <- gsub('<br/>', ' ', var2)
        var_sel_uno <- names(df)[2]
        var_sel_dos <- names(df)[3]
        dta <- df[df[var_sel_uno] == var2 & df[var_sel_dos] == var1,]
      }
      
      dt <- dta %>% drop_na() %>% distinct(id_caso)
      dt <- dt %>% left_join(casos)
    } else {
      click_red <- input$clickRed
      if (is.null(click_red)) return()
      click_red_mod <- gsub('[a-z]', '', click_red)
      if(grepl('c', click_red)) {
        dt <- casos_data() %>% filter(id_caso %in% click_red_mod)
      } else {
        dt <- actores_data() %>% filter(id_actor %in% click_red_mod)
      }
    }
    dt
  })
  output$bla <- renderPrint({
    data_ficha()
  })
  
  output$ficha_peque <- renderUI({
    info <- data_ficha()
    l_o <- input$last_option 
    print(l_o)
    if (is.null(l_o)) l_o <- "Basico"
    if (l_o != "Red") {
      txt <- HTML(
        '<div class = "indicacion"><img src="click/click.svg" style="width: 50px; display:block;margin-left: 40%;"/>
    <b>A.</b> En esta sección podrás acceder a diferentes tipos de visualización por medio de una búsqueda básica con preguntas o avanzada que permite el cruce de una o más  variables de tu interés.</br> 
    <b>B.</b> Puedes descargar las gráficas que desees haciendo click en el botón de descarga </br>
    <b>C.</b> Al hacer click en los datos de la visualización de tu interés también podrás acceder al listado de hechos de corrupción asociados a la variable. Puedes acceder a todo el contenido de la ficha del hecho haciendo click en "ver más".
    </div>')
    } else {
      txt <- HTML(
        '<div class = "indicacion"><img src="click/click.svg" style="width: 50px; display:block;margin-left: 40%;"/>
      <br/>
      <b>¿Cómo explorar la red?</b>
      <p>En esta red podrás ver el los actores involucrados 
      en los hechos de corrupción reportados por la prensa.
      Al seleccionar los iconos fucsia podrás conocer los hechos de corrupción en el que se vio involucrado el actor.
      Al seleccionar los iconos naranja podrás conocer el detalle de los actores involucrados  sancionados por los hechos.</p>
      <b>¿Qué puedo hacer en esta sección?</b>
      <p>Conocer la red de actores involucrados en cada hecho
      de corrupción registrado y su historial judicial.</p>
      </div>'
      )
    }
    if (is.null(info)) return(txt)
    if(nrow(info) == 0) txt <- txt
    
    
    
    filas <- nrow(info)
    if (filas == 0) return(txt)
    print('hola')
    print(info)
    if (l_o != 'Red') {
      tx_tit <- input$hcClicked$id
      if (is.null(tx_tit)) tx_tit <- input$hcClicked$cat
      if (is.null(tx_tit)) return()
      HTML(paste0('<span style="font-size:15px;">', tx_tit, '</span>'))
      txt <- div(
      tx_tit,
      purrr::map(1:filas, function(x){
        div(class = "ficha",
            div(class = "cont_info",
                HTML(paste0('<div class = "title_ficha">',info$nombre_publico[x], '</div>')),
                HTML(paste0('<div class = "sub_ficha">',info$subtitulo_publico[x], '</div>'))),
            tags$button(id = info$id_caso[x], class = "click_ficha",  "Ver más")
        )
      })) } else {
         click_red <- input$clickRed
         print(click_red)
      if (is.null(click_red)) return()
      click_red_mod <- gsub('c|a', '', click_red)
      if(grepl('c', click_red)) {
        txt <- purrr::map(1:filas, function(x){
          div(class = "ficha",
              div(class = "cont_info",
                  HTML(paste0('<div class = "title_ficha">',info$nombre_publico[x], '</div>')),
                  HTML(paste0('<div class = "sub_ficha">',info$subtitulo_publico[x], '</div>')),
                  HTML(paste0('<div class = "info_red"><b>Lugar del hecho:</b> ', info$departamento[x],
                              '</br><b>Año del hecho:</b> ', info$ano_hecho[x],
                              '</br><b>Tipo de corrupción:</b> ', info$tipo_corrupcion[x],
                              '</br><b>Sector afectado:</b> ', info$sector_afectado[x],
                              '</div>'))),
              tags$button(id = info$id_caso[x], class = "click_ficha",  "Ver más")
          )
        })} else {
          txt <- purrr::map(1:filas, function(x){
            div(class = "ficha",
                div(class = "cont_info",
                    HTML(paste0('<div class = "title_ficha">', info$nombre_actor[x], '</div>')),
                    HTML(paste0('<div class = "info_red"><b>Tipo de investigación:</b> ', info$tipo_investigacion[x],
                                '</br><b>Institución:</b> ', info$institucion[x],
                                '</br><b>Situación judicial:</b> ', info$situacion_judicial[x],
                                '</div>'))),
                tags$button(id = info$id_caso[x], class = "click_ficha",  "Ver más")
            )
          })
        }
      }
   # div(
    #tx_tit,
    txt
    #)
  })
  
  fichaInfo <- reactive({
    
    id <- input$last_case
    
    if(is.null(id)) return()
    getFicha(id)
  })
  
  output$ficha <- renderUI({
    #list(
    fichaInfo()#,
    #br(),
    #downloadButton('descarga_ficha', 'Descargar')
    #)
  })
  
  observeEvent(input$last_case, {
    showModal(modalDialog(
      title = '',
      size = 'l',
      easyClose = TRUE,
      footer = modalButton("Cerrar"), 
      uiOutput('ficha'), 
      br()
    )
    )
  })
  
  
  output$map_d <- renderHighchart({
    id <- input$last_case
    if(is.null(id)) return()
    map_c(id)
    
  })
  
  
  
  output$descarga_ficha <-
    downloadHandler(
      "results_from_shiny.pdf",
      content =
        function(file)
        {
          id <- input$last_case
          caso_i <- caso(id)
          params <- list(
            id = id,
            title =  gsub("\"","'",caso_i$nombre_publico),
            subtitle =  gsub("\"","'",caso_i$subtitulo_publico),
            mapc = map_c(id),
            abstract =  gsub("\"","'",caso_i$hecho),
            lugar = toupper(caso_i$departamento),
            inicio = ifelse(is.na(caso_i$ano_hecho), 'No disponible', caso_i$ano_hecho),
            actor = ifelse(is.na(caso_i$nombre_actor), 'No disponible', caso_i$nombre_actor),
            tipo =  ifelse(is.na(caso_i$tipo_corrupcion), 'No disponible', caso_i$tipo_corrupcion),
            delito = ifelse(is.na(caso_i$derecho_vulnerado), '', caso_i$derecho_vulnerado),
            sector = toupper(caso_i$sector_afectado),
            # # dinero =  as.character(ifelse(is.na(caso_i$dinero_juego), 'No disponible', paste0(' $',  format(caso_i$dinero_juego, nsmall= 0, big.mark=",")))),
            entidad = ifelse(is.na(caso_i$institucion), '', caso_i$institucion),
            estado = ifelse(is.na(caso_i$situacion_judicial), '', caso_i$situacion_judicial),
            fecha = ifelse(is.na(caso_i$fecha_informacion_actualizada), '', as.character(caso_i$fecha_informacion_actualizada))
          )
          rmarkdown::render("temp_latex/untitle.Rmd",
                            #output_format = pdf_document(template="default.tex"),
                            params = params,
                            output_file = "built_report.pdf")
          
          readBin(con = "temp_latex/built_report.pdf",
                  what = "raw",
                  n = file.info("temp_latex/built_report.pdf")[, "size"]) %>%
            writeBin(con = file)
        },
      contentType = 'temp_latex/built_report.pdf'
    )
  
  
  output$viz_res <- renderUI({
    l_o <- input$last_option 
    if (is.null(l_o)) l_o <- 'Basico'
    
    if (l_o != 'Red') {
      v <- highchartOutput("viz_hgch", height = 'auto')
    } else {
      v <- visNetworkOutput("netViz", height = 550)
    }
    v
  })
  

  
  # Salida Panel Uno
  output$panel_1 <- renderUI({
    div(
      uiOutput('base_opts'),
      uiOutput('basicos'),
      uiOutput('avanzadas'),
      div(id = "type_viz",
          HTML("<div style = 'margin-top:15px; font-size:15px;font-weight: 700;'>Tipo de visualización </div>"),
          uiOutput('operacion'),
          uiOutput('vizOptions')
      ),
      uiOutput('red')
    )
  })
  
  # Salidad Panel Dos
  
  output$panel_2 <- renderUI({
    div(class = "viz_out", style = "height: 100%; margin-top: 11px;",
        HTML("<div class = 'title_panel'>VISUALIZACIÓN</div>"),
        uiOutput('title_viz'),
        withLoader(uiOutput('viz_res'), type = "html", loader = "loader10")
        
    )
  })
  
  # Salida Panel Tres
  output$panel_3 <- renderUI({
    div(class = "tj_out",
        HTML("<div class = 'title_panel'>HECHOS</div>"),
        div(class = "content_ficha_mini",
            #verbatimTextOutput('bla')
            uiOutput("ficha_peque")
        )
    )
  })
  
})