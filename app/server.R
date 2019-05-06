shinyServer(function(input, output, session) {
  
  # Opciones Básicas
  
  output$basicos <- renderUI({
    
    l <- map(1:nrow(basicos), function(z){
      actionButton(inputId = basicos[z,]$id, label = basicos[z,]$preguntas, class = "needed")
    })
    l[[1]] <- gsub("needed", "needed active", l[[1]])
    l[[1]] <- HTML(paste0(paste(l[[1]], collapse = '')))
    
    div(class = "opciones_basicas",
    tags$button(id = "Basico", class = "click_option",  HTML("<div id='opts' class = 'active_opt'>&gt;</div> OPCIONES BÁSICAS")),
    HTML("Visualizar"),
    div(class = "preguntas_basicas",
    l
    ))
  })
 
  
  # variables según base
  #variables_avanzadas <- reactiveValues(var = NULL)
  
  outVar <- reactive({
    data_sel <- input$base_sel
    if (is.null(data_sel)) return()
    
    chart <- input$last_chart
    
    df <- avanzados %>% filter(base == data_sel)
    
    if(chart != "lines") {
      df <- df 
    } else {
      df <- df %>% filter(variables_id != c('ano_final_hecho', 'ano_inicial_hecho'))
    }   
    
    setNames(df$variables_id, df$variables_label)
  })
  
  observe({
    updateSelectInput(session, "var_principal",
                      choices = outVar()
    )})
  
  
  
  outCross <- reactive({
    viz_sel <- input$last_chart
    if (is.null(viz_sel)) return()
    var_cho <- input$var_principal
    if (is.null(var_cho)) return()

    sometimes <- data.frame(org = c('none','departamento', 'region', 'municipio'),
                            var_label = c(' ', 'Departamento', 'Region', 'Municipio'))
    
    if (viz_sel == "barras" | viz_sel == "barrash"){
      df <- cruces %>% filter(var_sel == var_cho)
      if (nrow(df) == 0) {
        l <- sometimes
      } else {
        l <- bind_rows(data.frame(org = c('none'), var_label = c(' ')),df)
      }
    } else if (viz_sel == "map") {
      l <- sometimes[-1,]
    } else if (viz_sel == "lines") {
      l <- cruces %>% filter(plot == "lineas")
    } else {
      l <- NULL
    }
    setNames(as.character(l$org), as.character(l$var_label))
  })
  

  observe({
    updateSelectInput(session, "var_cruce",
                      choices = outCross()
    )})
  
  # Opciones avanzadas
  
  output$avanzadas <- renderUI({
    div(class = "opciones_avanzadas",
        tags$button(id = "Avanzada", class = "click_option",  HTML("<div id='opts_a'>&gt;</div> OPCIONES AVANZADAS")),
        radioButtons(inputId = "base_sel", label = "Información de", c('Hechos', 'Actores'), inline = T),
        selectInput(inputId = 'var_principal', label = "Visualizar", choices = NULL),
        selectInput(inputId = 'var_cruce', label = "organizado por", choices = NULL)
    )
  })
  
  # Gráficos
  
  output$vizOptions <- renderUI({
    charts <- c("barrash", "barras" ,"treemap", "map", "pie", "lines")
    buttonImage(id = "last_chart", charts, charts, file = "icons/", format = "svg", classImg = "imgStyle")
  })
  
  
  data_viz <- reactive({
    l_o <- input$last_option 
    if (is.null(l_o)) l_o <- "Basico"
    
    if (l_o == "Basico") {
      q_sel <- input$last_click
      if (is.null(q_sel)) q_sel <- 'q1'
      dt_bs <- basicos %>% filter(id == q_sel)
      
      var_sel <- c(dt_bs$variable)
      
      if (var_sel == 'delito') var_sel <- paste0('delito_', 1:7)
      if (input$last_chart == "map" & q_sel != 'q3') var_sel <- c(var_sel, 'departamento')
      
      var_sel <- c("id_caso", var_sel)
      print(var_sel)
      if (dt_bs$base == 'notas') {
        dt <- notas
      } else if (dt_bs$base == 'casos') {
        dt <- casos[var_sel]
        if (dt_bs$variable == 'delito') {
          dt <- dt %>% gather("delitoxx", "delito", delito_1:delito_7) %>% select(-delitoxx) %>% drop_na(delito)
        }
      } else {
        dt <- actores %>% filter(actores$tipo_de_participacion == 'Actor involucrado')
        dt <- dt[var_sel]
      }
    } else {
      base <- input$base_sel
      if (is.null(base)) return()
      var_prim <- input$var_principal
      if (is.null(var_prim)) return()
      var_cruce <- input$var_cruce
      if (is.null(var_cruce)) return()
      click_chart <- input$last_chart
      
      if (click_chart == "pie" | click_chart == "treemap") var_cruce <- "none"
      
     
      if (base == "Hechos") {
        if (var_cruce == "none"){
          dt <- casos %>% select_('id_caso', var_prim)
        } else {
          dt <- casos %>% select_('id_caso', var_prim, var_cruce)
        }
        
      } else {
        if (var_cruce == "none" | is.null(var_cruce)){
          dt <- actores %>% select_('id_caso', var_prim)
        } else {
          dt <- actores %>% select_('id_caso', var_prim, var_cruce)
        }
      }
    }
    dt
  })

  

  output$viz_hgch <- renderHighchart({
    
    click_chart <- input$last_chart
    if (is.null(click_chart) | click_chart == 'map') return()
    click_chart <- gsub('lines', 'line',  click_chart)
    
    orientacion <- 'ver'
    if (click_chart == 'barrash') orientacion <- 'hor'
    
    click_chart <- gsub('barras|barrash', 'bar', click_chart)
    
    df <- data_viz() %>% select(-id_caso)
    colSc <- 'no'
    colors <- c("#fdb731")
    
    if (click_chart == 'pie' | click_chart == 'treemap') {
      colSc <- 'discrete'
      colors <-  c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    }
    
    
    myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}")
    
    if (click_chart == 'line') myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
    
    
    if (ncol(df) == 2) {
      colSc <- "discrete"
      colors <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
      myFunc <- JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, cat:this.name, timestamp: new Date().getTime()});}")
    }
    
    opts_viz <- list(
      title = NULL,
      subtitle = NULL,
      caption = NULL,
      horLabel = NULL,
      verLabel = NULL,
      labelWrap = 30,
      colors = colors,
      color_scale = colSc,
      agg = "sum",
      agg_text = NULL,
      orientation = orientacion,
      marks = c(".", ","),
      nDigits = NULL,
      dropNa = FALSE,
      highlight_valueColor = '#F9B233',
      percentage = FALSE,
      highlight_value = NULL,
      sort = "desc",
      sliceN = 30,
      showText = TRUE,
      tooltip = list(headerFormat = NULL, pointFormat = NULL),
      export = FALSE,
      lang = 'es',
      allow_point = TRUE,
      cursor =  'pointer',
      clickFunction = myFunc,
      color_hover = "#fa8223",
      color_click  = "#fa8223",
      labelWrapV = c(30, 30),
      legend_position  = "center",
      startAtZero = TRUE,
      spline = FALSE,
      theme = tma(custom = list(stylesX_lineWidth = 0, colors = colors))
    )
    
    typeDt <- 'Cat'
    if (ncol(df) != 1) typeDt <- 'CatCat'
    
    viz <- paste0('hgch_', click_chart, '_', typeDt)
    
    do.call(viz, c(list(df, opts = opts_viz)))
    
  })
  
  
 
  data_ficha <- reactive({
    df <- data_viz() 
    
    if (is.null(df) | nrow(df) == 0) return()  
    
    var1 <- input$hcClicked$id
    
    if (is.null(var1)) return()
    
    var2 <- input$hcClicked$cat
    

    if (is.null(var2)) {
      var_sel <- names(df)[2]
      dta <- df[df[var_sel] == var1,]
    } else {
      var_sel_uno <- names(df)[2]
      var_sel_dos <- names(df)[3]
      dta <- df[df[var_sel_uno] == var2 & df[var_sel_dos] == var1,]
    }
    
    dt <- dta %>% drop_na() %>% distinct(id_caso)
    dt <- dt %>% left_join(casos)
    dt
  })
  
  output$lala <- renderPrint({
    data_viz() 
  })
  
  output$ficha_peque <- renderUI({
    info <- data_ficha()
    txt <- HTML("<div class = 'indicacion'><img src='click/click.svg' style='width: 50px; display:block;margin-left: 40%;'/> Haz click en la gráfica </br>
                  para ver los hechos de </br> corrupción relacionados</div>")
    
    if (is.null(info)) return(txt)
    if(nrow(info) == 0) txt <- txt
    
    
    filas <- nrow(info)
    txt <- map(1:filas, function(x){
      div(class = "ficha",
          div(class = "cont_info",
          HTML(paste0('<div class = "title_ficha">',info$`nombre_hecho_de_corrupcion_(publico)`[x], '</div>')),
          HTML(paste0('<div class = "sub_ficha">',info$`subtitulo_hecho_de_corrupcion_(publico)`[x], '</div>'))),
          tags$button(id = info$id_caso[x], class = "click_ficha",  "Ver más")
          )
    })

    txt
  })
  
  output$map_d <- renderPlot({
    id <- input$last_case
    if(is.null(id)) return()
    map_c(id)
    
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
      easyClose = TRUE,
      footer = modalButton("Cerrar"), 
      uiOutput('ficha'), 
      br()
    )
    )
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
            title =  gsub("\"","'",caso_i$`nombre_hecho_de_corrupcion_(publico)`),
            subtitle =  gsub("\"","'",caso_i$`subtitulo_hecho_de_corrupcion_(publico)`),
            mapc = map_c(id),
            abstract =  gsub("\"","'",caso_i$hecho_de_corrupcion),
            lugar = paste0(toupper(caso_i$departamento), ifelse(is.na(caso_i$municipio), '', paste0(' - ', toupper(caso_i$municipio)))),
            inicio = ifelse(is.na(caso_i$ano_inicial_hecho), 'No disponible', caso_i$ano_inicial_hecho),
            actor = ifelse(is.na(caso_i$new), 'No disponible', caso_i$new),
            tipo =  ifelse(is.na(caso_i$tipo_corrupcion), 'No disponible', caso_i$tipo_corrupcion),
            delito = ifelse(is.na(caso_i$delito), '', caso_i$delito),
            sector = toupper(caso_i$sector),
            # dinero =  as.character(ifelse(is.na(caso_i$dinero_juego), 'No disponible', paste0(' $',  format(caso_i$dinero_juego, nsmall= 0, big.mark=",")))),
            entidad = ifelse(is.na(caso_i$institucion), '', caso_i$subcategoria_1_actor_indivual),
            estado = ifelse(is.na(caso_i$situacion_judicial), '', caso_i$situacion_judicial),
            actualizacion = ifelse(is.na(caso_i$fecha_del_historial), '', as.character(caso_i$fecha_del_historial))
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
  # Salida Panel Uno
  output$panel_1 <- renderUI({
    div(
      uiOutput('basicos'),
      uiOutput('avanzadas'),
      uiOutput('vizOptions')
    )
  })
  
  
  # Salidad Panel Dos
  
  output$panel_2 <- renderUI({
    div(class = "viz_out", style = "height: 100%;",
    HTML("<div class = 'title_panel'>VISUALIZACIÓN</div>"),  
    highchartOutput('viz_hgch',height = 570),
    verbatimTextOutput("lala")
    )
  })
  
  # Salida Panel Tres
  
  output$panel_3 <- renderUI({
    div(class = "tj_out",
        HTML("<div class = 'title_panel'>HECHOS</div>"), 
        div(class = "content_ficha_mini",
    uiOutput("ficha_peque")
        )
    )
  })

  
  
  
})