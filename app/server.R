shinyServer(function(input, output, session) {
  
  # Opciones Básicas
  
  output$basicos <- renderUI({
    div(class = "opciones_basicas",
    tags$button(id = "Basico", class = "click_option",  HTML("<div id='opts' class = 'active_opt'>&gt;</div> OPCIONES BÁSICAS")),
    HTML("Visualizar"),
    div(class = "preguntas_basicas",
    map(1:nrow(basicos), function(z){
     actionButton(inputId = basicos[z,]$id, label = basicos[z,]$preguntas, class = "needed")
     })
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
    charts <- c("barras", "barrash" ,"treemap", "map", "pie", "lines")
    buttonImage(id = "last_chart", charts, charts, file = "icons/", format = "svg", classImg = "imgStyle")
  })
  
  
  data_viz <- reactive({
    l_o <- input$last_option 
    if (is.null(l_o)) l_o <- "Basico"
    
    if (l_o == "Basico") {
      q_sel <- input$last_click
      if (is.null(q_sel)) q_sel <- 'q1'
      dt_bs <- basicos %>% filter(id == q_sel)
      
      var_sel <- dt_bs$variable
      
      if (var_sel == 'delito') var_sel <- paste0('delito_', 1:7)
      if (input$last_chart == "map" & q_sel != 'q3') var_sel <- c(var_sel, 'departamento')
      
      if (dt_bs$base == 'notas') {
        dt <- notas
      } else if (dt_bs$base == 'casos') {
        dt <- casos[var_sel]
        if (dt_bs$variable == 'delito') {
          dt <- dt %>% gather("delitoxx", "delito", delito_1:delito_7) %>% select(-delitoxx)
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
          dt <- casos %>% select_(var_prim)
        } else {
          dt <- casos %>% select_(var_prim, var_cruce)
        }
        
      } else {
        if (var_cruce == "none" | is.null(var_cruce)){
          dt <- actores %>% select_(var_prim)
        } else {
          dt <- actores %>% select_(var_prim, var_cruce)
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
    
    df <- data_viz()
    colSc <- 'no'
    if (click_chart == 'pie') colSc <- 'discrete'
    
    opts_viz <- list(
      title = NULL,
      subtitle = NULL,
      caption = NULL,
      horLabel = NULL,
      verLabel = NULL,
      labelWrap = 30,
      colors = c("#fdb731"),
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
      sliceN = 10,
      showText = TRUE,
      tooltip = list(headerFormat = NULL, pointFormat = NULL),
      export = FALSE,
      theme = NULL,
      lang = 'es',
      allow_point = TRUE,
      cursor =  'pointer',
      clickFunction = JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}"),
      graphType = "stacked",
      labelWrapV = c(30, 30),
      legend_position  = "center",
      startAtZero = TRUE,
      spline = FALSE
    )
    
    typeDt <- 'Cat'
    if (ncol(df) != 1) typeDt <- 'CatCat'
    
    viz <- paste0('hgch_', click_chart, '_', typeDt)
    
    do.call(viz, c(list(df, opts = opts_viz)))
    
  })
  
  
  output$lala <- renderPrint({
    data_viz()
  })
  
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
    div(
    HTML("VISUALIZACIÓN"),  
    highchartOutput('viz_hgch')#,
    #verbatimTextOutput("lala")
    )
  })
  
  
  #############
  data_basic <- reactive({
    q_i <- input$last_click
    if (is.null(q_i)) q_i <- "q1"
    df <- basicos %>% filter(id %in% q_i)
    var_sel <- df$variable
    d <- casos %>% 
      group_by_(var_sel) %>% 
      summarise(Conteo = n())
    d[[var_sel]] <- as.character(d[[var_sel]])
    d
  })
  
  viz_hgch <- reactive({
    data <- data_basic()
    hgch_bar_CatNum(data, opts = list(color_scale = "no", 
                                      allow_point = TRUE,
                                      cursor =  "pointer",
                                      clickFunction = JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.name, timestamp: new Date().getTime()});}"
                                                         )
    )
    )
  })
  
  output$viz_hgch_out <- renderHighchart({
    viz_hgch()
  })
  
  output$click_viz <- renderPrint({
    input$hcClicked
  })

  data_ficha <- reactive({
    cat_sel <- input$hcClicked$id
    
    q_i <- input$last_click

    if (is.null(q_i)) q_i <- "q1"
    df <- basicos %>% filter(id %in% q_i)
    var_sel <- df$variable
    

    if (is.null(cat_sel)) {
      tx <- "Haz click en la gráfica para ver los hechos de
             corrupción relacionados."
    } else {
      tx <- casos[casos[, var_sel] == cat_sel, ]
    }

    tx
  })
  
  output$fichas <- renderPrint({
    data_ficha()
  })
  
  
  # options_viz <- reactive({
  #   opts <- list(
  #     highlight_
  #   )
  # })
  
  output$click <- renderPrint({
    data_basic()
  })
  
  
  
})