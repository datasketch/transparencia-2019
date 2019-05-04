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
    )
    )
  })
 
  
  # variables según base
  #variables_avanzadas <- reactiveValues(var = NULL)
  
  outVar <- reactive({
    data_sel <- input$base_sel
    if (is.null(data_sel)) return()
    df <- avanzados %>% filter(base == data_sel) 
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
  
  
  # Salida Panel Uno
  output$panel_1 <- renderUI({
    div(
      uiOutput('basicos'),
      uiOutput('avanzadas'),
      uiOutput('vizOptions')
    )
  })
  
  
  
  
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