shinyServer(function(input, output, session) {
  
  # Básicos
  
  output$basicos <- renderUI({
    map(1:nrow(basicos), function(z){
     actionButton(inputId = basicos[z,]$id, label = basicos[z,]$preguntas, class = "needed")
     })
  })
 
  
  output$vizOptions <- renderUI({
    charts <- c("map", "bar_hor", "bar_ver" ,"treemap", "pie", "lines")
    buttonImage(id = "last_chart", charts, charts, file = "img/", format = "svg")
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