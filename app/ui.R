shinyUI(
  fluidPage(
    tags$head(tags$script(HTML("$(document).on('click', '.needed', function () {
                                Shiny.onInputChange('last_click',this.id);
                             });"))),
    uiOutput("basicos"),
    uiOutput('vizOptions'),
    verbatimTextOutput("click"),
    highchartOutput("viz_hgch_out"),
    verbatimTextOutput("click_viz"),
    verbatimTextOutput("fichas")
  )
)