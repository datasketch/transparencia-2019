shinyUI(
  fluidPage(
    tags$head(tags$head(
      tags$link(rel="stylesheet", type="text/css", href="styles.css"),
      includeScript("js/iframeSizer.contentWindow.min.js"),
      includeScript("js/transparencia.js")
    )),
    div(class = "panels",
    uiOutput("panel_1"),
    uiOutput("panel_2"),
    uiOutput("panel_3")
    )
  )
)