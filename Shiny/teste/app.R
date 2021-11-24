
ui <- fluidPage(
    tags$style(type = "text/css", "
      .irs-bar {width: 100%; height: 25px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}
      .irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
      .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
      .irs-grid-text {font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}
      .irs-grid-pol {display: none;}
      .irs-max {font-family: 'arial'; color: black;}
      .irs-min {font-family: 'arial'; color: black;}
      .irs-single {color:black; background:#6666ff;}
      .irs-slider {width: 30px; height: 30px; top: 22px;}
    "),
    uiOutput("testSlider")
)

server <- function(input, output, session){
    output$testSlider <- renderUI({
        
        sliderInput(inputId="test", label=NULL, min=1, max=10, value=5, step = 1, width='100%')
        
    })
}

shinyApp(ui = ui, server=server)