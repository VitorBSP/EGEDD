## ui.R ##
library(shiny)
library(shinydashboard)
library(plotly)
library(magrittr)
library(rsconnect)

ui <- dashboardPage(
    dashboardHeader(title = "Exponentiated Generalized Exponential Dagum Distribution",
                    titleWidth = 500),
    dashboardSidebar(       
        width = 300,
        withMathJax(),
        div(style = "font-size:14px;",
            sliderInput(inputId = "alpha", "mysliderInput",
                        label=(helpText(c("Escolha o valor de $\\alpha$:"))),
                        min = 0.01, max = 10, value = 3.48, step = .1, 
                        animate = animationOptions(interval = 300, loop = T))),
        div(style = "font-size:14px;",
            sliderInput(inputId = "delta", "mysliderInput", 
                        label=(helpText(c("Escolha o valor de $\\delta$:"))),
                        min = 0.01, max = 10, value = 0.88, step = .1, width = '500px',
                        animate = animationOptions(interval = 300, loop = T))),
        div(style = "font-size:14px;",
            sliderInput(inputId = "sigma", "mysliderInput",
                        label=(helpText(c("Escolha o valor de $\\sigma$:"))),
                        min = 0.01, max = 10, value = 2.06, step = .1, width = '500px',
                        animate = animationOptions(interval = 300, loop = T))),
        div(style = "font-size:14px;",   
            sliderInput(inputId = "gama", "mysliderInput",
                        label=(helpText(c("Escolha o valor de $\\gamma$:"))),
                        min = 0.01, max = 10, value = 2.97, step = .1, width = '500px',
                        animate = animationOptions(interval = 300, loop = T))),
        div(style = "font-size:14px;",     
            sliderInput(inputId = "eta", "mysliderInput",
                        label=(helpText(c("Escolha o valor de $\\eta$:"))),
                        min = 0.01, max = 10, value = 4.98, step = .1, width = '500px',
                        animate = animationOptions(interval = 300, loop = T))),
        div(style = "font-size:14px;",    
            sliderInput(inputId = "lambda", "mysliderInput",
                        label=(helpText(c("Escolha o valor de $\\lambda$:"))),
                        min = 0.01, max = 10, value = 9.8, step = .1, width = '500px',
                        animate = animationOptions(interval = 300, loop = T)))
    ),
    dashboardBody(

        fluidRow(
            box(plotlyOutput("plot2", height = '600px'), width = 300))))





server <- function(input, output){
    output$plot2 <- renderPlotly({
        peged<-function(x,alpha,delta,sigma,gama,eta,lambda)
        {
            fxn<-1-(1-(1-(1-(1+alpha*(x^(-delta)))^
                              (-sigma))^eta)^gama)^lambda
            return(fxn)
        }
        x<-seq(0,10, by = 0.01)
        yp<-peged(x,input$alpha,input$delta,input$sigma,
                  input$gama,input$eta,input$lambda)
        figp <- plot_ly(x=x,y=yp,type="scatter",mode="lines")
        figp <- figp %>% layout(title=paste0(c("Cumulative Distribution Function")))
        figp
    })
}



shinyApp(ui, server)