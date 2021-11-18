## ui.R ##
library(shiny)
library(shinydashboard)
library(plotly)
library(magrittr)
library(rsconnect)

ui <- dashboardPage(
  dashboardHeader(title = "Exponentiated Generalized Exponential Dagum Distribution",
                  titleWidth = 750),
  dashboardSidebar(       
                  width = 500,
                    withMathJax(),
                    div(style = "font-size:20px;",
                       sliderInput(inputId = "alpha", "mysliderInput",
                                   label=(helpText(c("Escolha o valor de &alpha;:"))),
                                   min = 0.01, max = 10, value = 3.48, step = .1, width = '500px',
                                   animate = animationOptions(interval = 300, loop = T))),
                    div(style = "font-size:20px;",
                       sliderInput(inputId = "delta", "mysliderInput", 
                                   label=(helpText(c("Escolha o valor de &delta;:"))),
                                   min = 0.01, max = 10, value = 0.88, step = .1, width = '500px',
                                   animate = animationOptions(interval = 300, loop = T))),
                  div(style = "font-size:20px;",
                       sliderInput(inputId = "sigma", "mysliderInput",
                                   label=(helpText(c("Escolha o valor de $\\sigma$:"))),
                                   min = 0.01, max = 10, value = 2.06, step = .1, width = '500px',
                                   animate = animationOptions(interval = 300, loop = T))),
                  div(style = "font-size:20px;",   
                    sliderInput(inputId = "gama", "mysliderInput",
                                   label=(helpText(c("Escolha o valor de $\\gamma$:"))),
                                   min = 0.01, max = 10, value = 2.97, step = .1, width = '500px',
                                   animate = animationOptions(interval = 300, loop = T))),
                  div(style = "font-size:20px;",     
                  sliderInput(inputId = "eta", "mysliderInput",
                                   label=(helpText(c("Escolha o valor de $\\eta$:"))),
                                   min = 0.01, max = 10, value = 4.98, step = .1, width = '500px',
                                   animate = animationOptions(interval = 300, loop = T))),
                  div(style = "font-size:20px;",    
                  sliderInput(inputId = "lambda", "mysliderInput",
                                   label=(helpText(c("Escolha o valor de $\\lambda$:"))),
                                   min = 0.01, max = 10, value = 9.8, step = .1, width = '500px',
                                   animate = animationOptions(interval = 300, loop = T)))
  ),
  dashboardBody(
    fluidRow(

      box(plotlyOutput("plot1"), width = 300)),
      
      
    fluidRow(
        box(plotlyOutput("plot2"), width = 300),
      )
    )
  )



  
server <- function(input, output){
  output$plot1 <- renderPlotly({
    deged<-function(x,alpha,delta,sigma,gama,eta,lambda){
      A<-(1+alpha*(x^(-delta)))^(-sigma-1)
      B<-1-(1+alpha*(x^(-delta)))^(-sigma)
      fxn<-(lambda*alpha*sigma*delta*eta*gama)*
        (x^(-delta-1))*A*(B^(gama-1))*
        ((1-(B^gama))^(eta-1))*
        ((1-(1-(B^gama))^eta)^(lambda-1))
      return(fxn)
    }
    x<-seq(0,10, by = 0.01)
    yd<-deged(x,input$alpha,input$delta,input$sigma,
              input$gama,input$eta,input$lambda)
    figd <- plot_ly(x=x,y=yd,type="scatter",mode="lines")
    figd <- figd %>% layout(title=paste0(c("Probability Density Function")))
    figd
  })
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