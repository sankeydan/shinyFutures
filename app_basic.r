
############### GGPLOT MT CARS DATA - SHINY APP ###################

## For: Workshop: Shiny futures. 

## By: Dan Sankey

## Date: 13/05/2022

## See completed app (but not code) at sankey-dan.shinyapps.io/ggplotMtcarsComp/

## Your goal is to make this app look more like (or even better than) the app above

###### SCRIPT ##############

# LIBRARIES
library(shiny)
library(ggplot2)


#### HOUSEKEEPING ###

# Clear workspace
rm( list = ls())

# Data
data = mtcars
i=1
namdat= names(data)
for( i in 1:ncol (data)){
  assign( namdat[i],  unique(data[,i]))
  print ( class(data[,i]))
}

infoTable = rbind(
  c( "mpg"	,"Miles/(US) gallon"),
  c( "cyl"	,"Number of cylinders"),
  c( "disp","Displacement (cu.in.)"),
  c( "hp"	,"Gross horsepower"),
  c( "drat","Rear axle ratio"),
  c( "wt"	,"Weight (* 1000 lbs)"),
  c( "qsec","1/4 mile time (s)"),
  c( "vs"	,"Engine (0 = V-shaped, 1 = straight)"),
  c( "am"	,"Transmission (0 = automatic, 1 = manual)"),
  c( "gear", "Number of forward gears"))


###############################################
# UI - USER INTERFACE
###############################################

ui <- fluidPage(
  h1("My mtcars exploration app!"),
  sidebarLayout(
    sidebarPanel (
      radioButtons( "xax1" , "Pick your predictor variable",choices = c(
        "Rear axle ratio",
        "Weight (* 1000 lbs)",
        "Gross horsepower"
      ) ),
      sliderInput("secs", "1/4 mile in less than x seconds" ,min= 10 , max = 25,value = 16)
    ),
    mainPanel(
      plotOutput("qsecplot"),
      textOutput("fasterthan")
    )
    
  )
)





##################################################
# SERVER 
##################################################

server <- function(input, output) {
  
  # input = list(
  # xax1 = "Rear axle ratio",
  # secs = 16
  # )
  
  # QSEC plot  (number of SEConds for Quarter mile)
  output$qsecplot = renderPlot({
    
    # find the column
    whi =  which( infoTable[,2] == input$xax1)
    data$xvar = data[,whi]
    
    # Plot
    p = ggplot ( data, aes ( x = xvar , y = qsec))+
      xlab( input$xax1)+
      ylab( "1/4 mile time (s)")+
      theme_classic(base_size =22)+
      geom_hline(yintercept = input$secs,lty = 2, col = "grey")
    
    # type of plot will depend on input variable
    if( class(data$xvar)== "numeric"){
      p = p+geom_point()  
    }
    
    # print plot to output
    p
    
  })
  
  output$fasterthan = renderText( {
    print( paste0 ( paste( rownames( data)[ data$qsec < input$secs], collapse = "; ") ,
           "; all drive a quarter mile in less than ",
           input$secs, 
           " seconds from standstill."))
  })
  
  
}


shinyApp(ui, server)
