
############### GGPLOT MT CARS DATA - SHINY APP ###################

## For: Workshop: Shiny futures.

## By: Dan Sankey

## Date: 13/05/2022


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
  if ( namdat[i] %in% c("cyl","vs",   "am",   "gear", "carb")){
    data[,i]= as.factor(as.character(data[,i]))
  }
  print ( class( data[,i]))
}


infoTable = rbind(
  c( "mpg"	,"Miles/(US) gallon"),
  c( "cyl"	,"Number of cylinders"),
  c( "disp","Displacement (cu.in.)"),
  c( "hp"	,"Gross horsepower"),
  c( "drat","Rear axle ratio"),
  c( "wt"	,"Weight (* 1000 lbs)"),
  c( "qsec","1/4 mile time"),
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
      selectInput( "xax1" , "Pick your predictor variable",
                   choices = infoTable[ which ( ! infoTable[,1] %in% c("mpg" , "qsec")),2] ,
                   selected = "Rear axle ratio"),
      sliderInput("secs", "1/4 mile in less than x seconds" ,min= 10 , max = 25,value = 16),
      sliderInput("mpg" , "Better than x miles per gallon" , min= 10 , max = 35,value = 16),
      actionButton("lm", "Plot linear models?")
      
    ),
    mainPanel(
      plotOutput("qsecplot"),
      textOutput("fasterthan"),
      plotOutput("mpgplot"),
      textOutput("moreefficientthan"),
      plotOutput("qsecOvermpg"),
      textOutput("meetingStandards")
    )
    
  )
)





##################################################
# SERVER
##################################################

server <- function(input, output) {
  
  # input = list(
  # xax1 ="Engine (0 = V-shaped, 1 = straight)",
  # secs = 16,
  # mpg = 16,
  # lm = T
  # )
  
  ##################################################
  # QSEC plot  (number of SEConds for Quarter mile)
  #################################################
  
  output$qsecplot = renderPlot({
    
    # find the column
    whi =  which( infoTable[,2] == input$xax1)
    data$xvar = data[,whi]
    
    # Plot
    p = ggplot ( data, aes ( x = xvar , y = qsec))+
      xlab( input$xax1)+
      ylab( "1/4 mile time")+
      theme_classic(base_size =22)+
      geom_hline(yintercept = input$secs,lty = 2, col = "grey")
    
    # type of plot will depend on input variable
    if( class(data$xvar)== "numeric"){
      p = p+geom_point()
      
      # plot a linear model?
      if ( input$lm ){
        sum = summary ( lm ( data$qsec ~ data$xvar))
        pval = sum$coefficients[ "data$xvar" , "Pr(>|t|)"]
        pval = round(pval, 3)
        if ( pval == 0){
          pval = "p < 0.001"
        } else {
          pval = paste0 ( "p = ", pval)
        }
        p = p + geom_smooth(method = "lm")+
          ggtitle(pval)
        
      }
    } else {
      p = p+geom_violin()
    }
    
    # print plot to output
    p
    
  })
  
  ##################################################
  # QSEC text  (number of SEConds for Quarter mile)
  #################################################
  
  output$fasterthan = renderText( {
    
    # Which cars were fast enough for your fussy friend
    fastcars =  rownames( data)[ data$qsec < input$secs]
    len = length ( fastcars)
    len = ifelse( len >3,3,len)
    addthis = c("", "both" , "all")[len]
    if (length(fastcars ) == 0 ){
      fastcars =  "No cars "
    } else {
      fastcars = paste(c(fastcars,addthis) , collapse = "; ")
    }
    print( paste0 (  fastcars,
                     " drove a quarter mile in less than ",
                     input$secs,
                     " seconds from standstill."))
  })
  
  
  ##################################################
  # MPG plot  (number of Miles Per Gallon)
  #################################################
  
  output$mpgplot = renderPlot({
    
    # find the column
    whi =  which( infoTable[,2] == input$xax1)
    data$xvar = data[,whi]
    
    # Plot
    p = ggplot ( data, aes ( x = xvar , y = mpg))+
      xlab( input$xax1)+
      ylab( "Miles per gallon")+
      theme_classic(base_size =22)+
      geom_hline(yintercept = input$mpg,lty = 2, col = "grey")
    
    # type of plot will depend on input variable
    if( class(data$xvar)== "numeric"){
      p = p+geom_point()
      
      # plot a linear model?
      if ( input$lm ){
        sum = summary ( lm ( data$mpg ~ data$xvar))
        pval = sum$coefficients[ "data$xvar" , "Pr(>|t|)"]
        pval = round(pval, 3)
        if ( pval == 0){
          pval = "p < 0.001"
        } else {
          pval = paste0 ( "p = ", pval)
        }
        p = p + geom_smooth(method = "lm")+
          ggtitle(pval)
        
      }
    }else {
      p = p+geom_violin()
    }
    
    
    # print plot to output
    p
    
  })
  
  ##################################################
  # MPG text  (number of Miles Per Gallon)
  #################################################
  
  output$moreefficientthan = renderText( {
    
    # Which cars were fast enough for your fussy friend
    efficientcars =  rownames( data)[ data$mpg > input$mpg]
    len = length ( efficientcars)
    len = ifelse( len >3,3,len)
    addthis = c("was", "both were" , "all were")[len]
    if (length(efficientcars ) == 0 ){
      efficientcars =  "No cars were"
    } else {
      efficientcars = paste(c(efficientcars,addthis) , collapse = "; ")
    }
    print( paste0 (  efficientcars,
                     " more efficient than ",
                     input$mpg,
                     " miles per gallon."))
  })
  
  ##################################################
  # QSEC OVER MPG PLOT
  #################################################
  
  output$qsecOvermpg = renderPlot({
    sum = summary ( lm ( data$mpg ~ data$qsec))
    pval = sum$coefficients[ "data$qsec" , "Pr(>|t|)"]
    pval = round(pval, 3)
    p = ggplot ( data, aes ( x = qsec , y = mpg))+
      xlab( "1/4 mile time")+
      ylab( "Miles per gallon")+
      theme_classic(base_size =22)+
      geom_hline(yintercept = input$mpg ,lty = 2, col = "grey")+
      geom_vline(xintercept = input$secs,lty = 3, col = "grey")+
      geom_point()+
      ggtitle(paste ( "p =", pval))+
      geom_smooth(method = "lm")
    
    p
    
  })
  
  ##################################################
  # QSEC OVER MPG TEXT
  #################################################
  
  output$meetingStandards = renderText({
    
    # Which cars were fast enough for your fussy friend
    efficientfastcars =  rownames( data)[ data$mpg > input$mpg &
                                            data$qsec< input$secs]
    len = length ( efficientfastcars)
    len = ifelse( len >3,3,len)
    addthis = c("was", "both were" , "all were")[len]
    if (length(efficientfastcars ) == 0 ){
      efficientfastcars =  "No cars were"
    } else {
      efficientfastcars = paste(c(efficientfastcars,addthis) , collapse = "; ")
    }
    print( paste0 (  efficientfastcars,
                     " more efficient than ",
                     input$mpg,
                     " miles per gallon, and drove 1/4 mile in less than ",
                     input$secs,
                     " seconds."
    ))
  })
  
}


shinyApp(ui, server)
