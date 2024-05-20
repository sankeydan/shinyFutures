################################################
################################################
################################################
######### SHINY FUTURES WORKSHOP ###############
################################################
########### Daniel W. E. Sankey ################
################################################

# Date last updated 2022.05.19

# Many of the examples are inspired by Hadley Wickham's "Mastering shiny"

####### 1. Basics

# Welcome ! Here is the code for your first shiny app.  These next few lines are
# common to all shiny apps, excluding the "Hello, world!" statement of course.
# Save this script somewhere so you can refer back to them

# IMPORTANT NOTE. IF YOU RUN THESE LINES, YOU WILL NEED TO USE THE STOP FUNCTION
# (RED CIRCLE AT TOP RIGHT OF CONSOLE) TO RESUME WORKING

####### 1. Building basic UI and server

library( shiny)

ui <- fluidPage(
  
  # Sidebar with a slider input for number of bins
  sliderInput("bins",               # notice I didn't put "inputID =" this time
              "Number of bins:",    # notice I didn't put "label =" this time; doing this way (without the =) for the first two arguments is recommended by Hadley Wickham
              min = 1,
              max = 50,
              value = 30),
  
  # Show a plot of the generated distribution
  plotOutput("my_histogram")
)

server <- function(input, output) {
  
  output$my_histogram <- renderPlot({
    
    # data
    x    <- faithful$waiting
    
    # change something based on input
    my_bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = my_bins, col = 'darkgray', border = 'white',main = "Time between eruptions")
  })
}

shinyApp(ui, server)

#####
#USE THE STOP FUNCTION NOW
#####

# 2. now let's add to this app

ui <- fluidPage(
  
  # Sidebar with a slider input for number of bins
  sliderInput("bins",               # notice I didn't put "inputID =" this time
              "Number of bins:",    # notice I didn't put "label =" this time; doing this way (without the =) for the first two arguments is recommended by Hadley Wickham
              min = 1,
              max = 50,
              value = 30),
  
  # Text input to comment on the histograms
  textInput("te1", "Comment on the first histogram"),
  textInput("te2", "Comment on the second histogram"),
  
  # Show a plot of the generated distribution
  plotOutput("my_histogram"),
  textOutput("text1"),
  plotOutput("histogram2"),
  textOutput("text2")
)

server <- function(input, output) {
  
  #### PLOT 1.
  output$my_histogram <- renderPlot({
    x    <- faithful$waiting
    my_bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = my_bins, col = 'darkgray', border = 'white',main = "Time between eruptions")
  })
  
  #### PLOT 2.
  output$histogram2 <- renderPlot({
    x    <- faithful$eruptions
    my_bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = my_bins, col = 'darkgray', border = 'white',main = "Eruption duration")
  })
  
  #### TEXT 1.
  output$text1 <- renderText ( {
   input$te1
  })
  
  #### TEXT 2.
  output$text2 <- renderText ( {
    input$te2
  })
}

shinyApp(ui, server)



####### 3. What's wrong with the following apps? Can you debug them for me?

#3.1 
rm(list=ls())
ui <- fluidPage(
  textInput("words", "Enter words here"),
  textOutput("text"),
  "are jumbled letters being pasted below where you type them? (they should be)"
)
server <- function(input, output, session) {
  output =  renderText({
    #input = list ( words = "pants")
    my_words = input$words
    new_words = rep(NA, nchar(my_words))
    for ( i in 1:nchar(my_words)){
      new_words[i] = substr(my_words,i,i)
    }
    paste (new_words [ sample( 1:nchar(my_words))], collapse = "")
  })
}
shinyApp(ui, server)

#####
#USE THE STOP FUNCTION NOW
#####

# 3.2
rm(list=ls())
ui <- fluidPage(
  inputDate(inputID = "dob", "When were you born?"),
  dateRangeInput( inputID="holiday", "When do you next want to go on holiday?"),
  textOutput("text")
)
server <- function(input, output, session) {
  output$text =  renderText({
   if (  input$dob %in% input$holiday) {
     print("you were born on your next holiday????")
   }else {
     print("I hear the Scilly Isles are nice this time of year")
   }
  })
}
shinyApp(ui, server)


#####
#USE THE STOP FUNCTION NOW
#####

# 3.3
animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel (
      radioButtons(inputId = "rb", "Choose one:",
                   choiceNames = list(
                     icon("smile"),
                     icon("angry"),
                     icon("sad-tear")
                   ),
                   choiceValues = list( "happy", "angry", "sad")
      ),
  textInput(inputId = "name", "What's your name?"),
  passwordInput(inputID="password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3),
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
  sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100),
  selectInput("state", "What's your favourite state?", state.name),
  radioButtons("animal", "What's your favourite animal?", animals),
  checkboxGroupInput("animal", "What animals do you like?", animals),
   selectInput(
    "state", "What's your favourite state?", state.name,
    multiple = TRUE
  ),
  fileInput("upload", NULL),
  actionButton("click", "Click me!"),
  actionButton("drink", "Drink me!", icon = icon("cocktail"))
    ), 
  mainPanel( 
    textOutput("text")
    )
  )
)
server <- function(input, output, session) {
  output$text =  renderText({
     paste ( "you are" , rb , "today, and that's the only thing that matters." )
  })
}
shinyApp(ui, server)

#####
#USE THE STOP FUNCTION NOW
#####

