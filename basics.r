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

library(shiny)
ui <- fluidPage(
 "Hello, world!"
)
server <- function(input, output, session) {

}
shinyApp(ui, server)


#####
#USE THE STOP FUNCTION NOW
#####

####### 2. Building up the input (UI)

ui <- fluidPage(
  
  # Sidebar with a slider input for number of bins
  sliderInput(inputId = "bins",
              label = "Number of bins:",
              min = 1,
              max = 50,
              value = 30),
  
  # Show a plot of the generated distribution
  plotOutput("my_histogram")
)

shinyApp(ui, server)

# Oh wait, it's just input with no behaviour,

#####
#USE THE STOP FUNCTION NOW
#####

####### 3. Building UI and server

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



####### 4. What's wrong with the following apps? Can you debug them for me?

#4.1 
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

# 4.2
rm(list=ls())
ui <- fluidPage(
  inputDate("dob", "When were you born?"),
  dateRangeInput("holiday", "When do you next want to go on holiday?"),
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

# 4.3
animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel (
      radioButtons("rb", "Choose one:",
                   choiceNames = list(
                     icon("smile"),
                     icon("angry"),
                     icon("sad-tear")
                   ),
                   choiceValues = list( "happy", "angry", "sad")
      ),
  textInput("name", "What's your name?"),
  passwordInput("password", "What's your password?"),
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

