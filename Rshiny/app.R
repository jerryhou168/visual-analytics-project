library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Loan Default Prediction"),
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)
