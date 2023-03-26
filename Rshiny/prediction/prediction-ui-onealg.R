
prediction_output_panel_1 <- fluidRow(
  id="p1orow",
  fluidRow(id="one_porow1",
           column(6, plotOutput(outputId = "p1co11")),
           column(6, plotOutput(outputId = "p1co12")),
  ),
  fluidRow(id="one_porow2",
           column(6, plotOutput(outputId = "p1co21")),
           column(6, plotOutput(outputId = "p1co22")),
  ),
  fluidRow(id="one_porow3",
           column(6, plotOutput(outputId = "p1co31")),
           column(6, plotOutput(outputId = "p1co32")),
  ),
  fluidRow(id="one_porow4",
           column(6, plotOutput(outputId = "p1co41")),
           column(6, plotOutput(outputId = "p1co42")),
  ),
  fluidRow(id="one_porow5",
           column(6, plotOutput(outputId = "p1co51")),
           column(6, dataTableOutput(outputId = "p1to52")),
  ),
  style = "margin:0px 15px 0px 15px;"
)

prediction_display_1 <- function(output, toggle){
  
  panels <- c(1,2,3,4,5)
  
  if(toggle == 'show'){
    for(idx in panels){
      shinyjs::show(paste0('one_porow', idx))
    }
    shinyjs::show('p1orow')
  }
  if(toggle == 'hide'){
    shinyjs::hide('p1orow')
    for(idx in panels){
      
      shinyjs::hide(paste0('one_porow', idx))
    }
  }
  
  plots <- c("p1co11","p1co12",
             "p1co21","p1co22",
             "p1co31","p1co32",
             "p1co41","p1co42",
             "p1co51")
  for(p in plots){
    output[[p]] <- renderPlot({
      ggplot()
    })
  }
  
  tables <- c("p1to52")
  for(t in tables){
    output[[t]] <- renderDataTable({})
  }
}
