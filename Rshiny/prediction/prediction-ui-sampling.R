
sampling_output_panel <- fluidRow(
  id="sorow",
  fluidRow(
    column(1,),
    column(5, plotOutput(outputId = "spo12")),
    column(5, plotOutput(outputId = "spo11")),
    column(1,)
  ),
  fluidRow(
    column(1,),
    column(5, dataTableOutput(outputId = "sto11")),
    column(5, dataTableOutput(outputId = "sto12")),
    column(1,)
  )
)

sampling_display <- function(output, toggle){
  
  plots <- c("spo11","spo12")
  for(p in plots){
    output[[p]] <- renderPlot({
      ggplot()
    })
  }
  
  tables <- c("sto11", "sto12")
  for(t in tables){
    output[[t]] <- renderDataTable({})
  }
}
