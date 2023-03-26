
prediction_output_panel_3 <- fluidRow(
  id="p3orow",
  fluidRow(id="three_porow0",
           column(4, htmlOutput(outputId = "p3so01")),
           column(4, htmlOutput(outputId = "p3so02")),
           column(4, htmlOutput(outputId = "p3so03")),
           style = "margin:1px 1px 1px 1px; background-color:#75D5D7;"
  ),
  fluidRow(id="three_porow1",
           column(4, plotOutput(outputId = "p3co11")),
           column(4, plotOutput(outputId = "p3co12")),
           column(4, plotOutput(outputId = "p3co13")),
  ),
  fluidRow(id="three_porow2",
           column(4, plotOutput(outputId = "p3co21")),
           column(4, plotOutput(outputId = "p3co22")),
           column(4, plotOutput(outputId = "p3co23")),
  ),
  fluidRow(id="three_porow3",
           column(4, plotOutput(outputId = "p3co31")),
           column(4, plotOutput(outputId = "p3co32")),
           column(4, plotOutput(outputId = "p3co33")),
  ),
  fluidRow(id="three_porow4",
           column(4, plotOutput(outputId = "p3co41")),
           column(4, plotOutput(outputId = "p3co42")),
           column(4, plotOutput(outputId = "p3co43")),
  ),
  fluidRow(id="three_porow5",
           column(4, plotOutput(outputId = "p3co51")),
           column(4, plotOutput(outputId = "p3co52")),
           column(4, plotOutput(outputId = "p3co53")),
  ),
  fluidRow(id="three_porow6",
           column(4, plotOutput(outputId = "p3co61")),
           column(4, plotOutput(outputId = "p3co62")),
           column(4, plotOutput(outputId = "p3co63")),
  ),
  fluidRow(id="three_porow7",
           column(4, plotOutput(outputId = "p3co71")),
           column(4, plotOutput(outputId = "p3co72")),
           column(4, plotOutput(outputId = "p3co73")),
  ),
  fluidRow(id="three_porow8",
           column(4, plotOutput(outputId = "p3co81")),
           column(4, plotOutput(outputId = "p3co82")),
           column(4, plotOutput(outputId = "p3co83")),
  ),
  fluidRow(id="three_porow9",
           column(4, plotOutput(outputId = "p3co91")),
           column(4, plotOutput(outputId = "p3co92")),
           column(4, plotOutput(outputId = "p3co93")),
  ),
  fluidRow(id="three_porow10",
           column(4, dataTableOutput(outputId = "p3to101")),
           column(4, dataTableOutput(outputId = "p3to102")),
           column(4, dataTableOutput(outputId = "p3to103")),
  ),
  style = "margin:0px 15px 0px 15px;"
)

prediction_display_3 <- function(output, toggle){
  
  panels <- c(0, 1,2,3,4,5,6,7,8,9,10)
  
  if(toggle == 'show'){
    for(idx in panels){
      shinyjs::show(paste0('three_porow', idx))
    }
    shinyjs::show('p3orow')
  }
  if(toggle == 'hide'){
    shinyjs::hide('p3orow')
    for(idx in panels){
      shinyjs::hide(paste0('three_porow', idx))
    }
  }
  
  htmls <- c("p3so03", "p3so03", "p3so03")
  for(t in htmls){
    output[[t]] <- renderText({"<div><span/></div>"})
  }
  
  plots <- c("p3co11","p3co12","p3co13",
             "p3co21","p3co22","p3co23",
             "p3co31","p3co32","p3co33",
             "p3co41","p3co42","p3co43",
             "p3co51","p3co52","p3co53",
             "p3co61","p3co62","p3co63",
             "p3co71","p3co72","p3co73",
             "p3co81","p3co82","p3co83",
             "p3co91","p3co92","p3co93"
  )
  for(p in plots){
    output[[p]] <- renderPlot({
      ggplot()
    })
  }
  
  tables <- c("p3to101", "p3to102", "p3to103")
  for(t in tables){
    output[[t]] <- renderDataTable({ })
  }
}

