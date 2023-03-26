
prediction_output_panel_2 <- fluidRow(
  id="p2orow",
  fluidRow(id="two_porow0",
           column(6, htmlOutput(outputId = "p2so01")),
           column(6, htmlOutput(outputId = "p2so02")),
           style = "margin:1px 1px 1px 1px; background-color:#75D5D7;"
  ),
  fluidRow(id="two_porow1",
           column(6, plotOutput(outputId = "p2co11")),
           column(6, plotOutput(outputId = "p2co12")),
  ),
  fluidRow(id="two_porow2",
           column(6, plotOutput(outputId = "p2co21")),
           column(6, plotOutput(outputId = "p2co22")),
  ),
  fluidRow(id="two_porow3",
           column(6, plotOutput(outputId = "p2co31")),
           column(6, plotOutput(outputId = "p2co32")),
  ),
  fluidRow(id="two_porow4",
           column(6, plotOutput(outputId = "p2co41")),
           column(6, plotOutput(outputId = "p2co42")),
  ),
  fluidRow(id="two_porow5",
           column(6, plotOutput(outputId = "p2co51")),
           column(6, plotOutput(outputId = "p2co52")),
  ),
  fluidRow(id="two_porow6",
           column(6, plotOutput(outputId = "p2co61")),
           column(6, plotOutput(outputId = "p2co62")),
  ),
  fluidRow(id="two_porow7",
           column(6, plotOutput(outputId = "p2co71")),
           column(6, plotOutput(outputId = "p2co72")),
  ),
  fluidRow(id="two_porow8",
           column(6, plotOutput(outputId = "p2co81")),
           column(6, plotOutput(outputId = "p2co82")),
  ),
  fluidRow(id="two_porow9",
           column(6, plotOutput(outputId = "p2co91")),
           column(6, plotOutput(outputId = "p2co92")),
  ),
  fluidRow(id="two_porow10",
           column(6, dataTableOutput(outputId = "p2to101")),
           column(6, dataTableOutput(outputId = "p2to102")),
  ),
  style = "margin:0px 15px 0px 15px;"
)


prediction_display_2 <- function(output, toggle){
  
  panels <- c(0, 1,2,3,4,5,6,7,8,9,10)
  
  if(toggle == 'show'){
    for(idx in panels){
      shinyjs::show(paste0('two_porow', idx))
    }
    shinyjs::show('p2orow')
  }
  if(toggle == 'hide'){
    shinyjs::hide('p2orow')
    for(idx in panels){
      shinyjs::hide(paste0('two_porow', idx))
    }
  }
  
  htmls <- c("p2so01", "p2so02")
  for(t in htmls){
    output[[t]] <- renderText({"<div><span/></div>"})
  }
  
  plots <- c("p2co11","p2co12",
             "p2co21","p2co22",
             "p2co31","p2co32",
             "p2co41","p2co42",
             "p2co51","p2co52",
             "p2co61","p2co62",
             "p2co71","p2co72",
             "p2co81","p2co82",
             "p2co91","p2co92"
  )
  for(p in plots){
    output[[p]] <- renderPlot({
      ggplot()
    })
  }
  
  tables <- c("p2to101", "p2to102")
  for(t in tables){
    output[[t]] <- renderDataTable({ })
  }
}
