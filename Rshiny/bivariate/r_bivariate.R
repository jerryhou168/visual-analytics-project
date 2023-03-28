
bivariate_color_sch <- c("#E3120B", "skyblue", "lightblue")

renderBivariateMosaicplot <- function(input, output, session, ds, x, y, xl, yl) {
  
  output$bivarplot <- renderPlot({
    
    mosaicplot(table(ds[[x]], ds[[y]]),
               color = TRUE,
               main = paste("Mosaic plot of", xl, "vs", yl),
               xlab = xl,
               ylab = yl,
               cex.axis = 0.8,
               las = 2)
  })
}

renderBivariateBoxplotXfactor <- function(input, output, session, ds, x, y, xl, yl) {
  
  p <- ggplot(ds, aes(x = factor(.data[[x]]), y = .data[[y]])) + 
    geom_boxplot(aes(colour = factor(ds$good_bad_flag))) +
    theme_economist() +
    labs(title = "Bivariate Analysis", colour = "Loan Quality") +
    xlab(xl)+
    ylab(yl)+
    theme(axis.title.y = element_text(vjust = 2.5),
          axis.text.x = element_text(angle = 60,
                                     vjust = 0.5))
  
  output$bivarplot <- renderPlot({
    p
  })
}

renderBivariateBoxplotYfactor <- function(input, output, session, ds, x, y, xl, yl) {
  
  p <- ggplot(ds, aes(x = .data[[x]], y = factor(.data[[y]]))) + 
    geom_boxplot(aes(colour = factor(ds$good_bad_flag))) +
    theme_economist() +
    labs(title = "Bivariate Analysis", colour = "Loan Quality") +
    xlab(xl)+
    ylab(yl)+
    theme(axis.title.y = element_text(vjust = 2.5),
          axis.text.x = element_text(angle = 60,
                                     vjust = 0.5))
  
  output$bivarplot <- renderPlot({
    p
  })
}

renderBivariateScatterPlot <- function(input, output, session, ds, x, y, xl, yl) {
  
  p <- ggplot(ds, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(aes(colour = factor(ds$good_bad_flag)),
               shape = "circle",
               size = 3) + 
    scale_color_manual(values = bivariate_color_sch) + 
    theme_economist() +
    labs(title = "Bivariate Analysis", colour = "Loan Quality") +
    xlab(xl)+
    ylab(yl)+
    theme(axis.title.y = element_text(vjust = 2.5),
          axis.text.x = element_text(angle = 60,
                                     vjust = 0.5))
  
  output$bivarplot <- renderPlot({
    p
  })
}


renderBivariateChart <- function(input, output, session, x, y) {
  
  ds <- repeatloan
  
  if (input$t2_loanType == "S") {
    ds <- newloan
  }
  
  xtype <- class(ds[[x]])
  
  ytype <- class(ds[[y]])
  
  xl <- common_variable_name(input$t2_loanType, x)
  yl <- common_variable_name(input$t2_loanType, y)
  
  if(isTRUE(xtype == 'character') & isTRUE(ytype == 'character')){
    # Mosaic plot for two categorical variables  
    renderBivariateMosaicplot(input, output, session, ds, x, y, xl, yl)
  }else if(isTRUE(xtype == 'character') & isTRUE(ytype == 'numeric')){
    # Boxplot for one categorical and one continuous variables  
    renderBivariateBoxplotXfactor(input, output, session, ds, x, y, xl, yl)
  }else if(isTRUE(xtype == 'numeric') & isTRUE(ytype == 'character')){
    # Boxplot for one categorical and one continuous variables
    renderBivariateBoxplotYfactor(input, output, session, ds, x, y, xl, yl)
  }else if(isTRUE(xtype == 'numeric') & isTRUE(ytype == 'numeric')){
    # Scatter plot for two continuous variables
   renderBivariateScatterPlot(input, output, session, ds, x, y, xl, yl)
  }
  
}
