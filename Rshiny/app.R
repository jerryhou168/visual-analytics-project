library(shiny)

source("app-common.R", local = TRUE)
source("univariate.R", local = TRUE)
source("bivariate.R", local = TRUE)
source("correlation.R", local = TRUE)
source("multicollinearity.R", local = TRUE)
source("quasiseparation.R", local = TRUE)
source("prediction.R", local = TRUE)

#############################################################
### ui
#############################################################
ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    tabPanel("univariate",
             sidebarLayout(
               sidebarPanel(
                 univariate_nav
               ),
               mainPanel(
                 univariate_main
               )
             ),
             id = "t1"
    ),
    tabPanel("bivariate",
             sidebarLayout(
               sidebarPanel(
                 bivariate_nav
               ),
               mainPanel(
                 bivariate_main
               )
             ),
             id = "t2"
    ),
    tabPanel("correlation",
             sidebarLayout(
               sidebarPanel(
                 correlation_nav
               ),
               mainPanel(
                 correlation_main
               )
             ),
             id = "t3"
    ),
    tabPanel("multicollinearity",
             sidebarLayout(
               sidebarPanel(
                 multicollinearity_nav
               ),
               mainPanel(
                 multicollinearity_main
               )
             ),
             id = "t4"
    ),
    tabPanel("quasi-Complete Separation",
             sidebarLayout(
               sidebarPanel(
                 quasiseparation_nav
               ),
               mainPanel(
                 quasiseparation_main
               )
             ),
             id = "t5"
    ),
    tabPanel("loan Default Prediction",
             sidebarLayout(
               sidebarPanel(
                 prediction_nav
               ),
               mainPanel(
                 prediction_main
               )
             ),
             id = "t6"
    )
  )
)

#############################################################
### server
#############################################################

server <- function(input, output, session) {
  ## for prediction page
  univar(input, output)
  prediction(input, output, session)
  bivar(input, output)
  corr(input, output)
}

#############################################################
### shinyApp
#############################################################

shinyApp(ui = ui, server = server)
