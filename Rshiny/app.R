library(shiny)
library(shinythemes)

source("app-common.R", local = TRUE)
source("prediction.R", local = TRUE)
source("univariate.R", local = TRUE)
source("bivariate.R", local = TRUE)
source("correlation.R", local = TRUE)
source("multicollinearity.R", local = TRUE)
source("quasiseparation.R", local = TRUE)

#############################################################
### ui
#############################################################
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("united"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Univariate",
             sidebarLayout(
               sidebarPanel(
                 univariate_nav,
                 width = 3
               ),
               mainPanel(
                 univariate_main,
                 width = 9
               )
             ),
             id = "t1"
    ),
    tabPanel("Bivariate",
             sidebarLayout(
               sidebarPanel(
                 bivariate_nav,
                 width = 3
               ),
               mainPanel(
                 bivariate_main,
                 width = 9
               )
             ),
             id = "t2"
    ),
    tabPanel("Correlation",
             sidebarLayout(
               sidebarPanel(
                 correlation_nav,
                 width = 3
               ),
               mainPanel(
                 correlation_main,
                 width = 9
               )
             ),
             id = "t3"
    ),
    tabPanel("Multicollinearity",
             sidebarLayout(
               sidebarPanel(
                 multicollinearity_nav,
                 width = 3
               ),
               mainPanel(
                 multicollinearity_main,
                 width = 9
               )
             ),
             id = "t4"
    ),
    tabPanel("Quasi-Complete Separation",
             sidebarLayout(
               sidebarPanel(
                 quasiseparation_nav,
                 width = 3
               ),
               mainPanel(
                 quasiseparation_main,
                 width = 9
               )
             ),
             id = "t5"
    ),
    tabPanel("Loan Default Prediction",
             sidebarLayout(
               fluid = TRUE,
               sidebarPanel(
                 prediction_nav,
                 width = 12
               ),
               mainPanel(
                 width = 12,
                 prediction_main
               )
             ),
             id = "t6"
    )
  ),
  shinythemes::themeSelector()
)

#############################################################
### server
#############################################################

application <- function(input, output, session) {
  ## for prediction page
  univar(input, output, session)
  prediction(input, output, session)
  bivar(input, output, session)
  corr(input, output, session)
  quasicomplete(input, output, session)
  multi(input, output, session)

}

server <- function(input, output, session) {
  tryCatch(application(input, output, session),
           error = function(c) {
               print(c)
               print(paste0('error details', c))
             }
  )
}

#############################################################
### shinyApp
#############################################################

shinyApp(ui = ui, server = server)
