fedSurveysUI <- function(id)
{
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(title = "Employment Index", 
    ),
    tabPanel(title = "Indeed",
             fluidRow(
               box(plotlyOutput(ns("indeed_plot1"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("indeed_plot2"), height = 250), width = 12)
             )
    ),
    tabPanel(title = "Claims",
             fluidRow(
               box(plotlyOutput(ns("claims_plot1"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("claims_plot1"), height = 250), width = 12)
             )
    )
  )
}