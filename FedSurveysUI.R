fedSurveysUI <- function(id)
{
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(title = "Employment", 
             fluidRow(
               box(plotlyOutput(ns("employment_plot1"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("employment_plot2"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("employment_plot3"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("employment_plot4"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("employment_plot5"), height = 250), width = 12)
             )
    ),
    tabPanel(title = "Inflation",
             fluidRow(
               box(plotlyOutput(ns("inflation_plot1"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("inflation_plot2"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("inflation_plot3"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("inflation_plot4"), height = 250), width = 12)
             )
    ),
    tabPanel(title = "Demand & Biz climate",
             fluidRow(
               box(plotlyOutput(ns("demand_plot1"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("demand_plot2"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("climate_plot1"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("climate_plot2"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("diffusion_plot"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("aggregate_plot"), height = 250), width = 12)
             )
    )
  )
  
}