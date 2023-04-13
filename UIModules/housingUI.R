housingUI <- function(id)
{
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(title = "Affordability", 
             fluidRow(
               box(plotlyOutput(ns("affordability_plot"), height = 250), width = 12)
             )
    ),
    tabPanel(title = "Construction", 
             fluidRow(
               tabBox(
                 id = "construction_spending",
                 tabPanel("Tab1", plotlyOutput(ns("construction_spending_plot1"))),
                 tabPanel("Tab2", plotlyOutput(ns("construction_spending_plot2"))),
                 tabPanel("Tab3", plotlyOutput(ns("construction_spending_plot3"))),
                 tabPanel("Tab4", plotlyOutput(ns("construction_spending_plot4")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "resi_construction_spending",
                 tabPanel("Tab1", plotlyOutput(ns("residential_construction_spending_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("real_residential_construction_spending_plot")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "housing_under_construction",
                 tabPanel("Tab1", plotlyOutput(ns("housing_under_construction_single_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("housing_under_construction_multi_plot")))
               )
             ),
             fluidRow(
               box(plotlyOutput(ns("housing_flow_plot1")))
             )
    )
  )
}