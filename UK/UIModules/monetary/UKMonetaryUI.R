UKMonetaryUI <- function(id)
{
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(title = "Credit & Money Supply", 
             fluidRow(
               tabBox(
                 id = "m4_tab",
                 tabPanel("Tab1", plotlyOutput(ns("uk_m4_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("uk_m4_growth_plot")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "m4_lending_tab",
                 tabPanel("Tab1", plotlyOutput(ns("uk_m4_lending_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("uk_m4_lending_growth_plot")))
               )
             )
    ),
    tabPanel(title = "Yield curves", 
             fluidRow(
               tabBox(
                 id = "yield_curve_tab",
                 tabPanel("Tab1", plotlyOutput(ns("uk_10y_bank_rate_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("uk_5y5y_bank_rate_plot")))
               )
             )
    )
  )
}