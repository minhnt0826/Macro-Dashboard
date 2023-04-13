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
               box(plotlyOutput(ns("workweek_plot"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("f_workweek_plot"), height = 250), width = 12)
             )
    ),
    tabPanel(title = "Inflation",
             fluidRow(
               tabBox(
                 id = "price_paid_tab",
                 tabPanel("Tab1", plotlyOutput(ns("price_paid_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("price_paid_svc_plot")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "price_received_tab",
                 tabPanel("Tab1", plotlyOutput(ns("price_received_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("price_received_svc_plot")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "f_price_paid_tab",
                 tabPanel("Tab1", plotlyOutput(ns("f_price_paid_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("f_price_paid_svc_plot")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "f_price_received_tab",
                 tabPanel("Tab1", plotlyOutput(ns("f_price_received_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("f_price_received_svc_plot")))
               )
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
    ),
    tabPanel(title = "Capex",
             fluidRow(
               box(plotlyOutput(ns("capex_plot1"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("capex_plot2"), height = 250), width = 12)
             )
    ),
    tabPanel(title = "Manufacturing",
             fluidRow(
               box(plotlyOutput(ns("lead_time_plot1"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("lead_time_plot2"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("new_orders_plot1"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("new_orders_plot2"), height = 250), width = 12)
             )
    )
  )
  
}
