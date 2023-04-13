growthUI <- function(id)
{
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(title = "Growth Indices", 
             fluidRow(
               tabBox(
                 id = "real_growth_tab",
                 tabPanel("Tab1", plotlyOutput(ns("real_growth_index_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("real_growth_index_plot1.2"))),
                 tabPanel("Tab3", plotlyOutput(ns("real_growth_index_plot1.3")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "nominal_growth_tab",
                 tabPanel("Tab1", plotlyOutput(ns("nominal_growth_index_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("nominal_growth_index_plot1.2"))),
                 tabPanel("Tab3", plotlyOutput(ns("nominal_growth_index_plot1.3")))
               )
             )
    ),
    tabPanel(title = "Nominal growth", 
             box(fluidRow(plotlyOutput(ns("nominal_personal_consumption_plot")))),
             box(fluidRow(plotlyOutput(ns("nominal_retail_sales_plot")))),
             box(fluidRow(plotlyOutput(ns("nominal_personal_income_plot"))))
    ),
    tabPanel(title = "Real growth", 
             box(fluidRow(plotlyOutput(ns("real_personal_consumption_plot")))),
             box(fluidRow(plotlyOutput(ns("real_retail_sales_plot")))),
             box(fluidRow(plotlyOutput(ns("real_personal_income_plot")))),
             box(fluidRow(plotlyOutput(ns("real_nonfarm_payrolls_plot")))),
             box(fluidRow(plotlyOutput(ns("real_employment_level_plot")))),
             box(fluidRow(plotlyOutput(ns("real_industrial_production_plot"))))
    )
  )
}
