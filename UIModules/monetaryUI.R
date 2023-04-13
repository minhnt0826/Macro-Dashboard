monetaryUI <- function(id)
{
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(title = "Credit & Money Supply", 
             fluidRow(
               tabBox(
                 id = "credit_tab",
                 tabPanel("Tab1", plotlyOutput(ns("credit_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("credit_plot1.2"))),
                 tabPanel("Tab3", plotlyOutput(ns("credit_plot1.3"))),
                 tabPanel("Tab4", plotlyOutput(ns("credit_plot1.4")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "real_odl_tab",
                 tabPanel("Tab1", plotlyOutput(ns("real_odl_plot1"))),
                 tabPanel("Tab2", plotlyOutput(ns("real_odl_plot2")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "bank_credit_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("bank_credit_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("bank_credit_plot1.2"))),
                 tabPanel("Tab3", plotlyOutput(ns("bank_credit_plot1.3"))),
               )
             ),
             fluidRow(
               tabBox(
                 id = "loan_leases_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("loan_leases_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("loan_leases_plot1.2"))),
                 tabPanel("Tab3", plotlyOutput(ns("loan_leases_plot1.3")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "ci_loans_tab",
                 tabPanel("Tab1", plotlyOutput(ns("ci_loans_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("ci_loans_plot1.2"))),
                 tabPanel("Tab3", plotlyOutput(ns("ci_loans_plot1.3"))),
               )
             ),
             fluidRow(
               tabBox(
                 id = "real_estate_tab",
                 tabPanel("Tab1", plotlyOutput(ns("real_estate_plot1"))),
                 tabPanel("Tab2", plotlyOutput(ns("real_estate_plot2"))),
                 tabPanel("Tab3", plotlyOutput(ns("real_estate_plot3"))),
               )
             ),
             fluidRow(
               tabBox(
                 id = "consumer_loans_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("consumer_loans_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("consumer_loans_plot1.2"))),
                 tabPanel("Tab3", plotlyOutput(ns("consumer_loans_plot1.3"))),
               )
             )
    ),
    tabPanel(title = "Monetary Policy", 
             fluidRow(
               tabBox(
                 id = "fed_funds_tab",
                 tabPanel("Tab1", plotlyOutput(ns("fed_funds_plot1"))),
                 tabPanel("Tab2", plotlyOutput(ns("fed_funds_plot2"))),
               )
             ),
             fluidRow(
               tabBox(
                 id = "current_rate_less_neutral_rate",
                 tabPanel("Tab1", plotlyOutput(ns("diff_3m_to_5y5y_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("diff_1y_to_5y5y_plot"))),
               )
             ),
             fluidRow(
               tabBox(
                 id = "2y_less)growth",
                 tabPanel("Tab1", plotlyOutput(ns("bond_2y_yield_less_growth_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("bond_2y_yield_less_growth_plot1.2"))),
               )
             ),
             fluidRow(
               tabBox(
                 id = "prime_rate_tab",
                 tabPanel("Tab1", plotlyOutput(ns("baa_yield_less_growth_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("baa_yield_less_growth_plot1.2"))),
               )
             )
    )
  )
}