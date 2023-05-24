employmentUI <- function(id)
{
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(title = "Employment Index", 
             fluidRow(
               box(plotlyOutput(ns("labor_market_activity_plot"), height = 400), width = 12)
             ),
    ),
    tabPanel(title = "NFP", 
             fluidRow(
               tabBox(
                 id = "nfp_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("aggregate_payrolls_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("aggregate_payrolls_production_plot")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "aggregate_weekly_hours",
                 tabPanel("Tab1", plotlyOutput(ns("aggregate_weekly_hours_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("aggregate_weekly_hours_production_plot")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "nfp_tab2",
                 tabPanel("Tab1", plotlyOutput(ns("avg_hourly_earnings_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("avg_hourly_earnings_production_plot")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "nfp_tab2",
                 tabPanel("Tab1", plotlyOutput(ns("employment_level_plot")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "nfp_tab4",
                 tabPanel("Tab1", plotlyOutput(ns("nfp_plot3.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("nfp_plot3.2")))
               ), 
               width = 12
             ),
             fluidRow(
               box(plotlyOutput(ns("nfp_plot4"), height = 400), width = 12)
             ),
             fluidRow(
               align = "center",
               h4("Leading indicators")
             ),
             fluidRow(
               tabBox(
                 id = "temp_help_tab",
                 tabPanel("Tab1", plotlyOutput(ns("temp_help_plot1"))),
                 tabPanel("Tab2", plotlyOutput(ns("temp_help_plot2"))),
                 tabPanel("Tab3", plotlyOutput(ns("temp_help_plot3"))),
                 tabPanel("Tab4", plotlyOutput(ns("temp_help_plot4"))),
               )
             ),
             fluidRow(
               tabBox(
                 id = "job_losers_tab",
                 tabPanel("Tab1", plotlyOutput(ns("job_losers_permanent_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("job_losers_permanent_growth_plot")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "nfp_tab5",
                 tabPanel("Tab1", plotlyOutput(ns("nfp_plot5.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("nfp_plot5.2"))),               
                 tabPanel("Tab3", plotlyOutput(ns("nfp_plot5.3"))),
                 tabPanel("Tab4", plotlyOutput(ns("nfp_plot5.4")))
               )
             ),
             fluidRow(
               box(plotlyOutput(ns("residential_emp_plot"), height = 400), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("truck_emp_plot"), height = 400), width = 12)
             )
    ),
    tabPanel(title = "Indeed",
             fluidRow(
               tabBox(
                 id = "total_postings",
                 tabPanel("Tab1", plotlyOutput(ns("indeed_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("indeed_plot1.2")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "new_postings",
                 tabPanel("Tab1", plotlyOutput(ns("indeed_plot2.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("indeed_plot2.2")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "indeed_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("indeed_plot3.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("indeed_plot3.2")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "indeed_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("indeed_plot4.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("indeed_plot4.2")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "indeed_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("indeed_plot5.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("indeed_plot5.2")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "indeed_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("indeed_plot6.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("indeed_plot6.2")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "indeed_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("indeed_plot7.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("indeed_plot7.2")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "indeed_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("indeed_plot8.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("indeed_plot8.2")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "indeed_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("indeed_plot9.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("indeed_plot9.2")))
               )
             )
    ),
    tabPanel(title = "Claims",
             fluidRow(
               tabBox(
                 id = "claims_tab1",
                 tabPanel("Tab1", plotlyOutput(ns("claims_plot1.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("claims_plot1.2"))),
                 tabPanel("Tab2", plotlyOutput(ns("claims_plot1.3")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "claims_tab2",
                 tabPanel("Tab1", plotlyOutput(ns("claims_plot2.1"))),
                 tabPanel("Tab2", plotlyOutput(ns("claims_plot2.2")))
               )
             ),
             fluidRow(
               tabBox(
                 id = "cclaims_vs_prev_years",
                 tabPanel("Tab1", plotlyOutput(ns("cc_claims_vs_prev_years_plot"))),
                 tabPanel("Tab2", plotlyOutput(ns("insured_unrate_vs_prev_years_plot")))
               )
             ),
             fluidRow(
               box(plotlyOutput(ns("ic_claims_vs_prev_years_plot"), height = 400), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("continuing_claims_adjusted_plot"), height = 400), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("initial_claims_adjusted_plot"), height = 400), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("continuing_claims_breath_plot"), height = 400), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("states_insured_unrate_breath_plot"), height = 400), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("initial_claims_breath_plot"), height = 400), width = 12)
             )
    ),
    tabPanel(title = "Unemployment indicators",
             fluidRow(
               box(plotlyOutput(ns("permanent_unrate_ma3_ma12_plot"), height = 400), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("unemployment_rate_u2_ma3_ma12_plot"), height = 400), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("unemployment_rate_ma3_ma12_plot"), height = 400), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("insured_unemployment_rate_yoy_plot"), height = 400), width = 12)
             )
    )
  )
}