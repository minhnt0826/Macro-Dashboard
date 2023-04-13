inflationUI <- function(id)
{
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(title = "CPI", 
             fluidRow(
               box(plotlyOutput(ns("all_items_ex_shelter_cpi_plot"), height = 250), width = 12)
             ),
    ),
    tabPanel(title = "PPI", 
             fluidRow(
               box(plotlyOutput(ns("ppi_final_demand_plot"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("ppi_final_demand_core_plot"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("ppi_stage_4_plot"), height = 250), width = 12)
             ),
             fluidRow(
               box(plotlyOutput(ns("ppi_stage_3_plot"), height = 250), width = 12)
             ),             
             fluidRow(
               box(plotlyOutput(ns("ppi_stage_2_plot"), height = 250), width = 12)
             ),             
             fluidRow(
               box(plotlyOutput(ns("ppi_stage_1_plot"), height = 250), width = 12)
             )
             
    )
  )
}