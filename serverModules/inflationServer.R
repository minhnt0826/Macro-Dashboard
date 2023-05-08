source("fred.R")
source("helper.R")
# source("pce_median.R")
library(readxl)
library(TTR)
library(plotly)
library(dplyr)
# 
# services_cpi = fred_request_data("CUSR0000SASLE") %>%
#   mutate(services_change = (value / lag(value, 1) - 1) * 100)
# 
# shelter_cpi = fred_request_data("CUSR0000SAH1") %>%
#   mutate(shelter_change = (value / lag(value, 1) - 1) * 100)
# 
# weight = 0.59

# services_ex_shelter = services_cpi %>%
#   inner_join(shelter_cpi, by = "date") %>%
#   mutate(shelter_weight = weight) %>%
#   filter(date >= "2000-01-01") %>%
#   mutate(change = (services_change - shelter_change * shelter_weight) / (1-shelter_weight)) %>%
#   mutate(change_ann = (1 + change / 100) ^ 12) %>%
#   mutate(change_ann_z = scale(change_ann))
# 
# all_items_ex_shelter_cpi = fred_request_data("CUSR0000SA0L2") %>%
#   mutate(growth_3m = n_month_growth_ann(value,3),
#          growth_6m = n_month_growth_ann(value,6),
#          growth_12m = n_month_growth_ann(value,12))



# super_core_cpi = read_excel("data/BLS/CUSR0000SA0L12E4.xlsx", skip = 11) %>%
#   mutate(date = paste(Year, Period, "01", sep = "/")) %>%
#   mutate(date = as.Date(date, format = "%Y/M%m/%d"),
#          value = Value) %>%
#   select(date, value) %>%
#   mutate(growth_3m = n_month_growth_ann(value,3),
#          growth_6m = n_month_growth_ann(value,6),
#          growth_12m = n_month_growth_ann(value,12))

# core_pce = fred_request_data("PCEPILFE") %>%
#   mutate(growth_3m = n_month_growth_ann(value,3))
  

# plot1 <- plot_ly(services_ex_shelter, x = ~date, y = ~SMA(change_ann, 6), type = 'scatter', mode = 'lines') %>%
#   layout(title = 'Services ex shelter')

# plot2 <- plot_ly(all_items_ex_shelter_cpi, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines', name = "3m ann") %>%
#   add_trace(y = ~growth_6m, name = "6m ann") %>%
#   layout(title = 'All items ex shelter')

# plot3 <- plot_ly(super_core_cpi, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
#   add_trace(y = ~growth_6m, name = "6m ann") %>%
#   layout(title = 'Supercore')

# plot4 <- plot_ly(super_core_cpi, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'Supercore 6m annualized')

# plot5 <- plot_ly(super_core_cpi, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'Supercore 12m annualized')

# CPI ####
all_items_ex_shelter_cpi = fred_request_data("CUSR0000SA0L2") %>%
  select(date, value) %>%
  mutate(growth_3m = n_month_growth_ann(value, 3),
         growth_6m = n_month_growth_ann(value, 6),
         growth_12m = n_month_growth_ann(value, 12))

all_items_ex_shelter_cpi_plot = create_plotly_plot_with_growth_data(all_items_ex_shelter_cpi)


# PPI ####

ppi_final_demand = fred_request_data("PPIFIS") %>%
  create_growth_data_for_df(., .$value)

ppi_final_demand_core = fred_request_data("PPIFES") %>%
  create_growth_data_for_df(., .$value)

ppi_stage_4 = fred_request_data("WPUID54" )%>%
  create_growth_data_for_df(., .$value)

ppi_stage_3 = fred_request_data("WPUID53") %>%
  create_growth_data_for_df(., .$value)

ppi_stage_2 = fred_request_data("WPUID52") %>%
  create_growth_data_for_df(., .$value)

ppi_stage_1 = fred_request_data("WPUID51") %>%
  create_growth_data_for_df(., .$value)

ppi_final_demand_plot = create_plotly_plot_with_growth_data(ppi_final_demand) %>%
  layout(title = "PPI Final demand")

ppi_final_demand_core_plot = create_plotly_plot_with_growth_data(ppi_final_demand_core) %>%
  layout(title = "PPI Final demand core")

ppi_stage_4_plot = create_plotly_plot_with_growth_data(ppi_stage_4) %>%
  layout(title = "PPI stage 4")

ppi_stage_3_plot = create_plotly_plot_with_growth_data(ppi_stage_3) %>%
  layout(title = "PPI stage 3")

ppi_stage_2_plot = create_plotly_plot_with_growth_data(ppi_stage_2) %>%
  layout(title = "PPI stage 2")

ppi_stage_1_plot = create_plotly_plot_with_growth_data(ppi_stage_1) %>%
  layout(title = "PPI stage 1")

# Shiny server ####
inflationServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
      output$all_items_ex_shelter_cpi_plot <- renderPlotly({
        all_items_ex_shelter_cpi_plot
      })
      
      
      output$ppi_final_demand_plot <- renderPlotly({
        ppi_final_demand_plot
      })
      
      output$ppi_final_demand_core_plot <- renderPlotly({
        ppi_final_demand_core_plot
      })
      
      output$ppi_stage_4_plot <- renderPlotly({
        ppi_stage_4_plot
      })
      
      output$ppi_stage_3_plot <- renderPlotly({
        ppi_stage_3_plot
      })
      
      output$ppi_stage_2_plot <- renderPlotly({
        ppi_stage_2_plot
      })
      
      output$ppi_stage_1_plot <- renderPlotly({
        ppi_stage_1_plot
      })
      
    }
  )
}

