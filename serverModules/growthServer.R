source("fred.R")
source("serverModules/inflationServer.R")
library(TTR)


personal_income = fred_request_data("PI") %>%
  left_join(fred_request_data("PCTR"), by = "date") %>%
  mutate(value = value.x - value.y) %>%
  select(date, value)
real_personal_income = fred_request_data("W875RX1") 

personal_consumption = fred_request_data("PCE")
real_personal_consumption = fred_request_data("DPCERA3M086SBEA")


retail_sales = fred_request_data("RSAFS")
real_retail_sales = retail_sales %>%
  left_join(all_items_ex_shelter_cpi, by = "date") %>%
  mutate(value = value.x / value.y) %>%
  select(date, value)


real_industrial_production = fred_request_data("INDPRO")
industrial_production = real_industrial_production %>%
  left_join(all_items_ex_shelter_cpi, by = "date") %>%
  mutate(value = value.x * value.y) %>%
  select(date, value)


aggregate_weekly_hours = fred_request_data("AWHI") %>%
  mutate(chng_prod = value / lag(value, 1)) %>%
  full_join(fred_request_data("AWHAE"), by = "date") %>%
  mutate(chng_private= value.y / lag(value.y, 1)) %>%
  mutate(chng_private = coalesce(chng_private, chng_prod)) %>%
  filter(!is.na(chng_private)) %>%
  mutate(value = 100 * cumprod(chng_private)) %>%
  select(date, value)

aggregate_payrolls = fred_request_data("CES0500000035") %>%
  mutate(chng_prod = value / lag(value, 1)) %>%
  full_join(fred_request_data("CES0500000017"), by = "date") %>%
  mutate(chng_private= value.y / lag(value.y, 1)) %>%
  mutate(chng_private = coalesce(chng_private, chng_prod)) %>%
  filter(!is.na(chng_private)) %>%
  mutate(value = 100 * cumprod(chng_private)) %>%
  select(date, value)


n_month_growth_ann <- function(x, n, na.rm = TRUE) ((x / lag(x, n)) ^ (12/n) - 1)*100

# Growth indices ####
real_growth_index = aggregate_weekly_hours %>%
  left_join(aggregate_weekly_hours, by = "date") %>%
  left_join(real_personal_income, by = "date") %>%
  left_join(real_industrial_production, by = "date") %>%
  left_join(real_retail_sales, by = "date") %>%
  left_join(real_personal_consumption, by = "date") %>%
  select(contains('date') | contains('value')) %>%
  mutate(across(contains('value'), 
                   .fns = list(growth_1m = ~n_month_growth_ann(., 1),
                               growth_3m = ~n_month_growth_ann(., 3),
                               growth_6m = ~n_month_growth_ann(., 6),
                               growth_12m = ~n_month_growth_ann(., 12)))) %>%
  mutate(growth_1m = rowMeans(across(contains("growth_1m")), na.rm = T),
         growth_3m = rowMeans(across(contains("growth_3m")), na.rm = T),
         growth_6m = rowMeans(across(contains("growth_6m")), na.rm = T),
         growth_12m = rowMeans(across(contains("growth_12m")), na.rm = T)) %>%
  select(date, growth_1m, growth_3m, growth_6m, growth_12m)
  
real_growth_index_plot1 <- plot_ly(real_growth_index, x = ~date, y = ~growth_1m, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~rollmean(growth_1m, k = 3, fill = NA, align = "right"), name = "MA3") %>%
  layout(title = 'Real growth index 1m annualized')

real_growth_index_plot1.1 <- plot_ly(real_growth_index, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~rollmean(growth_3m, k = 3, fill = NA, align = "right"), name = "MA3") %>%
  layout(title = 'Real growth index 3m annualized')

real_growth_index_plot1.2 <- plot_ly(real_growth_index, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~rollmean(growth_6m, k = 3, fill = NA, align = "right"), name = "MA3") %>%
  layout(title = 'Real growth index 6m annualized')


real_growth_index_plot1.3 <- plot_ly(real_growth_index, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~rollmean(growth_12m, k = 3, fill = NA, align = "right"), name = "MA3") %>%
  layout(title = 'Real growth index 12m annualized')

nominal_growth_index = aggregate_payrolls %>%
  left_join(aggregate_payrolls, by = "date") %>%
  left_join(personal_income, by = "date") %>%
  left_join(industrial_production, by = "date") %>%
  left_join(retail_sales, by = "date") %>%
  left_join(personal_consumption, by = "date") %>%
  select(contains('date') | contains('value')) %>%
  mutate(across(contains('value'), 
                .fns = list(growth_1m = ~n_month_growth_ann(., 1),
                            growth_3m = ~n_month_growth_ann(., 3),
                            growth_6m = ~n_month_growth_ann(., 6),
                            growth_12m = ~n_month_growth_ann(., 12)))) %>%
  mutate(growth_1m = rowMeans(across(contains("growth_1m")), na.rm = T),
         growth_3m = rowMeans(across(contains("growth_3m")), na.rm = T),
         growth_6m = rowMeans(across(contains("growth_6m")), na.rm = T),
         growth_12m = rowMeans(across(contains("growth_12m")), na.rm = T)) %>%
  select(date, growth_1m, growth_3m, growth_6m, growth_12m)


nominal_growth_index_plot1.1 <- plot_ly(nominal_growth_index, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~rollmean(growth_3m, 3, fill = NA, align = "right"), name = "MA3") %>%
  layout(title = 'Nominal growth index 3m annualized')

nominal_growth_index_plot1.2 <- plot_ly(nominal_growth_index, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~rollmean(growth_6m, 3, fill = NA, align = "right"), name = "MA3") %>%
  layout(title = 'Nominal growth index 6m annualized')

nominal_growth_index_plot1.3 <- plot_ly(nominal_growth_index, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~rollmean(growth_12m, 3, fill = NA, align = "right"), name = "MA3") %>%
  layout(title = 'Nominal growth index 12m annualized')


real_growth_index_ex_employment = real_personal_income %>%
  left_join(real_industrial_production, by = "date") %>%
  left_join(real_retail_sales, by = "date") %>%
  left_join(real_personal_consumption, by = "date") %>%
  select(contains('date') | contains('value')) %>%
  mutate(across(contains('value'), 
                .fns = list(growth_3m = ~n_month_growth_ann(., 3),
                            growth_6m = ~n_month_growth_ann(., 6),
                            growth_12m = ~n_month_growth_ann(., 12)))) %>%
  mutate(growth_3m = rowMeans(across(contains("growth_3m")), na.rm = T),
         growth_6m = rowMeans(across(contains("growth_6m")), na.rm = T),
         growth_12m = rowMeans(across(contains("growth_12m")), na.rm = T)) %>%
  select(date, growth_3m, growth_6m, growth_12m)

# Nominal growth factors ####
nominal_personal_consumption_plot = personal_consumption %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(.) %>%
  layout(title = "Personal consumption")

nominal_retail_sales_plot = retail_sales %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(.) %>%
  layout(title = "Retail sales")

nominal_personal_income_plot = personal_income %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(.) %>%
  layout(title = "Personal income")

# Real growth factors ####

real_personal_consumption_plot = real_personal_consumption %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(., growth_1m = T) %>%
  layout(title = "Real personal consumption")

real_retail_sales_plot = real_retail_sales %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(., growth_1m = T) %>%
  layout(title = "Real retail sales")

real_personal_income_plot = real_personal_income %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(.) %>%
  layout(title = "Real personal income")

real_nonfarm_payrolls_plot = aggregate_weekly_hours %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(.) %>%
  layout(title = "Aggregate weekly hours")
# 
# real_employment_level_plot = real_employment_level %>%
#   create_growth_data_for_df(., .$value) %>%
#   create_plotly_plot_with_growth_data(.) %>%
#   layout(title = "Real employment level")

real_industrial_production_plot = real_industrial_production %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(.) %>%
  layout(title = "Real industrial production")



# real_personal_income_ex_transfers = fred_request_data("W875RX1")
# 
# plot4 = create_plotly_plot_with_series(real_personal_income_ex_transfers, real_personal_income_ex_transfers$value)
# plot4



real_growth_index_2st_dev = real_growth_index %>%
  mutate(ma_3 = rollmean(growth_3m, k = 3, fill = NA, align = "right")) %>%
  mutate(chng = ma_3 - lag(ma_3))

nominal_growth_index_2st_dev = nominal_growth_index %>%
  mutate(ma_3 = rollmean(growth_3m, k = 3, fill = NA, align = "right")) %>%
  mutate(chng = ma_3 - lag(ma_3))

# retail_sales = fred_request_data("RSAFS") %>%
#   mutate(growth_3m = n_month_growth_ann(value, 3),
#          growth_6m = n_month_growth_ann(value, 6),
#          growth_12m = n_month_growth_ann(value, 12))
# 
# retail_sales_plot1 = plot_ly(retail_sales, x = ~date, y = ~SMA(growth_3m, 3), type = 'scatter', mode = 'lines', name = "3m") %>%
#   add_trace(y = ~growth_6m, name = "6m") %>%
#   add_trace(y = ~growth_12m, name = "12m") %>%
#   layout(title = "retail sales growth")
# 
# retail_sales_plot1


# pce = fred_request_data("PCE") %>%
#   mutate(growth_3m = n_month_growth_ann(value, 3),
#          growth_6m = n_month_growth_ann(value, 6),
#          growth_12m = n_month_growth_ann(value, 12))
# 
# pce_plot1 = plot_ly(pce, x = ~date, y = ~SMA(growth_3m, 3), type = 'scatter', mode = 'lines', name = "3m") %>%
#   add_trace(y = ~growth_6m, name = "6m") %>%
#   add_trace(y = ~growth_12m, name = "12m") %>%
#   layout(title = "PCE growth")
# 
# pce_plot1

# Shiny server ####
growthServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$real_growth_index_plot1 <- renderPlotly({
        real_growth_index_plot1
      })
      
      output$real_growth_index_plot1.1 <- renderPlotly({
        real_growth_index_plot1.1
      })
      
      output$real_growth_index_plot1.2 <- renderPlotly({
        real_growth_index_plot1.2
      })
      
      output$real_growth_index_plot1.3 <- renderPlotly({
        real_growth_index_plot1.3
      })
      
      output$nominal_growth_index_plot1.1 <- renderPlotly({
        nominal_growth_index_plot1.1
      })
      
      output$nominal_growth_index_plot1.2 <- renderPlotly({
        nominal_growth_index_plot1.2
      })
      
      output$nominal_growth_index_plot1.3 <- renderPlotly({
        nominal_growth_index_plot1.3
      })
      
      output$nominal_personal_consumption_plot <- renderPlotly({
        nominal_personal_consumption_plot
      })
      
      output$nominal_retail_sales_plot <- renderPlotly({
        nominal_retail_sales_plot
      })
      
      output$nominal_personal_income_plot <- renderPlotly({
        nominal_personal_income_plot
      })
      
      output$real_personal_consumption_plot <- renderPlotly({
        real_personal_consumption_plot
      })
      
      output$real_retail_sales_plot <- renderPlotly({
        real_retail_sales_plot
      })
      
      output$real_personal_income_plot <- renderPlotly({
        real_personal_income_plot
      })
      
      output$real_nonfarm_payrolls_plot <- renderPlotly({
        real_nonfarm_payrolls_plot
      })
      
      output$real_employment_level_plot <- renderPlotly({
        real_employment_level_plot
      })
      
      output$real_industrial_production_plot <- renderPlotly({
        real_industrial_production_plot
      })
    }
  )
}



