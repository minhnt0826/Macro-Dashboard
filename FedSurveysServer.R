library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(zoo)

create_z_scores_for_df <- function(df){
  df.z = df %>% 
    mutate_at(vars(-date), scale) %>%
    mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T))
  
  return(df.z)
}


# Set up ####
data_folder_name = "data/Fed Surveys"
fnames <- list.files(data_folder_name)
for(i in fnames)
{
  file_name = paste(data_folder_name, '/', i, sep = '')
  variable_name = substr(i, 1, nchar(i)-4)
  a = read.csv(file_name)
  a$date = as.Date(a$date)
  colnames(a)[-1] = paste(colnames(a)[-1],variable_name,sep="_")
  
  assign(variable_name, a)
}
rm(a)

merged_data = texas_svc %>%
  left_join(texas_man, by = 'date') %>%
  left_join(texas_retail, by = 'date') %>%
  left_join(kansas_svc, by = 'date') %>%
  left_join(kansas_man, by = 'date') %>%
  left_join(ny_man, by = 'date') %>%
  left_join(ny_svc, by = 'date') %>%
  left_join(phil_man, by = 'date') %>%
  left_join(phil_svc, by = 'date') %>%
  left_join(richmond_man, by = 'date') %>%
  left_join(richmond_svc, by = 'date') %>%
  mutate(date = as.Date(date))

# Employment data ####
employment_data <- merged_data %>%
  select(contains("date") | starts_with("employment"))

employment_data.z = create_z_scores_for_df(employment_data)

f_employment_data <- merged_data %>%
  select(contains("date") | starts_with("f_employment"))

f_employment_data.z = create_z_scores_for_df(f_employment_data)


wages_data <- merged_data %>%
  select(contains("date") | starts_with("wages"))

wages_data.z = create_z_scores_for_df(wages_data)

f_wages_data <- merged_data %>%
  select(contains("date") | starts_with("f_wages"))

f_wages_data.z = create_z_scores_for_df(f_wages_data)


workweek_data <- merged_data %>%
  select(contains("date") | contains("work") & !contains("richmond") & !contains("f_"))

workweek_data.z = create_z_scores_for_df(workweek_data)

# Inflation data ####
price_paid_data <- merged_data %>%
  select(contains("date") | starts_with("input_prices") | starts_with("prices_paid")) 

price_paid_data.z = price_paid_data %>% 
  mutate_at(vars(-date), scale) %>%
  mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T))

f_price_paid_data <- merged_data %>%
  select(contains("date") | starts_with("f_input_prices") | starts_with("f_prices_paid")) 

f_price_paid_data.z = f_price_paid_data %>% 
  mutate_at(vars(-date), scale) %>%
  mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T))


price_received_data = merged_data %>%
  select(contains("date") | starts_with("selling_prices") | starts_with("prices_received")) 

price_received_data.z = price_received_data %>% 
  mutate_at(vars(-date), scale) %>%
  mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T))


f_price_received_data = merged_data %>%
  select(contains("date") | starts_with("f_selling_prices") | starts_with("f_prices_received")) 

f_price_received_data.z = f_price_received_data %>% 
  mutate_at(vars(-date), scale) %>%
  mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T))

margin_data.z = price_paid_data.z %>%
  rename(price_paid_mean.z = mean.z) %>%
  left_join(price_received_data.z, by = 'date') %>%
  mutate(diff.z = mean.z - price_paid_mean.z) %>%
  select(date, diff.z)

# Demand data ####
demand_data = texas_svc %>%
  left_join(texas_retail, by = 'date') %>%
  left_join(kansas_svc, by = 'date') %>%
  left_join(phil_svc, by = 'date') %>%
  left_join(richmond_svc, by = 'date') %>%
  select(matches('date') | starts_with('revenue')) 

demand_data <- texas_man %>%
  left_join(kansas_man, by = 'date') %>%
  left_join(ny_man, by = 'date') %>%
  left_join(phil_man, by = 'date') %>%
  left_join(richmond_man, by = 'date') %>%
  select(matches('date') | starts_with('new_orders')) %>%
  right_join(demand_data, by = 'date')

demand_data = ny_svc %>%
  select(matches('date')| starts_with('biz_activity')) %>%
  right_join(demand_data, by = 'date') %>%
  mutate(date = as.Date(date))

demand_data.z = demand_data %>% 
  mutate_at(vars(-date), scale) %>%
  mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T)) %>%
  mutate(mean.z.3ma = rollmean(mean.z, 3, fill = NA, align = 'right'))



f_demand_data = texas_svc %>%
  left_join(texas_retail, by = 'date') %>%
  left_join(kansas_svc, by = 'date') %>%
  select(matches('date') | starts_with('f_revenue')) 

f_demand_data <- texas_man %>%
  left_join(kansas_man, by = 'date') %>%
  left_join(ny_man, by = 'date') %>%
  left_join(phil_man, by = 'date') %>%
  left_join(richmond_man, by = 'date') %>%
  select(matches('date') | starts_with('f_new_orders')) %>%
  right_join(f_demand_data, by = 'date')

f_demand_data = richmond_svc %>%
  select(matches('date')|starts_with('f_demand')) %>%
  right_join(f_demand_data, by = 'date')

f_demand_data = ny_svc %>%
  left_join(phil_svc, by = 'date') %>%
  select(matches('date')| starts_with('f_biz_activity')) %>%
  right_join(f_demand_data, by = 'date') %>%
  mutate(date = as.Date(date))

f_demand_data.z = f_demand_data %>% 
  mutate_at(vars(-date), scale) %>%
  mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T)) %>%
  mutate(mean.z.3ma = rollmean(mean.z, 3, fill = NA, align = 'right'))


biz_climate_data = merged_data %>%
  select(contains("date") | starts_with("biz_climate")) 

biz_climate_data.z = biz_climate_data %>% 
  mutate_at(vars(-date), scale) %>%
  mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T))

f_biz_climate_data = merged_data %>%
  select(contains("date") | starts_with("f_biz_climate")) 

f_biz_climate_data.z = f_biz_climate_data %>% 
  mutate_at(vars(-date), scale) %>%
  mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T))

# Diffusion indexes ####
important_data = merged_data %>%
  select(contains("date") | starts_with("f_biz_climate") | starts_with("employment") | 
        starts_with("f_employment") | starts_with("f_new_orders") | starts_with("f_revenue")
        | starts_with("f_prices_paid") | starts_with("f_prices_received")) 

important_data.z = create_z_scores_for_df(important_data)

important_data$diffusion = 0

for (i in 3:nrow(important_data))
{
  increased = 0
  total = 0
  for (j in 2:(ncol(important_data)-1))
  {

    if (is.na(important_data[i,j]) | is.na(important_data[i-1,j]) | is.na(important_data[i-2,j]))
    {
      next
    }
    
    total = total + 1
    if (important_data[i, j] > ((important_data[i-1, j] + important_data[i-2, j]) / 2))
    {
      increased = increased + 1
    }
    important_data$diffusion[i] = as.integer(increased / total * 100)
     
  }
}

# Plotting ####

employment_plot <- plot_ly(employment_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'employment')

f_employment_plot <- plot_ly(f_employment_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'future employment')

wages_plot <- plot_ly(wages_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'wages')

f_wages_plot <- plot_ly(f_wages_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'future wages')

workweek_plot <- plot_ly(workweek_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'average work week')

price_paid_plot <- plot_ly(price_paid_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'prices paid')

price_received_plot <- plot_ly(price_received_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'prices received')

f_price_paid_plot <- plot_ly(f_price_paid_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'future prices paid')

f_price_received_plot <- plot_ly(f_price_received_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'future prices received')
# margin_plot <- plot_ly(margin_data.z, x = ~date, y = ~diff.z, type = 'scatter', mode = 'lines') %>%
#                   layout(title = 'margin')

demand_plot <- plot_ly(demand_data.z, x = ~date, y = ~mean.z.3ma, type = 'scatter', mode = 'lines') %>%
  layout(title = 'demand (revenue for services and new orders for man)')

f_demand_plot <- plot_ly(f_demand_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'future demand (revenue for services and new orders for man)')

biz_climate_plot <- plot_ly(biz_climate_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'business climate')

f_biz_climate_plot <- plot_ly(f_biz_climate_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'future business climate')

diffusion_plot <- plot_ly(important_data, x = ~date, y = ~diffusion, type = 'scatter', mode = 'lines') %>%
  layout(title = 'diffusion index')

aggregate_plot <- plot_ly(important_data.z, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  layout(title = 'aggregate data points')

# Shiny server ####


fedSurveyServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$employment_plot1 <- renderPlotly({
        employment_plot
      })
      
      output$employment_plot2 <- renderPlotly({
        f_employment_plot
      })
      
      output$employment_plot3 <- renderPlotly({
        wages_plot
      })
      
      output$employment_plot4 <- renderPlotly({
        f_wages_plot
      })
      
      output$employment_plot5 <- renderPlotly({
        workweek_plot
      })
      
      output$inflation_plot1 <- renderPlotly({
        price_paid_plot
      })
      
      output$inflation_plot2 <- renderPlotly({
        price_received_plot
      })
      
      output$inflation_plot3 <- renderPlotly({
        f_price_paid_plot
      })
      
      output$inflation_plot4 <- renderPlotly({
        f_price_received_plot
      })
      
      output$demand_plot1 <- renderPlotly({
        demand_plot
      })
      
      output$demand_plot2 <- renderPlotly({
        f_demand_plot
      })
      
      output$climate_plot1 <- renderPlotly({
        biz_climate_plot
      })
      
      output$climate_plot2 <- renderPlotly({
        f_biz_climate_plot
      })
      
      output$diffusion_plot <- renderPlotly({
        diffusion_plot
      })
      
      output$aggregate_plot <- renderPlotly({
        aggregate_plot
      })
    }
  )
}



