source("fred.R")
source("helper.R")
library(TTR)



calculate_monthly_payment = function(house_price, mortgage_rate, na.rm = TRUE) {
  r = mortgage_rate / 100 / 12
  n = 360
  p = house_price
  
  monthly_payment = p * ((r * (1+r)^n) / ((1+r)^n -1))
  
  return(monthly_payment)
}

# Affordability ####
median_house_prices = fred_request_data("MSPNHSUS")
average_house_prices = fred_request_data("ASPNHSUS")
mortgage_rate_data = fred_request_data("MORTGAGE30US", freq = "m", agg = "avg")
personal_income = fred_request_data("PI")
population = fred_request_data("POPTHM")

personal_income_per_capita = personal_income %>%
  left_join(population, by = "date" ) %>%
  mutate(personal_income_per_capita = (value.x / value.y * 1000000) / 2)

affordability = median_house_prices %>%
  rename(median_house_prices = value) %>%
  left_join(mortgage_rate_data, by = "date") %>%
  rename(mortgage_rate = value) %>%
  left_join(average_house_prices, by = "date") %>%
  rename(average_house_prices = value) %>%
  left_join(personal_income_per_capita, by = "date") %>%
  select(date, median_house_prices, average_house_prices, mortgage_rate, personal_income_per_capita)

affordability = affordability %>%
  mutate(monthly_payment = calculate_monthly_payment(average_house_prices, mortgage_rate)) %>%
  mutate(mortgage_payment_as_income = monthly_payment / personal_income_per_capita * 100 )

affordability_plot <- plot_ly(affordability, x = ~date, y = ~mortgage_payment_as_income, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Monthly morgtage payment as % of income')

affordability_plot


# Construction ####
construction_spending = fred_request_data("TTLCONS") %>%
  rename(construction_spending = value) %>%
  select(date, construction_spending)

construction_spending = construction_spending %>%
  mutate(growth_3m = n_month_growth_ann(construction_spending, 3),
         growth_6m = n_month_growth_ann(construction_spending, 6),
         growth_12m = n_month_growth_ann(construction_spending, 12))

construction_spending_plot1 <- plot_ly(construction_spending, x = ~date, y = ~construction_spending, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Construction spending')

construction_spending_plot2 <- plot_ly(construction_spending, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Construction spending 3m annualized')
         
construction_spending_plot3 <- plot_ly(construction_spending, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
 layout(title = 'Construction spending 6m annualized')

construction_spending_plot4 <- plot_ly(construction_spending, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Construction spending 12m annualized')

residential_construction_spending = fred_request_data("TLRESCONS") %>%
  create_growth_data_for_df(., .$value)

real_residential_construction_spending = fred_request_data("TLRESCONS") %>%
  left_join(fred_request_data("CUSR0000SA0L2"), by = "date") %>%
  mutate(value = value.x / value.y) %>%
  create_growth_data_for_df(., .$value)

residential_construction_spending_plot = create_plotly_plot_with_growth_data(residential_construction_spending) %>%
  layout(title = "Residential construction spending")

real_residential_construction_spending_plot = create_plotly_plot_with_growth_data(real_residential_construction_spending) %>%
  layout(title = "Real residential construction spending")

housing_under_construction_multi = fred_request_data("UNDCON5MUSA") %>%
  create_growth_data_for_df(., .$value)

housing_under_construction_single = fred_request_data("UNDCON1USA") %>%
  create_growth_data_for_df(., .$value)

housing_under_construction_multi_plot <- create_plotly_plot_with_growth_data(housing_under_construction_multi) %>%
  layout(title = 'Multi-family units under construction')

housing_under_construction_single_plot <- create_plotly_plot_with_growth_data(housing_under_construction_single) %>%
  layout(title = 'Single-family units under construction')

housing_starts = fred_request_data("HOUST")
housing_completed = fred_request_data("COMPUTSA")
housing_units_flow = housing_starts %>%
  rename(starts = value) %>%
  inner_join(housing_completed, by = "date") %>%
  mutate(flow = value - starts)

housing_flow_plot1 =  plot_ly(housing_units_flow, x = ~date, y = ~flow, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SMA(flow, 3), name = "3 month avg") %>%
  layout(title = 'Housing starts minus completions')


mbs_holdings = fred_request_data("H8B1301NLGCMG") %>%
  rename(change_annualized = value) %>%
  mutate(change = ((100 + change_annualized) / 100)^(1/12)) %>%
  mutate(value = 100 * cumprod(change))
         
mbs_holdings = create_growth_data_for_df(mbs_holdings, mbs_holdings$value)

mbs_holdings_plot = create_plotly_plot_with_growth_data(mbs_holdings)
mbs_holdings_plot

# Shiny server
housingServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$affordability_plot <- renderPlotly({
        affordability_plot
      })
      
      output$construction_spending_plot1 <- renderPlotly({
        construction_spending_plot1
      })
      
      output$construction_spending_plot2 <- renderPlotly({
        construction_spending_plot2
      })
      
      output$construction_spending_plot3 <- renderPlotly({
        construction_spending_plot3
      })
      
      output$construction_spending_plot4 <- renderPlotly({
        construction_spending_plot4
      })
      
      output$residential_construction_spending_plot <- renderPlotly({
        residential_construction_spending_plot
      })
      
      output$real_residential_construction_spending_plot <- renderPlotly({
        real_residential_construction_spending_plot
      })
      
      
      output$housing_under_construction_multi_plot <- renderPlotly({
        housing_under_construction_multi_plot
      })
      
      output$housing_under_construction_single_plot <- renderPlotly({
        housing_under_construction_single_plot
      })
      
      output$housing_flow_plot1 <- renderPlotly({
        housing_flow_plot1
      })

    }
  )
}



