
# Money supply ####

uk_cpi = ons_request_data("UKCPI") %>%
  mutate(yoy = value / lag(value, 12)) 

uk_m4 = boe_request_data("RPMB53Q") %>%
  left_join(uk_cpi, by = "date") %>%
  mutate(real_m4 = value.x / value.y) %>%
  create_growth_data_for_df(., .$real_m4)

uk_m4_plot = plot_ly(uk_m4, x = ~date, y = ~real_m4, type = "scatter", mode = "lines") %>%
  layout(title = "real m4",
         yaxis = list(type = "log"))

uk_m4_growth_plot = uk_m4 %>%
  create_plotly_plot_with_growth_data(.) %>%
  layout(title = "UK m4 growth")

uk_m4_lending = boe_request_data("RPMB57Q") %>%
  left_join(uk_cpi, by = "date") %>%
  mutate(real_m4_lending = value.x / value.y)

uk_m4_lending_plot = plot_ly(uk_m4_lending, x = ~date, y = ~real_m4_lending, type = "scatter", mode = "lines") %>%
  layout(title = "real m4 lending",
         yaxis = list(type = "log"))

uk_m4_lending_growth_plot = uk_m4_lending %>%
  create_growth_data_for_df(., .$real_m4_lending) %>%
  create_plotly_plot_with_growth_data(.)


# 
# 
# plot1.1 = plot_ly(uk_m4, x = ~date, y = ~real_m4_lending, type = "scatter", mode = "lines") %>%
#   layout(title = "real m4 lending")
# 
# plot1.1
# 


uk_inflation_plot = uk_cpi %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(.)


uk_retail_deposits = boe_request_data("LPMB3SF") %>%
  left_join(uk_cpi, by = "date") %>%
  mutate(real_deposits = value.x / value.y)

uk_retail_deposits_plot = plot_ly(uk_retail_deposits, x = ~date, y = ~real_deposits, type = "scatter", mode = "lines") %>%
  layout(title = "real m4",
         yaxis = list(type = "log"))

uk_retail_deposits_plot

uk_retail_deposits_growth_plot = uk_retail_deposits %>%
  create_growth_data_for_df(., .$real_deposits) %>%
  create_plotly_plot_with_growth_data(.)

uk_retail_deposits_growth_plot



# 
# uk_m1 = boe_request_data("LPMVWYT") %>%
#   create_growth_data_for_df(., .$value)
# 
# uk_m1_plot = plot_ly(uk_m1, x=~date, y=~value, mode = "lines", type = "scatter")
# 
# uk_m1_growth_plot = uk_m1 %>%
#   create_plotly_plot_with_growth_data(.)
# 
# uk_m2 = boe_request_data("LPMVWYW") %>%
#   create_growth_data_for_df(., .$value)
# 
# uk_m2_plot = plot_ly(uk_m2, x=~date, y=~value, mode = "lines", type = "scatter")
# uk_m2_growth_plot = uk_m2 %>%
#   create_plotly_plot_with_growth_data(.)

# Yield curves ####

uk_bank_rate = boe_request_data("IUMBEDR")
uk_5y_yield = boe_request_data("IUMASNPY")
uk_10y_yield = boe_request_data("IUMAMNPY")

plot_ly(uk_bank_rate, x=~date, y=~value, mode = "lines", type = "scatter")

uk_bank_rate_to_5y5y = uk_5y_yield %>%
  left_join(uk_10y_yield, by = "date") %>%
  mutate(cum_10 = (1 + (value.y/100)) ^ 10,
         cum_5 = (1 + (value.x/100)) ^ 5) %>%
  mutate(value = (cum_10/cum_5) ^ (1/5)) %>%
  mutate(v5y5y = (value - 1) * 100) %>%
  select(date, v5y5y) %>%
  left_join(uk_bank_rate, by = "date") %>%
  rename(uk_bank_rate = value) %>%
  mutate(tightness_bankrate = v5y5y - uk_bank_rate)

uk_10y_bank_rate = uk_10y_yield %>%
  left_join(uk_bank_rate, by = "date") %>%
  mutate(value = value.x - value.y)

uk_10y_bank_rate_plot = plot_ly(uk_10y_bank_rate, x=~date, y=~value, mode = "lines", type = "scatter") %>%
  layout(title = " 10 year yield minus bank rate")

uk_5y5y_bank_rate_plot = plot_ly(uk_bank_rate_to_5y5y, x=~date, y=~tightness_bankrate, mode = "lines", type = "scatter") %>%
  layout(title = " 5y5y yield minus bank rate")






UKMonetaryServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$uk_m4_plot <- renderPlotly({
        uk_m4_plot
      })
      
      output$uk_m4_growth_plot <- renderPlotly({
        uk_m4_growth_plot
      })
      
      output$uk_m4_lending_plot <- renderPlotly({
        uk_m4_lending_plot
      })
      
      output$uk_m4_lending_growth_plot <- renderPlotly({
        uk_m4_lending_growth_plot
      })
      
      output$uk_10y_bank_rate_plot <- renderPlotly({
        uk_10y_bank_rate_plot
      })
      
      output$uk_5y5y_bank_rate_plot <- renderPlotly({
        uk_5y5y_bank_rate_plot
      })
    }
  )
}
