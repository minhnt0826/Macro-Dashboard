uk_retail_sales = ons_request_data("UKRSIEXFUEL") %>%
  create_growth_data_for_df(., .$value)

uk_retail_sales_plot = plot_ly(uk_retail_sales, x=~date, y=~growth_3m3m, name = "3m3m", mode = "lines", type = "scatter") %>%
  add_trace(x = ~date, y = ~growth_6m, name = "6m") %>%
  add_trace(x = ~date, y = ~growth_12m, name = "12m")

uk_retail_sales_plot

uk_real_retail_sales = ons_request_data("UKRSIEXFUELVOL") %>%
  create_growth_data_for_df(., .$value)

uk_real_retail_sales_plot = plot_ly(uk_real_retail_sales, x=~date, y=~growth_3m3m, name = "3m3m", mode = "lines", type = "scatter") %>%
  add_trace(x = ~date, y = ~growth_6m, name = "6m") %>%
  add_trace(x = ~date, y = ~growth_12m, name = "12m")

uk_real_retail_sales_plot

uk_aggregate_hours = ons_request_data("UKTOTALHOURSWORKED") %>%
  create_growth_data_for_df(., .$value)

uk_aggregate_hours_plot = plot_ly(uk_aggregate_hours, x=~date, y=~growth_3m3m, name = "3m3m", mode = "lines", type = "scatter") %>%
  add_trace(x = ~date, y = ~growth_6m, name = "6m") %>%
  add_trace(x = ~date, y = ~growth_12m, name = "12m")

uk_aggregate_hours_plot