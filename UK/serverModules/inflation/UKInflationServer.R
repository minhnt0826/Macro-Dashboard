uk_cpi_goods = ons_request_data("UKCPIGOODS") %>%
  create_growth_data_for_df(., .$value) 

uk_cpi_svc = ons_request_data("UKCPISERVICES") %>%
  create_growth_data_for_df(., .$value) 

uk_inflation_sectors_plot = plot_ly(uk_cpi_goods, x=~date, y=~SMA(growth_3m,3), name = "goods", mode = "lines", type = "scatter") %>%
  add_second_yxis_plotly(.,
                         uk_cpi_svc,
                         x = uk_cpi_svc$date,
                         y = ~SMA(growth_3m,3),
                         title = "services")

uk_cpi = ons_request_data("UKCPI") %>%
  create_growth_data_for_df(., .$value)

uk_cpi_plot = plot_ly(uk_cpi, x=~date, y=~SMA(growth_3m3m, 1), name = "3m3m", mode = "lines", type = "scatter") %>%
  add_trace(x = ~date, y = ~growth_6m, name = "6m")
uk_cpi_plot

uk_m4_lending = boe_request_data("LPMBC69") %>%
  create_growth_data_for_df(., .$value)

uk_m4 = boe_request_data("LPMAUYN") %>%
  create_growth_data_for_df(., .$value)
 

uk_m2 = boe_request_data("LPMVWYW") %>%
  create_growth_data_for_df(., .$value) %>%
  filter(date >= "1988-01-01")


uk_core_cpi = ons_request_data("UKCORECPI") %>%
  create_growth_data_for_df(., .$value)

plot_ly(uk_m2, x=~date, y=~growth_12m, name = "m2", mode = "lines", type = "scatter") %>%
  add_second_yxis_plotly(.,
                         data = uk_cpi,
                         x = ~date,
                         y=~growth_12m,
                         "cpi")

uk_core_cpi_plot = plot_ly(uk_core_cpi, x=~date, y=~SMA(growth_3m3m, 1), name = "3m3m", mode = "lines", type = "scatter") %>%
  add_trace(x = ~date, y = ~growth_6m, name = "6m")
uk_core_cpi_plot

uk_ppi = ons_request_data("UKPPIINPUT") %>%
  create_growth_data_for_df(., .$value)

uk_ppi_plot = plot_ly(uk_ppi, x=~date, y=~growth_3m3m, name = "3m3m", mode = "lines", type = "scatter") %>%
  add_trace(x = ~date, y = ~growth_6m, name = "6m")
uk_ppi_plot



uk_ppi_output = ons_request_data("UKPPIOUTPUT") %>%
  create_growth_data_for_df(., .$value)

uk_ppi_output_plot = plot_ly(uk_ppi_output, x=~date, y=~growth_3m3m, name = "3m3m", mode = "lines", type = "scatter") %>%
  add_trace(x = ~date, y = ~growth_6m, name = "6m")

uk_ppi_output_plot


uk_ppi_food_output = ons_request_data("UKPPIOUTPUTFOOD") %>%
  create_growth_data_for_df(., .$value)

uk_ppi_food_output_plot = plot_ly(uk_ppi_food_output, x=~date, y=~growth_3m3m, name = "3m3m", mode = "lines", type = "scatter") %>%
  add_trace(x = ~date, y = ~growth_6m, name = "6m")

uk_ppi_food_output_plot



# 
# eu_cpi = read_excel("/Users/nguyenthanhminh/Downloads/ei_cphi_m__custom_6238647_page_spreadsheet.xlsx", sheet = 4) 
# 
# eu_cpi_t = transpose(eu_cpi) %>%
#   filter(!is.na(V1)) %>%
#   select(date = V1, value = V2) %>%
#   mutate(date = as.Date(as.yearmon(date)),
#          value = as.numeric(value)) %>%
#   mutate(value_seas = seasonal_adjust_series(value, date_start = c(2000,12)))
# 
# eu_core_cpi_plot = eu_cpi_t %>%
#   create_growth_data_for_df(., .$value_seas) %>%
#   create_plotly_plot_with_growth_data(.)
