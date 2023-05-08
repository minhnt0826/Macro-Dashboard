permanent_unrate = fred_request_data("LNS13026638") %>%
  left_join(fred_request_data("CLF16OV"), by = "date") %>%
  mutate(value = (value.x / value.y) * 100) %>%
  select(date, value) %>%
  mutate(value_ma3 = EMA(value, 3),
         value_ma12 = SMA(value, 12)) %>%
  mutate(diff_ma_3_12 = value_ma3 - value_ma12)

permanent_unrate_ma3_ma12_plot = plot_ly(permanent_unrate, x=~date, y=~diff_ma_3_12, mode = "lines") %>%
  layout(title = "Permanent unemployment rate ema3 - sma12")  

unemployment_rate_u2 = fred_request_data("LNS13023621") %>%
  left_join(fred_request_data("CLF16OV"), by = "date") %>%
  mutate(value = (value.x / value.y) * 100) %>%
  select(date, value) %>%
  mutate(value_ma3 = EMA(value, 3),
         value_ma12 = SMA(value, 12)) %>%
  mutate(diff_ma_3_12 = value_ma3 - value_ma12)

unemployment_rate_u2_ma3_ma12_plot = plot_ly(unemployment_rate_u2, x=~date, y=~diff_ma_3_12, mode = "lines") %>%
  layout(title = "Job losers unemployment rate (u2) ema3 - sma12")  



unemployment_rate = fred_request_data("UNRATE") %>%
  mutate(value_ma3 = EMA(value, 3),
         value_ma12 = SMA(value, 12)) %>%
  mutate(diff_ma_3_12 = value_ma3 - value_ma12)

unemployment_rate_ma3_ma12_plot = plot_ly(unemployment_rate, x=~date, y=~diff_ma_3_12, mode = "lines") %>%
  layout(title = "Unemployment rate ema3 - sma12")  


insured_unemployment_rate = fred_request_data("CCNSA") %>%
  left_join(fred_request_data("COVEMP"), by = "date") %>%
  fill(names(.), .direction = "down") %>%
  mutate(value = (value.x / value.y) * 100) %>%
  mutate(value_ma4 = SMA(value, 4)) %>%
  mutate(yoy = value_ma4 - lag(value_ma4, 52))  %>%
  mutate(insured_employment = 100 - value_ma4) %>%
  mutate(yoy_insured_employment = (insured_employment / lag(insured_employment, 52) - 1) * 100)


insured_unemployment_rate_yoy_plot = plot_ly(insured_unemployment_rate, x=~date, y=~yoy, mode = "lines") %>%
  layout(title = "Insured unemployment rate yoy chng")  

insured_employment_yoy_plot = plot_ly(insured_unemployment_rate, x=~date, y=~yoy_insured_employment, mode = "lines") %>%
  layout(title = "Insured employment yoy")  
