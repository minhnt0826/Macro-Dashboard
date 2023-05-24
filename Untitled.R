trade_trans_emp = fred_request_data("USTPU") %>%
  mutate(yoy = n_month_growth_ann(value, 12)) %>%
  mutate(yoy_sma_120 = SMA(yoy, 120)) %>%
  mutate(yoy_relative_10_years = yoy - yoy_sma_120)

plot = plot_ly(data = trade_trans_emp, x=~date, y=~yoy_relative_10_years, mode = "lines")
plot


plot1 = plot_ly(data = trade_trans_emp, x=~date, y=~yoy_sma_120, mode = "lines")
plot1


leisure = fred_request_data("USLAH") %>%
  mutate(yoy = n_month_growth_ann(value, 12)) %>%
  mutate(yoy_sma_120 = SMA(yoy, 120)) %>%
  mutate(yoy_relative_10_years = yoy - yoy_sma_120)

plot = plot_ly(data = leisure, x=~date, y=~yoy_relative_10_years, mode = "lines")
plot


plot1 = plot_ly(data = leisure, x=~date, y=~yoy_sma_120, mode = "lines")
plot1

manufacturing_emp = fred_request_data("MANEMP") %>%
  mutate(yoy = n_month_growth_ann(value, 12)) %>%
  mutate(yoy_sma_120 = SMA(yoy, 120)) %>%
  mutate(yoy_relative_10_years = yoy - yoy_sma_120)




nfp_private = fred_request_data("USPRIV") %>%
  mutate(yoy = n_month_growth_ann(value, 12)) %>%
  mutate(yoy_sma_120 = SMA(yoy, 120)) %>%
  mutate(yoy_med_120 = rollmedian(yoy, k = 120, fill = NA, align = "right")) %>%
  mutate(yoy_relative_10_years = yoy - yoy_sma_120) %>%
  mutate(yoy_relative_10_years_med = yoy - yoy_med_120)

temp_help = fred_request_data("TEMPHELPS") %>%
  mutate(yoy = n_month_growth_ann(value, 12)) %>%
  mutate(yoy_sma_120 = SMA(yoy, 120)) %>%
  mutate(yoy_med_120 = rollmedian(yoy, k = 120, fill = NA, align = "right")) %>%
  mutate(yoy_relative_10_years = yoy - yoy_sma_120) %>%
  mutate(yoy_relative_10_years_med = yoy - yoy_med_120) %>%
  add_months_to_df(., 3) %>%
  mutate(yoy_relative_10_years_lag3 = lag(yoy_relative_10_years, 3),
         yoy_relative_10_years_lag3_med = lag(yoy_relative_10_years_med, 3))

plot2 = nfp_private %>%
  plot_ly(., x = ~date, y=~yoy_relative_10_years, name = "nfp",type = "scatter", mode = "lines") %>%
  add_second_yxis_plotly(.,
                         temp_help,
                         temp_help$date,
                         temp_help$yoy_relative_10_years,
                         "Temp help")

plot2


plot2.1 = nfp_private %>%
  plot_ly(., x = ~date, y=~yoy_relative_10_years_med, name = "nfp",type = "scatter", mode = "lines") %>%
  add_second_yxis_plotly(.,
                         temp_help,
                         temp_help$date,
                         temp_help$yoy_relative_10_years_lag3_med,
                         "Temp help")

plot2.1





goods_producing_emp = fred_request_data("USGOOD") %>%
  mutate(yoy = n_month_growth_ann(value, 12)) %>%
  mutate(yoy_sma_120 = SMA(yoy, 120)) %>%
  mutate(yoy_med_120 = rollmedian(yoy, k = 120, fill = NA, align = "right")) %>%
  mutate(yoy_relative_10_years = yoy - yoy_sma_120) %>%
  mutate(yoy_relative_10_years_med = yoy - yoy_med_120)

services_producing_emp = fred_request_data("CES0800000001") %>%
  # left_join(fred_request_data("USEHS"), by = "date") %>%
  # mutate(value = value.x-value.y) %>%
  mutate(yoy = n_month_growth_ann(value, 12)) %>%
  mutate(yoy_sma_120 = SMA(yoy, 120)) %>%
  mutate(yoy_med_120 = rollmedian(yoy, k = 120, fill = NA, align = "right")) %>%
  mutate(yoy_relative_10_years = yoy - yoy_sma_120) %>%
  mutate(yoy_relative_10_years_med = yoy - yoy_med_120)

plot3 = goods_producing_emp %>%
  plot_ly(., x = ~date, y=~yoy_relative_10_years, name = "goods",type = "scatter", mode = "lines") %>%
    add_trace(data = services_producing_emp,
              x = services_producing_emp$date,
              y = services_producing_emp$yoy_relative_10_years,
              name = "Services")
                         

plot3


plot4 = goods_producing_emp %>%
  plot_ly(., x = ~date, y=~yoy_relative_10_years_med, name = "goods",type = "scatter", mode = "lines") %>%
  add_trace(data = services_producing_emp,
            x = services_producing_emp$date,
            y = services_producing_emp$yoy_relative_10_years_med,
            name = "Services")


plot4

plot5 = goods_producing_emp %>%
  plot_ly(., x = ~date, y=~yoy_med_120, name = "goods",type = "scatter", mode = "lines") %>%
  add_trace(data = services_producing_emp,
            x = services_producing_emp$date,
            y = services_producing_emp$yoy_med_120,
            name = "Services")


plot5