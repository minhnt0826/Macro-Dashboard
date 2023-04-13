# library(zoo)
# library(lubridate)
tightening_ci = fred_request_data("DRTSCILM") %>%
  mutate(shifted_date = date %m+% months(15))

ci_loans = fred_request_data("H8B1023NCBCMG") %>%
  rename(change_annualized = value) %>%
  mutate(change = ((100 + change_annualized) / 100)^(1/12)) %>%
  mutate(value = 100 * cumprod(change)) %>%
  mutate(growth_3m = n_month_growth_ann(value, 3),
         growth_6m = n_month_growth_ann(value, 6),
         growth_12m = n_month_growth_ann(value, 12))


ci_loans_y2 <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "ci loans yoy",
  range = c(-30,15))


tightening_plot = plot_ly(data = tightening_ci, x = ~shifted_date, y = ~-value, type = 'scatter', mode = 'lines', name = "tightening")
tightening_plot <- tightening_plot %>%
  add_trace(data = ci_loans, x = ~date, y = ~growth_12m,  yaxis = "y2", type = 'scatter', name = "loans growth", mode = 'lines', connectgaps = TRUE )

tightening_plot <- tightening_plot %>% layout(
  yaxis2 = ci_loans_y2,
  xaxis = list(title="date"),
  yaxis = list(title="tightening standards")
)

tightening_plot
# 
# 
# 
# tightening_card = fred_request_data("DRTSCLCC") %>%
#   mutate(shifted_date = date %m+% months(15))
# 
# card_loans = fred_request_data("H8B1023NCBCMG") %>%
#   rename(change_annualized = value) %>%
#   mutate(change = ((100 + change_annualized) / 100)^(1/12)) %>%
#   mutate(value = 100 * cumprod(change)) %>%
#   mutate(growth_3m = n_month_growth_ann(value, 3),
#          growth_6m = n_month_growth_ann(value, 6),
#          growth_12m = n_month_growth_ann(value, 12)) 
# 
# 
# card_loans_y2 <- list(
#   tickfont = list(color = "red"),
#   overlaying = "y",
#   side = "right",
#   title = "card loans yoy",
#   range = c(-35,20))
# 
# 
# tightening_plot_2 = plot_ly(data = tightening_card, x = ~shifted_date, y = ~-value, type = 'scatter', mode = 'lines', name = "tightening")
# tightening_plot_2 <- tightening_plot_2 %>% 
#   add_trace(data = card_loans, x = ~date, y = ~growth_12m,  yaxis = "y2", type = 'scatter', name = "loans growth", mode = 'lines', connectgaps = TRUE )
# 
# tightening_plot_2 <- tightening_plot_2 %>% layout(
#   yaxis2 = card_loans_y2,
#   xaxis = list(title="date"),
#   yaxis = list(title="tightening standards")
# )
# 
# tightening_plot_2
# 
# library(ggplot2)
# 
# nfp_payrolls = fred_request_data("PAYEMS") %>%
#   mutate(chng = value - lag(value , 1)) %>%
#   right_join(employment_data.z, by = "date") %>%
#   select(date, chng, mean.z) %>%
#   filter(chng > -1000 & chng < 1000)
# 
# plot1 <- plot_ly(nfp_payrolls, x = ~date, y = ~mean.z, type = 'scatter') %>%
#   add_trace(x = ~date, y = ~chng, name = "nfp", mode = 'lines')
# plot1
# 
# ggplot(nfp_payrolls, aes(x=chng, y=mean.z)) + 
#   geom_point()+
#   geom_smooth(method=lm) 
# 
# 
# chng_y2 <- list(
#   tickfont = list(color = "red"),
#   overlaying = "y",
#   side = "right",
#   title = "ratio",
#   range = c(-800,600))
# 
# plot2 = plot_ly(data = nfp_payrolls, x = ~date, y = ~mean.z, type = 'scatter', mode = 'lines')
# plot2 <- plot2 %>%
#   add_trace(data = nfp_payrolls, x = ~date, y = ~chng,  yaxis = "y2", type = 'scatter',
#             mode = 'lines', connectgaps = TRUE )
# 
# plot2 <- plot2 %>% layout(
#   yaxis2 = chng_y2,
#   xaxis = list(title="date"),
#   yaxis = list(title="mean z")
# )
# 
# plot2

# 
# nfp_weather = read_excel("/Users/nguyenthanhminh/Downloads/weather-adjustment-time-series.xlsx", sheet = "Data") %>%
#   mutate(time = paste(time, "-01", sep = "")) %>%
#   mutate(date = as.Date(time, format = "%Ym%m-%d")) %>%
#   mutate(weather_adjustment = (WA_RH + WA_noRH) / 2) %>%
#   select(date, weather_adjustment) %>%
#   left_join(fred_request_data("PAYEMS"), by = "date") %>%
#   mutate(chng = value - lag(value , 1)) %>%
#   filter(weather_adjustment > 0.1 | lag(weather_adjustment, 1) > 0.1)
# 
# data = fredr_releases_dates(
#   realtime_start  = as.Date("2023-03-01"))




# retail_sales_furniture = fred_request_data("RSFHFS") %>%
#   mutate(sales_furniture_chng = value - lag(value, 12)) %>%
#   select(date, sales_furniture_chng)
# 

real_residential_spending = read_excel("/Users/nguyenthanhminh/Documents/Stocks/Macro Dashboard/data/Archived/construction_spending_02.xls") %>%
  select(date, value = `RESIDENTIAL BUILDINGS`) %>%
  mutate(value = value * 0.9575) %>%
  filter(date < "2002-01-01") %>%
  rbind(., fred_request_data("TLRESCONS")) %>%
  left_join(fred_request_data("CUSR0000SA0L2"), by = "date") %>%
  mutate(value = value.x / value.y * 277.166) %>%
  mutate(value = value / lag(value, 6)) %>%
  select(date, resi_spending = value) 

housing_under_construction_lm = fred_request_data("CES2023610001") %>%
  mutate(construction_emp_chng = value / lag(value, 12)) %>%
  select(date, construction_emp_chng) %>%
  left_join(fred_request_data("UNDCON5MUSA"), by = "date") %>%
  mutate(housing_5_chng = value / lag(value, 12)) %>%
  select(date, construction_emp_chng, housing_5_chng) %>%
  left_join(fred_request_data("UNDCON1USA"), by = "date") %>%
  mutate(housing_1_chng = value / lag(value, 12)) %>%
  select(date, construction_emp_chng, housing_5_chng, housing_1_chng) %>%
  left_join(fred_request_data("PERMIT5"), by = "date") %>%
  mutate(permits5 = SMA(value / lag(value, 12), 3)) %>%
  select(date, construction_emp_chng, housing_5_chng, housing_1_chng, permits5) %>%
  left_join(fred_request_data("PERMIT1"), by = "date") %>%
  mutate(permits1 = SMA(value / lag(value, 12), 3)) %>%
  left_join(real_residential_spending, by = "date") %>%
  select(date, construction_emp_chng, housing_5_chng, housing_1_chng, permits5, permits1, resi_spending) %>%
  filter(date >= "1971-01-01")

# housing_under_construction_lm = fred_request_data("CES2023610001") %>%
#   mutate(construction_emp = value) %>%
#   select(date, construction_emp) %>%
#   left_join(fred_request_data("UNDCON5MUSA"), by = "date") %>%
#   mutate(housing_5 = value) %>%
#   select(date, construction_emp, housing_5) %>%
#   left_join(fred_request_data("UNDCON1USA"), by = "date") %>%
#   mutate(housing_1 = value) %>%
#   select(date, construction_emp, housing_5, housing_1) %>%
#   filter(date >= "1971-01-01")
# 
write.csv(housing_under_construction_lm, file = "/Users/nguyenthanhminh/Documents/Stocks/Python Projects/ML macro data/housing_data.csv", row.names = F)
#   left_join(retail_sales_furniture, by = "date") %>%
#   mutate(lead_construction_emp_chng = lead(construction_emp_chng, 12))
# 
# 
# 
# linear <- lm(construction_emp_chng ~ housing_5_chng + housing_1_chng, data = housing_under_construction_lm)
# linear2 <- lm(lead_construction_emp_chng ~ housing_5_chng + housing_1_chng, data = housing_under_construction_lm)
# 
# summary(linear)
# summary(linear2)
# 
# 
# housing_under_construction_lm$predict <- predict(linear2, newdata = housing_under_construction_lm)
# 
# # plot = plot_ly(housing_under_construction_lm, x = ~date, )
# 
# knn = train(construction_emp_chng ~ housing_5_chng + housing_1_chng, data = housing_under_construction_lm,
#             method = "knn",
#             tuneLength = 10)
# 
# knn_lead = train(lead_construction_emp_chng ~ housing_5_chng + housing_1_chng, data = housing_under_construction_lm %>% filter(lead_construction_emp_chng >= -50000),
#                  method = "knn",
#                  tuneLength = 10)
# 
# housing_under_construction_lm$knn_pred = predict(knn_lead, newdata = housing_under_construction_lm)




# 
#
# isee$date_new = 
# 
# personal_saving = fred_request_data("PMSAVE") %>% 
#   mutate(value = value / 12) %>%
#   mutate(cum_value = 60 + cumsum(value)) %>%
#   select(date, cum_value ) %>%
#   left_join(fred_request_data("CPIAUCSL"), by = "date") %>%
#   mutate(real_savings = cum_value / value) %>%
#   create_growth_data_for_df(., .$real_savings)
# 
# plot = plot_ly(personal_saving, x = ~date, y = ~real_savings, type = "scatter", mode = "lines")
# plot
# 
# plot1 = create_plotly_plot_with_growth_data(personal_saving)
# plot1

# spx_earning = read_excel("/Users/nguyenthanhminh/Documents/Stocks/Excels/P to IE.xlsx", sheet = "Sheet2")
# spx_earning = spx_earning %>%
#   mutate(date = as.Date(date))%>%
#   filter(date >= "1978-01-01")
# 
# spx_earning = spx_earning %>%
#   arrange(date) %>%
#   mutate(ttm_eps = rollsum(eps, 4, align = "right", fill = NA)) %>% 
#   mutate(avg_last_2yr = rollsum(eps, 8, align = "right", fill = NA)/2) %>% 
#   mutate(eps_relative_2yr =  ((ttm_eps / avg_last_2yr) - 1)*100 ) %>%
#   mutate(yoy = growth_over_lag(eps, 4))
# 
# plot = plot_ly(spx_earning, x = ~date, y = ~SMA(yoy , 1), type = "scatter", mode = "lines")
# plot
# 
# plot1 = plot_ly(spx_earning, x = ~date, y = ~eps_relative_2yr, type = "scatter", mode = "lines")
# plot1

# 
# plot = read.csv("/Users/nguyenthanhminh/Documents/Stocks/Python Projects/ML macro data/housing_data_pred.csv") %>%
#   mutate(date = shift_date_series(date, 0, 9, 0)) %>%
#   plot_ly(., x = ~date, y=~construction_emp_chng, type = "scatter", mode = "lines") %>%
#   add_trace(y = ~y_pred_linear, name = "linear pred") %>%
#   add_trace(y = ~y_pred_knn, name = "knn pred") %>%
#   add_trace(y = ~(y_pred_knn + y_pred_linear) / 2, name = "mean")
# plot


personal_witholding = fred_request_data("W988RC1Q027SBEA") %>%
  left_join(fred_request_data("W279RC1Q027SBEA"), by = "date") %>%
  mutate(net_investment = value.x - value.y) %>%
  left_join(fred_request_data("W986RC1Q027SBEA"), by = "date") %>%
  mutate(witholding = value - net_investment) %>%
  select(date, witholding) %>%
  mutate(value = witholding / 4) %>%
  mutate(cum_value = 60 + cumsum(value)) %>%
  select(date, cum_value ) %>%
  left_join(fred_request_data("CPIAUCSL"), by = "date") %>%
  mutate(real_witholding = cum_value / value) %>%
  create_growth_data_for_df(., .$real_witholding)

plot = plot_ly(personal_witholding, x = ~date, y = ~real_witholding, type = "scatter", mode = "lines") %>%
  layout(yaxis = list(type = "log"))
plot

plot1 = create_plotly_plot_with_growth_data(personal_witholding)
plot1


