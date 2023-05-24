library(seasonal)
library(xts)

# cpi_uk = read.csv("/Users/nguyenthanhminh/Downloads/series-130523.csv") %>% 
#   mutate(date = as.Date(paste0(date, "-01"), format = "%Y %B-%d")) %>%
#   mutate(date = format(date, "%Y:%m"))
# 
# cpi_core_uk_raw = read.csv("/Users/nguyenthanhminh/Downloads/cpi_uk_ex_energy.csv") %>% 
#   mutate(date = as.Date(paste0(date, "-01"), format = "%Y:%m-%d"))

# 
# cpi_core_uk_ts = ts(cpi_core_uk_raw$value, start = c(1988, 01), frequency = 12)
# 
# m = seas(cpi_core_uk_ts)
# plot(m)
# plot(final(m))
# 
# 
# cpi_core_uk_seas = as.data.frame(m)
# 
# 
# cpi_core_uk = cpi_core_uk_seas %>%
#   select(date, final) %>%
#   mutate(value = 100 * cumprod(1 + final/100)) %>%
#   create_growth_data_for_df(., .$value) %>%
#   arrange(date)
# 
# cpi_core_uk_plot = create_plotly_plot_with_growth_data(cpi_core_uk)
#   
# 
# uk_cpi = ons_request_data("UKCPI") %>%
#   left_join(ons_request_data("UKPPIOUTPUT"), by = "date") %>%
#   mutate(yoy_cpi = value.x / lag(value.x, 12),
#          yoy_ppi = value.y / lag(value.y, 12)) %>%
#   add_months_to_df(., 3) %>%
#   mutate(yoy_ppi_lag = lag(yoy_ppi, 3))
# 
# plot = plot_ly(uk_cpi, x=~date, y=~yoy_cpi, type = "scatter", mode = "lines") %>%
#   add_second_yxis_plotly(.,
#                          uk_cpi,
#                          x = ~date,
#                          y = ~yoy_ppi_lag,
#                          title = "ppi")
#   
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
