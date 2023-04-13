library(readxl)
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)
library(seasonal)
library(zoo)
source("fred.R")
source("alphaVantage.R")


rescale_value_range <- function(old_min, old_max, new_min, new_max, value){
  # print(type)
  # if (value <= old_min)
  # {
  #   return(new_min)
  # }
  # 
  # if (value >= old_max)
  # {
  #   return(new_max)
  # }
  # 
  new_value = rescale(value, from = c(old_min,old_max), to = c(new_min,new_max))
       
  new_value = sapply(new_value, function(y) min(max(y,new_min),new_max))
  
  return(new_value)
}


spx_earning = read_excel("/Users/nguyenthanhminh/Documents/Stocks/Excels/P to IE.xlsx", sheet = "Sheet2")
spx_earning = spx_earning %>%
  mutate(date = as.Date(date))%>%
  filter(date >= "1978-01-01")

spx_earning = spx_earning %>%
  arrange(date) %>%
  mutate(ttm_eps = rollsum(eps, 4, align = "right", fill = NA)) %>% 
  mutate(avg_last_2yr = rollsum(eps, 8, align = "right", fill = NA)/2) %>% 
  mutate(eps_relative_2yr =  ((ttm_eps / avg_last_2yr) - 1)*100 ) %>%
  mutate(yoy = eps / lag(eps, 4))



spread_2y10y = fred_request_data(series_id = "T10Y2Y", start_date = "1990-01-01")

# spread_2y10y = spread_2y10y %>%
#   complete(date = seq.Date(min(date), max(date), by='day'))



spread_2y10y = spread_2y10y %>%
  mutate(shifted_date = date %m+% months(16)) %>%
  filter(shifted_date >= "1980-01-01")


# goverment_net_spending = fred_request_data("AD01RC1Q027SBEA")
# goverment_net_spending = goverment_net_spending %>%
#   mutate(last2 = rollsum(value, 2, align = "right", fill = NA)) %>% 
#   mutate(last8 = rollsum(value, 8, align = "right", fill = NA)) %>% 
#   mutate(last2_vs_last8 =  ((last2 * 4 / last8) - 1)*100 ) %>%
#   filter(date >= "1990-01-01")


temp_emp = fred_request_data("TEMPHELPS")

temp_emp = temp_emp %>%
  mutate(three_m_chng = value - lag(value, n = 2)) %>%
  mutate(shifted_date = date %m+% months(6))


B_yield = fred_request_data("BAMLH0A2HYBEY", freq = "m")

B_yield = B_yield %>%
  mutate(yoy = value - lag(value, n = 12)) %>%
  mutate(shifted_date = date %m+% months(6))

# 
# XLP = get_stock_close_data("XLP")
# XLY = get_stock_close_data("XLY")
# SPY = get_stock_close_data("SPY")
# 
# RCD = get_stock_close_data("RCD")
# RSP = get_stock_close_data("RSP")

# XHB = XHB %>%
#   rename(XHB.Close = adjusted_close) %>%
#   mutate(date = as.Date(timestamp)) %>%
#   arrange(date)


xly_to_xlp = XLP %>%
  rename(XLP.close = adjusted_close) %>%
  left_join(XLY, by = "date") %>%
  rename(XLY.close = adjusted_close) %>%
  select(date, XLP.close, XLY.close) %>%
  mutate(ratio = XLY.close / XLP.close) %>%
  mutate(ratio_200dma = rollmean(ratio, 200, align = 'right', fill = NA)) %>%
  mutate(ratio_20dma = rollmean(ratio, 20, align = 'right', fill = NA)) %>%
  mutate(vs_prev_yr = ratio_20dma - lag(ratio_20dma, 252)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(shifted_date = date %m+% months(6))

rcd_to_rsp = RCD %>%
  rename(RCD.close = adjusted_close) %>%
  left_join(RSP, by = "date") %>%
  rename(RSP.close = adjusted_close) %>%
  select(date, RCD.close, RSP.close) %>%
  mutate(ratio = RCD.close / RSP.close) %>%
  mutate(ratio_200dma = rollmean(ratio, 200, align = 'right', fill = NA)) %>%
  mutate(ratio_20dma = rollmean(ratio, 20, align = 'right', fill = NA)) %>%
  mutate(vs_prev_yr = ratio_20dma - lag(ratio_20dma, 252)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(shifted_date = date %m+% months(6))


# good
  
# xly_to_spy = XLY %>%
#   left_join(SPY, by = "date") %>%
#   select(date, XLY.Close, SPY.Close) %>%
#   mutate(ratio = XLY.Close / SPY.Close) %>%
#   mutate(ratio_20dma = rollmean(ratio, 20, align = 'right', fill = NA)) %>%
#   mutate(diff = ratio_20dma / lag(ratio_20dma, 120)) %>%
#   mutate(date = as.Date(date)) %>%
#   mutate(shifted_date = date %m+% months(12))

# xhb_to_spy = XHB %>%
#   left_join(SPY, by = "date") %>%
#   select(date, XHB.Close, SPY.Close) %>%
#   mutate(ratio = XHB.Close / SPY.Close) %>%
#   mutate(ratio_200dma = rollmean(ratio, 200, align = 'right', fill = NA)) %>%
#   mutate(ratio_20dma = rollmean(ratio, 50, align = 'right', fill = NA)) %>%
#   mutate(diff = ratio_20dma / ratio_200dma) %>%
#   mutate(date = as.Date(date)) %>%
#   mutate(shifted_date = date %m+% months(14)) %>%
#   mutate(diff_20dma = rollmean(diff, 50, align = 'right', fill = NA)) 


# Tightening credit ####

banks_tightening = fred_request_data(series_id = "DRTSCILM") %>%
  mutate(shifted_date = date %m+% months(8)) 


# BBKI ####
bbki_coincident = fred_request_data("BBKMCOIX")

bbki_coincident = bbki_coincident %>%
  mutate(three_m_chng = value - lag(value, 3)) %>%
  filter(date >= "1999-01-01")

# Bank credit ####

bank_credit = fred_request_data("TOTBKCR", freq = "m", agg = "eop")
bank_credit = bank_credit %>%
  mutate(yoy = value / lag(value, n = 12))

# Plotting ####

# Temp emp plot ####
temp_emp_y2 <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "temp emp",
  range = c(-200,200))


temp_emp_plot = plot_ly(data = spx_earning, x = ~date, y = ~eps_relative_2yr, type = 'scatter', mode = 'lines')
temp_emp_plot <- temp_emp_plot %>% 
  add_trace(data = temp_emp, x = ~shifted_date, y = ~three_m_chng,  yaxis = "y2", type = 'scatter', 
            mode = 'lines', connectgaps = TRUE )

temp_emp_plot <- temp_emp_plot %>% layout(
  yaxis2 = temp_emp_y2,
  xaxis = list(title="date"),
  yaxis = list(title="eps")
)

temp_emp_plot





# spread_y2 <- list(
#   tickfont = list(color = "red"),
#   overlaying = "y",
#   side = "right",
#   title = "spread",
#   range = c(-1,3))
# 
# spread_plot = plot_ly(data = spx_earning, x = ~date, y = ~eps_relative_2yr, type = 'scatter', mode = 'lines')
# spread_plot <- spread_plot %>%
#   add_trace(data = spread_2y10y, x = ~shifted_date, y = ~value,  yaxis = "y2", type = 'scatter',
#             mode = 'lines', connectgaps = TRUE )
# 
# spread_plot <- spread_plot %>% layout(
#   yaxis2 = spread_y2,
#   xaxis = list(title="date"),
#   yaxis = list(title="eps")
# )
# 
# spread_plot


# B Plot ####
B_plot_y2 <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "yield",
  range = c(10,-10))

B_yield_plot = plot_ly(data = spx_earning, x = ~date, y = ~eps_relative_2yr, type = 'scatter', mode = 'lines')
B_yield_plot <- B_yield_plot %>%
  add_trace(data = B_yield, x = ~shifted_date, y = ~yoy,  yaxis = "y2", type = 'scatter',
            mode = 'lines', connectgaps = TRUE )

B_yield_plot <- B_yield_plot %>% layout(
  yaxis2 = B_plot_y2,
  xaxis = list(title="date"),
  yaxis = list(title="eps")
)

B_yield_plot

# XLY XLP Plot ####

xly_to_xlp_y2 <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "ratio")

xly_to_xlp_plot = plot_ly(data = spx_earning, x = ~date, y = ~eps_relative_2yr, type = 'scatter', mode = 'lines')
xly_to_xlp_plot <- xly_to_xlp_plot %>%
  add_trace(data = xly_to_xlp, x = ~shifted_date, y = ~vs_prev_yr,  yaxis = "y2", type = 'scatter',
            mode = 'lines', connectgaps = TRUE )

xly_to_xlp_plot <- xly_to_xlp_plot %>% layout(
  yaxis2 = xly_to_xlp_y2,
  xaxis = list(title="date"),
  yaxis = list(title="eps")
)

xly_to_xlp_plot

# RCD to RSP plot
rcd_to_rsp_y2 <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "ratio")

rcd_to_rsp_plot = plot_ly(data = spx_earning, x = ~date, y = ~eps_relative_2yr, type = 'scatter', mode = 'lines')
rcd_to_rsp_plot <- rcd_to_rsp_plot %>%
  add_trace(data = rcd_to_rsp, x = ~shifted_date, y = ~vs_prev_yr,  yaxis = "y2", type = 'scatter',
            mode = 'lines', connectgaps = TRUE )

rcd_to_rsp_plot <- rcd_to_rsp_plot %>% layout(
  yaxis2 = rcd_to_rsp_y2,
  xaxis = list(title="date"),
  yaxis = list(title="eps")
)

rcd_to_rsp_plot

# tightening plot ####

banks_tightening_y2 <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "percentage of banks tightening credit",
  range = c(80,-60))

banks_tightening_plot = plot_ly(data = spx_earning, x = ~date, y = ~eps_relative_2yr, type = 'scatter', mode = 'lines')
banks_tightening_plot <- banks_tightening_plot %>%
  add_trace(data = banks_tightening, x = ~shifted_date, y = ~value,  yaxis = "y2", type = 'scatter',
            mode = 'lines', connectgaps = TRUE )

banks_tightening_plot <- banks_tightening_plot %>% layout(
  yaxis2 = banks_tightening_y2,
  xaxis = list(title="date"),
  yaxis = list(title="eps")
)

banks_tightening_plot

# BBKI plot ####

bbki_y2 <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "BBKI Coincident index",
  range = c(-4,2))

bbki_plot = plot_ly(data = spx_earning, x = ~date, y = ~eps_relative_2yr, type = 'scatter', mode = 'lines')
bbki_plot <- bbki_plot %>%
  add_trace(data = bbki_coincident, x = ~date, y = ~value,  yaxis = "y2", type = 'scatter',
            mode = 'lines', connectgaps = TRUE )

bbki_plot <- bbki_plot %>% layout(
  yaxis2 = bbki_y2,
  xaxis = list(title="date"),
  yaxis = list(title="eps")
)

bbki_plot

# Bank credit plot ####
bank_credit_y2 <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "bank credit yoy")

bank_credit_plot = plot_ly(data = spx_earning, x = ~date, y = ~eps_relative_2yr, type = 'scatter', mode = 'lines')
bank_credit_plot <- bank_credit_plot %>%
  add_trace(data = bank_credit, x = ~date, y = ~yoy,  yaxis = "y2", type = 'scatter',
            mode = 'lines', connectgaps = TRUE )

bank_credit_plot <- bank_credit_plot %>% layout(
  yaxis2 = bank_credit_y2,
  xaxis = list(title="date"),
  yaxis = list(title="eps")
)

bank_credit_plot


earnings_leading = xly_to_xlp %>% 
  mutate(year_mon = as.yearmon(shifted_date)) %>%
  select(year_mon, vs_prev_yr) %>%
  group_by(year_mon) %>%
  summarise(vs_prev_yr = mean(vs_prev_yr, na.rm = TRUE)) %>%
  mutate(year_qtr = as.yearqtr(year_mon)) %>%
  mutate(earnings1 = rescale_value_range(-0.5,0.7,-20, 25, vs_prev_yr)) 


earnings_leading = temp_emp %>% 
  mutate(year_mon = as.yearmon(shifted_date)) %>%
  select(year_mon, three_m_chng) %>%
  right_join(earnings_leading, by = "year_mon") %>%
  mutate(earnings2 = rescale_value_range(-180,100,-28, 20, three_m_chng))

earnings_leading = B_yield %>% 
  mutate(year_mon = as.yearmon(shifted_date)) %>%
  select(year_mon, yoy) %>%
  right_join(earnings_leading, by = "year_mon") %>%
  mutate(earnings3 = rescale_value_range(10,-10,-30, 30, yoy))

earnings_leading = banks_tightening %>% 
  mutate(year_qtr = as.yearqtr(shifted_date)) %>%
  select(year_qtr, value) %>%
  right_join(earnings_leading, by = "year_qtr") %>%
  mutate(earnings4 = rescale_value_range(83,-32,-35, 20, value)) %>%
  select(year_mon, year_qtr, earnings1, earnings2, earnings3, earnings4) %>%
  mutate(earnings = rowMeans(across(starts_with("earnings")))) %>%
  mutate(date = as.Date(year_mon))

earnings_y2 <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "leading earnings indicator",
  range = c(-30,30))

earnings_plot = plot_ly(data = spx_earning, x = ~date, y = ~eps_relative_2yr, type = 'scatter', mode = 'lines')
earnings_plot <- earnings_plot %>%
  add_trace(data = earnings_leading, x = ~date, y = ~earnings, type = 'scatter',
            mode = 'lines', connectgaps = TRUE )

earnings_plot <- earnings_plot %>% layout(
  xaxis = list(title="date"),
  yaxis = list(title="eps")
)

earnings_plot











