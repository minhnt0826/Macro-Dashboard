

# spy_intraday_org = read.csv("/Users/nguyenthanhminh/Downloads/extended_intraday_SPY_1min_year1month1_adjusted.csv")

library(dplyr)
library(lubridate)
library(hms)
library(pracma)
library(TTR)


source("alphaVantage.R")

spy_intraday_vwap = spy_intraday_org %>%
  mutate(timestamp = ymd_hms(time)) %>%
  filter(as_hms(timestamp) <= as_hms("16:00:00")) %>%
  filter(as_hms(timestamp) >= as_hms("09:31:00")) %>%
  mutate(avg_hi_lo = (high + low) / 2) %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(vwap = weighted.mean(avg_hi_lo, volume)) 

spy_close = get_stock_close_data("SPY")

spy_data = spy_close %>%
  select(date, close, high, low , volume) %>%
  rename(close = "adjusted_close") %>%
  mutate(v = volume / SMA(volume, 21)) %>%
  mutate(log_hi_clo = log(high, close),
         log_lo_clo = log(low, close))


