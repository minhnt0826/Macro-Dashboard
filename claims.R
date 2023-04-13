library(dplyr)
library(lubridate)
library(TTR)
library(zoo)
library(readxl)
library(plotly)
source("fred.R")
source("helper.R")
# 
# growth_over_lag <- function(x, n = 52, na.rm = TRUE) {
#   return((x / lag(x, n) - 1)*100)
# }

populate_week = function(data){
  data$week = 1
  
  week_count = 2
  for(i in 2:nrow(data))
  {
    if (year(data$date[i]) > year(data$date[i-1]))
    {
      week_count = 1
    }
    
    data$week[i] <- week_count
    
    week_count = week_count + 1
  }
  
  return (data)
}


# States level data ####
states_continuing_claims = read_xls("data/States Claims Data/State_continuing_claims.xls", sheet = 2)

states_continuing_claims = states_continuing_claims %>%
  mutate_at(vars(-DATE), ~growth_over_lag (x = ., n = 52)) %>%
  mutate(morethan25 = rowSums(. > 25) -1,
         morethan10 = rowSums(. > 10) -1,
         morethan5 = rowSums(. > 5) - 1,
         morethan0 = rowSums(. > 0) - 1)
  # rowwise() %>%
  # mutate(med = median(c_across(2:51), na.rm = TRUE))  %>%
  # ungroup()

# states_continuing_claims = states_continuing_claims %>%
#   mutate(change_3m = med - lag(med, 12))

continuing_claims_breath_plot <- plot_ly(states_continuing_claims, x = ~DATE, y = ~morethan0, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~morethan5, name = "More than 5") %>%
  add_trace(y = ~morethan10, name = "More than 10") %>%
  add_trace(y = ~morethan25, name = "More than 25") %>%
  # add_trace(y = ~zoo::rollmean(med, k = 4, fill = NA), name = "Median") %>%
  layout(title = 'Continuing claims states breadth')


init_claims = read_xls("data/States Claims Data/State_init_claims.xls", sheet = 2)

init_claims = init_claims %>%
  mutate_at(vars(-DATE), ~growth_over_lag(x = ., n = 52)) %>%
  mutate(morethan25 = rowSums(. > 25) -1,
         morethan10 = rowSums(. > 10) -1,
         morethan5 = rowSums(. > 5) - 1,
         morethan0 = rowSums(. > 0) - 1)
  # rowwise() %>%
  # mutate(med = median(c_across(2:51), na.rm = TRUE))  %>%
  # ungroup()

# init_claims = init_claims %>%
#   mutate(change_3m = med - lag(med, 12))


initial_claims_breath_plot <- plot_ly(init_claims, x = ~DATE, y = ~morethan0, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~morethan5, name = "More than 5") %>%
  add_trace(y = ~SMA(morethan10,4), name = "More than 10") %>%
  add_trace(y = ~morethan25, name = "More than 25") %>%
  # add_trace(y = ~zoo::rollmean(med, k = 4, fill = NA, align = "right"), name = "Median") %>%
  layout(title = 'Initial claims')

CCNSA = fred_request_data("CCNSA") %>%
  mutate(growth_12m = growth_over_lag(value, 52)) %>%
  mutate(chng_12m = value - lag(value, 52))


CCSA = fred_request_data("CCSA") 


ICNSA = fred_request_data("ICNSA") %>%
  mutate(growth_12m = growth_over_lag(value, 52)) %>%
  mutate(chng_12m = value - lag(value, 52))

ICSA = fred_request_data("ICSA")

# Compared vs previous years ####
CCNSA_prev_years = fred_request_data("CCNSA") %>%
  filter((date >= "2018-01-01" & date <= "2019-12-31") | (date >= "2022-01-01" & date <= "2022-12-31")) %>%
  populate_week(.) %>%
  group_by(week) %>%
  summarise(prev_years_value = mean(value))


CCNSA_2023 = fred_request_data("CCNSA") %>%
  filter(date >= "2023-01-01") %>%
  populate_week(.) %>%
  left_join(CCNSA_prev_years, by = "week") %>%
  mutate(yoy = (value / prev_years_value) - 1)


ICNSA_prev_years = fred_request_data("ICNSA") %>%
  filter((date >= "2018-01-01" & date <= "2019-12-31") | (date >= "2022-01-01" & date <= "2022-12-31") ) %>%
  populate_week(.) %>%
  group_by(week) %>%
  summarise(prev_years_value = mean(value))


ICNSA_2023 = fred_request_data("ICNSA") %>%
  filter(date >= "2023-01-01") %>%
  populate_week(.) %>%
  left_join(ICNSA_prev_years, by = "week") %>%
  mutate(yoy = value / prev_years_value - 1)

cc_claims_vs_prev_years_plot <- plot_ly(CCNSA_2023, x = ~date, y = ~yoy, type = 'bar') %>%
  layout(title = "Continuing claims 2023 vs avg years (2018, 2019, 2022)")

ic_claims_vs_prev_years_plot <- plot_ly(ICNSA_2023, x = ~date, y = ~yoy, type = 'bar') %>%
  layout(title = "Initial claims 2023 vs avg years (2018, 2019, 2022)")

# Claims ####

CCNSA = populate_week(CCNSA)
CCSA = populate_week(CCSA)

ICNSA = populate_week(ICNSA)
ICSA = populate_week(ICSA)


claims_plot1.1 <- plot_ly(CCSA, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Continuing claims SA', 
         xaxis = list(type = 'date', tickformat = "%m %Y"))

claims_plot1.2 = plot_ly(CCNSA, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Continuing claims YoY')

claims_plot1.3 = plot_ly(CCNSA, x = ~date, y = ~chng_12m, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SMA(chng_12m, 4), name = "4ma") %>%
  layout(title = 'Continuing claims YoY change')

claims_plot2.1 <- plot_ly(ICSA, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Initial claims SA', 
         xaxis = list(type = 'date', tickformat = "%m %Y"))

claims_plot2.2 <- plot_ly(ICNSA, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SMA(growth_12m, 4)) %>%
  layout(title = 'Initial claims YoY')

# claims_plot3 <- plot_ly(cc_post_covid, x = ~date, y = ~yoy_vs_pre_covid, type = 'scatter', mode = 'lines') %>%
#   add_trace(y = ~SMA(yoy_vs_pre_covid, 4)) %>%
#   layout(title = 'change in continuing claims NSA vs pre covid (2017-2019)')
# 
# claims_plot4 <- plot_ly(ic_post_covid, x = ~date, y = ~yoy_vs_pre_covid, type = 'scatter', mode = 'lines') %>%
#   add_trace(y = ~SMA(yoy_vs_pre_covid, 4)) %>%
#   layout(title = 'change in initial claims NSA vs pre covid (2017-2019)')


CCNSA_2017 = CCNSA %>%
  filter(date >= "2017-01-01" & date <= "2019-12-31") %>%
  mutate(yr = year(date))

CCSA_2017 = CCSA %>%
  filter(date >= "2017-01-01" & date <= "2019-12-31") %>%
  mutate(yr = year(date))

CCSA_seasonal = CCNSA_2017 %>%
  rename(raw = value) %>%
  left_join(CCSA_2017, by = c("week", "yr")) %>%
  select(raw, value, week) %>%
  mutate(adjustment = value / raw) %>%
  group_by(week) %>%
  summarise(mean_adjustment = mean(adjustment))


CCNSA = CCNSA %>%
  filter(week <= 52) %>%
  left_join(CCSA_seasonal, by = "week") %>%
  mutate(adjusted = value * mean_adjustment) %>%
  mutate(adjusted_4ma = zoo::rollmean(adjusted, k = 4, fill = NA, align = "right")) %>%
  mutate(max_diff = adjusted_4ma + zoo::rollmax(-adjusted_4ma, 78, fill = NA, align = "right")) %>%
  select(date, adjusted, adjusted_4ma, max_diff)
  
continuing_claims_adjusted_plot <- plot_ly(CCNSA, x = ~date, y = ~adjusted, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~adjusted_4ma, name = "4ma") %>%
  layout(title = "CCNSA Adjusted mannually")

CCNSA = CCNSA %>%
  left_join(fred_request_data("CCSA"), by = "date") %>%
  mutate(diff = value - adjusted) 







ICNSA_2017 = ICNSA %>%
  filter(date >= "2017-01-01" & date <= "2019-12-31") %>%
  mutate(yr = year(date))

ICSA_2017 = ICSA %>%
  filter(date >= "2017-01-01" & date <= "2019-12-31") %>%
  mutate(yr = year(date))

ICSA_seasonal = ICNSA_2017 %>%
  rename(raw = value) %>%
  left_join(ICSA_2017, by = c("week", "yr")) %>%
  select(raw, value, week) %>%
  mutate(adjustment = value / raw) %>%
  group_by(week) %>%
  summarise(mean_adjustment = mean(adjustment))


ICNSA = ICNSA %>%
  filter(week <= 52) %>%
  left_join(ICSA_seasonal, by = "week") %>%
  mutate(adjusted = value * mean_adjustment) %>%
  mutate(adjusted_4ma = zoo::rollmean(adjusted, k = 4, fill = NA, align = "right")) %>%
  mutate(max_diff = adjusted_4ma + zoo::rollmax(-adjusted_4ma, 78, fill = NA, align = "right")) %>%
  select(date, adjusted, max_diff) %>%
  left_join(fred_request_data("ICSA"), by = 'date') %>%
  mutate(diff = value - adjusted)

initial_claims_adjusted_plot <- plot_ly(ICNSA, x = ~date, y = ~adjusted, type = 'scatter', mode = 'lines') %>%
  layout(title = "ICNSA Adjusted mannually")

plot_ly(ICNSA, x = ~date, y = ~SMA(adjusted, 4), type = 'scatter', mode = 'lines')


  



