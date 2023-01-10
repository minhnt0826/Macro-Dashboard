# library(fredr)
# 
# fredr_set_key("e86c15cf4098a7176f6edd1f9ef45600")
# 
# CCNSA_org = fredr(
#   series_id = "CCNSA",
#   observation_start = as.Date("2015-01-01"),
#   observation_end = as.Date("2023-02-01")
# )

# ICNSA_org = fredr(
#   series_id = "ICNSA",
#   observation_start = as.Date("2015-01-01"),
#   observation_end = as.Date("2023-02-01")
# )


library(dplyr)
library(ggplot2)
library(plotly)

CCNSA = CCNSA_org

CCNSA$week = 1

week_count = 2
for(i in 2:nrow(CCNSA))
{
  if (year(CCNSA$date[i]) > year(CCNSA$date[i-1]))
  {
    week_count = 1
  }
  
  CCNSA$week[i] <- week_count
  
  week_count = week_count + 1
}

ICNSA = ICNSA_org

ICNSA$week = 1

week_count = 2
for(i in 2:nrow(ICNSA))
{
  if (year(ICNSA$date[i]) > year(ICNSA$date[i-1]))
  {
    week_count = 1
  }
  
  ICNSA$week[i] <- week_count
  
  week_count = week_count + 1
}



cc_pre_covid  = CCNSA %>% 
  filter(date <= '2019-12-31') %>% 
  mutate(year = year(date)) %>%
  select(year, value, week) %>% 
  group_by(week) %>% summarise(avg_claims = mean(value)) %>%
  filter(week <= 52)

cc_post_covid  = CCNSA %>% 
  filter(date >= '2022-01-01') %>% 
  mutate(year = year(date)) %>%
  select(date, week, value) %>% 
  left_join(cc_pre_covid, by = 'week') %>% 
  select(date, week, value, avg_claims) %>%
  mutate(yoy_vs_pre_covid = value / avg_claims) %>%
  mutate(yoy_mean_4m = rollmean(yoy_vs_pre_covid, 4, fill = NA, align = 'right'))

ic_pre_covid  = ICNSA %>% 
  filter(date <= '2019-12-31') %>% 
  mutate(year = year(date)) %>%
  select(year, value, week) %>% 
  group_by(week) %>% summarise(avg_claims = mean(value)) %>%
  filter(week <= 52)

ic_post_covid  = ICNSA %>% 
  filter(date >= '2022-01-01') %>% 
  mutate(year = year(date)) %>%
  select(date, week, value) %>% 
  left_join(ic_pre_covid, by = 'week') %>% 
  select(date, week, value, avg_claims) %>%
  mutate(yoy_vs_pre_covid = value / avg_claims) %>%
  mutate(yoy_mean_4m = rollmean(yoy_vs_pre_covid, 4, fill = NA, align = 'right'))


plot1 <- plot_ly(cc_post_covid, x = ~date, y = ~yoy_mean_4m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'change in continuing claims vs pre covid (2015-2019)')

plot2 <- plot_ly(ic_post_covid, x = ~date, y = ~yoy_mean_4m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'change in initial claims vs pre covid (2015-2019)')

plot1
plot2




  
