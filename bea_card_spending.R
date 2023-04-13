library(TTR)
library(matrixStats)

card_spending = read.csv("data/bea_card_spending.csv") %>%
  select(week_start_date, industry, median) %>%
  mutate(week_start_date = as.Date(week_start_date, format = "%m/%d/%Y"))

card_spending = reshape(card_spending, idvar = "week_start_date", timevar = "industry", direction = "wide")

cyclical_card_spending = card_spending[-c(8,14,18,19,20)] %>%
  mutate_at(vars(-week_start_date), scale) %>%
  mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T)) %>%
  rowwise() %>% 
  mutate(median.z = median(c_across(2:15), na.rm = TRUE))

plot1.1 <- plot_ly(cyclical_card_spending, x = ~week_start_date, y = ~mean.z, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SMA(mean.z, 4)) %>%
  layout(title = 'Mean discretionary Card spending')

plot1.2 <- plot_ly(cyclical_card_spending, x = ~week_start_date, y = ~median.z, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SMA(median.z, 4)) %>%
  layout(title = 'Median Discretionary Card spending')

plot2 <- plot_ly(card_spending, x = ~week_start_date, y = ~`median.Total retail and food service`, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SMA(`median.Total retail and food service`, 4)) %>%
  layout(title = 'Total Card spending')

plot1.1
plot1.2

plot2