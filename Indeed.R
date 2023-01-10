library(ggplot2)
library(plotly)
library(dplyr)

indeed_data = read.csv("data/Indeed/aggregate_job_postings_US.csv") %>%
  mutate(date = as.Date(date))

total_postings = indeed_data %>%
  filter(variable == "total postings")

new_postings = indeed_data %>%
  filter(variable == "new postings") %>%
  mutate(rolling = rollmean(indeed_job_postings_index_SA, 20, fill= "NA", align = "right"))

plot1 <- plot_ly(total_postings, x = ~date, y = ~indeed_job_postings_index_SA, type = 'scatter', mode = 'lines') %>%
  layout(title = 'total job postings')

plot2 <- plot_ly(new_postings, x = ~date, y = ~rolling, type = 'scatter', mode = 'lines') %>%
  layout(title = 'new job postings')

plot1
plot2


