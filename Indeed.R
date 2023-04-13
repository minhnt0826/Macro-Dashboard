
library(plotly)
library(dplyr)
source("helper.R")

# indeed_data = read.csv("data/Indeed/aggregate_job_postings_US.csv") %>%
#   mutate(date = as.Date(date))
# 
# total_postings = indeed_data %>%
#   filter(variable == "total postings")
# 
# new_postings = indeed_data %>%
#   filter(variable == "new postings")
# 
# plot1 <- plot_ly(total_postings, x = ~date, y = ~indeed_job_postings_index_SA, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'total job postings')
# 
# plot2 <- plot_ly(new_postings, x = ~date, y = ~indeed_job_postings_index_SA, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'new job postings')

# sector_data = read.csv("https://raw.githubusercontent.com/hiring-lab/job_postings_tracker/master/US/job_postings_by_sector_US.csv") %>%
#   mutate(date = as.Date(date))
# 
# construction = sector_data %>%
#   filter(display_name == "Construction") %>%
#   mutate(growth_12m = indeed_job_postings_index / lag(indeed_job_postings_index, 52) )
# 
# 
# manufacturing = sector_data %>%
#   filter(display_name == "Production & Manufacturing")
# 
# food_service = sector_data %>%
#   filter(display_name == "Food Preparation & Service")
# 
# hospitality = sector_data %>%
#   filter(display_name == "Hospitality & Tourism") %>%
#   mutate(growth_12m = indeed_job_postings_index / lag(indeed_job_postings_index, 52) )
# 
# 
# plot3 <- plot_ly(construction, x = ~date, y = ~indeed_job_postings_index, type = 'scatter', mode = 'lines') %>%
#   add_trace(y = ~growth_12m) %>%
#   layout(title = 'Construction job postings')
# 
# plot4 <- plot_ly(manufacturing, x = ~date, y = ~indeed_job_postings_index, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'Manufacturing job postings')
# 
# plot5 <- plot_ly(food_service, x = ~date, y = ~indeed_job_postings_index, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'Food service job postings')
# 
# plot6 <- plot_ly(hospitality, x = ~date, y = ~indeed_job_postings_index, type = 'scatter', mode = 'lines') %>%
#   add_trace(y = ~growth_12m) %>%
#   layout(title = 'Hospitality job postings')
# 
# plot3
# plot4
# plot5
# plot6

indeed_data = read.csv("https://raw.githubusercontent.com/hiring-lab/job_postings_tracker/master/US/aggregate_job_postings_US.csv") %>%
  mutate(us_jobs_index = indeed_job_postings_index_SA) %>%
  select(date, us_jobs_index, variable) %>%
  left_join(read.csv("https://raw.githubusercontent.com/hiring-lab/job_postings_tracker/master/DE/aggregate_job_postings_DE.csv"), by = c("date","variable"))  %>%
  mutate(de_jobs_index = indeed_job_postings_index_SA) %>%
  select(date, us_jobs_index, de_jobs_index, variable) %>%
  mutate(date = as.Date(date)) 


total_postings = indeed_data %>%
  filter(variable == "total postings") %>%
  mutate(growth_12m = growth_over_lag(us_jobs_index, 365))

new_postings = indeed_data %>%
  filter(variable == "new postings") %>%
  mutate(growth_12m = growth_over_lag(us_jobs_index, 365))
# mutate(rolling = rollmean(indeed_job_postings_index_SA, 20, fill= "NA", align = "right"))

indeed_plot1.1 <- plot_ly(total_postings, x = ~date, y = ~us_jobs_index, type = 'scatter', mode = 'lines') %>%
  layout(title = 'total job postings US')

indeed_plot1.2 <- plot_ly(total_postings, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'total job postings US YoY')

indeed_plot2.1 <- plot_ly(new_postings, x = ~date, y = ~us_jobs_index, type = 'scatter', mode = 'lines') %>%
  layout(title = 'new job postings US')

indeed_plot2.2 <- plot_ly(new_postings, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'new job postings US YoY')


indeed_hospitality = fred_request_data("IHLIDXUSTPHOTO") %>%
  mutate(growth_12m = growth_over_lag(value, 352))

indeed_food_service = fred_request_data("IHLIDXUSTPFOPRSE")%>%
  mutate(growth_12m = growth_over_lag(value, 352))

indeed_retail_service = fred_request_data("IHLIDXUSTPRETA")%>%
  mutate(growth_12m = growth_over_lag(value, 352))

indeed_architecture = fred_request_data("IHLIDXUSTPARCH")%>%
  mutate(growth_12m = growth_over_lag(value, 352))

indeed_construction = fred_request_data("IHLIDXUSTPCONS")%>%
  mutate(growth_12m = growth_over_lag(value, 352))

indeed_civil = fred_request_data("IHLIDXUSTPCIVIENGI")%>%
  mutate(growth_12m = growth_over_lag(value, 352))

indeed_manufacturing = fred_request_data("IHLIDXUSTPPRMA")%>%
  mutate(growth_12m = growth_over_lag(value, 352))


# Leisure and hospitality #### 
indeed_plot3.1 = plot_ly(indeed_hospitality, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Hospitality job postings US')

indeed_plot3.2 = plot_ly(indeed_hospitality, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Hospitality job postings US YoY')

indeed_plot4.1 = plot_ly(indeed_food_service, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Food service job postings US')

indeed_plot4.2 = plot_ly(indeed_food_service, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Food service job postings US YoY')

indeed_plot5.1 = plot_ly(indeed_retail_service, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Retail service job postings US')

indeed_plot5.2 = plot_ly(indeed_retail_service, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Retail service job postings US YoY')

# Construction #### 
indeed_plot6.1 = plot_ly(indeed_architecture, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Architecture job postings US')

indeed_plot6.2 = plot_ly(indeed_architecture, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Architecture job postings US YoY')

indeed_plot7.1 = plot_ly(indeed_civil, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Civil job postings US')

indeed_plot7.2 = plot_ly(indeed_civil, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Civil job postings US YoY')

indeed_plot8.1 = plot_ly(indeed_construction, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Construction job postings US')

indeed_plot8.2 = plot_ly(indeed_construction, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Construction job postings US YoY')

# Manufacturing ####
indeed_plot9.1 = plot_ly(indeed_manufacturing, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Manufacturing job postings US')

indeed_plot9.2 = plot_ly(indeed_manufacturing, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Manufacturing job postings US YoY')






