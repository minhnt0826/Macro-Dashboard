library(dplyr)
library(lubridate)
library(TTR)
source("fred.R")
source("helper.R")


# source("global.R")


# NFP Data ####
aggregate_payrolls = fred_request_data("CES0500000017") %>%
  create_growth_data_for_df(., . $value)

aggregate_payrolls_production = fred_request_data("CES0500000035") %>%
  create_growth_data_for_df(., . $value)

aggregate_weekly_hours = fred_request_data("AWHAE") %>%
  create_growth_data_for_df(., . $value)

aggregate_weekly_hours_production = fred_request_data("AWHI") %>%
  create_growth_data_for_df(., . $value)

temp_help_emp = fred_request_data("TEMPHELPS")
overtime_hours_man = fred_request_data("CES3000000004")
partime_emp_econ_reasons = fred_request_data("LNS12032194")
partime_emp = fred_request_data("LNS12600000")

nonfarm_payrolls = fred_request_data("PAYEMS")
weekly_hours = fred_request_data("AWHNONAG")

employment_level = fred_request_data("CE16OV")

avg_hourly_earnings = fred_request_data("CES0500000003") %>%
  create_growth_data_for_df(., . $value)

avg_hourly_earnings_production = fred_request_data("AHETPI") %>%
  create_growth_data_for_df(., .$value)




employment_level = employment_level %>%
  mutate(growth_3m = n_month_growth_ann(value , 3),
         growth_6m = n_month_growth_ann(value , 6),
         growth_12m = n_month_growth_ann(value , 12))

weekly_hours = weekly_hours %>%
  mutate(growth_3m = n_month_growth_ann(value , 3),
         growth_6m = n_month_growth_ann(value , 6))


temp_help_emp = temp_help_emp %>%
  left_join(nonfarm_payrolls, by = "date") %>%
  mutate(value = value.x / value.y) %>%
  mutate(growth_3m = n_month_growth_ann(value , 3),
         growth_6m = n_month_growth_ann(value , 6),
         growth_12m = n_month_growth_ann(value , 12))


partime_emp_econ_reasons = partime_emp_econ_reasons %>%
  left_join(partime_emp, by = "date") %>%
  mutate(value = value.x / value.y) %>%
  mutate(growth_3m = n_month_growth_ann(value , 3),
         growth_6m = n_month_growth_ann(value , 6),
         growth_12m = n_month_growth_ann(value , 12))


aggregate_payrolls_plot <- create_plotly_plot_with_growth_data(aggregate_payrolls, growth_1m = T) %>%
  layout(title = 'Aggregate weekly payrolls')

aggregate_payrolls_production_plot <- create_plotly_plot_with_growth_data(aggregate_payrolls_production, growth_1m = T) %>%
  layout(title = 'Aggregate weekly payrolls production and nonsupervisory employees')


aggregate_weekly_hours_plot <- create_plotly_plot_with_growth_data(aggregate_weekly_hours, growth_1m = T) %>%
  layout(title = 'Aggregate weekly hours')

aggregate_weekly_hours_production_plot <- create_plotly_plot_with_growth_data(aggregate_weekly_hours_production, growth_1m = T) %>%
  layout(title = 'Aggregate weekly hours production and nonsupervisory employees')


avg_hourly_earnings_plot = create_plotly_plot_with_growth_data(avg_hourly_earnings) %>%
  layout(title = "avg hourly earnings annualized")

avg_hourly_earnings_production_plot = create_plotly_plot_with_growth_data(avg_hourly_earnings_production) %>%
  layout(title = "avg hourly earnings production & nonsupervisory annualized")


employment_level_plot = create_plotly_plot_with_growth_data(employment_level) %>%
  layout(title = "Employment level")

temp_help_plot1 <- plot_ly(temp_help_emp, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'temporary help services employment / total employment')

temp_help_plot2 <- plot_ly(temp_help_emp, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'temporary help services employment / total employment 3m annualized')

temp_help_plot3 <- plot_ly(temp_help_emp, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'temporary help services employment / total employment 6m annualized')

temp_help_plot4 <- plot_ly(temp_help_emp, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'temporary help services employment / total employment 12m annualized')

nfp_plot3.1 <- plot_ly(weekly_hours, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'weekly hours')

nfp_plot3.2 <- plot_ly(weekly_hours, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'weekly hours 3m annualized')

nfp_plot4 <- plot_ly(overtime_hours_man, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'overtime hours (manufacturing)')

nfp_plot5.1 <- plot_ly(partime_emp_econ_reasons, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'part time employment for economic reasons ratio')

nfp_plot5.2 <- plot_ly(partime_emp_econ_reasons, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  add_trace(x = ~date, y = ~SMA(SMA(growth_3m, 3),3)) %>%
  layout(title = 'part time employment for economic reasons ratio 3m annualized')

nfp_plot5.3 <- plot_ly(partime_emp_econ_reasons, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
  add_trace(x = ~date, y = ~SMA(growth_6m, 3)) %>%
  layout(title = 'part time employment for economic reasons ratio 6m annualized')

nfp_plot5.4 <- plot_ly(partime_emp_econ_reasons, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'part time employment for economic reasons ratio 12m annualized')

job_losers_permanent_plot = fred_request_data("LNS13026638") %>%
  plot_ly(.,x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  add_trace(x = ~date, y = ~ SMA(value, 3), name = "3ma") %>%
  layout(title = 'Job losers permanent')

job_losers_permanent_growth_plot = fred_request_data("LNS13026638") %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(.) %>%
  layout(title = 'Job losers permanent growth')

job_losers_permanent_plot_test = fred_request_data("LNS13026638") %>%
  mutate(value = SMA(value, 2)) %>%
  mutate(off_lows = value + rollmax(x = -value, k = 12, fill = NA, align = "right")) %>%
  plot_ly(., x = ~date, y=~off_lows, mode = "lines")

job_losers_permanent_plot_test_2 = fred_request_data("LNS13026638") %>%
  mutate(value = SMA(value, 2)) %>%
  mutate(off_lows = value + rollmax(x = -value, k = 12, fill = NA, align = "right")) %>%
  mutate(off_lows_chng = off_lows / lag(off_lows,  3)) %>%
  plot_ly(., x = ~date, y=~off_lows_chng, mode = "lines")
  
  
residential_emp_plot = fred_request_data("CES2023610001") %>%
  left_join(fred_request_data("CES2023800101"), by = "date") %>%
  mutate(value = value.x + value.y) %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(.) %>%
  layout(title = "residential employment")

truck_emp_plot = fred_request_data("CES4348400001") %>%
  create_growth_data_for_df(., .$value) %>%
  create_plotly_plot_with_growth_data(.) %>%
  layout(title = "trucking employment")

# wholesale_durable_emp_plot = fred_request_data("CES4142300001") %>%
#   create_growth_data_for_df(., .$value) %>%
#   create_plotly_plot_with_growth_data(.) %>%
#   layout(title = "Wholesale durable employment")
# 
# ratio_emp_plot = fred_request_data("CES4300000001") %>%
#   left_join(fred_request_data("PAYEMS"), by = "date") %>%
#   mutate(value = value.x / value.y) %>%
#   select(date, value) %>%
#   create_growth_data_for_df(., .$value) %>%
#   create_plotly_plot_with_growth_data(.)
# 
# normal_emp_plot = fred_request_data("SRVPRD") %>%
#   create_growth_data_for_df(., .$value) %>%
#   create_plotly_plot_with_growth_data(.)
# 
# resi_specialty_durable_emp_plot = fred_request_data("CES2023800101") %>%
#   left_join(fred_request_data("PAYEMS"), by = "date") %>%
#   mutate(value = value.x / value.y) %>%
#   select(date, value) %>%
#   create_growth_data_for_df(., .$value) %>%
#   create_plotly_plot_with_growth_data(.) %>%
#   layout(title = "Residential Specialty Trade Contractors employment")
# 
# resi_specialty_durable_emp_plot
# 
# payrolls_plot = fred_request_data("PAYEMS") %>%
#   create_growth_data_for_df(., .$value) %>%
#   create_plotly_plot_with_growth_data(.) %>%
#   layout(title = "total employment")

# Indeed data ####
source("indeed.R")

# indeed_plot3 <- plot_ly(total_postings, x = ~date, y = ~de_jobs_index, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'total job postings DE')
# 
# indeed_plot4 <- plot_ly(new_postings, x = ~date, y = ~de_jobs_index, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'new job postings DE')

# Claims data ####
source("claims.R")

# ADP ####
adp_weekly_m_avg = fred_request_data("ADPWNUSNERSA", freq = "m", agg ="avg") %>%
  create_growth_data_for_df(., .$value)

plot1 = create_plotly_plot_with_growth_data(adp_weekly_m_avg)
plot1

# States unemployment ####
change_over_local_low <- function(x, n, na.rm = TRUE) {
  return (x + rollmax(-x, n, align = "right", fill = NA))
}

states_unemployment_data = read_excel("data/States unemployment data/States_unemployment_rate.xls", sheet = 2)
states_unemployment_data = states_unemployment_data %>%
  mutate_at(vars(-DATE), ~change_over_local_low(., 18)) %>%
  mutate(sahm_triggered = rowSums(. >= 0.5) - 1)

states_unemployment_plot <- plot_ly(states_unemployment_data, x = ~DATE, y = ~sahm_triggered, type = 'scatter', mode = 'lines') %>%
  layout(title = "Number of states with Sahm rule triggered")


# KC labor ####

kansas_labor_activity_plot = fred_request_data("FRBKCLMCILA") %>%
  mutate(chng_6m = value - lag(value, 6)) %>%
  plot_ly(., x=~date, y=~chng_6m, mode = "lines")
kansas_labor_activity_plot

# Labor activity
source("serverModules/employment/labor_indicator.R")

# Unemployment indicators
source("serverModules/employment/unemployment_indicators.R")

# Shiny server ####
employmentServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$aggregate_payrolls_plot <- renderPlotly({
        aggregate_payrolls_plot
      })
      
      output$aggregate_payrolls_production_plot <- renderPlotly({
        aggregate_payrolls_production_plot
      })
      
      output$aggregate_weekly_hours_plot <- renderPlotly({
        aggregate_weekly_hours_plot
      })
      
      output$aggregate_weekly_hours_production_plot <- renderPlotly({
        aggregate_weekly_hours_production_plot
      })
      
      
      output$temp_help_plot1 <- renderPlotly({
        temp_help_plot1
      })
      
      output$temp_help_plot2 <- renderPlotly({
        temp_help_plot2
      })
      
      output$temp_help_plot3 <- renderPlotly({
        temp_help_plot3
      })
      
      output$temp_help_plot4 <- renderPlotly({
        temp_help_plot4
      })
      
      output$avg_hourly_earnings_plot <- renderPlotly({
        avg_hourly_earnings_plot
      })
      
      output$avg_hourly_earnings_production_plot <- renderPlotly({
        avg_hourly_earnings_production_plot
      })
      
      output$employment_level_plot <- renderPlotly({
        employment_level_plot
      })
      
      
      output$nfp_plot3.1 <- renderPlotly({
        nfp_plot3.1
      })
      
      output$nfp_plot3.2 <- renderPlotly({
        nfp_plot3.2
      })
      
      output$nfp_plot4 <- renderPlotly({
        nfp_plot4
      })
      
      output$nfp_plot5.1 <- renderPlotly({
        nfp_plot5.1
      })
      
      output$nfp_plot5.2 <- renderPlotly({
        nfp_plot5.2
      })
      
      output$nfp_plot5.3 <- renderPlotly({
        nfp_plot5.3
      })
      
      output$nfp_plot5.4 <- renderPlotly({
        nfp_plot5.4
      })
      
      output$truck_emp_plot <- renderPlotly({
        truck_emp_plot
      })
      
      output$residential_emp_plot <- renderPlotly({
        residential_emp_plot
      })
      
      output$job_losers_permanent_plot <- renderPlotly({
        job_losers_permanent_plot
      })
      
      output$job_losers_permanent_growth_plot <- renderPlotly({
        job_losers_permanent_growth_plot
      })
      
      output$indeed_plot2 <- renderPlotly({
        indeed_plot2
      })
      
      output$indeed_plot1.1 <- renderPlotly({
        indeed_plot1.1
      })
      
      output$indeed_plot1.2 <- renderPlotly({
        indeed_plot1.2
      })
      
      output$indeed_plot2.1 <- renderPlotly({
        indeed_plot2.1
      })
      
      output$indeed_plot2.2 <- renderPlotly({
        indeed_plot2.2
      })
      
      output$indeed_plot3.1 <- renderPlotly({
        indeed_plot3.1
      })
      
      output$indeed_plot3.2 <- renderPlotly({
        indeed_plot3.2
      })
      
      output$indeed_plot4.1 <- renderPlotly({
        indeed_plot4.1
      })
      
      output$indeed_plot4.2 <- renderPlotly({
        indeed_plot4.2
      })
      
      output$indeed_plot5.1 <- renderPlotly({
        indeed_plot5.1
      })
      
      output$indeed_plot5.2 <- renderPlotly({
        indeed_plot5.2
      })
      
      output$indeed_plot6.1 <- renderPlotly({
        indeed_plot6.1
      })
      
      output$indeed_plot6.2 <- renderPlotly({
        indeed_plot6.2
      })
      
      output$indeed_plot7.1 <- renderPlotly({
        indeed_plot7.1
      })
      
      output$indeed_plot7.2 <- renderPlotly({
        indeed_plot7.2
      })
      
      output$indeed_plot8.1 <- renderPlotly({
        indeed_plot8.1
      })
      
      output$indeed_plot8.2 <- renderPlotly({
        indeed_plot8.2
      })
      
      output$indeed_plot9.1 <- renderPlotly({
        indeed_plot9.1
      })
      
      output$indeed_plot9.2 <- renderPlotly({
        indeed_plot9.2
      })
      
      output$claims_plot1.1 <- renderPlotly({
        claims_plot1.1
      })
      
      output$claims_plot1.2 <- renderPlotly({
        claims_plot1.2
      })      
      
      output$claims_plot1.3 <- renderPlotly({
        claims_plot1.3
      })
      
      output$claims_plot2.1 <- renderPlotly({
        claims_plot2.1
      })
      
      output$claims_plot2.2 <- renderPlotly({
        claims_plot2.2
      })
      
      output$cc_claims_vs_prev_years_plot <- renderPlotly({
        cc_claims_vs_prev_years_plot
      })
      
      output$ic_claims_vs_prev_years_plot <- renderPlotly({
        ic_claims_vs_prev_years_plot
      })
      
      
      output$continuing_claims_adjusted_plot <- renderPlotly({
        continuing_claims_adjusted_plot
      })
      
      output$initial_claims_adjusted_plot <- renderPlotly({
        initial_claims_adjusted_plot
      })
      
      output$continuing_claims_breath_plot <- renderPlotly({
        continuing_claims_breath_plot
      })
      
      output$initial_claims_breath_plot <- renderPlotly({
        initial_claims_breath_plot
      })
      
      output$initial_claims_breath_plot <- renderPlotly({
        initial_claims_breath_plot
      })
      
      output$permanent_unrate_ma3_ma12_plot <- renderPlotly({
        permanent_unrate_ma3_ma12_plot
      })
      
      output$unemployment_rate_u2_ma3_ma12_plot <- renderPlotly({
        unemployment_rate_u2_ma3_ma12_plot
      })
      
      output$unemployment_rate_ma3_ma12_plot <- renderPlotly({
        unemployment_rate_ma3_ma12_plot
      })
      
      output$insured_unemployment_rate_yoy_plot <- renderPlotly({
        insured_unemployment_rate_yoy_plot
      })
    }
  )
}