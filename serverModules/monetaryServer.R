source("fred.R")
source("serverModules/growthServer.R")
source("helper.R")
# source("pce_median.R")

library(plotly)



# Credit ####
m2 = fred_request_data("M2SL")
currency = fred_request_data("CURRSL")
money_market_funds = fred_request_data("RMFSL")


other_deposits_liabilites = m2 %>%
  rename(m2 = value) %>%
  inner_join(currency, by = "date") %>%
  rename(currency = value) %>%
  inner_join(money_market_funds, by = "date") %>%
  rename(money_market_funds = value) %>%
  arrange(date) %>%
  mutate(odl = m2 - currency - money_market_funds) %>%
  mutate(growth_3m = n_month_growth_ann(odl, 3),
         growth_6m = n_month_growth_ann(odl, 6)) %>%
  mutate(growth_12m = n_month_growth_ann(odl, 12))

credit_plot1.1 = plot_ly(data = other_deposits_liabilites, x = ~date, y = ~odl, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Other deposits liabilities',
         yaxis = list(type = "log"))

credit_plot1.2 = plot_ly(data = other_deposits_liabilites, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'ODL 3m annualized')

credit_plot1.3 = plot_ly(data = other_deposits_liabilites, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'ODL 6m annualized')

credit_plot1.4 = plot_ly(data = other_deposits_liabilites, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'ODL yoy')

real_odl = fred_request_data("ODSACBM027SBOG") %>%
  rename(odl = value) %>%
  left_join(fred_request_data("CUSR0000SA0L2"), by = "date") %>%
  rename(cpi = value) %>%
  mutate(value = odl / cpi) %>%
  mutate(growth_3m = n_month_growth_ann(value, 3),
         growth_6m = n_month_growth_ann(value, 6),
         growth_12m = n_month_growth_ann(value, 12))

real_odl_plot1 =  plot_ly(real_odl, x = ~date, y = ~value, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Real odl (Other deposits at commercial banks deflated by CPI)')

real_odl_plot2 = plot_ly(real_odl, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines', name = "3m") %>%
  add_trace(y = ~growth_6m, name = "6m") %>%
  add_trace(y = ~growth_12m, name = "12m") %>%
  layout(title = "Real odl growth")

bank_credit = fred_request_data("H8B1001NCBCMG") %>%
  rename(change_annualized = value) %>%
  mutate(change = ((100 + change_annualized) / 100)^(1/12)) %>%
  mutate(value = 100 * cumprod(change)) %>%
  mutate(growth_3m = n_month_growth_ann(value, 3),
         growth_6m = n_month_growth_ann(value, 6),
         growth_12m = n_month_growth_ann(value, 12))

loan_leases = fred_request_data("H8B1020NCBCMG") %>%
  rename(change_annualized = value) %>%
  mutate(change = ((100 + change_annualized) / 100)^(1/12)) %>%
  mutate(value = 100 * cumprod(change)) %>%
  mutate(growth_3m = n_month_growth_ann(value, 3),
         growth_6m = n_month_growth_ann(value, 6),
         growth_12m = n_month_growth_ann(value, 12))

ci_loans = fred_request_data("H8B1023NCBCMG") %>%
  rename(change_annualized = value) %>%
  mutate(change = ((100 + change_annualized) / 100)^(1/12)) %>%
  mutate(value = 100 * cumprod(change)) %>%
  mutate(growth_3m = n_month_growth_ann(value, 3),
         growth_6m = n_month_growth_ann(value, 6),
         growth_12m = n_month_growth_ann(value, 12))

real_estate_loans = fred_request_data("H8B1026NCBCMG") %>%
  rename(change_annualized = value) %>%
  mutate(change = ((100 + change_annualized) / 100)^(1/12)) %>%
  mutate(value = 100 * cumprod(change)) %>%
  mutate(growth_3m = n_month_growth_ann(value, 3),
         growth_6m = n_month_growth_ann(value, 6),
         growth_12m = n_month_growth_ann(value, 12))

consumer_loans = fred_request_data("H8B1029NCBCMG") %>%
  rename(change_annualized = value) %>%
  mutate(change = ((100 + change_annualized) / 100)^(1/12)) %>%
  mutate(value = 100 * cumprod(change)) %>%
  mutate(growth_3m = n_month_growth_ann(value, 3),
         growth_6m = n_month_growth_ann(value, 6),
         growth_12m = n_month_growth_ann(value, 12),
         growth_5y = n_month_growth_ann(value, 60),
         growth_10y = n_month_growth_ann(value, 120))


bank_credit_plot1.1 = plot_ly(data = bank_credit, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Bank credit 3m annualized')

bank_credit_plot1.2 = plot_ly(data = bank_credit, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Bank credit 6m annualized')

bank_credit_plot1.3 = plot_ly(data = bank_credit, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Bank credit 12m annualized')

loan_leases_plot1.1 = plot_ly(data = loan_leases, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Loan and leases 3m annualized')

loan_leases_plot1.2 = plot_ly(data = loan_leases, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Loan and leases 6m annualized')

loan_leases_plot1.3 = plot_ly(data = loan_leases, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Loan and leases 12m annualized')

ci_loans_plot1.1 = plot_ly(data = ci_loans, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Commercial & industrial loans 3m annualized')

ci_loans_plot1.2 = plot_ly(data = ci_loans, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Commercial & industrial loans 6m annualized')

ci_loans_plot1.3 = plot_ly(data = ci_loans, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Commercial & industrial loans 12m annualized')

real_estate_plot1 = plot_ly(data = real_estate_loans, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Real estate loans 3m annualized')

real_estate_plot2 = plot_ly(data = real_estate_loans, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Real estate loans 6m annualized')

real_estate_plot3 = plot_ly(data = real_estate_loans, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Real estate loans 12m annualized')

consumer_loans_plot1.1 = plot_ly(data = consumer_loans, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Consumer loans 3m annualized')

consumer_loans_plot1.2 = plot_ly(data = consumer_loans, x = ~date, y = ~growth_6m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Consumer loans 6m annualized')

consumer_loans_plot1.3 = plot_ly(data = consumer_loans, x = ~date, y = ~growth_12m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Consumer loans 12m annualized')

consumer_loans_plot1.4 = plot_ly(data = consumer_loans, x = ~date, y = ~growth_10y, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Consumer loans 10 year annualized')


# Monpol ####

fed_funds_rate = fred_request_data("FEDFUNDS")
bond_2y_yield = fred_request_data("GS2")
Baa_yield = fred_request_data("BAA")

monpol_restrictive = fed_funds_rate %>%
  select(date, value) %>%
  rename(fed_funds_rate = value) %>%
  left_join(Baa_yield, by = "date") %>%
  rename(baa_yield = value) %>%
  left_join(bond_2y_yield, by = "date") %>%
  rename(bond_2y_yield = value) %>%
  left_join(nominal_growth_index, by = "date" ) %>%
  rename(nominal_growth_3m = growth_3m,
         nominal_growth_6m = growth_6m,
         nominal_growth_12m = growth_12m)

fed_funds_less_inflation = fed_funds_rate %>%
  rename(fed_funds_rate = value) %>%
  left_join(all_items_ex_shelter_cpi, by = "date") %>%
  fill(c("growth_3m","growth_6m","growth_12m"), .direction = "down")

fed_funds_plot1 = plot_ly(data = monpol_restrictive, x = ~date, y = ~fed_funds_rate - nominal_growth_3m, type = 'scatter', mode = 'line' ,name = "3m") %>%
                    add_trace(y = ~fed_funds_rate - nominal_growth_6m, name = '6m') %>%
                    add_trace(y = ~fed_funds_rate - nominal_growth_12m, name = '12m') %>%
                    layout(title = "FFR less annualized nominal growth" )
  
fed_funds_plot2 = plot_ly(data = fed_funds_less_inflation, x = ~date, y = ~fed_funds_rate - growth_3m, type = 'scatter', mode = 'line' ,name = "3m") %>%
  add_trace(y = ~fed_funds_rate - growth_6m, name = '6m') %>%
  add_trace(y = ~fed_funds_rate - growth_12m, name = '12m') %>%
  layout(title = "FFR less annualized run rate inflation" )

bond_2y_yield_less_growth_plot1.1 = plot_ly(data = monpol_restrictive, x = ~date, y = ~bond_2y_yield - nominal_growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Bond 2 year yield less 3m annualized nominal growth')

bond_2y_yield_less_growth_plot1.2 = plot_ly(data = monpol_restrictive, x = ~date, y = ~bond_2y_yield - nominal_growth_6m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Bond 2 year yield less 6m annualized nominal growth')

baa_yield_less_growth_plot1.1 = plot_ly(data = monpol_restrictive, x = ~date, y = ~baa_yield - nominal_growth_3m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Baa corporate yield less 3m annualized nominal growth')

baa_yield_less_growth_plot1.2 = plot_ly(data = monpol_restrictive, x = ~date, y = ~baa_yield - nominal_growth_6m, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Baa corporate yield less 6m annualized nominal growth')


diff_rate_to_5y5y = fred_request_data("DGS5") %>%
  left_join(fred_request_data("DGS10"), by = "date") %>%
  mutate(cum_10 = (1 + (value.y/100)) ^ 10,
         cum_5 = (1 + (value.x/100)) ^ 5) %>%
  mutate(value = (cum_10/cum_5) ^ (1/5)) %>%
  mutate(v5y5y = (value - 1) * 100) %>%
  select(date, v5y5y) %>%
  left_join(fred_request_data("DGS3MO"), by = "date") %>%
  rename(v3m = value) %>%
  left_join(fred_request_data("DGS1"), by = "date") %>%
  rename(v1y = value) %>%
  mutate(tightness_3m = v5y5y - v3m,
         tightness_1y = v5y5y - v1y)

diff_3m_to_5y5y_plot = plot_ly(diff_rate_to_5y5y, x = ~date, y = ~tightness_3m, type = "scatter", mode = "lines", connectgaps = TRUE) %>%
  layout(title = "5y5y forward minus 3 month tbills rate")

diff_1y_to_5y5y_plot = plot_ly(diff_rate_to_5y5y, x = ~date, y = ~tightness_1y, type = "scatter", mode = "lines", connectgaps = TRUE) %>%
  layout(title = "5y5y forward minus 1 year treasury rate")
  
# consumer_credit = fred_request_data("TOTALSL")
# consumer_credit_plot = create_plotly_plot_with_series(consumer_credit, consumer_credit$value)

# gov_debt = read.csv("/Users/nguyenthanhminh/Downloads/MSPD_SumSecty_20010131_20221231.csv")
# gov_debt = gov_debt %>%
#   dplyr::filter(Security.Type.Description == "Total Public Debt Outstanding") %>%
#   select(Record.Date, Total.Public.Debt.Outstanding..in.Millions.) %>%
#   rename(date = Record.Date,
#          debt = Total.Public.Debt.Outstanding..in.Millions.) %>%
#   arrange(date) %>%
#   mutate(yoy = debt / lag(debt, n = 12, fill = NA)) %>%
#   mutate(date = as.Date(date)) %>%
#   mutate(shifted_date = date %m+% months(12))
# 
# 
# credit_plot2 <- plot_ly(gov_debt, x = ~date, y = ~yoy, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'gov_debt')

total_debt = fred_request_data("TODNS") %>%
  mutate(chng = value / lag(value, 20))
plot <- plot_ly(total_debt, x = ~date, y = ~chng, type = 'scatter', mode = 'lines') 
plot 
# Shiny server ####

monetaryServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$credit_plot1.1 <- renderPlotly({
        credit_plot1.1
      })
      
      output$credit_plot1.2 <- renderPlotly({
        credit_plot1.2
      })
      
      output$credit_plot1.3 <- renderPlotly({
        credit_plot1.3
      })
      
      output$credit_plot1.4 <- renderPlotly({
        credit_plot1.4
      })
      
      output$real_odl_plot1 <- renderPlotly({
        real_odl_plot1
      })
      
      output$real_odl_plot2 <- renderPlotly({
        real_odl_plot2
      })

      output$bank_credit_plot1.1 <- renderPlotly({
        bank_credit_plot1.1
      })
      
      output$bank_credit_plot1.2 <- renderPlotly({
        bank_credit_plot1.2
      })
      
      output$bank_credit_plot1.3 <- renderPlotly({
        bank_credit_plot1.3
      })
      
      output$loan_leases_plot1.1 <- renderPlotly({
        loan_leases_plot1.1
      })
      
      output$loan_leases_plot1.2 <- renderPlotly({
        loan_leases_plot1.2
      })
      
      output$loan_leases_plot1.3 <- renderPlotly({
        loan_leases_plot1.3
      })
      
      
      
      output$ci_loans_plot1.1 <- renderPlotly({
        ci_loans_plot1.1
      })
      
      output$ci_loans_plot1.2 <- renderPlotly({
        ci_loans_plot1.2
      })
      
      output$ci_loans_plot1.3 <- renderPlotly({
        ci_loans_plot1.3
      })
      
      output$real_estate_plot1 <- renderPlotly({
        real_estate_plot1
      })
      
      output$real_estate_plot2 <- renderPlotly({
        real_estate_plot2
      })
      
      output$real_estate_plot3 <- renderPlotly({
        real_estate_plot3
      })
      
      output$consumer_loans_plot1.1 <- renderPlotly({
        consumer_loans_plot1.1
      })
      
      output$consumer_loans_plot1.2 <- renderPlotly({
        consumer_loans_plot1.2
      })
      
      output$consumer_loans_plot1.3 <- renderPlotly({
        consumer_loans_plot1.3
      })
      

      
      
      output$credit_plot2 <- renderPlotly({
        credit_plot2
      })
      
      output$fed_funds_plot1 <- renderPlotly({
        fed_funds_plot1
      })
      
      output$fed_funds_plot2 <- renderPlotly({
        fed_funds_plot2
      })
      
      output$diff_3m_to_5y5y_plot <- renderPlotly({
        diff_3m_to_5y5y_plot
      })
      
      output$diff_1y_to_5y5y_plot <- renderPlotly({
        diff_1y_to_5y5y_plot
      })
      
      output$bond_2y_yield_less_growth_plot1.1 <- renderPlotly({
        bond_2y_yield_less_growth_plot1.1
      })
      
      output$bond_2y_yield_less_growth_plot1.2 <- renderPlotly({
        bond_2y_yield_less_growth_plot1.2
      })
      
      output$baa_yield_less_growth_plot1.1 <- renderPlotly({
        baa_yield_less_growth_plot1.1
      })
      
      output$baa_yield_less_growth_plot1.2 <- renderPlotly({
        baa_yield_less_growth_plot1.2
      })
    }
  )
}


