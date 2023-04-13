beaKey 	<- '67B43A18-486E-46C1-B8BF-3237BA90FC02'

library(httr)
library(jsonlite)
library(dplyr)
library(Hmisc)
library(zoo)
library(plotly)


data_folder_path = "data/BEA"


get_nipa_table <- function (table_name, year = "X", frequency = "Q", force_new = FALSE){
  
  file_name = paste(data_folder_path, '/', table_name, ".csv", sep = '')
  # print(file_name)
  if (file.exists(file_name) && force_new == FALSE)
  {
    data = read.csv(file_name)
    
    data$date = as.Date(data$date)
    
    return(data)
  }
  else
  {
    url = "https://apps.bea.gov/api/data/?UserID=67B43A18-486E-46C1-B8BF-3237BA90FC02&method=GetData&datasetname=NIPA&ResultFormat=json"
    
    url = paste(url, "&", "year=", year, sep = "")
    
    url = paste(url, "&", "frequency=", frequency, sep = "")
    
    url = paste(url, "&", "tablename=", table_name, sep = "")
    
    print(url)
    
    # return(url)
    
    r = GET(url)
    
    
    r_text = content(r, as = "text")
    r_json <- fromJSON(r_text)
    
    data = r_json$BEAAPI$Results$Data %>% as.data.frame
    
    data = data %>%
      mutate(date = as.Date(as.yearqtr(TimePeriod, format = "%YQ%q")))
    
    write.csv(data, file_name, row.names = FALSE)
    
    return(data)  
  }
  

}

nipa5.1 = get_nipa_table("T50100", year = "X")

nipa5.1 = nipa5.1 %>%
  mutate(DataValue = gsub(",","",DataValue))  %>%
  mutate_if(all.is.numeric, as.numeric)

nipa1.12 = get_nipa_table("T11200", year = "X")

nipa1.12 = nipa1.12 %>%
  mutate(DataValue = gsub(",","",DataValue))  %>%
  mutate_if(all.is.numeric, as.numeric)

nipa4.1 = get_nipa_table("T40100", year = "X")

nipa4.1 = nipa4.1 %>%
  mutate(DataValue = gsub(",","",DataValue))  %>%
  mutate_if(all.is.numeric, as.numeric)


# Corporate investment ####
corporate_investment = nipa5.1 %>%
  filter(LineNumber == 23) %>%
  select(date, DataValue) %>%
  rename(investment = DataValue)

corporate_net_investment =  nipa5.1 %>%
  filter(LineNumber == 15) %>%
  select(date, DataValue) %>%
  rename(depreciation = DataValue) %>%
  left_join(corporate_investment, by = "date") %>%
  mutate(net_investment = investment - depreciation) %>%
  select(date, net_investment)

# Federal gov ####
federal_gov_investment = nipa5.1 %>%
  filter(LineNumber == 26) %>%
  select(date, DataValue) %>%
  rename(investment = DataValue)

federal_gov_depreciation = nipa5.1 %>%
  filter(LineNumber == 18) %>%
  select(date, DataValue) %>%
  rename(depreciation = DataValue)

federal_gov_saving= nipa5.1 %>%
  filter(LineNumber == 11) %>%
  select(date, DataValue) %>%
  rename(savings = DataValue)

federal_gov_deficit_spending = federal_gov_investment %>%
  left_join(federal_gov_depreciation, by = "date") %>%
  left_join(federal_gov_saving, by = "date") %>%
  mutate(deficit_spending = investment - depreciation - savings)

# State gov ####
state_gov_investment = nipa5.1 %>%
  filter(LineNumber == 27) %>%
  select(date, DataValue) %>%
  rename(investment = DataValue)

state_gov_depreciation = nipa5.1 %>%
  filter(LineNumber == 19) %>%
  select(date, DataValue) %>%
  rename(depreciation = DataValue)

state_gov_saving= nipa5.1 %>%
  filter(LineNumber == 12) %>%
  select(date, DataValue) %>%
  rename(savings = DataValue)

state_gov_deficit_spending = state_gov_investment %>%
  left_join(state_gov_depreciation, by = "date") %>%
  left_join(state_gov_saving, by = "date") %>%
  mutate(deficit_spending = investment - depreciation - savings)

# Household ####

household_investment = nipa5.1 %>%
  filter(LineNumber == 24) %>%
  select(date, DataValue) %>%
  rename(investment = DataValue)

household_depreciation = nipa5.1 %>%
  filter(LineNumber == 16) %>%
  select(date, DataValue) %>%
  rename(depreciation = DataValue)

household_saving= nipa5.1 %>%
  filter(LineNumber == 8) %>%
  select(date, DataValue) %>%
  rename(savings = DataValue)

household_deficit_spending = state_gov_investment %>%
  left_join(household_depreciation, by = "date") %>%
  left_join(household_saving, by = "date") %>%
  mutate(deficit_spending = investment - depreciation - savings)


# Current account balance ####

current_account_balance = nipa4.1 %>%
  filter(LineNumber == 33) %>%
  select(date, DataValue) %>%
  rename(current_account = DataValue)

# Dividends ####
dividends = nipa1.12 %>%
  filter(LineNumber == 16) %>%
  select(date, DataValue) %>%
  rename(dividends = DataValue)

corporate_profit = corporate_investment %>%
  left_join(dividends, by = "date") %>%
  left_join(current_account_balance, by = "date") %>%
  left_join(federal_gov_deficit_spending, by = "date") %>%
  left_join(state_gov_deficit_spending, by = "date") %>%
  left_join(household_deficit_spending, by = "date")
  
# plot1 <- plot_ly(corporate_net_investment, x = ~date, y = ~net_investment, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'Corp investment')
# 
# plot2 <- plot_ly(federal_gov_deficit_spending, x = ~date, y = ~deficit_spending, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'Federal deficit spending')
# 
# plot3 <- plot_ly(state_gov_deficit_spending, x = ~date, y = ~deficit_spending, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'State deficit spending')
# 
# plot4 <- plot_ly(household_deficit_spending, x = ~date, y = ~deficit_spending, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'Household deficit spending')
# 
# plot1
# plot2
# plot3
# plot4



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
# plot5 <- plot_ly(gov_debt, x = ~date, y = ~yoy, type = 'scatter', mode = 'lines') %>%
#   layout(title = 'gov_debt')
# 
# plot5
# 
# govt_debt_y2 <- list(
#   tickfont = list(color = "red"),
#   overlaying = "y",
#   side = "right",
#   title = "Govt debt",
#   range = c(0.9, 1.3))
# 
# govt_debt_plot = plot_ly(data = spx_earning, x = ~date, y = ~eps_relative_2yr, type = 'scatter', mode = 'lines')
# govt_debt_plot <- govt_debt_plot %>%
#   add_trace(data = gov_debt, x = ~shifted_date, y = ~yoy,  yaxis = "y2", type = 'scatter',
#             mode = 'lines', connectgaps = TRUE )
# 
# govt_debt_plot <- govt_debt_plot %>% layout(
#   yaxis2 = govt_debt_y2,
#   xaxis = list(title="date"),
#   yaxis = list(title="eps")
# )
# 
# govt_debt_plot
#   