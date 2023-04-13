# for (i in 10:24) {
#   url = "https://www.cftc.gov/files/dea/history/dea_com_xls_20%i.zip"
#   url = gsub("%i", i, url)
# 
#   download.file(url, paste("data/CFTC/",i, ".zip", sep = ""))
#   
#   print(i)
# }

library(tidyverse)
library(zoo)

data_folder_name = "data/CFTC"
fnames <- list.files(data_folder_name)
count = 1
first = TRUE

fnames = fnames[grepl("*\\.xls", fnames)]

for(i in fnames)
{
  file_name = paste(data_folder_name, '/', i, sep = '')
  variable_name = paste("data", count, sep = "")
  count = count + 1
  a = read_excel(file_name)
  a$date = as.Date(a$Report_Date_as_MM_DD_YYYY)
  
  a = a %>%
    select(date, Market_and_Exchange_Names, Open_Interest_All, CFTC_Market_Code,
           NonComm_Positions_Long_All, NonComm_Positions_Short_All,
           Comm_Positions_Long_All, Comm_Positions_Short_All)
  
  
  if (first){
    data = a
    first = FALSE
  }
  else
  {
    data = rbind(data, a)
  }
  
}


data = data %>% 
  rename(product = Market_and_Exchange_Names,
         product_code = CFTC_Market_Code,
         open_interest = Open_Interest_All, 
         noncomm_long = NonComm_Positions_Long_All,
         noncomm_short = NonComm_Positions_Short_All,
         comm_long = Comm_Positions_Long_All,
         comm_short = Comm_Positions_Short_All) %>%
  mutate(noncomm_long_net = noncomm_long - noncomm_short,
         comm_long_net = comm_long - comm_short)

  usd = data %>%
    filter(product == "U.S. DOLLAR INDEX - ICE FUTURES U.S." | product == "USD INDEX - ICE FUTURES U.S.") %>%
    arrange(date) %>%
    mutate(max_noncomm_long_net = rollapplyr(noncomm_long_net, 26, max, partial = T),
           min_noncomm_long_net = rollapplyr(noncomm_long_net, 26, min, partial = T)) %>%
    mutate(index = (noncomm_long_net - min_noncomm_long_net) / (max_noncomm_long_net - min_noncomm_long_net) * 100) %>%
    mutate(rolling_7week_max_index = rollapplyr(index, 7, max, partial = T)) %>%
    mutate(z_score = as.numeric(scale(noncomm_long_net))) %>%
    mutate(rolling_7week_max_z_score = rollapplyr(z_score, 7, max, partial = T))
  
  plot1 <- plot_ly(usd, x = ~date, y = ~rolling_7week_max_index, type = 'scatter', mode = 'lines')
  plot1
  
  # eurodollar = data %>%
  #   filter(str_detect(product, "EURODOLLARS")) %>%
  #   arrange(date) %>%
  #   mutate(max_noncomm_long_net = rollapplyr(noncomm_long_net, 52, max, partial = T),
  #          min_noncomm_long_net = rollapplyr(noncomm_long_net, 52, min, partial = T)) %>%
  #   mutate(index = (noncomm_long_net - min_noncomm_long_net) / (max_noncomm_long_net - min_noncomm_long_net) * 100) %>%
  #   mutate(rolling_7week_max_index = rollapplyr(index, 7, max, partial = T)) %>%
  #   mutate(z_score = as.numeric(scale(noncomm_long_net))) %>%
  #   mutate(rolling_7week_max_z_score = rollapplyr(z_score, 7, max, partial = T))
  # 
  # plot2 <- plot_ly(eurodollar, x = ~date, y = ~rolling_7week_max_index, type = 'scatter', mode = 'lines')
  # plot2


