library(dplyr)
library(lubridate)



load_data_from_boe <- function(series_code, start_date = "1965-01-01", end_date = "now")
{
  data_folder_path = "UK/data/boe"
  data_url = "https://www.bankofengland.co.uk/boeapps/database/_iadb-fromshowcolumns.asp?csv.x=yes&CSVF=CN&UsingCodes=Y&VPD=Y"
  
  
  start_date_format = format(as.Date(start_date, format = "%Y-%m-%d"), "%d/%b/%Y")
  
  if (end_date != "now")
  {
    end_date_format = format(as.Date(end_date, format = "%Y-%m-%d"), "%d/%b/%Y")
  }
  else
  {
    end_date_format = end_date
  }
  
  data_url = paste(data_url, "&", "SeriesCodes=", series_code, sep = "")
  
  data_url = paste(data_url, "&", "Datefrom=", start_date_format, sep = "")
  
  data_url = paste(data_url, "&", "Dateto=", end_date_format, sep = "")
  
  print(data_url)
  
  data = read.csv(data_url)

  data = data %>%
    select(date = DATE, value = VALUE) %>%
    mutate(date = format(as.Date(date, format = "%d %b %Y"), "%Y-%m-01"))
    # mutate(date = as.Date(date, format = "%d %b %Y"))

  file_name = paste(data_folder_path, '/', series_code, '.csv', sep = '')
  write.csv(data, file_name, row.names = FALSE)
}

boe_request_data <- function(series_code, start_date = "1965-01-01", end_date = "now", force_new = FALSE)
{
  data_folder_path = "UK/data/boe"
  file_name = paste(data_folder_path, '/', series_code, '.csv', sep = '')
  if (file.exists(file_name) & force_new == FALSE)
  {
    data = read.csv(file_name)
    data$date = as.Date(data$date)
    
    return(data) 
  }
  else
  {
    load_data_from_boe(series_code = series_code, start_date = start_date, end_date = end_date)
    
    data = read.csv(file_name)
    data$date = as.Date(data$date)
    
    return(data)
  }
}




