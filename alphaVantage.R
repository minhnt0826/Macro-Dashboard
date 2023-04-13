data_folder_path = "/Users/nguyenthanhminh/Documents/Stocks/Macro Dashboard/data/Alpha Vantage"

alpha_vantage_url = "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=%symbol_placeholder&outputsize=full&apikey=C5X4GZORZ21YDPWK&datatype=csv"

load_stock_close_from_alpha_vantage <- function (symbol) {
  
  url = gsub("%symbol_placeholder", symbol, alpha_vantage_url)
  
  data = read.csv(url)
  
  data = data %>%
    rename(date = timestamp)
  
  return(data)
}

get_stock_close_data <- function(symbol, force_new = FALSE) {

  file_name = paste(data_folder_path, '/', symbol, ".csv", sep = '')
  if (file.exists(file_name))
  {
    data = read.csv(file_name)
    
    last_modified_time = as.Date(file.info(file_name)$mtime)
    today = Sys.Date()
    
    diff_dates = difftime(today, last_modified_time, units = "days")
    
    print(diff_dates)
    
    if (force_new | as.numeric(diff_dates) >= 1)
    {
      data = load_stock_close_from_alpha_vantage(symbol)
      write.csv(data, file_name, row.names = FALSE)
    }
    
    data$date = as.Date(data$date)
    
    data = data %>%
      arrange(date)
    
    return(data) 
  }
  else
  {
    data = load_stock_close_from_alpha_vantage(symbol)
    
    write.csv(data, file_name, row.names = FALSE)
    
    data$date = as.Date(data$date)
    
    data = data %>%
      arrange(date)
    
    return(data)
  }
    
}