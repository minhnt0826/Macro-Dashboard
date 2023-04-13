library(dplyr)

source("dataRetrievers/fredDataUpdater.R")

data_folder_path = "data/Fred"

fred_request_data <- function(series_id, start_date = "1900-01-01", end_date = "2100-01-01", force_new = FALSE,
                              freq = "", agg = "")
{
  fred_series_requested(series_id, freq, agg)
  
  key = paste0(series_id, "_", freq, "_", agg)
  
  file_name = paste(data_folder_path, '/', key, '.csv', sep = '')
  if (file.exists(file_name))
  {
    data = read.csv(file_name)
    data$date = as.Date(data$date)
    return(data) 
  }
  else
  {
    load_data_from_fred(key, series_id, start_date, end_date, freq = freq, agg = agg)
    
    data = read.csv(file_name)
    data$date = as.Date(data$date)
    
    return(data)
  }
  
}



