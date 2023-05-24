# https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l55o/mm23

library(stringi)

ons_request_data = function(series_id) {
  data_folder_path = "UK/data/ONS/"

  file_name = paste0(data_folder_path, '/', series_id, '.csv')
  
  if (file.exists(file_name))
  {
    data = read.csv(file_name) %>%
      mutate(date = as.Date(date, format = "%Y-%m-%d"))
    
    return(data)
  }
}

# 
# add_timeseries_to_config(key = "UKCOREINFLATION",
#                          ons_uri = "/economy/inflationandpriceindices/timeseries/dkc6/mm23",
#                          seas_adjusted = F)
# 
# add_timeseries_to_config(key = "UKGDPPRICE",
#                          ons_uri = "economy/grossdomesticproductgdp/timeseries/l8gg/pn2")

add_timeseries_to_config = function(key, description = "", ons_uri, seas_adjusted) {
  ons_url = "https://www.ons.gov.uk/generator?format=csv&uri="
  ons_url = paste0(ons_url, ons_uri)

  uk_config = read.csv("UK/data/ONS/config/timeseries_config.csv")

  uk_config = uk_config %>%
    add_row(key = key, description = description, ons_url = ons_url, seas_adjusted = seas_adjusted)

  write.csv(uk_config, "UK/data/ONS/config/timeseries_config.csv", row.names = F)
}

ons_update_data_from_cofig = function(){
  uk_config = read.csv("UK/data/ONS/config/timeseries_config.csv")
  
  for (i in 1:nrow(uk_config)){
    download_to_path = paste0("UK/data/ONS/", uk_config$key[i], ".csv")
    
    if(file.exists(download_to_path)) {
      next
    }
    
    download.file(uk_config$ons_url[i], destfile = download_to_path)
    format_data(uk_config$key[i],uk_config$seas_adjusted[i] == T)
  }
}


format_data <- function(series_id, seas_adjusted) {
  data_folder_path = "UK/data/ONS/"


  file_name = paste0(data_folder_path, '/', series_id, '.csv')

  data = read.csv(file_name, skip = 8, header = F, col.names = c("date", "value"))

  if (any(stri_detect_fixed(data$date, "JAN")))
  {
    data = data %>%
      filter(nchar(date) == 8) %>%
      mutate(date = as.Date(paste0(date, "-01"), format = "%Y %B-%d")) %>%
      filter(date >= "1980-01-01")

    if (!seas_adjusted) {
      data = data %>%
        mutate(value = seasonal_adjust_series(value, as.yearmon(date[1])))
    }
  }
  else if (any(stri_detect_fixed(data$date, "Q1")))
  {
    data = data %>%
      filter(nchar(date) == 7) %>%
      mutate(date = as.Date(as.yearqtr(date, format = "%Y Q%q")))
  }



  data_path = paste0("UK/data/ONS/", series_id, ".csv")
  write.csv(data, data_path, row.names = F)

}
# format_data("UKCORECPI", T)
# 
# data = read.csv("UK/data/ONS/UKCORECPI.csv") %>%
#   mutate(date = as.Date(date)) %>%
#   mutate(value = seasonal_adjust_series(value, as.yearmon(date[1])))
# 
# write.csv(data, "UK/data/ONS/UKCORECPI.csv", row.names = F)
