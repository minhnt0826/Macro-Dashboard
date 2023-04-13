library(fredr)
fredr_set_key("e86c15cf4098a7176f6edd1f9ef45600")

library(dplyr)
library(lubridate)

data_folder_path = "data/Fred"

load_data_from_fred <- function(key, series_id, start_date, end_date, freq, agg)
{
  print(series_id)
  
  data = fredr(
    series_id = series_id,
    observation_start = as.Date(start_date),
    observation_end = as.Date(end_date),
    frequency = freq,
    aggregation_method = agg
  )
  
  data = data %>%
    select(date, value)
  
  data$date = as.Date(data$date)
  
  

  file_name = paste(data_folder_path, '/', key, '.csv', sep = '')
  write.csv(data, file_name, row.names = FALSE)
}


update_outdated_series <- function(){
  releases_dates = fredr_releases_dates(realtime_start = today() - 14) %>%
    rename(last_updated = date) %>%
    select(release_id, last_updated) %>%
    group_by(release_id) %>%
    arrange(desc(last_updated)) %>%
    slice(1) %>%
    ungroup()

  series_releases = read.csv("data/Fred/config/series_releases.csv")
  
  series_releases_update_table = series_releases %>%
    inner_join(releases_dates, by = c("releases_id" = "release_id")) %>%
    filter(last_updated >= last_downloaded)
  
  for (key_str in series_releases_update_table$key){
    
    series_to_load = series_releases_update_table %>%
      filter(key == key_str)
    
    load_data_from_fred(key = key_str, series_id = series_to_load$series_id, start_date = "1900-01-01", end_date = "2100-01-01", freq = series_to_load$freq, agg = series_to_load$agg)
    
    series_releases$last_downloaded[match(key_str, series_releases$key)] = format(today(), "%Y-%m-%d")
  }

  write.csv(series_releases, "data/Fred/config/series_releases.csv", row.names = FALSE)
}

fred_series_requested <- function(series_id_requested, freq, agg){
  series_releases = read.csv("data/Fred/config/series_releases.csv")
  

  key_to_check = paste0(series_id_requested, "_", freq, "_", agg)
  if (key_to_check %in% series_releases$key){
    series_releases$last_requested[match(key_to_check, series_releases$key)] = format(today(), "%Y-%m-%d")
    
    write.csv(series_releases, "data/Fred/config/series_releases.csv", row.names = FALSE)
  }
  else 
  {
    print(nrow(series_releases) + 1)
    
    series_info = fredr_series(series_id = series_id_requested)
    releases_info = fredr_series_release(series_id = series_id_requested)
    
    key = paste0(series_info$id, "_", freq, "_", agg)
    
    series_releases[nrow(series_releases) + 1,] = c(key, series_info$title, series_info$id, releases_info$name, releases_info$id, 
                                                     format(today(), "%Y-%m-%d"), freq, agg, format(today(), "%Y-%m-%d"))
    
    write.csv(series_releases, "data/Fred/config/series_releases.csv", row.names = FALSE)
  }

}
# load_data_from_fred(series_id = "ADPWNUSNERSA")
# fred_series_requested("ADPWNUSNERSA", freq = "m", agg = "avg")
# 
# 
# df = data.frame(key = c(""), series_title = c(""), series_id = c(""), releases_title = c(""), releases_id = c(""), last_downloaded = c(""), freq = c(""), agg = c(""))
# write.csv(df, "data/Fred/config/series_releases.csv", row.names = FALSE)

# update_outdated_series()


# test_match_df = data.frame(series_id = c("ADPWNUSNERSA"), freq = c("w"), agg = c("avg"))
# test_match = test_match_df %>%
#   inner_join(series_releases, by = c("series_id", "freq","agg"))
# 
# nrow(test_match)
# series_releases = read.csv("data/Fred/config/series_releases.csv")
# 
# 
# write.csv(series_releases, "data/Fred/config/series_releases.csv", row.names = FALSE)

