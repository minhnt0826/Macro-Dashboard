# install.packages("rvest")
library(httr)
library(dplyr)
# 
# data_path = "data/States Claims Data"
# df = data.frame(init_start = c("1984-06-09"),
#                 init_end = c("2023-02-25"),
#                 continuing_start = c("1984-06-02"),
#                 continuing_end = c("2023-02-18"))
# 
# write_csv(df, paste0(data_path, "/config.csv"))

# init_claims_url = "https://fredaccount.stlouisfed.org/public/datalist/3023/download"
# continuing_claims_url = "https://fredaccount.stlouisfed.org/public/datalist/3024/download"

add_week_to_config <- function() {
  config_current = read.csv("data/States Claims Data/config.csv")
  
  config_current$init_end[1] = format(as.Date(config_current$init_end[1]) + 7, "%Y-%m-%d") 
  
  config_current$continuing_end[1] =  format(as.Date(config_current$continuing_end[1]) + 7, "%Y-%m-%d")   
  
  write.csv(config_current, "data/States Claims Data/config.csv")
}


download_state_init_claims <- function(start_date, end_date){
  POST("https://fredaccount.stlouisfed.org/public/datalist/3023/download", 
       encode = "form",
       body = list(obs_start_date = start_date,
                   obs_end_date = end_date,
                   file_format = "xls",
                   download_data ="Download Data"),
       httr::write_disk("data/States Claims Data/init_claims.zip", overwrite = T))
  unzip("data/States Claims Data/init_claims.zip",exdir = "data/States Claims Data", overwrite = T)
}


download_state_continuing_claims <- function(start_date, end_date){
  POST("https://fredaccount.stlouisfed.org/public/datalist/3024/download", 
       encode = "form",
       body = list(obs_start_date = start_date,
                   obs_end_date = end_date,
                   file_format = "xls",
                   download_data ="Download Data"),
       httr::write_disk("data/States Claims Data/cont_claims.zip", overwrite = T))
  unzip("data/States Claims Data/cont_claims.zip",exdir = "data/States Claims Data", overwrite = T)
  
}

add_week_to_config()
config = read.csv("data/States Claims Data/config.csv")
download_state_init_claims(config$init_start[1], config$init_end[1])
download_state_continuing_claims(config$continuing_start[1], config$continuing_end[1])
