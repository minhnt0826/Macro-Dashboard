library(janitor)
library(dplyr)
library(readxl)
library(zoo)
# Cali ####
cali_warn_org = read_excel("data/WARN/cali_warn_report.xlsx", skip =  0) %>%
  select(!contains("...")) %>%
  rename(effective_date = 'Effective Date',
         received_date = 'Received Date',
         emp_num = `No. Of Employees`) %>%
  select(received_date, effective_date, emp_num)


# cali_warn = read.csv("data/WARN/cali.csv")

cali_warn = cali_warn_org %>%
  group_by(effective_date = as.yearmon(effective_date)) %>%
  summarise(total = sum(emp_num))

# Texas ####
texas_warn_1 = read_excel("data/Warn/texas/warn-act-listings-2021.xlsx")
texas_warn_2 = read_excel("data/Warn/texas/warn-act-listings-2022.xlsx")
texas_warn_3 = read_excel("data/Warn/texas/warn-act-listings-2023-twc.xlsx")

texas_warn = rbind(texas_warn_1, texas_warn_2, texas_warn_3) %>%
    rename(effective_date = LayOff_Date,
           received_date = WFDD_RECEIVED_DATE,
           emp_num = TOTAL_LAYOFF_NUMBER) %>%
    select(received_date, effective_date, emp_num)


# texas_warn = read.csv("data/WARN/texas.csv")

texas_warn = texas_warn %>%
  group_by(effective_date = as.yearmon(effective_date)) %>%
  summarise(total = sum(emp_num))


# illinois ####
# illinois_warn = read.csv("data/WARN/illinois.csv") %>%
#   mutate(received_date = as.Date(received_date, format = "%d/%m/%Y"),
#          effective_date = as.Date(effective_date, format = "%d/%m/%Y")) %>%
#   group_by(effective_date = as.yearmon(effective_date)) %>%
#   summarise(total = sum(emp_num))

illinois_clean_df <- function(df){
    clean_df_1 = df %>%
      drop_na("COMPANY NAME:")
    
    row_slice_num = match("DBA", clean_df_1$`DBA:`)
    
    print(row_slice_num)
    clean_df_2 = clean_df_1 %>%
      slice(row_slice_num:nrow(clean_df_1)) %>%
      row_to_names(row_number = 1) %>%
      clean_names() %>%
      rename(effective_date = 'first_layoff_date',
             received_date = 'supp_notice_received_date',
             emp_num = 'additional_workers_affected') %>%
      mutate(effective_date = ifelse(effective_date == "Not Provided", received_date, effective_date)) %>%
      drop_na(effective_date) %>%
      mutate(effective_date = as.Date(as.numeric(effective_date), origin = "1899-12-30"),
             received_date = as.Date(as.numeric(received_date), origin = "1899-12-30")) %>%
      select(effective_date, received_date, emp_num)
    
    
    clean_df_1 = clean_df_1 %>%
      slice(1:row_slice_num-1) %>%
      clean_names() %>%
      rename(effective_date = 'first_layoff_date',
             received_date = 'warn_received_date',
             emp_num = 'number_workers_affected') %>%
      mutate(effective_date = ifelse(effective_date == "Not Provided", received_date, effective_date)) %>%
      drop_na(effective_date) %>%
      mutate(effective_date = as.Date(as.numeric(effective_date), origin = "1899-12-30"),
             received_date = as.Date(as.numeric(received_date), origin = "1899-12-30")) %>%
      select(effective_date, received_date, emp_num)

    clean_df = rbind(clean_df_1, clean_df_2)
    # 
  return(clean_df)
}

data_folder_name = "data/WARN/illinois"
fnames <- list.files(data_folder_name)
rm(illinois_warn)
for(i in fnames)
{
  file_name = paste(data_folder_name, '/', i, sep = '')
  df = read_excel(file_name)
  clean_df = illinois_clean_df(df)
  
  if (exists("illinois_warn"))
  {
    illinois_warn = rbind(illinois_warn, clean_df)
  }
  else
  {
    assign("illinois_warn", clean_df)
  }
}

illinois_warn = illinois_warn %>%
  group_by(effective_date = as.yearmon(effective_date)) %>%
  mutate(emp_num = as.numeric(emp_num)) %>%
  summarise(total = sum(emp_num))


# florida_warn = read.csv("data/WARN/florida.csv") %>%
#   mutate(received_date = as.Date(received, format = "%m-%d-%y")) %>%
#   mutate(effective_date = as.Date(substring(effective_dae, 0, 8), format = "%m-%d-%y")) %>%
#   select(received_date, effective_date, emp_num) %>%
#   group_by(effective_date = as.yearmon(effective_date)) %>%
#   summarise(total = sum(emp_num))



# warn_total = rbind(illinois_warn, texas_warn, cali_warn, florida_warn) %>%
#   group_by(effective_date) %>%
#   summarise(total = sum(total))
