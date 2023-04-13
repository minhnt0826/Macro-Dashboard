
library(data.table)
library(readxl)
library(dplyr)
library(janitor)
source("helper.R")
# install.packages("janitor")

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

pce_detail = read_excel("data/PCE Detail/pce_detail.xlsx", sheet = "U20404-M", skip = 7)

pce_detail = pce_detail[-c(1,3)] %>%
  filter(row_number() <= 360)


pce_detail_t = data.table::transpose(pce_detail)

pce_detail_t = pce_detail_t %>%
  row_to_names(row_number = 1)

pce_detail_t[] <- sapply(pce_detail_t, as.numeric)

pce_detail_t$date = colnames(pce_detail)[2:770]
pce_detail_t = pce_detail_t %>%
  select(date, everything()) %>%
  mutate(date = as.Date(paste(date, "01", sep = "-"), format = "%YM%m-%d")) %>%
  select(where(not_all_na)) %>%
  mutate(date = as.Date(date))


pce_detail_t = pce_detail_t %>%
  mutate(across(where(is.numeric), ~(. / lag(., 1) - 1)*100 ))

pce_detail_t = pce_detail_t  %>%
  rowwise() %>%
  mutate(med_chng = median(c_across(2:359), na.rm = TRUE)) %>%
  mutate(avg_chng = mean(c_across(2:359), na.rm = TRUE)) %>%
  ungroup()

pce_median = pce_detail_t %>%
  select(date, med_chng) %>%
  slice(-1) %>%
  mutate(value = 100 * cumprod(med_chng / 100 + 1)) %>%
  mutate(growth_3m = n_month_growth_ann(value, 3),
         growth_6m = n_month_growth_ann(value, 6),
         growth_12m = n_month_growth_ann(value, 12))

pce_avg = pce_detail_t %>%
  select(date, avg_chng) %>%
  slice(-1) %>%
  mutate(value = 100 * cumprod(avg_chng / 100 + 1)) %>%
  mutate(growth_3m = n_month_growth_ann(value, 3),
         growth_6m = n_month_growth_ann(value, 6),
         growth_12m = n_month_growth_ann(value, 12))



plot1 = plot_ly(data = pce_median, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines', name = "3m") %>%
  add_trace(y = ~growth_6m, name = "6m") %>%
  add_trace(y = ~growth_12m, name = "12m") %>%
  layout(title = 'PCE median')

plot1

plot2 = plot_ly(data = pce_avg, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines', name = "3m") %>%
  add_trace(y = ~growth_6m, name = "6m") %>%
  add_trace(y = ~growth_12m, name = "12m") %>%
  layout(title = 'PCE avg')

plot2


