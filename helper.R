library(TTR)
library(dplyr)
library(plotly)
library(lubridate)

n_month_growth_ann <- function(x, n, na.rm = TRUE) {
  return( ((x / lag(x, n)) ^ (12/n) - 1) * 100 )
}

n_month_growth_ann_sma <- function(x, n, na.rm = TRUE) SMA(((x / lag(x, n)) ^ (12/n) - 1) * 100, 3)

growth_over_lag <- function(x, n, na.rm = TRUE) {
  return((x / lag(x, n) - 1)*100)
}

create_growth_data_for_df <- function(data, series){
  df = data %>%
    mutate(growth_1m = n_month_growth_ann(series , 1),
           growth_3m = n_month_growth_ann(series , 3),
           growth_6m = n_month_growth_ann(series , 6),
           growth_12m = n_month_growth_ann(series , 12))
    
  return (df)
}

create_plotly_plot_with_growth_data <- function(data, sma = F, growth_1m = F) {
  
  if (growth_1m)
  {
    plot1 = plot_ly(data, x = ~date, y = ~growth_1m, type = 'scatter', mode = 'lines', name = "1m") %>%
      add_trace(y = ~growth_3m, name = "3m") %>%
      add_trace(y = ~growth_6m, name = "6m") %>%
      add_trace(y = ~growth_12m, name = "12m") 
  }
  else
  {
    plot1 = plot_ly(data, x = ~date, y = ~growth_3m, type = 'scatter', mode = 'lines', name = "3m") %>%
      add_trace(y = ~growth_6m, name = "6m") %>%
      add_trace(y = ~growth_12m, name = "12m") 
  }

  
  return (plot1)
}

shift_date_series <- function(x, yr = 0, m = 0, d = 0) {
  d = ymd(x)
  
  # print(d)
  
  d = d %m+% years(yr)
  d = d %m+% months(m)
  # d = d %m+% days(d)
  
  return(as.Date(d))
}
  
add_second_yxis_plotly <- function(plot, data, x, y, title){
  
  yaxis_2 <- list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right",
    title = title)
  
  plot_added_yaxis <- plot %>%
    add_trace(data = data, x = x, y = y,  yaxis = "y2", name = title, type = 'scatter', mode = 'lines', connectgaps = TRUE )
  
  plot_added_yaxis <- plot_added_yaxis %>% layout(
    yaxis2 = yaxis_2,
    xaxis = list(title="date"),
    yaxis = list(title=title)
  )
  
  
  return(plot_added_yaxis)
}

add_months_to_df  <- function(df, num_months){
  
  max_date = ymd(max(df$date))
  
  max_date_plus_months = max_date %m+% months(num_months)
  
  date_months <- data.frame(date = seq(max_date, max_date_plus_months, by='month'))
  
  df_new = df %>%
    full_join(date_months, by = "date")
  
  return (df_new)
}

# create_plotly_plot_with_series <- function(data, series, sma = F) {
# 
#   plot1 = plot_ly(data, x = ~date, y = ~n_month_growth_ann(series , 3), type = 'scatter', mode = 'lines', name = "3m") %>%
#     add_trace(y = ~n_month_growth_ann(series, 6), name = "6m") %>%
#     add_trace(y = ~n_month_growth_ann(series, 12), name = "12m")
#     # add_trace(y = ~n_month_growth_ann(series, 1), type = "bar", name = "1m", marker = list(color = "grey")) %>%
#     # layout(yaxis = list(range = c(-30,50)))
#   
#   return (plot1)
# }