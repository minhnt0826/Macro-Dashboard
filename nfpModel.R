
library(forecast)
library(dplyr)
source("Fred.R")
library(plotly)
library(lubridate)
source("helper.R")

# add_second_yxis_plotly <- function(plot, data, x, y, title){
#   
#   yaxis_2 <- list(
#     tickfont = list(color = "red"),
#     overlaying = "y",
#     side = "right",
#     title = title)
#   
#   plot_added_yaxis <- plot %>%
#     add_trace(data = data, x = x, y = y,  yaxis = "y2", name = title, type = 'scatter', mode = 'lines', connectgaps = TRUE )
# 
#   plot_added_yaxis <- plot_added_yaxis %>% layout(
#     yaxis2 = yaxis_2,
#     xaxis = list(title="date")
#   )
#   
#   
#   return(plot_added_yaxis)
# }

add_3_month_to_df  <- function(df){
  date_3months <- data.frame(date = seq(ymd('2023-04-01'),ymd('2023-06-01'),by='month'))
  
  df_new = df %>%
    full_join(date_3months, by = "date")
  
  return (df_new)
}


# Define NFP as focus group + main NFP excluding focus groups. Focus groups sectors 
# are gov, manufacturing, leisure hospitality and education & health services



# predicting main nfp using temporary help services ####
private_nfp_exclude_focus_groups = fred_request_data("USPRIV") %>%
  left_join(fred_request_data("USEHS"), by = "date") %>%
  left_join(fred_request_data("USLAH"), by = "date") %>%
  mutate(value = value.x - (value.y + value)) %>%
  select(date, value) %>%
  left_join(fred_request_data("MANEMP"), by = "date") %>%
  mutate(nfp_main = value.x - value.y) %>%
  select(date, nfp_main) %>%
  mutate(chng_nfp_main = (nfp_main / lag(nfp_main, 1) - 1) * 100,
         chng_nfp_main_yoy = (nfp_main / lag(nfp_main, 12) - 1) * 100,
         chng_nfp_main_yoy_rel = chng_nfp_main_yoy - SMA(chng_nfp_main_yoy, 48))

temp_help = fred_request_data("TEMPHELPS") %>%
  mutate(chng_temp = (value / lag(value, 1) - 1) * 100,
         chng_temp_yoy = (value / lag(value, 12) - 1) * 100,
         chng_temp_yoy_rel = chng_temp_yoy - SMA(chng_temp_yoy, 48))

nfp_model_data = private_nfp_exclude_focus_groups %>%
  left_join(temp_help, by = "date") %>%
  add_3_month_to_df(.) %>%
  select(date, nfp_main, chng_nfp_main_yoy, chng_nfp_main_yoy_rel, chng_temp_yoy, chng_temp_yoy_rel) %>%
  mutate(chng_temp_yoy_rel_lag3 = lag(chng_temp_yoy_rel, 3))
  # mutate(chng_nfp_main_sma3 = SMA(chng_nfp_main, 3),
  #        chng_temp_lag3_sma3 = SMA(chng_temp_lag3, 3))

data_train = nfp_model_data %>%
  select(date, chng_nfp_main_yoy_rel, chng_temp_yoy_rel_lag3) %>%
  filter(date <= "2019-12-01")
  
  
linear_model <- lm(chng_nfp_main_yoy_rel ~ chng_temp_yoy_rel_lag3, data = data_train)
summary(linear_model)

nfp_model_data$predict <- predict(linear_model, newdata = nfp_model_data)




plot = plot_ly(nfp_model_data, x = ~date, y=~chng_nfp_main_yoy_rel, type = "scatter", mode = "lines") %>%
       add_trace(y = ~predict, name = "linear pred")
plot

nfp_model_data_2 = nfp_model_data %>%
  # fill(chng_nfp_main_yoy, .direction = "down") %>%
  mutate(chng_nfp_main_yoy_sma48 = rollmean(chng_nfp_main_yoy, 48, fill = NA, align = "right")) %>%
  fill(chng_nfp_main_yoy_sma48, .direction = "down") %>%
  mutate(predict_add_back = predict + chng_nfp_main_yoy_sma48 + 1) %>%
  mutate(predict_add_back_per = predict_add_back / 100 + 1) %>%
  mutate(nfp_pred = coalesce(nfp_main, predict_add_back_per * lag(nfp_main, 12))) %>%
  mutate(chng_nfp_pred = nfp_pred - lag(nfp_pred , 1))
         # mutate(predict = 1 + predict / 100) %>%
  # mutate(nfp_main_predict = coalesce(nfp_main, predict * ))

plot2 = plot_ly(nfp_model_data_2, x = ~date, y=~chng_nfp_main_yoy, type = "scatter", mode = "lines") %>%
    add_trace(y = ~predict_add_back, name = "linear pred")
plot2
  






# Predict govt, leisure & hospitality and heathcare & ed using ARIMA ####
us_govt = fred_request_data("USGOVT")
us_govt_ts= ts(us_govt$value, start = c(1939, 01), frequency = 12)

focus_group = fred_request_data("USEHS") %>%
  left_join(fred_request_data("USLAH"), by = "date") %>%
  mutate(focus_group = value.x + value.y)


ts_data_log <- log(us_govt_ts)
fit <- arima(ts_data_log, order = c(3,1,12))

forecast_months <- 3
forecast_result <- forecast(fit, h = forecast_months)
forecast_values <- exp(forecast_result$mean)

plot(forecast_values)

autoplot(forecast_result)

us_govt_pred = as.data.frame(forecast_values)

us_focus_group_ts = ts(focus_group$focus_group, start = c(1939, 01), frequency = 12)

us_focus_group_ts_log <- log(us_focus_group_ts)


us_focus_group_fit <- arima(us_focus_group_ts_log, order = c(3,1,12))

us_focus_group_forecast_result <- forecast(us_focus_group_fit, h = forecast_months)

us_focus_group_forecast_values <- exp(us_focus_group_forecast_result$mean)

us_focus_dat_pred = as.data.frame(us_focus_group_forecast_values)


# Predict manufacturing payrolls using forward hiring intentions of 
# Fed regional surveys ####
man_future_employment = f_employment_data.z %>%
  select(contains("date" ) | contains('man')) %>%
  mutate(mean.z = rowMeans(across(where(is.numeric)), na.rm = T))
write.csv(man_future_employment, file = "fed_surveys_man.csv", row.names = F)

nfp_manufacturing = fred_request_data("MANEMP") %>%
  mutate(chng = value / lag(value , 1)) %>%
  mutate(chng.z = scale(chng)) %>%
  right_join(man_future_employment, by = "date") %>%
  add_3_month_to_df(.) %>%
  mutate(mean.z_lag3 = lag(mean.z, 2)) %>%
  select(date, value, chng, chng.z, mean.z, mean.z_lag3)

nfp_manufacturing_train = nfp_manufacturing %>%
  filter(date < "2020-01-01")
# plot1 <- plot_ly(nfp_payrolls, x = ~date, y = ~mean.z, type = 'scatter') %>%
#   add_trace(x = ~date, y = ~chng, name = "nfp", mode = 'lines')
# plot1

man_linear = lm(chng ~ mean.z_lag3, data = nfp_manufacturing_train)
summary(man_linear)

# data_test = data.frame(y )

ggplot(nfp_manufacturing_train, aes(x=chng, y=mean.z_lag3)) +
  geom_point()+
  geom_smooth(method=lm)

nfp_manufacturing$man_pred = predict(man_linear, newdata = nfp_manufacturing)

nfp_manufacturing = nfp_manufacturing %>%
  mutate(value = coalesce(value, man_pred * lag(value, 1))) %>%
  mutate(value = coalesce(value, man_pred * lag(value, 1))) %>%
  mutate(value = coalesce(value, man_pred * lag(value, 1))) %>%
  mutate(chng_man = value - lag(value, 1))


# Predict nfp gains from changes in U3 rate ####

private_nfp_exclude_focus_groups = fred_request_data("USPRIV") %>%
  left_join(fred_request_data("USEHS"), by = "date") %>%
  left_join(fred_request_data("USLAH"), by = "date") %>%
  mutate(value = value.x - (value.y + value)) %>%
  select(date, value) %>%
  left_join(fred_request_data("MANEMP"), by = "date") %>%
  mutate(nfp_main = value.x - value.y)

u3_main_nfp = fred_request_data("UNEMPLOY") %>%
  left_join(fred_request_data("CLF16OV"), by = "date") %>%
  mutate(u3 = (value.x / value.y) * 100) %>%
  select(date, u3) %>%
  mutate(chng_u3 = u3 - lag(u3, 1)) %>%
  left_join(private_nfp_exclude_focus_groups, by = "date") %>%
  mutate(chng_nfp_main = (nfp_main / lag(nfp_main, 1) - 1) * 100) %>%
  mutate(chng_u3_sma6 = SMA(chng_u3, 6))

u3_main_nfp_train = u3_main_nfp %>%
  filter(date < "2020-02-01" & date >= "1985-01-01")


ggplot(u3_main_nfp_train, aes(x=chng_nfp_main, y=chng_u3_sma6)) +
  geom_point()+
  geom_smooth(method=lm)



plot3 = u3_main_nfp_train%>%
  plot_ly(., x = ~date, y=~chng_nfp_main, name = "nfp",type = "scatter", mode = "lines") %>%
  add_second_yxis_plotly(., 
                         u3_main_nfp_train, 
                         u3_main_nfp_train$date,
                         -u3_main_nfp_train$chng_u3_sma6,
                         "u3")

plot3

u3_main_nfp_lm = lm(chng_nfp_main ~ chng_u3_sma6, data = u3_main_nfp_train)
summary(u3_main_nfp_lm)


# Predict u3 changes from u2 changes ####

u2 = fred_request_data("LNS13026638") %>%
  left_join(fred_request_data("CLF16OV"), by = "date") %>%
  mutate(u2 = (value.x / value.y) * 100) %>%
  mutate(u2_chng_3m = u2 - lag(u2, 6)) %>%
  left_join(u3, by = "date")  %>%
  mutate(u3_chng_3m = u3 - lag(u3, 6)) %>%
  select(date, u2, u2_chng_3m, u3, u3_chng_3m) %>%
  add_3_month_to_df(.) %>%
  mutate(u2_lag3 = lag(u2, 3),
         u2_chng_3m_lag_3 = lag(u2_chng_3m, 3))

u2_train = u2 %>%
  filter(date < "2020-01-01")

ggplot(u2_train, aes(x=u3, y=u2_lag3)) +
  geom_point()+
  geom_smooth(method=lm)

ggplot(u2_train, aes(x=u3_chng_3m, y=u2_chng_3m_lag_3)) +
  geom_point()+
  geom_smooth(method=lm)

u2_linear = lm(u3_chng_3m ~ u2_chng_3m_lag_3, data = u2_train)
summary(u2_linear)

u2$u3_chng_3m_pred = predict(u2_linear, newdata = u2)

u2 = u2 %>%
  mutate(u3_pred = coalesce(u3, u3_chng_3m_pred + lag(u3, 3)))


plot4 = u2_train%>%
  plot_ly(., x = ~date, y=~u3_chng_3m, name = "u3",type = "scatter", mode = "lines") %>%
  add_second_yxis_plotly(., 
                         u2, 
                         u2_train$date,
                         u2_train$u2_chng_3m_lag_3,
                         "u2")

plot4

# Predict u3 changes from nfib hiring plans and challenger job cuts ####

challenger = read.csv("data/Challenger/challenger_cuts.csv") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(challenger.z = as.vector(scale(value))) %>%
  left_join(nfib_emp_plans, by = "date") %>%
  mutate(nfib_plans = 100 - nfib_plans) %>%
  mutate(nfib_plans.z = as.vector(scale(nfib_plans))) %>%
  select(date, value, challenger.z, nfib_plans, nfib_plans.z) %>%
  mutate(nfib_plans.z_yoy = nfib_plans.z - lag(nfib_plans.z , 3)) %>%
  mutate(plans_minus_firing.z = nfib_plans.z + challenger.z) %>%
  add_3_month_to_df(.) %>%
  mutate(plans_minus_firing.z_lag3 = lag(plans_minus_firing.z,3)) %>%
  mutate(plans_minus_firing.z_lag3_yoy = plans_minus_firing.z_lag3 - lag(plans_minus_firing.z_lag3, 12)) %>%
  left_join(u3, by = "date") %>%
  select(date, plans_minus_firing.z_lag3_yoy, u3) %>%
  mutate(u3_yoy = u3 - lag(u3, 3))

plot6 = challenger%>%
  plot_ly(., x = ~date, y=~SMA(plans_minus_firing.z_lag3_yoy,3), name = "hiring plans - firing",type = "scatter", mode = "lines") %>%
  add_second_yxis_plotly(., 
                         challenger, 
                         challenger$date,
                         challenger$u3_yoy,
                         "u3 yoy")

plot6

challenger_hiring_u3_lm = lm(u3_yoy ~ plans_minus_firing.z_lag3_yoy, data = challenger)
summary(challenger_hiring_u3_lm)

challenger = challenger %>%
  mutate(u3_yoy_pred = predict(challenger_hiring_u3_lm, newdata = challenger)) %>%
  mutate(u3_pred = coalesce(u3, u3_yoy_pred + lag(u3, 3)))


# Get the predicted u3 changes from previous 2 models and translate it to 
# expected NFP main changes using U3->NFP model

u3_pred = challenger %>%
  rename(u3_pred_by_challenger = u3_pred) %>%
  select(date, u3_pred_by_challenger) %>%
  left_join(u2, by = "date") %>%
  rename(u3_pred_by_u2 = u3_pred) %>%
  select(date, u3_pred_by_challenger, u3_pred_by_u2) %>%
  mutate(u3_pred_mean = (u3_pred_by_challenger + u3_pred_by_u2) / 2) %>%
  mutate(chng_u3_pred_mean = u3_pred_mean - lag(u3_pred_mean, 1)) %>%
  mutate(chng_u3_sma6 = SMA(chng_u3_pred_mean, 6)) 

u3_pred$main_nfp_pred = predict(u3_main_nfp_lm, newdata = u3_pred) 


# Merge everything toghether to get nfp number. NFP = output from main nfp models 
# + output from sectors models.

main_nfp_pred = u3_pred %>%
  select(date, main_nfp_pred_chng = main_nfp_pred) %>%
  left_join(private_nfp_exclude_focus_groups, by = "date") %>%
  select(date, nfp_main, main_nfp_pred_chng) %>%
  mutate(main_nfp_pred_chng = 1 + main_nfp_pred_chng / 100) %>%
  mutate(nfp_main = coalesce(nfp_main, lag(nfp_main, 1) * main_nfp_pred_chng)) %>%
  mutate(nfp_main = coalesce(nfp_main, lag(nfp_main, 1) * main_nfp_pred_chng)) %>%
  mutate(nfp_main = coalesce(nfp_main, lag(nfp_main, 1) * main_nfp_pred_chng)) %>%
  mutate(chng_nfp_main = nfp_main - lag(nfp_main, 1)) %>%
  left_join(nfp_model_data_2, by ="date") %>%
  select(date, chng_nfp_main, chng_nfp_pred) %>%
  mutate(mean_pred_chng_nfp_main = (chng_nfp_main + chng_nfp_pred) / 2) %>%
  left_join(nfp_manufacturing, by = "date") %>%
  select(date, mean_pred_chng_nfp_main, chng_man)

focus_group = fred_request_data("USEHS") %>%
  left_join(fred_request_data("USLAH"), by = "date") %>%
  mutate(focus_group = value.x + value.y) %>%
  mutate(focus_group_chng = focus_group - lag(focus_group, 1)) %>%
  mutate(focus_group_chng_sma6 = SMA(focus_group_chng, 12)) 

  
main_nfp_full = main_nfp_pred %>%
  left_join(focus_group, by = "date") %>%
  select(date, mean_pred_chng_nfp_main, chng_man, focus_group) %>%
  left_join(fred_request_data("USGOVT"), by = "date") %>%
  rename(usgovt = value) %>%
  select(date, mean_pred_chng_nfp_main, chng_man, focus_group, usgovt)

main_nfp_full$focus_group[352:354] = us_focus_dat_pred$x
main_nfp_full$usgovt[352:354] = us_govt_pred$x

main_nfp_full = main_nfp_full %>%
  mutate(focus_group_chng = focus_group - lag(focus_group, 1),
         usgovt_chng = usgovt - lag(usgovt, 1)) %>%
  mutate(total_chng = mean_pred_chng_nfp_main + chng_man + usgovt_chng + focus_group_chng)
