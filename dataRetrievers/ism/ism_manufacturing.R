# ism_pre_2016 = read.csv("/Users/nguyenthanhminh/Downloads/FRED-NAPMEI.csv") %>%
#   mutate(date = as.Date(DATE)) %>%
#   select(date, value = VALUE)
# 
# ism_from_2016 = read.csv("/Users/nguyenthanhminh/Downloads/ism_emp_from_2012.csv") %>%
#   mutate(date = as.Date(Date, format = "%Y.%m.%d")) %>%
#   mutate(date = as.Date(as.yearmon(date), frac = 0)) %>%
#   mutate(date = shift_date_series(x=date, m=-1)) %>%
#   select(date, value = ActualValue) %>%
#   filter(date > "2016-05-01")
# 
# ism_emp = rbind(ism_from_2016, ism_pre_2016)
# 
# write.csv(ism_emp, "data/ISM/ism_manufacturing_employment.csv", row.names = F)
# 
