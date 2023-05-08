m_chng = 6


unemployment_rate = fred_request_data("UNRATE") %>%
  mutate(u3_rate = value)

broad_unemployment_rate = fred_request_data("U6RATE") %>%
  mutate(u6_rate = value)

unemployed_to_employed_flows = fred_request_data("LNS17100000") %>%
  left_join(fred_request_data("UNEMPLOY"), by = "date") %>%
  mutate(unemployed_employed_flows = value.x / lag(value.y,1))

jolts_quits = fred_request_data("JTS1000QUR") %>%
  mutate(quits = value)

emp_to_pop_ratio = fred_request_data("EMRATIO") %>%
  mutate(emp_to_pop_ratio = value)

partime_emp_econ_reasons = fred_request_data("LNS12032194") %>%
  left_join(fred_request_data("CE16OV"), by = "date") %>%
  mutate(partime_econ_reasons = value.x / value.y)

job_leavers_percent = fred_request_data("LNS13023706") %>%
  mutate(job_leavers = value)
  # mutate(partime_econ_3m = n_month_growth_ann(value, m_chng)) %>%
  # mutate(partime_econ_3m = scale(partime_econ_3m))

unemployed_27_weeks = fred_request_data("LNS13025703") %>%
  mutate(unemployed_27_weeks = value)

nfib_jobs_opening = read_excel("data/NFIB/with_vacant_position.xlsx") %>%
  mutate(date = as.Date(`Month/Year`)) %>%
  mutate(nfib_jobs_opening = as.numeric(`Current Job Openings`)) %>%
  add_row(date = as.Date("2023-03-01"), nfib_jobs_opening = 43) %>%
  mutate(nfib_jobs_opening = nfib_jobs_opening)

job_losers_percent = fred_request_data("LNS13023622") %>%
  mutate(job_losers = value)

jolt_hires = fred_request_data("JTS1000HIR") %>%
  mutate(hires = value)

nfib_emp_plans = read_excel("data/NFIB/plans_increase_employment.xlsx") %>%
  mutate(date = as.Date(`Month/Year`)) %>%
  mutate(nfib_plans = as.numeric(`Plans to Increase Employment`)) %>%
  add_row(date = as.Date("2023-03-01"), nfib_plans = 15)

average_hourly_earnings_private = fred_request_data("AHETPI") %>%
  mutate(avg_hourly_earnings_3m = (value / lag(value, m_chng) - 1) * 100)

ic_claims = fred_request_data("ICSA", agg = "avg", freq = "m") %>%
  left_join(fred_request_data("CLF16OV"), by = "date") %>%
  mutate(icsa = value.x / value.y)

employment_private = fred_request_data("USPRIV") %>%
  mutate(employment_private_3m = (value / lag(value, m_chng) - 1) * 100)

aggregate_hours_private = fred_request_data("AWHI") %>%
  mutate(aggregate_hours_private_3m = (value / lag(value, m_chng) - 1) * 100)

temp_help = fred_request_data("TEMPHELPS") %>%
  mutate(temp_help_3m = (value / lag(value, m_chng) - 1) * 100)

labor_participation_rate = fred_request_data("CIVPART") %>%
  mutate(participation_rate = value)

ism_employment = read.csv("data/ISM/ism_manufacturing_employment.csv") %>%
  mutate(date = as.Date(date),
         ism_emp = value)

# 
# residential = fred_request_data("CES2023610001") %>%
#   left_join(fred_request_data("CES2023800101"), by = "date") %>%
#   mutate(value = value.x + value.y) %>%
#   mutate(residential =  log(scale(value.x))) %>%
#   mutate(residential_building_3m = n_month_growth_ann(value.x, m_chng)) %>%
#   mutate(residential_3m = n_month_growth_ann(value, m_chng)) %>%
#   mutate(residential_3m = coalesce(residential_3m, residential_building_3m)) %>%
#   mutate(residential_3m = scale(residential_3m))
# 
# manufacturing = fred_request_data("MANEMP") %>%
#   mutate(manu = log(scale(value))) %>%
#   mutate(manu_3m = n_month_growth_ann(value, m_chng)) %>%
#   mutate(manu_3m = scale(manu_3m))
# 
# 
# job_losers_permanent = fred_request_data("LNS13026638") %>%
#   mutate(job_losers = scale(value)) %>%
#   mutate(job_losers_3m = n_month_growth_ann(value, m_chng)) %>%
#   mutate(job_losers_3m = scale(job_losers_3m))
# 
# 
# 
# aggregate_weekly_hours = fred_request_data("AWHI") %>%
#   mutate(chng_prod = value / lag(value, 1)) %>%
#   full_join(fred_request_data("AWHAE"), by = "date") %>%
#   mutate(chng_private= value.y / lag(value.y, 1)) %>%
#   mutate(chng_private = coalesce(chng_private, chng_prod)) %>%
#   filter(!is.na(chng_private)) %>%
#   mutate(value = 100 * cumprod(chng_private)) %>%
#   select(date, value) %>%
#   mutate(aggregate_weekly_hours = log(scale(value))) %>%
#   mutate(aggregate_weekly_hours_3m = n_month_growth_ann(value, m_chng)) %>%
#   mutate(aggregate_weekly_hours_3m = scale(aggregate_weekly_hours_3m))
# 
# 
# cc_claims = fred_request_data("CCSA", agg = "avg", freq = "m") %>%
#   left_join(fred_request_data("CLF16OV"), by = "date") %>%
#   mutate(ccsa = value.x / value.y) 
  # mutate(ccsa = ccsa)


  # mutate(icsa = scale(icsa))


# insured_unemployment_rate = fred_request_data("IURSA", agg = "avg", freq = "m") %>%
#   mutate(insured_unrate = log(scale(value)))
# mutate(insured_unrate = scale(insured_unrate - lag(insured_unrate, 6)))
# 
# fed_survey_emp = phil_man %>%
#   left_join(texas_man, by = 'date') %>%
#   left_join(texas_svc, by = 'date') %>%
#   left_join(texas_retail, by = 'date') %>%
#   left_join(kansas_svc, by = 'date') %>%
#   left_join(kansas_man, by = 'date') %>%
#   left_join(ny_svc, by = 'date') %>%
#   left_join(ny_man, by = 'date') %>%
#   left_join(phil_svc, by = 'date') %>%
#   left_join(richmond_man, by = 'date') %>%
#   left_join(richmond_svc, by = 'date') %>%
#   mutate(date = as.Date(date)) %>%
#   select(contains("date") | contains("employment")) %>%
#   create_z_scores_for_df(.) %>%
#   filter(date >= "1994-04-01") %>%
#   rename(fed_surveys_emp = mean.z)
# 
# manu_overtime_hours = fred_request_data("AWOTMAN") %>%
#   mutate(manu_overtime_hours = value)
# 
# jolts = fred_request_data("JTSJOL") %>%
#   mutate(date = shift_date_series(date, yr = 0, m = 1))

dataset2 = temp_help %>%
  left_join(unemployment_rate, by = "date") %>%
  left_join(broad_unemployment_rate, by = "date") %>%
  left_join(unemployed_to_employed_flows, by = "date") %>%
  left_join(jolts_quits, by = "date") %>%
  left_join(emp_to_pop_ratio, by = "date") %>%
  left_join(partime_emp_econ_reasons, by = "date") %>%
  left_join(job_leavers_percent, by = "date") %>%
  left_join(unemployed_27_weeks, by = "date") %>%
  left_join(nfib_jobs_opening, by = "date") %>%
  left_join(job_losers_percent, by = "date") %>%
  left_join(jolt_hires, by = "date") %>%
  left_join(nfib_emp_plans, by = "date") %>%
  left_join(average_hourly_earnings_private, by = "date") %>%
  left_join(ic_claims, by = "date") %>%
  left_join(employment_private, by = "date") %>%
  left_join(aggregate_hours_private, by = "date") %>%
  left_join(labor_participation_rate, by = "date") %>%
  left_join(ism_employment, by = "date") %>%
  select(date,
         temp_help_3m,
         u3_rate, 
         # u6_rate,
         unemployed_employed_flows,
         # quits,
         emp_to_pop_ratio,
         partime_econ_reasons,
         job_leavers,
         unemployed_27_weeks,
         nfib_jobs_opening,
         job_losers,
         # hires,
         nfib_plans,
         avg_hourly_earnings_3m,
         icsa,
         employment_private_3m,
         aggregate_hours_private_3m,
         # participation_rate,
         ism_emp) %>%
  filter(date >= "1992-01-01") %>%
  fill(names(.), .direction = "down")

pca_rotated = psych::principal(dataset2[, -1], rotate = "varimax", nfactors = 2, scores = TRUE)
pca_rotated = psych::principal(dataset2[, -1], rotate = "varimax", nfactors = 2, scores = TRUE)

dataset_rotated = dataset2 %>%
  select("date") %>%
  bind_cols(pca_rotated$scores %>% as.data.frame() %>% round(3)) %>% 
  arrange(date) %>%
  rename("activity" = "RC1") %>%
  rename("momentum" = "RC2")

# pca_model_2 <- princomp(dataset2[, -1], cor = TRUE)
# pca_summary_2 <- summary(pca_model_2, loadings = TRUE, scores = TRUE, cutoff = 0)
# 
# dataset2 = dataset2 %>% dplyr::select("date") %>% 
#   bind_cols(pca_summary_2$scores %>% as.data.frame() %>% round(3)) %>% 
#   arrange(date) %>%
#   rename("activity" = "Comp.1") %>%
#   rename("momentum" = "Comp.2") %>%
#   mutate(activity = as.vector(scale(activity))) %>%
#   mutate(chng_12m = activity - lag(activity, 12),
#          chng_6m = activity - lag(activity, 6),
#          chng_3m = activity - lag(activity, 3))
# 
# labor_market_activity_plot_2 = plot_ly(dataset2, x=~date, y=~activity, type = "scatter", mode = "lines")
# 
# labor_market_activity_plot_3 = plot_ly(dataset2, x=~date, y=~momentum, type = "scatter", mode = "lines")
# 

plot_ly(dataset_rotated, x=~date, y=~-activity, type = "scatter", mode = "lines")
plot_ly(dataset_rotated, x=~date, y=~momentum, type = "scatter", mode = "lines")

