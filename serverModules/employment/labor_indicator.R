m_chng = 6

temp_help = fred_request_data("TEMPHELPS") %>%
  mutate(temp_help_3m = n_month_growth_ann(value, m_chng)) %>%
  mutate(temp_help_3m = scale(temp_help_3m))

residential = fred_request_data("CES2023610001") %>%
  mutate(residential_3m = n_month_growth_ann(value, m_chng)) %>%
  mutate(residential_3m = scale(residential_3m))

manufacturing = fred_request_data("MANEMP") %>%
  mutate(manu_3m = n_month_growth_ann(value, m_chng)) %>%
  mutate(manu_3m = scale(manu_3m))


job_losers_permanent = fred_request_data("LNS13026638") %>%
  mutate(job_losers_3m = n_month_growth_ann(value, m_chng)) %>%
  mutate(job_losers_3m = scale(job_losers_3m))

partime_emp_econ_reasons = fred_request_data("LNS12032194") %>%
  mutate(partime_econ_3m = n_month_growth_ann(value, m_chng)) %>%
  mutate(partime_econ_3m = scale(partime_econ_3m))

aggregate_weekly_hours = fred_request_data("AWHI") %>%
  mutate(chng_prod = value / lag(value, 1)) %>%
  full_join(fred_request_data("AWHAE"), by = "date") %>%
  mutate(chng_private= value.y / lag(value.y, 1)) %>%
  mutate(chng_private = coalesce(chng_private, chng_prod)) %>%
  filter(!is.na(chng_private)) %>%
  mutate(value = 100 * cumprod(chng_private)) %>%
  select(date, value) %>%
  mutate(aggregate_weekly_hours_3m = n_month_growth_ann(value, m_chng)) %>%
  mutate(aggregate_weekly_hours_3m = scale(aggregate_weekly_hours_3m))


cc_claims = fred_request_data("CCSA", agg = "avg", freq = "m") %>%
  left_join(fred_request_data("CLF16OV"), by = "date") %>%
  mutate(ccsa = value.x / value.y) %>%
  mutate(ccsa = scale(ccsa))

ic_claims = fred_request_data("ICSA", agg = "avg", freq = "m") %>%
  left_join(fred_request_data("CLF16OV"), by = "date") %>%
  mutate(icsa = value.x / value.y) %>%
  mutate(icsa = scale(icsa))

unemployment_rate = fred_request_data("UNRATE") %>%
  mutate(u3_rate = scale(value))

broad_unemployment_rate = fred_request_data("U6RATE") %>%
  mutate(u6_rate = scale(value))

nfib_emp_plans = read_excel("data/NFIB/plans_increase_employment.xlsx") %>%
  mutate(date = as.Date(`Month/Year`)) %>%
  mutate(nfib_plans = as.numeric(`Plans to Increase Employment`)) %>%
  add_row(date = as.Date("2023-03-01"), nfib_plans = 15) %>%
  mutate(nfib_plans = scale(nfib_plans))

nfib_jobs_opening = read_excel("data/NFIB/with_vacant_position.xlsx") %>%
  mutate(date = as.Date(`Month/Year`)) %>%
  mutate(nfib_jobs_opening = as.numeric(`Current Job Openings`)) %>%
  add_row(date = as.Date("2023-03-01"), nfib_jobs_opening = 43) %>%
  mutate(nfib_jobs_opening = scale(nfib_jobs_opening))
  
unemployed_to_employed_flows = fred_request_data("LNS17100000") %>%
  left_join(fred_request_data("UNEMPLOY"), by = "date") %>%
  mutate(unemployed_employed_flows = SMA(value.x / value.y, 3))

insured_unemployment_rate = fred_request_data("IURSA", agg = "eop", freq = "m") %>%
  mutate(insured_unrate = value)
  # mutate(insured_unrate = scale(insured_unrate - lag(insured_unrate, 6)))

fed_survey_emp = phil_man %>%
  left_join(texas_man, by = 'date') %>%
  left_join(texas_svc, by = 'date') %>%
  left_join(texas_retail, by = 'date') %>%
  left_join(kansas_svc, by = 'date') %>%
  left_join(kansas_man, by = 'date') %>%
  left_join(ny_svc, by = 'date') %>%
  left_join(ny_man, by = 'date') %>%
  left_join(phil_svc, by = 'date') %>%
  left_join(richmond_man, by = 'date') %>%
  left_join(richmond_svc, by = 'date') %>%
  mutate(date = as.Date(date)) %>%
  select(contains("date") | starts_with("employment")) %>%
  create_z_scores_for_df(.) %>%
  filter(date >= "1994-04-01") %>%
  rename(fed_surveys_emp = mean.z)

jolts = fred_request_data("JTSJOL") %>%
  mutate(date = shift_date_series(date, yr = 0, m = 1))

dataset = temp_help %>%
  left_join(residential, by = "date") %>%
  left_join(manufacturing, by = "date") %>%
  left_join(job_losers_permanent, by = "date") %>%
  left_join(partime_emp_econ_reasons, by = "date") %>%
  left_join(aggregate_weekly_hours, by = "date") %>%
  left_join(cc_claims, by = "date") %>%
  left_join(ic_claims, by = "date") %>%
  left_join(unemployment_rate, by = "date") %>%
  left_join(broad_unemployment_rate, by = "date") %>%
  left_join(nfib_emp_plans, by = "date") %>%
  left_join(nfib_jobs_opening, by = "date") %>%
  left_join(unemployed_to_employed_flows, by = "date") %>%
  left_join(insured_unemployment_rate, by = "date") %>%
  left_join(fed_survey_emp, by = "date") %>%
  select(date,
         temp_help_3m,
         residential_3m,
         manu_3m,
         job_losers_3m,
         partime_econ_3m,
         aggregate_weekly_hours_3m,
         ccsa, icsa,
         u3_rate, u6_rate,
         nfib_plans, nfib_jobs_opening,
         unemployed_employed_flows,
         insured_unrate,
         fed_surveys_emp) %>%
  filter(date >= "1994-07-01") %>%
  drop_na()


pca_model <- princomp(dataset[, -1], cor = TRUE)
pca_summary <- summary(pca_model, loadings = TRUE, scores = TRUE, cutoff = 0)

dataset = dataset %>% dplyr::select("date") %>% 
  bind_cols(pca_summary$scores %>% as.data.frame() %>% round(3)) %>% 
  arrange(date) %>%
  rename("activity" = "Comp.1") %>%
  mutate(chng_12m = activity - lag(activity, 12),
         chng_6m = activity - lag(activity, 6),
         chng_3m = activity - lag(activity, 3))

labor_market_activity_plot = plot_ly(dataset, x=~date, y=~activity, type = "scatter", mode = "lines") %>%
  layout(title = "Labor market index. PCA from 15 components")

# plot1.1 = plot_ly(dataset, x=~date, y=~chng_12m, type = "scatter", mode = "lines") %>%
#   add_trace(y=~chng_6m, name = "6m") %>%
#   add_trace(y=~chng_3m, name = "3m")
# 
# plot1.1

# plot2 = plot_ly(dataset, x=~date, y=~`Comp.2`, type = "scatter", mode = "lines")
# plot2

# plot2 = plot_ly(dataset, x=~date, y=~chng_12m, type = "scatter", mode = "lines")
# plot2
# 
# plot3 = plot_ly(dataset, x=~date, y=~chng_6m, type = "scatter", mode = "lines")
# plot3

