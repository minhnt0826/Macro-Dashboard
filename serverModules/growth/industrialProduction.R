ip_manufacturing_capacity = fred_request_data("CUMFNS") %>%
  create_growth_data_for_df(., .$value) %>%
  mutate(value = value / lag(value, 12)) %>%
  filter(date >= "1960-01-01")

ip_manufacturing = fred_request_data("IPMANSICS") %>%
  create_growth_data_for_df(., .$value) %>%
  mutate(value = value / lag(value, 12)) %>%
  filter(date >= "1960-01-01")

ip_durable_goods = fred_request_data("IPDCONGD") %>%
  create_growth_data_for_df(., .$value) %>%
  mutate(value = value - lag(value, 12)) %>%
  filter(date >= "1960-01-01")

ip_construction_supplies = fred_request_data("IPB54100S") %>%
  create_growth_data_for_df(., .$value) %>%
  mutate(value = value - lag(value, 12)) %>%
  filter(date >= "1960-01-01")

ip_business_equipment = fred_request_data("IPBUSEQ") %>%
  create_growth_data_for_df(., .$value) %>%
  mutate(value = value - lag(value, 12)) %>%
  filter(date >= "1960-01-01")


ip_manufacturing_capacity_plot = create_plotly_plot_with_growth_data(ip_manufacturing_capacity)
ip_manufacturing_plot = create_plotly_plot_with_growth_data(ip_manufacturing)
ip_durable_goods_plot = create_plotly_plot_with_growth_data(ip_durable_goods)
ip_construction_supplies_plot = create_plotly_plot_with_growth_data(ip_construction_supplies)
ip_business_equipment_plot = create_plotly_plot_with_growth_data(ip_business_equipment)

ip_cyclical_index = ip_manufacturing %>%
  left_join(ip_manufacturing, by = "date") %>%
  left_join(ip_durable_goods, by = "date") %>%
  left_join(ip_construction_supplies, by = "date") %>%
  left_join(ip_business_equipment, by = "date") %>%
  select(contains("date") | contains("value"))

pca_model <- princomp(ip_cyclical_index[, -1], cor = TRUE)
pca_summary <- summary(pca_model, loadings = TRUE, scores = TRUE, cutoff = 0)

ip_cyclical_index = ip_cyclical_index %>% dplyr::select("date") %>% 
  bind_cols(pca_summary$scores %>% as.data.frame() %>% round(3)) %>% 
  arrange(date) %>%
  rename("activity" = "Comp.1") %>%
  mutate(chng_12m = activity - lag(activity, 12),
         chng_6m = activity - lag(activity, 6),
         chng_3m = activity - lag(activity, 3))

plot = plot_ly(ip_cyclical_index, x=~date, y=~activity, type = "scatter", mode = "lines")
plot

plot1.1 = plot_ly(ip_cyclical_index, x=~date, y=~chng_12m, type = "scatter", mode = "lines") %>%
  add_trace(y=~chng_6m, name = "6m") %>%
  add_trace(y=~chng_3m, name = "3m")

plot1.1
