pce = fred_request_data("PCE") %>%
  filter((date >= "2018-12-01" & date <= "2019-03-31") | (date >= "2021-12-01" & date <= "2022-03-31")) %>%
  mutate(yoy = value / lag(value, 4))
