# cof_data = read_excel("data/Bank Delinquency/cof.xlsx")

cof_data_clean = as.data.frame(t(cof_data)) %>%
  slice(-1) %>%
  mutate(date = as.Date(as.numeric(V1), origin = "1899-12-30"), 
         type = V2,
         type2 = coalesce(V3, V4),
         card = as.numeric(V5),
         auto = as.numeric(V6)) %>%
  select(!contains("V")) 

cof_card_delinquency = cof_data_clean %>%
  select(-auto) %>%
  filter(grepl("Performing Delinquencies", type)) %>%
  filter(grepl('Rate', type2))

cof_card_charge_offs = cof_data_clean %>%
  select(-auto) %>%
  filter(grepl("Offs", type)) %>%
  filter(grepl('Rate', type2))


cof_auto_delinquency = cof_data_clean %>%
  select(-card) %>%
  filter(grepl("Performing Delinquencies", type)) %>%
  filter(grepl('Rate', type2))

cof_auto_charge_offs = cof_data_clean %>%
  select(-card) %>%
  filter(grepl("Offs", type)) %>%
  filter(grepl('Rate', type2))

plot = plot_ly(cof_card_delinquency, x =~date, y=~card*100, mode = "lines")
plot

plot1 = plot_ly(cof_card_charge_offs, x =~date, y=~card*100, mode = "lines")
plot1

plot2 = plot_ly(cof_auto_delinquency, x =~date, y=~auto, mode = "lines")
plot2

plot3 = plot_ly(cof_auto_charge_offs, x =~date, y=~auto, mode = "lines")
plot3
