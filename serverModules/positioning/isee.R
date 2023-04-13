
download_isee_data <- function() {

}

isee_org = read.csv("data/ISEE/ISEE-Index.csv")

isee_org$ALL.SECURITIES[4081] = 110
isee_org$X10.AVG = SMA(isee_org$ALL.SECURITIES, 10)
isee_org$X20.AVG = SMA(isee_org$ALL.SECURITIES, 20)

isee_index_10 = isee_org %>%
  select(date = DATE, avg_10_all = X10.AVG) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  filter(date >= "2006-01-17") %>%
  mutate(max_isee = rollapplyr(avg_10_all, 126, max, partial = T),
         min_isee = rollapplyr(avg_10_all, 126, min, partial = T)) %>%
  mutate(index = (avg_10_all - min_isee) / (max_isee - min_isee) * 100)

isee_index_20 = isee_org %>%
  select(date = DATE, avg_10_all = X20.AVG) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  filter(date >= "2006-01-17") %>%
  mutate(max_isee = rollapplyr(avg_10_all, 126, max, partial = T),
         min_isee = rollapplyr(avg_10_all, 126, min, partial = T)) %>%
  mutate(index = (avg_10_all - min_isee) / (max_isee - min_isee) * 100)


isee_index_10_plot = plot_ly(isee_index_10, x = ~date, y = ~index, type = "scatter", mode = "lines") %>%
  layout(title = "ISEE 10 index")

isee_index_20_plot = plot_ly(isee_index_20, x = ~date, y = ~index, type = "scatter", mode = "lines") %>%
  layout(title = "ISEE 20 index")

isee_index_10_plot
isee_index_20_plot
