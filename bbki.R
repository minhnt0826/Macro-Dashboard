library(readxl)
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)
library(seasonal)
library(zoo)
source("fred.R")


bbki_coincident = fred_request_data("BBKMCOIX")

bbki_coincident = bbki_coincident %>%
  select(date, value) %>%
  mutate(change = value - lag(value, 1))