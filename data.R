library(dplyr)
library(DT)
library(htmlwidgets)
library(leaflet)
library(lubridate)
library(plotly)
library(purrr)
library(readr)
library(RColorBrewer)
library(scales)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyr)
library(tidyverse)
library(widgetframe)

load("COVID.RData")
root = "https://raw.githubusercontent.com/"
repo = "CSSEGISandData/COVID-19"
folder = "/master/csse_covid_19_data/csse_covid_19_time_series/"
url_data_folder = str_c(root, repo, folder, sep="")

url_confirmed_US = str_c(url_data_folder, "time_series_covid19_confirmed_US.csv", sep="")
url_confirmed_global = str_c(url_data_folder, "time_series_covid19_confirmed_global.csv", sep="")
url_death_US = str_c(url_data_folder, "time_series_covid19_deaths_US.csv", sep="")
url_death_global = str_c(url_data_folder, "time_series_covid19_deaths_global.csv", sep="")

confirmed_us_df = read_csv(url_confirmed_US, col_select = c(6:10, "10/18/21":format(Sys.Date() - 1, format = "%m/%d/%y"))) %>%
  rename(sub_region = Province_State, region = Country_Region) %>%
  gather("10/18/21":format(Sys.Date() - 1, format = "%m/%d/%y"), key = "date", value = "confirmed")
 
confirmed_state_df = confirmed_us_df %>%
  summarise(county = Admin2, state = sub_region, Lat, Long = Long_, date = mdy(date), confirmed)

confirmed_us_df = confirmed_us_df %>%
  group_by(region, sub_region, date) %>%
  summarise(Lat = mean(Lat), Long = mean(Long_), confirmed = sum(confirmed)) %>%
  mutate(date = mdy(date))

death_us_df = read_csv(url_death_US, col_select = c(6:10, "10/18/21":format(Sys.Date() - 1, format = "%m/%d/%y"))) %>%
  rename(sub_region = Province_State, region = Country_Region) %>%
  gather("10/18/21":format(Sys.Date() - 1, format = "%m/%d/%y"), key = "date", value = "death")

death_state_df = death_us_df %>%
  summarise(county = Admin2, state = sub_region, Lat, Long = Long_, date = mdy(date), death)

death_us_df = death_us_df %>%
  group_by(region, sub_region, date) %>%
  summarise(Lat = mean(Lat), Long = mean(Long_), death = sum(death)) %>%
  mutate(date = mdy(date))

df_state_update = left_join(confirmed_state_df, death_state_df)

df_us_islands = df_state_update %>% filter(state == "Guam" | state == "Northern Mariana Islands") %>% summarise(county, state, Lat, Long = Long - 360, date, confirmed, death)
df_state_update = df_state_update[!(df_state_update$Lat == 0 & df_state_update$Long == 0), ]
df_state_update = df_state_update[!(grepl("Unassigned", df_state_update$county)), ]
df_state_update = df_state_update[!(df_state_update$state == "Guam" | df_state_update$state == "Northern Mariana Islands"),] %>%
  full_join(df_us_islands)

df_state = full_join(df_state, df_state_update)

confirmed_global_df = read_csv(url_confirmed_global, col_select = c(1:4, "10/18/21":format(Sys.Date() - 1, format = "%m/%d/%y"))) %>%
  rename(sub_region = "Province/State", region = "Country/Region") %>%
  gather("10/18/21":format(Sys.Date() - 1, format = "%m/%d/%y"), key = "date", value = "confirmed") %>%
  mutate(date = mdy(date))

death_global_df = read_csv(url_death_global, col_select = c(1:4, "10/18/21":format(Sys.Date() - 1, format = "%m/%d/%y"))) %>%
  rename(sub_region = "Province/State", region = "Country/Region") %>%
  gather("10/18/21":format(Sys.Date() - 1, format = "%m/%d/%y"), key = "date", value = "death") %>%
  mutate(date = mdy(date))

confirmed_df = bind_rows(confirmed_us_df, confirmed_global_df)
death_df = bind_rows(death_us_df, death_global_df)

df_update = left_join(confirmed_df, death_df) %>%
  mutate("Fatality Rate" = percent(death / confirmed, accuracy = .01))

df = full_join(df, df_update)

df_aus <- df_update %>%
  filter(region == "Australia") %>%
  group_by(date) %>%
  summarise(region, sub_region = NA, Lat = mean(na.omit(Lat)), Long = mean(na.omit(Long)), confirmed = sum(confirmed), death = sum(death)) %>% distinct()

df_can <- df_update %>%
  filter(region == "Canada") %>%
  group_by(date) %>%
  summarise(region, sub_region = NA, Lat = mean(na.omit(Lat)), Long = mean(na.omit(Long)), confirmed = sum(confirmed), death = sum(death)) %>% distinct()

df_chn <- df_update %>%
  filter(region == "China") %>%
  group_by(date) %>%
  summarise(region, sub_region = NA, Lat = mean(na.omit(Lat)), Long = mean(na.omit(Long)), confirmed = sum(confirmed), death = sum(death)) %>% distinct()

df_no_subregions_update <- 
  df_update[!(df_update$region == "Australia" | df_update$region == "China" | df_update$region == "Canada" | (df_update$region == "US" & !is.na(df_update$sub_region))),] %>%
  full_join(df_aus) %>% full_join(df_can) %>% full_join(df_chn) %>%
  mutate("Fatality Rate" = percent(death / confirmed, accuracy = .01))

df_no_subregions = full_join(df_no_subregions, df_no_subregions_update)