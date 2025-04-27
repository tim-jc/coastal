library(tidyverse)

# clear the memory
rm(list=ls(all = TRUE))

setwd("~/Documents/Coding/R/Strava/coastal/lindisfarne_tides/")

# Load
apr <- read_tsv("apr_25.tsv", trim_ws = T) %>% mutate(mth = "Apr25")
may <- read_tsv("may_25.tsv", trim_ws = T) %>% mutate(mth = "May25")
jun <- read_tsv("jun_25.tsv", trim_ws = T) %>% mutate(mth = "Jun25")
jul <- read_tsv("jul_25.tsv", trim_ws = T) %>% mutate(mth = "Jul25")
aug <- read_tsv("aug_25.tsv", trim_ws = T) %>% mutate(mth = "Aug25")
sep <- read_tsv("sep_25.tsv", trim_ws = T) %>% mutate(mth = "Sep25")


# Clean
tide_times <- bind_rows(apr, may, jun, jul, aug, sep) %>% 
  mutate(date = str_c(str_extract(Date, "\\d{1,2}"),mth),
         date = dmy(date)) %>% 
  select(-c(Date, Day, mth)) %>% 
  pivot_longer(cols = -date, names_to = "window_status") %>% 
  mutate(window_id = row_number(),
         window_status = str_remove(window_status, "\\s.*"),
         value = str_split(value, "\\suntil\\s"),
         from = map_vec(value,1),
         until = map_vec(value,2)) %>% 
  select(-value) %>% 
  pivot_longer(c(from, until)) %>% 
  mutate(time_val = str_extract(value, "\\d{2}:\\d{2}"),
         add_a_day = str_detect(value, "\\(\\w{3}\\)"),
         adjusted_date = if_else(add_a_day, date + days(1), date),
         datetime = as_datetime(str_c(adjusted_date," ",time_val,":00")),
         rising_tide = window_status == "Safe" & name == "until" | window_status == "Unsafe" & name == "from",
         datetime = if_else(rising_tide, datetime - hours(1), datetime)) %>% 
  select(window_id, window_status, name, datetime) %>% 
  pivot_wider(values_from = datetime) %>% 
  select(-window_id) %>% 
  distinct() # some crossing windows are repeated


# Create tibble of evening / morning crossing time pairs
date_start <- min(tide_times$from) %>% floor_date("day") %>% as_date()
date_end <- max(tide_times$until) %>% floor_date("day") %>% as_date()

time_pairs <- tibble(date = seq.Date(date_start, date_end, by = "day")) %>% 
  mutate(pair_id = row_number(),
         evening_target = as_datetime(str_c(date, " 18:00:00")),
         morning_target = as_datetime(str_c(date + days(1), " 08:00:00"))) %>% 
  pivot_longer(matches("target$"))


# Find date options
date_options <- tide_times %>% 
  filter(window_status == "Safe") %>% 
  left_join(time_pairs, by = join_by(from <= value, until >= value))

workable_crossings <- date_options %>% count(pair_id) %>% filter(n == 2) %>% pull(pair_id)

date_options <- date_options %>% 
  filter(pair_id %in% workable_crossings)

write_csv(date_options, "lindisfarne_tide_options.csv")

