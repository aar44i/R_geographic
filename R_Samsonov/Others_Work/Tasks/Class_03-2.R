library(tidyverse)
data(storms, package = 'dplyr')


(storms2 = storms %>% 
  mutate(Date = as.Date(paste(year, month, day, sep = '/'))) %>% 
  group_by(name, year) %>% 
  arrange(Date) %>% # не обязательно
  summarise(date_begin = min(Date),
            date_end = max(Date),
            duration = date_end - date_begin,
            max_wind = max(wind),
            min_pres = min(pressure)))
