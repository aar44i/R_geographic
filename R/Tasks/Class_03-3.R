library(tidyverse)

(tab =  read_table('https://raw.githubusercontent.com/tsamsonov/r-geo-course/master/data/wind_energy.txt',
                  col_names = c('id', 'lat', 'lon', '50', '100')))

(tab_tidy = tab %>% 
  pivot_longer(-id:-lon, names_to = 'height', values_to = 'energy'))

tab_tidy %>% 
  group_by(height) %>% 
  summarize(mean_energy = mean(energy))

