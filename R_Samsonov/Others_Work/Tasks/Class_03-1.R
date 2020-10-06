library(tidyverse)
data(quakes, package = 'datasets')
# ѕодсчет количества землетр€сений
# с заданной магнитудой
(nquakes = quakes %>% 
  group_by(mag) %>% 
  summarise(ncases = n())) # количесвто строк в группе

summarise(group_by(quakes, mag), ncases = n())

grouped = group_by(quakes, mag)
result = summarise(grouped, 
                   ncases = n())
n