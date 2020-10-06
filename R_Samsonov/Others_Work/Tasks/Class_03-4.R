library(tidyverse)
okruga = read_csv('https://raw.githubusercontent.com/tsamsonov/r-geo-course/master/data/okruga.csv',
                  locale = locale(encoding = 'UTF-8'))

(okruga_tidy = okruga %>%
  gather(year, pop,`2005`:`2013`) %>%
  group_by(year) %>%
  mutate(ratio = pop / sum(pop)))