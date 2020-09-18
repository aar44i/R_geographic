library(dplyr)
data(quakes)

quakes_gr = group_by(quakes, mag)
quakes_sum = summarise(quakes_gr, freq = n())

quakes %>% # определяем самое глубокое землятрясение
  group_by(mag) %>%
  arrange(desc(depth)) %>%
  filter(row_number() == 1)

quakes %>% # определяем колличество землятрясений
  group_by(mag) %>%
  summarise(freq = n())