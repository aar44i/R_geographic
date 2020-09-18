library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(eurostat)

tables = c('tag00115')

trades = lapply(tables, function(X) { 
  get_eurostat(X) %>% label_eurostat()
}) %>% # подготовка данных 
  bind_rows() %>%  
  select(-strucpro)%>% 
  filter(stringr::str_detect(crops, 'Carrots|Tomatoes|Cucumbers|Onions|Carrots')) %>% 
  filter(!stringr::str_detect(geo, 'European Union')) %>% 
  mutate(geo = as.factor(geo))

  trades %>% # построение первого графика 
  dplyr::filter(time == as.Date('2010-01-01')) %>% 
  ggplot(mapping = aes(x = geo, y = values, fill = crops)) +
    geom_col(color = 'black', size = 0.1) +
    coord_flip() + ggtitle('Harvested area of tomatoes, cucumbers, carrot and onions in Europe') +
    xlab('Country') + ylab('Thausand ga') 
 
  trades_flt = trades %>% # подготовка данных для второго графика
  dplyr::filter(time == as.Date('2010-01-01')) %>%
  filter(stringr::str_detect(geo, 'Spain|Italy|Poland|Romania')) %>%
  arrange(geo)
  
  trades_group = trades_flt %>% # таблица со значениями общей занятой площади дл каждой страны
  group_by(geo) %>% 
  summarise(total = sum(values)) 
  
  trades_flt = trades_flt %>% 
  inner_join(trades_group, by = c("geo" = "geo")) %>% 
  mutate(percent = round(c(values / total * 100), 2))

  ggplot(trades_flt, mapping = aes(x = total, y = percent, fill = crops, width = total * 2)) + # построение второго графика 
    geom_col(color = 'black', size = 0.5) + 
    ggtitle("Harvested area of tomatoes, cucumbers, carrots and onions", subtitle = "2010") +
    xlab("Thousand ha") +
    ylab(NULL) +  
    coord_polar(theta = 'y') + facet_wrap(~geo) 

  trades_flt2 = trades %>% # подготовка данных дл третьего графика
  filter(stringr::str_detect(geo, 'Spain|Italy|Poland|Romania')) %>%
  filter(stringr::str_detect(crops, 'Tomatoes')) 
  ggplot(trades_flt2, mapping = aes(x = time, y = values, color = geo)) + # построение третьего графика
    geom_line(size = 1) + geom_point(size = 2) + ggtitle('Harvested area of tomatoes') +
    xlab('Year') + ylab('Thausand ha') + labs(color = 'Country')
  
  