library(readxl)
library(tidyverse)
library(writexl)
setwd('C:/Users/gorba/DataSciense/R_geographic/R/HW_2020')
emissions = read_excel('emissions.xlsx', 1,skip = 1, 
                       col_types = c('text', rep('numeric', 8)))
colnames(emissions)[1] = "Region"
capture = read_excel('emissions.xlsx', 2, skip = 1,
                     col_types = c('text', rep('numeric', 8)))
colnames(capture)[1] = "Region"

emissions = emissions %>% # Первый конвейер манипуляций для выбросов
  mutate(okrug = if_else(str_detect(emissions$Region, 'федеральный округ'), Region, NULL)) %>% 
  fill(okrug) %>% 
  filter(!str_detect(emissions$Region, 'Федерация|федеральный округ'))  %>% 
  pivot_longer(cols = 2:9, names_to = "year", values_to = "value")
  
  
capture = capture %>% # Второй конвейер манипуляций для улавоивания
  mutate(okrug = if_else(str_detect(capture$Region, 'федеральный округ'), Region, NULL)) %>% 
  fill(okrug) %>% 
  filter(!str_detect(capture$Region, 'Федерация|федеральный округ'))  %>% 
  pivot_longer(cols = 2:9, names_to = "year", values_to = "value")
  
# присоединение таблиц
polluters = inner_join(emissions, capture, by = c("Region" = "Region", 
                                                  "year" = "year", "okrug" = "okrug")) %>% 
# вычисление разницы
  transmute(region = Region, okrug = okrug, year = year, emissions = value.x,
            capture = value.y, delta = capture - emissions) %>% 

  group_by(okrug, year) %>% # нахождение субьекта с наихудшим соотношением выбросов и улавливания
  arrange(delta) %>% 
  filter(row_number() == 1)
write_xlsx(polluters, "polluters.xlsx")
