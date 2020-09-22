library(readxl)
library(tidyverse)
library(writexl)
emissions = read_excel('emissions.xlsx', 1,skip = 1, col_types = c("text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) # счет данных с первого листа фаила
capture = read_excel('emissions.xlsx', 2, skip = 1, col_types = c("text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) # счет данных со второго листа фаила

colnames(emissions) = c( "Region", "y2005", "y2010", "y2011", "y2012", "y2013", "y2014", "y2015", "y2016") # переименование столбцов
colnames(capture) = c( "Region", "y2005", "y2010", "y2011", "y2012", "y2013", "y2014", "y2015", "y2016")
  str(emissions) # провер€ем типы данных 
  str(capture)
Flt_e = stringr::str_detect(emissions$Region, '‘едераци€|федеральный округ') # ищем округа
Flt_c = stringr::str_detect(capture$Region, '‘едераци€|федеральный округ') 

(Flt_emissions = emissions %>% # ѕервый конвейер манипул€ций дл€ выбросов
  mutate(okrug = if_else(Flt_e, Region, NULL)) %>% 
  tidyr::fill(okrug) %>% 
  filter(!Flt_e) %>% 
  pivot_longer(cols = y2005:y2016, 
               names_to = 'year', 
               names_prefix = 'y',
               names_ptypes = list(year = integer()),
               values_to = 'value'))

(Flt_capture = capture %>% #  ¬торой конвейер манипул€ций дл€ улавоивани€
    mutate(okrug = if_else(Flt_c, Region, NULL)) %>% 
    tidyr::fill(okrug) %>% 
    filter(!Flt_c) %>% 
    pivot_longer(cols = y2005:y2016, 
                 names_to = 'year', 
                 names_prefix = 'y',
                 names_ptypes = list(year = integer()),
                 values_to = 'value'))

  (Polluters = Flt_emissions %>% # обьединение таблиц по загра€знению и улавливанию в одну и нахождение между ними разницы
      inner_join(Flt_capture, by = c("Region" = "Region", "year" = "year")) %>% 
      transmute(Region = Region,
                okrug = okrug.x,
                year = year,
                emissions = value.x,
                capture = value.y, delta = capture - emissions) %>% 
      
      group_by(okrug, year) %>% # нахождение субьекта с наихудшим соотношением выбросов и улавливани€
      arrange((delta)) %>% 
      filter(row_number() == 1))
  write_xlsx(Polluters, "polluters.xlsx") # вывод фрейма в фаил 
   
