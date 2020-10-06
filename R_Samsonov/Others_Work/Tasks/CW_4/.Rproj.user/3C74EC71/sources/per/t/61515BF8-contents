library(readxl)
library(tidyverse)
wind = read_excel('wind.xlsx')
#View(wind)

main_dir = wind %>% 
  rename(dir = 1) %>%
  filter(dir != 'штиль') %>%
  select(-dir) %>%
  sapply(function(x){
    which.max(x)
  })
tibble(dir = colnames(wind)[-1],
       main_dir = wind[main_dir, 'направл.'])