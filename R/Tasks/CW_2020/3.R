install.packages('tidyverse')
install.packages('writexl')
# # ctrl + shift + M  =  %>% %>%
as_tibble(starwars)


tab1 = data.frame(type = c('trees', 'buses', 'grass'),
                  id = c(1,5,12))


tab2 = tibble(type = c('trees', 'buses', 'grass'),
                  id = c(1,5,12))
