library(tidyverse)
library(readxl)
library(RColorBrewer)

getwd()
temp = read.table('graph.txt', skip = 5)
colnames(temp) = c('year', 'no_smooth', 'lowess')




# Среднегодовая аномалия температуры
trees = read.csv('fao_treecover_extent__ha.csv')
plot(temp$year, temp$no_smooth, 
     main = list('Аномалия температуры по отношению к средней\n за период 1951-1980 (по данным NASA, 2020 г.)', cex = 0.8),
     xlab = 'Год',
     ylab = '°С',
     col = 'grey',
     type = 'l')                  
grid()
points(temp$year, temp$no_smooth,
       pch = 20,
       col = 'grey') 
abline(h = 0, col = 'red') 
lines(temp$year, temp$lowess, col = 'blue') 
legend('topleft', c('Скользяшее среднее', 'Среднегодовая температура'),
       col = c('blue', 'grey'),
       lwd = c(1, 1),
       text.width = strwidth(c('Скользяшее среднее', 'Среднегодовая температура'))[1]/1.8,
       pch = c(NA, 20),
       cex = 0.7,
       pt.cex = 1.3) 

#  Среднегодовая аномалия температуры
trees = trees %>% 
  select(c(-1, -2, -6, -7)) %>%
  sapply(sum, na.rm = T) %>%
  as_tibble() %>%
  mutate(name = c('Высаженный', 'Первичный', 'Восстановленный'), .before = 1)

par(mar = c(2, 2, 2, 2))
names <- paste(trees$name, ' (', round((100* trees$value / sum(trees$value)), 0), '%)', sep = '')
pie(trees$value, names,
    clockwise = T,
    main = "Структура лесного покрова Земли\n (по данным ФАО, 2015 г.)",
    col = c('olivedrab1', 'forestgreen', 'palegreen3'))

#  Содержание органического углерода в верхнем слое почвы (0-30 см)

soil = read_xlsx('PgC.xlsx', skip = 1)[3:14,] %>% 
   rename(name = '...1', Tier = 'Tier 1')

# soil = tibble(soil$name, as.data.frame(lapply(soil[, 2:6], as.numeric))) %>% 
#   rename(name = soil$name)
# par(mar = c(5, 10, 4, 2))
# barplot(soil$Topsoil, names.arg = soil$name, 
#         horiz = T,
#         las = 1,
#         main = 'Содержание органического углерода в верхнем слое почвы 
#         (0-30 см)\n по климатическим регионам IPCC (2010 г.)',
#         xlab = 'PgC',
#         col = rainbow(length(soil$name))) 






