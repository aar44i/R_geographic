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

soil <- read_excel('PgC.xlsx', range = 'A5:B16', col_names = c('Region', 'PgS'),
                   col_types = c('text', 'numeric')) 

par(mar = c(6, 12, 4, 2)) 
barplot(soil$PgS, horiz = T, 
        main = 'Содержание органического углерода в верхнем слое почвы (0-30 см)\nпо климатическим регионам IPCC',
        names.arg = soil$Region, las = 1, xlab = 'PgC', 
        xlim = c(0, 140), col = rainbow(12), 
        cex.main = 2, cex.names = 1.2, cex.lab = 1.7, 
        cex.axis = 1.2)

# распределение высот действующих вулканов мира
volcanos <- read_excel('GVP_Volcano_List_Holocene.xlsx', skip = 1) 

par(mar = c(5, 6, 5, 0))
hist(volcanos$`Elevation (m)`, xlim = c(-6000, 8000), breaks = seq(-6000, 7000, 500),
     main = 'Распределение высот действующих вулканов мира\nподанным Смитсоновского института, 2020 г.',
     xlab = 'Абсолютная отметка вершины над уровнем моря, м', ylab = 'Количество',
     col = c(rep(rgb(0.3, 0.3, 1, 0.8), 12), rep(rgb(1, 0.5, 0.3), 14)), 
     cex.axis = 1.1, cex.lab = 1.7, cex.main = 2) 
legend('right', c('Подводные', 'Надводные'),
       fill = c(rgb(0.3, 0.3, 1, 0.8), rgb(1, 0.5, 0.3)), 
       cex = 1.5, text.width = 2000) 





