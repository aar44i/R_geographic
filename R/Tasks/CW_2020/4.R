# # Функция Тоблера показывает зависимость скорости пешего маршрута (в км/ч) 
# от угла наклона на местности. Предположим, в вашем распоряжении имеется матрица профиля рельефа, 
# в которой в одном столбце указано расстояние от начала маршрута, 
# а во втором — абсолютная отметка точки. Напишите функцию hiking_time(profile), 
# которая вычисляет время прохождения маршрута на основе переданной ей матрицы. 
# Используйте для тестирования функции маршрут из 10 точек с шагом в 1 км 
# и случайным разбросом высот в диапазоне от 500 до 1000 метров (равномерное распределение).
get_speed = function(slope){
  return(6 * exp(-3.5 * abs(slope * 0.5)))
}

hiking_time = function(profile){
  time = 0
  # dh = profile[2:nrow(profile), 2] - profile[2:nrow(profile) - 1, 2]
  # dx = profile[2:nrow(profile), 2] - profile[2:nrow(profile) - 1, 1]
  dh = diff(profile[, 2])
  dx = diff(profile[, 1])
  speed = get_speed(dh/dx)
  time = sqrt(dh * dh + dx * dx) / speed
  return(sum(time))
}

prof = cbind(seq(0, 10000, 1000),
             runif(10, 500, 1000))

hiking_time(prof / 1000)
plot(prof, type = 'l')

# Создайте на основе данных по Москве с сайта pogodaiklimat таблицу Excel 
# с повторяемостью различных направлений ветра. Не преобразовывая структуру данных, 
# вычислите на ее основе с помощью lapply() 
# преобладающее направление для каждого месяца. Представьте результат как фрейм данных.

library(readxl)
library(tidyverse)
setwd('C:/Users/gorba/DataSciense/R_geographic/R/Tasks/CW_2020')
tab = read_excel('wind.xlsx') %>% 
  rename(dir = 1) %>% 
  filter(dir != 'штиль')
#View(wind)
num = sapply(tab[-1], which.max)
prime_dir = tibble(month = colnames(tab[-1]), 
                   dir = tab[num, 'dir'])
# 
# main_dir = wind %>% 
#   rename(dir = 1) %>%
#   filter(dir != 'штиль') %>%
#   select(-dir) %>%
#   sapply(function(x){
#     which.max(x)
#   })
# tibble(dir = colnames(wind)[-1],
#        main_dir = wind[main_dir, 'направл.'])