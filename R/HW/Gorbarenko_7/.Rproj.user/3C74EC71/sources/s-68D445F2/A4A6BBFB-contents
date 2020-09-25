library(readxl)
library(tidyverse)
library(writexl)

setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions')
df = read_excel('oka.xlsx')
summary(df)

df = na.omit(df) # удаляет все строки с пустыми значениями 

plot(x = df$dist, y = df$len)
hist(df$area, labels = T, breaks = 3)

df$side = factor(df$side) 
levels(df$side)
plot(x = df$side, y = df$area)
# разделение имеющихся данных по каким либо признакам
df$size = cut(df$area, labels = c('small', 'mid', 'big'), 
              breaks = c(0, 3500, 10000, 100000))
plot(x = df$size, y = df$len)

library(ggplot2)
#1
ggplot(df, aes(x = size, y = len, col = size)) + 
  geom_boxplot() + stat_boxplot(geom = 'errorbar')
#2
ggplot(df, aes(x = area, fill = side)) + geom_histogram(binwidth =  5000, position = 'dodge')
#3
ggplot(df, aes(x = len, y = area))+ geom_point(size = 5) + 
  geom_smooth(method = 'lm', formula = y ~ x, se = F)

area_model = lm(data = df, formula = area ~ len)
df$pred_area = predict(area_model)

coef_a = as.character(round(coef(area_model)[2],2))
coef_b = as.character(round(coef(area_model)[1],2))
cor_coef = as.character(round(cor(df$area, df$pred_area), 2))

model_text = paste('y = ', coef_a, '* x', coef_b, "R = ", cor_coef)
model_text

p = ggplot(df, aes(x = len)) + geom_point(aes(y = area, col = 'факт')) + 
  geom_line(aes(y = pred_area, col = 'модель'), size = 1) +
  geom_text(aes(x =100, y = 40000, label = model_text))
ggsave(plot = p, filename = 'linear_model.png', device = 'png', width = 10, height = 8,
       units = 'in', dpi = 300)

# cor(df$area, df$pred_area)
# coef(area_model)[2]
# coef(area_model)[1]
