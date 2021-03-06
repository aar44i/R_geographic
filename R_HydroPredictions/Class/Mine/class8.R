Sys.setlocale("LC_ALL","Russian")
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
# Данные по Протве
setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/Class/Mine')
protva_h <- read.csv('protva_sz_2008-2020.csv')
protva_h$Dates <- as.Date(protva_h$Dates, format = '%d-%m-%Y')
summary(protva_h)

ggplot(protva_h, aes(x= Dates, y = Absolute_Levels, col = 'Протва - с. Спас-Загорье')) + geom_line() + geom_point()

protva_meteo <- read_xlsx('protva_meteo_2008-2020.xlsx')
protva_meteo$dates <- as.Date(protva_meteo$dates)
protva_meteo <- melt(protva_meteo, id.vars = c('dates', 'index'))
protva_meteo <- dcast(protva_meteo, dates~variable+index)
summary(protva_meteo)

df <- merge(protva_h, protva_meteo, by.x = 'Dates', by.y = 'dates')
summary(df)
df <- na.omit(df[,c(1, 2, 4:9)])
summary(df)

# построение набора предикторов - сдвиг назад и вперед 
shift = 7

shift_data <- function(x, shift, target_name){
  for(col in colnames(x[,-1])){
    for(i in seq(1, shift, 1)){
      print(col)
      print(i)
      col_lag <- paste0(col, '(t-', i, ')')
      col_lead <- paste0(target_name, '(t+', i, ')')
      x[[col_lag]] <- lag(x[[col]], n = i)
      x[[col_lead]] <- lead(x[[target_name]], n = i)
    }
  }
  return(x)
}

df <- shift_data(df, shift, 'Absolute_Levels')

# анализ корреляции и автокорреляции

ach <- acf(df$Absolute_Levels, plot = T)
ach

cm <- cor(df[,-1], use = "complete.obs")
ids <- c(colnames(df)[grepl('t\\+', names(df))])
ids

cm1 <- reshape2::melt(cm)

ggplot(cm1[cm1$Var1 == 'Absolute_Levels(t+1)',], aes(x=Var2, y=value, fill=value)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle=90))

target = 'Absolute_Levels(t+5)'
features = colnames(df[,-1])[!grepl('t\\+', colnames(df[,-1]))]
features
features_levels = features[grepl('Levels', features)]
features_levels
# разделение на тренинг и тест

ggplot(df, aes(x=Dates, y=Absolute_Levels)) + geom_line() + geom_point()

model_cols = paste(c('Dates', target, features_levels), sep = "")
model_cols

train_df <- df[(year(df$Dates) > 2013) & (year(df$Dates) < 2020),]
test_df <- df[year(df$Dates) <= 2013,]
val_df <- df[year(df$Dates) == 2020,]

# построение множественной линейной регрессии

mod1 <- lm(data = train_df[,-1], formula = `Absolute_Levels(t+1)` ~ .)
summary(mod1)
coef(mod1)

# проверка на тестовой выборке
test_df$pred <- predict(mod1, newdata = test_df[,-1])

ggplot(test_df, aes(x=Dates)) + geom_line(aes(y=Absolute_Levels, col='obs')) +
  geom_line(aes(y=pred, col='mod'))

ggplot(test_df, aes(x=Absolute_Levels, y=pred)) + geom_point() + geom_abline()

# предсказание в оперативном режиме

val_df$pred <- predict(mod1, newdata = val_df)

ggplot(val_df, aes(x=Dates)) + geom_line(aes(y=Absolute_Levels, col='obs')) +
  geom_line(aes(y=pred, col='mod'))

linranry(verification)
verify(obs = test_df$`Absolute_levels(t+1)`, pred = test_df$pred1), obs.type = 'cont', frcst.type = 'cont')
