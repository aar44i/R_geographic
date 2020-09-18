# Анна Якимова. Самостоятельная работа №3 (dplyr) -------------------------

#открытие библиотек
library(openxlsx)
library(dplyr)

#указание рабочей директории и чтение таблицы
setwd('/Users/Anna Yakimova/Desktop/Program/homework3.tables')
imp_exp <- read.xlsx("ExpImp.xlsx", 1)
str(imp_exp) #просмотр структуры таблицы

#преобразование типов данных
imp_exp$ПродЭкспорт <- as.numeric(imp_exp$ПродЭкспорт)
imp_exp$ПродИмпорт <- as.numeric(imp_exp$ПродИмпорт)
imp_exp$ТЭКЭкспорт <- as.numeric(imp_exp$ТЭКЭкспорт)
imp_exp$ТЭКИмпорт <- as.numeric(imp_exp$ТЭКИмпорт)
imp_exp$ХимЭкспорт <- as.numeric(imp_exp$ХимЭкспорт)
imp_exp$ДревЭкспорт <- as.numeric(imp_exp$ДревЭкспорт)
imp_exp$МетЭкспорт <- as.numeric(imp_exp$МетЭкспорт)
imp_exp$МашЭкспорт <- as.numeric(imp_exp$МашЭкспорт)
str(imp_exp)

#Переименовываем названия столбцов 
colnames(imp_exp) <- c("Region", "ProdExp", "ProdImp", "TEKExp", "TEKImp", "ChemExp", "ChemImp", "DrevExp", "DrevImp", "MetExp", "MetImp", "MachExp", "MachImp")
str(imp_exp)

#делим таблицу на две
imp <- select(imp_exp, ProdImp, TEKImp, ChemImp, DrevImp, MetImp, MachImp)
exp <- select(imp_exp, ProdExp, TEKExp, ChemExp, DrevExp, MetExp, MachExp) %>% 
       mutate(Region = imp_exp$Region) #Создаем новый столбец, отвечающий за тип субъекта
View(exp)  

#создаем переменные, отвечающие за тот или иной тип субъекта
avt_obl <- grep("автономная область", imp_exp$Region)
avt_okr <- grep("округ", imp_exp$Region)
city <- grep("г.", imp_exp$Region)
krai <- grep("край", imp_exp$Region)
resp <- grep("Республика", imp_exp$Region)
obl <- grep("область", imp_exp$Region)

#Присваиваем столбцу Type значения переменых, содержащих информацию о типах субъекта
exp[avt_obl,"Type"] <- "Автономная область"
exp[avt_okr,"Type"] <- "Автономный округ"
exp[city,"Type"] <- "Город федерального значения"
exp[krai,"Type"] <- "Край"
exp[resp,"Type"] <- "Республика"
exp[obl,"Type"] <- "Область"

#превращение столбца в номинальную переменную (фактор)
exp$Type <- as.factor(exp$Type)
#проверка подсчета количества каждого из субъектов
summary(exp)
View(exp)

#создание таблицы по республикам
rexp <- filter(exp, Type == "Республика")
View(rexp)
total <- rexp[, c(1:6)]
View(total)
rexp <- mutate(rexp, TotalExp = rowSums(total, na.rm = TRUE)) 
rexp <- arrange(rexp, desc(TotalExp))

View(rexp)

#экспорт таблицы в форматы Microsoft Excel и CSV
#write.csv(rexp, "rexp.csv", fileEncoding = 'CP1251')
#write.xlsx(rexp, "rexp.xlsx")

#чтение таблиц
#rexp_csv <- read.csv("rexp.csv", encoding = 'CP1251') 
#rexp_xlsx <- read.xlsx("rexp.xlsx", 1)

#Вывод на экран
#View(rexp_csv)
#view(rexp_xlsx)
