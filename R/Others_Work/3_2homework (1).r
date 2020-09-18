#подключение библиотек
library(openxlsx, lib.loc = "C:/Users/korni/Desktop/Rstudio/mkk/lib")
library(dplyr, lib.loc = "C:/Users/korni/Desktop/Rstudio/mkk/lib")

setwd("C:/Users/korni/Desktop/Rstudio/mkk/homework/3") #установка рабочей директории
tabl <-read.xlsx("ExpImp.xlsx", 1) #чтение файла
#преобразование типов данных
tabl$ПродЭкспорт <- as.numeric(tabl$ПродЭкспорт)
tabl$ПродИмпорт <- as.numeric(tabl$ПродИмпорт)
tabl$ТЭКЭкспорт <- as.numeric(tabl$ТЭКЭкспорт)
tabl$ТЭКИмпорт <- as.numeric(tabl$ТЭКИмпорт)
tabl$ХимЭкспорт <- as.numeric(tabl$ХимЭкспорт)
tabl$МетЭкспорт <- as.numeric(tabl$МетЭкспорт)
tabl$ДревЭкспорт <- as.numeric(tabl$ДревЭкспорт)
tabl$МашЭкспорт <- as.numeric(tabl$МашЭкспорт)
str(tabl) #проверка структуры 

#разделение таблицы на 2 других таблицы exp и imp, хранящих данные только по импорту и только по экспорту
exp <- select(tabl, Регион, ПродЭкспорт, ТЭКЭкспорт, ХимЭкспорт, ДревЭкспорт, МетЭкспорт, МашЭкспорт)
imp <- select(tabl, Регион, ПродИмпорт,  ТЭКИмпорт, ХимИмпорт, ДревИмпорт,  МетИмпорт, МашИмпорт)

#добавление к таблице exp нового столбца Type, отвечающего за тип субъекта
exp <- mutate(exp, Type = Регион)
exp[grep("автономная область", tabl$Регион), "Type"] <- "Автономная область"
exp[grep("автономный округ", tabl$Регион), "Type"] <- "Автономный округ"
exp[grep("город федерального значения", tabl$Регион), "Type"] <- "Город федерального значения"
exp[grep("край", tabl$Регион), "Type"] <- "Край"
exp[grep("область", tabl$Регион), "Type"] <- "Область"
exp[grep("Республика", tabl$Регион), "Type"] <- "Республика"
#превращение столбца Type в номинальную переменную
exp$Type <- as.character(exp$Type)
exp$Type <- as.factor(exp$Type)
summary(exp) #подсчет количества субъектов каждого типа

#выгрузка из полученной таблицы данных по республикам и сортировка по суммарному экспорту
rexp <- filter(exp, Type == "Республика")
total <- rexp[c(-1, -8)]
rexp <- mutate(rexp, TotalExp = rowSums(total, na.rm=TRUE))
rexp <- arrange(rexp, desc(TotalExp))

#сохранение таблицы в CSV в кодировке Unicode без названия строк и Exsel
write.csv2(rexp, "rexp2.csv", fileEncoding = 'UTF-8', row.names=FALSE) 
write.xlsx(rexp, "rexp2.xlsx")

#считывание таблицы
rexp.csvsaved <- read.csv2("rexp2.csv", encoding = 'UTF-8') 
rexp.xlsxsaved <- read.xlsx("rexp2.xlsx",1)

View(rexp.csvsaved)
View(rexp.xlsxsaved)