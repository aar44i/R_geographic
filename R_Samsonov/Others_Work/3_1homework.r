#подключение библиотеки
library(openxlsx, lib.loc = "C:/Users/korni/Desktop/Rstudio/mkk/lib") 

setwd("C:/Users/korni/Desktop/Rstudio/mkk/homework/3") #установка рабочей директории
tabl <-read.xlsx("ExpImp.xlsx", 1) #чтение файла
#преобразование типов данных
tabl$ѕродЁкспорт <- as.numeric(tabl$ѕродЁкспорт)
tabl$ѕрод»мпорт <- as.numeric(tabl$ѕрод»мпорт)
tabl$“Ё Ёкспорт <- as.numeric(tabl$“Ё Ёкспорт)
tabl$“Ё »мпорт <- as.numeric(tabl$“Ё »мпорт)
tabl$’имЁкспорт <- as.numeric(tabl$’имЁкспорт)
tabl$ћетЁкспорт <- as.numeric(tabl$ћетЁкспорт)
tabl$ƒревЁкспорт <- as.numeric(tabl$ƒревЁкспорт)
tabl$ћашЁкспорт <- as.numeric(tabl$ћашЁкспорт) 
str(tabl) #проверка структуры 

#разделение таблицы на 2 других таблицы exp и imp, хран€щих данные только по импорту и только по экспорту
export <- grep("Ёкспорт", colnames(tabl)) 
import <- grep("»мпорт", colnames(tabl)) 
exp <- tabl[, export]
imp <- tabl[, import]
exp$–егион <- tabl$–егион

#добавление к таблице exp нового столбца Type, отвечающего за тип субъекта
exp[grep("автономна€ область", tabl$–егион), "Type"] <- "јвтономна€ область"
exp[grep("автономный округ", tabl$–егион), "Type"] <- "јвтономный округ"
exp[grep("город федерального значени€", tabl$–егион), "Type"] <- "√ород федерального значени€"
exp[grep("край", tabl$–егион), "Type"] <- " рай"
exp[grep("область", tabl$–егион), "Type"] <- "ќбласть"
exp[grep("–еспублика", tabl$–егион), "Type"] <- "–еспублика"
#превращение столбца Type в номинальную переменную
exp$Type <- as.character(exp$Type)
exp$Type <- as.factor(exp$Type) 

summary(exp) #подсчет количества субъектов каждого типа


rexp <- exp[grep("–еспублика", exp$Type), ] #выгрузка из полученной таблицы данных по республикам
total <- rexp[c(-7, -8)]
rexp$TotalExp <- rowSums(total, na.rm=TRUE)
indexes<-order(rexp$TotalExp, decreasing = TRUE) #сортировка по суммарному экспорту

#сохранение таблицы в CSV в кодировке Unicode без названи€ строк и Exsel
write.csv2(rexp[indexes, ], "rexp.csv", fileEncoding = 'UTF-8', row.names=FALSE) 
write.xlsx(rexp[indexes, ], "rexp.xlsx")

#считывание таблицы
rexp.csvsaved <- read.csv2("rexp.csv", encoding = 'UTF-8') 
rexp.xlsxsaved <- read.xlsx("rexp.xlsx",1)

#вывод на экран
View(rexp.csvsaved)
View(rexp.xlsxsaved) 

