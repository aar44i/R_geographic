# јнна якимова. —амосто€тельна€ работа є3 (стандартные операции с таблицами) --------

#открытие библиотек
library(readr)
library(openxlsx) 

#указание рабочей директории
setwd('/Users/Anna Yakimova/Desktop/Program/homework3.tables')  

#чтение таблицы и проверка ее структуры
exp_imp <- read.xlsx("ExpImp.xlsx", 1)  
str(exp_imp)
#View(exp_imp)

#преобразование типов данных
exp_imp$ѕродЁкспорт <- as.numeric(exp_imp$ѕродЁкспорт)
exp_imp$ѕрод»мпорт <- as.numeric(exp_imp$ѕрод»мпорт)
exp_imp$“Ё Ёкспорт <- as.numeric(exp_imp$“Ё Ёкспорт)
exp_imp$“Ё »мпорт <- as.numeric(exp_imp$“Ё »мпорт)
exp_imp$’имЁкспорт <- as.numeric(exp_imp$’имЁкспорт)
exp_imp$ƒревЁкспорт <- as.numeric(exp_imp$ƒревЁкспорт)
exp_imp$ћетЁкспорт <- as.numeric(exp_imp$ћетЁкспорт)
exp_imp$ћашЁкспорт <- as.numeric(exp_imp$ћашЁкспорт)
str(exp_imp) #проверка

#разделение таблицы на "Ёкспорт и »мпорт"
export <- grep("Ёкспорт", colnames(exp_imp))
exp <- exp_imp[, export]
print(exp)
import <- grep("»мпорт", colnames(exp_imp))
imp <- exp_imp[, import]
print(imp)
exp$–егион <- exp_imp$–егион

#добавление столбца, отвечающего за тип региона, в таблицу экспорта
exp[grep("автономна€ область", exp_imp$–егион), "Type"] <- "јвтономна€ область"
exp[grep("автономный округ", exp_imp$–егион), "Type"] <- "јвтономный округ"
exp[grep("город федерального значени€", exp_imp$–егион), "Type"] <- "√ород федерального значени€"
exp[grep("край", exp_imp$–егион), "Type"] <- " рай"
exp[grep("область", exp_imp$–егион), "Type"] <- "ќбласть"
exp[grep("–еспублика", exp_imp$–егион), "Type"] <- "–еспублика"

#превращение столбца в номинальную переменную (фактор)
exp$Type <- as.factor(exp$Type)
str(exp)

#проверка подсчета количества субъектов разного типа
summary(exp$Type)

#выгрузка из таблицы экспорта данных по республикам
resp_exp <- grep("–еспублика", exp$Type)
rexp <- exp[resp_exp, ]

#создание нового столбца, заполненного суммарным объемом экспорта по всем направлени€м
cleaned <- grep("Ёкспорт", colnames(rexp))
summa <- rexp[, cleaned]
rexp$TotalExp <- rowSums(summa, na.rm = TRUE)

#сортировка таблицы по убыванию значений суммарного объема экспорта
TotalExp_sort <- order(rexp$TotalExp, decreasing = TRUE)
rexp_sort <- rexp[TotalExp_sort, ]


#экспорт таблицы в форматы Microsoft Excel и CSV
write.csv(rexp_sort, "rexp_sort.csv", fileEncoding = 'CP1251')
write.xlsx(rexp_sort, "rexp_sort.xlsx")

#чтение полученных таблиц
rexp_sort_csv <- read.csv("rexp_sort.csv", encoding = 'CP1251') 
rexp_sort_xlsx <- read.xlsx("rexp_sort.xlsx", 1)

#вывод на экран
View(rexp_sort_csv)
View(rexp_sort_xlsx)