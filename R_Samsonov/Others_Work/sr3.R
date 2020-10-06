

library(openxlsx) # подключение библиотеки

setwd("C:/Users/Lera/Desktop/4 level/MKK/Dates 4")#рабоча€ директори€
expimp <- read.xlsx("ExpImp.xlsx", 1) # чтение таблицы
str(expimp) #вывод изначальных типов данных
expimp$ѕродЁкспорт <- as.numeric(expimp$ѕродЁкспорт) #изменение типов данных
expimp$ѕрод»мпорт <- as.numeric(expimp$ѕрод»мпорт)
expimp$“Ё Ёкспорт <- as.numeric(expimp$“Ё Ёкспорт)
expimp$“Ё »мпорт <- as.numeric(expimp$“Ё »мпорт)
expimp$’имЁкспорт <- as.numeric(expimp$’имЁкспорт)
expimp$ƒревЁкспорт <- as.numeric(expimp$ƒревЁкспорт)
expimp$ћетЁкспорт <- as.numeric(expimp$ћетЁкспорт)
expimp$ћашЁкспорт <- as.numeric(expimp$ћашЁкспорт)
str(expimp) #проверка полученных типов данных


expo <- grep("Ёкспорт", colnames(expimp)) #поиск слова "Ёкспорт"
impo <- grep("»мпорт", colnames(expimp)) #поиск слова "»мпорт"
exp <- expimp[expo] #таблтца, хран€ща€ данные только по экспорту
imp <- expimp[impo] #таблтца, хран€ща€ данные только по импорту

exp <- cbind(exp, expimp$–егион)#в таблицу с экспортом добавл€ем все регионы из общей таблицы

#поиск субъекта одного из типов
type_aob <- grep("автономна€ область", expimp$–егион)
type_aok <- grep("автономный округ", expimp$–егион)
type_sf <- grep("г.",expimp$–егион)
type_kray <- grep("край", expimp$–егион)
type_rep <- grep("–еспублика", expimp$–егион)
type_obl <- grep("область", expimp$–егион)

#добавление субъектов в столбец "Type"
exp[type_aob, "Type"] <- "јвтономна€ область"
exp[type_aok, "Type"] <- "јвтономный округ"
exp[type_sf, "Type"] <- "√ород федерального назначени€"
exp[type_kray, "Type"] <- " рай"
exp[type_rep, "Type"] <- "–еспублика"
exp[type_obl, "Type"] <- "ќбласть"

# превращение в номинальную переменную
exp$Type <- as.character(exp$Type)
exp$Type <- as.factor(exp$Type)

#подсчет количетва субъектов разного типа
summary(exp$Type)

#выгрузка данных по республикам из таблицы "exp"
resp <- grep("–еспублика", exp$Type)
rexp <- exp[resp,]
forsum <- rexp[c(1:6)]

#добавление нового столбца с суммарным экспортом в таблицу "rexp"
rexp$TotalExp <- rowSums(forsum, na.rm=TRUE)

#сортировка
sorti <- order(rexp$TotalExp, decreasing = TRUE)
rexp_sorti <- rexp[sorti, ]
rexp <- rexp_sorti


#русско€зычные названи€ столбцов
colnames(rexp) <- c("ѕродЁкспорт", "“Ё Ёкспорт", "’имЁкспорт", "ƒревЁкспорт", "ћетЁкспорт", "ћашЁкспорт", "–егионы", "“ип", "—уммарный экспорт")
View(rexp)

#сохранение таблицы
write.csv(rexp, 'rexp.csv', fileEncoding = 'UTF-8')
write.xlsx(rexp, 'rexp.xlsx')

#чтение таблиц
rexp.csvsaved <- read.csv("rexp.csv", encoding = 'UTF-8') 
rexp.xlsxsaved <- read_xlsx("rexp.xlsx",1)

View(rexp.csvsaved)
View(rexp.xlsx)