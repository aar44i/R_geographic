library(readxl)
library(tidyverse)
library(writexl)

soil_data = read_excel('soil_data.xls', 1,  # чтение фаила 
                col_types = c(rep('numeric', 2), rep('text', 240)))
description = read_excel('soil_data.xls',2 , col_names = FALSE, 
                col_types = c(rep('text', 2)))
colnames(description) = c('name', 'desc')

check_completeness = function(df){ # написание функции для генерации нового фрейма со столбцами name, completeness, status
  name = colnames(df)
  completeness = sapply(df, function(X) {round((length(X) - sum(is.na(X))) / length(X) * 100, digits = 0)})
  status = sapply(completeness, function(X) {if (X == 100) status = 'полностью' 
                                              else if (X == 0) status = 'не заполнен' 
                                              else status = 'частично'})
  df2 = data.frame(name, completeness, status)
}

soil_types = split(soil_data, soil_data$SOIL_ID) # разделение фрейма на фреймы по каждому типу почв
soil_types2 = lapply(soil_types, check_completeness) # применение функции copleteness ко всем фреймам данных
complete = lapply(soil_types2, function(X) {transmute(X, name = name, desc = description$desc, # добавление рассшифровки для каждого SOIL_ID
                                                      completeness = completeness, status = status)})
write_xlsx(complete, 'completeness.xlsx') # вывод готового фаила 