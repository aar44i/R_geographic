library(readxl)
library(tidyverse)
library(writexl)

soil_data = read_excel('soil_data.xlsx', 1, 
                col_types = c(rep('numeric', 2), rep('text', 240)))
description = read_excel('soil_data.xlsx',2 , col_names = FALSE, 
                col_types = c(rep('text', 2)))
colnames(description) = c('name', 'desc')

check_completeness = function(df){
  name = colnames(df)
  completeness = sapply(df, function(X) {round((length(X) - sum(is.na(X))) / length(X) * 100, digits = 0)})
  status = sapply(completeness, function(X) {if (X == 100) status = 'полностью' else if (X == 0) status = 'не заполнен' else status = 'частично'})
  df2 = data.frame(name, completeness, status)
}
soil_types = split(soil_data, soil_data$SOIL_ID)
soil_types2 = lapply(soil_types, check_completeness)
complete = lapply(soil_types2, function(X) {transmute(X, name = name, desc = description$desc,
                                                      completeness = completeness, status = status)})
write_xlsx(complete, 'completeness.xlsx')