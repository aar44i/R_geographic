Sys.setlocale("LC_ALL","Russian")
library(readxl)
library(tidyverse)
library(writexl)

setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/1/4/байкал')
getwd()
xls_files = list.files(pattern = '.xls')
# df_15_16 = read_excel('new_data.xlsx')


prog_df=data.frame()
for (x in xls_files){
  print(x)
  df=read_xls(x, skip = 10, 
                 col_names = c('year', 'pred', 'pred1', 'obs'), 
                 col_types = c('numeric', 'numeric', 'skip', 'numeric'))
  print(dim(df))
  prog_df=rbind(prog_df, df)
}
prog_df$month=rep(month.abb[c(1, 10:12, 2:9)], each = 54)
write_xlsx(prog_df, "baikal.xlsx")
getwd()
# добавление данных за 2015-2016 год 
setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/1/4')
df_15_16 = read_excel('new_data.xlsx', 
                      col_names = c('date', 'pred', 'obs'),  skip = 1 )

df_15_16$year = format(as.Date(df_15_16$date, format="%d/%m/%Y"),"%Y")
df_15_16$year = as.numeric(as.character(df_15_16$year))
df_15_16$month = rep(month.abb[c(1, 10:12, 2:9)])

i = 0
for (i in length(prog_df$year)){
  if (prog_df$year == df_15_16[i,]$year& prog_df$month == df_15_16[i,]$month){
    prog_df$pred[i] = df_15_16$pred[i]
    prog_df$obs[i] = df_15_16$obs[i]
  }
}

