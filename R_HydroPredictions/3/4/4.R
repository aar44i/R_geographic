Sys.setlocale("LC_ALL","Russian")
library(readxl)
library(tidyverse)
library(writexl)

setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/2/4/??????')
getwd()
xls_files = list.files(pattern = '.xls')
# df_new = read_excel('new_data.xlsx')


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
# ?????????? ?????? ?? 2015-2016 ??? 
setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/1/4')
df_new = read_excel('new_data.xlsx', 
                      col_names = c('date', 'pred', 'obs'),  skip = 1 )

df_new$year = format(as.Date(df_new$date, format="%d/%m/%Y"),"%Y")
df_new$year = as.numeric(as.character(df_new$year))
df_new$month = rep(month.abb[c(1, 10:12, 2:9)])

i = 0
for (i in length(prog_df$year)){
  if (prog_df$year == df_new[i,]$year& prog_df$month == df_new[i,]$month){
    prog_df$pred[i] = df_new$pred[i]
    prog_df$obs[i] = df_new$obs[i]
  }
}

