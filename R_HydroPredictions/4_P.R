# df = data.frame(obs = rnorm(10),
#                 pred = rnorm(10))
# 
# newcol = set.Date(from = as.Date('2020-09-21'), by = '1 day', length.out = 10)
# newcol 
# 
# df = cbind(df, newcol)
# df
# newrow = c(rnorm(1), rnorm(1), '2020-10-01')
# newrow
# 
# newrow = list(rnorm(1), rnorm(1), '2020-10-01')

setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/байкал')
xls_files = list.files(pattern = '.xls')
xls_files

library(readxl)
prog_agr = read_xls(xls_files[7], skip = 10, 
                col_names = c('year', 'pred', 'obs'), 
                col_types = c('numeric', 'numeric', 'skip', 'numeric'))