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

prog_df = data.frame()
for (x in xls_files){
  print(x)
  df = read_xls(x, skip = 10,
                col_names = c('year', 'pred', 'obs'), 
                col_types = c('numeric', 'numeric', 'skip', 'numeric'))
  print(dim(df))
  prog_df = rbind(prog_df, df)             
}
prog_df$month = rep(month.abb[c(1, 10:12, 2:9)], each = 54)
prog_df$month = factor(prog_df$month, levels = month.abb, ordered = TRUE)

read_prog = function(x){
  df = read_xls(x, skip = 10,
                col_names = c('year', 'pred', 'obs'), 
                col_types = c('numeric', 'numeric', 'skip', 'numeric'))
}

prog_list = lapply(xls_files, read_prog)
prog_lis_df = do.call(what = rbind, arg = prog_list)

summary(prog_df)
# libraby(ggplot)

library(ggplot2)
ggplot(prog_df, aes(x = year, col = month)) + 
  geom_line(aes(y = obs, col ='Наблюдения'),  size = 2) + 
  geom_line(aes(y = pred, col = 'Прогноз'), linetype = 'dashed') +
  facet_wrap(.~month, scales = 'free_y') + 
  labs(x = 'Год', y = expression('Приток'), col = 'Приток')

ggplot(prog_df ,aes(x = obs, y = pred, col = month)) + geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula = y ~ x, show.legend = F) + 
  geom_abline() + xlim(-1000, 10000) + ylim(-1000, 10000) + facet_wrap(.~month)
  

ggplot(prog_df ,aes(x = obs, y = pred, col = month)) + geom_point(size = 3) + 
  geom_smooth(method = 'lm', formula = y ~ x, show.legend = F) + 
  geom_abline() + facet_wrap(.~month, scales = 'free')

cor(na.omit(prog_df[,2:3]))





