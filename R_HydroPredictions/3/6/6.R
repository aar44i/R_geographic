library(readxl)
library(ggplot2)
setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/2/6')
baikal = read_excel('baikal.xlsx')
baikal = na.omit(baikal) #

baikal_sep = baikal[baikal$month == 'Sep',]

# ????
ggplot(baikal_sep, aes(x = year)) + geom_point(aes(y = pred, col = '???????')) + 
  geom_line(aes(y = pred, col= '???????')) + 
  geom_point(aes(y = obs, col = '??????????')) + 
  geom_line(aes(y = obs, col = '??????????'))

mean_pred = mean(baikal_sep$pred)
sd_pred = sd(baikal_sep$pred)
sd_fact = sd(baikal_sep$obs)

baikal_sep$err = baikal_sep$pred - baikal_sep$obs
baikal_sep$AE = abs(baikal_sep$pred - baikal_sep$obs)

ME = mean(baikal_sep$err)
MAE = mean(baikal_sep$AE)

MSE = mean(baikal_sep$err ^ 2)
RMSE = sqrt(MSE)

SSc = RMSE / sd_fact

R = cor(baikal_sep$pred, baikal_sep$obs)


ggplot(baikal_sep, aes(x = obs, y = pred, col = err)) + 
  geom_point(size = 3) + xlim(0,2000) + ylim(0,2000) + geom_abline() + 
  geom_vline(xintercept = 1000) + geom_hline(yintercept  = 1000)

baikal_sep$opr = baikal_sep$err >=0.674 * sd_fact
sumsepy(baikal_sep)


OPR = sum(baikal_sep$opr) / length(baikal_sep$opr)

error_check = function(x){
  mean_pred = mean(x$pred)
  mean_fact = mean(x$obs)
  sd_pred = sd(x$pred)
  sd_fact = sd(x$obs)
  x$err = x$pred - x$obs
  x$AE = abs(x$pred - x$obs)
  ME = mean(x$err)
  MAE = mean(x$AE)
  MSE = mean(x$err ^ 2)
  RMSE = sqrt(MSE)
  SSc = RMSE / sd_fact
  R = cor(baikal_sep$pred, baikal_sep$obs)
  result = list(mean_pred, mean_fact, sd_pred, sd_fact, ME, MAE, MSE, RMSE, SSc, R)
  names(result) = c('mean_pred', 'mean_fact', 'sd_pred', 'sd_fact', 'ME', 'MAE', 'MSE', 'RMSE', 'SSc', 'R')
  return(result)
}

error_apr = error_check(baikal_sep)
error_apr
