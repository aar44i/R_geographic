library(readxl)
library(ggplot2)
setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/1/6')
baikal = read_excel('baikal.xlsx')
baikal = na.omit(baikal) #

baikal_mar = baikal[baikal$month == 'Mar',]

# ????
ggplot(baikal_mar, aes(x = year)) + geom_point(aes(y = pred, col = '???????')) + 
  geom_line(aes(y = pred, col= '???????')) + 
  geom_point(aes(y = obs, col = '??????????')) + 
  geom_line(aes(y = obs, col = '??????????'))

mean_pred = mean(baikal_mar$pred)
sd_pred = sd(baikal_mar$pred)
sd_fact = sd(baikal_mar$obs)

baikal_mar$err = baikal_mar$pred - baikal_mar$obs
baikal_mar$AE = abs(baikal_mar$pred - baikal_mar$obs)

ME = mean(baikal_mar$err)
MAE = mean(baikal_mar$AE)

MSE = mean(baikal_mar$err ^ 2)
RMSE = sqrt(MSE)

SSc = RMSE / sd_fact

R = cor(baikal_mar$pred, baikal_mar$obs)


ggplot(baikal_mar, aes(x = obs, y = pred, col = err)) + 
  geom_point(size = 3) + xlim(0,2000) + ylim(0,2000) + geom_abline() + 
  geom_vline(xintercept = 1000) + geom_hline(yintercept  = 1000)

baikal_mar$opr = baikal_mar$err >=0.674 * sd_fact
summary(baikal_mar)


OPR = sum(baikal_mar$opr) / length(baikal_mar$opr)

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
  R = cor(baikal_mar$pred, baikal_mar$obs)
  result = list(mean_pred, mean_fact, sd_pred, sd_fact, ME, MAE, MSE, RMSE, SSc, R)
  names(result) = c('mean_pred', 'mean_fact', 'sd_pred', 'sd_fact', 'ME', 'MAE', 'MSE', 'RMSE', 'SSc', 'R')
  return(result)
}

error_apr = error_check(baikal_mar)
error_apr
