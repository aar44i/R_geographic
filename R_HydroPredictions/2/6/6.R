library(readxl)
library(ggplot2)
setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/2/6')
baikal = read_excel('baikal.xlsx')
baikal = na.omit(baikal) #

baikal_feb = baikal[baikal$month == 'Feb',]

# ????
ggplot(baikal_feb, aes(x = year)) + geom_point(aes(y = pred, col = '???????')) + 
  geom_line(aes(y = pred, col= '???????')) + 
  geom_point(aes(y = obs, col = '??????????')) + 
  geom_line(aes(y = obs, col = '??????????'))

mean_pred = mean(baikal_feb$pred)
sd_pred = sd(baikal_feb$pred)
sd_fact = sd(baikal_feb$obs)

baikal_feb$err = baikal_feb$pred - baikal_feb$obs
baikal_feb$AE = abs(baikal_feb$pred - baikal_feb$obs)

ME = mean(baikal_feb$err)
MAE = mean(baikal_feb$AE)

MSE = mean(baikal_feb$err ^ 2)
RMSE = sqrt(MSE)

SSc = RMSE / sd_fact

R = cor(baikal_feb$pred, baikal_feb$obs)


ggplot(baikal_feb, aes(x = obs, y = pred, col = err)) + 
  geom_point(size = 3) + xlim(0,2000) + ylim(0,2000) + geom_abline() + 
  geom_vline(xintercept = 1000) + geom_hline(yintercept  = 1000)

baikal_feb$opr = baikal_feb$err >=0.674 * sd_fact
sumfeby(baikal_feb)


OPR = sum(baikal_feb$opr) / length(baikal_feb$opr)

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
  R = cor(baikal_feb$pred, baikal_feb$obs)
  result = list(mean_pred, mean_fact, sd_pred, sd_fact, ME, MAE, MSE, RMSE, SSc, R)
  names(result) = c('mean_pred', 'mean_fact', 'sd_pred', 'sd_fact', 'ME', 'MAE', 'MSE', 'RMSE', 'SSc', 'R')
  return(result)
}

error_apr = error_check(baikal_feb)
error_apr
