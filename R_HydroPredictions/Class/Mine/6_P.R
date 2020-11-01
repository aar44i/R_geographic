library(readxl)
library(ggplot2)
setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/Class/Mine')
baikal = read_excel('baikal.xlsx')
baikal = na.omit(baikal) #

baikal_aug = baikal[baikal$month == 'Aug',]

# август
ggplot(baikal_aug, aes(x = year)) + geom_point(aes(y = pred, col = 'прогноз')) + 
  geom_line(aes(y = pred, col= 'Прогноз')) + 
  geom_point(aes(y = obs, col = 'Наблюдения')) + 
  geom_line(aes(y = obs, col = 'Наблюдения'))

mean_pred = mean(baikal_aug$pred)
sd_pred = sd(baikal_aug$pred)
sd_fact = sd(baikal_aug$obs)

baikal_aug$err = baikal_aug$pred - baikal_aug$obs
baikal_aug$AE = abs(baikal_aug$pred - baikal_aug$obs)

ME = mean(baikal_aug$err)
MAE = mean(baikal_aug$AE)

MSE = mean(baikal_aug$err ^ 2)
RMSE = sqrt(MSE)

SSc = RMSE / sd_fact

R = cor(baikal_aug$pred, baikal_aug$obs)


ggplot(baikal_aug, aes(x = obs, y = pred, col = err)) + 
  geom_point(size = 3) + xlim(0,2000) + ylim(0,2000) + geom_abline() + 
  geom_vline(xintercept = 1000) + geom_hline(yintercept  = 1000)

baikal_aug$opr = baikal_aug$err >=0.674 * sd_fact
summary(baikal_aug)


OPR = sum(baikal_aug$opr) / length(baikal_aug$opr)

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
  R = cor(baikal_aug$pred, baikal_aug$obs)
  result = list(mean_pred, mean_fact, sd_pred, sd_fact, ME, MAE, MSE, RMSE, SSc, R)
  # names(result) = c('F_mean')
  return(result)
}

error_apr = error_check(baikal_aug)
error_apr
