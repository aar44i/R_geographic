library(readxl)

setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/Class/Mine')
# C:\Users\gorba\DataSciense\R_geographic\R_HydroPredictions\Class\Mine
# load('baikal.xlsx')
baikal = read_excel('baikal.xlsx')
baikal = na.omit(baikal) #

baikal_apr = baikal[baikal$month == 'Apr',]

# август
ggplot(baikal_apr, aes(x = year)) + geom_point(aes(y = pred, col = 'прогноз')) + 
  geom_line(aes(y = pred, col= 'Прогноз')) + 
  geom_point(aes(y = obs, col = 'Наблюдения')) + 
  geom_line(aes(y = obs, col = 'Наблюдения'))

mean_pred = mean(baikal_apr$pred)
sd_pred = sd(baikal_apr$pred)
sd_fact = sd(baikal_apr$obs)

baikal_apr$err = baikal_apr$pred - baikal_apr$obs
baikal_apr$AE = abs(baikal_apr$pred - baikal_apr$obs)

ME = mean(baikal_apr$err)
MAE = mean(baikal_apr$AE)

MSE = mean(baikal_apr$err ^ 2)
RMSE = sqrt(MSE)

SSc = RMSE / sd_fact

R = cor(baikal_apr$pred, baikal_apr$obs)


ggplot(baikal_apr, aes(x = obs, y = pred, col = err)) + 
  geom_point(size = 2) + xlim(0,2000) + ylim(0,2000)+ geom_abline() + 
  geom_vline(xintercept = 1000) + geom_hline(yintercept  = 1000)

baikal_apr$opr = baikal_apr$err >=0.674 * sd_fact
summary(baikal_apr)


OPR = sum(baikal_apr$opr) / length(baikal_apr$opr)

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
  R = cor(baikal_apr$pred, baikal_apr$obs)
  result = list(mean_pred, mean_fact, sd_pred, sd_fact, ME, MAE, MSE, RMSE, SSc, R)
  # names(result) = c('F_mean')
  return(result)
}

error_apr = error_check(baikal_apr)
error_apr
