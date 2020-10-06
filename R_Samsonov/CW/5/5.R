# data(quakes)
# hist(quakes$depth, 
#      col = rgb(0.5, 1, 0.8), 
#      xlab = 'Deep',
#      ylab = 'count',
#      main = 'hist\n
#      earhquakes')
# 
# plot(quakes$depth, quakes$mag, 
#      pch  = 21, cex = 1, col ='red', 
#      main = 'quakes')
library(readxl)
library(tidyverse)
library(writexl)
df = read_xlsx('garabashi.xlsx')
df = df %>% 
  rename(year = 1, acc = 2, abl = 3, bal = 4) %>% 
  filter(!str_detect(year, '-')) %>% 
  separate(year, c('year', 'y'), 4 ) %>% 
  select(-y) %>% 
  mutate(year = as.integer(year),
         cumbal = cumsum(bal))

plot(df$year, df$cumbal, type = 'l', 
     ylim = c(-1200, 400),
     xlab = '√од', ylab ='см. в.э', 
     main = 'Ѕаланс массы ледника')

abline(h = 0, lty = 3)
lines(df$year, df$acc, col = 'green')
lines(df$year, df$abl, col = 'red')
legend('bottomleft', 
       c('јккумул€ци€', 'јбл€ци€', 'Ѕаланс массы'),
       title = 'legend',
       col = c('green', 'red', 'blue'),
       lwd = c(1,1,2))