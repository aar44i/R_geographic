# temps = c(-7.8,-6.9, -2.2, 4.0, 10.9, 15.6, 17.7, 16.2, 11.1, 5.7, 0.1, -4.6)
# dt = diff(temps)
# dt
# dt1 = dt[1:11]
# dt2 = dt[2:12]
# dt_new = dt2 - dt1
# #добавляем в конец ветора первый его элемент 
# temps_new = c(temps, temps[1])
# dt_new = diff(temps_new)
# 
# warning = ifelse(dt_new < 0, 'cold', 'warm')
# 
# #ищем зимние месяцы
# winter = which()
# 
# x1 = 0 
# y1 = 0 
# x2 = 1000
# y2 = 1000
# 
# N = 200
# x = runif(N, x1, x2)
# y = runif(N, y1, y2)
# 
# xy = cbind(x,y)
# plot(xy)
# hist(xy)
# Hmin = c(400, 500, 800, 1300, 1600, 2300, 2500, 3300)
Hmax = c(500, 800, 1300, 1600, 2300, 2500, 3300, 5000)
Zone = c('step', 'shirikolistv', 'srednegor', 'smechannie', 
         'krivoles', 'subalp', 'subnival', 'glacio')
df = data.frame(Hmin = c(400, Hmax[-length(Hmax)]), Hmax, Zone)

# Напишите программу, которая просит пользователя ввести высоту и возвращает высотный пояс, 
# соответствующую введенной высоте (достаточно вывести строчку фрейма данных).
H = as.numeric(readline('Insert'))
for(i in 1:nrow(df)) {
  if (H < df$Hmax[i]) {
    print(df$Zone[i])
    break
  }
}