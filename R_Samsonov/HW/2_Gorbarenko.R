num = c(1, 2, 3, 4, 5, 6, 7)
towns = c( "Орёл", "Калуга", "Серпухов", "Коломна", "Рязань", "Муром", "Нижний Новгород")
lat = c(52.938023, 54.505862,  54.883524, 55.070474, 54.651824, 55.576834, 56.210471)
long = c(36.065684, 36.233586, 37.418129,38.830827, 39.806943, 42.072086, 43.872036)
dist_s = c(111, 391, 522,  645, 801, 1285, 1500)
dist_m = c(1389, 1109, 978, 855, 699, 215, 0)
zero_h = c(146.31, 116.72, 107.54, 100.26, 93.41, 73.27, 62.00)

Cities = data.frame(num = num, towns = towns, lat = lat, long = long, 
                    dist_s = dist_s, dist_m = dist_m, 
                    zero_h = zero_h) # первый фрейм

  D = acos(sin(lat[1:6]) * sin(lat[2:7]) + cos(lat[1:6]) * cos(lat[2:7])
           * cos( long[1:6] - long[2:7])) # расстояние между городами по ортодромии 
  S = D * 111.1
  
  distanse = dist_s [2:7] - dist_s [1:6] #расстояние между городами по реке
  tortusity = S / distanse #извилистость 
  
Sections = data.frame(first_city = num[1:6], second_city =  num[2:7],
                        Distanse = distanse,
                        Hight = zero_h[1:6] - zero_h[2:7],
                        Tortusity = tortusity) #второй фрейм

#Cities  #пробный вывод фреймов
#Sections

user_dist = as.numeric(readline('Введите расстояние от начала маршрута (в км):'))

if ( user_dist > 1389){
  print('Данная точка находится за пределами обследуемой территории, попробуйте заново')
  user_dist = as.numeric(readline('Введите расстояние от начала маршрута (в км):'))
} else { 
    i = 1
    while (user_dist > 1389 - Cities$dist_m[i]){
      i = i + 1  
    } 
}

#UserDistance
bcity = Cities$towns[i - 1] # Город который уже проплыл
fcity = Cities$towns[i] # Город до которого еще не доплыл


prsnt = 100 * user_dist / 1389 # процент пройденного пути 
prsnt = round(prsnt, 0)


current_h = Cities$zero_h[i - 1]  - (user_dist - (1389 - Cities$dist_m[i - 1])) * 
              (Cities$zero_h[i - 1] - Cities$zero_h[i]) / 
              abs(Cities$dist_m[i] - Cities$dist_m[i - 1]) # точное значение абс. высоты в точке

current_h = round(current_h, 1) #округление

cat(sep = "", "Вы находитесь между городами ",bcity," и ",fcity,", пройдено ",prsnt,"% маршрута, ваша высота примерно ",current_h,"м.")
