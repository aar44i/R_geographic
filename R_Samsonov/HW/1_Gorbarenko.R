

lat_START = as.numeric(readline('Введите широту начальной точки:')) # ввод координа  пользователем
long_START = as.numeric(readline('Введите долготу начальной точки:'))
lat_FINISH = as.numeric(readline('Введите широту конечной точки:'))
long_FINISH = as.numeric(readline('Введите долготу конечной точки:'))
#pi = 3.14
PP = pi / 180
lat_START = lat_START * PP   # перевод координат из градусы в радианы
long_START = long_START * PP  
lat_FINISH = lat_FINISH * PP  
long_FINISH = long_FINISH * PP  


D = acos(sin(lat_START) * sin(lat_FINISH) + cos(lat_START) * 
           cos(lat_FINISH) * cos( long_START - long_FINISH )) # вычисление длины ртодромии 
S = D * 6375 # вычисление расстояния 
S = round(S, 0) # округление расстояния

Time = S / 850 # вычисление времени
Time = round(Time * 2) / 2


if (long_START * long_FINISH < 0) { #Определение, пересекает ли самолет 180 или 0 меридиан 
  
  if (((abs(long_START) + abs(long_FINISH)) / pi * 180) > 180) { 
    cat("Длина полета составила ",S," км, время в пути ~ ",Time," часа(ов). 
        Маршрут полета не пересекает нулевой меридиан и пересекает 180-й меридиан.")
  } else { 
    cat("Длина полета составила ",S," км, время в пути ~ ",Time," часа(ов). 
        Маршрут полета  пересекает нулевой меридиан и не пересекает 180-й меридиан.")
  }
} else { 
  cat("Длина полета составила ",S," км, время в пути ~ ",Time," часа(ов). 
        Маршрут полета  не пересекает нулевой меридиан и не пересекает 180-й меридиан.")
}


