x1 = -20
y1 = -10
x2 = 50
y2 = 40
N = 500

x = vector('numeric', N)
y = vector('numeric', N)

x[1] = runif(1, x1, x2) 
y[1] = runif(1, y1, y2)

k = 2 # минимально допустимое расстояние
m = 1 # количество сгенерированных точек

while (m < N) {
  xc = runif(1, x1, x2)
  yc = runif(1, y1, y2)
  
  near = FALSE
  for (i in 1:m) {
    d = sqrt((xc - x[i]) ** 2 + (yc - y[i]) ** 2)
    
    if (d <= k) {
      near = TRUE
      break
    }
  }
  
  if (!near) {
    x[m + 1] = xc
    y[m + 1] = yc
    m = m + 1
  }
}

coords = cbind(x, y)
plot(coords)