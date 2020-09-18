x1 = -20
y1 = -10
x2 = 50
y2 = 40
N = 500
x = runif(N, x1, x2)
y = runif(N, y1, y2)
coords = cbind(x, y)
plot(coords)
