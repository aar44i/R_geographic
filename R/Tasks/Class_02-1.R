temp = c(-7.8, -6.9, -2.2, 4.0, 10.9, 15.6, 
         17.7, 16.2, 11.1, 5.7, 0.1, -4.6)

(deltas = diff(temp))
(change = ifelse(deltas < 0, 'похолодание', 'потепление'))
(stats = summary(temp))

winter = which(temp < 0)

cat('Номера месяцев с отрицательной средней температурой:', winter)

