library(sf)
library(stars)
library(raster)
library(tidyverse)
library(units)
# Чтение данных
stations = read_csv("Stations.csv", locale = locale(encoding = "UTF-8")) 
route = read_csv("Route.csv")
field = read_csv("Field.csv")
img = stack("Snimok.tif")
# Создание Simple Features
route = st_linestring(cbind(route$X, route$Y)) %>% # линия маршрута
  st_sfc() %>% 
  st_sf()
field = st_linestring(cbind(field$X, field$Y)) %>% # линия отбивки пашни
  st_sfc() %>% 
  st_sf() %>% 
  st_node() %>% # дополнительные вершины в самопересечениях
  st_polygonize() # полигон пашни
stations = st_as_sf(stations, coords = c("X", "Y"), crs = 4326) # искомые пункты
# Задание системы координат
st_crs(field) = st_crs(4326)
st_crs(route) = st_crs(4326)
# Преобразование системы координат
field = st_transform(field, as.character(crs(img)))
route = st_transform(route, as.character(crs(img)))
stations = st_transform(stations, as.character(crs(img)))
# Создание точек начала и конца маршрута
start = st_point(st_coordinates(route)[1, ]) %>% # начало маршрута
  st_sfc() %>% 
  st_sf()
len = length(st_coordinates(route)[ , 1]) 
end = st_point(st_coordinates(route)[len, ]) %>% # конец маршрута
  st_sfc() %>% 
  st_sf()
# Подготовка подписи площади, цвета заливкм пашни
p_area = paste("Пашня \n", round(st_area(field) / 10000, digits = 1), "\n га")
# Построение карты
centr = st_centroid(field) %>% st_geometry()
box = st_bbox(img) %>%
  st_as_sfc()

plot(st_geometry(box), graticule = T, axes = T, main = 'Карта полевых работ')
plotRGB(img, add = T)
plot(field, col = adjustcolor('green', alpha = 0.5), add = T)
plot(route, col = 'yellow', lwd = 2, add = T)
plot(stations, pch = 22, col = 'black', bg = 'white', cex = 2, add = T)
plot(start, pch = 21, col = 'yellow', bg = 'red', cex = 1.5, add = T)
text(start %>% st_coordinates(), "Начало маршрута", pos = 3, col = "white")
plot(end, pch = 21, col = 'yellow', bg = 'red', cex = 1.5, add = T)
text(end %>% st_coordinates(), "Конец маршрута", pos = 3, col = "white")
text(st_coordinates(stations), stations$Name, pos = 3, col = "white")
text(field %>% st_centroid() %>% st_coordinates(), p_area, col = "black")
scalebar(divs = 2, below = 'м', type = 'bar', d =500)


# plot(field, col = adjustcolor('green', alpha = 0.5), add = T)
# plot(route, col = 'yellow', lwd = 2, add = T)
# plot(field, col = adjustcolor('green', alpha = 0.5))
# 
# plot(route %>% st_geometry(), lwd = 1, col = "yellow",
#      main = "Карта полевых работ", graticule = TRUE, axes = TRUE)
# plotRGB(img)
# plot( field %>% st_geometry(), col = color)
# text(field %>% st_centroid() %>% st_coordinates(), p_area, col = "white")
# plot( start %>% st_geometry(), pch = 16, col = "blue")
# text(start %>% st_coordinates(), "Начало маршрута", pos = 3, col = "white")
# plot( stations %>% st_geometry(), pch = 15, col = "red")
# text(st_coordinates(stations), stations$Name, pos = 3, col = "white")
# plot( end %>% st_geometry(), pch = 16, col = "blue")
# text(end %>% st_coordinates(), "Конец маршрута", pos = 3, col = "white")
# scalebar(1000)






# library(sf)
# library(raster)
# library(mapview)
# library(tidyverse)
# 
# 
# stations = read_csv('stations.csv') 
# route = read_csv('route.csv')
# field = read_csv('field.csv')
# image = stack('Snimok.tif') 

# route = st_linestring(cbind(route$X, route$Y)) %>% 
#   st_sfc() %>%
#   st_sf() %>% 
#   st_set_crs(4326) 
# field = st_linestring(cbind(field$X, field$Y)) %>%
#   st_sfc() %>% 
#   st_sf() %>% 
#   st_set_crs(4326) 
# stations = st_as_sf(stations, 
#                     coords = c("X", "Y"), 
#                     crs = 4326)
# mapview(stations)
# 
# 
# start = st_point(st_coordinates(route)[1, ]) %>% 
#   st_sfc() %>% 
#   st_sf()
# len = length(st_coordinates(route)[ , 1]) 
# end = st_point(st_coordinates(route)[len, ]) %>% 
#   st_sfc() %>% 
#   st_sf()
# 
# field = st_transform(field, as.character(crs(image)))
# route = st_transform(route, as.character(crs(image)))
# stations = st_transform(stations, as.character(crs(image)))
# 
# p_area = paste("Пашня \n", round(st_area(field) / 10000, digits = 1), "\n га")
# color = adjustcolor("green", alpha = 0.5)
# 
# 
# # plot(route %>% st_geometry(), lwd = 1, col = "yellow", 
# #      main = "Карта полевых работ", graticule = TRUE, axes = TRUE)
# # 
# 
# plotRGB(img, add = TRUE)
# plot(route %>% st_geometry(), lwd = 1, col = "yellow", 
#      main = "Карта полевых работ", graticule = TRUE, axes = TRUE, add = T)
# 
# # plot(add = TRUE, field %>% st_geometry(), col = color)
# plot(field, col = adjustcolor('green', alpha = 0.5), add = T)
# text(field %>% st_centroid() %>% st_coordinates(), p_area, col = "white")
# plot(add = TRUE, start %>% st_geometry(), pch = 16, col = "blue")
# text(start %>% st_coordinates(), "Начало маршрута", pos = 3, col = "white")
# plot(add = TRUE, stations %>% st_geometry(), pch = 15, col = "red")
# text(st_coordinates(stations), stations$Name, pos = 3, col = "white")
# plot(add = TRUE, end %>% st_geometry(), pch = 16, col = "blue")
# text(end %>% st_coordinates(), "Конец маршрута", pos = 3, col = "white")
# scalebar(1000)
# 
# mapview(route, color = 'black') +
#   mapview(field, color = 'green') + mapview(stations) + 
#   mapview(start, color = 'red') + mapview(end, color = 'red') 
# 
