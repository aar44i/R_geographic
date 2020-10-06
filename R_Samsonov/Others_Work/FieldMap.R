library(sf)
library(raster)
library(dplyr)
library(Cairo)

setwd('C:/Users/Lera/Desktop/4 level/MKK/dates 6')

# Считываем снимок
snimok <- stack('Snimok.tif')

# Искомые точки
stations <- read.csv('stations.csv')
p1 <- st_point(c(stations[1, 3], stations[1, 4])) # координаты станции 1
p2 <- st_point(c(stations[2, 3], stations[2, 4])) # координаты станции 2
pg <- st_sfc(list(p1, p2))
ps <- st_sf(stations, geometry = pg)
st_crs(ps) <- st_crs(4326)
ps_UTM <- st_transform(ps, 32637)


# Маршрут
routeread <- read.csv('route.csv')
route <- routeread[, c(-1,-4)]
route_cor <- data.matrix(route, rownames.force = NA)
route_line <- st_linestring(route_cor) 
route_line <- st_sfc(route_line) 
st_crs(route_line) <- st_crs(4326)  # задается система координат
route_lUTM <- st_transform(route_line, 32637)  # перевод координат в систему UTM N37
rp1 <- st_cast(route_lUTM, 'POINT') [1]  # первая точка маршрута
rp2 <- tail(st_cast(route_lUTM, 'POINT'), n = 1)  # последняя точка маршрута

# Полигон пашни
fieldread <- read.csv('field.csv')
field <- fieldread[, c(-1, -4)]
field_cor <- data.matrix(field, rownames.force = NA)
field_line <- st_multilinestring(list(field_cor))  # линии, ограничивающие пашню
field_line_n <- st_node(field_line)  
field_pol <- st_polygonize(field_line_n)  # полигон по линии пашни
field_pol <- st_sfc(field_pol) 
st_crs(field_pol) <- st_crs(4326)
field_polUTM <- st_transform(field_pol, 32637)

# Нахождение центра пашни в га
squarefield1 <- st_area(field_polUTM)
squarefield2 <- round(squarefield1 / 10000, 1) 
squarefield2 # площадь пашни

squarefield <- st_bbox(field_polUTM)  # прямоугольник, ограничивающий пашню
middleX <- mean(c(squarefield['xmin'], squarefield['xmax']))  # центр по X
middleY <- mean(c(squarefield['ymin'], squarefield['ymax']))  # центр по Y
middle <- st_point(c(middleX, middleY))  # соединяем обе середины. находим середину прямоугольника
middle_sq <- st_sfc(middle)
st_crs(middle_sq) <- st_crs(4326)
middleUTM <- st_transform(middle_sq, 32637)

# Добавление координатной сетки
box <- snimok %>% 
  extent() %>% 
  as('SpatialPolygons') %>% 
  st_as_sf() %>% 
  st_set_crs(st_crs(32637))

grt <- st_graticule(box)

# Создание карты в PNG
CairoPNG('FieldMap.png', width = 800, height = 600, pointsize = 12)
par(mar = c(4,3,4,2))

plot(box,
     graticule = grt,
     main = 'Карта полевых работ',
     axes = TRUE)


plotRGB(snimok,
        add = TRUE)

plot( st_geometry(grt), 
      col = 'grey10',
      add = TRUE) # координатная сетка

plot(route_lUTM,
     col = 'yellow1',
     lwd = 1.2,
     add = TRUE) # линия маршрута

plot(st_geometry(rp1),
     col = 'red',
     pch = 19,
     cex = 1,
     add = TRUE) # точка начала маршрута

plot(st_geometry(rp2),
     col = 'red',
     pch = 19,
     cex = 1,
     add = TRUE) # точка конеца маршрута

text(st_coordinates(rp1),
     labels = 'Начало маршрута',
     pos = 4,
     cex = 1,
     col = 'white') # подпись начала маршрута

text(st_coordinates(rp2),
     labels = 'Конец маршрута',
     pos = 4,
     cex = 1,
     col = 'white') # подпись конца маршрута

plot(st_geometry(ps_UTM),
     col = 'black',
     pch = 22,
     cex = 2,
     add = TRUE)

ps_UTM$Name <- c('Сенокос', 'Африка')

text(st_coordinates(ps_UTM),
     labels = ps_UTM$Name,
     pos = 1,
     cex = 1,
     col = 'white') # точки искомых пунктов

fieldcol <- adjustcolor('lawngreen', alpha = 0.5)
plot(field_polUTM,
     col = fieldcol, 
     main = middleUTM,
     border = 'black',
     add = TRUE)
text(st_coordinates(middle_sq),
     col = 'black',
     labels = 'Пашня\n35.8\nга',
     cex = 0.8) # полигон пашни

scalebar(500, 
         adj = c(2, 2),
         type = 'bar', 
         divs = 2,
         below = "м")  # добавление масштабной линейки

dev.off()  #завершение рисования

