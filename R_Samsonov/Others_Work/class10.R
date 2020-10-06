library(sp)
library(sf)
library(raster)
library(classInt)

# ЛОКАЛЬНЫЕ ОПЕРАЦИИ

# Вычисление толщины покровного оледенения
setwd('C:/Users/Lera/Desktop/4 level/MKK/12.12.2017')
bed <- raster('etopo1_bed.tif')
ice <- raster('etopo1_ice.tif')
countries <- st_read('countries.gpkg')
borders <- countries %>% st_geometry()

par(mar = c(2,2,2,2))
par(mfrow = c(2,1))
classes <- classIntervals(values(bed), 20)
brks <- classes$brks
nclass <- length(brks) - 1

plot(bed, breaks = brks, col = gray.colors(nclass))
plot(ice, breaks = brks, col = gray.colors(nclass))

ice.depth <- ice - bed

plot(ice.depth, col = cm.colors(255))
plot(borders, border = 'black', lwd = 0.5, add = TRUE)

values(ice.depth)[values(ice.depth) <= 0] <- NA

plot(ice.depth, col = cm.colors(255))
plot(borders, border = 'black', lwd = 0.5, add = TRUE)

# Разность в численности населения

gpw2000 <- raster('gpw2000.tif')
gpw2015 <- raster('gpw2015.tif')
delta <- gpw2015 - gpw2000

par(mfrow = c(1,1))
par(mar = c(1,1,1,1))
intervals <- classIntervals(values(delta), 10, style = "quantile")
brks <- as.integer(intervals$brks)
pinkcyancol <- colorRampPalette(c("cyan","deeppink"))
plot(delta, #карта
     breaks = brks, 
     col = pinkcyancol(10), 
     legend = F)
plot(borders, border = 'black', lwd = 0.5, add = TRUE)

plot(delta, #легенда
     zlim = c(1,11),
     axis.args = list(at = 1:11,
                      labels = brks),
     col = pinkcyancol(10),
     legend.only = TRUE)


# ФОКАЛЬНЫЕ ОПЕРАЦИИ

# Фильтрация ЦМР (сглаживание) — фиксированное соседство
dem <- crop(ice, extent(-120, -75, 10, 40))
spplot(dem)

# Среднее
wgt <- matrix(1/9, 3, 3)
filtered <- focal(dem, w = wgt)
filtered2 <- dem %>% focal(wgt) %>% focal(wgt)
spplot(stack(dem, filtered))

# Гауссово
wgt <- focalWeight(dem, 0.5, "Gauss")
filtered <- focal(dem, wgt)
spplot(stack(dem, filtered))

# Выделение границ методом Собеля (резких перепадов значений)
wgt <- matrix(c(1, 2, 1,
                0, 0, 0,
                -1,-2,-1) / 4, 
              nrow=3)
filtered <- focal(dem, wgt)
plot(filtered,
     col = gray.colors(128))

faults <- (filtered < -1500) | (filtered > 1500)

faults[faults == 0] <- NA

plot(dem, col = rev(rainbow(20)), legend = FALSE)
plot(faults,
     col = 'black',
     legend = FALSE,
     add = TRUE)

# Морфометрия рельефа — фиксированное соседство
dem <- raster('dem_fergana.tif')
spplot(dem)

# углы наклона
slope <- terrain(dem, 
                 opt = 'slope', 
                 unit = 'degrees')
spplot(slope, col.regions = heat.colors(20))

# экспозиция
aspect <- terrain(dem, opt = 'aspect', unit = 'degrees')
spplot(aspect, col.regions = rainbow(20))

# отмывка
slope2 <- terrain(dem*50, opt = 'slope')
aspect2 <- terrain(dem*50, opt = 'aspect')

hill <- hillShade(slope2, aspect2, angle = 45, direction = 315)
plot(hill, col = gray.colors(128))
# Определение Евклидовых расстояний — расширенное соседство

# Чтение данных
roads <- st_read("roads.gpkg") # Дороги
poi <- st_read("poi_point.gpkg") # Точки интереса
rayons <- st_read("boundary_polygon.gpkg") # Границы районов
stations <- st_read("metro_stations.gpkg") # Станции метро
water <- st_read("water_polygon.gpkg") # Водные объекты

# Создаем пустой растр
r <-  raster(extent(as(stations, "Spatial")), nrows = 200, ncols = 200)

# Конвертируем ячейки в точки
cells <- r %>% as("SpatialPoints") %>% st_as_sf() %>% st_set_crs(st_crs(stations))

# Вычисляем расстояния
d <- st_distance(stations, cells)

r[] = apply(d, 2, min)

plot(r, col = rev(heat.colors(100)))
contour(r, lwd = 0.2, add= TRUE)
plot(water, col = 'blue', border = 'darkblue', add = TRUE)
plot(roads, lwd = 0.5, col = 'black', add = TRUE)
plot(stations, pch = 20, col = 'black', add = TRUE)
