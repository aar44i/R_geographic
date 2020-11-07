library(sf)
library(raster)
library(mapview)
library(tidyverse)


stations = read_csv('stations.csv') 
route = read_csv('route.csv')
field = read_csv('field.csv')
image = stack('Snimok.tif') 

route = st_linestring(cbind(route$X, route$Y)) %>% 
  st_sfc() %>%
  st_sf() %>% 
  st_set_crs(4326) 
field = st_linestring(cbind(field$X, field$Y)) %>%
  st_sfc() %>% 
  st_sf() %>% 
  st_set_crs(4326) 
stations = st_as_sf(stations, 
                    coords = c("X", "Y"), 
                    crs = 4326)



start = st_point(st_coordinates(route)[1, ]) %>% 
  st_sfc() %>% 
  st_sf()
len = length(st_coordinates(route)[ , 1]) 
end = st_point(st_coordinates(route)[len, ]) %>% 
  st_sfc() %>% 
  st_sf()

p_area = paste("Пашня \n", round(st_area(field) / 10000, digits = 2), "\n га")
color = adjustcolor("green", alpha = 0.5)

mapview(route, color = 'black') +
  mapview(field, color = 'green') + mapview(stations) + 
  mapview(start, color = 'red') + mapview(end, color = 'red') 

