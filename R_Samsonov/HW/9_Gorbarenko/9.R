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
field = st_transform(field, as.character(crs(image)))
route = st_transform(route, as.character(crs(image)))
stations = st_transform(stations, as.character(crs(image)))

mapview(route, color = 'black') +
  mapview(field, color = 'green') + mapview(stations) + 
  mapview(start, color = 'red') + mapview(end, color = 'red') 

