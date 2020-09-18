library(sp)
library(sf)
library(raster)
library(classInt)

# ��������� ��������
# ���������� ������� ���������� ����������

# ������ ������
bed <- raster('etopo1_bed.tif')
ice <- raster('etopo1_ice.tif')
countries <- st_read('countries.gpkg')
borders <- countries %>% st_geometry()

# ����������� ������
classes <- classIntervals(values(bed), 20)
brks <- classes$brks
nclass <- length(brks) - 1

plot(bed, 
     breaks = brks, 
     col = gray.colors(nclass),
     main = 'ETOPO Bedrock',
     legend = F)
plot(ice, 
     breaks = brks, 
     col = gray.colors(nclass),
     main = 'ETOPO Ice surface',
     legend = F)

# ���������� ��������
ice.depth <- ice - bed
plot(ice.depth, 
     col = cm.colors(255),
     main = '�������� ���������� ����������')
plot(borders, 
     border = 'black', 
     lwd = 0.5, 
     add = TRUE)

# ������� ������� ��� ������, � ������� ������� ���� ����� ����
ice.depth[ice.depth == 0] <- NA

plot(ice.depth, 
     col = cm.colors(255), 
     main = '�������� ���������� ����������')
plot(borders, 
     border = 'black', 
     lwd = 0.5, 
     add = TRUE)
# ��������� ��������

# ������� ����� �� ���
dem <- crop(ice, extent(-120, -75, 10, 40))
spplot(dem)

# �������
wgt <- matrix(c(1, 1, 1,
                1, 1, 1,
                1, 1, 1) / 9, 
              nrow = 3)
# �� ����� ���� ����� �������� ���:
# wgt <- matrix(1/9, 3, 3), �� ������ ����� �������� ��� �����������

# �������� ��������� ��� � ������� ���������� �������
filtered <- focal(dem, w = wgt)
spplot(stack(dem, filtered),
       names.attr=c('�������� ������', '����������� �������'))
# �������� (�������� 0.5 - ��� ����������� ���������� � �������� ��������� ������)
wgt <- focalWeight(dem, 0.5, "Gauss")
filtered <- focal(dem, wgt)
spplot(stack(dem, filtered),
       names.attr=c('�������� ������', '�������� �����������'))
# ������� ������:
wgt <- matrix(c(1, 2, 1,
                0, 0, 0,
                -1,-2,-1) / 4, 
              nrow=3)
filtered <- focal(dem, wgt)

# ��� ����������� �����������:
plot(filtered,
     col = gray.colors(128),
     main = '����������� �����������')

# ������� ��� ������, ���������� �������� ���������� �����������
faults <- (filtered < -1500) | (filtered > 1500)
faults[faults == 0] <- NA

# ������������� ���������
plot(dem, 
     col = rev(rainbow(20)),
     main = '������ ���������������� ������',
     legend = FALSE)
plot(faults,
     col = 'black',
     legend = FALSE,
     add = TRUE)

# ����������� ������� � ������������� ���������
dem <- raster('dem_fergana.tif')
spplot(dem)

# ���� �������
slope <- terrain(dem, opt = 'slope', unit = 'degrees')
spplot(slope, 
       col.regions = heat.colors(20),
       names.attr=c('���� �������'))

# ����������
aspect <- terrain(dem, opt = 'aspect', unit = 'degrees')
spplot(aspect, 
       col.regions = rainbow(20),
       names.attr=c('���������� ������'))
# �������
slope2 <- terrain(dem * 20, opt = 'slope')
aspect2 <- terrain(dem * 20, opt = 'aspect')

# ��������� angle � direction ������� hillShade ���������� ������ � ������ ��������� ���������:
hill <- hillShade(slope2, aspect2, angle = 45, direction = 315)
plot(hill, 
     col = gray.colors(128),
     main = '������� �������')
# ����������� ���������� ���������� � ����������� ���������

# ������ ������
roads <- st_read("roads.gpkg") # ������
poi <- st_read("poi_point.gpkg") # ����� ��������
rayons <- st_read("boundary_polygon.gpkg") # ������� �������
stations <- st_read("metro_stations.gpkg") # ������� �����
water <- st_read("water_polygon.gpkg") # ������ �������

# ������� ������ ����� � �������, ������ ������ �������
r <-  raster(extent(stations), nrows = 200, ncols = 200)

# ������������ ������ � �����
cells <- r %>% as("SpatialPoints") %>% st_as_sf() %>% st_set_crs(st_crs(stations))

# ��������� ����������
d <- st_distance(stations, cells)

# ������� ����������� ���������� ��� ������ ����� � ��������� 
# ����������� ���������� ������� ���������� ������
r[] = apply(d, 2, min)

# ������������� ���������
plot(r, 
     col = rev(heat.colors(100)),
     main = '���������� �� ��������� ������� �����')
contour(r, 
        lwd = 0.5, 
        add= TRUE)
plot(water, 
     col = 'blue', 
     border = 'darkblue', 
     add = TRUE)
plot(roads, 
     lwd = 0.2, 
     col = 'black', 
     add = TRUE)
plot(stations, 
     pch = 20, 
     col = 'black', 
     add = TRUE)