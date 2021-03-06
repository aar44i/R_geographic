library(sf)
library(raster)
library(dplyr)
library(Cairo)

setwd('C:/Users/Lera/Desktop/4 level/MKK/dates 6')

# ��������� ������
snimok <- stack('Snimok.tif')

# ������� �����
stations <- read.csv('stations.csv')
p1 <- st_point(c(stations[1, 3], stations[1, 4])) # ���������� ������� 1
p2 <- st_point(c(stations[2, 3], stations[2, 4])) # ���������� ������� 2
pg <- st_sfc(list(p1, p2))
ps <- st_sf(stations, geometry = pg)
st_crs(ps) <- st_crs(4326)
ps_UTM <- st_transform(ps, 32637)


# �������
routeread <- read.csv('route.csv')
route <- routeread[, c(-1,-4)]
route_cor <- data.matrix(route, rownames.force = NA)
route_line <- st_linestring(route_cor) 
route_line <- st_sfc(route_line) 
st_crs(route_line) <- st_crs(4326)  # �������� ������� ���������
route_lUTM <- st_transform(route_line, 32637)  # ������� ��������� � ������� UTM N37
rp1 <- st_cast(route_lUTM, 'POINT') [1]  # ������ ����� ��������
rp2 <- tail(st_cast(route_lUTM, 'POINT'), n = 1)  # ��������� ����� ��������

# ������� �����
fieldread <- read.csv('field.csv')
field <- fieldread[, c(-1, -4)]
field_cor <- data.matrix(field, rownames.force = NA)
field_line <- st_multilinestring(list(field_cor))  # �����, �������������� �����
field_line_n <- st_node(field_line)  
field_pol <- st_polygonize(field_line_n)  # ������� �� ����� �����
field_pol <- st_sfc(field_pol) 
st_crs(field_pol) <- st_crs(4326)
field_polUTM <- st_transform(field_pol, 32637)

# ���������� ������ ����� � ��
squarefield1 <- st_area(field_polUTM)
squarefield2 <- round(squarefield1 / 10000, 1) 
squarefield2 # ������� �����

squarefield <- st_bbox(field_polUTM)  # �������������, �������������� �����
middleX <- mean(c(squarefield['xmin'], squarefield['xmax']))  # ����� �� X
middleY <- mean(c(squarefield['ymin'], squarefield['ymax']))  # ����� �� Y
middle <- st_point(c(middleX, middleY))  # ��������� ��� ��������. ������� �������� ��������������
middle_sq <- st_sfc(middle)
st_crs(middle_sq) <- st_crs(4326)
middleUTM <- st_transform(middle_sq, 32637)

# ���������� ������������ �����
box <- snimok %>% 
  extent() %>% 
  as('SpatialPolygons') %>% 
  st_as_sf() %>% 
  st_set_crs(st_crs(32637))

grt <- st_graticule(box)

# �������� ����� � PNG
CairoPNG('FieldMap.png', width = 800, height = 600, pointsize = 12)
par(mar = c(4,3,4,2))

plot(box,
     graticule = grt,
     main = '����� ������� �����',
     axes = TRUE)


plotRGB(snimok,
        add = TRUE)

plot( st_geometry(grt), 
      col = 'grey10',
      add = TRUE) # ������������ �����

plot(route_lUTM,
     col = 'yellow1',
     lwd = 1.2,
     add = TRUE) # ����� ��������

plot(st_geometry(rp1),
     col = 'red',
     pch = 19,
     cex = 1,
     add = TRUE) # ����� ������ ��������

plot(st_geometry(rp2),
     col = 'red',
     pch = 19,
     cex = 1,
     add = TRUE) # ����� ������ ��������

text(st_coordinates(rp1),
     labels = '������ ��������',
     pos = 4,
     cex = 1,
     col = 'white') # ������� ������ ��������

text(st_coordinates(rp2),
     labels = '����� ��������',
     pos = 4,
     cex = 1,
     col = 'white') # ������� ����� ��������

plot(st_geometry(ps_UTM),
     col = 'black',
     pch = 22,
     cex = 2,
     add = TRUE)

ps_UTM$Name <- c('�������', '������')

text(st_coordinates(ps_UTM),
     labels = ps_UTM$Name,
     pos = 1,
     cex = 1,
     col = 'white') # ����� ������� �������

fieldcol <- adjustcolor('lawngreen', alpha = 0.5)
plot(field_polUTM,
     col = fieldcol, 
     main = middleUTM,
     border = 'black',
     add = TRUE)
text(st_coordinates(middle_sq),
     col = 'black',
     labels = '�����\n35.8\n��',
     cex = 0.8) # ������� �����

scalebar(500, 
         adj = c(2, 2),
         type = 'bar', 
         divs = 2,
         below = "�")  # ���������� ���������� �������

dev.off()  #���������� ���������

