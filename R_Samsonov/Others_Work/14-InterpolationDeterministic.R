library(sf)
library(sp)
library(dismo)
library(akima) # ���������� ��� ������������ �� ������ ������������
library(rgdal)
library(gstat) # ���������� ��� ����������������� ������������, ���������� ������� � IDW
library(raster)
library(plotly)
library(maptools)
library(lattice)
library(deldir) # ���������� ��� ���������� ������������ ������ � ��������� ��������
library(fields) # ���������� �������� ������� (������� ����������� ��������)
library(MBA) # ������������� �������� �������

setwd('C:/Users/Lera/Desktop/4 level/MKK/19.12.17')
# ������� ���������������� ������������� ������� �����
options(scipen=999)

# ������ ���� ���������������� ������
cities <- st_read("Italy_Cities.gpkg") # ������
rivers <- st_read("Italy_Rivers.gpkg") # ����
lakes <- st_read("Italy_Lakes.gpkg")   # �����

# ������ ��� � �������� ������ ������� �� ���������� �����
dem <- raster("gtopo.tif")

# ������ ������ �� �������
rainfall<-read.table("Rainfall.dat", header = TRUE)

# ������� �������� ������� �� ����������� �� ������ �������
coords <- cbind(rainfall$x, rainfall$y)
pts <- SpatialPointsDataFrame(coords, rainfall)

# �������� ����� ��� �������
rain.colors <- colorRampPalette(c("white", "dodgerblue", "dodgerblue4"))

# ����� ���������� ������� � �������������� ����� ������
rain.levels <- seq(0,80,by=10)
rain.ncolors <- length(rain.levels)-1

# �������� ����� ��� �������
dem.colors<-colorRampPalette(c("darkolivegreen4","lightyellow","orange", "firebrick"))

# ����� ����� ��� �������
dem.levels <- c(0,50,100,200,500,1000,1500,2000,2500,3000,3500,4000,5000)
dem.ncolors <- length(dem.levels) - 1

# ����� �������� ����� ��� ������� �������
dem.labels <- c(0,200,500,1000,1500,2000,2500,3000,3500,4000,5000)

par(mfrow = c(1,1))

# ����������� ��������� �������
args <- list(at = dem.labels, cex.axis = 0.7)

# ������ ������ ���������������� ��������
plot(dem, 
     breaks = dem.levels, 
     col = dem.colors(dem.ncolors),
     legend.width = 1.5,
     legend.shrink = 1.0,
     axis.args = args)

# ������� �����������
contour(dem, 
        levels = dem.levels, 
        add = TRUE, 
        col = rgb(0, 0, 0, 0.5))

# ������� �������� ���������������� ������
plot(rivers, 
     col = "midnightblue", 
     add = TRUE)

plot(lakes, 
     border = "midnightblue", 
     col = "lightblue", 
     add = TRUE)

# ������� ����� � �������
plot(pts,
     pch = 20,
     cex = 0.75,
     add = TRUE)

# ������� ������ ������ �����, ����� ��� �� ����������
plot(cities %>% st_geometry(),
     pch = 21,
     bg = rgb(1, 1, 1, 0.7),
     cex = 0.8,
     add = TRUE)

# ������� ������� �������
text(st_coordinates(cities)[,1],
     st_coordinates(cities)[,2],
     cities$name,
     pos = 4,
     col = "black")
# ���������� ����� ��� ������������
pts.grid <- spsample(pts, type = 'regular', cellsize = 10000)
plot(pts,
     pch = 20,
     cex = 0.75)
plot(pts.grid, pch = '+', cex = 0.75, add = TRUE)
pixels.grid <- SpatialPixels(pts.grid)
plot(pts,
     pch = 20,
     cex = 0.75)
plot(pixels.grid, add = TRUE, col = "black")
pts.grid <- spsample(pts, type = 'regular', cellsize = 1000)
pixels.grid <- SpatialPixels(pts.grid)

# �������� ���������� ����� � ��������������� �������, ��� ��� ����������:
coords.grid <- data.frame(coordinates(pts.grid))
names(coords.grid) <- c("x", "y")

# ������� ����� �������������� ������������� ������ �����:
box <- bbox(pts)
envelope <- extent(pts) %>% as.vector()

# ����� ���������� ������ (NEAREST NEIGHBOR)
voronoi.sp <- voronoi(pts, ext = envelope)
plot(voronoi.sp)
plot(pts, pch = 19, cex = 0.5, add = TRUE)

# ��������� �������� �������� ����������� ������������ ������:
edges <- deldir(pts$x, pts$y, rw = envelope) %>% triang.list()
plot(edges, border = 'blue', lwd = 0.5, add = TRUE)
# ������ � �������������� ��������� ������� � �������
layout <- list("sp.points", pts, pch = 20, col = "black")

spplot(voronoi.sp,
       zcol = "rain_24",
       at = rain.levels,
       col.regions = rain.colors(rain.ncolors),
       sp.layout = layout)
# ������� �����
raster.out <- raster(pixels.grid)

# ������� � ���� �������� ���������
rnn <- rasterize(voronoi.sp, 
                 raster.out, 
                 field = "rain_24")

# �������������:
plot(rnn, breaks = rain.levels, col=rain.colors(rain.ncolors))
plot(pts, pch=16, cex=0.5, col = "black", add = TRUE)
rain.colors3d <- colorRamp(c("white", "dodgerblue", "dodgerblue4"))

x <- unique(pts.grid@coords[,1]) # ������� ���������� ��������
y <- unique(pts.grid@coords[,2]) # ������� ���������� �����
z <- as.matrix(rnn) # ������� ������ � ���� �������

p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

# ������������ �� ������ ������������ (TRIANGULATION)

# ������������ �� ������ ������������ �������, ����� �������������� ����� ����� ��� SpatialPointsDataFrame:
pts.grid.df <- SpatialPointsDataFrame(pts.grid, data = coords.grid)

# �������������. �������� linear ������� � ���, ��� ���������� ����� �������� ����� ����� ������������ �������:
z.linear <- interpp(x = pts,
                    z = "rain_24", 
                    xo = pts.grid.df,
                    linear = TRUE)

# ��������� ��������� �� ����� ��������:
px.linear <- SpatialPixelsDataFrame(pts.grid.df, data = as.data.frame(z.linear$z))

# ������� ������ ���� raster, ������� ����� ��������������� � ��������� ����:

r.linear <- raster(px.linear)

# ������� ��� �������� ���������
plot(r.linear, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r.linear, levels = rain.levels, add = TRUE)
plot(edges, pch = 19, lwd = 0.5, lty = 3, cex = 0.5, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)
z <- as.matrix(r.linear)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))
# �������������
z.spline <- interpp(x = pts, 
                    z = "rain_24", 
                    xo = pts.grid.df,
                    linear = FALSE,
                    extrap = TRUE)

# ��������� ��������� �� ����� ��������:
px.spline <- SpatialPixelsDataFrame(pts.grid.df, data = as.data.frame(z.spline$rain_24))

# ������� ������ ���� raster:
r.spline <- raster(px.spline)

# ������������� ���������:
plot(r.spline, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r.spline, levels = rain.levels, add = TRUE)
plot(edges, pch = 19, lwd = 0.5, lty = 3, cex = 0.5, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)
z <- as.matrix(r.spline)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

# ����� ������� ��������� ���������� (IDW --- INVERSE DISTANCE WEIGHTED)

# ������������� ���������� �������:
z1 <- idw(rain_24 ~ 1, locations = pts, newdata = pts.grid, idp = 2.0)
z2 <- idw(rain_24 ~ 1, locations = pts, newdata = pts.grid, idp = 3.0)
z3 <- idw(rain_24 ~ 1, locations = pts, newdata = pts.grid, idp = 4.0)
z4 <- idw(rain_24 ~ 1, locations = pts, newdata = pts.grid, idp = 5.0)

# ��������� ��������� �� ����� ��������.
px1 <- SpatialPixelsDataFrame(z1, data = z1@data)
px2 <- SpatialPixelsDataFrame(z2, data = z2@data)
px3 <- SpatialPixelsDataFrame(z3, data = z3@data)
px4 <- SpatialPixelsDataFrame(z4, data = z4@data)

# ������� ������ ���� raster ��� ������������
r1 <- raster(px1, values = px1$var1.pred)
r2 <- raster(px2, values = px2$var1.pred)
r3 <- raster(px3, values = px3$var1.pred)
r4 <- raster(px4, values = px4$var1.pred)

# ������� ��� �������� ���������
plot(r1, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r1, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

plot(r2, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r2, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

plot(r3, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r3, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

plot(r4, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r4, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

# ������ ����� ����� ��������� ������ ��������������� ���������� lattice, ��������� �� � ��������� ����:
r <- stack(r1, r2, r3, r4)

spplot(r, 
       at = rain.levels, 
       col.regions = rain.colors(rain.ncolors),
       sp.layout = list("sp.points", pts, pch=20, cex=0.5, col='black')
)
z <- as.matrix(r3)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))
z30 <- idw(rain_24 ~ 1, locations = pts, newdata = pts.grid, idp = 30.0)
px30 <- SpatialPixelsDataFrame(z30, data = z30@data)
r30 <- raster(px30, values = px30$var1.pred)

plot(r30, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r30, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

# ������� ������� ����� ��������� ��������:
plot(voronoi.sp, border = "red", add = TRUE)

# ���������� ����������� � 3D:
z <- as.matrix(r30)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

# ���������� �������� ������� (RADIAL BASIS FUNCTIONS)

pred <- Tps(coords, pts$rain_24, scale.type = 'unscaled')
# ����� ����� ����� ��������������� �������� � ������� ������� predict():
z.tps <- predict(pred, coords.grid)

# �������������� ������ �������� ��������, ������� ��� ����� �������� � ���� data.frame ����� ������������ �� ����� ��������:
px.tps <- SpatialPixelsDataFrame(pts.grid, data = data.frame(z.tps))

# ����� ������� �������� ����������
names(px.tps) <- c("rain_24")

# ����������� � �����
r.tps <- raster(px.tps, values = px.tps$rain_24)

# �������� ��������� �����, ��� ��� ���������� ����������� ������� �� ������� �������� ��������:
tps.breaks = seq(-10,90,by=10)
tps.ncolors = length(tps.breaks) - 1

# ������������� ���������:
plot(r.tps, breaks = tps.breaks, col=rain.colors(tps.ncolors))
contour(r.tps, levels = seq(-10,90,by=10), add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)
z <- as.matrix(r.spline)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

# ������������� �������� ������� (HIERARCHICAL BASIS SPLINES)

mba.data <- cbind(pts$x, pts$y, pts$rain_24)
ratio <- (envelope[2] - envelope[1])/(envelope[4] - envelope[3])

# ����� ���� ������������� ����� ����������� ������������
pred <- mba.points(mba.data, coords.grid, n = 1, m = ratio)

# ����� �������� ���������� ��������
pred.z = data.frame(pred$xyz.est[,"z"])

# ��������� ��������� �� ����� ��������:
px.bspline <- SpatialPixelsDataFrame(pts.grid, data = pred.z)

# ������ �������� �������� ����������:
names(px.bspline) <- c("rain_24")

# �������� ����� ��� ������������ �����������
r.bspline <- raster(px.bspline, values = px.bspline$rain_24)

# �������, ��� ����������:
plot(r.bspline, breaks = seq(-10,90,by=10), col=rain.colors(10))
contour(r.bspline, levels = seq(-10,90,by=10), add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)
z <- as.matrix(r.bspline)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

# ���������� ������������� (GLOBAL APPROXIMATION)

# 1-� ������� -------------------------------------------------------------

# ������� ������ gstat. �������, ����������� � ��������� formula, ������ �����������
# � ������ ������, ������� ���������� � �������� data:
trend <- gstat(formula = rain_24 ~ 1, data = pts, degree = 1)

# ������������ ������������� �� �������� ����� �����:
z <- predict(trend, newdata = pts.grid)

# ��������� ��������� �� ����� ��������:
px <- SpatialPixelsDataFrame(z, data = z@data)

# ����������� � �����:
r.trend1 <- raster(px, values = px$var1.pred)

# ������������� ���������:
plot(r.trend1, breaks = tps.breaks, col=rain.colors(tps.ncolors))
contour(r.trend1, levels = tps.breaks, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

# ������ �������� ��� �� �� ����� ��� �������� 2 � 3:

# 2-� ������� -------------------------------------------------------------
trend <- gstat(formula = rain_24 ~ 1, data = pts, degree = 2)
z <- predict(trend, newdata = pts.grid)
px <- SpatialPixelsDataFrame(z, data = z@data)
r.trend2 <- raster(px, values = px$var1.pred)

# �������������:
plot(r.trend2, breaks = tps.breaks, col=rain.colors(tps.ncolors))
contour(r.trend2, levels = tps.breaks, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

# 3-� ������� -------------------------------------------------------------
trend <- gstat(formula = rain_24 ~ 1, data = pts, degree = 3)
z <- predict(trend, newdata = pts.grid)
px <- SpatialPixelsDataFrame(z, data = z@data)
r.trend3 <- raster(px, values = px$var1.pred)

# �������������:
plot(r.trend3, breaks = tps.breaks, col=rain.colors(tps.ncolors))
contour(r.trend3, levels = tps.breaks, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)
z <- as.matrix(r.trend1)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

z <- as.matrix(r.trend2)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

z <- as.matrix(r.trend3)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

# ��������� ������������� (LOWESS)

# 0-� ������� -------------------------------------------------------------
local.trend <- loess(rain_24 ~ x + y, pts, degree = 0, span = 0.07, normalize = FALSE)

# ���������� ������ � �������� ������
z.local.trend <- predict(local.trend, coords.grid)

# ��������� ��������� �� ����� ��������
px.local.trend  <- SpatialPixelsDataFrame(pts.grid, data = data.frame(z.local.trend))

# ������ �������� ����������
names(px.spline) <- c("rain_24")

# ����������� � ����� ��� ������������
r.local.trend0 <- raster(px.local.trend, values = px.spline$rain_24)

# �������������
plot(r.local.trend0, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r.local.trend0, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

# �������� ����������� �������� ��� �������� 1 � 2

# 1-� ������� -------------------------------------------------------------
local.trend <- loess(rain_24 ~ x + y, pts, degree = 1, span = 0.07, normalize = FALSE)
z.local.trend <- predict(local.trend, coords.grid)
px.local.trend  <- SpatialPixelsDataFrame(pts.grid, data = data.frame(z.local.trend))
names(px.spline) <- c("rain_24")
r.local.trend1 <- raster(px.local.trend, values = px.spline$rain_24)

plot(r.local.trend1, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r.local.trend1, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

# 2-� ������� -------------------------------------------------------------
local.trend <- loess(rain_24 ~ x + y, pts, degree = 2, span = 0.07, normalize = FALSE)
z.local.trend <- predict(local.trend, coords.grid)
px.local.trend  <- SpatialPixelsDataFrame(pts.grid, data = data.frame(z.local.trend))
names(px.spline) <- c("rain_24")
r.local.trend2 <- raster(px.local.trend, values = px.spline$rain_24)

plot(r.local.trend2, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r.local.trend2, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)
z <- as.matrix(r.local.trend0)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

z <- as.matrix(r.local.trend1)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

z <- as.matrix(r.local.trend2)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))
# degree = 1, span = 0.05 -------------------------------------------------

local.trend <- loess(rain_24 ~ x + y, pts, degree = 1, span = 0.05, normalize = FALSE)
z.local.trend <- predict(local.trend, coords.grid)
px.local.trend  <- SpatialPixelsDataFrame(pts.grid, data = data.frame(z.local.trend))
names(px.spline) <- c("rain_24")
r.local.trend.a05 <- raster(px.local.trend, values = px.spline$rain_24)

plot(r.local.trend.a05, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r.local.trend.a05, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

# degree = 1, span = 0.1 -------------------------------------------------

local.trend <- loess(rain_24 ~ x + y, pts, degree = 1, span = 0.1, normalize = FALSE)
z.local.trend <- predict(local.trend, coords.grid)
px.local.trend  <- SpatialPixelsDataFrame(pts.grid, data = data.frame(z.local.trend))
names(px.spline) <- c("rain_24")
r.local.trend.a10 <- raster(px.local.trend, values = px.spline$rain_24)

plot(r.local.trend.a10, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r.local.trend.a10, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)

# degree = 1, span = 0.2 -------------------------------------------------

local.trend <- loess(rain_24 ~ x + y, pts, degree = 1, span = 0.2, normalize = FALSE)
z.local.trend <- predict(local.trend, coords.grid)
px.local.trend  <- SpatialPixelsDataFrame(pts.grid, data = data.frame(z.local.trend))
names(px.spline) <- c("rain_24")
r.local.trend.a20 <- raster(px.local.trend, values = px.spline$rain_24)

plot(r.local.trend.a20, breaks = rain.levels, col=rain.colors(rain.ncolors))
contour(r.local.trend.a20, levels = rain.levels, add = TRUE)
plot(pts, pch = 20, cex = 0.5, add = TRUE)
z <- as.matrix(r.local.trend.a05)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

z <- as.matrix(r.local.trend.a10)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))

z <- as.matrix(r.local.trend.a20)
p <- plot_ly(x = x, 
             y = y, 
             z = z, 
             type = "surface",
             colors = rain.colors3d)
layout(p, scene = list(aspectratio = 
                         list(x = 1, y = 1, z = 0.3)
))