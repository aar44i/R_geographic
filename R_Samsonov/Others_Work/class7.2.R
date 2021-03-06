library(sf)
library(openxlsx) # ������ ������ Excel
library(lattice) # ����� lattice ������������ ��� ���������� ����� ����/��������
library(classInt) # ����� classInt ��������� ������ ������������� ������
library(RColorBrewer) # �������� �����

library(sp) # ����� sp ����� ��� ��������� ������� Spatial, ����������� ��� �������� ���� ���������� lattice

# ������ ����-���� � ��������� ��������
reg <- read_sf("Regions.gpkg")
## View(reg)
# ������ ������� �� �����������
tab <- read.xlsx("Regions.xlsx")
## View(tab)
# ����� �������� ����������� ��������� �� �������, 
# ����� �� ����� ���� � �������� �������:
tab[3:11] <- tab[3:11] / 1000000
head(tab)
reg <- merge(reg, tab, by.x="TARGET_FID", by.y="FID")
## View(reg)
# ������� ����� ������� � ����������
nclasses <- 5

# ���������� �������� ����� �� ������-�������� � ��������
ramp <- colorRampPalette(c("mistyrose", "red"))

intervals <- classIntervals(reg$X2015, n = nclasses, style = "equal")

# ������� ���������� ������� ����� ����� $brks
intervals$brks

plot(intervals, pal = ramp(nclasses), cex=0.5, main = "������ ��������� MIN/MAX")
intervals <- classIntervals(reg$X2015, n = nclasses, style = "pretty")
intervals$brks
plot(intervals, pal = ramp(nclasses), cex=0.5, main = "����������� ������ ���������")
intervals <- classIntervals(reg$X2015, n = nclasses, style = "quantile")
intervals$brks
plot(intervals, pal = ramp(nclasses), cex=0.5, main = "�������� (�������������������)")
intervals <- classIntervals(reg$X2015, n = nclasses, style = "jenks")
intervals$brks
plot(intervals, pal = ramp(nclasses), cex=0.5, main = "������������ ���������")
data <- as.vector(as.matrix(tab[3:11]))

# ������������� ��� ��� ������� ������������ ����������:
intervals <- classIntervals(data, n = nclasses, style = "jenks")
intervals$brks

plot(intervals, pal = ramp(nclasses), cex=0.5, main = "������������ ���������")
spreg <- as(reg, 'Spatial')
spplot(spreg, 
       zcol = c("X1959", "X1979", "X2002", "X2014"), 
       at = intervals$brks, 
       col.regions = ramp(nclasses)
)
names <- list(at = intervals$brks, 
              labels = intervals$brks, 
              cex = 1.0
)

# ������ �������� ��������� ������� � ���� ������. �������� labels �������� �� �������, � width � �� ������
legend <- list(labels = names, width=3.0)

# �������, ��������� ������� ����� ���������� � �������� colorkey=
spplot(spreg, 
       zcol = c("X1959", "X1979", "X2002", "X2014"), 
       at = intervals$brks, 
       col.regions = ramp(nclasses),
       colorkey = legend
)
breaks <-c(0,1,2.5,5,10,15)

nclasses <- length(breaks) - 1

intervals <- classIntervals(data, nclasses, style = "fixed", fixedBreaks=breaks)

plot(intervals, pal = ramp(nclasses), cex=0.5, main = "���������������� ���������")

names <- list(at = intervals$brks, 
              labels = intervals$brks, 
              cex = 1.0
)

legend <- list(labels = names, width=3.0)

spplot(spreg, 
       zcol = c("X1959", "X1979", "X2002", "X2014"),
       names.attr = c("1959", "1979", "2002", "2014"),
       at = intervals$brks, 
       col.regions = ramp(nclasses),
       colorkey = legend
)
# ��������� ��������� ����������� ���:
centers <- st_centroid(reg)
head(centers)
plot(reg %>% st_geometry())
plot(centers %>% st_geometry(), pch = 20, add = TRUE)
# ����� ������������� ������������� ��������� �������
fixed.breaks <- c(0, 500, 1000, 2000, 5000, 10000, 15000)

# ���������� ���������� ��������� ��������
nbreaks <- length(fixed.breaks)

# ���������� ���������� �������
nclasses <- nbreaks-1

# ���������� ������ ��� �������� ��������
size.classes <- vector(mode = 'numeric', length = nclasses)

# ������� ������� ������� ������
size.classes[1] <- 2 

# �������� ��������� ������� �� �������� ���������� � 1.3 ����
for(i in 2:nclasses){
  size.classes[i] <- 1.3 * size.classes[i-1]
}

# ���������, ��� ����������:
plot(1:nclasses, 
     rep(0, nclasses), 
     pch = 20, 
     cex = size.classes)
# �������������� ������ ����������:
size1 <- cut(centers$PopUrban, 
             breaks = fixed.breaks, 
             labels = size.classes,
             include.lowest = TRUE)
size2 <- cut(centers$PopRural, 
             breaks = fixed.breaks, 
             labels = size.classes,
             include.lowest = TRUE)
size <- as.numeric(c(as.character(size1), 
                     as.character(size2)))

columns <- c("PopUrban", "PopRural")

# ��������� ����� �������� ��������������� �����
diag.color <- adjustcolor("green4", alpha.f = 0.5) 

# ������� ������� �����
spcenters <- centers %>% as('Spatial')
spplot(spcenters, 
       zcol = columns,
       cuts = fixed.breaks, # ��� ��������� ������� ������� ������� ����������� � ��������� cuts
       cex = size, # ������ ��������� ����� ����� ���� ������
       pch = 20,   # ������� pch ��������, ��� ������������ ����� ������������� � ������� �������� ��������
       col.regions = diag.color, # ������������� ������� �������������� ����
       names.attr = c("���������", "��������"),
       main = list("���������", cex = 2.0, just="centre"),
       as.table = TRUE     # ������������ ���������� ���� ������ ����, ����� �������
)
# ���������� ������ ���������� �������, ������� ��� �������, ���� � �������:
legend.points <- list(pch = 20, 
                      col = diag.color, 
                      cex = size.classes)
# ������� ��������� ������� ��� ����� � ������ ������ ����������:
low <- fixed.breaks[1:nbreaks-1]
high <- fixed.breaks[2:nbreaks]

# ������� ������� ���� � �����. ����� ������ ���� ��������� ��� ������,
# ������� ������������� ����������� ��������� ������� � list():
labels = list(paste(low, " � ", high))

# �������, ���������� ��������� �������:
legend <- list(points = legend.points, # �������
               text = labels,          # �������
               columns = 1,            # ���������� ��������
               between = 0,            # ���������� ����� ��������
               reverse.rows = TRUE,    # ���������� ���������
               padding.text = 10.0,    # ������ ������� �� �������
               title = "���.���",      # ���������
               cex.title = 1.3         # ������� ������ ���������
)
# �������� �����
spplot(spcenters, 
       zcol = columns,
       cuts = fixed.breaks,
       cex = size,
       pch = 20,
       col.regions = diag.color,
       key.space="right",   # �������������� �������
       key = legend,       # �������
       names.attr = c("���������", "��������"),
       main = list("���������", cex = 2.0, just="centre"),
       par.settings = list(strip.background = list(col = "lightgrey")), # ������ ���� ����� ������ ������ �� �����
       par.strip.text = list(cex = 0.9), # ��������� ������ ������ � ������ ������
       as.table = TRUE     # ������������ ���������� ���� ������ ����, ����� �������
)
# ������ �������� ������
splakes <- st_read("Lakes.gpkg") %>% as('Spatial')
sprivers <- st_read("Rivers.gpkg") %>% as('Spatial')
spcities <- st_read("Cities.gpkg") %>% as('Spatial')

# ��������� ����������� �������
layout.cities <- list("sp.points", 
                      spcities, 
                      pch = 19, 
                      cex = 0.75, 
                      col = "black")

# ��������� ����������� ���
layout.rivers <- list("sp.lines", 
                      sprivers, 
                      col = "steelblue3", 
                      lwd = 1, 
                      first = TRUE)

# �������� � ���� ������ ��� �������� � spplot()
layout <- list(layout.rivers, layout.cities)
# ������� �������� 
layout.reg <- list("sp.polygons", 
                   spreg,
                   lwd = 0.5,
                   col = "slategrey", 
                   first = TRUE)

# �����
layout.lakes <- list("sp.polygons", 
                     splakes,
                     col = "steelblue3",
                     fill = "lightblue1", 
                     lwd = 0.5, 
                     first = TRUE)

# ������� �������
layout.text <- list("sp.text", 
                    coordinates(spcities), 
                    spcities$name_2, 
                    cex = 0.75, 
                    pos = 2)

layout <- list(layout.reg, 
               layout.rivers, 
               layout.lakes, 
               layout.cities,
               layout.text)
# �������� ������� ��������
extent <- st_bbox(reg)

# �������� �����
spplot(spcenters, 
       zcol = columns,
       cuts = fixed.breaks,
       cex = size,
       pch = 20,
       col.regions = diag.color,
       xlim = c(extent[1], extent[3]),
       ylim = c(extent[2], extent[4]),
       key.space="right",   # �������������� �������
       sp.layout = layout, # �������������� ����
       key = legend,       # �������
       names.attr = c("���������", "��������"),
       main = list("���������", cex = 2.0, just="centre"),
       par.settings = list(strip.background = list(col = "lightgrey")),
       par.strip.text = list(cex = 0.9), 
       as.table = TRUE     # ������������ ���������� ���� ������ ����, ����� �������
)
library(raster)
# ��������� ��������� ����
temp <- raster("tmean_1.tif")

# ��������, ����� �� ����� �����:
class(temp)

# ��������� ������� ����� �� �����:
plot(temp)
temp <- temp/10
plot(temp)
# �������� �������� ������� � ����� �����
pal <- colorRampPalette(c("dodgerblue4","dodgerblue1"))

# �������� ������� � �������� �����������
min <- cellStats(temp, "min")
max <- cellStats(temp, "max")

# ��������� ������ ��������� ���������� ������ 2.5 ��������
step <- 2.5

# �������� ������� ������ ����������, ��������� ������� fullseq() �� ������ scales
library(scales)
levels <- fullseq(c(min, max), step)

# ��������� ���������� ���������� ����������
nclasses <- length(levels)-1

# ������������� ������
plot(temp, 
     breaks = levels, # � breaks ����������� ������� ����������
     col = pal(nclasses))
# ������������� ������
plot(temp, 
     breaks = levels, # � breaks ����������� ������� ����������
     col = pal(nclasses))
plot(spreg, 
     border = "black", 
     add = TRUE, 
     lwd = 0.3)
plot(sprivers, 
     col = "midnightblue", 
     add = TRUE, 
     lwd = 1)
plot(splakes, 
     col = "steelblue1", 
     add = TRUE, 
     lwd = 0.5)
plot(spcities, 
     add = TRUE, 
     pch = 20)

pts <- coordinates(spcities)
text(pts[,1], pts[,2], 
     labels = spcities$name_2, 
     cex = 0.8, pos = 3)
grid(col="grey20")
colors <- c("dodgerblue4","white","orange","firebrick")
pal <- colorRampPalette(colors)

# �����, ������, ��������������� ���������� ����� ��� ���������� �����������:
ncolors <- length(colors)
image(x = 1:ncolors,
      y = 1,
      z = as.matrix(1:ncolors),
      col = colors)
# ������ �������, ��� ������� � �������� ���������� � ������ �����
min <- cellStats(temp, "min")
max <- cellStats(temp, "max")

# �������� ������ ��� �������, ����� ��������� �� �������� � ����� ������������ �� ���� ������
rasters <- vector(mode = "list", length = 12)

# ������� ������ ����� � ������
rasters[[1]] = temp

# ��������� �� ��������� ������ � ������� �� �������� � ��������� � �������
for (i in 2:12){
  # ���������� ��� �����
  file <- paste("tmean_", i, ".tif", sep="")
  
  # ��������� ����
  temp <- raster(file)/10
  
  # ������� ������� ����� � ������
  rasters[[i]] <- temp
  
  # �������� ������� ������� � ��������
  vmin <- cellStats(temp, "min")
  vmax <- cellStats(temp, "max")
  
  # �������, �� ������ �� ������� ��������, ��� ��� �������, ��� �� ������
  if(vmin < min){
    min <- vmin # ���� ��, �� ������� ������� �� ������� ��������
  }
  
  # ���������� ��� ���������
  if(vmax > max){
    max <- vmax
  }
}

cat("����������� ����������� - ", min)
cat("������������ ����������� - ", max)

# ���� ������������, ��� ����������� ����� �����������������
# � ����� 2.5 �������, �� ����� ������� ����� ��������� �������������

# ���������� ������� �������, ����������� �������� ������, 
# ��������� ������� ������� fullseq() �� ������ scales
levels <- fullseq(c(min, max), step)

# ���������� ���������� � ����� ����� ���������� ��������� �������� -1
nclasses <- length(levels)-1

# ������� ������ ������ �� �����
plot(temp, 
     breaks = levels, 
     col = pal(nclasses))

# ������� ����� ������� � ����������. �� ������ ����� �������� ��������
# legend.width, � �� �� ���������� �� ������ � legend.shrink.
# ���� legend.shrink = 1, �� ������� ����� �� ������ ����� �� ��� � �����:

plot(temp, 
     breaks = levels, 
     col = pal(nclasses), 
     legend.shrink = 1, 
     legend.width = 1.5)

# �� ����� ������ ����� ����� ��������� ��������� ������� ���������� ����, ���������
# ��������� legend.args � axis.args. ��� ��������� �������� ��������

# legend.args  �������� �� ��������� �������,
# axis.args �������� �� ����� ��������.

legendargs <- list(text='��', 
                   side=3, 
                   font=2, 
                   line=0.3, 
                   cex=0.8)

axisargs <- list(cex.axis=0.7)

# ������� ���� �����:
par(mar=c(6,3,5,1)+0.1)
# � �� 2 ������� �� ������
par(mfrow = c(2,2))

months<-c("������","�������", "����", "������", "���", "����", "����", "������", "��������", "�������", "������", "�������")

for (i in 1:12){
  plot(rasters[[i]], 
       breaks = levels, 
       col = pal(nclasses), 
       legend.mar = 4.5, 
       legend.shrink = 1, 
       legend.width = 1.5, 
       legend.args = legendargs, 
       axis.args = axisargs,
       main = months[i]
  )
  
  plot(spreg, 
       border = "black", 
       add = TRUE, 
       lwd = 0.3)
  plot(sprivers, 
       col = "midnightblue", 
       add = TRUE, 
       lwd = 1)
  plot(splakes, 
       col = "steelblue1", 
       add = TRUE, 
       lwd = 0.5)
  plot(spcities, 
       add = TRUE, 
       pch = 20)
  
  pts <- coordinates(spcities)
  text(pts[,1], pts[,2], 
       labels = spcities$name_2, 
       cex = 0.8, pos = 3)
  grid(col="grey20")
}
png("October.png", width = 500, height = 500)
plot(rasters[[10]],
     breaks = levels,
     col = pal(nclasses),
     legend.mar = 4.5,
     legend.shrink = 1,
     legend.width = 1.5,
     legend.args = legendargs,
     axis.args = axisargs,
     main = months[10]
)

dev.off() # �����: ��������� ��������� � ����
png("Allmonths.png", width = 40, height = 30, units = "cm", res = 300)
par(mar=c(5,4,5,6))
par(mfrow = c(3,4))
for (i in 1:12){
  plot(rasters[[i]],
       breaks = levels,
       col = pal(nclasses),
       legend.mar = 4.5,
       legend.shrink = 1,
       legend.width = 1.5,
       legend.args = legendargs,
       axis.args = axisargs,
       main = months[i]
  )
}
library(Cairo)
dev.off() # �����: ��������� ��������� � ����
# �� ��������� ������ � ������ �������� � ������
CairoPDF("Results.pdf", width = 10, height = 10)
par(mfrow=c(2,2))
par(mar=c(5,4,5,6))

for(i in 1:12){
  plot(rasters[[i]],
       breaks = levels,
       col=pal(nclasses),
       legend.mar=4.5,
       legend.shrink = 1,
       legend.width = 1.5,
       legend.args = legendargs,
       axis.args = axisargs,
       axes = FALSE
  )

  # ����� �������� ���������� �� ������ ��������� ����� � ��������:
  plot(spreg, border="black", add=TRUE, lwd=0.3)
  plot(sprivers, col="midnightblue", add=TRUE, lwd=1)
  plot(splakes, col="steelblue1", add=TRUE, lwd=0.5)
  plot(spcities, add=TRUE, pch=20)
  text(pts[,1], pts[,2], labels = spcities$name_2, cex = 0.8, pos = 3)

  # ������� ��� �� ��� X ������ 5 ��������, � �� Y � 4 �������
  xseq = seq(30,55,by=5)
  yseq = seq(46,60,by=4)

  # ���������� ������� ���������, ���������� ������ �������:
  xlabels <- paste(xseq, "�", sep="")
  ylabels <- paste(yseq, "�", sep="")

  # ������� �� ����� ��� X � Y
  axis(1, at = xseq, labels = xlabels)
  axis(2, at = yseq, labels = ylabels)

  # ������� ����� �������, ��������� ������� abline():

  abline(h=yseq, lty=3, col="grey20")
  abline(v=xseq, lty=3, col="grey20")

  # ������� ��������� �����
  title(main=months[i], sub="�������������� �����������")

  # �������
  box("plot", lwd=2)
}
dev.off() # �����: ��������� ��������� � ����

#����� ��������� ���� ��������� lattice

library(lattice)
raster.stack <- stack(rasters)
spplot(raster.stack)  

colors <- c("cyan2","white","gold","deeppink")
min <- cellStats(raster.stack, "min")
max <- cellStats(raster.stack, "max")

names <- list(at = levels, labels = levels)
legend <- list(labels = names)

spplot(raster.stack,
       at = levels,
       col.regions = colorRampPalette(colors),
       names.attr = months,
       colorkey = legend)
