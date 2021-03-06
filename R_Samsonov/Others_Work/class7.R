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
View(reg)
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
