Sys.setlocale("LC_ALL","Russian")

setwd('d:/YandexDisk/������/R forecasts/������')
# ��� �� ��������� ��������� � ���������� �� ����������� �������
# save(prog_df, file = 'baikal.RData')
# � ��� �� ��������� �� ���� ������ � ������� R
load(file = 'baikal.RData')

# ����������� �� ��������
meteo <- read.csv('d:/YandexDisk/������/R forecasts/2020/MeteoMean.REZ', sep = '')
meteo$Data <- as.Date(strptime(as.character(meteo$Data), format = '%Y%m%d'))
meteo <- meteo[,c(1:3)]

# ������������� ������ ��� ����������� � ������
library(lubridate)
# ������ ������� � ������
meteo$year <- year(meteo$Data)
# c ��������
meteo$month <- month(meteo$Data)
# ������������ ����� ������ � �������� ��������
meteo$month <- month.abb[meteo$month]
# � ���������� � ������ � ��������
meteo$month <- factor(meteo$month, levels = month.abb, ordered = T)

# ��������� �������� �� �����
meteo_t_year <- aggregate(Temperat.C. ~ year, meteo, mean)
meteo_p_year <- aggregate(Precip.mm. ~ year, meteo, sum)

# ���������� ��� ���������� �� ������ �������� - ����
meteo_year <- merge(x = meteo_t_year, y = meteo_p_year, by = 'year')
# ���������� �������� - �� �� �� �����
m1 <- cbind(meteo_t_year, meteo_p_year)

# ��������� �� ����� � �������
meteo_t_month <- aggregate(Temperat.C. ~ year + month, meteo, mean)
meteo_p_month <- aggregate(Precip.mm. ~ year + month, meteo, sum)

# ���������� �� ���� ����� ���������
meteo_month <- merge(x = meteo_t_month, y = meteo_p_month, by = c('year', 'month'))

# ���������� �� ����� ��������� � ������� �� �������
all_df <- merge(x = prog_df, y = meteo_month, by = c('year', 'month'))
# ����� �� ���������� ������, ��� ����� ���, ���������� all = TRUE
all2 <-  merge(x = prog_df, y = meteo_month, by = c('year', 'month'), all = TRUE)

# ��������� ���
library(ggplot2)
library(reshape2)
# ����������� � ������� ������
mdf <- melt(all_df, id.vars = c('year', 'month'))
# ������ ����
mdf$date <- ymd(paste(mdf$year,  as.integer(mdf$month), 1))
# �������
ggplot(mdf, aes(x = date, y = value, col = variable)) + geom_point() + geom_line() +
  facet_wrap(.~variable, scales = 'free_y') + geom_smooth()
 

