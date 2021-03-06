library(tidyverse)
data(quakes, package = 'datasets')
# ������� ���������� �������������
# � �������� ����������
(nquakes = quakes %>% 
  group_by(mag) %>% 
  summarise(ncases = n())) # ���������� ����� � ������

summarise(group_by(quakes, mag), ncases = n())

grouped = group_by(quakes, mag)
result = summarise(grouped, 
                   ncases = n())
n