library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(eurostat)
library(nasapower)
library(soilDB)
library(RColorBrewer)

soils = c('cecil',  'altavista', 'lloyd', 'wickham') 
series = fetchOSD(soils, extended = TRUE)

hillpos = series$hillpos %>% 
  select(-7,-8) %>% 
  pivot_longer(2:6) %>% 
  mutate(position = factor(name, levels = unique(name)))

ggplot(hillpos, mapping = aes(y = series, x = value, fill = position)) +
  geom_col(color = 'black', size = 0.1) + #, position = 'fill') +
  scale_fill_viridis_d(direction = -1) +
  ggtitle('Empirical probability for hillslope position') +
  xlab('Probability') +
  ylab('soilseries') +
  labs(fill = 'Hillslope position')


pm_origin = series$pmorigin

ggplot(pm_origin, mapping = aes(x = '', y = P, fill = pmorigin)) +
  geom_col(color = 'black', size = 0.2) +
  coord_polar(theta = 'y') + 
  facet_wrap(~series) +
  scale_y_continuous(breaks = seq(0, 0.95, 0.05), labels = seq(0, 0.95, 0.05)) + 
  scale_fill_manual(values = c(brewer.pal(8, 'Set3'), brewer.pal(8, 'Set1'))) + 
  labs(fill = 'Origin') +
  ggtitle('Empirical probability for parent materal origin') 


temp = series$climate.monthly %>% 
  filter(!str_detect(variable, 'Potential'))

ggplot(temp, mapping = aes(x = month, y = q50, color = series, group = series)) +
  geom_line(size = 1) +
  geom_point() +
  ylab('mm') +
  xlab("Month") +
  ggtitle('Precipitation') +
  labs(color = 'Series') +
  scale_x_discrete(labels = month.abb)
