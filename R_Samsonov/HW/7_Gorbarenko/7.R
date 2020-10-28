library(tidyverse)
library(googlesheets4)
library(lubridate)
# library(rlang)

catalog = read_sheet('1Q6HCY4jxiYefjPdWrN5erwgEee-Gz_BywqZW2mQcftw') 
meteo_data <- lapply(as.character(catalog$ID), function(X)  read_sheet('1FWC_YBrlINnjR5POC2kxa_LnLC7n5fFA90oNA7dLTP0', 
                                                                        sheet = X, col_types = c('ccncccnninncnnnnncc'))) 
meteo_data = meteo_data %>% 
  set_names(catalog$ID) %>% 
  bind_rows(.id = 'id') %>% 
  mutate(id = as.numeric(id)) %>% 
  full_join(catalog, by = c('id' = 'ID')) %>% 
  mutate(Datetime = as.POSIXct(Datetime)) %>% 
  relocate(id, .before = NAME) %>% 
  set_names(c('Datetime', 'Wdir', 'Wspd', 'Vis', 'Phen', 'Cloud', 'T', 'Td', 'F', 'Te', 'Tes', 
              'Comf', 'P', "Po", 'Tmin', 'Tmax', 'R', 'R24', 'S', 'ID', 'NAME', 'LON', 'LAT', 'H'))
                                                                       
flt_df <- meteo_data %>% 
  filter(str_detect(NAME, 'Балчуг') | str_detect(NAME, 'Сареево'))
flt_df_night <- flt_df %>% 
  filter(hour(Datetime) == 0 | hour(Datetime) == 3)

ggplot(flt_df, mapping = aes(x = Datetime, y = `T`, color = NAME)) +
  geom_point() +
  ggtitle('Плотности распределения температур на станциях "Малое Сараево", "Балчуг"') +
  ylab('C°') +
  xlab('Месяц') +
  labs(color = 'Метеостанция') + 
  scale_color_manual(values = c("orange", "purple"))

ggplot(flt_df_night, mapping = aes(x = Datetime, y = `T`, color = NAME)) +
  geom_point() +
  ggtitle('Плотности распределенияночных температур на станциях "Малое Сараево", "Балчуг"') +
  ylab('C°') +
  xlab('Месяц') +
  labs(color = 'Метеостанция') + 
  scale_color_manual(values = c("red", "blue"))


ggplot(flt_df, mapping = aes(NAME, `T`, fill = NAME)) +
  geom_boxplot(alpha = 0.4, width = 0.6) +
  ylab('C°') +
  theme(axis.title.x = element_blank()) +
  ggtitle('Диаграммы размаха температур на метеостанциях') +
  theme(legend.position="none")
