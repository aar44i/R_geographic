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


ggplot(flt_df, mapping = aes(NAME, `T`, color = NAME)) +
  geom_boxplot( width = 0.4, col = 'black', fill = c("orange", "purple")) +
  ylab('C°') +
  theme(axis.title.x = element_blank()) +
  ggtitle('Диаграммы размаха температур на метеостанциях') +
  theme(legend.position="none") 
 

ggplot(flt_df_night, mapping = aes(NAME, `T`, color = NAME)) +
  geom_boxplot( width = 0.4, col = 'black', fill = c("red", "blue")) +
  ylab('C°') +
  theme(axis.title.x = element_blank()) +
  ggtitle('Диаграммы размаха ночных температур на метеостанциях') +
  theme(legend.position="none") 



t.test(flt_df %>% filter(NAME == 'Малое Сареево') %>% pull(`T`),
       flt_df %>% filter(NAME == 'Москва (центр, Балчуг)') %>% pull(`T`))

t.test(flt_df_night %>% filter(NAME == 'Малое Сареево') %>% pull(`T`),
       flt_df_night %>% filter(NAME == 'Москва (центр, Балчуг)') %>% pull(`T`))


var.test(flt_df %>% filter(NAME == 'Малое Сареево') %>% pull(`T`),
         flt_df %>% filter(NAME == 'Москва (центр, Балчуг)') %>% pull(`T`))  

var.test(flt_df_night %>% filter(NAME == 'Малое Сареево') %>% pull(`T`),
       flt_df_night %>% filter(NAME == 'Москва (центр, Балчуг)') %>% pull(`T`))


dif_temp = data.frame(Datetime = flt_df %>% 
                        filter(flt_df$NAME == 'Москва (центр, Балчуг)') %>% 
                        pull(Datetime)) %>%
  mutate(delta_temp = (flt_df %>% filter(flt_df$NAME == 'Москва (центр, Балчуг)') %>% pull(`T`) -
                       flt_df %>% filter(flt_df$NAME == 'Малое Сареево') %>% pull(`T`))) %>% 
  inner_join(meteo_data %>% group_by(Datetime) %>% summarise(mean_wind = round(mean(Wspd, na.rm = TRUE), 2)))

dif_temp_night = dif_temp %>% 
  filter(hour(Datetime) == 0 | hour(Datetime) == 3)


ggplot(dif_temp, mapping = aes(mean_wind, delta_temp)) +
  geom_point(color = 'orange') + 
  geom_smooth(method = 'loess', color = 'purple',  span = 1) +
  ggtitle('Диаграмма рассеяния между скоростью ветра\nи разницей температур (Малое Сареево и Балчуг)') +
  xlab('Скорость ветра, м/с') +
  ylab('Разница температур, C°')

ggplot(dif_temp_night, mapping = aes(mean_wind, delta_temp)) +
  geom_point(color = 'blue') + 
  geom_smooth(method = 'loess', color = 'red',  span = 1) +
  ggtitle('Диаграмма рассеяния между скоростью ветра\nи разницей ночных температур (Малое Сареево и Балчуг)') +
  xlab('Скорость ветра, м/с') +
  ylab('Разница температур, C°')





ggplot(dif_temp, mapping = aes(mean_wind, delta_temp)) +
  geom_point(color = 'orange') + scale_x_log10() + 
  geom_smooth(method = 'loess', color = 'purple',  span = 2) +
  ggtitle('Диаграмма рассеяния между скоростью ветра\nи разницей температур (Малое Сареево и Балчуг)') +
  xlab('Скорость ветра, м/с') +
  ylab('Разница температур, C°')

ggplot(dif_temp_night, mapping = aes(mean_wind, delta_temp)) +
  geom_point(color = 'blue') + scale_x_log10() + 
  geom_smooth(method = 'loess', color = 'red',  span = 2) +
  ggtitle('Диаграмма рассеяния между скоростью ветра\nи разницей ночных температур (Малое Сареево и Балчуг)') +
  xlab('Скорость ветра, м/с') +
  ylab('Разница температур, C°')




cor.test(log10(dif_temp$mean_wind), dif_temp$delta_temp)
cor.test(log10(dif_temp_night$mean_wind), dif_temp_night$delta_temp)


linear_model = lm(log10(dif_temp$mean_wind) ~ dif_temp$delta_temp)
summary(linear_model)

