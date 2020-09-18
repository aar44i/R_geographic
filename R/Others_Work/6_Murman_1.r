library ('dplyr')
library ('tidyr')
library ('ggplot2')
library ('eurostat')

# первый график
fresh = get_eurostat('tag00115', time_format = "num") %>% 
  label_eurostat() %>%   # полные наименования
  filter(!stringr::str_detect(geo, 'European Union|Euro area')) %>%  # оставляем только страны
  filter(stringr::str_detect(crops, 'Tomatoes|Cucumbers|Onions|Carrots'))    # оставляем нужную продукцию
fresh_2010 = fresh %>% 
  filter(stringr::str_detect(time, '2010'))

ggplot(data = fresh_2010, mapping = aes(x = geo, y = values, fill = crops)) +
  geom_col(color = 'black', size = 0.15) +
  coord_flip()+
  ggtitle("Harvested area of tomatoes, cucumbers, carrots and onions in Europe", subtitle = "2010")+ # заголовок
  xlab("country")+  # подпись оси х
  ylab("Thousand ha")

# второй график
fresh2 = fresh_2010 %>% 
  filter(stringr::str_detect(geo, 'Italy|Spain|Romania|Poland')) # выбор нужных стран
short = fresh2 %>% 
  group_by(geo) %>% 
  summarise(total = sum(values)) # подсчет общего количества по странам
fresh2 = fresh2 %>% 
  inner_join(short, by = c("geo" = "geo")) %>% 
  mutate(perc = round(c(values / total * 100), 2)) # добавляем столбец с долей каждого вида овоща

brks = c(25, 50, 75, 100)
ggplot(fresh2, mapping = aes(x = total, y = perc, fill = crops, width = total * 2)) +
  geom_col(color = 'black', size = 0.5) +  # параметры обводки
  scale_y_continuous(breaks = brks, labels = brks) +
  ggtitle("Harvested area of tomatoes, cucumbers, carrots and onions", subtitle = "2010") +
  xlab("Thousand ha") +
  ylab(NULL) +  # убираем название оси у
  coord_polar(theta = 'y') +
  facet_wrap(~geo)

# третий график
fresh_all = fresh %>% 
  filter(stringr::str_detect(geo, 'Italy|Spain|Romania|Poland')) %>% 
  filter(stringr::str_detect(crops, 'Tomatoes')) 

ggplot(fresh_all, mapping = aes(x = time, y = values, color = geo)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  ylim(0, 125) +
  scale_x_continuous(breaks = seq(2007, 2019, 2)) + # разметка оси x
  ggtitle("Harvested area of tomatoes") +
  xlab("Year") +
  ylab("Thousand ha") +
  labs(color = "Country") #название легенды
  



 