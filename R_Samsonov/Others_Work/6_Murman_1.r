library ('dplyr')
library ('tidyr')
library ('ggplot2')
library ('eurostat')

# ������ ������
fresh = get_eurostat('tag00115', time_format = "num") %>% 
  label_eurostat() %>%   # ������ ������������
  filter(!stringr::str_detect(geo, 'European Union|Euro area')) %>%  # ��������� ������ ������
  filter(stringr::str_detect(crops, 'Tomatoes|Cucumbers|Onions|Carrots'))    # ��������� ������ ���������
fresh_2010 = fresh %>% 
  filter(stringr::str_detect(time, '2010'))

ggplot(data = fresh_2010, mapping = aes(x = geo, y = values, fill = crops)) +
  geom_col(color = 'black', size = 0.15) +
  coord_flip()+
  ggtitle("Harvested area of tomatoes, cucumbers, carrots and onions in Europe", subtitle = "2010")+ # ���������
  xlab("country")+  # ������� ��� �
  ylab("Thousand ha")

# ������ ������
fresh2 = fresh_2010 %>% 
  filter(stringr::str_detect(geo, 'Italy|Spain|Romania|Poland')) # ����� ������ �����
short = fresh2 %>% 
  group_by(geo) %>% 
  summarise(total = sum(values)) # ������� ������ ���������� �� �������
fresh2 = fresh2 %>% 
  inner_join(short, by = c("geo" = "geo")) %>% 
  mutate(perc = round(c(values / total * 100), 2)) # ��������� ������� � ����� ������� ���� �����

brks = c(25, 50, 75, 100)
ggplot(fresh2, mapping = aes(x = total, y = perc, fill = crops, width = total * 2)) +
  geom_col(color = 'black', size = 0.5) +  # ��������� �������
  scale_y_continuous(breaks = brks, labels = brks) +
  ggtitle("Harvested area of tomatoes, cucumbers, carrots and onions", subtitle = "2010") +
  xlab("Thousand ha") +
  ylab(NULL) +  # ������� �������� ��� �
  coord_polar(theta = 'y') +
  facet_wrap(~geo)

# ������ ������
fresh_all = fresh %>% 
  filter(stringr::str_detect(geo, 'Italy|Spain|Romania|Poland')) %>% 
  filter(stringr::str_detect(crops, 'Tomatoes')) 

ggplot(fresh_all, mapping = aes(x = time, y = values, color = geo)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  ylim(0, 125) +
  scale_x_continuous(breaks = seq(2007, 2019, 2)) + # �������� ��� x
  ggtitle("Harvested area of tomatoes") +
  xlab("Year") +
  ylab("Thousand ha") +
  labs(color = "Country") #�������� �������
  



 