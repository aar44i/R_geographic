library(eurostat)
library(tidyverse)

veg = as_tibble(get_eurostat("tag00115"), time_format = num) %>% 
  label_eurostat()
select(veg, -strucpro)
veg = filter(veg, !stringr::str_detect(veg$geo, "European Union"))
veg = filter(veg, stringr::str_detect(veg$crops, "Tomatoes|Cucumbers|Onions|Carrots"))
veg1 = filter(veg, time == "2010-01-01")
ggplot(mapping = aes(x = veg1$geo, y = veg1$values, fill = veg1$crops)) +
  geom_col() +
  coord_flip() +
  labs(color = "Crops") +
  xlab(NULL) +
  ylab("Thousand ha") +
  ggtitle('Harvested area of tomatoes, cucumbers, onions and carrots in Europe',
          subtitle = '2010')


veg_f = rbind(filter(veg1, geo == "Poland"), 
              filter(veg1, geo == "Spain"), 
              filter(veg1, geo == "Romania"), 
              filter(veg1, geo == "Italy")) 
veg_fgr = group_by(veg_f, veg_f$geo) %>% summarise(summ = sum(values))
veg_f = left_join(veg_f, veg_fgr, by = c("geo" = "veg_f$geo"))
veg_f = mutate(veg_f, sh = values/summ)
ggplot(veg_f, mapping = aes(x = veg_f$summ, y = veg_f$sh, fill = veg_f$crops, width = 2 * veg_f$summ), 
         color = "black", size = 0.2) +
  geom_col() +
  coord_polar(theta = "y") +
  facet_wrap(~geo) +
  ylab("")+
  xlab("Thousand ha") +
  labs(color = "Crops") +
  ggtitle("Harvested area of tomates, cucumbers, carrots and onions", 
          subtitle = "2010")

veg_t = filter(veg, crops == "Tomatoes")
veg_t = rbind(filter(veg_t, geo == "Poland"), 
              filter(veg_t, geo == "Spain"), 
              filter(veg_t, geo == "Romania"), 
              filter(veg_t, geo == "Italy")) 

ggplot(veg_t, mapping = aes(x = time, y = values, color = geo)) +
  geom_line() +
  geom_point() +
  labs(color = "Country") +
  xlab("Year") +
  ylab("Thousand ha") +
  ggtitle("Harvested area of tomatoes")