library(readxl)
library(tidyverse)
library(writexl)
emissions = read_excel('emissions.xlsx', 1,skip = 1, col_types = c("text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) # ���� ������ � ������� ����� �����
capture = read_excel('emissions.xlsx', 2, skip = 1, col_types = c("text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) # ���� ������ �� ������� ����� �����

colnames(emissions) = c( "Region", "y2005", "y2010", "y2011", "y2012", "y2013", "y2014", "y2015", "y2016") # �������������� ��������
colnames(capture) = c( "Region", "y2005", "y2010", "y2011", "y2012", "y2013", "y2014", "y2015", "y2016")
  str(emissions) # ��������� ���� ������ 
  str(capture)
Flt_e = stringr::str_detect(emissions$Region, '���������|����������� �����') # ���� ������
Flt_c = stringr::str_detect(capture$Region, '���������|����������� �����') 

(Flt_emissions = emissions %>% # ������ �������� ����������� ��� ��������
  mutate(okrug = if_else(Flt_e, Region, NULL)) %>% 
  tidyr::fill(okrug) %>% 
  filter(!Flt_e) %>% 
  pivot_longer(cols = y2005:y2016, 
               names_to = 'year', 
               names_prefix = 'y',
               names_ptypes = list(year = integer()),
               values_to = 'value'))

(Flt_capture = capture %>% #  ������ �������� ����������� ��� �����������
    mutate(okrug = if_else(Flt_c, Region, NULL)) %>% 
    tidyr::fill(okrug) %>% 
    filter(!Flt_c) %>% 
    pivot_longer(cols = y2005:y2016, 
                 names_to = 'year', 
                 names_prefix = 'y',
                 names_ptypes = list(year = integer()),
                 values_to = 'value'))

  (Polluters = Flt_emissions %>% # ����������� ������ �� ������������ � ����������� � ���� � ���������� ����� ���� �������
      inner_join(Flt_capture, by = c("Region" = "Region", "year" = "year")) %>% 
      transmute(Region = Region,
                okrug = okrug.x,
                year = year,
                emissions = value.x,
                capture = value.y, delta = capture - emissions) %>% 
      
      group_by(okrug, year) %>% # ���������� �������� � ��������� ������������ �������� � �����������
      arrange((delta)) %>% 
      filter(row_number() == 1))
  write_xlsx(Polluters, "polluters.xlsx") # ����� ������ � ���� 
   
