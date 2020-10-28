library(tidyverse)
library(googlesheets4)
library(lubridate)
library(rlang)

catalog = read_sheet('1Q6HCY4jxiYefjPdWrN5erwgEee-Gz_BywqZW2mQcftw') 
meteo_data <- lapply(as.character(catalog$ID), 
                     function(X) read_sheet('1FWC_YBrlINnjR5POC2kxa_LnLC7n5fFA90oNA7dLTP0', 
                                            sheet = X, col_types = 'ccncccnninncnnnnncc')) %>% 
  set_names(catalog$ID) %>% 
  bind_rows(.id = 'id') %>% 
  mutate(id = as.numeric(id)) %>% 
  full_join(catalog, by = c('id' = 'ID')) %>% 
  mutate(Datetime = as.POSIXct(Datetime)) %>% 
  relocate(id, .before = NAME) %>% 
  set_names(c('Datetime', 'Wdir', 'Wspd', 'Vis', 'Phen', 'Cloud', 'T', 'Td', 'F', 'Te', 
              'Tes', 'Comf', 'P', "Po", 'Tmin', 'Tmax', 'R', 'R24', 'S', 'ID', 'NAME', 'LON', 'LAT', 'H'))