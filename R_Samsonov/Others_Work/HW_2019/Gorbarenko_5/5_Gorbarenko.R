library(readxl)
library(tidyverse)
library(RColorBrewer)

routes = read_excel('data-101782-2019-10-14.xlsx', 
col_types = c(rep('numeric', 4), rep('text', 1), rep('numeric', 3)))  # ������ ����� (��������)
transport_ent = read_excel('data-101780-2019-10-21.xlsx', 
col_types = c(rep('numeric', 2), rep('text', 3)))  # ������ ����� (������������ �����������)    
schedule = read_excel('data-101784-2019-10-14.xlsx') # ������ ����� (�����)


    names = c('�������', '�������', '����������') # ��������� ������� ���  1  ������� 
    (routes_types = routes %>% 
        group_by(route_type) %>% 
        summarise(n()) %>% 
        mutate(names) %>%
        arrange(names)) 
    colnames(routes_types ) <- c('type', 'col', 'names')
    names_plot = paste(routes_types$names, " (", routes_types$col, ")", sep = "")
    
    par(mar = c(3, 3, 3, 3)) # ���������� 1 �������
    colors = brewer.pal(length(names_plot),'Accent')
    pie(routes_types$col, names_plot,
        main = "����������� ���������� ��������� \n �� ����� ����������",
        clockwise = TRUE,
        radius = 1,
        col = colors,
        cex.names = 0.5,
        cex.main = 0.9,
        cex = 0.7)
    
    
    (tras_comp = group_by(routes, agency_code) %>% # ��������� ������� ���  2  ������� 
        summarise(n()) %>% 
        inner_join(transport_ent, by = c('agency_code' = 'agency_code')))
        colnames(tras_comp) = c('agency_code', 'col', 'global_id', 'agency_name', 'agency_url', 'agency_timezone') 
        tras_comp = arrange(tras_comp, col)
        
        par(mar = c(6, 11, 5, 5)) # ���������� 2 �������
        barplot(tras_comp$col,
                names.arg = tras_comp$agency_name,
                main = '������������� ��������� �� ������������',
                xlab = '���������� ���������',
                las = 1,
                cex.names = 0.7,
                horiz = T,
                xlim = c(0,1000),
                col = 'lightblue')
      
    sched_hour = as.numeric(substr(schedule$departure_time, 0,2)) # ��������� ������� ���  3  ������� 
    sched_min = round(as.numeric(substr(schedule$departure_time, 4,5))/60, digits = 1)
    sched_time = sched_hour + sched_min 
    sched_time = sapply(sched_time, function(X) 
                                        {if (X > 24) X = X - 24 
                                        else X = X})
    schedule = mutate(schedule, sched_time)
        
    par(mar = c(5, 5, 5, 5)) # ���������� 3 �������
    colors = rep('lightblue', 24) 
    colors[9] = 'red'
    colors[19] = 'red'
    hist(sched_time, 
         main = '���������� ����������� � ��������� ������������� ����������',
         xlab = '���',
         ylab = '���������� �����������',
         breaks = seq(0, 24, 1),
         xlim = c(0, 24),
         ylim = c(0, 25000),
         col = colors,
         type = "l",
         xaxt ='n',
         cex.axis =  0.7,
         cex.lab = 0.7,
         cex.main = 0.8)
         axis(side = 1, at = seq(0, 24, 1),tck = - 0.07, cex.axis = 0.7) 
             

    stops = schedule %>% # ��������� ������� ���  4  �������  
    group_by(trip_id = as.character(trip_id)) %>%
    summarise(num_stops = n(), 
              max_time = max(sched_time), 
              min_time = min(sched_time)) %>%
    mutate(delta = max_time - min_time) 
    med = stops %>% 
    group_by(num_stops) %>%
    summarise(delta = median(delta))
      
    
    par(mar = c(5, 5, 5, 5)) # ���������� 4 �������
    plot(stops$num_stops, 
         stops$delta, 
         main = '����������������� ���������',
         xlab = '���������� ��������� � ��������',
         ylab = '����������������� ��������, �',
         pch = 20, 
         col = 'gray',
         ylim = c(0, 2), 
         cex.axis =  0.7,
         cex.lab = 0.7)
    lines(med$num_stops, med$delta, type = 'o', pch = 20, col = 'red')
    legend('topright',  '��������� ��������', lwd = 1, pch = 20, col = 'red',cex = 0.7, bg = 'white')
    grid()

            
