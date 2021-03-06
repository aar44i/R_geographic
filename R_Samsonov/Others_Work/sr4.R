library(readxl)
library(dplyr)
#library(readxl)
#library(xlsx)

setwd("C:/Users/Lera/Desktop/4 level/MKK/Dates 4")
tab <- read_xlsx("IncomeConsumption.xlsx", 1)
str(tab)

tab <- tab %>% mutate(ratio=tab$�����/tab$�������) #���������� ������ �������
col1 <- rgb(1, 0, 0, 0.5)
plot(tab$�����, 
     tab$�������,
     col=col1,
     xlab="�����, ���. ���.",
     ylab="��������������� �������, ���. ���.",
     main="����������� �������������� �������� � �������\n �� ���� ��������� �� �������� ������",
     pch = 19, #����� �������
     cex = tab$ratio) # ������ ������
grid()

legend(x="topleft",
       y = c('1', '2', '3'),
       pch = c(19, 19, 19),
       pt.cex = c(1, 2, 3),
       col = col1,
       horiz = TRUE)


# ����������� ������������� ������� � �������� ��������� �� ��������� ���������
hist(tab$�����, 
     breaks = seq(10000, 70000, 2000),  
     col = "deeppink",
     main = '������������� ������� ���������\n �� ��������� ���������',
     xlab = '������, ���',
     ylab = '���������� ��������� ��')       

hist(tab$�������, 
     breaks = seq(4000, 42000, 2000),  
     col = "cyan",
     main = '������������� �������� ���������\n �� ��������� ���������',
     xlab = '������, ���',
     ylab = '���������� ��������� ��')


# ���������� ��������� �� ������� � �������� � ����������� ������� ��
stolbfo <- grep('����������� �����', tab$������)
fok <- tab[stolbfo, ] # ������� ������ � ������������ ��������

## ��������� �������
sortirovka <- order(fok$�����, decreasing = TRUE)
fok_sortirovka <- fok[sortirovka, ]# ���������� ������� � ������� �������� �������

names <- c('�����������', '���������', '��������������', '������-��������','����������', '�����', '���������', '������-����������')
par(mar = c(5, 8, 6, 2)) # ����������� ����
barplot(fok_sortirovka$�����, # ���������� ���������� ���������
        col = "darkorange",
        main = '������������� ������� �����������\n � ����������� ������� ��',
        xlab = '������, ���',
        names.arg = names, # ������� �������� ��������
        xlim = c(0, 35000), # ����� ��� �
        ylim = c(0, 8), # ����� ��� �
        horiz = TRUE, # ��������� ������� �������������
        las = 1, # ������� ��� ������ �������������
        cex.names = 0.7) # ������������� ������ ������

## ��������� ��������
sortirovka2 <- order(fok$�������, decreasing = TRUE)
fok_sortirovka2 <- fok[sortirovka2, ]# ���������� ������� � ������� �������� �������� 

names2 <- c('�����������', '���������', '��������������', '������-��������','�����', '����������', '���������', '������-����������')
par(mar = c(5, 8, 6, 2)) # ����������� ����
barplot(fok_sortirovka2$�������, # ���������� ���������� ���������
        col = "firebrick",
        main = '������������� �������� �����������\n � ����������� ������� ��',
        xlab = '�������, ���',
        names.arg = names2, # ������� �������� ��������
        xlim = c(0, 25000), # ����� ��� �
        ylim = c(0, 8), # ����� ��� �
        horiz = TRUE, # ��������� ������� �������������
        las = 1, # ������� ��� ������ �������������
        cex.names = 0.7) # ������������� ������ ������
