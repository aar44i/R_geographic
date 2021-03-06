
# ������� 1. ������������ ����������� ������� � �������� ��������� --------

library(openxlsx) #��������� ����������
setwd("/Users/Anna Yakimova/Desktop/Program/4")
tab <- read.xlsx("IncomeConsumption.xlsx",1)

# ���������� ��������� ��������� �� ��������� ��
ratio <- tab$����� / tab$�������
KPACH <- rgb(1, 0, 0)
KPACH.trans <- adjustcolor(KPACH, alpha = 0.4)
plot(tab$����� / 1000, 
     tab$������ / 1000,
     col = KPACH.trans, 
     xlab = "�����, ���. ���.", 
     ylab = "��������������� �������, ���. ���.", 
     main = "����������� ����������� ������� � �������� \n �� ���� ��������� �� �������� ������",
     pch = 19,
     cex = ratio) #������ ������ ������� �� ����������� ������� � ��������

grid(col = "lightgrey") #��������� �����

#���������� �������
legend(x = "topleft", 
       y =  c('1', '2', '3'),
       pch = c(19, 19, 19),
       pt.cex = c(1, 2, 3),
       col = KPACH.trans,
       horiz = TRUE)


# ���������� ���������� ������������� ������� � �������� �� ��������� ���������
hist(tab$�����,
     breaks = seq(10000, 70000, 2000),  #������ ������� 2000
     col = 'lightblue', 
     border = 'black', 
     main = '������������� ������� ��������� \n �� ��������� ��', 
     xlab = "������ ���������, ���.",
     ylab = '���������� ��������� ��')

hist(tab$�������,
     breaks = seq(1000, 50000, 2000), 
     col = 'lightgreen', 
     border = 'black', 
     main = '������������� �������� ��������� \n �� ��������� ��', 
     xlab = "������� ���������, ���.",
     ylab = '���������� ��������� ��')


# ���������� ���������� ��������� �� ������� � ����������� �������
rows <- grep('����������� �����', tab$������) #�������� �� ������� ����������� ������
tab_d <- tab[rows,] #��������� �������, ������� ���� ����������� ������

sor1 <- order(tab_d$�����, decreasing = TRUE)  #��������� ������� ������� �� ��������
tab_d_sort1 <- tab_d[sor1, ] #��������� �������


# ���������� ���������� ��������� �� ������� � ����������� �������
arg1 <- c('�����������', '���������', '���������������', '������-��������', '�����������', '�����', '���������', '������-����������')
barplot(tab_d_sort1$����� / 1000, 
        col = 'blue', 
        border = 'black', #������� ��������
        main = "������������� ������� ��������� \n �� ����������� ������� ��",
        ylab = "����������� ������", 
        xlab = "������ ���������, ���. ���.",
        xlim = c(0, 35),
        names.arg = arg1,
        horiz = TRUE,  #�������������� ����������
        cex.names = 0.8)  #������ ������


# ���������� ���������� ��������� �� �������� � ����������� �������
sor2 <- order(tab_d$�������, decreasing = TRUE)
tab_d_sort2 <- tab_d[sor2,]

arg2 <- c('�����������', '���������', '���������������', '������-��������', '�����', '�����������', '���������', '������-����������')
barplot(tab_d_sort2$������� / 1000, 
        col = 'green', 
        border = 'black', 
        main = "������������� �������� ��������� \n �� ����������� ������� ��",
        ylab = "����������� ������", 
        xlab = "������� ���������, ���. ���.",
        xlim = c(0, 35),
        names.arg = arg2,
        horiz = TRUE,
        cex.names = 0.8)

