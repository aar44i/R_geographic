#����������� ���������
library(openxlsx, lib.loc = "C:/Users/korni/Desktop/Rstudio/mkk/lib")
library(dplyr, lib.loc = "C:/Users/korni/Desktop/Rstudio/mkk/lib")

setwd("C:/Users/korni/Desktop/Rstudio/mkk/homework/3") #��������� ������� ����������
tabl <-read.xlsx("ExpImp.xlsx", 1) #������ �����
#�������������� ����� ������
tabl$����������� <- as.numeric(tabl$�����������)
tabl$���������� <- as.numeric(tabl$����������)
tabl$���������� <- as.numeric(tabl$����������)
tabl$��������� <- as.numeric(tabl$���������)
tabl$���������� <- as.numeric(tabl$����������)
tabl$���������� <- as.numeric(tabl$����������)
tabl$����������� <- as.numeric(tabl$�����������)
tabl$���������� <- as.numeric(tabl$����������)
str(tabl) #�������� ��������� 

#���������� ������� �� 2 ������ ������� exp � imp, �������� ������ ������ �� ������� � ������ �� ��������
exp <- select(tabl, ������, �����������, ����������, ����������, �����������, ����������, ����������)
imp <- select(tabl, ������, ����������,  ���������, ���������, ����������,  ���������, ���������)

#���������� � ������� exp ������ ������� Type, ����������� �� ��� ��������
exp <- mutate(exp, Type = ������)
exp[grep("���������� �������", tabl$������), "Type"] <- "���������� �������"
exp[grep("���������� �����", tabl$������), "Type"] <- "���������� �����"
exp[grep("����� ������������ ��������", tabl$������), "Type"] <- "����� ������������ ��������"
exp[grep("����", tabl$������), "Type"] <- "����"
exp[grep("�������", tabl$������), "Type"] <- "�������"
exp[grep("����������", tabl$������), "Type"] <- "����������"
#����������� ������� Type � ����������� ����������
exp$Type <- as.character(exp$Type)
exp$Type <- as.factor(exp$Type)
summary(exp) #������� ���������� ��������� ������� ����

#�������� �� ���������� ������� ������ �� ����������� � ���������� �� ���������� ��������
rexp <- filter(exp, Type == "����������")
total <- rexp[c(-1, -8)]
rexp <- mutate(rexp, TotalExp = rowSums(total, na.rm=TRUE))
rexp <- arrange(rexp, desc(TotalExp))

#���������� ������� � CSV � ��������� Unicode ��� �������� ����� � Exsel
write.csv2(rexp, "rexp2.csv", fileEncoding = 'UTF-8', row.names=FALSE) 
write.xlsx(rexp, "rexp2.xlsx")

#���������� �������
rexp.csvsaved <- read.csv2("rexp2.csv", encoding = 'UTF-8') 
rexp.xlsxsaved <- read.xlsx("rexp2.xlsx",1)

View(rexp.csvsaved)
View(rexp.xlsxsaved)