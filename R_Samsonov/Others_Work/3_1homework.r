#����������� ����������
library(openxlsx, lib.loc = "C:/Users/korni/Desktop/Rstudio/mkk/lib") 

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
export <- grep("�������", colnames(tabl)) 
import <- grep("������", colnames(tabl)) 
exp <- tabl[, export]
imp <- tabl[, import]
exp$������ <- tabl$������

#���������� � ������� exp ������ ������� Type, ����������� �� ��� ��������
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


rexp <- exp[grep("����������", exp$Type), ] #�������� �� ���������� ������� ������ �� �����������
total <- rexp[c(-7, -8)]
rexp$TotalExp <- rowSums(total, na.rm=TRUE)
indexes<-order(rexp$TotalExp, decreasing = TRUE) #���������� �� ���������� ��������

#���������� ������� � CSV � ��������� Unicode ��� �������� ����� � Exsel
write.csv2(rexp[indexes, ], "rexp.csv", fileEncoding = 'UTF-8', row.names=FALSE) 
write.xlsx(rexp[indexes, ], "rexp.xlsx")

#���������� �������
rexp.csvsaved <- read.csv2("rexp.csv", encoding = 'UTF-8') 
rexp.xlsxsaved <- read.xlsx("rexp.xlsx",1)

#����� �� �����
View(rexp.csvsaved)
View(rexp.xlsxsaved) 

