

library(openxlsx) # ����������� ����������

setwd("C:/Users/Lera/Desktop/4 level/MKK/Dates 4")#������� ����������
expimp <- read.xlsx("ExpImp.xlsx", 1) # ������ �������
str(expimp) #����� ����������� ����� ������
expimp$����������� <- as.numeric(expimp$�����������) #��������� ����� ������
expimp$���������� <- as.numeric(expimp$����������)
expimp$���������� <- as.numeric(expimp$����������)
expimp$��������� <- as.numeric(expimp$���������)
expimp$���������� <- as.numeric(expimp$����������)
expimp$����������� <- as.numeric(expimp$�����������)
expimp$���������� <- as.numeric(expimp$����������)
expimp$���������� <- as.numeric(expimp$����������)
str(expimp) #�������� ���������� ����� ������


expo <- grep("�������", colnames(expimp)) #����� ����� "�������"
impo <- grep("������", colnames(expimp)) #����� ����� "������"
exp <- expimp[expo] #�������, �������� ������ ������ �� ��������
imp <- expimp[impo] #�������, �������� ������ ������ �� �������

exp <- cbind(exp, expimp$������)#� ������� � ��������� ��������� ��� ������� �� ����� �������

#����� �������� ������ �� �����
type_aob <- grep("���������� �������", expimp$������)
type_aok <- grep("���������� �����", expimp$������)
type_sf <- grep("�.",expimp$������)
type_kray <- grep("����", expimp$������)
type_rep <- grep("����������", expimp$������)
type_obl <- grep("�������", expimp$������)

#���������� ��������� � ������� "Type"
exp[type_aob, "Type"] <- "���������� �������"
exp[type_aok, "Type"] <- "���������� �����"
exp[type_sf, "Type"] <- "����� ������������ ����������"
exp[type_kray, "Type"] <- "����"
exp[type_rep, "Type"] <- "����������"
exp[type_obl, "Type"] <- "�������"

# ����������� � ����������� ����������
exp$Type <- as.character(exp$Type)
exp$Type <- as.factor(exp$Type)

#������� ��������� ��������� ������� ����
summary(exp$Type)

#�������� ������ �� ����������� �� ������� "exp"
resp <- grep("����������", exp$Type)
rexp <- exp[resp,]
forsum <- rexp[c(1:6)]

#���������� ������ ������� � ��������� ��������� � ������� "rexp"
rexp$TotalExp <- rowSums(forsum, na.rm=TRUE)

#����������
sorti <- order(rexp$TotalExp, decreasing = TRUE)
rexp_sorti <- rexp[sorti, ]
rexp <- rexp_sorti


#������������� �������� ��������
colnames(rexp) <- c("�����������", "����������", "����������", "�����������", "����������", "����������", "�������", "���", "��������� �������")
View(rexp)

#���������� �������
write.csv(rexp, 'rexp.csv', fileEncoding = 'UTF-8')
write.xlsx(rexp, 'rexp.xlsx')

#������ ������
rexp.csvsaved <- read.csv("rexp.csv", encoding = 'UTF-8') 
rexp.xlsxsaved <- read_xlsx("rexp.xlsx",1)

View(rexp.csvsaved)
View(rexp.xlsx)