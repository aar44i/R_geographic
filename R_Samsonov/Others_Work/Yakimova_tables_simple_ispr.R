# ���� �������. ��������������� ������ �3 (����������� �������� � ���������) --------

#�������� ���������
library(readr)
library(openxlsx) 

#�������� ������� ����������
setwd('/Users/Anna Yakimova/Desktop/Program/homework3.tables')  

#������ ������� � �������� �� ���������
exp_imp <- read.xlsx("ExpImp.xlsx", 1)  
str(exp_imp)
#View(exp_imp)

#�������������� ����� ������
exp_imp$����������� <- as.numeric(exp_imp$�����������)
exp_imp$���������� <- as.numeric(exp_imp$����������)
exp_imp$���������� <- as.numeric(exp_imp$����������)
exp_imp$��������� <- as.numeric(exp_imp$���������)
exp_imp$���������� <- as.numeric(exp_imp$����������)
exp_imp$����������� <- as.numeric(exp_imp$�����������)
exp_imp$���������� <- as.numeric(exp_imp$����������)
exp_imp$���������� <- as.numeric(exp_imp$����������)
str(exp_imp) #��������

#���������� ������� �� "������� � ������"
export <- grep("�������", colnames(exp_imp))
exp <- exp_imp[, export]
print(exp)
import <- grep("������", colnames(exp_imp))
imp <- exp_imp[, import]
print(imp)
exp$������ <- exp_imp$������

#���������� �������, ����������� �� ��� �������, � ������� ��������
exp[grep("���������� �������", exp_imp$������), "Type"] <- "���������� �������"
exp[grep("���������� �����", exp_imp$������), "Type"] <- "���������� �����"
exp[grep("����� ������������ ��������", exp_imp$������), "Type"] <- "����� ������������ ��������"
exp[grep("����", exp_imp$������), "Type"] <- "����"
exp[grep("�������", exp_imp$������), "Type"] <- "�������"
exp[grep("����������", exp_imp$������), "Type"] <- "����������"

#����������� ������� � ����������� ���������� (������)
exp$Type <- as.factor(exp$Type)
str(exp)

#�������� �������� ���������� ��������� ������� ����
summary(exp$Type)

#�������� �� ������� �������� ������ �� �����������
resp_exp <- grep("����������", exp$Type)
rexp <- exp[resp_exp, ]

#�������� ������ �������, ������������ ��������� ������� �������� �� ���� ������������
cleaned <- grep("�������", colnames(rexp))
summa <- rexp[, cleaned]
rexp$TotalExp <- rowSums(summa, na.rm = TRUE)

#���������� ������� �� �������� �������� ���������� ������ ��������
TotalExp_sort <- order(rexp$TotalExp, decreasing = TRUE)
rexp_sort <- rexp[TotalExp_sort, ]


#������� ������� � ������� Microsoft Excel � CSV
write.csv(rexp_sort, "rexp_sort.csv", fileEncoding = 'CP1251')
write.xlsx(rexp_sort, "rexp_sort.xlsx")

#������ ���������� ������
rexp_sort_csv <- read.csv("rexp_sort.csv", encoding = 'CP1251') 
rexp_sort_xlsx <- read.xlsx("rexp_sort.xlsx", 1)

#����� �� �����
View(rexp_sort_csv)
View(rexp_sort_xlsx)