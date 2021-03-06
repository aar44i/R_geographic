# ���� �������. ��������������� ������ �3 (dplyr) -------------------------

#�������� ���������
library(openxlsx)
library(dplyr)

#�������� ������� ���������� � ������ �������
setwd('/Users/Anna Yakimova/Desktop/Program/homework3.tables')
imp_exp <- read.xlsx("ExpImp.xlsx", 1)
str(imp_exp) #�������� ��������� �������

#�������������� ����� ������
imp_exp$����������� <- as.numeric(imp_exp$�����������)
imp_exp$���������� <- as.numeric(imp_exp$����������)
imp_exp$���������� <- as.numeric(imp_exp$����������)
imp_exp$��������� <- as.numeric(imp_exp$���������)
imp_exp$���������� <- as.numeric(imp_exp$����������)
imp_exp$����������� <- as.numeric(imp_exp$�����������)
imp_exp$���������� <- as.numeric(imp_exp$����������)
imp_exp$���������� <- as.numeric(imp_exp$����������)
str(imp_exp)

#��������������� �������� �������� 
colnames(imp_exp) <- c("Region", "ProdExp", "ProdImp", "TEKExp", "TEKImp", "ChemExp", "ChemImp", "DrevExp", "DrevImp", "MetExp", "MetImp", "MachExp", "MachImp")
str(imp_exp)

#����� ������� �� ���
imp <- select(imp_exp, ProdImp, TEKImp, ChemImp, DrevImp, MetImp, MachImp)
exp <- select(imp_exp, ProdExp, TEKExp, ChemExp, DrevExp, MetExp, MachExp) %>% 
       mutate(Region = imp_exp$Region) #������� ����� �������, ���������� �� ��� ��������
View(exp)  

#������� ����������, ���������� �� ��� ��� ���� ��� ��������
avt_obl <- grep("���������� �������", imp_exp$Region)
avt_okr <- grep("�����", imp_exp$Region)
city <- grep("�.", imp_exp$Region)
krai <- grep("����", imp_exp$Region)
resp <- grep("����������", imp_exp$Region)
obl <- grep("�������", imp_exp$Region)

#����������� ������� Type �������� ���������, ���������� ���������� � ����� ��������
exp[avt_obl,"Type"] <- "���������� �������"
exp[avt_okr,"Type"] <- "���������� �����"
exp[city,"Type"] <- "����� ������������ ��������"
exp[krai,"Type"] <- "����"
exp[resp,"Type"] <- "����������"
exp[obl,"Type"] <- "�������"

#����������� ������� � ����������� ���������� (������)
exp$Type <- as.factor(exp$Type)
#�������� �������� ���������� ������� �� ���������
summary(exp)
View(exp)

#�������� ������� �� �����������
rexp <- filter(exp, Type == "����������")
View(rexp)
total <- rexp[, c(1:6)]
View(total)
rexp <- mutate(rexp, TotalExp = rowSums(total, na.rm = TRUE)) 
rexp <- arrange(rexp, desc(TotalExp))

View(rexp)

#������� ������� � ������� Microsoft Excel � CSV
#write.csv(rexp, "rexp.csv", fileEncoding = 'CP1251')
#write.xlsx(rexp, "rexp.xlsx")

#������ ������
#rexp_csv <- read.csv("rexp.csv", encoding = 'CP1251') 
#rexp_xlsx <- read.xlsx("rexp.xlsx", 1)

#����� �� �����
#View(rexp_csv)
#view(rexp_xlsx)
