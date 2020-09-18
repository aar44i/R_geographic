library(openxlsx)
library(dplyr)
setwd("/Users/355/Desktop/3 kurs/R")
tab <- read.table("oxr_vod.csv",
                  sep = ';',
                  dec = ',',
                  header = TRUE,
                  encoding = 'UTF-8')
## View(tab)
tab2<-read.csv2("oxr_vod.csv", encoding = 'UTF-8')
## View(tab2)
tab[c(5,2,4), ]
indexes<-order(tab$����������)
head(tab[indexes, ])
head(tab[order(tab$����������), ])
condition <- tab$���������� > 10
condition  # ��������� ��� ����������
tab[condition, ] # ���������� ��� ��� ���������� ����� �������:
tab[tab$���������� > 10, ]


caspian <- data.frame(tab$���, tab$�����, tab$����������)
colnames(caspian)<-c("Year", "Total", "Caspian")
ratio <- caspian$Caspian / caspian$Total
ratio
ratio <- round(ratio, digits = 3)
ratio
caspian$CaspianRatio <- ratio
mutate(caspian, ratio2 = Caspian / Total)
caspian2 <- caspian %>% mutate(ratio3 = Caspian / Total) %>% 
  filter(ratio3 > 0.44) %>% 
  select(Year, ratio3)

## View(caspian)
head(caspian[2])  # ������ ������� (���������� � ������� �� ������ �������)
head(caspian[c(1,4)])  # ������ � ��������� �������
caspian[,2]
sewage<-read.xlsx("sewage.xlsx",1) # ������ ������� �� ������� �����
## View(sewage)
colnames(sewage) <- c("Region", "Year05", "Year10", "Year11", "Year12", "Year13")
## View(sewage)
max(sewage$Year12)
max(sewage$Year13, na.rm = TRUE)
filter<-complete.cases(sewage)
filter  # ��������� ��� ����������. ��� ��� ����� FALSE - ���� �������� � �������

sewage.complete <- sewage[filter, ] # ����������� ������ ������
## View(sewage.complete)
# ������ �������� - ������� ���������, ������ �������� - ��� �����
rows <- grep("����������� �����",sewage$Region)
rows  # ���������, ����� �������� ������� Region ��� �������������
okruga <- sewage[rows,] # ����������� ��������� ������
## View(okruga)
rows2 <- grepl("����������� �����",sewage$Region)
rows2 # ��� ��� �������� ��������� grepl

neokruga <- sewage[!rows2,]
## View(neokruga)
rows2 <- grepl("�����������|�����|����������|��|�",sewage$Region)
rows2
neokruga <- sewage[!rows2,] # �������� �������� �� ��������������� ���� ����� rows2
## View(neokruga)
tab <- read.csv2("SatinoLanduse.csv", encoding = 'UTF-8')
str(tab) # ���������, ������ ��������� ������
## View(tab)
s <- "5456.788"
s + 1
n <- as(s, "numeric")
n + 1
s <- as(n, "character")
s
nchar(s)
as.numeric(s) # �� �� �����, ��� � as(s, "numeric")
tab$Comment <- as.character(tab$Comment)
str(tab)
as.numeric(tab$Perimeter)
levels(tab$Perimeter)[1:10]
tab$Perimeter <- as.numeric(as.character(tab$Perimeter))
str(tab)

# ������ ��������� ������������� ������� Area
temp <- as.numeric(as.character(tab$Area))
temp[1:10]
tab[is.na(temp), "Area"]
tab$Area <- gsub(',', '.', tab$Area) # ������� ������� �� �����
tab$Area <- as.numeric(as.character(tab$Area)) # ������ ����� ������������� � �����
str(tab)
levels(tab$Type)
levels(tab$Administration)
filter <- grep("������������", tab$Administration) # ������ ��� ������
tab[filter, "Administration"] <- "������������ �������� �������������" # ������� �� ����� ���������
tab$Administration <- droplevels(tab$Administration) # ������� �������������� ������
levels(tab$Administration)
filter <- nchar(as.vector(tab$Administration)) == 0 # TRUE ���� ����� ����� 0
# ������� ��������:
tab[filter, "Administration"] <- "������"
tab$Administration <- as.character(tab$Administration)
tab[filter, "Administration"] <- "������"
tab$Administration <- as.factor(tab$Administration)
levels(tab$Administration)
summary(tab)
write.csv2(okruga, "okruga.csv", fileEncoding = 'UTF-8') # �������� ������ ������� � CSV � ��������� Unicode
write.xlsx(neokruga, "neokruga.xlsx") # �������� ������ ������� � XLSX ��� �������� �����

# ��������, ��� �� � ������� � ������������ ���������:

okruga.saved <- read.csv2("okruga.csv", encoding = 'UTF-8')
head(okruga.saved)

neokruga.saved <- read.xlsx("neokruga.xlsx",1)
head(neokruga.saved)