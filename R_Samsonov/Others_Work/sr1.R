
# ������� 1 ---------------------------------------------------------------
num <- seq(from=2,to=26,by=2)#��������� ������������������ �� 2 �� 26 � ����� 2
count <- letters[num]#��������� ����� ���������� �������� ��� ������������ ������
count#�����


# ������� 2 ---------------------------------------------------------------

ges <- c('�����������', '��������', '���������', '��������', '�����������', '������������', '�����������', '�����-���������', '����-��������', '������������') #������ � ���������� ���
mosh <- c(2997, 4515, 2010, 2650, 2404, 6000, 1391, 6400, 3840, 1404)#������ � ���������� ���
len1 <- sort(mosh,decreasing=T)#���������� �� �������� ��������
t <- len1[3]#�������� 3 �� �������� ��� ������� � ����������
idx <- match(t,mosh)#������� ������ ��������, ������� t, � ������ mosh
c <- ges[idx]#������������� 3 �� �������� ��� � �� ���������
#����� ������
paste(' ������ �� �������� ��� ������ - ',c,'. �� �������� ���������� ',t,' ���.',sep='')


