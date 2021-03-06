number = c(1, 2, 3, 4, 5, 6, 7)
towns = c( "���", "������", "��������", "�������", "������", "�����", "������ ��������")
latitude = c(52.938023, 54.505862,  54.883524, 55.070474, 54.651824, 55.576834, 56.210471)
longitude = c(36.065684, 36.233586, 37.418129,38.830827, 39.806943, 42.072086, 43.872036)
distance_source = c(111, 391, 522,  645, 801, 1285, 1500)
distance_mouth = c(1389, 1109, 978, 855, 699, 215, 0)
zero_hydropost = c(146.31, 116.72, 107.54, 100.26, 93.41, 73.27, 62.00)

Cities = data.frame(Number = number, Towns = towns, Latitude = latitude, Longitude = longitude, 
                    Distance_source = distance_source, Distance_mouth = distance_mouth, 
                    Zero_hydropost = zero_hydropost) # ������ �����

  D = acos(sin(latitude[1:6]) * sin(latitude[2:7]) + cos(latitude[1:6]) * cos(latitude[2:7])
           * cos( longitude[1:6] - longitude[2:7])) # ���������� ����� �������� �� ���������� 
  S = D * 111.1
  
  distanse = distance_source [2:7] - distance_source [1:6] #���������� ����� �������� �� ����
  tortusity = S / distanse #������������ 
  
Sections = data.frame(FirstCity = number[1:6], SecondCity =  number[2:7],
                        Distanse = distanse,
                        Hight = zero_hydropost[1:6] - zero_hydropost[2:7],
                        Tortusity = tortusity) #������ �����

#Cities  #������� ����� �������
#Sections

UserDistance = as.numeric(readline('������� ���������� �� ������ �������� (� ��):'))

if ( UserDistance > 1389){
  print('������ ����� ��������� �� ��������� ����������� ����������, ���������� ������')
     UserDistance = as.numeric(readline('������� ���������� �� ������ �������� (� ��):'))
} else { 
    i = 1
    while (UserDistance > 1389 - Cities$Distance_mouth[i]){
      i = i + 1   
    } 
}

#UserDistance
BackwardCity = Cities$Towns[i - 1] # ����� ������� ��� �������
ForwardCity = Cities$Towns[i] # ����� �� �������� ��� �� ������


Persent = 100 * UserDistance / 1389 # ������� ����������� ���� 
Persent = round(Persent, 0)


CurrentHigh = Cities$Zero_hydropost[i - 1]  - (UserDistance - (1389 - Cities$Distance_mouth[i - 1])) * 
              (Cities$Zero_hydropost[i - 1] - Cities$Zero_hydropost[i]) / 
              abs(Cities$Distance_mouth[i] - Cities$Distance_mouth[i - 1]) # ������ �������� ���. ������ � �����

CurrentHigh = round(CurrentHigh, 1) #����������
print(paste(sep = "", "�� ���������� ����� �������� ",BackwardCity," � ",ForwardCity,", �������� ",Persent,"% ��������, ���� ������ �������� ",CurrentHigh,"�."))

