

lat_START = as.numeric(readline('������� ������ ��������� �����:')) # ���� ��������  �������������
long_START = as.numeric(readline('������� ������� ��������� �����:'))
lat_FINISH = as.numeric(readline('������� ������ �������� �����:'))
long_FINISH = as.numeric(readline('������� ������� �������� �����:'))
#pi = 3.14
PP = pi / 180
lat_START = lat_START * PP   # ������� ��������� �� ������� � �������
long_START = long_START * PP  
lat_FINISH = lat_FINISH * PP  
long_FINISH = long_FINISH * PP  


D = acos(sin(lat_START) * sin(lat_FINISH) + cos(lat_START) * 
           cos(lat_FINISH) * cos( long_START - long_FINISH )) # ���������� ����� ��������� 
S = D * 6375 # ���������� ���������� 
S = round(S, 0) # ���������� ����������

Time = S / 850 # ���������� �������
Time = round(Time * 2) / 2


if (long_START * long_FINISH < 0) { #�����������, ���������� �� ������� 180 ��� 0 �������� 
  
  if (((abs(long_START) + abs(long_FINISH)) / pi * 180) > 180) { 
    cat("����� ������ ��������� ",S," ��, ����� � ���� ~ ",Time," ����(��). 
        ������� ������ �� ���������� ������� �������� � ���������� 180-� ��������.")
  } else { 
    cat("����� ������ ��������� ",S," ��, ����� � ���� ~ ",Time," ����(��). 
        ������� ������  ���������� ������� �������� � �� ���������� 180-� ��������.")
  }
} else { 
  cat("����� ������ ��������� ",S," ��, ����� � ���� ~ ",Time," ����(��). 
        ������� ������  �� ���������� ������� �������� � �� ���������� 180-� ��������.")
}


