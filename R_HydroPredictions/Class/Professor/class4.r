Sys.setlocale("LC_ALL","Russian")
# ������������ ���������
df <- data.frame(obs = rnorm(10),
                 pred = rnorm(10))
# ����� ������� - ������������������ ���
newcol <- seq.Date(from = as.Date('2020-09-21'), by = '1 day', length.out = 10)
newcol
# ��������� � ��
df <- cbind(df, newcol)
df
# ����� ������ - ������
newrow <- c(rnorm(1), rnorm(1), '2020-10-01')
newrow
# ��������� � ������� �� ��� ������, � ������� ������������ ������� ��
df <- rbind(df, newrow)

# ���������� ������ ��� ����������
newrow <- list(rnorm(1), rnorm(1), '2020-10-01')
df <- rbind(df, newrow)

setwd('d:/YandexDisk/������/R forecasts/������')
# ������ ������ �� ������� ���������� ������������� ����������
xls_files <- list.files(pattern = '*.xls')
xls_files
library(readxl)
# ��������� ���� �� ������ � ������ ��� ��������� � ��������� ��������
prog_apr <- read_xls(xls_files[7], skip = 10, 
                     col_names = c('year', 'pred', 'pred1', 'obs'), 
                     col_types = c('numeric', 'numeric', 'skip', 'numeric'))

# ������� 1: ������ ���������� ���� ������ � ����� � ����������� � ������ ��������� �� ������
prog_df <- data.frame()
for (x in xls_files){
  print(x)
  df <- read_xls(x, skip = 10, 
                 col_names = c('year', 'pred', 'pred1', 'obs'), 
                 col_types = c('numeric', 'numeric', 'skip', 'numeric'))
  print(dim(df))
  prog_df <- rbind(prog_df, df)
}

# ������� 2: ������� ������� ��� ����������...
read_prog <- function(x){
  df <- read_xls(x, skip = 10, 
                 col_names = c('year', 'pred', 'pred1', 'obs'), 
                 col_types = c('numeric', 'numeric', 'skip', 'numeric'))
}
# ...� ��������� �� �� ���� ������ � ������� lapply
prog_list <- lapply(xls_files, read_prog)
# ����� ���� ���������� �� ������ � ���������
prog_list_df <- do.call(what = rbind, args = prog_list)

# ��������� ������� � ���������� �������
prog_df$month <- rep(month.abb[c(1, 10:12, 2:9)], each = 54)
prog_df$month <- factor(prog_df$month, levels = month.abb, ordered = T)
str(prog_df)

summary(prog_df)
# ���������� ������ 
library(ggplot2)
# �����
ggplot(prog_df, aes(x = year)) + 
  geom_line(aes(y=obs, col='����������'), size=2) + 
  geom_line(aes(y=pred, col='�������'), linetype='dashed') + 
  facet_wrap(.~month, scales = 'free_y') + 
  labs(x='���', y=expression('������, �'^3*'/�'), col='������')
# �����
ggplot(prog_df, aes(x=obs, y=pred, col=month)) + geom_point(size=3) + 
  geom_smooth(aes(group=1),method = 'lm', formula = y~x, show.legend = F, se = F) + 
  geom_abline() + xlim(-1000, 10000) + ylim(-1000, 10000) + facet_wrap(.~month)
# ����� �� �������
ggplot(prog_df, aes(x=obs, y=pred, col=month)) + geom_point(size=3) + 
  geom_smooth(aes(group=1),method = 'lm', formula = y~x, show.legend = F, se = F) + 
  geom_abline() + facet_wrap(.~month, scales = 'free')
