my_matrix = matrix(rnorm(n = 12, mean = 0, sd = 1), byrow = T, nrow = 4)
my_matrix

class(my_matrix)
typeof(my_matrix)
dim(my_matrix)
str(my_matrix)
ncol(my_matrix)
nrow(my_matrix)
length(my_matrix)
summary(my_matrix)

my_matrix[my_matrix > 1]
my_matrix[,1]

# списки
my_list = list(month.name, my_matrix, runif(10))
my_list
class(my_list)
my_list[1]


# датафреймы
df = data.frame(id = 1:30, 
                a = rnorm(30,0,1),
                b = rnorm(30,15,1),
                c = rnorm(30,30,1))
df$id
df[,1]

library(reshape2)
df_long = melt(df, id.vars = 'id')
df_long

# графика
library(ggplot2)

ggplot(df_long, aes(x = value, fill = variable)) + stat_density(alpha = 0.9)
ggplot(df_long, aes(x = id, y = value, col = variable)) + geom_line() + geom_point()
# факторы

df_long$variable = factor(df_long$variable, levels = c('a', 'b', 'c'), ordered = T)
# запись фаилов
write.csv(df, file = 'df.csv')
getwd()
setwd()
write.csv(df, file = 'df.csv', quote = F, row.names = F)
write.xlsx(df, path = 'df.xlsx')
