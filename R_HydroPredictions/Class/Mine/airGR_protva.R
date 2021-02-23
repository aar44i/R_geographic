Sys.setlocale("LC_ALL","Russian")
library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)
library(writexl)
library(airGRteaching)
setwd('C:/Users/gorba/DataSciense/R_geographic/R_HydroPredictions/Class/Mine')
getwd()
df <- read_xlsx('protva_t_p_q_2003-2015.xlsx')
summary(df)
# average T
df$T <- rowMeans(df[ ,grepl("[0-9]t",names(df))], na.rm = T)
# average P
df$P <- rowSums(df[ ,grepl("[0-9]p",names(df))], na.rm = T)

# evap
df$E <- PE_Oudin(JD = yday(df$date), Temp = df$T, Lat = 56, LatUnit = "deg") # jd

# ggplot(df, aes(x=date)) + geom_line(aes(y=P, col='prec')) + geom_line(aes(y=E, col='evap'))

# q to mm
df$Qmm <- (df$Qt * 86400 / 4620) / 1000

df <- df[, c(1, 10, 9, 11, 12)]
colnames(df) <- c('DatesR', 'Precip', 'TempMean', 'PotEvap', 'Qobs')

PREP <- PrepGR(ObsDF = df, HydroModel = "GR4J", CemaNeige = T, ZInputs =  120, NLayers = 2)
# png(filename="protva_obs_plot.png")
plot(PREP, main = "Observation")
# dev.off() 

CAL <- CalGR(PrepGR = PREP, CalCrit = "RMSE",
             WupPer = c("2003-01-01", "2005-12-31"), CalPer = c("2006-01-01", "2008-12-31"))
# png(filename="protva_cal_plot.png")
plot(CAL, which = "perf")
plot(CAL, which = "iter")
# dev.off() 

SIM <- SimGR(PrepGR = PREP, Param = CAL, EffCrit = "KGE2",
             WupPer = c("2006-01-01", "2008-12-31"), SimPer = c("2009-01-01", "2015-12-31"))

# png(filename="protva_sim_plot.png", width = 15, height = 21, units = 'cm', res = 300)
plot(SIM) 
plot(SIM, which='ts')
# dev.off() 


ShinyGR(ObsDF = df, SimPer = c("2004-01-01", "2015-12-31"), NamesObsBV = 'Протва - Спас-Загорье')
