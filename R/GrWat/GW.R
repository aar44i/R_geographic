library(sf)
library(grwat)

wd = "C:/Users/gorba/Desktop/Univercity/3 course/R/GrWat"
setwd(wd)

hdata = read.csv('in_Belyayevskiy.txt',
                 header = FALSE, 
                 sep = ' ') # read gauge data
head(hdata)

basin = st_read('Belyayevskiy.shp', quiet = TRUE) # read basin region
basin_pr = grwat::st_buffer_geo(basin, 50000)  # buffer region by 50 km

rean = grwat::read_interim('pre_.nc', 'temp_.nc') # read reanalysis data
hdata_rean = grwat::join_interim(hdata, rean, basin_pr) # join reanalysis data to hydrological series
head(hdata_rean$df)

grwat::map(rean$pts, hdata_rean$pts, basin, basin_pr)

#wd = "/Volumes/Work/_grwat/2018/"
grwat::process_gauge(wd, rean, bufsize = 50000) # process single folder
#grwat::process_basins(wd, rean, bufsize = 50000) # process single folder
 # 2ой блок работы

sep = grwat::read_separation('AllGrWat.txt')
head(sep)

grwat::plot_separation(sep, 1978) # plot single year
grwat::plot_separation(sep, 1994:1997, # plot four years on the same page
                       layout = matrix(c(1,2,3,4), nrow=2, byrow=TRUE))
df = grwat::read_variables('Total.txt')
head(df)
grwat::test_variables(df, Qmax)
grwat::plot_variables(df, Qmax)
grwat::plot_variables(df, date10w1, Wpol3)
grwat::plot_variables(df, Qmax, Qygr, date10w1, Wpol3, # plot four variables in matrix layout
                      layout = matrix(c(1,2,3,4), nrow=2, byrow=TRUE)) 
grwat::plot_variables(df, date10w1, Wpol3, DaysThawWin, Qmaxpavs,
                      tests = test_variables(df, date10w1, Wpol3, DaysThawWin, Qmaxpavs)) # add test information

grwat::plot_variables(df, tests = test_variables(df))
grwat::plot_periods(df, Qy, Qmax, 
                    tests = test_variables(df, Qy, Qmax),
                    layout = matrix(c(1,2)))
grwat::plot_periods(df, tests = test_variables(df))
grwat::plot_minmonth(df, year = 1978)

 grwat::report_gauge("C:/Users/gorba/Desktop/Univercity/3 course/R/GrWat")
grwat::report_basins("C:/Users/gorba/Desktop/Univercity/3 course/R/GrWat")
