# set working directory
setwd("/Users/chrisfield/Dropbox/PVA/MCMC/GCB")
# load projections for NYC from LocalizeSL from Kopp et al. 2014
NYC_LSL_allscens <- read.csv(file = "NYCSL_4scen.csv", header=FALSE, sep=",", stringsAsFactors=FALSE, quote="")

# scenarios are from L to R: rcp85, rcp60, rcp45, rcp26
# years are 10 year increments starting in 2010
# units are in mm
# create a matrix of NAs to populate with decadal projections using LocalizeSL
# matrix will be use to interpolate values using loess
NYC_LSL_rcp85 <- matrix(NA, 10000, 101)
NYC_LSL_rcp85[,11] <- NYC_LSL_allscens[,1]
NYC_LSL_rcp85[,21] <- NYC_LSL_allscens[,2]
NYC_LSL_rcp85[,31] <- NYC_LSL_allscens[,3]
NYC_LSL_rcp85[,41] <- NYC_LSL_allscens[,4]
NYC_LSL_rcp85[,51] <- NYC_LSL_allscens[,5]
NYC_LSL_rcp85[,61] <- NYC_LSL_allscens[,6]
NYC_LSL_rcp85[,71] <- NYC_LSL_allscens[,7]
NYC_LSL_rcp85[,81] <- NYC_LSL_allscens[,8]
NYC_LSL_rcp85[,91] <- NYC_LSL_allscens[,9]
NYC_LSL_rcp85[,101] <- NYC_LSL_allscens[,10]

NYC_LSL_rcp60 <- matrix(NA, 10000, 101)
NYC_LSL_rcp60[,11] <- NYC_LSL_allscens[,21]
NYC_LSL_rcp60[,21] <- NYC_LSL_allscens[,22]
NYC_LSL_rcp60[,31] <- NYC_LSL_allscens[,23]
NYC_LSL_rcp60[,41] <- NYC_LSL_allscens[,24]
NYC_LSL_rcp60[,51] <- NYC_LSL_allscens[,25]
NYC_LSL_rcp60[,61] <- NYC_LSL_allscens[,26]
NYC_LSL_rcp60[,71] <- NYC_LSL_allscens[,27]
NYC_LSL_rcp60[,81] <- NYC_LSL_allscens[,28]
NYC_LSL_rcp60[,91] <- NYC_LSL_allscens[,29]
NYC_LSL_rcp60[,101] <- NYC_LSL_allscens[,30]

NYC_LSL_rcp45 <- matrix(NA, 10000, 101)
NYC_LSL_rcp45[,11] <- NYC_LSL_allscens[,41]
NYC_LSL_rcp45[,21] <- NYC_LSL_allscens[,42]
NYC_LSL_rcp45[,31] <- NYC_LSL_allscens[,43]
NYC_LSL_rcp45[,41] <- NYC_LSL_allscens[,44]
NYC_LSL_rcp45[,51] <- NYC_LSL_allscens[,45]
NYC_LSL_rcp45[,61] <- NYC_LSL_allscens[,46]
NYC_LSL_rcp45[,71] <- NYC_LSL_allscens[,47]
NYC_LSL_rcp45[,81] <- NYC_LSL_allscens[,48]
NYC_LSL_rcp45[,91] <- NYC_LSL_allscens[,49]
NYC_LSL_rcp45[,101] <- NYC_LSL_allscens[,50]

NYC_LSL_rcp26 <- matrix(NA, 10000, 101)
NYC_LSL_rcp26[,11] <- NYC_LSL_allscens[,61]
NYC_LSL_rcp26[,21] <- NYC_LSL_allscens[,62]
NYC_LSL_rcp26[,31] <- NYC_LSL_allscens[,63]
NYC_LSL_rcp26[,41] <- NYC_LSL_allscens[,64]
NYC_LSL_rcp26[,51] <- NYC_LSL_allscens[,65]
NYC_LSL_rcp26[,61] <- NYC_LSL_allscens[,66]
NYC_LSL_rcp26[,71] <- NYC_LSL_allscens[,67]
NYC_LSL_rcp26[,81] <- NYC_LSL_allscens[,68]
NYC_LSL_rcp26[,91] <- NYC_LSL_allscens[,69]
NYC_LSL_rcp26[,101] <- NYC_LSL_allscens[,70]

# interpolate mean values from figures in Kopp et al. 2014
#NYC_LSL_8.5 <- rep(NA, 101)
#NYC_LSL_8.5[1] <- 0
#NYC_LSL_8.5[31] <- 20
#NYC_LSL_8.5[51] <- 38
#NYC_LSL_8.5[101] <- 96

#NYC_LSL_4.5 <- rep(NA, 101)
#NYC_LSL_4.5[1] <- 0
#NYC_LSL_4.5[31] <- 20
#NYC_LSL_4.5[51] <- 36
#NYC_LSL_4.5[101] <- 75

#NYC_LSL_2.6 <- rep(NA, 101)
#NYC_LSL_2.6[1] <- 0
#NYC_LSL_2.6[31] <- 20
#NYC_LSL_2.6[51] <- 35
#NYC_LSL_2.6[101] <- 62

x <- 1:101

plot(predict(loess(NYC_LSL_rcp85[1,] ~ x, span=0.5), x))
NYC_LSL_rcp85_int <- mat.or.vec(10000, 101)
for(i in 1:10000){
NYC_LSL_rcp85_int[i, ] <- predict(loess(NYC_LSL_rcp85[i,] ~ x, span=0.5), x)
}
# divided by 10 to covert mm to cm
write.csv(NYC_LSL_rcp85_int/10, "/Users/chrisfield/Dropbox/PVA/MCMC/GCB/SLR_Kopp_scen85.csv")

plot(predict(loess(NYC_LSL_rcp60[1,] ~ x, span=0.5), x))
NYC_LSL_rcp60_int <- mat.or.vec(10000, 101)
for(i in 1:10000){
  NYC_LSL_rcp60_int[i, ] <- predict(loess(NYC_LSL_rcp60[i,] ~ x, span=0.5), x)
}
# divided by 10 to covert mm to cm
write.csv(NYC_LSL_rcp60_int/10, "/Users/chrisfield/Dropbox/PVA/MCMC/GCB/SLR_Kopp_scen60.csv")

plot(predict(loess(NYC_LSL_rcp45[1,] ~ x, span=0.5), x))
NYC_LSL_rcp45_int <- mat.or.vec(10000, 101)
for(i in 1:10000){
  NYC_LSL_rcp45_int[i, ] <- predict(loess(NYC_LSL_rcp45[i,] ~ x, span=0.5), x)
}
# divided by 10 to covert mm to cm
write.csv(NYC_LSL_rcp45_int/10, "/Users/chrisfield/Dropbox/PVA/MCMC/GCB/SLR_Kopp_scen45.csv")

plot(predict(loess(NYC_LSL_rcp26[1,] ~ x, span=0.5), x))
NYC_LSL_rcp26_int <- mat.or.vec(10000, 101)
for(i in 1:10000){
  NYC_LSL_rcp26_int[i, ] <- predict(loess(NYC_LSL_rcp26[i,] ~ x, span=0.5), x)
}
# divided by 10 to covert mm to cm
write.csv(NYC_LSL_rcp26_int/10, "/Users/chrisfield/Dropbox/PVA/MCMC/GCB/SLR_Kopp_scen26.csv")

plot(colMeans(NYC_LSL_rcp85_int), type="l")
lines(colMeans(NYC_LSL_rcp60_int), type="l", col="red")
lines(colMeans(NYC_LSL_rcp45_int), type="l", col="blue")
lines(colMeans(NYC_LSL_rcp26_int), type="l", col="green")

