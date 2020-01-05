
NYC_LSL_8.5 <- rep(NA, 101)
NYC_LSL_8.5[1] <- 0
NYC_LSL_8.5[31] <- 20
NYC_LSL_8.5[51] <- 38
NYC_LSL_8.5[101] <- 96

NYC_LSL_4.5 <- rep(NA, 101)
NYC_LSL_4.5[1] <- 0
NYC_LSL_4.5[31] <- 20
NYC_LSL_4.5[51] <- 36
NYC_LSL_4.5[101] <- 75

NYC_LSL_2.6 <- rep(NA, 101)
NYC_LSL_2.6[1] <- 0
NYC_LSL_2.6[31] <- 20
NYC_LSL_2.6[51] <- 35
NYC_LSL_2.6[101] <- 62

x <- 1:101
plot(predict(loess(NYC_LSL_8.5 ~ x, span=1.5), x))
NYC_LSL_8.5_int <- predict(loess(NYC_LSL_8.5 ~ x, span=1.5), x)

plot(predict(loess(NYC_LSL_4.5 ~ x, span=1.5), x))
NYC_LSL_4.5_int <- predict(loess(NYC_LSL_4.5 ~ x, span=1.5), x)

plot(predict(loess(NYC_LSL_2.6 ~ x, span=1.5), x))
NYC_LSL_2.6_int <- predict(loess(NYC_LSL_2.6 ~ x, span=1.5), x)

Kopp_all <- cbind((x+1999), round(NYC_LSL_8.5_int), round(NYC_LSL_4.5_int), round(NYC_LSL_2.6_int))
write.csv(Kopp_all, "/Users/chrisfield/Dropbox/PVA/MCMC/GCB/SLR_Kopp.csv")

