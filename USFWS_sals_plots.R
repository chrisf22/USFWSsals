#load("/Users/chrisfield/Desktop/A1F1_1k_ordered.Rdata")

popsizes <- array(data = NA, c(Q, Y, 4))
pop_mean <- mat.or.vec(Y, 4)
pop_lower <- mat.or.vec(Y, 4)
pop_upper <- mat.or.vec(Y, 4)
PVA_EX <- mat.or.vec(Y, 4)
extinct = mat.or.vec(Q, 4)
geo_means = mat.or.vec(Q, 4)
extinct_manage = mat.or.vec(Q, 4)

for(z in 1:4){
for(q in 1:Q){
  popsizes[q, ,z] <- colSums(PVA[[q]][,1:Y,z])
}
pop_mean[,z] <- colMeans(popsizes[,,z])
pop_lower[,z] <- apply(popsizes[,,z], MARGIN=c(2), function (x) quantile(x, c(.025)))
pop_upper[,z] <- apply(popsizes[,,z], MARGIN=c(2), function (x) quantile(x, c(.975)))

for(i in 1:Q){
  #create an array that indexes whether the population is extinct (0)
  PVA_EX[,z] <- popsizes[i, ,z]
  PVA_EX[PVA_EX > 0] <- 1
  extinct[i,z] <- min(which(PVA_EX[,z] == 0))
}
geo_means[,z] <- (popsizes[,10,z]/popsizes[,1,z])^(1/9) - 1
}

### plot for global projections for tide management ###

#quartz.options(width=6.653, height=6.653)
quartz.options(width=4.33, height=4.5)
#layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
par(mar=c(3, 3, 1.1, 1))
plot(pop_mean[,1], ylim=c(-5000, 25500), xlim=c(0, 65), type="l", col="black", lwd=1.7, yaxt="n", xaxt="n", ylab=" ", xlab=" ")
lines(pop_lower[,1], lty=2, col="black", lwd=1.7)
lines(pop_upper[,1], lty=2, col="black", lwd=1.7)
axis(side=2, c(0, 5000, 10000, 15000, 20000, 25000), labels=c(0, 5, 10, 15, 20, 25), cex.axis=.8, mgp=c(3, .7, 0), las=1, tck=.02)
axis(side=1, at=c(2, 22, 42, 62), labels=c(1, 21, 41, 61)+2014, cex.axis=.8, mgp=c(3, .4, 0), tck=.02)
mtext(side=2, line=1.8, "Female population size (thousands)", cex=.75)
mtext(side=1, line=1.4, "Year", cex=.75)
#mtext(side=3, line=.25, font=2, "(a)", adj=0, cex=.75)

segments(quantile(extinct[,1], c(.025)), -1000, quantile(extinct[,1], c(.975)), -1000, lwd=7.5, col="black", lend="butt")
segments(quantile(extinct[,1], c(.25)), -1000 - 350, quantile(extinct[,1], c(.25)), -1000 + 350, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,1], c(.75)), -1000 - 350, quantile(extinct[,1], c(.75)), -1000 + 350, lwd=1, col="white", lend="butt")
segments(median(extinct[,1]), -1000 - 350, median(extinct[,1]), -1000 + 350, lwd=2, col="white", lend="butt")

segments(quantile(extinct[,2], c(.025)), -2000, quantile(extinct[,2], c(.975)), -2000, lwd=7.5, col="dark gray", lend="butt")
segments(quantile(extinct[,2], c(.25)), -2000 - 350, quantile(extinct[,2], c(.25)), -2000 + 350, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,2], c(.75)), -2000 - 350, quantile(extinct[,2], c(.75)), -2000 + 350, lwd=1, col="white", lend="butt")
segments(median(extinct[,2]), -2000 - 350, median(extinct[,2]), -2000 + 350, lwd=2, col="white", lend="butt")

segments(quantile(extinct[,3], c(.025)), -3000, quantile(extinct[,3], c(.975)), -3000, lwd=7.5, col="dark gray", lend="butt")
segments(quantile(extinct[,3], c(.25)), -3000 - 350, quantile(extinct[,3], c(.25)), -3000 + 350, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,3], c(.75)), -3000 - 350, quantile(extinct[,3], c(.75)), -3000 + 350, lwd=1, col="white", lend="butt")
segments(median(extinct[,3]), -3000 - 350, median(extinct[,3]), -3000 + 350, lwd=2, col="white", lend="butt")

lines(pop_mean[,4], col="dark gray", lwd=1.7)
lines(pop_lower[,4], lty=2, col="dark gray", lwd=1.7)
lines(pop_upper[,4], lty=2, col="dark gray", lwd=1.7)
segments(quantile(extinct[,4], c(.025)), -4000, quantile(extinct[,4], c(.975)), -4000, lwd=7.5, col="dark gray", lend="butt")
segments(quantile(extinct[,4], c(.25)), -4000 - 350, quantile(extinct[,4], c(.25)), -4000 + 350, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,4], c(.75)), -4000 - 350, quantile(extinct[,4], c(.75)), -4000 + 350, lwd=1, col="white", lend="butt")
segments(median(extinct[,4]), -4000 - 350, median(extinct[,4]), -4000 + 350, lwd=2, col="white", lend="butt")


### plot for global projections for different management scenarios ###

quartz.options(width=3.14, height=5)
#quartz.options(width=4.33, height=4.5)
layout(matrix(c(1, 2), 1, 2, byrow = TRUE))

delta_ex <- mat.or.vec(Q, 4)
for(i in 1:4){
delta_ex[, i] <- extinct[,1] - extinct[,i]
quantile(delta_ex, c(.025, 0.975))
mean(delta_ex)
}

# loop for geo means here

par(mar=c(3, 3, 1, 1))
plot(pop_mean, ylim=c(0.5,4.5), xlim=c(-10, 10), type="l", lwd=1.7, yaxt="n", xaxt="n", ylab=" ", xlab=" ", col=rgb(0, 0, 0, 0))
points(mean(delta_ex[,2]), 1, pch=16)
points(mean(delta_ex[,3]), 2, pch=16)
points(mean(delta_ex[,4]), 3, pch=16)
segments(quantile(delta_ex[,2], c(.025)), 1, quantile(delta_ex[,2], c(0.975)), 1, lend="butt")
segments(quantile(delta_ex[,3], c(.025)), 2, quantile(delta_ex[,3], c(0.975)), 2, lend="butt")
segments(quantile(delta_ex[,4], c(.025)), 3, quantile(delta_ex[,4], c(0.975)), 3, lend="butt")
abline(v=0)

### plot multi-state comparison of tide gate management ###

#load("/Users/chrisfield/Desktop/A1F1_1k_ordered.Rdata")

quartz.options(width=3.14, height=6.653)
#quartz.options(width=4.33, height=4.5)
layout(matrix(c(1, 2, 3, 4, 5, 6, 7), 7, 1, byrow = TRUE))

year_lab <- c(" ", " ", " ", " ", " ", " ", "Year")
mar <- c(2, 2, 2 ,2, 2, 2, 2.5)
states <- c("New Jersey", "New York", "Connecticut", "Rhode Island", "Massachusetts", "New Hampshire", "Maine")

for(s in 1:7){

popsizes <- array(data = NA, c(Q, Y, 4))
for(q in 1:Q){
  popsizes[q, ,1] <- PVA[[q]][s,1:Y,1]
  popsizes[q, ,2] <- PVA[[q]][s,1:Y,2]
  popsizes[q, ,3] <- PVA[[q]][s,1:Y,3]
  popsizes[q, ,4] <- PVA[[q]][s,1:Y,4]
}

pop_mean <- mat.or.vec(Y, 4)
pop_lower <- mat.or.vec(Y, 4)
pop_upper <- mat.or.vec(Y, 4)
pop_mean[,1] <- colMeans(popsizes[,,1])
pop_lower[,1] <- apply(popsizes[,,1], MARGIN=c(2), function (x) quantile(x, c(.025)))
pop_upper[,1] <- apply(popsizes[,,1], MARGIN=c(2), function (x) quantile(x, c(.975)))
pop_mean[,2] <- colMeans(popsizes[,,2])
pop_lower[,2] <- apply(popsizes[,,2], MARGIN=c(2), function (x) quantile(x, c(.025)))
pop_upper[,2] <- apply(popsizes[,,2], MARGIN=c(2), function (x) quantile(x, c(.975)))
pop_mean[,3] <- colMeans(popsizes[,,3])
pop_lower[,3] <- apply(popsizes[,,3], MARGIN=c(2), function (x) quantile(x, c(.025)))
pop_upper[,3] <- apply(popsizes[,,3], MARGIN=c(2), function (x) quantile(x, c(.975)))
pop_mean[,4] <- colMeans(popsizes[,,4])
pop_lower[,4] <- apply(popsizes[,,4], MARGIN=c(2), function (x) quantile(x, c(.025)))
pop_upper[,4] <- apply(popsizes[,,4], MARGIN=c(2), function (x) quantile(x, c(.975)))

PVA_EX <- mat.or.vec(Y, 4)
extinct = mat.or.vec(Q, 4)
extinct_manage = mat.or.vec(Q, 4)
for(i in 1:Q){
  #create an array that indexes whether the population is extinct (0)
  PVA_EX[,1] <- popsizes[i, ,1]
  PVA_EX[,2] <- popsizes[i, ,2]
  PVA_EX[,3] <- popsizes[i, ,3]
  PVA_EX[,4] <- popsizes[i, ,4]
  PVA_EX[PVA_EX > 0] <- 1
  extinct[i,1] <- min(which(PVA_EX[,1] == 0))
  extinct[i,2] <- min(which(PVA_EX[,2] == 0))
  extinct[i,3] <- min(which(PVA_EX[,3] == 0))
  extinct[i,4] <- min(which(PVA_EX[,4] == 0))
}

par(mar=c(mar[s], 1, 1.5, 1))
plot(pop_mean, ylim=c(0.5,4.5), xlim=c(0, 65), type="l", lwd=1.7, yaxt="n", xaxt="n", ylab=" ", xlab=" ", col=rgb(0, 0, 0, 0))
axis(side=1, at=c(2, 22, 42, 62), labels=c(1, 21, 41, 61)+2014, cex.axis=.8, mgp=c(3, .4, 0), tck=.02)
mtext(side=1, line=1.25, year_lab[s], cex=.6)
mtext(side=3, line=0.25, states[s], cex=.6, adj=0)
abline(v=c(2, 22, 42, 62), col="gray")

segments(quantile(extinct[,1], c(.025)), 1, quantile(extinct[,1], c(.975)), 1, lwd=5, col="black", lend="butt")
segments(quantile(extinct[,2], c(.025)), 2, quantile(extinct[,2], c(.975)), 2, lwd=5, col="dark gray", lend="butt")
segments(quantile(extinct[,3], c(.025)), 3, quantile(extinct[,3], c(.975)), 3, lwd=5, col="dark gray", lend="butt")
segments(quantile(extinct[,4], c(.025)), 4, quantile(extinct[,4], c(.975)), 4, lwd=5, col="dark gray", lend="butt")
segments(quantile(extinct[,1], c(.25)), 0.5, quantile(extinct[,1], c(.25)), 1.5, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,1], c(.75)), 0.5, quantile(extinct[,1], c(.75)), 1.5, lwd=1, col="white", lend="butt")
segments(median(extinct[,1]), 0.5, median(extinct[,1]), 1.5, lwd=2, col="white", lend="butt")
segments(quantile(extinct[,2], c(.25)), 1.5, quantile(extinct[,2], c(.25)), 2.5, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,2], c(.75)), 1.5, quantile(extinct[,2], c(.75)), 2.5, lwd=1, col="white", lend="butt")
segments(median(extinct[,2]), 1.5, median(extinct[,2]), 2.5, lwd=2, col="white", lend="butt")
segments(quantile(extinct[,3], c(.25)), 2.5, quantile(extinct[,3], c(.25)), 3.5, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,3], c(.75)), 2.5, quantile(extinct[,3], c(.75)), 3.5, lwd=1, col="white", lend="butt")
segments(median(extinct[,3]), 2.5, median(extinct[,3]), 3.5, lwd=2, col="white", lend="butt")
segments(quantile(extinct[,4], c(.25)), 3.5, quantile(extinct[,4], c(.25)), 4.5, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,4], c(.75)), 3.5, quantile(extinct[,4], c(.75)), 4.5, lwd=1, col="white", lend="butt")
segments(median(extinct[,4]), 3.5, median(extinct[,4]), 4.5, lwd=2, col="white", lend="butt")
}


### spare code ####
test <- (seq(50, 300, by=10)/(3*100))

test <- seq(0, 1, by=0.1)
(.5/(1+exp(-(-8 + 25*(allee_prop[individs_bysite]-(1/3))))))
(.5/(1+exp(-(-8 + 25*(test-(1/3))))))

plot(seq(50, 300, by=10), (.5/(1+exp(-(-6.2 + 27*(test - (1/2)))))))

