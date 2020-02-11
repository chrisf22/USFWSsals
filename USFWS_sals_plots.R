#load("/Users/chrisfield/Desktop/A1F1_1k_ordered.Rdata")

popsizes <- array(data = NA, c(Q, Y, 9))
pop_mean <- mat.or.vec(Y, 9)
pop_lower <- mat.or.vec(Y, 9)
pop_upper <- mat.or.vec(Y, 9)
PVA_EX <- mat.or.vec(Y, 9)
extinct = mat.or.vec(Q, 9)
geo_means = mat.or.vec(Q, 9)
extinct_manage = mat.or.vec(Q, 9)

for(z in 1:9){
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
extinct[extinct==Inf] <- 50

### plot for global projections for tide management ###

#quartz.options(width=6.653, height=6.653)
quartz.options(width=4.33, height=4)
#layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
par(mar=c(3, 3, 1.1, 1))
plot(pop_mean[,1], ylim=c(-7000, 25500), xlim=c(0, 65), type="l", col="black", lwd=1.7, yaxt="n", xaxt="n", ylab=" ", xlab=" ")
lines(pop_lower[,1], lty=2, col="black", lwd=1.7)
lines(pop_upper[,1], lty=2, col="black", lwd=1.7)
axis(side=2, c(0, 5000, 10000, 15000, 20000, 25000), labels=c(0, 5, 10, 15, 20, 25), cex.axis=.8, mgp=c(3, .7, 0), las=1, tck=.02)
axis(side=1, at=c(2, 22, 42, 62), labels=c(1, 21, 41, 61)+2014, cex.axis=.8, mgp=c(3, .4, 0), tck=.02)
mtext(side=2, line=1.8, "Female population size (thousands)", cex=.75)
mtext(side=1, line=1.4, "Year", cex=.75)
mtext(side=3, line=.25, font=2, "Tide gate management", adj=0, cex=.75)

segments(quantile(extinct[,1], c(.025)), -1000, quantile(extinct[,1], c(.975)), -1000, lwd=7.5, col="black", lend="butt")
segments(quantile(extinct[,1], c(.25)), -1000 - 350, quantile(extinct[,1], c(.25)), -1000 + 350, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,1], c(.75)), -1000 - 350, quantile(extinct[,1], c(.75)), -1000 + 350, lwd=1, col="white", lend="butt")
segments(median(extinct[,1]), -1000 - 350, median(extinct[,1]), -1000 + 350, lwd=2, col="white", lend="butt")

segments(quantile(extinct[,2], c(.025)), -2500, quantile(extinct[,2], c(.975)), -2500, lwd=7.5, col="dark gray", lend="butt")
segments(quantile(extinct[,2], c(.25)), -2500 - 350, quantile(extinct[,2], c(.25)), -2500 + 350, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,2], c(.75)), -2500 - 350, quantile(extinct[,2], c(.75)), -2500 + 350, lwd=1, col="white", lend="butt")
segments(median(extinct[,2]), -2500 - 350, median(extinct[,2]), -2500 + 350, lwd=2, col="white", lend="butt")

segments(quantile(extinct[,3], c(.025)), -4000, quantile(extinct[,3], c(.975)), -4000, lwd=7.5, col="dark gray", lend="butt")
segments(quantile(extinct[,3], c(.25)), -4000 - 350, quantile(extinct[,3], c(.25)), -4000 + 350, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,3], c(.75)), -4000 - 350, quantile(extinct[,3], c(.75)), -4000 + 350, lwd=1, col="white", lend="butt")
segments(median(extinct[,3]), -4000 - 350, median(extinct[,3]), -4000 + 350, lwd=2, col="white", lend="butt")

lines(pop_mean[,4], col="cadet blue", lwd=1.7)
lines(pop_lower[,4], lty=2, col="cadet blue", lwd=1.7)
lines(pop_upper[,4], lty=2, col="cadet blue", lwd=1.7)
segments(quantile(extinct[,4], c(.025)), -5500, quantile(extinct[,4], c(.975)), -5500, lwd=7.5, col="cadet blue", lend="butt")
segments(quantile(extinct[,4], c(.25)), -5500 - 350, quantile(extinct[,4], c(.25)), -5500 + 350, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,4], c(.75)), -5500 - 350, quantile(extinct[,4], c(.75)), -5500 + 350, lwd=1, col="white", lend="butt")
segments(median(extinct[,4]), -5500 - 350, median(extinct[,4]), -5500 + 350, lwd=2, col="white", lend="butt")

segments(quantile(extinct[,5], c(.025)), -7000, quantile(extinct[,5], c(.975)), -7000, lwd=7.5, col="dark gray", lend="butt")
segments(quantile(extinct[,5], c(.25)), -7000 - 350, quantile(extinct[,5], c(.25)), -7000 + 350, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,5], c(.75)), -7000 - 350, quantile(extinct[,5], c(.75)), -7000 + 350, lwd=1, col="white", lend="butt")
segments(median(extinct[,5]), -7000 - 350, median(extinct[,5]), -7000 + 350, lwd=2, col="white", lend="butt")

text(c(16, 16, 16, 16, 16), c(-1000, -2500, -4000, -5500, -7000), c("a", "b", "c", "d", "e"), cex=0.75)

### plot for global projections for different management scenarios ###

quartz.options(width=4.4, height=3)
#quartz.options(width=4.33, height=4.5)
layout(matrix(c(1, 2), 1, 2, byrow = TRUE))

delta_ex <- mat.or.vec(Q, 9)
for(i in 1:9){
delta_ex[, i] <- extinct[,i] - extinct[,1]
quantile(delta_ex, c(.025, 0.975))
mean(delta_ex)
}

delta_geo <- mat.or.vec(Q, 9)
for(i in 1:9){
  delta_geo[, i] <- geo_means[,i] - geo_means[,1]
  quantile(delta_geo, c(.025, 0.975))
  mean(delta_geo)
}

par(mar=c(3.5, 2, 1, 0))
plot(pop_mean, ylim=c(0.5,8.5), xlim=c(-10, 20), type="l", lwd=1.7, yaxt="n", ylab=" ", xlab=" ", col=rgb(0, 0, 0, 0), bty="n", xaxt="n")
abline(v=0)
segments(quantile(delta_ex[,9], c(.025)), 1, quantile(delta_ex[,9], c(0.975)), 1, lend="butt", lwd=7, col="chocolate1")
segments(quantile(delta_ex[,8], c(.025)), 2, quantile(delta_ex[,8], c(0.975)), 2, lend="butt", lwd=7, col="firebrick2")
segments(quantile(delta_ex[,7], c(.025)), 3, quantile(delta_ex[,7], c(0.975)), 3, lend="butt", lwd=7, col="cadet blue")
segments(quantile(delta_ex[,6], c(.025)), 4, quantile(delta_ex[,6], c(0.975)), 4, lend="butt", lwd=7, col="cadet blue")
segments(quantile(delta_ex[,2], c(.025)), 8, quantile(delta_ex[,2], c(0.975)), 8, lend="butt", lwd=7)
segments(quantile(delta_ex[,3], c(.025)), 7, quantile(delta_ex[,3], c(0.975)), 7, lend="butt", lwd=7)
segments(quantile(delta_ex[,4], c(.025)), 6, quantile(delta_ex[,4], c(0.975)), 6, lend="butt", lwd=7)
segments(quantile(delta_ex[,5], c(.025)), 5, quantile(delta_ex[,5], c(0.975)), 5, lend="butt", lwd=7)
points(mean(delta_ex[,9]), 1, pch=16, col="white")
points(mean(delta_ex[,8]), 2, pch=16, col="white")
points(mean(delta_ex[,7]), 3, pch=16, col="white")
points(mean(delta_ex[,6]), 4, pch=16, col="white")
points(mean(delta_ex[,2]), 8, pch=16, col="white")
points(mean(delta_ex[,3]), 7, pch=16, col="white")
points(mean(delta_ex[,4]), 6, pch=16, col="white")
points(mean(delta_ex[,5]), 5, pch=16, col="white")
text(c(-10, -10, -10, -10), c(8, 7, 6, 5, 4, 3, 2, 1), c("b", "c", "d", "e", "f", "g", "h", "i"), cex=0.8)
axis(side=1, cex.axis=0.75, at=c(-10, 0, 10, 20), mgp=c(3, 0.75, 0))
mtext(side=1, line=1.5, "Extinction delay (years)", cex=0.7)

par(mar=c(3.5, 1, 1, 1))
plot(pop_mean, ylim=c(0.5,8.5), xlim=c(-0.15, 0.3), type="l", lwd=1.7, yaxt="n", ylab=" ", xlab=" ", col=rgb(0, 0, 0, 0), bty="n", xaxt="n")
abline(v=0)
segments(quantile(delta_geo[,9], c(.025)), 1, quantile(delta_geo[,9], c(0.975)), 1, lend="butt", lwd=7, col="chocolate1")
segments(quantile(delta_geo[,8], c(.025)), 2, quantile(delta_geo[,8], c(0.975)), 2, lend="butt", lwd=7, col="firebrick2")
segments(quantile(delta_geo[,7], c(.025)), 3, quantile(delta_geo[,7], c(0.975)), 3, lend="butt", lwd=7, col="cadet blue")
segments(quantile(delta_geo[,6], c(.025)), 4, quantile(delta_geo[,6], c(0.975)), 4, lend="butt", lwd=7, col="cadet blue")
segments(quantile(delta_geo[,2], c(.025)), 8, quantile(delta_geo[,2], c(0.975)), 8, lend="butt", lwd=7)
segments(quantile(delta_geo[,3], c(.025)), 7, quantile(delta_geo[,3], c(0.975)), 7, lend="butt", lwd=7)
segments(quantile(delta_geo[,4], c(.025)), 6, quantile(delta_geo[,4], c(0.975)), 6, lend="butt", lwd=7)
segments(quantile(delta_geo[,5], c(.025)), 5, quantile(delta_geo[,5], c(0.975)), 5, lend="butt", lwd=7)
points(mean(delta_geo[,9]), 1, pch=16, col="white")
points(mean(delta_geo[,8]), 2, pch=16, col="white")
points(mean(delta_geo[,7]), 3, pch=16, col="white")
points(mean(delta_geo[,6]), 4, pch=16, col="white")
points(mean(delta_geo[,2]), 8, pch=16, col="white")
points(mean(delta_geo[,3]), 7, pch=16, col="white")
points(mean(delta_geo[,4]), 6, pch=16, col="white")
points(mean(delta_geo[,5]), 5, pch=16, col="white")
axis(side=1, cex.axis=0.75, mgp=c(3, 0.75, 0), at=c(-0.1, 0, 0.1, 0.2, 0.3), labels=c(-0.1, 0, 0.1, 0.2, 0.3)*100)
mtext(side=1, line=1.5, "Percent change in annual growth rate", cex=0.7)
mtext(side=1, line=2.1, "(over the first 10 years)", cex=0.7)

### plot multi-state comparison of tide gate management ###

#load("/Users/chrisfield/Desktop/A1F1_1k_ordered.Rdata")

quartz.options(width=3.14, height=6.653)
#quartz.options(width=4.33, height=4.5)
layout(matrix(c(1, 2, 3, 4, 5, 6, 7), 7, 1, byrow = TRUE))

year_lab <- c(" ", " ", " ", " ", " ", " ", "Year")
mar <- c(2, 2, 2 ,2, 2, 2, 2.5)
states <- c("New Jersey", "New York", "Connecticut", "Rhode Island", "Massachusetts", "New Hampshire", "Maine")
management <- c("Tide gate management", " ", " ", " ", " ", " ", " ")


for(s in 1:7){

popsizes <- array(data = NA, c(Q, Y, 5))
for(q in 1:Q){
  popsizes[q, ,1] <- PVA[[q]][s,1:Y,1]
  popsizes[q, ,2] <- PVA[[q]][s,1:Y,2]
  popsizes[q, ,3] <- PVA[[q]][s,1:Y,3]
  popsizes[q, ,4] <- PVA[[q]][s,1:Y,4]
  popsizes[q, ,5] <- PVA[[q]][s,1:Y,5]
}

pop_mean <- mat.or.vec(Y, 5)
pop_lower <- mat.or.vec(Y, 5)
pop_upper <- mat.or.vec(Y, 5)
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
pop_mean[,5] <- colMeans(popsizes[,,5])
pop_lower[,5] <- apply(popsizes[,,5], MARGIN=c(2), function (x) quantile(x, c(.025)))
pop_upper[,5] <- apply(popsizes[,,5], MARGIN=c(2), function (x) quantile(x, c(.975)))

PVA_EX <- mat.or.vec(Y, 5)
extinct = mat.or.vec(Q, 5)
extinct_manage = mat.or.vec(Q, 5)
for(i in 1:Q){
  #create an array that indexes whether the population is extinct (0)
  PVA_EX[,1] <- popsizes[i, ,1]
  PVA_EX[,2] <- popsizes[i, ,2]
  PVA_EX[,3] <- popsizes[i, ,3]
  PVA_EX[,4] <- popsizes[i, ,4]
  PVA_EX[,5] <- popsizes[i, ,5]
  PVA_EX[PVA_EX > 0] <- 1
  extinct[i,1] <- min(which(PVA_EX[,1] == 0))
  extinct[i,2] <- min(which(PVA_EX[,2] == 0))
  extinct[i,3] <- min(which(PVA_EX[,3] == 0))
  extinct[i,4] <- min(which(PVA_EX[,4] == 0))
  extinct[i,5] <- min(which(PVA_EX[,5] == 0))
}
extinct[extinct==Inf] <- 50

par(mar=c(mar[s], 1, 1.5, 1))
plot(pop_mean, ylim=c(0.5,5.5), xlim=c(0, 65), type="l", lwd=1.7, yaxt="n", xaxt="n", ylab=" ", xlab=" ", col=rgb(0, 0, 0, 0))
axis(side=1, at=c(2, 22, 42, 62), labels=c(1, 21, 41, 61)+2014, cex.axis=.8, mgp=c(3, .4, 0), tck=.02)
mtext(side=1, line=1.25, year_lab[s], cex=.6)
mtext(side=3, line=0.25, states[s], cex=.6, adj=0)
mtext(side=3, line=0.25, management[s], cex=.6, adj=1, font=2)
abline(v=c(2, 22, 42, 62), col="black", lwd=0.4)
text(c(10, 10, 10, 10, 10), c(5, 4, 3, 2, 1), c("a", "b", "c", "d", "e"), cex=0.75)

segments(quantile(extinct[,5], c(.025)), 1, quantile(extinct[,5], c(.975)), 1, lwd=5, col="dark gray", lend="butt")
segments(quantile(extinct[,4], c(.025)), 2, quantile(extinct[,4], c(.975)), 2, lwd=5, col="dark gray", lend="butt")
segments(quantile(extinct[,3], c(.025)), 3, quantile(extinct[,3], c(.975)), 3, lwd=5, col="dark gray", lend="butt")
segments(quantile(extinct[,2], c(.025)), 4, quantile(extinct[,2], c(.975)), 4, lwd=5, col="dark gray", lend="butt")
segments(quantile(extinct[,1], c(.025)), 5, quantile(extinct[,1], c(.975)), 5, lwd=5, col="black", lend="butt")
segments(quantile(extinct[,5], c(.25)), 0.5, quantile(extinct[,5], c(.25)), 1.5, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,5], c(.75)), 0.5, quantile(extinct[,5], c(.75)), 1.5, lwd=1, col="white", lend="butt")
segments(median(extinct[,5]), 0.5, median(extinct[,5]), 1.5, lwd=2, col="white", lend="butt")
segments(quantile(extinct[,4], c(.25)), 1.5, quantile(extinct[,4], c(.25)), 2.5, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,4], c(.75)), 1.5, quantile(extinct[,4], c(.75)), 2.5, lwd=1, col="white", lend="butt")
segments(median(extinct[,4]), 1.5, median(extinct[,4]), 2.5, lwd=2, col="white", lend="butt")
segments(quantile(extinct[,3], c(.25)), 2.5, quantile(extinct[,3], c(.25)), 3.5, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,3], c(.75)), 2.5, quantile(extinct[,3], c(.75)), 3.5, lwd=1, col="white", lend="butt")
segments(median(extinct[,3]), 2.5, median(extinct[,3]), 3.5, lwd=2, col="white", lend="butt")
segments(quantile(extinct[,2], c(.25)), 3.5, quantile(extinct[,2], c(.25)), 4.5, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,2], c(.75)), 3.5, quantile(extinct[,2], c(.75)), 4.5, lwd=1, col="white", lend="butt")
segments(median(extinct[,2]), 3.5, median(extinct[,2]), 4.5, lwd=2, col="white", lend="butt")
segments(quantile(extinct[,1], c(.25)), 4.5, quantile(extinct[,1], c(.25)), 5.5, lwd=1, col="white", lend="butt")
segments(quantile(extinct[,1], c(.75)), 4.5, quantile(extinct[,1], c(.75)), 5.5, lwd=1, col="white", lend="butt")
segments(median(extinct[,1]), 4.5, median(extinct[,1]), 5.5, lwd=2, col="white", lend="butt")
}


### spare code ####
test <- (seq(50, 300, by=10)/(3*100))

test <- seq(0, 1, by=0.1)
(.5/(1+exp(-(-8 + 25*(allee_prop[individs_bysite]-(1/3))))))
(.5/(1+exp(-(-8 + 25*(test-(1/3))))))

plot(seq(50, 300, by=10), (.5/(1+exp(-(-6.2 + 27*(test - (1/2)))))))

