#load("/Users/chrisfield/Desktop/A1F1_1k_ordered.Rdata")

popsizes <- mat.or.vec(Q, Y)
popsizes_manage <- mat.or.vec(Q, Y)
for(q in 1:Q){
  popsizes[q, ] <- colSums(PVA[[q]][,1:Y,1])
  popsizes_manage[q, ] <- colSums(PVA[[q]][,1:Y,2])
}

#popsizes <- matrix(unlist(PVA), ncol=Y+1, byrow=TRUE)
pop_mean <- colMeans(popsizes)
pop_lower <- apply(popsizes, MARGIN=c(2), function (x) quantile(x, c(.025)))
pop_upper <- apply(popsizes, MARGIN=c(2), function (x) quantile(x, c(.975)))
pop_mean_m <- colMeans(popsizes_manage)
pop_lower_m <- apply(popsizes_manage, MARGIN=c(2), function (x) quantile(x, c(.025)))
pop_upper_m <- apply(popsizes_manage, MARGIN=c(2), function (x) quantile(x, c(.975)))

extinct = mat.or.vec(Q, 1)
extinct_manage = mat.or.vec(Q, 1)
for(i in 1:Q){
  #create an array that indexes whether the population is extinct (0)
  PVA_EX <- popsizes[i, ]
  PVA_EX_manage <- popsizes_manage[i, ]
  PVA_EX[PVA_EX > 0] <- 1
  PVA_EX_manage[PVA_EX_manage > 0] <- 1
  extinct[i] <- min(which(PVA_EX == 0))
  extinct_manage[i] <- min(which(PVA_EX_manage == 0))
}

quartz.options(width=6.653, height=6.653)
#quartz.options(width=4.33, height=4.5)
#layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
par(mar=c(3, 3, 1.1, 1))
plot(pop_mean, ylim=c(-1300, 35500), xlim=c(0, 65), type="l", col="dark gray", lwd=1.7, yaxt="n", xaxt="n", ylab=" ", xlab=" ")
lines(pop_lower, lty=2, col="dark gray", lwd=1.7)
lines(pop_upper, lty=2, col="dark gray", lwd=1.7)
axis(side=2, c(0, 5000, 10000, 15000), labels=c(0, 5, 10, 15), cex.axis=.8, mgp=c(3, .7, 0), las=1, tck=.02)
axis(side=1, at=c(2, 22, 42, 62), labels=c(1, 21, 41, 61)+2014, cex.axis=.8, mgp=c(3, .4, 0), tck=.02)
mtext(side=2, line=1.8, "Female population size (thousands)", cex=.75)
mtext(side=1, line=1.4, "Year", cex=.75)
mtext(side=3, line=.25, font=2, "(a)", adj=0, cex=.75)

segments(quantile(extinct, c(.025)), -650, quantile(extinct, c(.975)), -650, lwd=7.5, col="dark gray", lend="butt")
segments(quantile(extinct, c(.25)), -1000, quantile(extinct, c(.25)), -300, lwd=1, col="white", lend="butt")
segments(quantile(extinct, c(.75)), -1000, quantile(extinct, c(.75)), -300, lwd=1, col="white", lend="butt")
segments(median(extinct), -1000, median(extinct), -300, lwd=2, col="white", lend="butt")

lines(pop_mean_m, col="black", lwd=1.7)
lines(pop_lower_m, lty=2, col="black", lwd=1.7)
lines(pop_upper_m, lty=2, col="black", lwd=1.7)

segments(quantile(extinct_manage, c(.025)), -1400, quantile(extinct_manage, c(.975)), -1400, lwd=7.5, col="black", lend="butt")
segments(quantile(extinct_manage, c(.25)), -1750, quantile(extinct_manage, c(.25)), -1050, lwd=1, col="white", lend="butt")
segments(quantile(extinct_manage, c(.75)), -1750, quantile(extinct_manage, c(.75)), -1050, lwd=1, col="white", lend="butt")
segments(median(extinct_manage), -1750, median(extinct_manage), -1050, lwd=2, col="white", lend="butt")



test <- (seq(50, 300, by=10)/(3*100))

test <- seq(0, 1, by=0.1)
(.5/(1+exp(-(-8 + 25*(allee_prop[individs_bysite]-(1/3))))))
(.5/(1+exp(-(-8 + 25*(test-(1/3))))))

plot(seq(50, 300, by=10), (.5/(1+exp(-(-6.2 + 27*(test - (1/2)))))))

