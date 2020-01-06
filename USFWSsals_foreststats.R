#for 30 m buffer
#setwd("/users/chrisfield/Documents/folders/SESYNC/GEEfiles/GEE_exports_20/")
# for 100 m buffer
setwd("/users/chrisfield/Documents/folders/SESYNC/GEEfiles/GEE_exports_20_100m/")

# load files that were exported from GEE
lossStats <- read.csv(file = "lossStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
gainStats <- read.csv(file = "gainStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
coverStats <- read.csv(file = "coverStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
slopeStats <- read.csv(file = "slopeStatsDEM.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
treeCoverStats <- read.csv(file = "treeCoverStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
totalCoverStats <- read.csv(file = "totalCoverStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
naturalStats <- read.csv(file = "naturalStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# calcualte some general stats on forest cover adjacent to tidal marshes
# not medium or high density development (23 or 24) according to NLCD
natural <- naturalStats$sum
# tree cover in each patch according to Hansen et al. 2018 in terms of number of pixels 
# original data shows the percent cover in each pixel summed over the patch)
treeCover <- treeCoverStats$sum/100
# total cover of any land type; the number of pixels of Hansen et al. 2018 (forest and not forest) in each patch
totalCover <- totalCoverStats$count
# get the total area of the 30m/100m buffer that is forest
#IN THIS CASE totalCover MIGHT INCLUDE TIDAL MARSH
totalTreeCoverProp <- sum(treeCover)/sum(totalCover)
# get the total proportion of tree cover by patch
treeCoverPropPatch <- treeCover/totalCover
# get total tree cover (from Hansen et al. 2018) as a proportion of total natural habitat (from NLCD)
#IN THIS CASE NATURAL INCLUDES TIDAL MARSH
treeCoverPropNatural <- sum(treeCover)/sum(natural)

# total number of pixels that experienced loss in each patch
loss <- lossStats$sum[coverStats$sum>0]
# total number of pixels that experience gain in each patch
gain <- gainStats$sum[coverStats$sum>0]
# total forest cover in 2000; sum of the proportion of each pixel that was forested in 2000
treecover <- treeCoverStats$sum[coverStats$sum>0]
# binary sum of forest cover in 2000
cover <- coverStats$sum[coverStats$sum>0]
# proportion of each patch that experiences forest loss between 2000-2018
observed <- lossStats$sum[coverStats$sum>0]/coverStats$sum[coverStats$sum>0]
long <- lossStats$X[coverStats$sum>0]
lat <- lossStats$Y[coverStats$sum>0]
# for both lat and long, 5305 and 5306 are zero; temp replace zeros with mean longitude 
long[long==0] <- mean(long[long!=0])
lat[lat==0] <- mean(lat[lat!=0])
# log transform area to handle extremely large patches
# use either area of total patch, including tidal marsh
# or the cover vector (from Hansen et al. 2018), which is the number of forest pixels greater than a cover threshold (30%)
#area <- log(lossStats$area_ha[coverStats$sum>0])
area <- log(cover)
#area <- cover
slope <- slopeStats$mean[coverStats$sum>0]
# 117 values for slope are NAs; temp replaces zeros with mean slope
slope[is.na(slope)] <- mean(slope, na.rm=TRUE)
# create binomial variable for loss or not at the patch level
loss_bin <- loss
loss_bin[loss_bin > 0] <- 1


head(lossStats)




# Pr of at least some loss
#BUGS model
PEEPS <- function(){
  sdC ~ dunif(0, 100)
  tauC <- 1/(sdC*sdC)
  mu ~ dnorm(0, 0.04)
  for(e in 1:10){
    #C[e] ~ dnorm(0, tauB)
    C[e] ~ dt(0, tauC, 4)
  }
  for(i in 1:length(loss)){ 
    logit(pi[i]) <- mu + C[1]*slope[i] + C[2]*area[i] + C[3]*evergreenProp[i] + C[4]*SLR[i] + C[6]*sandy[i] + C[7]*area[i]*area[i]
    + C[5]*slope[i]*SLR[i] + C[8]*evergreenProp[i]*lat[i]
    q[i] <- (1-pi[i])^cover[i]
    loss_bin_inv[i] ~ dbern(q[i])
  }
}

if (is.R()){
  filename <- file.path(tempdir(), "PEEPS.bug")}
write.model(PEEPS, filename)
inits <- list(list(C=c(1, -3.5, -2, 1.5, 0.5, -0.25, 1, -1, 0, 0), mu=-10))
data <- list("lat", "area", "slope", "cover", "evergreenProp", "SLR", "sandy", "loss", "loss_bin_inv")
parameters <- c("mu", "C", "sdC", "pi")
PEEPS <- jags(data=data, inits=inits, parameters.to.save=parameters, filename,
              n.chains=1, n.burnin=10000, n.iter=20000, n.thin=1, DIC=TRUE)

cor(cbind(lat, slope, area, evergreenProp, SLR))

library(pROC)
#to get ROC
pi <- mat.or.vec(length(loss_bin), 10000)
for(i in 1:length(loss_bin)){
  pi[i,] <- PEEPS$BUGSoutput$sims.array[, , paste("pi[", i,"]", sep="")]
}
AUC <- mat.or.vec(10000, 1)
for(i in 1:10000){
  AUC[i] <- auc(roc(loss_bin, pi[,i]))+0
}

plot.roc(loss_bin, rowMeans(pi))

index <- mat.or.vec(length(loss_bin), 1)
for(i in 1:length(loss_bin)){
  index[i] <- mean(PEEPS$BUGSoutput$sims.array[, ,paste("index[", i, "]", sep="")])
}

#index <- round(index)
index <- loss

sandy_cond <- sandy[index>0]
SLR_cond <- SLR[index>0]
evergreenProp_cond <- evergreenProp[index>0]
evergreenProp_cond[is.nan(evergreenProp_cond)] <- 0
long_cond <- long[index>0]
lat_cond <- lat[index>0]
lat_dd_cond <- lat_dd[index>0]
slope_cond <- slope[index>0]
area_cond <- area[index>0]
cover_cond <- cover[index>0]
treecover_cond <- treecover[index>0]
loss_cond <- loss[index>0]
year_cond <- year[index>0]
min_year_cond <- min_year[index>0]

###

# Pr of dieback given at least some
#BUGS model
PEEPS <- function(){
  sd_int ~ dnorm(0, 0.04)
  #sd_int ~ dnorm(0, 0.25)
  sdB ~ dunif(0, 100)
  #sdB ~ dunif(0, 10)
  tauB <- 1/(sdB*sdB)
  mu ~ dnorm(0, 0.04)
  #mu ~ dnorm(0, 0.25)
  for(e in 1:10){
    B[e] ~ dt(0, tauB, 4)
  }
  
  for(i in 1:length(loss_cond)){ 
    log(sd[i]) <- sd_int + B[8]*slope_cond[i] + B[9]*area_cond[i]
    tau[i] <- 1/(sd[i]*sd[i])
    int[i] ~ dnorm(0, tau[i])
    logit(p[i]) <- mu + int[i] + B[1]*slope_cond[i] + B[3]*evergreenProp_cond[i] + B[4]*SLR_cond[i]
    + B[6]*sandy_cond[i] + B[2]*area_cond[i] + B[7]*area_cond[i]*area_cond[i] + B[5]*slope_cond[i]*SLR_cond[i] + B[10]*evergreenProp_cond[i]*lat_cond[i]
    #logit(p[i]) <- mu + int[i] + B[1]*slope_cond[i] + B[3]*evergreenProp_cond[i] + B[4]*SLR_cond[i]
    #+ B[6]*sandy_cond[i] + B[2]*area_cond[i] + B[7]*area_cond[i]*slope_cond[i] + B[5]*slope_cond[i]*SLR_cond[i] + B[10]*evergreenProp_cond[i]*slope_cond[i] + B[11]*sandy_cond[i]*slope_cond[i]
    loss_cond[i] ~ dbin(p[i], cover_cond[i]); T(1,)
    }
}

if (is.R()){
  filename <- file.path(tempdir(), "PEEPS.bug")}
write.model(PEEPS, filename)
inits <- list(list(B=c(1, -3.5, -2, 1.5, 0.5, -0.25, 1, -1, 0.1, -2.5), sd_int=0.01, mu=-2))
data <- list("loss_cond", "cover_cond", "evergreenProp_cond", "area_cond", "slope_cond", "lat_cond", "SLR_cond", "sandy_cond")
parameters <- c("B", "mu", "sd_int", "int", "sdB")
PEEPS <- jags(data=data, inits=inits, parameters.to.save=parameters, filename,
              n.chains=1, n.burnin=20000, n.iter=60000, n.thin=1, DIC=TRUE)

# create a vector of the mean values of the log-normal variation
RE <- mat.or.vec(length(loss_cond), length(PEEPS$BUGSoutput$sims.array[, , "B[1]"]))
for(i in 1:length(loss_cond)){
  RE[i, ] <- PEEPS$BUGSoutput$sims.array[, ,paste("int[", i, "]", sep="")]
}

hist(rowMeans(RE))

quartz.options(width=3.54, height=6)
layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
par(mar=c(3, 3.5, 1, 1))
plot(lat_cond, rowMeans(RE), pch=16, col=rgb(0, 0, 0, 0.3), bty="n", xlab=" ", ylab=" ", las=1, cex.axis=0.7, cex=0.8, xaxt="n")
mtext(side=1, line=1.5, "Latitude (normalized)", cex=0.75)
mtext(side=2, line=2, "Residuals", cex=0.75)
axis(side=1, at=c(-0.75, -0.5, -0.25, 0, .25, 0.5, 0.75, 1), cex.axis=0.65, mgp=c(3, 0.5, 0))
text(-0.9, 4.5, "A", cex=0.85, font=2, xpd=TRUE)

###PLOT FOR LOOKING AT THE SHAPE OF THE AREA EFFECT###
mu <- PEEPS$BUGSoutput$sims.array[, , "mu"]
B2 <- PEEPS$BUGSoutput$sims.array[, , "B[2]"]
B7 <- PEEPS$BUGSoutput$sims.array[, , "B[7]"]
B8 <- PEEPS$BUGSoutput$sims.array[, , "B[8]"]
B9 <- PEEPS$BUGSoutput$sims.array[, , "B[9]"]
B11 <- PEEPS$BUGSoutput$sims.array[, , "B[11]"]
#quartz.options(width=3.54, height=3)
par(mar=c(2.5, 3.5, 1, 1))
area_pred <- seq(min(area_cond), max(area_cond), by=0.01)
p_trans <- mat.or.vec(10000, length(area_pred))
p_area <- mat.or.vec(10000, length(area_pred))
for(i in 1:10000){
  p_trans[i, ] <- mu[i] + B2[i]*area_pred + B7[i]*area_pred*area_pred
  #+ B7[i]*area_pred*area_pred
  #+ B7[i]*area_pred*area_pred + B8[i]*area_pred*area_pred*area_pred
  #+ B3[i]*area_pred*area_pred
  #+ B7[i]*area_pred*area_pred 
  #+ B8[i]*area_pred*area_pred*area_pred
  p_area[i, ] <- exp(p_trans[i, ])/(1 + exp(p_trans[i, ]))
}

upper <- mat.or.vec(length(area_pred), 1)
lower <- mat.or.vec(length(area_pred), 1)
for(i in 1:length(area_pred)){
  upper[i] <- quantile(p_area[,i], c(0.975))
  lower[i] <- quantile(p_area[,i], c(0.025))
}

plot(area_pred, colMeans(p_area, na.rm=TRUE), type="l", ylim=c(0, 1), lwd=1.25, bty="n", xlab=" ", ylab=" ", cex.axis=0.75, xaxt="n", las=1)
lines(area_pred, upper, lwd=0.75)
lines(area_pred, lower, lwd=0.75)
#points(area_cond, loss_cond/cover_cond, pch=16, cex=0.75, col=rgb(0, 0, 0, 0.2))
points(area_cond, loss_cond/cover_cond, pch=16, cex=0.75, col=rgb(0, 0, 0, 0.2))
mtext(side=1, line=1.35, "Log-scaled area", cex=0.75)
mtext(side=2, line=2.2, "Proportion of patch with forest loss", cex=0.75)
axis(side=1, mgp=c(3, 0.5, 0), cex.axis=0.75)
text(-1.3, 1.1, "B", cex=0.85, font=2, xpd=TRUE)

###MODEL FOR PREDICTIONS BY LATITUDE BAND; RUN WITH AND WITHOUT EFFECTS ON VARIANCE AND COVARIATES

#FULL
#BUGS model
PEEPS <- function(){
  sd_int ~ dnorm(0, 0.04)
  mu ~ dnorm(0, 0.04)
  Bsd ~ dunif(0, 10)
  Btau <- 1/(Bsd*Bsd)
  for(e in 1:11){
    B[e] ~ dt(0, Btau, 4)
  }
  
  for(i in 1:length(loss_cond)){ 
    log(sd[i]) <- sd_int + B[9]*slope_cond[i] + B[11]*area_cond[i]
    #+ B[9]*area_cond[i] + B[11]*area_cond[i]*area_cond[i] + B[8]*SLR_cond[i] 
    tau[i] <- 1/(sd[i]*sd[i])
    int[i] ~ dt(0, tau[i], 4)
    logit(p[i]) <- mu + int[i] + B[1]*slope_cond[i] + B[3]*evergreenProp_cond[i] + B[4]*SLR_cond[i]
    + B[6]*sandy_cond[i] + B[2]*area_cond[i] + B[5]*SLR_cond[i]*slope_cond[i] 
    + B[10]*evergreenProp_cond[i]*lat_cond[i] + B[7]*slope_cond[i]*area_cond[i]
    loss_cond[i] ~ dbin(p[i], cover_cond[i]); T(1,)
  }
}

if (is.R()){
  filename <- file.path(tempdir(), "PEEPS.bug")}
write.model(PEEPS, filename)
inits <- list(list(B=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), sd_int=0.01))
data <- list("loss_cond", "cover_cond", "evergreenProp_cond", "area_cond", "slope_cond", "lat_cond", "SLR_cond", "sandy_cond")
parameters <- c("B", "mu", "sd_int")
PEEPS <- jags(data=data, inits=inits, parameters.to.save=parameters, filename,
              n.chains=1, n.burnin=10000, n.iter=20000, n.thin=1, DIC=TRUE)


#FULL WITH NORMAL VARIANCE INSTEAD OF T
#BUGS model
PEEPS <- function(){
  sd_int ~ dnorm(0, 0.04)
  mu ~ dnorm(0, 0.04)
  Bsd ~ dunif(0, 10)
  Btau <- 1/(Bsd*Bsd)
  for(e in 1:11){
    B[e] ~ dt(0, Btau, 4)
  }
  
  for(i in 1:length(loss_cond)){ 
    log(sd[i]) <- sd_int + B[9]*slope_cond[i] + B[11]*area_cond[i]
    #+ B[9]*area_cond[i] + B[11]*area_cond[i]*area_cond[i] + B[8]*SLR_cond[i] 
    tau[i] <- 1/(sd[i]*sd[i])
    int[i] ~ dnorm(0, tau[i])
    logit(p[i]) <- mu + int[i] + B[1]*slope_cond[i] + B[3]*evergreenProp_cond[i] + B[4]*SLR_cond[i]
    + B[6]*sandy_cond[i] + B[2]*area_cond[i] + B[5]*SLR_cond[i]*slope_cond[i] 
    + B[10]*evergreenProp_cond[i]*lat_cond[i] + B[7]*area_cond[i]*area_cond[i]
    loss_cond[i] ~ dbin(p[i], cover_cond[i]); T(1,)
  }
}

if (is.R()){
  filename <- file.path(tempdir(), "PEEPS.bug")}
write.model(PEEPS, filename)
inits <- list(list(B=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), sd_int=0.01))
data <- list("loss_cond", "cover_cond", "evergreenProp_cond", "area_cond", "slope_cond", "lat_cond", "SLR_cond", "sandy_cond")
parameters <- c("B", "mu", "sd_int")
PEEPS <- jags(data=data, inits=inits, parameters.to.save=parameters, filename,
              n.chains=1, n.burnin=20000, n.iter=120000, n.thin=1, DIC=TRUE)



#WITHOUT VARIANCE INFLATION
#BUGS model
PEEPS <- function(){
  sd ~ dunif(0, 10)
  tau <- 1/(sd*sd)
  mu ~ dnorm(0, 0.04)
  Bsd ~ dunif(0, 10)
  Btau <- 1/(Bsd*Bsd)
  for(e in 1:11){
    B[e] ~ dt(0, Btau, 4)
  }
  
  for(i in 1:length(loss_cond)){ 
    int[i] ~ dt(0, tau, 4)
    logit(p[i]) <- mu + int[i] + B[1]*slope_cond[i] + B[3]*evergreenProp_cond[i] + B[4]*SLR_cond[i]
    + B[6]*sandy_cond[i] + B[2]*area_cond[i] + B[7]*area_cond[i]*area_cond[i] + B[5]*SLR_cond[i]*slope_cond[i] 
    + B[10]*evergreenProp_cond[i]*lat_cond[i]
    loss_cond[i] ~ dbin(p[i], cover_cond[i]); T(1,)
  }
}

if (is.R()){
  filename <- file.path(tempdir(), "PEEPS.bug")}
write.model(PEEPS, filename)
inits <- list(list(B=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), sd_int=0.01))
data <- list("loss_cond", "cover_cond", "evergreenProp_cond", "area_cond", "slope_cond", "lat_cond", "SLR_cond", "sandy_cond")
parameters <- c("B", "mu", "sd")
PEEPS <- jags(data=data, inits=inits, parameters.to.save=parameters, filename,
              n.chains=1, n.burnin=20000, n.iter=120000, n.thin=1, DIC=TRUE)


#WITHOUT VARIANCE INFLATION BUT WITH NORMAL INSTEAD OF T
#BUGS model
PEEPS <- function(){
  sd ~ dunif(0, 10)
  tau <- 1/(sd*sd)
  mu ~ dnorm(0, 0.04)
  Bsd ~ dunif(0, 10)
  Btau <- 1/(Bsd*Bsd)
  for(e in 1:11){
    B[e] ~ dt(0, Btau, 4)
  }
  
  for(i in 1:length(loss_cond)){ 
    int[i] ~ dnorm(0, tau)
    logit(p[i]) <- mu + int[i] + B[1]*slope_cond[i] + B[3]*evergreenProp_cond[i] + B[4]*SLR_cond[i]
    + B[6]*sandy_cond[i] + B[2]*area_cond[i] + B[7]*area_cond[i]*area_cond[i] + B[5]*SLR_cond[i]*slope_cond[i] 
    + B[10]*evergreenProp_cond[i]*lat_cond[i]
    loss_cond[i] ~ dbin(p[i], cover_cond[i]); T(1,)
  }
}

if (is.R()){
  filename <- file.path(tempdir(), "PEEPS.bug")}
write.model(PEEPS, filename)
inits <- list(list(B=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), sd_int=0.01))
data <- list("loss_cond", "cover_cond", "evergreenProp_cond", "area_cond", "slope_cond", "lat_cond", "SLR_cond", "sandy_cond")
parameters <- c("B", "mu", "sd")
PEEPS <- jags(data=data, inits=inits, parameters.to.save=parameters, filename,
              n.chains=1, n.burnin=20000, n.iter=120000, n.thin=1, DIC=TRUE)


#VARIANCE INFLATION WITHOUT COVARIATES
#BUGS model
PEEPS <- function(){
  sd_int ~ dnorm(0, 0.04)
  mu ~ dnorm(0, 0.04)
  Bsd ~ dunif(0, 10)
  Btau <- 1/(Bsd*Bsd)
  for(e in 1:11){
    B[e] ~ dt(0, Btau, 4)
  }
  
  for(i in 1:length(loss_cond)){ 
    log(sd[i]) <- sd_int + B[9]*slope_cond[i] + B[11]*area_cond[i]
    tau[i] <- 1/(sd[i]*sd[i])
    int[i] ~ dt(0, tau[i], 4)
    logit(p[i]) <- mu + int[i]
    loss_cond[i] ~ dbin(p[i], cover_cond[i]); T(1,)
  }
}

if (is.R()){
  filename <- file.path(tempdir(), "PEEPS.bug")}
write.model(PEEPS, filename)
inits <- list(list(B=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), sd_int=0.01))
data <- list("loss_cond", "cover_cond", "evergreenProp_cond", "area_cond", "slope_cond", "lat_cond", "SLR_cond", "sandy_cond")
parameters <- c("B", "mu", "sd_int")
PEEPS <- jags(data=data, inits=inits, parameters.to.save=parameters, filename,
              n.chains=1, n.burnin=20000, n.iter=120000, n.thin=1, DIC=TRUE)


#WITHOUT VARIANCE INFLATION AND WITHOUT COVARIATES
#BUGS model
PEEPS <- function(){
  sd ~ dunif(0, 10)
  tau <- 1/(sd*sd)
  mu ~ dnorm(0, 0.04)
  Bsd ~ dunif(0, 10)
  Btau <- 1/(Bsd*Bsd)
  for(e in 1:10){
    B[e] ~ dt(0, Btau, 4)
  }
  
  for(i in 1:length(loss_cond)){ 
    int[i] ~ dnorm(0, tau)
    logit(p[i]) <- mu + int[i]
    loss_cond[i] ~ dbin(p[i], cover_cond[i]); T(1,)
  }
}

if (is.R()){
  filename <- file.path(tempdir(), "PEEPS.bug")}
write.model(PEEPS, filename)
inits <- list(list(B=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), sd_int=0.01))
data <- list("loss_cond", "cover_cond", "evergreenProp_cond", "area_cond", "slope_cond", "lat_cond", "SLR_cond", "sandy_cond")
parameters <- c("B", "mu", "sd")
PEEPS <- jags(data=data, inits=inits, parameters.to.save=parameters, filename,
              n.chains=1, n.burnin=20000, n.iter=120000, n.thin=1, DIC=TRUE)


