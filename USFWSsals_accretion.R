setwd("/users/chrisfield/Dropbox/USFWScontract/accretion_data/")
NJ_accretion <- read.csv(file = "set.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
summary(lm(NJ_accretion$acc ~ 1))