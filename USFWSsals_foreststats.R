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
loss <- lossStats$sum
#[coverStats$sum>0]
# get loss in hectares
loss_ha <- loss*0.003
# total number of pixels that experience gain in each patch
gain <- gainStats$sum
#[coverStats$sum>0]
# total forest cover in 2000; sum of the proportion of each pixel that was forested in 2000
treecover <- treeCoverStats$sum
#[coverStats$sum>0]
# binary sum of forest cover in 2000
cover <- coverStats$sum
# get binary sum of forest cover in 2000 in hectares
cover_ha <- cover*0.003
#[coverStats$sum>0]
# proportion of each patch that experiences forest loss between 2000-2018
observed <- lossStats$sum/coverStats$sum
#[coverStats$sum>0]/[coverStats$sum>0]
long <- lossStats$X
#[coverStats$sum>0]
lat <- lossStats$Y
#[coverStats$sum>0]
# for both lat and long, 5305 and 5306 are zero; temp replace zeros with mean longitude 
long[long==0] <- mean(long[long!=0])
lat[lat==0] <- mean(lat[lat!=0])
# log transform area to handle extremely large patches
# use either area of total patch, including tidal marsh
# or the cover vector (from Hansen et al. 2018), which is the number of forest pixels greater than a cover threshold (30%)
#area <- log(lossStats$area_ha[coverStats$sum>0])
area <- log(cover)
#area <- cover
slope <- slopeStats$mean
#[coverStats$sum>0]
# 117 values for slope are NAs; temp replaces zeros with mean slope
slope[is.na(slope)] <- mean(slope, na.rm=TRUE)
# create binomial variable for loss or not at the patch level
loss_bin <- loss
loss_bin[loss_bin > 0] <- 1

# bind forest cover, loss, and slope states to object that will be used to merge with SHARP patch layer attribute table
SHARP_patches_att <- cbind(lossStats$PatchID, loss_ha, cover_ha, observed, slope)
colnames(SHARP_patches_att)[1] <- "PatchID"

SHARP_patches_4plot <- cbind(lossStats$PatchID, loss_ha, cover_ha, observed, slope, lossStats$SALS_abund)
colnames(SHARP_patches_4plot)[1] <- "PatchID"

write.csv(SHARP_patches_att, "/users/chrisfield/Desktop/test.csv")

