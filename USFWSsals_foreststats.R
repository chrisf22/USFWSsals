#for 30 m buffer
#setwd("/users/chrisfield/Documents/folders/SESYNC/GEEfiles/GEE_exports_20/")
# for 100 m buffer
setwd("/users/chrisfield/Documents/folders/SESYNC/GEEfiles/GEE_exports_20_100m/")

# load files that were exported from Google Earth Engine
lossStats <- read.csv(file = "lossStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
gainStats <- read.csv(file = "gainStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
coverStats <- read.csv(file = "coverStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
slopeStats <- read.csv(file = "slopeStatsDEM.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
treeCoverStats <- read.csv(file = "treeCoverStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
totalCoverStats <- read.csv(file = "totalCoverStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
naturalStats <- read.csv(file = "naturalStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
tide_restsStats <- read.csv(file = "tide_restsStats.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
tide_restsStats <- tide_restsStats[match(lossStats$PatchID, tide_restsStats$PatchID), ]

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
loss_ha <- loss*0.09
# total number of pixels that experience gain in each patch
gain <- gainStats$sum
#[coverStats$sum>0]
# total forest cover in 2000; sum of the proportion of each pixel that was forested in 2000
treecover <- treeCoverStats$sum
#[coverStats$sum>0]
# binary sum of forest cover in 2000
cover <- coverStats$sum
# get binary sum of forest cover in 2000 in hectares
cover_ha <- cover*0.09
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
# 117 values for slope are NAs; temporarily replace with mean slope
slope[is.na(slope)] <- mean(slope, na.rm=TRUE)
# create binomial variable for loss or not at the patch level
loss_bin <- loss
loss_bin[loss_bin > 0] <- 1
# the area of restricted tidal marsh (in terms of number of pixels); multiplied by 0.09 to get in ha
tide_rests <- tide_restsStats$sum*0.09
# create a numeric vector for SALS abundace
SALS <- as.numeric(lossStats$SALS_abund)
# replace missing value notation from SHARP patch layer with NAs
SALS[SALS>=9999] <- NA

# bind forest cover, loss, and slope to object that will be used to merge with SHARP patch layer attribute table
SHARP_patches_att <- cbind(lossStats$PatchID, loss_ha, cover_ha, observed, slope)
colnames(SHARP_patches_att)[1] <- "PatchID"

# create a vector for the proportion of each SHARP patch that is restricted
# because of small differences between the raster and vector boundaries, some values can be more than one
# change values greater than one to one
prop_rests <- tide_rests/lossStats$area_ha
prop_rests[prop_rests > 1] <- 1
# get the proportion of the saltmarsh sparrow population behind tidal restrictions as:
# the patch level abundance x the proportion of the patch that is restricted
# to avoid NAs, patches with missing values for saltmarsh sparrow abundance are zeros
SALS_noNA <- SALS
SALS_noNA[is.na(SALS_noNA)] <- 0
SALS_rests <- SALS_noNA*prop_rests
# bind area of tidal restriction and SALS abundance to inform population model
SHARP_patches_rest <- cbind(lossStats$PatchID, tide_rests, SALS_noNA, lossStats$area_ha, prop_rests, SALS_rests)
# turn matrix into a data frame
SHARP_patches_rest <- as.data.frame(SHARP_patches_rest, stringsAsFactors = FALSE)
# bind data frame with column for state
SHARP_patches_rest <- cbind(SHARP_patches_rest, lossStats$STATE)
colnames(SHARP_patches_rest)[1] <- "PatchID"
colnames(SHARP_patches_rest)[7] <- "States"

#get the area of restricted marsh by state
state_index <- unique(SHARP_patches_rest$States)
tide_rests_bystate <- mat.or.vec(length(state_index), 1)
SALS_rests_bystate <- mat.or.vec(length(state_index), 1)
SALS_bystate <- mat.or.vec(length(state_index), 1)
for(i in 1:length(state_index)){
  SALS_bystate[i] <- sum(SHARP_patches_rest$SALS_noNA[SHARP_patches_rest$States==state_index[i]])
  SALS_rests_bystate[i] <- sum(SHARP_patches_rest$SALS_rests[SHARP_patches_rest$States==state_index[i]])
  tide_rests_bystate[i] <- sum(SHARP_patches_rest$tide_rests[SHARP_patches_rest$States==state_index[i]])
}
tide_rests_bystate_table <- cbind(state_index, as.data.frame(cbind(SALS_bystate, SALS_rests_bystate, SALS_rests_bystate/SALS_bystate, tide_rests_bystate)))
colnames(tide_rests_bystate_table)[4] <- "Prop_SALS_rests"

# some values for observed are NaN since patches with no forest in 2000 were not removed
# replace NaN with NAs
observed[is.nan(observed)] <- NA
# create a matrix with SHARP patch ID, forest loss and cover, slope, and SALS abundance
SHARP_patches_4plot <- cbind(lossStats$PatchID, loss_ha, cover_ha, observed, slope, SALS, lat, long)
# turn matrix into a data frame
SHARP_patches_4plot <- as.data.frame(SHARP_patches_4plot, stringsAsFactors = FALSE)
# bind data frame with column for state
SHARP_patches_4plot <- cbind(SHARP_patches_4plot, lossStats$STATE)
# specify names for columns without
colnames(SHARP_patches_4plot)[1] <- "PatchID"
colnames(SHARP_patches_4plot)[9] <- "States"
# two strategies for getting the legends in 'plotly' in geographic rather than alphabetical order
# first created a sortable index for state; second treat state as a factor and specify the proper factor order
# create a character vector for state
state_num <- lossStats$STATE
# replace alpha codes with numbers to allow sorting
state_num[state_num=='VA'] <- 1
state_num[state_num=='MD'] <- 2
state_num[state_num=='DE'] <- 3
state_num[state_num=='NJ'] <- 4
state_num[state_num=='NY'] <- 5
state_num[state_num=='CT'] <- 6
state_num[state_num=='RI'] <- 7
state_num[state_num=='MA'] <- 8
state_num[state_num=='NH'] <- 9
state_num[state_num=='ME'] <- 10
# bind numeric state index to data frame
SHARP_patches_4plot <- cbind(SHARP_patches_4plot, as.numeric(state_num))
# specify column name for numeric state index
colnames(SHARP_patches_4plot)[10] <- "state_num"
# add area has a column
SHARP_patches_4plot <- cbind(SHARP_patches_4plot, lossStats$area_ha)
colnames(SHARP_patches_4plot)[11] <- "area_ha"
# order the data frame by the state numeric index, from south the north
SHARP_patches_4plot <- SHARP_patches_4plot[order(SHARP_patches_4plot$state_num), ]
# specify factors for alpha state index from south the north
SHARP_patches_4plot$States <- factor(SHARP_patches_4plot$States, levels = c("VA", "MD", "DE", "NJ", "NY", "CT", "RI", "MA", "NH", "ME"))

# export forest stats to be merged with SHARP patch layer .shp in QGIS
write.csv(SHARP_patches_att, "/users/chrisfield/Desktop/USFWSsals_foreststats.csv")

### find the minimum area of current marsh that will need to be managed to affect X% of the saltmarsh sparrow population

# create a vector of unique state names to cycle through
state_names <- levels(SHARP_patches_4plot$States)
num_marshes <- mat.or.vec(length(state_names), 2)
marsh_area <- mat.or.vec(length(state_names), 2)
# creating a table for the two management goals used in the pop. simulation (33% and 10% of the population)
sals_prop <- c(0.3333, 0.1)
for(e in 1:2){
for(i in 1:length(state_names)){
  # create a temporary file for state i
  temp <- SHARP_patches_4plot[SHARP_patches_4plot$States==state_names[i],]  
  # order temporary object by decreasing SALS abundance
  temp2 <- temp[order(temp$SALS, decreasing=TRUE), ]
  # calculate the total SALS population size for state i
  SALS_total <- sum(temp2$SALS, na.rm=TRUE)
  # find the cumulative sum of the SALS population size, from the object that is ordered highest to lowest SALS abundance
  sum_SALS <- cumsum(temp2$SALS)
  # find the cumulative sum of marsh area, as above
  sum_marsh <- cumsum(temp2$area_ha)
  # find the number of marshes it takes to meet the management goal e
  num_marshes[i, e] <- min(which(sum_SALS>=SALS_total*sals_prop[e]))
  # find the total area of the marshes identified above
  marsh_area[i, e] <- sum_marsh[num_marshes[i, e]]
}
}

pop_to_marsh_conv <- cbind(num_marshes, marsh_area)
colnames(pop_to_marsh_conv) <- c("Number of marshes (33% of pop.)", "Number of marshes (10% of pop.)", "Area of marsh (33% of pop.)", "Area of marsh (10% of pop.)")

write.csv(pop_to_marsh_conv, "/users/chrisfield/Desktop/USFWSsals_poptomarsh.csv")


###

library(plotly)

# updatemenus component
# create buttons that toggle on/off the points for each state; states are specified in alphabetical order by default in 'plotly'
updatemenus <- list(
  list(
    active = -1,
    type= 'buttons',
    x = -0.07,
    buttons = list(
      list(
        label = "VA",
        method = "update",
        args = list(list(visible = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)))),
      
      list(
        label = "MD",
        method = "update",
        args = list(list(visible = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)))), 
      
      list(
        label = "DE",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)))), 
      
      list(
        label = "NJ",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)))), 
      
      list(
        label = "NY",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)))), 
      
      list(
        label = "CT",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)))), 
      
      list(
        label = "RI",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)))), 
      
      list(
        label = "MA",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)))), 
      
      list(
        label = "NH",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)))), 
      
      list(
        label = "ME",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)))), 
      
      list(
        label = "All",
        method = "update",
        args = list(list(visible = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)))))
  )
)

# create 3 plots to be combined into one subplot; legend is turned off for all but 1; these lines primarily use piping to set plot parameters
p1 <- SHARP_patches_4plot%>%group_by(States)%>%plot_ly(x = ~SALS, y = ~observed, text = ~paste("SHARP patch ID = ", PatchID), 
                                                       color = ~States, type="scatter", colors = "Paired", legendgroup= ~States, 
                                                       showlegend=TRUE, mode="markers") %>%
  layout(title = " ",
         xaxis=list(title="Saltmarsh sparrow abundance", type = "log", range = c(-3.8, 4)),
         yaxis=list(title="Recent forest loss (proportion of patch)", type = "log"),
         updatemenus=updatemenus)

p2 <- SHARP_patches_4plot%>%group_by(States)%>%plot_ly(x = ~SALS, y = ~cover_ha, text = ~paste("SHARP patch ID = ", PatchID), 
                                                       color = ~States, type="scatter", colors = "Paired", legendgroup= ~States, 
                                                       showlegend=FALSE, mode="markers") %>%
  layout(title = " ",
         xaxis=list(title="Saltmarsh sparrow abundance", type = "log", range = c(-3.8, 4)),
         yaxis=list(title="Area of forest within 100m of marsh (ha)", type = "log"),
         updatemenus=updatemenus, xaxis = list(type = "log"), yaxis = list(type = "log"))

p3 <- SHARP_patches_4plot%>%group_by(States)%>%plot_ly(x = ~SALS, y = ~slope, text = ~paste("SHARP patch ID = ", PatchID), 
                                                       color = ~States, type="scatter", colors = "Paired", legendgroup= ~States, 
                                                       showlegend=FALSE, mode="markers") %>%
  layout(title = "Saltmarsh sparrow abundance vs. marsh migration potential",
         xaxis=list(title="Saltmarsh sparrow abundance", type = "log", range = c(-3.8, 4)),
         yaxis=list(title="Slope (degrees)", type = "log"),
         updatemenus=updatemenus, xaxis = list(type = "log"))

# combine plots 1:3 into a subplot; to export as html, plot in RStudio and choose Export > Save as webpage...
subplot(p1, p2, p3, titleY = TRUE, titleX = TRUE, margin = 0.05)


