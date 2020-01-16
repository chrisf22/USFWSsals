### this script runs individual-based (female-only) simulations for global or state-level populations to estimate extinction risk for saltmarsh sparrows by projecting tide height for Y years (80 years max)
### the simulation uses parallel computing to run Q iterations for quantifying uncertainty from parameter estimation
### it is also possible to partition environmental and demographic stochasticity by specifying a number other than 1 for E 
### loop E runs within each iteration for Q, giving the enviro/demo uncertainty for a single vector of values of the simulation's parameters 

# water level units are in ft above MSL to be consistent with NOAA tidal constituents
# all years are nonleap years, but this does not matter for population simulations because the season always starts on May 1 (leap years do matter for projecting tides heights using tidal constituents)

### CHECK BEFORE RUNNING ###
# consequential code for tide gate management is on L328; code for thin layer deposition is on L320-324
# check starting population sizes
# check the minimum population size before the Y loop is broken
# choose the correct SLR scenario
# to run as a single population model: 1) change both instances of updating 'new_lat' to be a single repeating value (site 8 = Long Island Sound)
#   2) activate 'sites <- 8:8', instead of 'site <- 1:7' 
#   3) activate starting population as scalar instead of vector
#   4) activate the line that stores each year's population size in the first row of 'popsize_matrix' 
#   5) activate the correct functions for specifying density dependence, including the correct 'daily_surv'
### END CHECK BEFORE RUNNING ###

# set working directory
setwd("/Users/chrisfield/Dropbox/PVA/MCMC/GCB")

start_time <- proc.time()
# specify the number of iterations for each loop
# E is demographic and environmental stochasticity (not necessary unless specifically partitioning uncertainty)
# currently specified to run once without management (E = 1) and once to simulate either tide gate manipulation or thin layer deposition (E = 2)
E <- 4
# Y is the number of years
#Y <-  80
Y <-  50
# Q is the number of iterations for estimating uncertainty from parameter estimation
#Q <-1000
Q <- 10
# number of high tides in each season with tide gate manipulation
num_saves <- c(0, 5, 10, 100)
# the proportion of individuals in each state that are behind tide gates
# when running as a single population model, make sure there is a value in position 8 as this is used for a global site, as in Field et al. 2016
behind_gate_bystate <- c( 0.078, 0.073, 0.024, 0.069, 0.030, 0.112, 0.027, 0.079)
# the proprotion of individuals in each state that are in marshes with thin layer deposition
prop_dep_bystate <- c(0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3)
# the depth of thin layer deposition for each state
thin_layer_bystate <- c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)
# latitudes of a major marsh complex in each state to use as a covariate for determining values for reproductive parameters
state_lats <- c(39.53554518, 40.59975147, 41.26211791, 41.48726854, 42.77556486, 43.07542386, 43.56329844)

# load astronomical tide heights and storm surge parameters for the New London tide station
tides12to90 <- read.csv(file = "high_tides_NL.csv", header=FALSE, sep=",", stringsAsFactors=FALSE, quote="")
tidedates12to90 <- read.csv(file = "high_dates_NL.csv", header=FALSE, sep=",", stringsAsFactors=FALSE, quote="")
surge_posteriors <- read.csv(file = "storm_surge_posteriors.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, quote="")
# load vital rates and SLR projections
VitalRates <- read.csv(file = "IBM_parameters.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, quote="")
SLR_Rahm_all <- read.csv(file = "SLR_Rahmstorf.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, quote="")
SLR_Kopp_scen85 <- read.csv(file = "SLR_Kopp_scen85.csv", header=FALSE, sep=",", stringsAsFactors=FALSE, quote="")
SLR_Kopp_scen26 <- read.csv(file = "SLR_Kopp_scen26.csv", header=FALSE, sep=",", stringsAsFactors=FALSE, quote="")
# load nest success probabilities
nest_succ <- read.csv(file = "nest_failure_MCMC.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, quote="")
# load values for adult survival
surv_CT <- read.csv(file = "surv_avgsite_MCMC.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, quote="")
# load renesting probability; varies by date and latitude, but no individual variation beyond binomial sampling variance
renest_prob_MCMC <- read.csv(file = "renest_prob_MCMC.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, quote="")

# function for Latin Hypercube Sampling for parameters drawn from posterior samples
lhs_samp <- function(x){
  y <- ecdf(x)
  z <- y(x)
  out <- sample(x[z>((q-1)*(1/Q))&z<(q*(1/Q))], 1)
  out_pos <- which(x==out)
  return(out_pos)
}

# function for Latin Hypercube Sampling for parameters drawn from distributions
lhs_dist <- function(mu, sd){
  simp_rand <- rnorm(10000, mu, sd)
  y <- ecdf(simp_rand)
  z <- y(simp_rand)
  out <- sample(simp_rand[z>((q-1)*(1/Q))&z<(q*(1/Q))], 1)
  #interval_cdf <- qnorm(c(((q-1)*(1/Q) + 0.00001), ((q*(1/Q))) - 0.00001), mu, sd)
  #out <- runif(1, interval_cdf[1], interval_cdf[2])
  return(out)
}

# load libraries, detect and register cores, and use the foreach command to do parallel computing for each iteration of the parameter uncertainty loop
library('parallel')
library('foreach')
library('doParallel')
cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl, cores=detectCores() - 2)

PVA <- foreach(q = 1:Q) %dopar% {
  # this for loop is for testing without using foreach
  #start_time <- proc.time()
  #for(q in 1:1){
  # create an empty length(site)-by-Y-by-E array to store results; length(site) = 1 for a single-population model
  popsize_matrix <- array(0, dim=c(7, Y + 1, E))
  #popsize_matrix <- array(0, dim=c(1, Y + 1, E))
  
  # pull parameter values from posteriors
  # draw values for renesting probability; varies by date and latitude, but no individual variation beyond binomial sampling variance
  # randomly select a row number, which will be used to draw a vector of parameter values from same step of the MCMC chain
  # using simple random sampling
  #renest_prob_row <- sample(1000, 1)
  # using hypercube sampling
  renest_prob_row <- lhs_samp(renest_prob_MCMC[, 1])
  renest_prob_int <- renest_prob_MCMC[renest_prob_row, 1]
  renest_prob_date <- renest_prob_MCMC[renest_prob_row, 3]
  renest_prob_lat <- renest_prob_MCMC[renest_prob_row, 2]
  renest_prob_sd <- 0
  
  # draw values nest success probabilities
  # randomly select a row number, which will be used to draw a vector of parameter values from the same step of the MCMC chain
  # using simple random sampling
  #nest_succ_row <- sample(1000, 1)
  # using hypercube sampling
  nest_succ_row <- lhs_samp(nest_succ[, 1])
  # intercept
  nest_succ_int <- nest_succ[nest_succ_row,1]
  # tide effect
  nest_succ_tide <- nest_succ[nest_succ_row,2]
  # variance term for nest-level random effect
  nest_succ_var <- nest_succ[nest_succ_row,3]
  
  # choose values from posteriors of biotic parameters from table (# days after success, # days after failure, reduction in survival for first year, sex ratio, nest building, # incubation days, # chick days, nest initiation date)
  # first column is the intercept, second is the latitudinal trend, third is the within-season date trend, fourth is individual variation, fifth is seasonal variation
  B_vital <- mat.or.vec(8, 5)
  for(i in 1:8){
    # using simple random sampling
    B_vital[i, 1] <- rnorm(1, VitalRates[i, 4], VitalRates[i, 5])
    B_vital[i, 2] <- rnorm(1, VitalRates[i, 6], VitalRates[i, 7])
    B_vital[i, 3] <- rnorm(1, VitalRates[i, 8], VitalRates[i, 9])
    B_vital[i, 4] <- rnorm(1, VitalRates[i, 2], VitalRates[i, 3])
    B_vital[i, 5] <- rnorm(1, VitalRates[i, 10], VitalRates[i, 11])
  }
  
  # choose values for clutch size probabilities for 3, 4, and 5 eggs; exp is used to ensure the appropriate link function when used to project
  # for random sampling
  #clutch_size_3 <- exp(rnorm(1, 1.5976767, 0.1520553))
  #clutch_size_4 <- exp(rnorm(1, 2.1171442, 0.1467794))
  #clutch_size_5 <- exp(rnorm(1, 0.4307773, 0.1781254))
  # using hypercube sampling
  clutch_size_3 <- exp(lhs_dist(1.5976767, 0.1520553))
  clutch_size_4 <- exp(lhs_dist(2.1171442, 0.1467794))
  clutch_size_5 <- exp(lhs_dist(0.4307773, 0.1781254))
  
  # draw values for adult survival
  # randomly select a row number, which will be used to pull parameter values from the same step of the MCMC chain
  # using simple random sampling
  #surv_bysite_row <- sample(1000, 1)
  # using hypercube sampling
  surv_bysite_row <- lhs_samp(surv_CT[ ,1])
  survival_sitevector <- surv_CT[surv_bysite_row, 1]
  survival_siteSD <- surv_CT[surv_bysite_row, 2]
  
  # get SLR from Vermeer and Rahmstorf 2009; subtract 10 because that is the value in 2013, the year before the first year of the simulation in the original script
  # data are given in cm, and so are converted to ft to be consistent with NOAA tidal constituent data
  # column 3 of SLR_Rahm_all is for A1F1; 6 is for B1
  #SLR_Rahm <- (SLR_Rahm_all[25:110 , 3] - 10)*0.0328084
  # when using Kopp data, start at the 15th row, so that the first value is for 2014, the first year of the original script; subtract the value in 2013
  # set as RCP 8.5, 6.0, 4.5, or 2.6 (make sure both objects are specified as the correct scenario)
  # using simple random sampling
  #SLR_sample <- sample(10000, 1)
  # using hypercube sampling
  SLR_sample <- lhs_samp(SLR_Kopp_scen85[, 101])
  SLR_Kopp <- as.numeric((SLR_Kopp_scen85[SLR_sample, 15:101] - SLR_Kopp_scen85[SLR_sample, 14])*0.0328084)
  
  # draw values for storm surge parameters
  # using simple random sampling
  #ssindex <- sample(1000, 1)
  # using hypercube sampling
  ssindex <- lhs_samp(surge_posteriors[, 1])
  # parameter for trend over the SALS breeding season
  beta_mu <- surge_posteriors[ssindex ,3]
  # variance parameter for trend over the SALS breeding season
  sd2 <- surge_posteriors[ssindex ,7]
  # intercept for baseline (Y = 0)
  S <- surge_posteriors[ssindex ,1]
  # year trend
  beta_year <- surge_posteriors[ssindex ,2]
  # variance parameter for annual trend
  sd3 <- surge_posteriors[ssindex ,8]
  # residual variance
  sd <- surge_posteriors[ ssindex,6]
  
  # get log(accretion) mean and standard devation from LIS studies; backtransform and convert from m to ft to be consistent with NOAA tidal constintuent data
  #accretion <- exp(rnorm(1, -5.7208, 0.1923))*3.28084
  accretion <- lhs_dist(4.3186, 0.6064)*0.00328084
  
  # loop for environmental and demographic stochasticity
  # currently specified to run once without management (E = 1) and once to simulate either tide gate manipulation or thin layer deposition (E = 2)
  for(e in 1:E){
    # create a matrix for tracking the number of suitable breeding windows in each year
    num_windows <- mat.or.vec(Y, 1)
    # when e is an even number, a certain proportion of nests behind tide gates will be saved during high tides (determined by prop_behind_gate)
    # or a certain proportion of nests will be subject to thin layer deposition (determined by prop_dep)
    #if(e %% 2 == 0){
    # alternativley, management scenarios are run when e > 1 
    if(e > 1){
      prop_behind_gate <- behind_gate_bystate
      prop_dep <- prop_dep_bystate
    } else{
      # when e is odd, the proportion of nests behind tide gates or subject to thin layer deposition is 0
      prop_behind_gate <- c(0, 0, 0, 0, 0, 0, 0, 0)
      prop_dep <- c(0, 0, 0, 0, 0, 0, 0, 0)
    }
    # create a vector of site indices
    # use just one site when simulating a single-population model (use 8 for Long Island Sound, since that sets it in the middle of the CT coastline when referenced by SHARP sites)
    #sites <- 8:8
    # there are seven states; in order: New Jersey (1), New York (2), Connecticut (3), Rhode Island (4), Massachusetts (5), New Hampshire (6), Maine (7)
    sites <- 1:7
    # starting population size
    # for a single population
    #popsize_bysite <- 1500
    # for a group of sites
    popsize_bysite <- c(20715, 5773, 1594, 900, 6512, 1085, 1622)/3
    # vector of starting population sizes by site for exporting; this vector will update annually, while "popsize_bysite" will remain as the starting population sizes
    # specify only one site for a single-population model
    popsize_bysite_export <- mat.or.vec(7, 1)
    # combine site indices and starting population sizes to get a vector of individuals indexed by site
    individs_bysite <- rep(sites, popsize_bysite)
    # get latitude for each individual; this vector will be all the same latitude for a single population model
    #new_lat <- rep(41.26211791, length(individs_bysite))
    new_lat <- state_lats[individs_bysite]
    # it is possible to allow annual variation in survival
    # survival_annual_var <- rnorm(1, 0, 0.311)
    survival_annual_var <- rnorm(1, 0, 0)
    # add annual variation in survival, if there is any, to mean survival
    survival_sitevector <- survival_sitevector + survival_annual_var
    # add spatial variation, "survival_siteSD", to mean survival for each individual
    survival_logit <- survival_sitevector + rnorm(length(individs_bysite), 0, survival_siteSD)
    # get backtransformed survival for each individual
    survival <- exp(survival_logit)/(1+exp(survival_logit))
    # get first and last egg dates; from Ruskin et al. 2015
    start_date <- rep(141, length(individs_bysite))
    end_date <- rep(203, length(individs_bysite))
    # calculate total population size over all sites
    popsize <- length(individs_bysite)

    # parameters and tide data are using a baseline of 2013, so they are indexed as (y+7) so that y = 1 is 2021; starting population size is for 2020 
    # year loop; tide projections can support a maximum of 80 years (Y)
    for(y in 1:Y){
      # max breeding season: May 1 through August 31, in days since Jan 1 (day 1) in a non-leap year
      julian <- seq(from=121, to=243)
      # convert julian to a matrix with number of rows = popsize and columns = number of days in the breeding season
      julian_matrix <- rep(julian, popsize)
      julian_matrix <- matrix(julian_matrix, ncol=length(julian), byrow=TRUE)
      
      # draw year-specific parameters from the parent distribution, which was specified in the Q loop
      surgeslope <- rnorm(1, beta_mu, sd2)
      # it's 35+y because slope = 0 is in 1979 and the Y loop originally started in 2014; indexed as (y+7) so that y = 1 is now 2021
      surgeint <- rnorm(1, S + beta_year*(35+(y+7)), sd3)
      
      # remove extra zeros at the end of tide and date csv table
      # we add 2 to (y+7) because the first column of the tide file is 2012 but script was originally written so that y = 1 is 2014 for the loop 
      tidessanszero <- tides12to90[tides12to90[,(y+7)+2]>0, (y+7)+2]
      datessanszero <- tidedates12to90[tidedates12to90[,(y+7)+2]>0, (y+7)+2]
      
      # get dates in units of days since Jan 1st (instead of May 1st)
      datessanszero  <- datessanszero + 121
      
      # iterations to simulate typical storm surge - z
      mu <- mat.or.vec(length(tidessanszero), 1)
      theta <- mat.or.vec(length(tidessanszero), 1)
      surgepredic <- mat.or.vec(length(tidessanszero), 1)
      
      mu[1] <- surgeint +surgeslope*(1-1) 
      theta[1] <- mu[1]
      surgepredic[1] <- rnorm(1, theta[1], sd)
      for(z in 2:length(tidessanszero)){
        mu[z] <- surgeint + surgeslope*(z-1) 
        # 0.62 is the first-order autoregressive term
        theta[z] <- mu[z] + 0.62*(surgepredic[z-1] - mu[z-1])
        surgepredic[z] <- rnorm(1, theta[z], sd)
      }
      
      # add the tide components
      tides <- tidessanszero + surgepredic
      
      # add rise scenario; 0.0017666 ft is the global rate of SLR between 1979 and 2014 (which was removed before non-tidal fluctuation modeling); 35 is the number of years in this time period
      tides <- tides + SLR_Kopp[(y+7)] - accretion*(y+7) + 35*0.0017666
      threshold_index <- mat.or.vec(123, 1)
      tides_byday <- mat.or.vec(2, 123)
      for(i in min(julian):max(julian)){
        tides_byday[ , i-120] <- tides[(datessanszero>i)&(datessanszero<i+1)]
        # 3.45 is the threshold beyond which greater than 95% of nests fail (in ft)
        threshold_index[i-120] <- length(which(tides_byday[ , i-120] > 3.45))
      }
      
      # order tides from highest to lowest to investigate the influence of the timing of high tides on population dynamics
      tides_ordered <- tides[order(tides, decreasing = TRUE)]
      # create an index for a gate is manipulated during that tide, determined by whether it is higher or equal to the X highest tides (X specified by num_saves)
      save_index <- tides_byday > tides_ordered[num_saves[e]]
      # create an empty matrix that will index whether each tide in the season is subjected to manipulation, a "save" (1), or not (0)
      # with rows for the tides in each day and columns equal to the number of days in the season
      save_mat <- mat.or.vec(2, length(tides_byday[1,]))
      # if a tide should be a "save", according to save_index, save_mat is equal to 1
      save_mat[save_index] <- 1
      # create an empty array so that every individual can have an independent matrix for save or not (save_mat)
      save_index_byind <- array(0, c(2, length(tides_byday[1,]), length(new_lat)))
      # randomly determine which individuals will be subject to saves according to a specified probability
      behind_gate <- rbinom(length(new_lat), 1, prop_behind_gate[individs_bysite])
      # if indiviuals are determined to be behind a gate and subject to saves, according to behind_gate, assign save_mat to their position in the array
      # otherwise their matrix stays all zeros (no saves)
      save_index_byind[, , behind_gate==1] <- save_mat
      
      # create an index for which individuals are at sites with thin-layer deposition
      dep_state <- rbinom(length(new_lat), 1, prop_dep[individs_bysite])
      
      # calculate the number of windows without a reproduction-stopping tide (one that would cause greater than 95% failure) 
      threshold_index[threshold_index >1] <- 1
      lengths <- rle(threshold_index)$lengths
      values <- rle(threshold_index)$values
      window_lengths <- lengths[values == 0]
      num_windows[y] <- length(which(window_lengths > 22))
      
      # get days from first egg date until initiation
      initiation <- rgeom(length(new_lat), max(B_vital[8, 1], 0.01))
      
      # demographic parameters that potentially have both systematic variation by latitude and individual-level variation 
      # FY survival is a proportion of adult annual survival
      FY_surv_reduction <- B_vital[3, 1]
      # if FY survival reduction is less than 0, replace with a 0 (only a very small chance of this happening)
      FY_surv_reduction[FY_surv_reduction<0] <- 0
      # specify sex ratio
      sex_ratio <- B_vital[4, 1]
      # specify no. of nest building days
      nest_building <- colSums(rmultinom(length(new_lat), 1, c(.25, .25, .5))*c(2, 3, 4))
      # specify no. of incubation days
      incubation <- round(rnorm(length(new_lat), B_vital[6, 1] + B_vital[6, 2]*new_lat, B_vital[6, 4]))
      
      # clutch size is used to estimate egg laying days; this parameter only has sampling variation by individual 
      # each female has a constant clutch size within-season, but it can change between years
      # backtransform multinomial regression parameters
      clutch_size_p2 <- 1/(1+ (clutch_size_3 + clutch_size_4 + clutch_size_5))
      clutch_size_p3 <- clutch_size_3/(1+ (clutch_size_3 + clutch_size_4 + clutch_size_5))
      clutch_size_p4 <- clutch_size_4/(1+ (clutch_size_3 + clutch_size_4 + clutch_size_5))
      clutch_size_p5 <- clutch_size_5/(1+ (clutch_size_3 + clutch_size_4 + clutch_size_5))
      clutch_size_pmatrix <- rbind(clutch_size_p2, clutch_size_p3, clutch_size_p4, clutch_size_p5)
      laying <- colSums(rmultinom(length(new_lat), size=1, clutch_size_pmatrix[])*c(2, 3, 4, 5))
      
      # calculate nestling period
      chicks <- round(rnorm(length(new_lat), B_vital[7, 1] + B_vital[7, 2]*new_lat, B_vital[7, 4]))
      
      # days until renesting after sucess or failure; values are rounded and constrained to be <= 34, which is the maximum observed wait period after failure
      renest_days_success <- round(rnorm(length(new_lat), B_vital[1, 1] + B_vital[1, 2]*new_lat, B_vital[1, 4]))
      # if any renest days are greater than 34, keep redrawing
      while(length(which(renest_days_success>34))>0){
        renest_days_success[renest_days_success>34] <- round(rnorm(length(which(renest_days_success>35)), B_vital[1, 1] + B_vital[1, 2]*new_lat, B_vital[1, 4]))
      }
      # project renesting days after failure
      renest_days_fail <- rpois(length(new_lat), max(B_vital[2, 1] + B_vital[2, 2]*new_lat, 0))
      
      # turn latitiude into a matrix that has 1 row for each individual and its latitude value carried over a vector of length(julian)
      new_lat_matrix <- rep(new_lat, length(julian))
      new_lat_matrix <- matrix(new_lat_matrix, ncol=length(julian), byrow=FALSE)
      
      # probability of renesting has both systematic and stochastic variation by date, as well as individual-level variation added after the regression equation
      # renesting probability after a previous success
      renest_success <- (renest_prob_int + renest_prob_lat*new_lat_matrix + renest_prob_date*julian_matrix) + rnorm(popsize, 0, renest_prob_sd)
      renest_success <- 1 - exp(renest_success)/(1+exp(renest_success))
      # renesting probability after a previous failure
      renest_fail <- (renest_prob_int + renest_prob_lat*new_lat_matrix + renest_prob_date*julian_matrix) + rnorm(popsize, 0, renest_prob_sd)
      renest_fail <- 1 - exp(renest_fail)/(1+exp(renest_fail))
      
      # density dependence: only kicks in when population size is 3 times starting size
      # the first set of lines is for quantifying density dependence at the site-level 
      # the second set is for a single-population model (with site set = 8, as in Field et al. 2016)
      # for a group of sites
      allee_index <- mat.or.vec(7, 1)
      for(i in 1:7){
        allee_index[i] <- length(which(popsize_bysite_export[i] > (1.5*popsize_bysite[i])))
      }
      # specify vectors that are the length of the no. of individuals to put in nest success regression equation
      allee_index_individs <- allee_index[individs_bysite]
      # get an object for the how close the current population size is to 9 times the starting population size (as a proportion)
      allee_prop <- (popsize_bysite_export/(3*popsize_bysite))
      
      # for a single population
      # density dependence: only kicks in when population size is 3 times starting size
      #allee_index <- mat.or.vec(8, 1)
      #for(i in 8:8){
      #  allee_index[i] <- length(which(popsize > (3*popsize_bysite)))
      #}
      # specify vectors that are the length of the no. of individuals to put in nest success regression equation
      #allee_index_individs <- allee_index[8]
      # get an object for the how close the current population size is to 9 times the starting population size (as a proportion)
      #allee_prop <- (popsize/(9*popsize_bysite))
      
      # nest success probabilities, which were drawn from posterior estimates in the Q loop
      nest_succ_logit <- nest_succ_int + nest_succ_tide*tides_byday 
      nest_succ_logit <- replicate(length(individs_bysite), nest_succ_logit  + rnorm(1, 0, nest_succ_var), simplify="array")
      # the following 5 lines should be activated to increase nest success probability by the effect of thin layer deposition
      #xx <- 1:length(individs_bysite)
      #thin_layer_adj <- function(xx){
      #  nest_succ_logit[, , xx] - nest_succ_tide*thin_layer_bystate[individs_bysite][xx]*dep_state[individs_bysite][xx]
      #}
      #nest_succ_logit <- sapply(xx, FUN=thin_layer_adj, simplify="array")
      # backtransform nest success probabilities 
      succ_prob <- exp(nest_succ_logit)/(1+exp(nest_succ_logit))
      # adjust nest success probabilities so that survival probability = 1 for tides with saves
      succ_prob[save_index_byind==1] <- 1
      # get nest success probability by day instead of by tide
      succ_prob_day <- apply(succ_prob, MARGIN = c(2,3), FUN = prod)
      # density dependence kicks in at 3 times the starting populations size and increases gradually until its maximum at 9 times the population size
      # -8 is the intercept at the point that density dependence kicks in; 25 is the slope
      # (1/3) is specified so that the slope is zero when allee_prop is = 1/3; i.e (start of dens. dep.)/K
      # only one of the two versions of density dependence below should be active
      # daily nest survival with site-level density dependence
      daily_surv <- succ_prob_day - t(t(succ_prob_day)*allee_index_individs*(.5/(1+exp(-(-8 + 25*(allee_prop[individs_bysite]-(1/3)))))))
      # daily nest survival for a single population 
      #daily_surv <- succ_prob_day - succ_prob_day*allee_index_individs*(.5/(1+exp(-(-8 + 25*(allee_prop-(1/3))))))
      
      # create a window for the "vulnerable period"; each individual has a unique value, which does not vary over the breeding season
      window <- laying + incubation + chicks
      
      # in the loop below, days are defined as the diurnal period, so start date begins on the morning of the day that is being indexed (Jan 1 is day 1)
      # set back start date by nest building and waiting days (because start date is actually first egg date) and then add initiation time
      start_date <- start_date + initiation - nest_building - renest_days_fail
      # end date is the last day a female will initiate the nesting process
      end_date <- end_date - nest_building
      # if start date is less than the minimum or more than the maximum in julian, change to min or max (very small chance of happening)
      start_date[start_date < 121] <- 121
      start_date[start_date > 243] <- 243
      end_date[end_date > 243] <- 243
      
      # create a vector to fill with the number of successful nests for each individual
      successes <- mat.or.vec(popsize, 1)
      # create a vector to fill with the number of fledged young for each individual
      fledged <- mat.or.vec(popsize, 1)
      # create a vector to fill with the first first egg date for each individual
      first_egg <- mat.or.vec(popsize, 1)
      # create a vector to fill with the last first egg date for each individual
      first_egg_end <- mat.or.vec(popsize, 1)
      # for each individual, z
      for(z in 1:popsize){
        # "end_vector" indexes (1 or 0) the last day an individual can start nest building, which can vary by individual
        # all nests end on max(julian)
        end_vector <- rep(1, max(julian_matrix))
        end_vector[end_date[z] + 1:length(end_vector)] <- 0
        first_egg[z] <- which(start_date[z]==julian_matrix[z,])
        first_egg_end[z] <- which(end_date[z]==julian_matrix[z,])
        # t and f below will index the number of successful days up until the beginning of day i
        # start a scalar that will determine whether a bird is in the "invulnerable period" or has at least one egg
        t <- 0
        # start a scalar that keeps track of the number of days in a row without nest failure
        f <- 0
        # start a scalar that kicks out of the loop if a nest fails and the renesting draw is zero OR a nest fledges and the renesting draw is zero
        terminate <- 0
        # start a scalar that tracks how many successful nests there have been
        succ_nests <- 0
        # start a scalar that tracks number of young fledged
        fledges <- 0
        # only loop through days in the breeding season between the randomly drawn start and end dates
        for(i in first_egg[z]:length(julian_matrix[1,])){
          if(terminate == 0){
            clutch <- laying[z]
            # if there has been a successful nest, this loop will specify how long the invulnerable period is
            if(succ_nests > 0){
              # enter this loop once the invulnerable period is over (t > than some value)
              if(t >= (nest_building[z] + renest_days_success[z])){
                # did the nest survive day i?
                alive <- rbinom(1, clutch, daily_surv[i, z])
                clutch <- alive
                # if the nest survived day i, add 1 to f
                if(clutch > 0){
                  f <- f + 1
                }
                # if the nest failed and the renest draw came up 1 (try again), reset the t and f scalars
                if(clutch == 0&(rbinom(1, 1, renest_fail[z,i])*(end_vector[i] + renest_days_fail[z]))==1){
                  t <- 0
                  f <- 0
                }
                # if the nest failed and the renest draw came up 0 (give up for the season), terminate scalar is 1
                if(clutch == 0&(rbinom(1, 1, renest_fail[z,i])*(end_vector[i] + renest_days_fail[z]))==0){
                  terminate <- 1
                }
                # if the nest has succeeded a certain number of days (specified by window)
                # terminate depends on a draw from the successful renesting rates, one is added to succ_nests, and t is reset
                # determining the number of fledged young takes place at the end of day i, before the next iteration, which will 					
                # be the first day of the wait period if an individual renests
                if(f==(window[z]-1)){
                  fledges <- fledges + clutch
                  succ_nests <- succ_nests + 1
                  t<-0
                  f<-0
                  terminate <- rbinom(1, 1, renest_success[z,i])*(end_vector[i] + renest_days_success[z])
                }
              }
              if(t < (nest_building[z] + renest_days_success[z])){
                t <- t + 1
              }
            }
            # if there has not been a successful nest, this loop will specify how long the invulnerable period is
            if(succ_nests == 0){
              # enter this loop once the invulnerable period is over (t > than some value)
              if(t >= (nest_building[z] + renest_days_fail[z])){
                # did the nest survive day i?
                alive <- rbinom(1, clutch, daily_surv[i, z])
                clutch <- alive
                # if the nest survived day i, add 1 to f
                if(clutch > 0){
                  f <- f + 1
                }
                # if the nest failed and the renest draw came up 1 (try again), reset the t and f scalars
                if(clutch == 0&(rbinom(1, 1, renest_fail[z,i])*(end_vector[i] + renest_days_fail[z]))==1){
                  t<-0
                  f<-0
                }
                # if the nest failed and the renest draw came up 0 (give up for the season), terminate scalar is one
                if(clutch == 0&(rbinom(1, 1, renest_fail[z,i])*(end_vector[i] + renest_days_fail[z]))==0){
                  terminate <- 1
                }
                # if the nest has succeeded a certain number of days (specified by window)
                # terminate depends on a draw from the successful renesting rates, one is added to succ_nests, and t is reset
                # determining the number of fledged young takes place at the end of day i, before the next iteration, which will 					
                # be the first day of the wait period if an individual renests
                if(f==(window[z]-1)){
                  fledges <- fledges + clutch
                  succ_nests <- succ_nests + 1
                  t<-0
                  f<-0
                  terminate <- rbinom(1, 1, renest_success[z,i])*(end_vector[i] + renest_days_success[z])
                }
              }
              if(t < (nest_building[z] + renest_days_fail[z])){
                t <- t + 1
              }
            }
          }
        }
        successes[z] <- succ_nests
        fledged[z] <- fledges
      }
      
      # only a proportion of fledges will be females
      fledged_F <- rbinom(length(fledged), fledged, sex_ratio)
      
      # get the adults and chicks that survive to the next breeding season
      adult_survivors <- rbinom(popsize, 1, survival)
      chick_survivors <- rbinom(length(fledged_F), fledged_F, (survival*FY_surv_reduction))
      
      # get a vector of latitude just for surviving individuals, both adults and chicks
      survivors_site_adults <- individs_bysite[adult_survivors==1]
      survivors_site_chicks_condensed <- individs_bysite[chick_survivors>0]
      
      # remove zeros from the vector that indexes which fledges survive
      fledged_F <- fledged_F[chick_survivors>0]
      
      # pre-allocate a vector to iteratively add expanded fledge info
      survivors_site_chicks <- 0
      # for each group of fledges from the same individual, expand the vector of latitudes so there is one position in the vector for each individual
      # only if there were fledges; otherwise the zero will carry to the next command
      if(length(fledged_F) > 0){
        for(i in 1:length(fledged_F)){
          survivors_site_chicks <- c(survivors_site_chicks, rep(survivors_site_chicks_condensed[i], fledged_F[i]))
        }
      }
      # remove the pre-allocated zero
      survivors_site_chicks <- survivors_site_chicks[-1]
      
      # combine adults and first years
      individs_bysite <- c(survivors_site_adults, survivors_site_chicks)
      
      # specify and new value for latitude for each individual; this will be a constant value for a single population model
      #new_lat <- rep(41.26211791, length(individs_bysite))
      new_lat <- state_lats[individs_bysite]
      # it is possible to add in annual variation in survival for the next year
      # survival_annual_var <- rnorm(1, 0, .311)
      survival_annual_var <- rnorm(1, 0, 0)
      # get backtransformed survival for each individual for the next year
      survival_logit <- survival_sitevector + rnorm(length(new_lat), 0, survival_siteSD)
      survival <- exp(survival_logit)/(1+exp(survival_logit))
      # get new first egg date for each indiviudal
      start_date <- rep(141, length(new_lat))
      end_date <- rep(203, length(new_lat))
      
      # no. of females produced/female
      fecundity <- length(survivors_site_chicks)/popsize
      
      # calculate new cumulative population size
      popsize <- length(individs_bysite)
      
      # calculate population size by site
      for(i in 1:7){
        #calculate population of survivors size by site
        popsize_bysite_export[i] <- length(which(individs_bysite==sites[i]))
      }
      
      # choose whether to export fecundity or population size
      # use this to store population sizes
      popsize_matrix[, y, e] <- popsize_bysite_export
      #popsize_matrix[1, y, e] <- popsize
      # use this to store no. of females produced/female
      # popsize_matrix[, y, e] <- fecundity
      
      # if the population ever dips below 1 individual(s), break the loop
      if(popsize < 1){
        break
      }
    }
    # store the number of suitable breeding windows at the end of each vector of population sizes over time (in position Y+1)
    popsize_matrix[,Y+1, e] <- min(which(num_windows==0))
  }
  popsize_matrix[ , , ]
}
# stop the cluster
stopCluster(cl)

proc.time() - start_time

# save convention: "USFWSsals_", "Q"[no.iterations]"_", "gate_"|"TLD_", [specs], ".RData"
save.image("/Users/chrisfield/Dropbox/USFWScontract/workspaces/test.RData")
