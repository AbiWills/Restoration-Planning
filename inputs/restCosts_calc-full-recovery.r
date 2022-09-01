################################################################################
############################ RESTORATION PLANNING ##############################
################################################################################

# This code was developed to inform spatially-explicit forest landscape restoration planning,
# first tested in the Udzungwa-Kilombero region in Tanzania. It is based on an approach that prioritizes
# cost-effective ecosystem recovery, accounting for:
# (1) current versus maximum potential biomass(biomass deficit),
# (2) most likely appropriate restoration methods,
# (3) implementation costs, and
# (4) likely biomass(and thus stored carbon) gains.
# Pessimistic, realistic, and optimistic scenarios are incorporated into all four stages and
# above-ground biomass (AGB) gains, implementation costs and cost-effectiveness are estimated
# over two investment timeframes:
# (a) five years, to represent a typical upper limit of donor investment; and
# (b) expected time to full AGB recovery.

################################################################################
######################## LABOUR & EQUIPMENT COSTS ##############################
################################################################################

##### Labour & Equipment Cost Calc (Full recovery) ######

# CONSTANT # START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cost <- combine_words(c("cost",scen), and="")
yrs <- combine_words(c("yrs",scen), and="")
reps <- combine_words(c("reps",scen), and="")
int <- combine_words(c("int",scen), and="")
eqp <- combine_words(c("eqp",scen), and="")
life <- combine_words(c("life",scen), and="")
styr <- combine_words(c("styr",scen), and="")

# Create vectors for running cost calculation code
durvec <- as.data.frame(getValues(dur))
names(durvec)[1] <- "years"
labvec <- rep(NA, length(durvec$years))     # Output vector for labour costs
eqpvec <- rep(NA, length(durvec$years))     # Output vector for equipment costs

### LABOUR COST
for (i in 1:length(durvec$years)){

  y <- durvec$years[i]

  if(is.na(y)){labvec[i] <- NA} else {

    # Calculate number of years activity is repeated to full recovery
         # If cost > 0... (else==0)   # If start year == 0 (else...)# If number of years method is repeated >1... (else==1)
    n <- ifelse(restCosts[m, cost]>0, ifelse(restCosts[m, styr]==0, ifelse(restCosts[m, yrs]>1,
         # If interval >= duration == 1, else (if num years repeated * interval between years >= duration == duration / interval, else == num years * interval
         if(restCosts[m, int] >= y) {1} else{ifelse(restCosts[m, yrs]*restCosts[m, int] >= y, floor(y/restCosts[m, int]), restCosts[m, yrs]*restCosts[m, int])}, 1),
         # If start year =! 0, (if start year + 1 >= duration == 1, else == (duration - (start year + 1)) / num years * interval)
         if((restCosts[m, styr]+1)+restCosts[m, int] >= y) {1} else{floor((y-(restCosts[m, styr]+1))/(restCosts[m, yrs]*restCosts[m, int]))
         }), 0)                      
    # Calculate cost of activity for this number of years, accounting for time between repeats and inflation 
    if(n>0) {x <- seq(1, n, 1)} else{x <- 0}
    # fv(rate[r], num periods[n], present value[pv], payments/period[pmt], type = 0 (i.e. payments occur at the end of each period; 1 = payments occur at the beginning of each period)    
    labvec[i] <- ifelse(n>1 & restCosts[m, cost]>0, sum(fv(infl, restCosts[m, styr] + restCosts[m, int]*(x-1), pv = -1 * restCosts[m, cost], pmt = 0, type = 0)), restCosts[m, cost] * n)   
  
  }
  
}

### EQUIPMENT COST
for (i in 1:length(durvec$years)){

  y <- durvec$years[i]

  if(is.na(y)){eqpvec[i] <- NA} else {

    # Calculate number of times equipment needs to be repurchased due to depreciation for duration of activity (number of years activity implemented)
    n <- ifelse(restCosts[m, eqp]>0, ifelse(restCosts[m, life]+restCosts[m, styr]>=y, 1, ifelse(restCosts[m, yrs]>1, if(restCosts[m, life] >= (restCosts[m, yrs]*restCosts[m, int]))
         {1} else{ ifelse(restCosts[m, yrs]*restCosts[m, int] >= y, y/restCosts[m, life], restCosts[m, yrs]*restCosts[m, int]/restCosts[m, life])}, 1)), 0)
    # Calculate cost of equipment for this number of repeats, accounting for time between repeats and inflation 
    if(n>0) {x <- seq(1, n, 1)} else{x <- 0}
    # fv(rate[r], num periods[n], present value[pv], payments/period[pmt], type = 0 (i.e. payments occur at the end of each period; 1 = payments occur at the beginning of each period)    
    eqpvec[i] <- ifelse(n>1 & restCosts[m, eqp]>0, sum(fv(infl, restCosts[m, styr]+restCosts[m, life]*(x-1), pv = -1 * restCosts[m, eqp], pmt = 0, type = 0)), restCosts[m, eqp] * n)   
  
  }
  
}

### TOTAL cost ~~~~~~~ 
# Apply values to raster surfaces
labras <- raster(dur)
labras <- setValues(labras,labvec) 
eqpras <- raster(dur)
eqpras <- setValues(eqpras,eqpvec) 
# Sum labour and equipment costs
tot <- labras + eqpras

##### END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~