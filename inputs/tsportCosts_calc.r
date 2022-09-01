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
############################ TRANSPORT COSTS ###################################
################################################################################

##### Transport Cost Calc (5 years) ######

# Based on cost per return trip, this code calculates the total tranport cost per method, 
# by factoring in the number of trips (determined by team size / number of stems),
# repeats, years, intervals between years, and equipment costs.

##### START LOOP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cost <- combine_words(c("cost",scen), and="")
yrs <- combine_words(c("yrs",scen), and="")
reps <- combine_words(c("reps",scen), and="")
int <- combine_words(c("int",scen), and="")
eqp <- combine_words(c("eqp",scen), and="")
life <- combine_words(c("life",scen), and="")
styr <- combine_words(c("styr",scen), and="")
team <- combine_words(c("team",scen), and="")
stems <- combine_words(c("stems",scen), and="")
haday <- combine_words(c("haday",scen), and="")

# Number of work days = area (ha) being restored (i.e. 50, 100, 1000ha under different scenarios) / ha restorable per day
days <- ha / restCosts[m, haday]

### TRANSPORT cost ~~~~~~~
# Determine number of trips needed as follows: 
# (a) Tree planting methods = (stems per ha x area (ha) being restored) / carrying capacity of transport; or 
# (b) Other methods = team size x work days needed to restore area (i.e. 50, 100, 1000ha under different scenarios) / maximum remote work days per trip.
trips <- ifelse(m=="enrich" | m=="enrichFSA" | m=="fsa" | m=="nurse" | m=="nurseenr", ceiling((restCosts[m, stems] * ha) / restCosts[tm, stems]), restCosts[m, team] * days/wdpt) 
trips
# Determine cost per trip as follows: 
# (a) Tree planting methods = cost of 1 return trip [r]; 
# (b) Other methods = cost of 1 return trip [r] + camping cost per person per day x (number of work days + rest days [1 for every 6 work days])
r <- if(m=="enrich" | m=="enrichFSA" | m=="fsa" | m=="nurse" | m=="nurseenr") { r } else { r + (restCosts["camp", cost] * (days + wdpt/wdptP)) }
r
# Cost per year = cost per trip [r] x # trips x # times activity is repeated per annum
r <- r * trips * restCosts[m, reps]
r
# Determine number of years activity is repeated within timeframe (dur = # years)
n <- ifelse(restCosts[m, cost]>0, ifelse(restCosts[m, styr]==0, ifelse(restCosts[m, yrs]>1,
if(restCosts[m, int] >= dur) {1} else{ifelse(restCosts[m, yrs]*restCosts[m, int] >= dur, floor(dur/restCosts[m, int]), floor(dur/(restCosts[m, yrs]*restCosts[m, int])))}, 1),
if((restCosts[m, styr]+1)+restCosts[m, int] >= dur) {1} else{floor((dur-(restCosts[m, styr]+1))/(restCosts[m, yrs]*restCosts[m, int]))
}), 0)
n
# Calculate cost of activity for this number of years
temp <- r * n
temp
# Now accounting for time interval (between years) and inflation:
# fv(rate[r], num periods[n], present value[pv], payments/period[pmt], type = 0)
# type=0, payments occur at the end of each period; type=1, payments occur at the beginning of each period
if(n>0) {num <- seq(1, n, 1)} else{num <- 0}
num
fvFun <- function(x) {ifelse(n>1, sum(fv(infl, restCosts[m, styr] + restCosts[m, int]*(num-1), pv = -1 * x, pmt = 0, type = 0)), x * n)}
r1 <- calc(r, fun=fvFun)
r1[is.na(r1[])] <- 0

### GOVERNMENT REPRESENTATIVE cost ~~~~~~~ 
# Assumes Government Representative visits site for tree planting activities only and only for first trip
if(m=="enrich" | m=="fsa" | m=="nurse") {  
if(exists("gov")){gov}else{gov <- restCosts["gov", cost]}
gov
# Number of trips = 1 since assume Government official only travels to site once:
trips <- 1                    
# Cost per year (cost per trip [r] x # times activity is repeated per annum) - # trips not considered here as assume Govt Rep makes just one trip per repeat
gov <- gov * restCosts[m, reps]
gov
# Determine number of years activity is repeated within timeframe (dur = # years)
n <- ifelse(restCosts[m, cost]>0, ifelse(restCosts[m, styr]==0, ifelse(restCosts[m, yrs]>1,
if(restCosts[m, int] >= dur) {1} else{ifelse(restCosts[m, yrs]*restCosts[m, int] >= dur, floor(dur/restCosts[m, int]), restCosts[m, yrs]*restCosts[m, int])}, 1),
if((restCosts[m, styr]+1)+restCosts[m, int] >= dur) {1} else{floor((dur-(restCosts[m, styr]+1))/(restCosts[m, yrs]*restCosts[m, int]))
}), 0)
n             
# Calculate cost of activity for this number of years
gov <- gov * n
gov
gov <- ext * gov # Creating raster where all relevant pixels==gov (needed when gov is an integer value not a raster)
gov
# Now accounting for time interval (between years) and inflation:
# fv(rate[r], num periods[n], present value[pv], payments/period[pmt], type = 0)
# type=0, payments occur at the end of each period; type=1, payments occur at the beginning of each period
if(n>0) {num <- seq(1, n, 1)} else{num <- 0}
num
fvFun <- function(x) {ifelse(n>1, sum(fv(infl, restCosts[m, styr] + restCosts[m, int]*(num-1), pv = -1 * x, pmt = 0, type = 0)), x * n)}
gov <- calc(gov, fun=fvFun)  
gov[is.na(gov[])] <- 0            # Govt cost for activity for duration of investment
} else {gov <- 0}

### EQUIPMENT cost ~~~~~~~
# Determine number of times equipment needs to be repurchased due to depreciation for duration of activity (number of years activity implemented):
n <- ifelse(restCosts[m, eqp]>0, ifelse(restCosts[m, life]+restCosts[m, styr]>=dur, 1, ifelse(restCosts[m, yrs]>1, if(restCosts[m, life] >= (restCosts[m, yrs]*restCosts[m, int]))
{1} else{ ifelse(restCosts[m, yrs]*restCosts[m, int] >= dur, dur/restCosts[m, life], restCosts[m, yrs]*restCosts[m, int]/restCosts[m, life])}, 1)), 0)
n
# Calculate cost of equipment for this number of repeats
restCosts[tm, eqp]
restCosts[tm, eqp] * n
# Accounting for time between repeats and inflation:
# fv(rate[r], num periods[n], present value[pv], payments/period[pmt], type = 0)
if(n>0) {num <- seq(1, n, 1)} else{num <- 0}
num
v1 <- ifelse(n>1 & restCosts[tm, eqp]>0, sum(fv(infl, restCosts[m, styr]+restCosts[tm, life]*(num-1), pv = -1 * restCosts[tm, eqp], pmt = 0, type = 0)), restCosts[tm, eqp] * n)

### TOTAL cost ~~~~~~~ (incl. transport, government rep and equipment)
r2 <- r1 + gov + v1
rm(gov)

##### END LOOP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~