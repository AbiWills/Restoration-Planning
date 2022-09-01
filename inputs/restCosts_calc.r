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

##### Labour & Equipment Cost Calc (5 years) ######

# CONSTANT # START LOOP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cost <- combine_words(c("cost",scen), and="")
yrs <- combine_words(c("yrs",scen), and="")
reps <- combine_words(c("reps",scen), and="")
int <- combine_words(c("int",scen), and="")
eqp <- combine_words(c("eqp",scen), and="")
life <- combine_words(c("life",scen), and="")
styr <- combine_words(c("styr",scen), and="")

### ACTIVITY COST
dur
restCosts[m, yrs]
# Determine number of years activity is repeated within specified timeframe (dur = # years): 
# (costs of repeats within the year already factored into the annual activity cost in csv)
n <- ifelse(restCosts[m, cost]>0, ifelse(restCosts[m, styr]==0, ifelse(restCosts[m, yrs]>1,
if(restCosts[m, int] >= dur) {1} else{ifelse(restCosts[m, yrs]*restCosts[m, int] >= dur, floor(dur/restCosts[m, int]), restCosts[m, yrs]*restCosts[m, int])}, 1),
if((restCosts[m, styr]+1)+restCosts[m, int] >= dur) {1} else{floor((dur-(restCosts[m, styr]+1))/(restCosts[m, yrs]*restCosts[m, int]))
}), 0)
n      
# Calculate cost of activity for this number of years
restCosts[m, cost]
restCosts[m, cost] * n
# Accounting for time between repeats and inflation:
# fv(rate[r], num periods[n], present value[pv], payments/period[pmt], type = 0)
# type=0, payments occur at the end of each period; type=1, payments occur at the beginning of each period
if(n>0) {x <- seq(1, n, 1)} else{x <- 0}
v1 <- ifelse(n>1 & restCosts[m, cost]>0, sum(fv(infl, restCosts[m, styr] + restCosts[m, int]*(x-1), pv = -1 * restCosts[m, cost], pmt = 0, type = 0)), restCosts[m, cost] * n) 

# EQUIPMENT COST
dur
restCosts[m, life]
# Determine number of times equipment needs to be repurchased due to depreciation for duration of activity (number of years activity implemented):
n <- ifelse(restCosts[m, eqp]>0, ifelse(restCosts[m, life]+restCosts[m, styr]>=dur, 1, ifelse(restCosts[m, yrs]>1, if(restCosts[m, life] >= (restCosts[m, yrs]*restCosts[m, int]))
{1} else{ ifelse(restCosts[m, yrs]*restCosts[m, int] >= dur, dur/restCosts[m, life], restCosts[m, yrs]*restCosts[m, int]/restCosts[m, life])}, 1)), 0)
n
# Calculate cost of equipment for this number of repeats
restCosts[m, eqp]
restCosts[m, eqp] * n
# Accounting for time between repeats and inflation:
# fv(rate[r], num periods[n], present value[pv], payments/period[pmt], type = 0)
if(n>0) {x <- seq(1, n, 1)} else{x <- 0}
v2 <- ifelse(n>1 & restCosts[m, eqp]>0, sum(fv(infl, restCosts[m, styr]+restCosts[m, life]*(x-1), pv = -1 * restCosts[m, eqp], pmt = 0, type = 0)), restCosts[m, eqp] * n)

##### END LOOP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
