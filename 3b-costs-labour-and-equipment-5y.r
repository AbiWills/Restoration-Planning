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

### INSTALL NECESSARY PACKAGES

necessary.packages<-c("raster", "sp", "rgdal")
already.installed<-necessary.packages%in%installed.packages()[, 'Package']    #asks if the necessary packages are already installed in the library?
if (length(necessary.packages[!already.installed])>=1) {    #if any are NOT installed, download them now.
  install.packages(necessary.packages[!already.installed],dep=T)   #are the dependencies really necessary (there are lots!)?
}
sapply(necessary.packages,function(p) {require(p,quietly=T,character.only=T)})

# SET WORKING DIRECTORY:
basename <- "C:/Users/wills/Documents/R/workspaces/UdzKilo_Restoration_Planning"
setwd(basename)

# SETUP INPUT AND OUTPUT STORAGE LOCATIONS IN WORKING DIRECTORY

# INPUTS
inDir <- paste(basename,"inputs",sep="/")
if (file.exists(inDir)){
  cat("\n check directory inputs")
} else {
  dir.create(file.path(inDir))
}

# OUTPUTS
outDir <- paste(basename,"outputs",sep="/")
if (file.exists(outDir)){
  cat("\n check directory outputs")
} else {
  dir.create(file.path(outDir))
}

# Restoration methods
outM<-paste(outDir,"methods",sep="/")
if (file.exists(outM)){
  cat("\n check directory methods")
} else {
  dir.create(file.path(outM))
}

# Methods grouped by approach (passive, ANR, planting)
outA<-paste(outM,"approach",sep="/")
if (file.exists(outA)){
  cat("\n check directory approach")
} else {
  dir.create(file.path(outA))
}

# Remoteness / travel distances to each landscape pixel
outR<-paste(outM,"remoteness",sep="/")
if (file.exists(outR)){
  cat("\n check directory remoteness")
} else {
  dir.create(file.path(outR))
}

# RESTORATION COSTS
outC<-paste(outDir,"costs",sep="/")
if (file.exists(outC)){
  cat("\n check directory costs")
} else {
  dir.create(file.path(outC))
}

# Labour and equipment costs (5 years)
outC5<-paste(outC,"5y",sep="/")
if (file.exists(outC5)){
  cat("\n check directory costs/5y")
} else {
  dir.create(file.path(outC5))
}

# Labour and equipment costs (Full recovery)
outCR<-paste(outC,"restored",sep="/")
if (file.exists(outCR)){
  cat("\n check directory costs/restored")
} else {
  dir.create(file.path(outCR))
}

# Transport costs (inputs, incl. cost surfaces for 1 return trip to each landscape pixel)
outT<-paste(outC,"transport",sep="/")
if (file.exists(outT)){
  cat("\n check directory transport")
} else {
  dir.create(file.path(outT))
}

# Transport costs (5 years)
outC5T<-paste(outC5,"transport",sep="/")
if (file.exists(outC5T)){
  cat("\n check directory 5y/transport")
} else {
  dir.create(file.path(outC5T))
}

# Transport costs (Full recovery)
outCRT<-paste(outCR,"transport",sep="/")
if (file.exists(outCRT)){
  cat("\n check directory restored/transport")
} else {
  dir.create(file.path(outCRT))
}

################################################################################
############################## RESTORATION COSTS ###############################                                <- RESTORATION COSTS
################################################################################

################################################################################
################## 5Y LABOUR AND EQUIPMENT COSTS CALCULATION ###################                                  <- 5Y LABOUR AND EQUIPMENT COSTS CALCULATION

# This code calculates the total labour and equipment costs incurred for each restoration
# pixel over a (adjustable) 5 year timeframe, based on:
# method (porters required? etc), location (road / off road transport? camping needeed?)
# number of treatments per year, number of years of intervention, etc.

### Set input work directory
setwd(inDir)

##### CALL IN RESTORATION COSTS TABLE / CSV:
### Incl. 3 columns (Pess, Real, Opt) each for: # Labour Cost (costP,R,O)
                                                # Num Repeats/Years (repsP,R,O)
                                                # Repeat interval (intP,R,O) - to do, for tree planting
                                                # Equipment cost (eqpP,R,O)
                                                # Equipment lifetime (lifeP,R,O)

restCosts <- read.table("restorationCosts.csv", sep=",", header=T, row.names=1, stringsAsFactors=F, na.strings=".") # Enables row names
head(restCosts) # Look at first 6 rows of data
str(restCosts) # Take a look at internal structure of data

# READ IN SEPARATE SECTIONS OF REPEAT CODE LOOPS USED TO COMPUTE LABOUR, EQUIPMENT AND TRANSPORT COSTS:
costcalc <- paste(inDir,"restCosts_calc.r",sep="/")                                                              # <- Labour and equipment costs calc input code
tcostcalc <- paste(inDir,"tsportCosts_calc.r",sep="/")                                                           # <- Transport costs calc input code
# Based on cost per return trip, this code calculates the total tranport cost per method,
# by factoring in the number of trips (determined by team size / number of stems),
# repeats, years, intervals between years, and equipment costs.

### Read in raster stack used to determine restoration costs
# (contains distance rasters used in this analysis)
st <- stack("costs_stack.grd")      # Restoration costs raster stack
lc <- subset(st,1)                  # r1 = Current Land Cover ("lc")
elev <- subset(st,2)                # r2 = Elevation ("elev")
campnights <- subset(st,3)          # r3 = Camping distance bands ("campnights") (# nights spent camping, if >3km from habitation or roads, 1 night spent per additional 7km travelled)
rdtsport <- subset(st,4)            # r4 = Road transport need ("rdtsport") (all sites >3km from habitation that are closer to roads than habitation)
distag <- subset(st,5)              # r5 = Distance Agriculture ("distag") (for areas requiring off-road transport only)
distrd <- subset(st,6)              # r6 = Distance Road ("distrd") (for areas requiring road transport)
rddist <- subset(st,7)              # r7 = Distance along road ("rddist") (for areas requiring road transport)

# Read in additional rasters needed: (determinants of costs)
land <- raster("landprices.tif")
priceha <- raster("landprices_nursery.tif")
ext <- raster("Extent_1.tif") # Extent raster where all pixels==1 for multiplying by gov, thus converting gov to raster in cases where it isn't already
fbplan <- raster("fbplan.tif")
fbCalc <- read.table("firebreakCalc.csv", sep=",", header=T, na.strings=".")
str(fbCalc)


##### DEFINE INVESTMENT DURATION (# YEARS) INTERESTED IN:

# Create additional vectors needed
dur <- 5          # Investment duration
infl <- 0.022     # Inflation rate
haP <- 50
haR <- 100
haO <- 1000

# Set output directory (folder in which outputs will be saved):
setwd(outC5)

# Create emply rasters to correct size, res, CRS (for assigning costs)
labC <- raster(lc)   # For activity cost
eqpC <- raster(lc)   # For equipment cost
totC <- raster(lc)   # For total cost

############################# LAND PROCUREMENT #################################

# PESS
landP <- raster(lc)
landP <- land*1.25
landP
writeRaster(landP, "landCostP.tif",overwrite=T)

# REAL
landR <- land*1
landR
writeRaster(landR, "landCostR.tif",overwrite=T)

# OPT
landO <- land*0
landO
writeRaster(landO, "landCostO.tif",overwrite=T)

########################### LAND MANAGEMENT / ANR ##############################

##### METHODS RELEVANT TO FOREST LC #####

### VINES
# PESS
rel <- raster(paste(outM, "vinesP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "vines"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "vnCostP.tif",overwrite=T)
# Data table
Method <- m
LabourP <- v1
EquipmentP <- v2
TotalP <- v3

# REAL
rel <- raster(paste(outM, "vinesR.grd", sep="/")) # Methods raster where relevant pixels == 1
rel
# Create additional vectors needed:
m <- "vines"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "vnCostR.tif",overwrite=T)
# Data table
LabourR <- v1
EquipmentR <- v2
TotalR <- v3

# OPT
rel <- raster(paste(outM, "vinesO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "vines"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "vnCostO.tif",overwrite=T)
# Data table
LabourO <- v1
EquipmentO <- v2
TotalO <- v3

### HERBS/SHRUBS
# PESS
rel <- raster(paste(outM, "herbsP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "shrub"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "sbCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "herbsR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "shrub"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "sbCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "herbsO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "shrub"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "sbCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### LANTANA
# PESS
rel <- raster(paste(outM, "lantP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "lantana"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "ltCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "lantR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "lantana"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "ltCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "lantO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "lantana"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "ltCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

##### METHODS RELEVANT TO SAVANNA & AGMOSAIC LC #####

### GRASS
# PESS
rel <- raster(paste(outM, "grassP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "grass"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "gsCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "grassR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "grass"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "gsCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "grassO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "grass"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "gsCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### FIREBREAKS
## Proportion of costs factored into calculation:
#  determined by firebreak size calc which differs between lc type and PA size (as per fbplan raster, see code: costs-input-rasts
#  and whether grass cutting is already being implemented as a management strategy

# PESS
scen <- "P"
rel <- raster(paste(outM, "fireP.grd", sep="/")) # Methods raster where relevant pixels == 1
grass <- raster(paste(outM, "grassP.grd", sep="/"))
grass[is.na(grass[])] <- 0
fbplan <- mask(fbplan, rel)
perc <- combine_words(c("perc",scen), and="")
fbPerc <- raster(lc)
fbPerc[(fbplan==1) & (grass==1)] <- fbCalc[2,perc]
fbPerc[(fbplan==2) & (grass==1)] <- fbCalc[3,perc]
fbPerc[(fbplan==3) & (grass==1)] <- fbCalc[4,perc]
fbPerc[(fbplan==1) & (grass==0)] <- fbCalc[5,perc]
fbPerc[(fbplan==2) & (grass==0)] <- fbCalc[6,perc]
fbPerc[(fbplan==3) & (grass==0)] <- fbCalc[7,perc]
# Create additional vectors needed:
m <- "firebreaks"  # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# Final calculation based on firebreak size:
totC <- totC * fbPerc
totC
labC <- labC * fbPerc
labC
eqpC <- eqpC * fbPerc
eqpC
writeRaster(totC, "fbCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
scen <- "R"
rel <- raster(paste(outM, "fireR.grd", sep="/")) # Methods raster where relevant pixels == 1
grass <- raster(paste(outM, "grassR.grd", sep="/"))
grass[is.na(grass[])] <- 0
fbplan <- mask(fbplan, rel)
perc <- combine_words(c("perc",scen), and="")
fbPerc <- raster(lc)
fbPerc[(fbplan==1) & (grass==1)] <- fbCalc[2,perc]
fbPerc[(fbplan==2) & (grass==1)] <- fbCalc[3,perc]
fbPerc[(fbplan==3) & (grass==1)] <- fbCalc[4,perc]
fbPerc[(fbplan==1) & (grass==0)] <- fbCalc[5,perc]
fbPerc[(fbplan==2) & (grass==0)] <- fbCalc[6,perc]
fbPerc[(fbplan==3) & (grass==0)] <- fbCalc[7,perc]
# Create additional vectors needed:
m <- "firebreaks"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# Final calculation based on firebreak size:
totC <- totC * fbPerc
totC
labC <- labC * fbPerc
labC
eqpC <- eqpC * fbPerc
eqpC
writeRaster(totC, "fbCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
scen <- "O"
rel <- raster(paste(outM, "fireO.grd", sep="/")) # Methods raster where relevant pixels == 1
grass <- raster(paste(outM, "grassO.grd", sep="/"))
grass[is.na(grass[])] <- 0
fbplan <- mask(fbplan, rel)
perc <- combine_words(c("perc",scen), and="")
fbPerc <- raster(lc)
fbPerc[(fbplan==1) & (grass==1)] <- fbCalc[2,perc]
fbPerc[(fbplan==2) & (grass==1)] <- fbCalc[3,perc]
fbPerc[(fbplan==3) & (grass==1)] <- fbCalc[4,perc]
fbPerc[(fbplan==1) & (grass==0)] <- fbCalc[5,perc]
fbPerc[(fbplan==2) & (grass==0)] <- fbCalc[6,perc]
fbPerc[(fbplan==3) & (grass==0)] <- fbCalc[7,perc]
# Create additional vectors needed:
m <- "firebreaks"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# Final calculation based on firebreak size:
totC <- totC * fbPerc
totC
labC <- labC * fbPerc
labC
eqpC <- eqpC * fbPerc
eqpC
writeRaster(totC, "fbCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

#####################  TREE PLANTING / ACTIVE RESTORATION ######################

### NURSERY PLOT, ESTABLISHMENT & MANAGEMENT

### NO COSTS IN THIS SECTION FOR ENRICHMENT PLANTING SINCE HAPPENS IN FOLLOW UP TO
### FSA / NURSE TREE PLANTING AND SO USE NURSERY ALREADY IN PLACE FOR THOSE METHODS

### FSA NURSERY

### NURSERY PLOT

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
price <- priceha * 0.03 # Estimated land price/ha in nursery hub location
                        # multiplied by estimated plot size in ha needed
                        # to grow sufficient seedlings to restore 1ha
price <- price * 1.25   # Estimated nursery plot price * estimated percentage paid (variable by scenario)
rel2 <- mask(price, rel)
rel2
writeRaster(rel2, "npFSACostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/"))
price <- priceha * 0.02
price <- price * 1
rel2 <- mask(price, rel)
rel2
writeRaster(rel2, "npFSACostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/"))
price <- priceha * 0.01
price <- price * 0
rel2 <- mask(price, rel)
rel2
writeRaster(rel2, "npFSACostO.tif",overwrite=T)

### NURSERY ESTABLISHMENT
### At present uses average building materials price of Kilombero Valley (2M, using bricks) and Iringa highlands (1.5M, using timber)
### Could improve by distinguishing between the two geographic areas for more accurate cost calcs

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryFSAestab"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haP # Labour cost
v2 <- v2 / haP # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "neFSACostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryFSAestab"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haR # Labour cost
v2 <- v2 / haR # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "neFSACostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryFSAestab"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haO # Labour cost
v2 <- v2 / haO # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "neFSACostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### NURSERY MANAGEMENT

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryFSAmgmt"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haP # Labour cost
v2 <- v2 / haP # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nmFSACostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryFSAmgmt"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haR # Labour cost
v2 <- v2 / haR # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nmFSACostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryFSAmgmt"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haO # Labour cost
v2 <- v2 / haO # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nmFSACostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## NURSERY TOTAL (PLOT + ESTAB + MGMT)

# PESS
r1 <- raster("npFSACostP.tif")
r2 <- raster("neFSACostP.tif")
r3 <- raster("nmFSACostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "nurseryFSACostP.tif",overwrite=T)

# REAL
r1 <- raster("npFSACostR.tif")
r2 <- raster("neFSACostR.tif")
r3 <- raster("nmFSACostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "nurseryFSACostR.tif",overwrite=T)

# OPT
r1 <- raster("npFSACostO.tif")
r2 <- raster("neFSACostO.tif")
r3 <- raster("nmFSACostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "nurseryFSACostO.tif",overwrite=T)

### NSE NURSERY

### NURSERY PLOT

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
price <- priceha * 0.03 # Estimated land price/ha in nursery hub location multiplied by estimated plot size in ha needed to grow seedlings
price <- price * 1.25 # Estimated nursery plot price * estimated percentage paid (variable by scenario)
rel2 <- mask(price, rel)
rel2
writeRaster(rel2, "npNseCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
price <- priceha * 0.02 # Estimated land price/ha in nursery hub location multiplied by estimated plot size in ha needed to grow seedlings
price <- price * 1 # Estimated nursery plot price * estimated percentage paid (variable by scenario)
rel2 <- mask(price, rel)
rel2
writeRaster(rel2, "npNseCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
price <- priceha * 0.01 # Estimated land price/ha in nursery hub location multiplied by estimated plot size in ha needed to grow seedlings
price <- price * 0 # Estimated nursery plot price * estimated percentage paid (variable by scenario)
rel2 <- mask(price, rel)
rel2
writeRaster(rel2, "npNseCostO.tif",overwrite=T)

### NURSERY ESTABLISHMENT
### At present uses average building materials price of Kilombero Valley (2M, using bricks) and Iringa highlands (1.5M, using timber)
### Could improve by distinguishing between the two geographic areas for more accurate cost calcs

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryNseestab"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haP # Labour cost
v2 <- v2 / haP # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "neNseCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryNseestab"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haR # Labour cost
v2 <- v2 / haR # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "neNseCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryNseestab"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haO # Labour cost
v2 <- v2 / haO # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "neNseCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### NURSERY MANAGEMENT

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryNsemgmt"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haP # Labour cost
v2 <- v2 / haP # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nmNseCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryNsemgmt"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haR # Labour cost
v2 <- v2 / haR # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nmNseCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseryNsemgmt"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haO # Labour cost
v2 <- v2 / haO # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nmNseCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## NURSERY TOTAL (PLOT + ESTAB + MGMT)

# PESS
r1 <- raster("npNseCostP.tif")
r2 <- raster("neNseCostP.tif")
r3 <- raster("nmNseCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "nurseryNseCostP.tif",overwrite=T)

# REAL
r1 <- raster("npNseCostR.tif")
r2 <- raster("neNseCostR.tif")
r3 <- raster("nmNseCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "nurseryNseCostR.tif",overwrite=T)

# OPT
r1 <- raster("npNseCostO.tif")
r2 <- raster("neNseCostO.tif")
r3 <- raster("nmNseCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "nurseryNseCostO.tif",overwrite=T)

## NURSERY TOTAL - ALL TREE PLANTING METHODS

# PESS
r2 <- raster("nurseryFSACostP.tif")
r3 <- raster("nurseryNseCostP.tif")
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r2 + r3
r
writeRaster(r, "nurseryCostP.tif",overwrite=T)

# REAL
r2 <- raster("nurseryFSACostR.tif")
r3 <- raster("nurseryNseCostR.tif")
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r2 + r3
r
writeRaster(r, "nurseryCostR.tif",overwrite=T)

# OPT
r2 <- raster("nurseryFSACostO.tif")
r3 <- raster("nurseryNseCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r2 + r3
r
writeRaster(r, "nurseryCostO.tif",overwrite=T)

### ENRICHMENT PLANTING (POST FSA)

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichFSA"   # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
labC[rel>0 & lc==3] <- if(scen=="P") {v1 * 2} else if(scen=="R") {v1 * 1.5} else if(scen=="O") {v1 * 1.25}
labC[rel>0 & lc!=3] <- v1
totC <- labC + eqpC
totC
writeRaster(totC, "enrCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichFSA"   # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
labC[rel>0 & lc==3] <- if(scen=="P") {v1 * 2} else if(scen=="R") {v1 * 1.5} else if(scen=="O") {v1 * 1.25}
labC[rel>0 & lc!=3] <- v1
totC <- labC + eqpC
totC
writeRaster(totC, "enrCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichFSA"   # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
labC[rel>0 & lc==3] <- if(scen=="P") {v1 * 2} else if(scen=="R") {v1 * 1.5} else if(scen=="O") {v1 * 1.25}
labC[rel>0 & lc!=3] <- v1
totC <- labC + eqpC
totC
writeRaster(totC, "enrCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### FRAMEWORK SPECIES

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsa"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
labC[rel>0 & lc==3] <- if(scen=="P") {v1 * 2} else if(scen=="R") {v1 * 1.5} else if(scen=="O") {v1 * 1.25}
labC[rel>0 & lc!=3] <- v1
totC <- labC + eqpC
totC
writeRaster(totC, "fsCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsa"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
labC[rel>0 & lc==3] <- if(scen=="P") {v1 * 2} else if(scen=="R") {v1 * 1.5} else if(scen=="O") {v1 * 1.25}
labC[rel>0 & lc!=3] <- v1
totC <- labC + eqpC
totC
writeRaster(totC, "fsCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsa"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
labC[rel>0 & lc==3] <- if(scen=="P") {v1 * 2} else if(scen=="R") {v1 * 1.5} else if(scen=="O") {v1 * 1.25}
labC[rel>0 & lc!=3] <- v1
totC <- labC + eqpC
totC
writeRaster(totC, "fsCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### NURSE TREES

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurse"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
labC[rel>0 & lc==3] <- if(scen=="P") {v1 * 2} else if(scen=="R") {v1 * 1.5} else if(scen=="O") {v1 * 1.25}
labC[rel>0 & lc!=3] <- v1
totC <- labC + eqpC
totC
writeRaster(totC, "nsCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurse"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
labC[rel>0 & lc==3] <- if(scen=="P") {v1 * 2} else if(scen=="R") {v1 * 1.5} else if(scen=="O") {v1 * 1.25}
labC[rel>0 & lc!=3] <- v1
totC <- labC + eqpC
totC
writeRaster(totC, "nsCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurse"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
labC[rel>0 & lc==3] <- if(scen=="P") {v1 * 2} else if(scen=="R") {v1 * 1.5} else if(scen=="O") {v1 * 1.25}
labC[rel>0 & lc!=3] <- v1
totC <- labC + eqpC
totC
writeRaster(totC, "nsCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

##### ALL TREE PLANTING

# PESS
r1 <- raster("fsCostP.tif")
r2 <- raster("nsCostP.tif")
r3 <- raster("enrCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "treesCostP.tif",overwrite=T)

# REAL
r1 <- raster("fsCostR.tif")
r2 <- raster("nsCostR.tif")
r3 <- raster("enrCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "treesCostR.tif",overwrite=T)

# OPT
r1 <- raster("fsCostO.tif")
r2 <- raster("nsCostO.tif")
r3 <- raster("enrCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "treesCostO.tif",overwrite=T)

##### SOIL IMPROVEMENT
### Types: Ploughing, (deep ripping); organic fertiliser, (green mulching, topsoil replacement)

### PLOUGHING
# Relevance: All cases with soil improvement
# PESS
rel <- raster(paste(outM, "soilP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "plough"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "phCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "soilR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "plough"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "phCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "soilO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "plough"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "phCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### ORGANIC FERTILISER
# Relevance: All cases with soil improvement
# Could incl. condition to exclude areas where MaxD method is employed [topsoil replacement]
# and in AgMosaic where green mulching = cheaper and greater benefit to communities?)
# PESS
rel <- raster(paste(outM, "soilP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fertilise"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "ftCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "soilR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fertilise"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "ftCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "soilO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fertilise"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "ftCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

##### SOIL COST (PLOUGHING + FERTILISER)
# PESS
r1 <- raster("phCostP.tif")
r2 <- raster("ftCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3 <- r1 + r2
r3
writeRaster(r3, "slCostP.tif",overwrite=T)

# REAL
r1 <- raster("phCostR.tif")
r2 <- raster("ftCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3 <- r1 + r2
r3
writeRaster(r3, "slCostR.tif",overwrite=T)

# OPT
r1 <- raster("phCostO.tif")
r2 <- raster("ftCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3 <- r1 + r2
r3
writeRaster(r3, "slCostO.tif",overwrite=T)

############################# POST PLANTING MANAGEMENT #########################

### ENRICHMENT PLANTING MANAGEMENT
# Includes: weeding, organic fertiliser, watering

## ENRICH WEEDING

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichweed"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "enrweedCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichweed"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "enrweedCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichweed"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "enrweedCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## ENRICH FERTILISER

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichfertilise"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "enrfertCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichfertilise"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "enrfertCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichfertilise"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "enrfertCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## ENRICH WATERING

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichwater"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "enrwatCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichwater"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "enrwatCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "enrichwater"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "enrwatCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## ENRICHMENT PLANTING MGMT

# PESS
r1 <- raster("enrweedCostP.tif")
r2 <- raster("enrfertCostP.tif")
r3 <- raster("enrwatCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "enrmCostP.tif",overwrite=T)

# REAL
r1 <- raster("enrweedCostR.tif")
r2 <- raster("enrfertCostR.tif")
r3 <- raster("enrwatCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "enrmCostR.tif",overwrite=T)

# OPT
r1 <- raster("enrweedCostO.tif")
r2 <- raster("enrfertCostO.tif")
r3 <- raster("enrwatCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "enrmCostO.tif",overwrite=T)

### FRAMEWORK SPECIES PLANTING MANAGEMENT
# Includes: weeding, organic fertiliser, watering

## FSA WEEDING

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsaweed"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fsaweedCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsaweed"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fsaweedCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsaweed"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fsaweedCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## FSA FERTILISER

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsafertilise"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fsafertCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsafertilise"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fsafertCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsafertilise"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fsafertCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## FSA WATERING

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsawater"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fsawatCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsawater"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fsawatCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "fsawater"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fsawatCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## FRAMEWORK SPECIES PLANTING MGMT

# PESS
r1 <- raster("fsaweedCostP.tif")
r2 <- raster("fsafertCostP.tif")
r3 <- raster("fsawatCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "fsamCostP.tif",overwrite=T)

# REAL
r1 <- raster("fsaweedCostR.tif")
r2 <- raster("fsafertCostR.tif")
r3 <- raster("fsawatCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "fsamCostR.tif",overwrite=T)

# OPT
r1 <- raster("fsaweedCostO.tif")
r2 <- raster("fsafertCostO.tif")
r3 <- raster("fsawatCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "fsamCostO.tif",overwrite=T)

### NURSE TREES PLANTING MANAGEMENT
# Includes: weeding, organic fertiliser, watering, thinning out

## NURSE WEEDING

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseweed"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nurseweedCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseweed"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nurseweedCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseweed"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nurseweedCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## NURSE FERTILISER

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nursefertilise"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nursefertCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nursefertilise"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nursefertCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nursefertilise"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nursefertCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## NURSE WATERING

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nursewater"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nursewatCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nursewater"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nursewatCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nursewater"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nursewatCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## NURSE THINNING

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nursethin"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nursethinCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nursethin"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nursethinCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nursethin"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nursethinCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## NURSE ENRICHMENT (ALONGSIDE THINNING)

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseenr"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nurseenrCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseenr"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nurseenrCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "nurseenr"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "nurseenrCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

## NURSE TREE PLANTING MGMT

# PESS
r1 <- raster("nurseweedCostP.tif")
r2 <- raster("nursefertCostP.tif")
r3 <- raster("nursewatCostP.tif")
r4 <- raster("nursethinCostP.tif")
r5 <- raster("nurseenrCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r4[is.na(r4[])] <- 0
r5[is.na(r5[])] <- 0
r <- r1 + r2 + r3 + r4 + r5
r
writeRaster(r, "nursemCostP.tif",overwrite=T)

# REAL
r1 <- raster("nurseweedCostR.tif")
r2 <- raster("nursefertCostR.tif")
r3 <- raster("nursewatCostR.tif")
r4 <- raster("nursethinCostR.tif")
r5 <- raster("nurseenrCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r4[is.na(r4[])] <- 0
r5[is.na(r5[])] <- 0
r <- r1 + r2 + r3 + r4 + r5
r
writeRaster(r, "nursemCostR.tif",overwrite=T)

# OPT
r1 <- raster("nurseweedCostO.tif")
r2 <- raster("nursefertCostO.tif")
r3 <- raster("nursewatCostO.tif")
r4 <- raster("nursethinCostO.tif")
r5 <- raster("nurseenrCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r4[is.na(r4[])] <- 0
r5[is.na(r5[])] <- 0
r <- r1 + r2 + r3 + r4 + r5
r
writeRaster(r, "nursemCostO.tif",overwrite=T)

### FIRE RESPONSE

# PESS
rel <- raster(paste(outA, "activeP.grd", sep="/"))
# Create additional vectors needed:
m <- "firemgmt"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fmCostP.tif", overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outA, "activeR.grd", sep="/"))
# Create additional vectors needed:
m <- "firemgmt"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fmCostR.tif", overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outA, "activeO.grd", sep="/"))
# Create additional vectors needed:
m <- "firemgmt"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "fmCostO.tif", overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

############################# POST PLANTING MONITORING #########################

### ENRICHMENT PLANTING MONITORING

### MONITORING Y1

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monEnry1"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monEnrY1CostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monEnry1"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monEnrY1CostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monEnry1"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monEnrY1CostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### MONITORING Y2 to canopy closure

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monEnry2"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monEnrY2CostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monEnry2"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monEnrY2CostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monEnry2"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monEnrY2CostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### MONITORING Continued to end of investment

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monEnrcont"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monEnrContCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monEnrcont"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monEnrContCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monEnrcont"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monEnrContCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### ENR MONITORING TOTAL

# PESS
r1 <- raster("monEnrY1CostP.tif")
r2 <- raster("monEnrY2CostP.tif")
r3 <- raster("monEnrContCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monEnrCostP.tif",overwrite=T)

# REAL
r1 <- raster("monEnrY1CostR.tif")
r2 <- raster("monEnrY2CostR.tif")
r3 <- raster("monEnrContCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monEnrCostR.tif",overwrite=T)

# OPT
r1 <- raster("monEnrY1CostO.tif")
r2 <- raster("monEnrY2CostO.tif")
r3 <- raster("monEnrContCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monEnrCostO.tif",overwrite=T)

### FSA MONITORING

### MONITORING Y1

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monFSAy1"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monFSAY1CostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monFSAy1"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monFSAY1CostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monFSAy1"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monFSAY1CostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### MONITORING Y2 to canopy closure

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monFSAy2"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monFSAY2CostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monFSAy2"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monFSAY2CostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monFSAy2"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monFSAY2CostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### MONITORING Continued to end of investment

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monFSAcont"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monFSAContCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monFSAcont"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monFSAContCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monFSAcont"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monFSAContCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### ENR MONITORING TOTAL

# PESS
r1 <- raster("monFSAY1CostP.tif")
r2 <- raster("monFSAY2CostP.tif")
r3 <- raster("monFSAContCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monFSACostP.tif",overwrite=T)

# REAL
r1 <- raster("monFSAY1CostR.tif")
r2 <- raster("monFSAY2CostR.tif")
r3 <- raster("monFSAContCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monFSACostR.tif",overwrite=T)

# OPT
r1 <- raster("monFSAY1CostO.tif")
r2 <- raster("monFSAY2CostO.tif")
r3 <- raster("monFSAContCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monFSACostO.tif",overwrite=T)

### NSE MONITORING

### MONITORING Y1

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monNsey1"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monNseY1CostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monNsey1"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monNseY1CostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monNsey1"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monNseY1CostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### MONITORING Y2 to canopy closure

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monNsey2"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monNseY2CostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monNsey2"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monNseY2CostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monNsey2"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monNseY2CostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### MONITORING Continued to end of investment

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monNsecont"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monNseContCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monNsecont"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monNseContCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monNsecont"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monNseContCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### NSE MONITORING TOTAL

# PESS
r1 <- raster("monNseY1CostP.tif")
r2 <- raster("monNseY2CostP.tif")
r3 <- raster("monNseContCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monNseCostP.tif",overwrite=T)

# REAL
r1 <- raster("monNseY1CostR.tif")
r2 <- raster("monNseY2CostR.tif")
r3 <- raster("monNseContCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monNseCostR.tif",overwrite=T)

# OPT
r1 <- raster("monNseY1CostO.tif")
r2 <- raster("monNseY2CostO.tif")
r3 <- raster("monNseContCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monNseCostO.tif",overwrite=T)

### ANR MONITORING

### MONITORING Y1

# PESS
rel <- raster(paste(outA, "anrP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monANRy1"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monANRY1CostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outA, "anrR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monANRy1"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monANRY1CostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outA, "anrO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monANRy1"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monANRY1CostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### MONITORING Y2 to canopy closure

# PESS
rel <- raster(paste(outA, "anrP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monANRy2"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monANRY2CostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outA, "anrR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monANRy2"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monANRY2CostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outA, "anrO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monANRy2"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monANRY2CostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### MONITORING Continued to end of investment

# PESS
rel <- raster(paste(outA, "anrP.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monANRcont"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monANRContCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outA, "anrR.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monANRcont"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monANRContCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outA, "anrO.grd", sep="/")) # Methods raster where relevant pixels == 1
# Create additional vectors needed:
m <- "monANRcont"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
writeRaster(totC, "monANRContCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

### ANR MONITORING TOTAL

# PESS
r1 <- raster("monANRY1CostP.tif")
r2 <- raster("monANRY2CostP.tif")
r3 <- raster("monANRContCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monANRCostP.tif",overwrite=T)

# REAL
r1 <- raster("monANRY1CostR.tif")
r2 <- raster("monANRY2CostR.tif")
r3 <- raster("monANRContCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monANRCostR.tif",overwrite=T)

# OPT
r1 <- raster("monANRY1CostO.tif")
r2 <- raster("monANRY2CostO.tif")
r3 <- raster("monANRContCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "monANRCostO.tif",overwrite=T)

############################# COMMUNITY ENGAGEMENT #############################
# Restoration work in majority agriculture/settlement areas (with the exception of large commercial farms)
# would require capacity-building and livelihood engagement
# Restoration within 3km of these areas would require 50% of these costs
# No community engagement costs incurred for restoration in PAs
# Community Engagement costs estimated per village
# Pessimistic = 50ha; Realistic = 100ha; Optimistic = 1000ha

# PESS # Land Use Planning
rel <- raster(paste(outM, "commengP.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "lup"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haP # Labour cost
v2 <- v2 / haP # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "lupCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL # Land Use Planning
rel <- raster(paste(outM, "commengR.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "lup"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haR # Labour cost
v2 <- v2 / haR # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "lupCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT # Land Use Planning
rel <- raster(paste(outM, "commengO.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "lup"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haO # Labour cost
v2 <- v2 / haO # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "lupCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

# PESS # Sustainability start-up
rel <- raster(paste(outM, "commengP.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "sust"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haP # Labour cost
v2 <- v2 / haP # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "sustCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL # Sustainability start-up
rel <- raster(paste(outM, "commengR.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "sust"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haR # Labour cost
v2 <- v2 / haR # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "sustCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT # Sustainability start-up
rel <- raster(paste(outM, "commengO.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "sust"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haO # Labour cost
v2 <- v2 / haO # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "sustCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

# PESS # Annual progress workshop (year 1)
rel <- raster(paste(outM, "commengP.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "wshop1"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haP # Labour cost
v2 <- v2 / haP # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "wshopy1CostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL # Annual progress workshop (year 1)
rel <- raster(paste(outM, "commengR.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "wshop1"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haR # Labour cost
v2 <- v2 / haR # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "wshopy1CostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT # Annual progress workshop (year 1)
rel <- raster(paste(outM, "commengO.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "wshop1"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haO # Labour cost
v2 <- v2 / haO # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "wshopy1CostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)


# PESS # Annual progress workshop (continued)
rel <- raster(paste(outM, "commengP.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "wshop2"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haP # Labour cost
v2 <- v2 / haP # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "wshopcontCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL # Annual progress workshop (continued)
rel <- raster(paste(outM, "commengR.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "wshop2"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haR # Labour cost
v2 <- v2 / haR # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "wshopcontCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT # Annual progress workshop (continued)
rel <- raster(paste(outM, "commengO.grd", sep="/"))
rel[rel==NA] <- 0
# Create additional vectors needed:
m <- "wshop2"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 <- v1 / haO # Labour cost
v2 <- v2 / haO # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- rel * totC
totC2
writeRaster(totC2, "wshopcontCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)


############################# STAFF ############################################

# PROJECT MANAGER YEAR 1-5
# PESS
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
mult <- raster(paste(inDir, "managerMultP.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy1mgmt"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy1mCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
mult <- raster(paste(inDir, "managerMultR.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy1mgmt"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy1mCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
mult <- raster(paste(inDir, "managerMultO.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy1mgmt"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy1mCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

# PROJECT FORESTER YEAR 1-5
# PESS
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
mult <- raster(paste(inDir, "foresterMultP.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy1for"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy1forCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
mult <- raster(paste(inDir, "foresterMultR.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy1for"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy1forCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
mult <- raster(paste(inDir, "foresterMultO.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy1for"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy1forCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

# COMMUNITY ENGAGEMENT OFFICER YEAR 1-5
# PESS
rel <- raster(paste(outM, "commengP.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultP.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy1ceng"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy1ceCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "commengR.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultR.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy1ceng"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy1ceCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "commengO.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultO.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy1ceng"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy1ceCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

# PROJECT MANAGER YEAR 5 ONWARD
# PESS
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
mult <- raster(paste(inDir, "managerMultP.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy5mgmt"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy5mCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
mult <- raster(paste(inDir, "managerMultR.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy5mgmt"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy5mCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
mult <- raster(paste(inDir, "managerMultO.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy5mgmt"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy5mCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

# PROJECT FORESTER YEAR 5 ONWARD
# PESS
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
mult <- raster(paste(inDir, "foresterMultP.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy5for"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy5forCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
mult <- raster(paste(inDir, "foresterMultR.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy5for"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy5forCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
mult <- raster(paste(inDir, "foresterMultO.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy5for"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # Labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy5forCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

# COMMUNITY ENGAGEMENT OFFICER YEAR 5 ONWARD
# PESS
rel <- raster(paste(outM, "commengP.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultP.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy5ceng"       # Focal method (m)
scen <- "P"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy5ceCostP.tif",overwrite=T)
# Data table
Method <- c(Method, m)
LabourP <- c(LabourP, v1)
EquipmentP <- c(EquipmentP, v2)
TotalP <- c(TotalP, v3)

# REAL
rel <- raster(paste(outM, "commengR.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultR.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy5ceng"       # Focal method (m)
scen <- "R"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy5ceCostR.tif",overwrite=T)
# Data table
LabourR <- c(LabourR, v1)
EquipmentR <- c(EquipmentR, v2)
TotalR <- c(TotalR, v3)

# OPT
rel <- raster(paste(outM, "commengO.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultO.tif", sep="/"))
# Create additional vectors needed:
m <- "staffy5ceng"       # Focal method (m)
scen <- "O"        # Scenario
perc <- 1          # Proportion of costs factored into calculation
## Cost Calc #
source(costcalc)
v1 # labour cost
v2 # Eqp cost
v3 <- v1 + v2
v3
# Now apply to rasters:
labC[rel>0] <- v1
labC
eqpC[rel>0] <- v2
eqpC
# TOTAL COST (LABOUR + EQUIPMENT)
totC <- labC + eqpC
totC2 <- totC / mult
writeRaster(totC2, "staffy5ceCostO.tif",overwrite=T)
# Data table
LabourO <- c(LabourO, v1)
EquipmentO <- c(EquipmentO, v2)
TotalO <- c(TotalO, v3)

mydata <- data.frame(Method, LabourP,EquipmentP,TotalP, LabourR,EquipmentR,TotalR, LabourO,EquipmentO,TotalO)
mydata[is.na(mydata)] <- 0
write.csv(mydata, "costs-summary.csv", row.names = FALSE)

############################## SUBTOTAL STAFF COSTS ############################

# PESS
r1 <- raster("staffy1mCostP.tif")
r2 <- raster("staffy5mCostP.tif")
r <- sum(r1, r2, na.rm=T)
writeRaster(r, "staffmCostP.tif", overwrite=T)

r1 <- raster("staffy1forCostP.tif")
r2 <- raster("staffy5forCostP.tif")
r <- sum(r1, r2, na.rm=T)
writeRaster(r, "staffforCostP.tif", overwrite=T)

r1 <- raster("staffy1ceCostP.tif")
r2 <- raster("staffy5ceCostP.tif")
r <- sum(r1, r2, na.rm=T)
writeRaster(r, "staffceCostP.tif", overwrite=T)

# REAL
r1 <- raster("staffy1mCostR.tif")
r2 <- raster("staffy5mCostR.tif")
r <- sum(r1, r2, na.rm=T)
writeRaster(r, "staffmCostR.tif", overwrite=T)

r1 <- raster("staffy1forCostR.tif")
r2 <- raster("staffy5forCostR.tif")
r <- sum(r1, r2, na.rm=T)
writeRaster(r, "staffforCostR.tif", overwrite=T)

r1 <- raster("staffy1ceCostR.tif")
r2 <- raster("staffy5ceCostR.tif")
r <- sum(r1, r2, na.rm=T)
writeRaster(r, "staffceCostR.tif", overwrite=T)

# OPT
r1 <- raster("staffy1mCostO.tif")
r2 <- raster("staffy5mCostO.tif")
r <- sum(r1, r2, na.rm=T)
writeRaster(r, "staffmCostO.tif", overwrite=T)

r1 <- raster("staffy1forCostO.tif")
r2 <- raster("staffy5forCostO.tif")
r <- sum(r1, r2, na.rm=T)
writeRaster(r, "staffforCostO.tif", overwrite=T)

r1 <- raster("staffy1ceCostO.tif")
r2 <- raster("staffy5ceCostO.tif")
r <- sum(r1, r2, na.rm=T)
writeRaster(r, "staffceCostO.tif", overwrite=T)

################################################################################
############################## TOTAL COSTS CALC ################################
################################################################################

################################################################################
############################## TOTAL ANR COSTS #################################

### SUM ANR COSTS IN FORESTS ###################################################

# PESS
r1 <- raster("vnCostP.tif")
r2 <- raster("sbCostP.tif")
r3 <- raster("ltCostP.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
r <- raster("monANRCostP.tif")
r <- mask(r, tot)
tot <- tot + r
writeRaster(tot, "anrforCostP.tif", overwrite=T)

# REAL
r1 <- raster("vnCostR.tif")
r2 <- raster("sbCostR.tif")
r3 <- raster("ltCostR.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
r <- raster("monANRCostR.tif")
r <- mask(r, tot)
tot <- tot + r
writeRaster(tot, "anrforCostR.tif", overwrite=T)

# OPT
r1 <- raster("vnCostO.tif")
r2 <- raster("sbCostO.tif")
r3 <- raster("ltCostO.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
r <- raster("monANRCostO.tif")
r <- mask(r, tot)
tot <- tot + r
writeRaster(tot, "anrforCostO.tif", overwrite=T)

### SUM OF OTHER ANR COSTS IN SAVANNA / AGMOSAIC ###############################

# PESS
r1 <- raster("gsCostP.tif")
r2 <- raster("fbCostP.tif")
rstack <- stack(r1, r2)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
r <- raster("monANRCostP.tif")
r <- mask(r, tot)
tot <- tot + r
writeRaster(tot, "anrsavagCostP.tif", overwrite=T)

# REAL
r1 <- raster("gsCostR.tif")
r2 <- raster("fbCostR.tif")
rstack <- stack(r1, r2)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
r <- raster("monANRCostR.tif")
r <- mask(r, tot)
tot <- tot + r
writeRaster(tot, "anrsavagCostR.tif", overwrite=T)

# OPT
r1 <- raster("gsCostO.tif")
r2 <- raster("fbCostO.tif")
rstack <- stack(r1, r2)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
r <- raster("monANRCostO.tif")
r <- mask(r, tot)
tot <- tot + r
writeRaster(tot, "anrsavagCostO.tif", overwrite=T)

### SUM OF ALL ANR COSTS #######################################################

# PESS
r1 <- raster("anrforCostP.tif")
r2 <- raster("anrsavagCostP.tif")
rstack <- stack(r1, r2)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "anrTotCostP.tif", overwrite=T)

# REAL
r1 <- raster("anrforCostR.tif")
r2 <- raster("anrsavagCostR.tif")
rstack <- stack(r1, r2)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "anrTotCostR.tif", overwrite=T)

# OPT
r1 <- raster("anrforCostO.tif")
r2 <- raster("anrsavagCostO.tif")
rstack <- stack(r1, r2)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "anrTotCostO.tif", overwrite=T)

################################################################################
############### TOTAL ACTIVE RESTORATION (TREE PLANTING) COSTS #################

### SUM OF ALL TREE PLANTING METHODS (NURSERY, PLANTING AND SOIL)

# PESS
r1 <- raster("nurseryCostP.tif")
r2 <- raster("treesCostP.tif")
r3 <- raster("slCostP.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treeplCostP.tif", overwrite=T)

# REAL
r1 <- raster("nurseryCostR.tif")
r2 <- raster("treesCostR.tif")
r3 <- raster("slCostR.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treeplCostR.tif", overwrite=T)

# OPT
r1 <- raster("nurseryCostO.tif")
r2 <- raster("treesCostO.tif")
r3 <- raster("slCostO.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treeplCostO.tif", overwrite=T)

### SUM OF ALL TREE PLANTING MANAGEMENT METHODS (TREE MGMT + FIRE MGMT)

# PESS
r1 <- raster("enrmCostP.tif")
r2 <- raster("fsamCostP.tif")
r3 <- raster("nursemCostP.tif")
r4 <- raster("fmCostP.tif")
rstack <- stack(r1, r2, r3, r4)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treemCostP.tif",overwrite=T)

# REAL
r1 <- raster("enrmCostR.tif")
r2 <- raster("fsamCostR.tif")
r3 <- raster("nursemCostR.tif")
r4 <- raster("fmCostR.tif")
rstack <- stack(r1, r2, r3, r4)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treemCostR.tif",overwrite=T)

# OPT
r1 <- raster("enrmCostO.tif")
r2 <- raster("fsamCostO.tif")
r3 <- raster("nursemCostO.tif")
r4 <- raster("fmCostO.tif")
rstack <- stack(r1, r2, r3, r4)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treemCostO.tif",overwrite=T)

### SUM OF ALL TREE PLANTING MONITORING METHODS

# PESS
r1 <- raster("monEnrCostP.tif")
r2 <- raster("monFSACostP.tif")
r3 <- raster("monNseCostP.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "monTreesCostP.tif",overwrite=T)

# REAL
r1 <- raster("monEnrCostR.tif")
r2 <- raster("monFSACostR.tif")
r3 <- raster("monNseCostR.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "monTreesCostR.tif",overwrite=T)

# OPT
r1 <- raster("monEnrCostO.tif")
r2 <- raster("monFSACostO.tif")
r3 <- raster("monNseCostO.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "monTreesCostO.tif",overwrite=T)

### TOTAL COST FOR TREE PLANTING METHODS (PLANTING, MANAGEMENT AND MONITORING)

# PESS
r1 <- raster("treeplCostP.tif")
r2 <- raster("treemCostP.tif")
r3 <- raster("monTreesCostP.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treesTotCostP.tif", overwrite=T)

# REAL
r1 <- raster("treeplCostR.tif")
r2 <- raster("treemCostR.tif")
r3 <- raster("monTreesCostR.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treesTotCostR.tif", overwrite=T)

# OPT
r1 <- raster("treeplCostO.tif")
r2 <- raster("treemCostO.tif")
r3 <- raster("monTreesCostO.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treesTotCostO.tif", overwrite=T)

################################################################################
################### TOTAL COMMUNITY ENGAGEMENT COSTS ###########################

# PESS
r1 <- raster("lupCostP.tif")
r2 <- raster("sustCostP.tif")
r3 <- raster("wshopy1CostP.tif")
r4 <- raster("wshopcontCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r4[is.na(r4[])] <- 0
r <- r1 + r2 + r3 + r4
r
writeRaster(r, "ceCostP.tif",overwrite=T)

# REAL
r1 <- raster("lupCostR.tif")
r2 <- raster("sustCostR.tif")
r3 <- raster("wshopy1CostR.tif")
r4 <- raster("wshopcontCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r4[is.na(r4[])] <- 0
r <- r1 + r2 + r3 + r4
r
writeRaster(r, "ceCostR.tif",overwrite=T)

# OPT
r1 <- raster("lupCostO.tif")
r2 <- raster("sustCostO.tif")
r3 <- raster("wshopy1CostO.tif")
r4 <- raster("wshopcontCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r4[is.na(r4[])] <- 0
r <- r1 + r2 + r3 + r4
r
writeRaster(r, "ceCostO.tif",overwrite=T)

################################################################################
########################### TOTAL STAFFING COSTS ###############################
# PESS
r1 <- raster("staffmCostP.tif")
r2 <- raster("staffforCostP.tif")
r3 <- raster("staffceCostP.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "staffCostP.tif",overwrite=T)

# REAL
r1 <- raster("staffmCostR.tif")
r2 <- raster("staffforCostR.tif")
r3 <- raster("staffceCostR.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "staffCostR.tif",overwrite=T)

# OPT
r1 <- raster("staffmCostO.tif")
r2 <- raster("staffforCostO.tif")
r3 <- raster("staffceCostO.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
r3[is.na(r3[])] <- 0
r <- r1 + r2 + r3
r
writeRaster(r, "staffCostO.tif",overwrite=T)
