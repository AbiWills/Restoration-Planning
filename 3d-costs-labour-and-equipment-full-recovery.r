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
############# FULL RECOVERY LABOUR AND EQUIPMENT COSTS CALCULATION #############                                <- FULL RECOVERY LABOUR AND EQUIPMENT COSTS CALCULATION

# This code calculates the total labour and equipment costs incurred for each restoration
# pixel to the point of full AGB recovery, based on:
# method (porters required? etc), location (road / off road transport? camping needeed?)
# number of treatments per year, number of years of intervention, etc.

### Set input work directory
setwd(inDir)

##### CALL IN RESTORATION COSTS TABLE / CSV:
### Incl. 3 columns (Pess, Real, Opt) each for: # labour Cost (costP,R,O)
                                                # Num Repeats/Years (repsP,R,O)
                                                # Repeat interval (intP,R,O) - to do, for tree planting
                                                # Equipment cost (eqpP,R,O)
                                                # Equipment lifetime (lifeP,R,O)

restCosts <- read.table("restorationCosts.csv", sep=",", header=T, row.names=1, stringsAsFactors=F, na.strings=".") # Enables row names
head(restCosts) # Look at first 6 rows of data
str(restCosts)  # Take a look at internal structure of data

costcalc <- paste(inDir,"restCosts_calc-full-recovery.r", sep="/")
tcostcalc <- paste(inDir,"tsportCosts_calc-full-recovery.r", sep="/")

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

# Read in rasters that determine number of years to full recovery for rach pixel under the different scenarios
durP <- raster(paste(outG, "years-to-recovery-P.tif", sep="/"))
durR <- raster(paste(outG, "years-to-recovery-R.tif", sep="/"))
durO <- raster(paste(outG, "years-to-recovery-O.tif", sep="/"))

# Read in additional rasters needed: (determinants of costs)
land <- raster("landprices.tif")
priceha <- raster("landprices_nursery.tif")
ext <- raster("Extent_1.tif") # Extent raster where all pixels==1 for multiplying by gov, thus converting gov to raster in cases where it isn't already
fbplan <- raster("fbplan.tif")
fbCalc <- read.table("firebreakCalc.csv", sep=",", header=T, na.strings=".")
str(fbCalc)

# Set inflation rate for calculating future costs
infl <- 0.022      # Inflation rate
haP <- 50          # Area (ha) restored (pessimistic)
haR <- 100         # Area (ha) restored (realistic)
haO <- 1000        # Area (ha) restored (optimistic)

# Define output directory (folder in which outputs will be saved):
setwd(outCR)

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

################################################################################
# Also check why generic trees rasters used for calculating nursery costs here and in 5y code

### VINES
# PESS
rel <- raster(paste(outM,"vinesP.grd",sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "vines"       # Focal method (m)
scen <- "P"        # Scenario

# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "vnCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "vinesR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "vines"       # Focal method (m)
scen <- "R"        # Scenario

# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "vnCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "vinesO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "vines"       # Focal method (m)
scen <- "O"        # Scenario

# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "vnCostO.tif",overwrite=T)

### HERBS/SHRUBS
# PESS
rel <- raster(paste(outM, "herbsP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "shrub"       # Focal method (m)
scen <- "P"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "sbCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "herbsR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "shrub"       # Focal method (m)
scen <- "R"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "sbCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "herbsO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "shrub"       # Focal method (m)
scen <- "O"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "sbCostO.tif",overwrite=T)

### LANTANA
# PESS
rel <- raster(paste(outM, "lantP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "lantana"       # Focal method (m)
scen <- "P"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "ltCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "lantR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "lantana"       # Focal method (m)
scen <- "R"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "ltCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "lantO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "lantana"       # Focal method (m)
scen <- "O"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "ltCostO.tif",overwrite=T)


##### METHODS RELEVANT TO SAVANNA & AGMOSAIC LC #####

### GRASS
# PESS
rel <- raster(paste(outM, "grassP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "grass"       # Focal method (m)
scen <- "P"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "gsCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "grassR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "grass"       # Focal method (m)
scen <- "R"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "gsCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "grassO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "grass"       # Focal method (m)
scen <- "O"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster
writeRaster(tot, "gsCostO.tif",overwrite=T)

### FIREBREAKS
## Proportion of costs factored into calculation:
#  determined by firebreak size calc which differs between lc type and PA size (as per fbplan raster, see code: costs-stack
#  and whether grass cutting is already being implemented as a management strategy

# PESS
scen <- "P"
rel <- raster(paste(outM, "fireP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
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
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
tot <- tot * fbPerc
# Save output raster
writeRaster(tot, "fbCostP.tif",overwrite=T)

# REAL
scen <- "R"
rel <- raster(paste(outM, "fireR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
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
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
tot <- tot * fbPerc
# Save output raster
writeRaster(tot, "fbCostR.tif",overwrite=T)

# OPT
scen <- "O"
rel <- raster(paste(outM, "fireO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
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
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot
tot <- tot * fbPerc
# Save output raster
writeRaster(tot, "fbCostO.tif",overwrite=T)

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
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "nurseryFSAestab"       # Focal method (m)
scen <- "P"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haP
tot
writeRaster(tot, "neFSACostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "nurseryFSAestab"       # Focal method (m)
scen <- "R"        # Scenario
# Run cost calculation code
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haR
tot
writeRaster(tot, "neFSACostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "nurseryFSAestab"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haO
tot
writeRaster(tot, "neFSACostO.tif",overwrite=T)

### NURSERY MANAGEMENT

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "nurseryFSAmgmt"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haP
tot
writeRaster(tot, "nmFSACostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "nurseryFSAmgmt"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haR
tot
writeRaster(tot, "nmFSACostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "nurseryFSAmgmt"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)

# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haO
tot
writeRaster(tot, "nmFSACostO.tif",overwrite=T)

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
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "nurseryNseestab"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haP
tot
writeRaster(tot, "neNseCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "nurseryNseestab"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haR
tot
writeRaster(tot, "neNseCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "nurseryNseestab"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haO
tot
writeRaster(tot, "neNseCostO.tif",overwrite=T)

### NURSERY MANAGEMENT

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "nurseryNsemgmt"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haP
tot
writeRaster(tot, "nmNseCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "nurseryNsemgmt"       # Focal method (m)
scen <- "R"        # Scenarion
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haR
tot
writeRaster(tot, "nmNseCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "nurseryNsemgmt"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / haO
tot
writeRaster(tot, "nmNseCostO.tif",overwrite=T)

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
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "enrichFSA"   # Focal method (m)
scen <- "P"        # Scenario
# Run cost calculation code
source(costcalc)

# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
temp <- raster(paste(inDir, "floodplainMultP.tif", sep="/"))
labras <- labras * temp
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "enrichFSA"   # Focal method (m)
scen <- "R"        # Scenario
# Run cost calculation code
source(costcalc)

# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
temp <- raster(paste(inDir, "floodplainMultR.tif", sep="/"))
labras <- labras * temp
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "enrichFSA"   # Focal method (m)
scen <- "O"        # Scenario
# Run cost calculation code
source(costcalc)

# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
temp <- raster(paste(inDir, "floodplainMultO.tif", sep="/"))
labras <- labras * temp
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrCostO.tif",overwrite=T)

### FRAMEWORK SPECIES

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "fsa"       # Focal method (m)
scen <- "P"        # Scenario
# Run cost calculation code
source(costcalc)

# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
temp <- raster(paste(inDir, "floodplainMultP.tif", sep="/"))
labras <- labras * temp
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "fsa"       # Focal method (m)
scen <- "R"        # Scenario
# Run cost calculation code
source(costcalc)

# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
temp <- raster(paste(inDir, "floodplainMultR.tif", sep="/"))
labras <- labras * temp
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "fsa"       # Focal method (m)
scen <- "O"        # Scenario
# Run cost calculation code
source(costcalc)

# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
temp <- raster(paste(inDir, "floodplainMultO.tif", sep="/"))
labras <- labras * temp
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsCostO.tif",overwrite=T)

### NURSE TREES

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "nurse"       # Focal method (m)
scen <- "P"        # Scenario
# Run cost calculation code
source(costcalc)

# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
temp <- raster(paste(inDir, "floodplainMultP.tif", sep="/"))
labras <- labras * temp
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nsCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "nurse"       # Focal method (m)
scen <- "R"        # Scenario
# Run cost calculation code
source(costcalc)

# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
temp <- raster(paste(inDir, "floodplainMultR.tif", sep="/"))
labras <- labras * temp
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nsCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "nurse"       # Focal method (m)
scen <- "O"        # Scenario
# Run cost calculation code
source(costcalc)

# if LC = Floodplain(3), costs factored in: 200% (Pess), 150% (Real), 125% (Opt)
temp <- raster(paste(inDir, "floodplainMultO.tif", sep="/"))
labras <- labras * temp
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nsCostO.tif",overwrite=T)

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
# Could include condition to exclude areas where MaxD planting is employed in Ag Mosaic and/or within 3000km of roads (when deep ripping employed instead)
# PESS
rel <- raster(paste(outM, "soilP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "plough"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "phCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "soilR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "plough"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "phCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "soilO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "plough"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "phCostO.tif",overwrite=T)

### ORGANIC FERTILISER
# Relevance: All cases with soil improvement
# Could incl. condition to exclude areas where MaxD method is employed [topsoil replacement]
# and in AgMosaic where green mulching = cheaper and greater benefit to communities?)
# PESS
rel <- raster(paste(outM, "soilP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "fertilise"       # Focal method (m)
scen <- "P"        # Scenario
writeRaster(tot, "ftCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "soilR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "fertilise"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "ftCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "soilO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "fertilise"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "ftCostO.tif",overwrite=T)

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
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "enrichweed"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrweedCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "enrichweed"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrweedCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "enrichweed"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrweedCostO.tif",overwrite=T)

## ENRICH FERTILISER

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "enrichfertilise"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrfertCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "enrichfertilise"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrfertCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "enrichfertilise"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrfertCostO.tif",overwrite=T)

## ENRICH WATERING

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "enrichwater"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrwatCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "enrichwater"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrwatCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "enrichwater"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "enrwatCostO.tif",overwrite=T)

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
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "fsaweed"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsaweedCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "fsaweed"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsaweedCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "fsaweed"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsaweedCostO.tif",overwrite=T)

## FSA FERTILISER

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "fsafertilise"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsafertCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "fsafertilise"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsafertCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "fsafertilise"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsafertCostO.tif",overwrite=T)

## FSA WATERING

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "fsawater"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsawatCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "fsawater"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsawatCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "fsawater"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fsawatCostO.tif",overwrite=T)

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
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "nurseweed"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nurseweedCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "nurseweed"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nurseweedCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "nurseweed"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nurseweedCostO.tif",overwrite=T)

## NURSE FERTILISER

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "nursefertilise"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nursefertCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "nursefertilise"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nursefertCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "nursefertilise"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nursefertCostO.tif",overwrite=T)

## NURSE WATERING

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "nursewater"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nursewatCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "nursewater"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nursewatCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "nursewater"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nursewatCostO.tif",overwrite=T)

## NURSE THINNING

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "nursethin"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nursethinCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "nursethin"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nursethinCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "nursethin"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nursethinCostO.tif",overwrite=T)

## NURSE ENRICHMENT (ALONGSIDE THINNING)

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "nurseenr"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nurseenrCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "nurseenr"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nurseenrCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "nurseenr"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "nurseenrCostO.tif",overwrite=T)

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
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "firemgmt"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fmCostP.tif", overwrite=T)

# REAL
rel <- raster(paste(outA, "activeR.grd", sep="/"))
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "firemgmt"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fmCostR.tif", overwrite=T)

# OPT
rel <- raster(paste(outA, "activeO.grd", sep="/"))
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "firemgmt"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "fmCostO.tif", overwrite=T)

############################# POST PLANTING MONITORING #########################

### ENRICHMENT PLANTING MONITORING

### MONITORING Y1

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monEnry1"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monEnrY1CostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monEnry1"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monEnrY1CostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monEnry1"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monEnrY1CostO.tif",overwrite=T)

### MONITORING Y2 to canopy closure

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monEnry2"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monEnrY2CostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monEnry2"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monEnrY2CostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monEnry2"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monEnrY2CostO.tif",overwrite=T)

### MONITORING Continued to end of investment

# PESS
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monEnrcont"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monEnrContCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monEnrcont"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monEnrContCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monEnrcont"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monEnrContCostO.tif",overwrite=T)

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
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monFSAy1"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monFSAY1CostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monFSAy1"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monFSAY1CostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monFSAy1"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monFSAY1CostO.tif",overwrite=T)

### MONITORING Y2 to canopy closure

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monFSAy2"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monFSAY2CostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monFSAy2"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monFSAY2CostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monFSAy2"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monFSAY2CostO.tif",overwrite=T)

### MONITORING Continued to end of investment

# PESS
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monFSAcont"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monFSAContCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monFSAcont"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monFSAContCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monFSAcont"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monFSAContCostO.tif",overwrite=T)

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
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monNsey1"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monNseY1CostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monNsey1"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monNseY1CostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monNsey1"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monNseY1CostO.tif",overwrite=T)

### MONITORING Y2 to canopy closure

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monNsey2"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monNseY2CostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monNsey2"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monNseY2CostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monNsey2"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monNseY2CostO.tif",overwrite=T)

### MONITORING Continued to end of investment

# PESS
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monNsecont"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monNseContCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monNsecont"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monNseContCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monNsecont"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monNseContCostO.tif",overwrite=T)

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
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monANRy1"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monANRY1CostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outA, "anrR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monANRy1"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monANRY1CostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outA, "anrO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monANRy1"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monANRY1CostO.tif",overwrite=T)

### MONITORING Y2 to canopy closure

# PESS
rel <- raster(paste(outA, "anrP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monANRy2"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monANRY2CostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outA, "anrR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monANRy2"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monANRY2CostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outA, "anrO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monANRy2"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monANRY2CostO.tif",overwrite=T)

### MONITORING Continued to end of investment

# PESS
rel <- raster(paste(outA, "anrP.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "monANRcont"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monANRContCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outA, "anrR.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "monANRcont"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monANRContCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outA, "anrO.grd", sep="/")) # Methods raster where relevant pixels == 1
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "monANRcont"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot
# Save output raster:
writeRaster(tot, "monANRContCostO.tif",overwrite=T)

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
# Community Engagement costs estimates per village
# Pessimistic = 50ha; Realistic = 100ha; Optimistic = 1000ha

# PESS # Land Use Planning
rel <- raster(paste(outM, "commengP.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "lup"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haP
# Save output raster:
writeRaster(tot, "lupCostP.tif",overwrite=T)

# REAL # Land Use Planning
rel <- raster(paste(outM, "commengR.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "lup"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haR
# Save output raster:
writeRaster(tot, "lupCostR.tif",overwrite=T)

# OPT # Land Use Planning
rel <- raster(paste(outM, "commengO.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "lup"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haO
# Save output raster:
writeRaster(tot, "lupCostO.tif",overwrite=T)

# PESS # Sustainability start-up
rel <- raster(paste(outM, "commengP.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "sust"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haP
# Save output raster:
writeRaster(tot, "sustCostP.tif",overwrite=T)

# REAL # Sustainability start-up
rel <- raster(paste(outM, "commengR.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "sust"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haR
# Save output raster:
writeRaster(tot, "sustCostR.tif",overwrite=T)

# OPT # Sustainability start-up
rel <- raster(paste(outM, "commengO.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "sust"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haO
# Save output raster:
writeRaster(tot, "sustCostO.tif",overwrite=T)

# PESS # Annual progress workshop (year 1)
rel <- raster(paste(outM, "commengP.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "wshop1"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haP
# Save output raster:
writeRaster(tot, "wshopy1CostP.tif",overwrite=T)

# REAL # Annual progress workshop (year 1)
rel <- raster(paste(outM, "commengR.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "wshop1"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haR
# Save output raster:
writeRaster(tot, "wshopy1CostR.tif",overwrite=T)

# OPT # Annual progress workshop (year 1)
rel <- raster(paste(outM, "commengO.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "wshop1"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haO
# Save output raster:
writeRaster(tot, "wshopy1CostO.tif",overwrite=T)

# PESS # Annual progress workshop (continued)
rel <- raster(paste(outM, "commengP.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "wshop2"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haP
# Save output raster:
writeRaster(tot, "wshopcontCostP.tif",overwrite=T)

# REAL # Annual progress workshop (continued)
rel <- raster(paste(outM, "commengR.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "wshop2"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haR
# Save output raster:
writeRaster(tot, "wshopcontCostR.tif",overwrite=T)

# OPT # Annual progress workshop (continued)
rel <- raster(paste(outM, "commengO.grd", sep="/"))
rel[rel==NA] <- 0
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "wshop2"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- rel * tot / haO
# Save output raster:
writeRaster(tot, "wshopcontCostO.tif",overwrite=T)

############################# STAFF ############################################

# PROJECT MANAGER YEAR 1-5
# PESS
rel <- raster(paste(outA, "restoreP.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "managerMultP.tif", sep="/"))
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "staffy1mgmt"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy1mCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outA, "restoreR.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "managerMultR.tif", sep="/"))
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "staffy1mgmt"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy1mCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outA, "restoreO.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "managerMultO.tif", sep="/"))
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "staffy1mgmt"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy1mCostO.tif",overwrite=T)

# PROJECT FORESTER YEAR 1-5
# PESS
rel <- raster(paste(outA, "restoreP.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "foresterMultP.tif", sep="/"))
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "staffy1for"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy1forCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outA, "restoreR.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "foresterMultR.tif", sep="/"))
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "staffy1for"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy1forCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outA, "restoreO.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "foresterMultO.tif", sep="/"))
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "staffy1for"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy1forCostO.tif",overwrite=T)

# COMMUNITY ENGAGEMENT OFFICER YEAR 1-5
# PESS
rel <- raster(paste(outA, "restoreP.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultP.tif", sep="/"))
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "staffy1ceng"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy1ceCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outA, "restoreR.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultR.tif", sep="/"))
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "staffy1ceng"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy1ceCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outA, "restoreO.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultO.tif", sep="/"))
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "staffy1ceng"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy1ceCostO.tif",overwrite=T)

# PROJECT MANAGER YEAR 5 ONWARD
# PESS
rel <- raster(paste(outA, "restoreP.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "managerMultP.tif", sep="/"))
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "staffy5mgmt"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy5mCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outA, "restoreR.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "managerMultR.tif", sep="/"))
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "staffy5mgmt"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy5mCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outA, "restoreO.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "managerMultO.tif", sep="/"))
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "staffy5mgmt"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy5mCostO.tif",overwrite=T)

# PROJECT FORESTER YEAR 5 ONWARD
# PESS
rel <- raster(paste(outA, "restoreP.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "foresterMultP.tif", sep="/"))
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "staffy5for"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy5forCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outA, "restoreR.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "foresterMultR.tif", sep="/"))
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "staffy5for"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy5forCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outA, "restoreO.grd", sep="/")) # Methods raster where relevant pixels == 1
mult <- raster(paste(inDir, "foresterMultO.tif", sep="/"))
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "staffy5for"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy5forCostO.tif",overwrite=T)

# COMMUNITY ENGAGEMENT OFFICER YEAR 5 ONWARD
# PESS
rel <- raster(paste(outA, "restoreP.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultP.tif", sep="/"))
dur <- mask(durP, rel)
# Create additional vectors needed:
m <- "staffy5ceng"       # Focal method (m)
scen <- "P"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy5ceCostP.tif",overwrite=T)

# REAL
rel <- raster(paste(outA, "restoreR.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultR.tif", sep="/"))
dur <- mask(durR, rel)
# Create additional vectors needed:
m <- "staffy5ceng"       # Focal method (m)
scen <- "R"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy5ceCostR.tif",overwrite=T)

# OPT
rel <- raster(paste(outA, "restoreO.grd", sep="/")) # Methods raster where relevant pixels == 1
rel[rel==NA] <- 0
mult <- raster(paste(inDir, "commengMultO.tif", sep="/"))
dur <- mask(durO, rel)
# Create additional vectors needed:
m <- "staffy5ceng"       # Focal method (m)
scen <- "O"        # Scenario
## Cost Calc #
source(costcalc)
# Sum labour and equipment costs
tot <- labras + eqpras
tot <- tot / mult
# Save output raster:
writeRaster(tot, "staffy5ceCostO.tif",overwrite=T)

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
writeRaster(tot, "anrforCostP.tif", overwrite=T)

# REAL
r1 <- raster("vnCostR.tif")
r2 <- raster("sbCostR.tif")
r3 <- raster("ltCostR.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "anrforCostR.tif", overwrite=T)

# OPT
r1 <- raster("vnCostO.tif")
r2 <- raster("sbCostO.tif")
r3 <- raster("ltCostO.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "anrforCostO.tif", overwrite=T)

### SUM OF OTHER ANR COSTS IN SAVANNA / AGMOSAIC ###############################

# PESS
r1 <- raster("gsCostP.tif")
r2 <- raster("fbCostP.tif")
rstack <- stack(r1, r2)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "anrsavagCostP.tif", overwrite=T)

# REAL
r1 <- raster("gsCostR.tif")
r2 <- raster("fbCostR.tif")
rstack <- stack(r1, r2)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "anrsavagCostR.tif", overwrite=T)

# OPT
r1 <- raster("gsCostO.tif")
r2 <- raster("fbCostO.tif")
rstack <- stack(r1, r2)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
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
writeRaster(tot, "monCostP.tif",overwrite=T)

# REAL
r1 <- raster("monEnrCostR.tif")
r2 <- raster("monFSACostR.tif")
r3 <- raster("monNseCostR.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "monCostR.tif",overwrite=T)

# OPT
r1 <- raster("monEnrCostO.tif")
r2 <- raster("monFSACostO.tif")
r3 <- raster("monNseCostO.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "monCostO.tif",overwrite=T)

### TOTAL COST FOR TREE PLANTING METHODS (PLANTING, MANAGEMENT AND MONITORING)

# PESS
r1 <- raster("treeplCostP.tif")
r2 <- raster("treemCostP.tif")
r3 <- raster("monCostP.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treesTotCostP.tif", overwrite=T)

# REAL
r1 <- raster("treeplCostR.tif")
r2 <- raster("treemCostR.tif")
r3 <- raster("monCostR.tif")
rstack <- stack(r1, r2, r3)
rstack
tot <- sum(rstack, na.rm=TRUE)
tot
writeRaster(tot, "treesTotCostR.tif", overwrite=T)

# OPT
r1 <- raster("treeplCostO.tif")
r2 <- raster("treemCostO.tif")
r3 <- raster("monCostO.tif")
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

