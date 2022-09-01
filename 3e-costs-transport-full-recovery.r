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
################## FULL RECOVERY TRANSPORT COSTS CALCULATION ###################                                <- FULL RECOVERY TRANSPORT COSTS CALCULATION

# This code calculates the total transport costs incurred for each restoration
# pixel to the point of full AGB recovery, based on:
# method (porters required? etc), location (road / off road transport? camping needeed?)
# number of treatments per year, number of years of intervention, etc.

### Set input work directory
setwd(inDir)

##### CALL IN RESTORATION COSTS TABLE / CSV:
### Incl. 3 columns (Pess, Real, Opt) each for: # Activity Cost (costP,R,O)
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

# Read in rasters containing number of years to full recovery for each pixel under the different scenarios
# Generated using code: agbgain-max-calc-burnal
durP <- raster(paste(outG, "years-to-recovery-P.tif", sep="/"))
durR <- raster(paste(outG, "years-to-recovery-R.tif", sep="/"))
durO <- raster(paste(outG, "years-to-recovery-O.tif", sep="/"))

# Read in additional rasters needed: (determinants of costs)
land <- raster("landprices.tif")
priceha <- raster("landprices_nursery.tif")
ext <- raster("Extent_1.tif") # Extent raster where all pixels==1 for multiplying by gov, thus converting gov to raster in cases where it isn't already

# Define transport input directory:
setwd(outT)

# Read in base rasters which include costs of 1 return trip to each pixel within the landscape
# Generated using code: costs-tspt-input-rasts
# On-road transport options:
busP <- raster("bus_onrdRtnTripP.tif") # Raster: transport costs of 1 trip per pixel
busR <- raster("bus_onrdRtnTripR.tif")
busO <- raster("bus_onrdRtnTripO.tif")
pwthP <- raster("pwthire_onrdRtnTripP.tif")
pwthR <- raster("pwthire_onrdRtnTripR.tif")
pwthO <- raster("pwthire_onrdRtnTripO.tif")
mbkhP <- raster("mbikehire_onrdRtnTripP.tif")
mbkhR <- raster("mbikehire_onrdRtnTripR.tif")
mbkhO <- raster("mbikehire_onrdRtnTripO.tif")
pwtoP <- raster("pwtown_onrdRtnTripP.tif")
pwtoR <- raster("pwtown_onrdRtnTripR.tif")
pwtoO <- raster("pwtown_onrdRtnTripO.tif")
mbkoP <- raster("mbikeown_onrdRtnTripP.tif")
mbkoR <- raster("mbikeown_onrdRtnTripR.tif")
mbkoO <- raster("mbikeown_onrdRtnTripO.tif")
truckP <- raster("truck_onrdRtnTripP.tif")
truckR <- raster("truck_onrdRtnTripR.tif")
truckO <- raster("truck_onrdRtnTripO.tif")

# Off-road transport options:
porterP <- raster("porter_offrdRtnTripP.tif")
porterR <- raster("porter_offrdRtnTripR.tif")
porterO <- raster("porter_offrdRtnTripO.tif")
govP <- raster("porter_govRtnTripP.tif")
govR <- raster("porter_govRtnTripR.tif")
govO <- raster("porter_govRtnTripO.tif")
walkP <- raster("walk_offrdRtnTripP.tif")
walkR <- raster("walk_offrdRtnTripR.tif")
walkO <- raster("walk_offrdRtnTripO.tif")

# Set inflation rate for calculating future costs
infl <- 0.022      # Inflation rate
haP <- 50          # Area (ha) restored (pessimistic)
haR <- 100         # Area (ha) restored (realistic)
haO <- 1000        # Area (ha) restored (optimistic)
wdptP <- 6         # Remote work max duration (Remote work days per trip; pessimistic = 1 week)
wdptR <- 12        # Remote work max duration (realistic = 2 weeks)
wdptO <- 24        # Remote work max duration (optimistic = 4 weeks)

# Set output directory (folder in which outputs will be saved):
setwd(outCRT)

################################################################################
############ CALCULATE TRANSPORT COSTS FOR EACH RESTORATION METHOD #############

                #################################################
                ################## ANR METHODS ##################
                #################################################
                      ##### ON & OFF ROAD TRANSPORT #####
                      ###################################

# Public bus is taken to be the most cost-effective mode of on-road transport
# When there is no additional cost of transporting equipment, seedlings, etc.
# Assume only able to access areas >3km from agmosaic / road by walking on foot
# And/or in the case of tree planting, paying porters to walk on foot.

##### VINES ###############
m <- "vines"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

# Methods raster where pixels closest to agmosaic==1, those closest to road==2
rel <- raster(paste(outR, "prox-agrd-vinesP.tif", sep="/"))
ext <- mask(ext, rel)
dur <- mask(durP, rel)

# Generate raster with costs per 1 trip of appropriate transport method per pixel
r1 <- rel
r2 <- rel
# Create raster that only has pixels closest to agmosaic:
r1[r1>1] <- NA
# Create raster that only has pixels closest to roads:
r2[r2<2] <- NA
# Apply walking cost to pixels closest to agmosiac:
r1 <- mask(walkP, r1)
# Create temporary raster that sums walking costs and bus costs:
temp <- sum(walkP, busP, na.rm=T)
# Apply walking and bus costs to pixels closest to roads:
r2 <- mask(temp, r2)
# Combine above rasters with transport costs per trip for pixels closest to agmosaic (walking only) and roads (walking and bus):
r <- sum(r1, r2, na.rm=T)
# Mask extent raster by relevant pixels to method:
ext <- mask(ext, rel)

# Create vectors for running cost calculation code
# Vector of values from years to recovery raster (calculated using code: agbgain-max-calc-burnal)
durvec <- as.data.frame(getValues(dur))
names(durvec)[1] <- "years"
# Empty vectors of same length as follows:
tsptvec <- rep(NA, length(durvec$years))     # Output vector for labourer transport costs
govvec <-  rep(NA, length(durvec$years))     # Output vector for government official transport costs
tspteqp <- rep(NA, length(durvec$years))     # Output vector for transport-related equipment/assets costs

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

# Divide transport cost raster by area (ha) being restored:
r2 <- r2 / ha
# Mask by pixels relevant to this method:
r2 <- mask(r2, rel)
r2

# Save output raster:
writeRaster(r2, "vines_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-vinesR.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "vines_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-vinesO.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkO, r1)
temp <- sum(walkO, busO, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "vines_tsptTotO.tif", overwrite=T)

##### HERBS/SHRUBS ###############
m <- "shrub"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-shrubP.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkP, r1)
temp <- sum(walkP, busP, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "shrub_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-shrubR.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "shrub_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-shrubO.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkO, r1)
temp <- sum(walkO, busO, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "shrub_tsptTotO.tif", overwrite=T)

##### LANTANA ###############
m <- "lantana"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-lantP.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkP, r1)
temp <- sum(walkP, busP, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "lant_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-lantR.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "lant_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-lantO.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkO, r1)
temp <- sum(walkO, busO, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "lant_tsptTotO.tif", overwrite=T)

##### GRASS ###############
m <- "grass"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-grassP.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkP, r1)
temp <- sum(walkP, busP, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "grass_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-grassR.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "grass_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-grassO.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkO, r1)
temp <- sum(walkO, busO, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "grass_tsptTotO.tif", overwrite=T)

##### FIREBREAKS ###############
m <- "firebreaks"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-fireP.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkP, r1)
temp <- sum(walkP, busP, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fireb_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-fireR.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fireb_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-fireO.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkO, r1)
temp <- sum(walkO, busO, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fireb_tsptTotO.tif", overwrite=T)

##### SOIL ###############
m <- "plough"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-soilP.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkP, r1)
temp <- sum(walkP, busP, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "soil_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-soilR.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "soil_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-soilO.tif", sep="/")) # Methods raster where pixels closest to agmosaic==1, those closest to road==2
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkO, r1)
temp <- sum(walkO, busO, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "soil_tsptTotO.tif", overwrite=T)

                #################################################
                ############ TREE PLANTING METHODS ##############
                #################################################
                      ######## ON-ROAD TRANSPORT ########
                      ###################################

# Public bus is taken to be the most cost-effective mode of on-road transport
# When there is no additional cost of transporting equipment, seedlings, etc.
# For rational behind choice of transport method when transporting seedlings, see RestorationCosts spreadsheet [TransportMethod tab]
# Most cost-effective method for all tree planting methods under the various scenarios were:
# P = Powertiller hire; R = Truck hire; O = Truck hire

##### PESSIMISTIC ##############################################################
scen <- "P"
ha <- haP
wdpt <- wdptP

##### ENRICHMENT PLANTING : POWERTILLER HIRE ###################################
m <- "enrichFSA"
tm <- "pwtillerhire"
rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- pwthP # Raster: transport costs of 1 trip per pixel
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enr_onrdTotP.tif", overwrite=T)

##### FSA : POWERTILLER HIRE ###################################################
m <- "fsa"
tm <- "pwtillerhire"
rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- pwthP
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsa_onrdTotP.tif", overwrite=T)

##### NURSE TREES : POWERTILLER HIRE ###########################################
m <- "nurse"
tm <- "pwtillerhire"
rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- pwthP
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nse_onrdTotP.tif", overwrite=T)


##### REALISTIC ################################################################
scen <- "R"
ha <- haR
wdpt <- wdptR

##### ENRICHMENT PLANTING : TRUCK HIRE #########################################
m <- "enrichFSA"
tm <- "truckhire"
rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- truckR # Raster: transport costs of 1 trip per pixel
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enr_onrdTotR.tif", overwrite=T)

##### FSA : TRUCK HIRE #########################################################
m <- "fsa"
tm <- "truckhire"
rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- truckR # Raster: transport costs of 1 trip per pixel
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsa_onrdTotR.tif", overwrite=T)

##### NURSE TREES : TRUCK HIRE ###########################################
m <- "nurse"
tm <- "truckhire"
rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- pwthP
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nse_onrdTotR.tif", overwrite=T)


##### OPTIMISTIC ################################################################
scen <- "O"
ha <- haO
wdpt <- wdptO

##### ENRICHMENT PLANTING : TRUCK HIRE #########################################
m <- "enrichFSA"
tm <- "truckhire"
rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- truckO # Raster: transport costs of 1 trip per pixel
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enr_onrdTotO.tif", overwrite=T)

##### FSA : TRUCK HIRE #########################################################
m <- "fsa"
tm <- "truckhire"
rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- truckO # Raster: transport costs of 1 trip per pixel
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsa_onrdTotO.tif", overwrite=T)

##### NURSE TREES : TRUCK HIRE ###########################################
m <- "nurse"
tm <- "truckhire"
rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- truckO
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nse_onrdTotO.tif", overwrite=T)

                #################################################
                ############ TREE PLANTING METHODS ##############
                #################################################
                      ######## OFF-ROAD TRANSPORT ########
                      ####################################

##### ENRICHMENT PLANTING : PORTER
m <- "enrichFSA"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outM, "enrichP.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterP
gov <- govP
r <- mask(r, rel)
gov <- mask(gov, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enr_offrdTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outM, "enrichR.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterR
gov <- govR
r <- mask(r, rel)
gov <- mask(gov, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enr_offrdTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outM, "enrichO.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterO
gov <- govO
r <- mask(r, rel)
gov <- mask(gov, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enr_offrdTotO.tif", overwrite=T)

##### FSA : PORTER
m <- "fsa"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outM, "fsaP.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterP
gov <- govP
r <- mask(r, rel)
gov <- mask(gov, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsa_offrdTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outM, "fsaR.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterR
gov <- govR
r <- mask(r, rel)
gov <- mask(gov, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsa_offrdTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outM, "fsaO.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterO
gov <- govO
r <- mask(r, rel)
gov <- mask(gov, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsa_offrdTotO.tif", overwrite=T)

##### NURSE TREES : PORTER
m <- "nurse"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterP
gov <- govP
r <- mask(r, rel)
gov <- mask(gov, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nse_offrdTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterR
gov <- govR
r <- mask(r, rel)
gov <- mask(gov, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nse_offrdTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterO
gov <- govO
r <- mask(r, rel)
gov <- mask(gov, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nse_offrdTotO.tif", overwrite=T)

# ENRICHMENT PLANTING(NURSE TREES)
m <- "nurseenr"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outM, "nurseP.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterP
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nurseenr_offrdTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outM, "nurseR.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterR
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nurseenr_offrdTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outM, "nurseO.grd", sep="/")) # Methods raster where relevant pixels == 1
r <- porterO
r <- mask(r, rel)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nurseenr_offrdTotO.tif", overwrite=T)

################################################################################

                #################################################
                ####### POST PLANTING MANAGEMENT METHODS ########
                #################################################
                      ##### ON & OFF ROAD TRANSPORT #####
                      ###################################

##### ENRICHMENT PLANTING ######################################################

# WEEDING & FERTILISING ########################################################
m <- "enrichweed"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

# Read in methods raster where pixels closest to agmosaic==1, those closest to road==2:
rel <- raster(paste(outR, "prox-agrd-enrichP.tif", sep="/"))
r1 <- rel
r2 <- rel
# Create raster that only has pixels closest to agmosaic:
r1[r1>1] <- NA
# Create raster that only has pixels closest to roads:
r2[r2<2] <- NA
# Apply walking cost to pixels closest to agmosiac:
r1 <- mask(walkP, r1)
# Create temporary raster that sums walking costs and bus costs:
temp <- sum(walkP, busP, na.rm=T)
# Apply walking and bus costs to pixels closest to roads:
r2 <- mask(temp, r2)
# Combine above rasters with transport costs per trip for pixels closest to agmosaic (walking only) and roads (walking and bus):
r <- sum(r1, r2, na.rm=T)
# Mask extent raster by relevant pixels to method:
ext <- mask(ext, rel)

# Create vectors for running cost calculation code
# Vector of values from years to recovery raster (calculated using code: agbgain-max-calc-burnal)
durvec <- as.data.frame(getValues(dur))
names(durvec)[1] <- "years"
# Empty vectors of same length as follows:
tsptvec <- rep(NA, length(durvec$years))     # Output vector for labourer transport costs
govvec <-  rep(NA, length(durvec$years))     # Output vector for government official transport costs
tspteqp <- rep(NA, length(durvec$years))     # Output vector for transport-related equipment/assets costs

## Transport Cost Calc # ~~~~~~~~~~~
# Based on cost per return trip, this code calculates the total tranport cost per method,
# by factoring in the number of trips (determined by team size or number of stems),
# repeats, years, intervals between years, and equipment costs.
source(tcostcalc)

# Divide transport cost raster by area (ha) being restored:
r2 <- r2 / ha
# Mask by pixels relevant to this method:
r2 <- mask(r2, rel)
r2
# Save output raster:
writeRaster(r2, "enrweed_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-enrichR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enrweed_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-enrichO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enrweed_tsptTotO.tif", overwrite=T)

# WATERING
m <- "enrichwater"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-enrichP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enrwater_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-enrichR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enrwater_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-enrichO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "enrwater_tsptTotO.tif", overwrite=T)

## FSA PLANTING

# WEEDING  & FERTILISING
m <- "fsaweed"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-fsaP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsaweed_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-fsaR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsaweed_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-fsaO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsaweed_tsptTotO.tif", overwrite=T)

# WATERING
m <- "fsawater"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-fsaP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsawater_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-fsaR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsawater_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-fsaO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "fsawater_tsptTotO.tif", overwrite=T)

## NURSE PLANTING

# WEEDING  & FERTILISING
m <- "nurseweed"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-nurseP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nurseweed_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-nurseR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nurseweed_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-nurseO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nurseweed_tsptTotO.tif", overwrite=T)

# WATERING
m <- "nursewater"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-nurseP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nursewater_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-nurseR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nursewater_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-nurseO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nursewater_tsptTotO.tif", overwrite=T)

# THINNING (NURSE TREES)
m <- "nursethin"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-nurseP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nursethin_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-nurseR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nursethin_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-nurseO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nursethin_tsptTotO.tif", overwrite=T)

# ENRICHMENT PLANTING(NURSE TREES)
m <- "nurseenr"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-nurseP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nurseenr_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-nurseR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nurseenr_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-nurseO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "nurseenr_tsptTotO.tif", overwrite=T)

### POST PLANTING MONITORING

## ENRICHMENT PLANTING

# MONITORING Y1
m <- "monEnry1"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-enrichP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monEnry1_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-enrichR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monEnry1_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-enrichO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monEnry1_tsptTotO.tif", overwrite=T)

# MONITORING Y2 until canopy recapture
m <- "monEnry2"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-enrichP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monEnry2_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-enrichR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monEnry2_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-enrichO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monEnry2_tsptTotO.tif", overwrite=T)

# MONITORING Continued until end of investment
m <- "monEnrcont"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-enrichP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monEnrcont_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-enrichR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monEnrcont_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-enrichO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monEnrcont_tsptTotO.tif", overwrite=T)

## FSA PLANTING

# MONITORING Y1
m <- "monFSAy1"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-fsaP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monFSAy1_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-fsaR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monFSAy1_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-fsaO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monFSAy1_tsptTotO.tif", overwrite=T)

# MONITORING Y2 until canopy recapture
m <- "monFSAy2"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-fsaP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monFSAy2_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-fsaR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monFSAy2_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-fsaO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monFSAy2_tsptTotO.tif", overwrite=T)

# MONITORING Until end of investment
m <- "monFSAcont"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-fsaP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monFSAcont_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-fsaR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monFSAcont_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-fsaO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monFSAcont_tsptTotO.tif", overwrite=T)

## NURSE PLANTING

# MONITORING Y1
m <- "monNsey1"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-nurseP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monNsey1_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-nurseR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monNsey1_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-nurseO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monNsey1_tsptTotO.tif", overwrite=T)

# MONITORING Y2 until canopy recapture
m <- "monNsey2"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-nurseP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monNsey2_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-nurseR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monNsey2_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-nurseO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monNsey2_tsptTotO.tif", overwrite=T)

# MONITORING Until end of investment
m <- "monNsecont"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

rel <- raster(paste(outR, "prox-agrd-nurseP.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monNsecont_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-nurseR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monNsecont_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-nurseO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monNsecont_tsptTotO.tif", overwrite=T)

################################################################################
################################## TOTALS ######################################
################################################################################

### ENRICHMENT PLANTING
## PESS
# Monitoring transport
r1 <- raster("monEnry1_tsptTotP.tif")
r2 <- raster("monEnry2_tsptTotP.tif")
r3 <- raster("monEnrcont_tsptTotP.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monEnr_tsptTotP.tif",overwrite=T)

# Management transport
r1 <- raster("enrweed_tsptTotP.tif")
r2 <- raster("enrwater_tsptTotP.tif")
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
writeRaster(r, "mgmtEnr_tsptTotP.tif",overwrite=T)

## REAL
# Monitoring transport
r1 <- raster("monEnry1_tsptTotR.tif")
r2 <- raster("monEnry2_tsptTotR.tif")
r3 <- raster("monEnrcont_tsptTotR.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monEnr_tsptTotR.tif",overwrite=T)

# Management transport
r1 <- raster("enrweed_tsptTotR.tif")
r2 <- raster("enrwater_tsptTotR.tif")
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
writeRaster(r, "mgmtEnr_tsptTotR.tif",overwrite=T)

## OPT
# Monitoring transport
r1 <- raster("monEnry1_tsptTotO.tif")
r2 <- raster("monEnry2_tsptTotO.tif")
r3 <- raster("monEnrcont_tsptTotO.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monEnr_tsptTotO.tif",overwrite=T)

# Management transport
r1 <- raster("enrweed_tsptTotO.tif")
r2 <- raster("enrwater_tsptTotO.tif")
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
writeRaster(r, "mgmtEnr_tsptTotO.tif",overwrite=T)

### FSA PLANTING
## PESS
# Monitoring transport
r1 <- raster("monFSAy1_tsptTotP.tif")
r2 <- raster("monFSAy2_tsptTotP.tif")
r3 <- raster("monFSAcont_tsptTotP.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monFSA_tsptTotP.tif",overwrite=T)

# Management transport
r1 <- raster("fsaweed_tsptTotP.tif")
r2 <- raster("fsawater_tsptTotP.tif")
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
writeRaster(r, "mgmtFSA_tsptTotP.tif",overwrite=T)

## REAL
# Monitoring transport
r1 <- raster("monFSAy1_tsptTotR.tif")
r2 <- raster("monFSAy2_tsptTotR.tif")
r3 <- raster("monFSAcont_tsptTotR.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monFSA_tsptTotR.tif",overwrite=T)

# Management transport
r1 <- raster("fsaweed_tsptTotR.tif")
r2 <- raster("fsawater_tsptTotR.tif")
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
writeRaster(r, "mgmtFSA_tsptTotR.tif",overwrite=T)

## OPT
# Monitoring transport
r1 <- raster("monFSAy1_tsptTotO.tif")
r2 <- raster("monFSAy2_tsptTotO.tif")
r3 <- raster("monFSAcont_tsptTotO.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monFSA_tsptTotO.tif",overwrite=T)

# Management transport
r1 <- raster("fsaweed_tsptTotO.tif")
r2 <- raster("fsawater_tsptTotO.tif")
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
writeRaster(r, "mgmtFSA_tsptTotO.tif",overwrite=T)

### NURSE TREES
## PESS
# Monitoring transport
r1 <- raster("monNsey1_tsptTotP.tif")
r2 <- raster("monNsey2_tsptTotP.tif")
r3 <- raster("monNsecont_tsptTotP.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monNse_tsptTotP.tif",overwrite=T)

# Management transport
r1 <- raster("nurseweed_tsptTotP.tif")
r2 <- raster("nursewater_tsptTotP.tif")
r3 <- raster("nursethin_tsptTotP.tif")
r4 <- raster("nurseenr_tsptTotP.tif")
st <- stack(r1, r2, r3, r4)
r <- sum(st, na.rm=T)
writeRaster(r, "mgmtNse_tsptTotP.tif",overwrite=T)

## REAL
# Monitoring transport
r1 <- raster("monNsey1_tsptTotR.tif")
r2 <- raster("monNsey2_tsptTotR.tif")
r3 <- raster("monNsecont_tsptTotR.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monNse_tsptTotR.tif",overwrite=T)

# Management transport
r1 <- raster("nurseweed_tsptTotR.tif")
r2 <- raster("nursewater_tsptTotR.tif")
r3 <- raster("nursethin_tsptTotR.tif")
r4 <- raster("nurseenr_tsptTotR.tif")
st <- stack(r1, r2, r3, r4)
r <- sum(st, na.rm=T)
writeRaster(r, "mgmtNse_tsptTotR.tif",overwrite=T)

## OPT
# Monitoring transport
r1 <- raster("monNsey1_tsptTotO.tif")
r2 <- raster("monNsey2_tsptTotO.tif")
r3 <- raster("monNsecont_tsptTotO.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monNse_tsptTotO.tif",overwrite=T)

# Management transport
r1 <- raster("nurseweed_tsptTotO.tif")
r2 <- raster("nursewater_tsptTotO.tif")
r3 <- raster("nursethin_tsptTotO.tif")
r4 <- raster("nurseenr_tsptTotO.tif")
st <- stack(r1, r2, r3, r4)
r <- sum(st, na.rm=T)
writeRaster(r, "mgmtNse_tsptTotO.tif",overwrite=T)

################################################################################
##### ANR MONITORING ###########################################################
################################################################################

##### MONITORING Y1 #####
m <- "monANRy1"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

# Read in methods raster where pixels closest to agmosaic==1, those closest to road==2:
rel <- raster(paste(outR, "prox-agrd-anrP.tif", sep="/"))
r1 <- rel
r2 <- rel
# Create raster that only has pixels closest to agmosaic:
r1[r1>1] <- NA
# Create raster that only has pixels closest to roads:
r2[r2<2] <- NA
# Apply walking cost to pixels closest to agmosiac:
r1 <- mask(walkP, r1)
# Create temporary raster that sums walking costs and bus costs:
temp <- sum(walkP, busP, na.rm=T)
# Apply walking and bus costs to pixels closest to roads:
r2 <- mask(temp, r2)
# Combine above rasters with transport costs per trip for pixels closest to agmosaic (walking only) and roads (walking and bus):
r <- sum(r1, r2, na.rm=T)
# Mask by study area extent:
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
# Based on cost per return trip, this code calculates the total tranport cost per method,
# by factoring in the number of trips (determined by team size or number of stems),
# repeats, years, intervals between years, and equipment costs.
source(tcostcalc)

# Divide by area (ha) being restored:
r2 <- r2 / ha
# Mask by pixels relevant to this method:
r2 <- mask(r2, rel)
# Save output raster:
writeRaster(r2, "monANRy1_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-anrR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monANRy1_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-anrO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkO, r1)
temp <- sum(walkO, busO, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)

ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monANRy1_tsptTotO.tif", overwrite=T)

##### MONITORING Y2 #####
m <- "monANRy2"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

# Read in methods raster where pixels closest to agmosaic==1, those closest to road==2:
rel <- raster(paste(outR, "prox-agrd-anrP.tif", sep="/"))
r1 <- rel
r2 <- rel
# Create raster that only has pixels closest to agmosaic:
r1[r1>1] <- NA
# Create raster that only has pixels closest to roads:
r2[r2<2] <- NA
# Apply walking cost to pixels closest to agmosiac:
r1 <- mask(walkP, r1)
# Create temporary raster that sums walking costs and bus costs:
temp <- sum(walkP, busP, na.rm=T)
# Apply walking and bus costs to pixels closest to roads:
r2 <- mask(temp, r2)
# Combine above rasters with transport costs per trip for pixels closest to agmosaic (walking only) and roads (walking and bus):
r <- sum(r1, r2, na.rm=T)
# Mask by study area extent:
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
# Based on cost per return trip, this code calculates the total tranport cost per method,
# by factoring in the number of trips (determined by team size or number of stems),
# repeats, years, intervals between years, and equipment costs.
source(tcostcalc)

# Divide by area (ha) being restored:
r2 <- r2 / ha
# Mask by pixels relevant to this method:
r2 <- mask(r2, rel)
# Save output raster:
writeRaster(r2, "monANRy2_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-anrR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monANRy2_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-anrO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkO, r1)
temp <- sum(walkO, busO, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)

ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monANRy2_tsptTotO.tif", overwrite=T)

##### MONITORING CONT #####
m <- "monANRcont"
tm <- "porter"

##### PESS
scen <- "P"
ha <- haP
wdpt <- wdptP

# Read in methods raster where pixels closest to agmosaic==1, those closest to road==2:
rel <- raster(paste(outR, "prox-agrd-anrP.tif", sep="/"))
r1 <- rel
r2 <- rel
# Create raster that only has pixels closest to agmosaic:
r1[r1>1] <- NA
# Create raster that only has pixels closest to roads:
r2[r2<2] <- NA
# Apply walking cost to pixels closest to agmosiac:
r1 <- mask(walkP, r1)
# Create temporary raster that sums walking costs and bus costs:
temp <- sum(walkP, busP, na.rm=T)
# Apply walking and bus costs to pixels closest to roads:
r2 <- mask(temp, r2)
# Combine above rasters with transport costs per trip for pixels closest to agmosaic (walking only) and roads (walking and bus):
r <- sum(r1, r2, na.rm=T)
# Mask by study area extent:
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
# Based on cost per return trip, this code calculates the total tranport cost per method,
# by factoring in the number of trips (determined by team size or number of stems),
# repeats, years, intervals between years, and equipment costs.
source(tcostcalc)

# Divide by area (ha) being restored:
r2 <- r2 / ha
# Mask by pixels relevant to this method:
r2 <- mask(r2, rel)
# Save output raster:
writeRaster(r2, "monANRcont_tsptTotP.tif", overwrite=T)

##### REAL
scen <- "R"
ha <- haR
wdpt <- wdptR

rel <- raster(paste(outR, "prox-agrd-anrR.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkR, r1)
temp <- sum(walkR, busR, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)
ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monANRcont_tsptTotR.tif", overwrite=T)

##### OPT
scen <- "O"
ha <- haO
wdpt <- wdptO

rel <- raster(paste(outR, "prox-agrd-anrO.tif", sep="/"))
r1 <- rel
r2 <- rel
r1[r1>1] <- NA
r2[r2<2] <- NA
r1 <- mask(walkO, r1)
temp <- sum(walkO, busO, na.rm=T)
r2 <- mask(temp, r2)
r <- sum(r1, r2, na.rm=T)

ext <- mask(ext, rel)

## Transport Cost Calc # ~~~~~~~~~~~
source(tcostcalc)

r2 <- r2 / ha
r2 <- mask(r2, rel)
writeRaster(r2, "monANRcont_tsptTotO.tif", overwrite=T)

##### ANR MONITORING TOTALS ##########

## PESS
r1 <- raster("monANRy1_tsptTotP.tif")
r2 <- raster("monANRy2_tsptTotP.tif")
r3 <- raster("monANRcont_tsptTotP.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monANR_tsptTotP.tif",overwrite=T)

## REAL
r1 <- raster("monANRy1_tsptTotR.tif")
r2 <- raster("monANRy2_tsptTotR.tif")
r3 <- raster("monANRcont_tsptTotR.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monANR_tsptTotR.tif",overwrite=T)

## OPT
r1 <- raster("monANRy1_tsptTotO.tif")
r2 <- raster("monANRy2_tsptTotO.tif")
r3 <- raster("monANRcont_tsptTotO.tif")
st <- stack(r1, r2, r3)
r <- sum(st, na.rm=T)
writeRaster(r, "monANR_tsptTotO.tif",overwrite=T)

