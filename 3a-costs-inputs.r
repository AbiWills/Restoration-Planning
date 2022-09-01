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

# SET WORKING DIRECTORY:
setwd(inDir)

################################################################################
############################## RESTORATION COSTS ###############################                                <- RESTORATION COSTS
################################################################################

################################################################################
################################# REMOTENESS ###################################                                <- REMOTENESS (TRAVEL DISTANCE)

# For each applicable pixel under each restoration method, this code determines:
# (a) whether closer to road or agmosaic (prox-agrd-XXX); and,
# (b) the minimum distance from either the most proximal road or agmosaic (remoteness-XXX)
# The output rasters are used in calculation of transport costs

setwd(inDir)

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

setwd(outM)

### Calculate measures of remoteness for pixels applicable to each restoration method

##### METHODS RELEVANT TO FOREST LC ############################################

r1 <- raster("vinesP.grd")
r2 <- raster("vinesR.grd")
r3 <- raster("vinesO.grd")

d1 <- mask(distag, r1)
d2 <- mask(distrd, r1)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-vinesP.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-vinesP.tif", overwrite=T)

d1 <- mask(distag, r2)
d2 <- mask(distrd, r2)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-vinesR.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-vinesR.tif", overwrite=T)

d1 <- mask(distag, r3)
d2 <- mask(distrd, r3)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-vinesO.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-vinesO.tif", overwrite=T)


r1 <- raster("herbsP.grd")
r2 <- raster("herbsR.grd")
r3 <- raster("herbsO.grd")

d1 <- mask(distag, r1)
d2 <- mask(distrd, r1)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-shrubP.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-shrubP.tif", overwrite=T)

d1 <- mask(distag, r2)
d2 <- mask(distrd, r2)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-shrubR.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-shrubR.tif", overwrite=T)

d1 <- mask(distag, r3)
d2 <- mask(distrd, r3)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-shrubO.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-shrubO.tif", overwrite=T)

r1 <- raster("lantP.grd")
r2 <- raster("lantR.grd")
r3 <- raster("lantO.grd")

d1 <- mask(distag, r1)
d2 <- mask(distrd, r1)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-lantP.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-lantP.tif", overwrite=T)

d1 <- mask(distag, r2)
d2 <- mask(distrd, r2)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-lantR.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-lantR.tif", overwrite=T)

d1 <- mask(distag, r3)
d2 <- mask(distrd, r3)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-lantO.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-lantO.tif", overwrite=T)

##### METHODS RELEVANT TO SAVANNA & AGMOSAIC LC ################################

r1 <- raster("grassP.grd")
r2 <- raster("grassR.grd")
r3 <- raster("grassO.grd")

d1 <- mask(distag, r1)
d2 <- mask(distrd, r1)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-grassP.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-grassP.tif", overwrite=T)

d1 <- mask(distag, r2)
d2 <- mask(distrd, r2)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-grassR.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-grassR.tif", overwrite=T)

d1 <- mask(distag, r3)
d2 <- mask(distrd, r3)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-grassO.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-grassO.tif", overwrite=T)

r1 <- raster("fireP.grd")
r2 <- raster("fireR.grd")
r3 <- raster("fireO.grd")

d1 <- mask(distag, r1)
d2 <- mask(distrd, r1)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-fireP.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-fireP.tif", overwrite=T)

d1 <- mask(distag, r2)
d2 <- mask(distrd, r2)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-fireR.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-fireR.tif", overwrite=T)

d1 <- mask(distag, r3)
d2 <- mask(distrd, r3)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-fireO.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-fireO.tif", overwrite=T)

##### ACTIVE PLANTING METHODS ####################################################

r1 <- raster("enrichP.grd")
r2 <- raster("enrichR.grd")
r3 <- raster("enrichO.grd")

d1 <- mask(distag, r1)
d2 <- mask(distrd, r1)
d3 <- mask(rddist, r1)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- out1
out2[out2==1] <- d1
out2[out2==2] <- d2 + d3
writeRaster(out1, "remoteness/prox-agrd-enrichP.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-enrichP.tif", overwrite=T)

d1 <- mask(distag, r2)
d2 <- mask(distrd, r2)
d3 <- mask(rddist, r2)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- out1
out2[out2==1] <- d1
out2[out2==2] <- d2 + d3
writeRaster(out1, "remoteness/prox-agrd-enrichR.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-enrichR.tif", overwrite=T)

d1 <- mask(distag, r3)
d2 <- mask(distrd, r3)
d3 <- mask(rddist, r3)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- out1
out2[out2==1] <- d1
out2[out2==2] <- d2 + d3
writeRaster(out1, "remoteness/prox-agrd-enrichO.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-enrichO.tif", overwrite=T)


r1 <- raster("fsaP.grd")
r2 <- raster("fsaR.grd")
r3 <- raster("fsaO.grd")

d1 <- mask(distag, r1)
d2 <- mask(distrd, r1)
d3 <- mask(rddist, r1)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- out1
out2[out2==1] <- d1
out2[out2==2] <- d2 + d3
writeRaster(out1, "remoteness/prox-agrd-fsaP.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-fsaP.tif", overwrite=T)

d1 <- mask(distag, r2)
d2 <- mask(distrd, r2)
d3 <- mask(rddist, r2)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- out1
out2[out2==1] <- d1
out2[out2==2] <- d2 + d3
writeRaster(out1, "remoteness/prox-agrd-fsaR.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-fsaR.tif", overwrite=T)

d1 <- mask(distag, r3)
d2 <- mask(distrd, r3)
d3 <- mask(rddist, r3)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- out1
out2[out2==1] <- d1
out2[out2==2] <- d2 + d3
writeRaster(out1, "remoteness/prox-agrd-fsaO.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-fsaO.tif", overwrite=T)

r1 <- raster("nurseP.grd")
r2 <- raster("nurseR.grd")
r3 <- raster("nurseO.grd")

d1 <- mask(distag, r1)
d2 <- mask(distrd, r1)
d3 <- mask(rddist, r1)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- out1
out2[out2==1] <- d1
out2[out2==2] <- d2 + d3
writeRaster(out1, "remoteness/prox-agrd-nurseP.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-nurseP.tif", overwrite=T)

d1 <- mask(distag, r2)
d2 <- mask(distrd, r2)
d3 <- mask(rddist, r2)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- out1
out2[out2==1] <- d1
out2[out2==2] <- d2 + d3
writeRaster(out1, "remoteness/prox-agrd-nurseR.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-nurseR.tif", overwrite=T)

d1 <- mask(distag, r3)
d2 <- mask(distrd, r3)
d3 <- mask(rddist, r3)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- out1
out2[out2==1] <- d1
out2[out2==2] <- d2 + d3
writeRaster(out1, "remoteness/prox-agrd-nurseO.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-nurseO.tif", overwrite=T)

r1 <- raster("soilP.grd")
r2 <- raster("soilR.grd")
r3 <- raster("soilO.grd")

d1 <- mask(distag, r1)
d2 <- mask(distrd, r1)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-soilP.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-soilP.tif", overwrite=T)

d1 <- mask(distag, r2)
d2 <- mask(distrd, r2)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-soilR.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-soilR.tif", overwrite=T)

d1 <- mask(distag, r3)
d2 <- mask(distrd, r3)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, "remoteness/prox-agrd-soilO.tif", overwrite=T)
writeRaster(out2, "remoteness/remoteness-soilO.tif", overwrite=T)

########################### ANR METHODS COLLATED ###############################
setwd(outA)

r1 <- raster("anrP.grd")
r2 <- raster("anrR.grd")
r3 <- raster("anrO.grd")

d1 <- mask(distag, r1)
d2 <- mask(distrd, r1)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, paste(outR, "prox-agrd-anrP.tif", sep = "/"), overwrite=T)
writeRaster(out2, paste(outR, "remoteness-anrP.tif", sep = "/"), overwrite=T)

d1 <- mask(distag, r2)
d2 <- mask(distrd, r2)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, paste(outR, "prox-agrd-anrR.tif", sep = "/"), overwrite=T)
writeRaster(out2, paste(outR, "remoteness-anrR.tif", sep = "/"), overwrite=T)

d1 <- mask(distag, r3)
d2 <- mask(distrd, r3)
st <- stack(d1, d2)
out1 <- which.min(st)
out2 <- min(st)
writeRaster(out1, paste(outR, "prox-agrd-anrO.tif", sep = "/"), overwrite=T)
writeRaster(out2, paste(outR, "remoteness-anrO.tif", sep = "/"), overwrite=T)

################################################################################
################## CREATING RESTORATION COSTS INPUTS STACK #####################                           <- RESTORATION COSTS INPUTS STACK

# This section of code creates a raster stack comprising all input rasters used to
# assign appropriate restoration methods.

### Set input work directory
setwd(inDir)

# Key determinants of Tsport Method: #Current Land Cover class
                                     #Elevation
                                     #Camping distance bands (# nights spent camping, if >3km from habitation or roads, 1 night spent per additional 7km travelled)
                                     #Road transport need (all sites >3km from habitation that are closer to roads than habitation)
                                     #Distance from Agriculture mosaic (for areas requiring off-road transport only)
                                     #Distance from Road (for areas requiring road transport)
# r1 = Current Land Cover ("lc")
# r2 = Elevation ("elev")
# r3 = Camping distance bands ("campnights")
# r4 = Road transport need ("rdtsport")
# r5 = Distance Agriculture ("distag")
# r6 = Distance Road ("distrd")
# r7 = Distance along road to nearest village/nursery hub ("rddist")

### FIRST NEED TO COMPUTE INPUT RASTERS ########################################
### FOR WHICH NEED RESTORATION METHODS STACK:

st <- stack("methods_stack.grd") # .grd Reads in all bands

### Rename bands in newly read raster stack
names(st) <- c("lc","agbdef","elev","distfor","distsav","distforsav","distag","distrd","pdistag")

# Create objects from layers in my raster stack:
lc <- subset(st,1)                    # Create object from layer in stack: Current Land Cover ("lc")
agbdef <- subset(st,2)                # Percent AGB Deficit ("agbdef")                                                #
elev <- subset(st,3)                  # Elevation ("elev")
distfor <- subset(st,4)               # Euclidean distance to intact Forest ("distfor")                               #
distsav <- subset(st,5)               # Euclidean distance to intact Savanna/Floodplain ("distsav")                   #
distforsav <- subset(st,6)            # Euclidean distance to intact Forest/Savanna/Floodplain ("distforsav")         #
distag <- subset(st,7)                # Euclidean distance to Agriculture mosaic ("distag")
distrd <- subset(st,8)                # Euclidean distance to Road ("distrd")
pdistag <- subset(st,9)               # Path distance to Agriculture ("pdistag")

### COMPUTE INPUT RASTERS ######################################################

### TRANSPORT COSTS ####################
## Generate rasters needed to determine transport mode and cost
# (developed from scratch to match study area resolution, extent and CRS)

#################### BASED ON PATH DISTANCE:
###(accounting for elevation, i.e. actual distance travelled)

# Road transport need:
r1 <- raster("pathdist_agmosaic.tif")
r2 <- raster("pathdist_road.tif")
r <- raster(r1)
r[r1 > 3000 & r2 < r1] <- 1
r[is.na(r[])] <- 0
ratify(r, count=T)
writeRaster(r, "road_tsport_need.grd", overwrite=T)

# Camping need:
r <- raster(r1)
r[r1>3000 & r2>3000] <- 1
ratify(r, count=T)
writeRaster(r, "campneed.grd", overwrite=T)

# Camping distance:
r1 <- r
r <- raster(r1)
r[is.na(r1[])] <- 1
r[r1==1] <- NA
writeRaster(r, "campneed_inv.grd", overwrite=T)
# Then campdist created from inverted raster in ArcGIS Pro, Path Distance tool (method = Planar)
# Something not computing properly in ArcGIS, so trying something different:
# Combine road and agmosaic rasters to calculate campdist
r1 <- raster("LandCover_AgMosaic.tif")
r2 <- raster("Road_final_dissolve_1ha.tif")
r1[is.na(r1[])] <- 0
r2[is.na(r2[])] <- 0
myFun <- function(x, y) {ifelse(x>=1 | y>=1, 1, 0)}
r <- overlay(r1, r2, fun=myFun)
r[r==0] <- NA
writeRaster(r, "roadag_combine.tif", overwrite=T)
# Then campdist created from combined road and agmosaic raster in ArcGIS Pro, Path Distance tool (method = Planar)
# In order to work had to convert raster to integer first, then use Path Distance tool on this.
# Output saved to file as: "campdist.tif"

# Travel with HEAVY load
# Camping dist bands (= nights) from campneed, i.e. 3km from rd/ag
r <- raster("campdist.tif")
r1 <- raster("campneed.grd")
m <- c(0, 7000, 1,  7000.001, 14000, 2,  14000.001, 21000, 3, 21000.001, 28000, 4, 28000.001, 35000, 5, 35000.001, 42000, 6, 42000.001, 49000, 7, 49000.001, 56000, 8, 56000.001, 63000, 9, 63000.001, 70000, 10, 70000.001, 77000, 11, 77000.001, 84000, 12)
cdb <- matrix(m, ncol=3, byrow=TRUE)
r <- reclassify(r, cdb)
r <- mask(r, r1)
writeRaster(r, "campnights_7km.grd", overwrite=T)

# Travel with LIGHT load
# Camping dist bands (= nights) from campneed, i.e. 3km from rd/ag
r <- raster("campdist.tif")
r1 <- raster("campneed.grd")
m <- c(0, 14000, 1,  14000.001, 28000, 2,  28000.001, 42000, 3, 42000.001, 56000, 4, 56000.001, 70000, 5, 70000.001, 84000, 6)
cdb <- matrix(m, ncol=3, byrow=TRUE)
r <- reclassify(r, cdb)
r <- mask(r, r1)
writeRaster(r, "campnights_14km.grd", overwrite=T)

### CREATE STACK: ##############################################################

######################### STACK WITH PATH DISTANCE RASTERS:
#####(accounting for elevation, i.e. actual distance travelled)

# Read in rasters
r1 <- raster("LandCoverFinal_1ha.tif")
r2 <- raster("dem_srtm_1ha.tif")
r3 <- raster("campnights_7km.grd")
r4 <- raster("road_tsport_need.grd")
r5 <- raster("pathdist_agmosaic.tif")
r6 <- raster("pathdist_road.tif")
r7 <- raster("road_dist_1ha.tif")

# Create my stack
st <- stack(r1, r2, r3, r4, r5, r6, r7)

# Assign meaningful names to layers in stack
names(st) <- c("lc","elev","campnights","rdtsport","distag","distrd", "rddist")
st <- mask(st, sa)

# Save raster stack
writeRaster(st,"costs_stack.grd",overwrite=T)

# Check appeared in file - yep!

################################################################################
######################## TRANSPORT COSTS INPUTS ################################                             <- TRANSPORT COSTS INPUTS

# This code calculates the cost of one return trip to each landscape pixel
# including different modes of transport by road and on foot. Transport modes include:
# Porter, Walking, Motorbike (hire / own),
# Powertiller (hire / own), Public bus & Truck rental.

### Set input work directory
setwd(inDir)

### Read in raster stack used to determine restoration costs
st <- stack("costs_stack.grd")      # Restoration costs raster stack
lc <- subset(st,1)                  # r1 = Current Land Cover ("lc")
elev <- subset(st,2)                # r2 = Elevation ("elev")
campnights <- subset(st,3)          # r3 = Camping distance bands ("campnights") (# nights spent camping, if >3km from habitation or roads, 1 night spent per additional 7km travelled)
rdtsport <- subset(st,4)            # r4 = Road transport need ("rdtsport") (all sites >3km from habitation that are closer to roads than habitation)
distag <- subset(st,5)              # r5 = Distance Agriculture ("distag") (for areas requiring off-road transport only)
distrd <- subset(st,6)              # r6 = Distance Road ("distrd") (for areas requiring road transport)
rddist <- subset(st,7)              # r7 = Distance along road ("rddist") (for areas requiring road transport)

# Read in additional rasters needed:
campn_lightload <- raster("campnights_14km.grd")

##### CALL IN CSVs:
# Restoration costs:
restCosts <- read.table("restorationCosts.csv", sep=",", header=T, row.names=1, stringsAsFactors=F, na.strings=".") # Enables row names

# Create additional vectors needed:
infl <- 0.022      # Inflation rate

# Define output directory (folder in which outputs will be saved):
setwd(outT)

################################################################################

##### COST CALCULATION FOR EACH TRANSPORT METHOD
##### Calculate base cost rasters for each transport method (1 return trip) in Y1

                       #################################
                       ##### OFF-ROAD TRANSPORT ########
                       #################################

## Relevance: areas >3km from habitation or roads (same as camping need)
## Options included: porters, powertiller, motorbike
## No need to account for NA values by transforming NA<-0 here since transport methods always have an associated cost

################################################################################
### PESSIMISTIC ################################################################
scen <- "P"

cost <- combine_words(c("cost",scen), and="")
yrs <- combine_words(c("yrs",scen), and="")
reps <- combine_words(c("reps",scen), and="")
int <- combine_words(c("int",scen), and="")
eqp <- combine_words(c("eqp",scen), and="")
life <- combine_words(c("life",scen), and="")
styr <- combine_words(c("styr",scen), and="")
stems <- combine_words(c("stems",scen), and="")

### MODE OF TRANSPORT : PORTERS ################################################

### LABOUR cost ~~~~~~~
tm <- "porter"      # Transport method (tm)
myFun <- function(rdtsport, distrd, distag, elev) { ifelse (
    rdtsport==1 & elev<=1000, distrd/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==1 & elev>1000, distrd/1000 * restCosts["porterH", cost], ifelse (
    rdtsport==0 & elev<=1000, distag/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==0 & elev>1000, distag/1000 * restCosts["porterH", cost], -9999))))}

s1 <- stack(rdtsport, distrd, distag, elev)
r1 <- overlay(s1,fun=myFun)
r1                                          # Cost per 1 porter per 1km travelled * km travelled

### CAMPING cost ~~~~~~~
tm <- "camp"
r2 <- campnights * restCosts[tm, cost]
r2                                          # Cost per 1 porter per 1 night * num nights
                                            # i.e. camping for one return trip to site being restored
# PORTER costs ~~~~~~~ (labour + camping)
r3 <- r1 + r2
r3

# GOVERNMENT REPRESENTATIVE cost ~~~~~~~
tm <- "gov"
r4 <- campnights * restCosts[tm, cost]
r4                                          # Cost per 1 official per 1 night * num nights

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 porter for 1 return trip to site being restored
writeRaster(r1, "porter_labRtnTripP.tif", overwrite=T)
# Camping cost for 1 porter for 1 return trip to site being restored
writeRaster(r2, "porter_campRtnTripP.tif", overwrite=T)
# Total cost for one return trip to site being restored (not incl. gov costs, to be added separately after mutiplying porter costs by num trips)
writeRaster(r3, "porter_offrdRtnTripP.tif", overwrite=T)
# Camping cost for 1 govt official for 1 return trip to site being restored
writeRaster(r4, "porter_govRtnTripP.tif", overwrite=T)

### MODE OF TRANSPORT : WALKING (WORKERS WITH LIGHT LOAD) ######################

### LABOUR cost ~~~~~~~
tm <- "porter"      # Transport method (tm)
myFun <- function(rdtsport, distrd, distag, elev) { ifelse (
    rdtsport==1 & elev<=1000, distrd/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==1 & elev>1000, distrd/1000 * restCosts["porterH", cost], ifelse (
    rdtsport==0 & elev<=1000, distag/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==0 & elev>1000, distag/1000 * restCosts["porterH", cost], -9999))))}

s1 <- stack(rdtsport, distrd, distag, elev)
r1 <- overlay(s1,fun=myFun)                        # Cost per 1 porter per 1km travelled * km travelled
r1

### CAMPING cost ~~~~~~~
tm <- "camp"
r2 <- campn_lightload * restCosts[tm, cost]
r2                                           # Cost per 1 porter per 1 night * num nights
                                             # i.e. camping for one return trip to site being restored
# WALKING costs ~~~~~~~ (labour + camping)
r3 <- r1 + r2
r3

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 porter for 1 return trip to site being restored
writeRaster(r1, "walk_labRtnTripP.tif", overwrite=T)
# Camping cost for 1 porter for 1 return trip to site being restored
writeRaster(r2, "walk_campRtnTripP.tif", overwrite=T)
# Total cost for one return trip to site being restored (not incl. gov costs, to be added separately after mutiplying porter costs by num trips)
writeRaster(r3, "walk_offrdRtnTripP.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER HIRE #######################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerhire"      # Transport method (tm)
#myFun <- function(rdtsport, distrd, distag) { ifelse(rdtsport==1, distrd/1000 * restCosts[tm, cost], distag/1000 * restCosts[tm, cost]) }
#s1 <- stack(rdtsport, distrd, distag)
#r1 <- overlay(s1,fun=myFun)
# Above method returned the following error, couldn't solve quickly by trouble shooting so went with the below long-winded option.
# Error in ifelse(rdtsport == 1, distrd/1000 * restCosts[tm, cost], distag/1000 *  :
#  argument "distag" is missing, with no default
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "pwthire_offrdRtnTripP.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER OWN ########################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "pwtown_offrdRtnTripP.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE HIRE #########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikehire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "mbikehire_offrdRtnTripP.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE OWN ##########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikeown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "mbikeown_offrdRtnTripP.tif", overwrite=T)

################################################################################
### REALISTIC ##################################################################
scen <- "R"

cost <- combine_words(c("cost",scen), and="")
yrs <- combine_words(c("yrs",scen), and="")
reps <- combine_words(c("reps",scen), and="")
int <- combine_words(c("int",scen), and="")
eqp <- combine_words(c("eqp",scen), and="")
life <- combine_words(c("life",scen), and="")
styr <- combine_words(c("styr",scen), and="")
stems <- combine_words(c("stems",scen), and="")

### MODE OF TRANSPORT : PORTERS ################################################

### LABOUR cost ~~~~~~~
tm <- "porter"      # Transport method (tm)
myFun <- function(rdtsport, distrd, distag, elev) { ifelse (
    rdtsport==1 & elev<=1000, distrd/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==1 & elev>1000, distrd/1000 * restCosts["porterH", cost], ifelse (
    rdtsport==0 & elev<=1000, distag/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==0 & elev>1000, distag/1000 * restCosts["porterH", cost], -9999))))}

s1 <- stack(rdtsport, distrd, distag, elev)
r1 <- overlay(s1,fun=myFun)                        # Cost per 1 porter per 1km travelled * km travelled
r1

### CAMPING cost ~~~~~~~
tm <- "camp"
r2 <- campnights * restCosts[tm, cost]
r2                                           # Cost per 1 porter per 1 night * num nights
                                             # i.e. camping for one return trip to site being restored
# PORTER costs (labour + camping)
r3 <- r1 + r2
r3

# GOVERNMENT REPRESENTATIVE cost
tm <- "gov"
r4 <- campnights * restCosts[tm, cost]
r4                                          # Cost per 1 official per 1 night * num nights

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 porter for 1 return trip to site being restored
writeRaster(r1, "porter_labRtnTripR.tif", overwrite=T)
# Camping cost for 1 porter for 1 return trip to site being restored
writeRaster(r2, "porter_campRtnTripR.tif", overwrite=T)
# Total cost for one return trip to site being restored (not incl. gov costs, to be added separately after mutiplying porter costs by num trips)
writeRaster(r3, "porter_offrdRtnTripR.tif", overwrite=T)
# Camping cost for 1 govt official for 1 return trip to site being restored
writeRaster(r4, "porter_govRtnTripR.tif", overwrite=T)

### MODE OF TRANSPORT : WALKING (WORKERS WITH LIGHT LOAD) ######################

### LABOUR cost ~~~~~~~
tm <- "porter"      # Transport method (tm)
myFun <- function(rdtsport, distrd, distag, elev) { ifelse (
    rdtsport==1 & elev<=1000, distrd/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==1 & elev>1000, distrd/1000 * restCosts["porterH", cost], ifelse (
    rdtsport==0 & elev<=1000, distag/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==0 & elev>1000, distag/1000 * restCosts["porterH", cost], -9999))))}

s1 <- stack(rdtsport, distrd, distag, elev)
r1 <- overlay(s1,fun=myFun)                        # Cost per 1 porter per 1km travelled * km travelled
r1

### CAMPING cost ~~~~~~~
tm <- "camp"
r2 <- campn_lightload * restCosts[tm, cost]
r2                                           # Cost per 1 porter per 1 night * num nights
                                             # i.e. camping for one return trip to site being restored
# WALKING costs ~~~~~~~ (labour + camping)
r3 <- r1 + r2
r3

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 porter for 1 return trip to site being restored
writeRaster(r1, "walk_labRtnTripR.tif", overwrite=T)
# Camping cost for 1 porter for 1 return trip to site being restored
writeRaster(r2, "walk_campRtnTripR.tif", overwrite=T)
# Total cost for one return trip to site being restored (not incl. gov costs, to be added separately after mutiplying porter costs by num trips)
writeRaster(r3, "walk_offrdRtnTripR.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER HIRE #######################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerhire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "pwthire_offrdRtnTripR.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER OWN ########################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "pwtown_offrdRtnTripR.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE HIRE #########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikehire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "mbikehire_offrdRtnTripR.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE OWN ##########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikeown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "mbikeown_offrdRtnTripR.tif", overwrite=T)

################################################################################
### OPTIMISTIC #################################################################
scen <- "O"

cost <- combine_words(c("cost",scen), and="")
yrs <- combine_words(c("yrs",scen), and="")
reps <- combine_words(c("reps",scen), and="")
int <- combine_words(c("int",scen), and="")
eqp <- combine_words(c("eqp",scen), and="")
life <- combine_words(c("life",scen), and="")
styr <- combine_words(c("styr",scen), and="")
stems <- combine_words(c("stems",scen), and="")

### MODE OF TRANSPORT : PORTERS ################################################

### LABOUR cost ~~~~~~~
tm <- "porter"      # Transport method (tm)
myFun <- function(rdtsport, distrd, distag, elev) { ifelse (
    rdtsport==1 & elev<=1000, distrd/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==1 & elev>1000, distrd/1000 * restCosts["porterH", cost], ifelse (
    rdtsport==0 & elev<=1000, distag/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==0 & elev>1000, distag/1000 * restCosts["porterH", cost], -9999))))}

s1 <- stack(rdtsport, distrd, distag, elev)
r1 <- overlay(s1,fun=myFun)                        # Cost per 1 porter per 1km travelled * km travelled
r1

### CAMPING cost ~~~~~~~
tm <- "camp"
r2 <- campnights * restCosts[tm, cost]
r2                                           # Cost per 1 porter per 1 night * num nights
                                             # i.e. camping for one return trip to site being restored
# PORTER costs (labour + camping)
r3 <- r1 + r2
r3

# GOVERNMENT REPRESENTATIVE cost
tm <- "gov"
r4 <- campnights * restCosts[tm, cost]
r4                                          # Cost per 1 official per 1 night * num nights

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 porter for 1 return trip to site being restored
writeRaster(r1, "porter_labRtnTripO.tif", overwrite=T)
# Camping cost for 1 porter for 1 return trip to site being restored
writeRaster(r2, "porter_campRtnTripO.tif", overwrite=T)
# Total cost for one return trip to site being restored (not incl. gov costs, to be added separately after mutiplying porter costs by num trips)
writeRaster(r3, "porter_offrdRtnTripO.tif", overwrite=T)
# Camping cost for 1 govt official for 1 return trip to site being restored
writeRaster(r4, "porter_govRtnTripO.tif", overwrite=T)

### MODE OF TRANSPORT : WALKING (WORKERS WITH LIGHT LOAD) ######################

### LABOUR cost ~~~~~~~
tm <- "porter"      # Transport method (tm)
myFun <- function(rdtsport, distrd, distag, elev) { ifelse (
    rdtsport==1 & elev<=1000, distrd/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==1 & elev>1000, distrd/1000 * restCosts["porterH", cost], ifelse (
    rdtsport==0 & elev<=1000, distag/1000 * restCosts["porterL", cost], ifelse (
    rdtsport==0 & elev>1000, distag/1000 * restCosts["porterH", cost], -9999))))}

s1 <- stack(rdtsport, distrd, distag, elev)
r1 <- overlay(s1,fun=myFun)                        # Cost per 1 porter per 1km travelled * km travelled
r1

### CAMPING cost ~~~~~~~
tm <- "camp"
r2 <- campn_lightload * restCosts[tm, cost]
r2                                          # Cost per 1 porter per 1 night * num nights
                                            # i.e. camping for one return trip to site being restored
# WALKING costs ~~~~~~~ (labour + camping)
r3 <- r1 + r2
r3

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 porter for 1 return trip to site being restored
writeRaster(r1, "walk_labRtnTripO.tif", overwrite=T)
# Camping cost for 1 porter for 1 return trip to site being restored
writeRaster(r2, "walk_campRtnTripO.tif", overwrite=T)
# Total cost for one return trip to site being restored (not incl. gov costs, to be added separately after mutiplying porter costs by num trips)
writeRaster(r3, "walk_offrdRtnTripO.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER HIRE #######################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerhire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "pwthire_offrdRtnTripO.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER OWN ########################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "pwtown_offrdRtnTripO.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE HIRE #########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikehire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "mbikehire_offrdRtnTripO.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE OWN ##########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikeown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(distrd, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r2 <- rdtsport
r2[r2==1] <- NA
r2 <- mask(distag, r2)
r2 <- r2/1000 * restCosts[tm, cost]
r <- sum(r1, r2, na.rm=T)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Labour cost per 1 powertiller for 1 return trip to site being restored
writeRaster(r, "mbikeown_offrdRtnTripO.tif", overwrite=T)

################################################################################

                       #################################
                       ###### ON-ROAD TRANSPORT ########
                       #################################

################################################################################
### PESSIMISTCIC ###############################################################
scen <- "P"

cost <- combine_words(c("cost",scen), and="")
yrs <- combine_words(c("yrs",scen), and="")
reps <- combine_words(c("reps",scen), and="")
int <- combine_words(c("int",scen), and="")
eqp <- combine_words(c("eqp",scen), and="")
life <- combine_words(c("life",scen), and="")
styr <- combine_words(c("styr",scen), and="")
stems <- combine_words(c("stems",scen), and="")

### MODE OF TRANSPORT : PUBLIC BUS (PORTERS) ###################################

### TRANSPORT cost ~~~~~~~
tm <- "publicbus"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per person for 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "bus_onrdRtnTripP.tif", overwrite=T)

### MODE OF TRANSPORT : TRUCK ##################################################

### TRANSPORT cost ~~~~~~~
tm <- "truck"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "truck_onrdRtnTripP.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER HIRE #######################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerhire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "pwthire_onrdRtnTripP.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER OWN ########################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "pwtown_onrdRtnTripP.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE HIRE #########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikehire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "mbikehire_onrdRtnTripP.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE OWN ##########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikeown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "mbikeown_onrdRtnTripP.tif", overwrite=T)

################################################################################
### REALISTIC ##################################################################
scen <- "R"

cost <- combine_words(c("cost",scen), and="")
yrs <- combine_words(c("yrs",scen), and="")
reps <- combine_words(c("reps",scen), and="")
int <- combine_words(c("int",scen), and="")
eqp <- combine_words(c("eqp",scen), and="")
life <- combine_words(c("life",scen), and="")
styr <- combine_words(c("styr",scen), and="")
stems <- combine_words(c("stems",scen), and="")

### MODE OF TRANSPORT : PUBLIC BUS (PORTERS) ###################################

### TRANSPORT cost ~~~~~~~
tm <- "publicbus"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per person for 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "bus_onrdRtnTripR.tif", overwrite=T)

### MODE OF TRANSPORT : TRUCK ##################################################

### TRANSPORT cost ~~~~~~~
tm <- "truck"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "truck_onrdRtnTripR.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER HIRE #######################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerhire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "pwthire_onrdRtnTripR.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER OWN ########################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "pwtown_onrdRtnTripR.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE HIRE #########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikehire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "mbikehire_onrdRtnTripR.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE OWN ##########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikeown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "mbikeown_onrdRtnTripR.tif", overwrite=T)

################################################################################
### OPTIMISTIC #################################################################
scen <- "O"

cost <- combine_words(c("cost",scen), and="")
yrs <- combine_words(c("yrs",scen), and="")
reps <- combine_words(c("reps",scen), and="")
int <- combine_words(c("int",scen), and="")
eqp <- combine_words(c("eqp",scen), and="")
life <- combine_words(c("life",scen), and="")
styr <- combine_words(c("styr",scen), and="")
stems <- combine_words(c("stems",scen), and="")

### MODE OF TRANSPORT : PUBLIC BUS (PORTERS) ###################################

### TRANSPORT cost ~~~~~~~
tm <- "publicbus"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per person for 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "bus_onrdRtnTripO.tif", overwrite=T)

### MODE OF TRANSPORT : TRUCK ##################################################

### TRANSPORT cost ~~~~~~~
tm <- "truck"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "truck_onrdRtnTripO.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER HIRE #######################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerhire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "pwthire_onrdRtnTripO.tif", overwrite=T)

### MODE OF TRANSPORT : POWERTILLER OWN ########################################

### TRANSPORT cost ~~~~~~~
tm <- "pwtillerown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "pwtown_onrdRtnTripO.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE HIRE #########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikehire"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "mbikehire_onrdRtnTripO.tif", overwrite=T)

### MODE OF TRANSPORT : MOTORBIKE OWN ##########################################

### TRANSPORT cost ~~~~~~~
tm <- "mbikeown"      # Transport method (tm)
r1 <- rdtsport
r1[r1==0] <- NA
r1 <- mask(rddist, r1)
r1 <- r1/1000 * restCosts[tm, cost]
r1[is.na(r1)] <- 0
r1 <- mask(r1, lc)

# GOVERNMENT representative cost
# In this case is not variable spatially as assume reach site within 1 day.
# Calculated per restoration method below as cost of gov official * number of reps and yrs

### EQUIPMENT cost ~~~~~~~
### Calculated per restoration method below as variable with duration of project and activity implementation (yrs)

### Save output Rasters:
# Cost per 1 return trip from nearest village centre (nursery hub) to closest point on road network to site being restored
writeRaster(r1, "mbikeown_onrdRtnTripO.tif", overwrite=T)

################################################################################
#################### LABOUR AND EQUIPMENT COSTS INPUTS #########################                             <- LABOUR AND EQUIPMENT COSTS INPUTS

# This code generates rasters that are used as determinants of restoration costs in addition to rest-costs.csv. Including:
# Resampled Land Prices raster to match study area resolution, extent, crs, etc.
# Firebreak plan raster (different areas managed and cost for: large PAs >10Kha / small PAs and savanna / agriculture mosaic / forests and other [NA])
# Multiplier surfaces for various staff costs depending on ha they are expected to manage (economies of scale):
# - Protected areas assume 1 employee for entire PA (i.e. staff cost divided by PA area in ha) with multiplier per scenario for % salary shared by PA mgmt
# - Agriculture mosaic (outdside PAs) costs estimates per village and spread across Pessimistic-4ha, Realistic-50ha, Optimistic-150ha
# - Unprotected savanna, floodplain and forest = Pessimistic-25ha, Realistic-7,000ha, Optimistic-14,000ha.

### Set input work directory
setwd(inDir)

### LAND PROCUREMENT ####################

# Resample to match study area extent, resolution and CRS
#land <- raster("landprices.tif")
#land

# resample to same extent as lc (and all other rasters in analysis):
#land <- resample(land,lc,method='bilinear') # use nearest neighbour 'ngb' method for categorical datasets; 'bilinear' for continuous
#land
#writeRaster(land, "landprices.tif", overwrite=T)

### LAND MANAGEMENT ####################

### FIREBREAKS
# Firebreak planning organised into 3 categories:
# 1=large PAs (>100km2/10000ha),
# 2=all other PAs and unprotected savanna,
# 3=agriculture mosaic (none in group 1 and 2),
# 0=other/forests (NA)

setwd(inDir)

# Read in input rasters:
sa <- raster("Study_Area.tif") # Template raster used to resample other input rasters
lc <- raster("LandCoverFinal_1ha.tif")
pas <- raster("WDPA_less-open-gamecontrol-wma_area.tif") # Raster with pixels in PAs==PA area (ha), except for Selous where area==average of Sectors, resampled to lc extent, method='ngb'. Doesn't include open areas, game-controlled areas or WMAs.
pas[is.na(pas)] <- 0

# Compute firebreak plan:
fb <- raster(pas)
fb[pas>10000] <- 1
fb[pas<10000 & pas>0] <- 2
fb[is.na(fb[])] <- 0
fb[fb!=1 & (lc==2 | lc==3)] <- 2
fb[fb==0] <- 3
fb[lc==1 | lc==5] <- 0
fb <- mask(fb, sa)
writeRaster(fb, "fbplan.tif", overwrite=T)

### STAFF COSTS ########################

setwd(inDir)

# Read in input rasters:
temp <- raster("Study_Area.tif") # Template raster used to resample other input rasters
pas <- raster("WDPA_less-open-gamecontrol-wma_area.tif") # Raster with pixels in PAs==PA area (ha), except for Selous where area==average of Sectors, resampled to lc extent, method='ngb'. Doesn't include open areas, game-controlled areas or WMAs.
pas[is.na(pas)] <- 0

### PROJECT MANAGER # Assumed necessary for pixels where any form of restoration intervention is required
# PAs = 1 manager for entire area; Pessimistic-100% of time paid for by project, Realistic-50% of time; Optimistic-0% of time (salary covered by PA management); So, annual salary divided by PA area (ha) * 1 (Pessimistic); * 0.5 (Realistic); * 0 (Optimistic)
# Outside PAs = Pessimistic-50ha, Realistic-300ha, Optimistic-3000ha

# Pessimistic
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
r1 <- pas * 1
r2 <- raster(pas)
r2[pas==0] <- 50
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(r, "managerMultP.tif", overwrite=T)

# Realistic
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
r1 <- pas * 0.5
r2 <- raster(pas)
r2[pas==0] <- 300
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(r, "managerMultR.tif", overwrite=T)

# Optimistic
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
r1 <- pas * 0
r2 <- raster(pas)
r2[pas==0] <- 3000
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(r, "managerMultO.tif", overwrite=T)

# PROJECT FORESTER # Assumed necessary for pixels where any form of restoration intervention is required
# PAs = 1 forester for entire area; Same across all scenarios; So, annual salary divided by PA area (ha)
# Outside PAs = Pessimistic-50ha, Realistic-150ha, Optimistic-1000ha

# Pessimistic
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
r1 <- pas * 1
r2 <- raster(pas)
r2[pas==0] <- 50
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(r, "foresterMultP.tif", overwrite=T)

# Realistic
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
r1 <- pas * 1
r2 <- raster(pas)
r2[pas==0] <- 150
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(r, "foresterMultR.tif", overwrite=T)

# Optimistic
rel <- raster(paste(outA, "restorOP.grd", sep="/"))
r1 <- pas * 1
r2 <- raster(pas)
r2[pas==0] <- 1000
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(r, "foresterMultO.tif", overwrite=T)

# COMMUNITY ENGAGEMENT OFFICER # Assumed necessary for pixels where any kind of restoration intervention is needed that are:
# (a) within agmosaic (100%) or within 3km of agmosaic (50%); and, (b) outside Protected Areas and large commercial farms
# Costs estimates per village and spread across Pessimistic-50ha, Realistic-300ha, Optimistic-3000ha

# Pessimistic
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
r1 <- raster(pas)
r2 <- raster(pas)
r1[pas==0] <- 50
r2[pas>0] <- 0
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(r, "commengMultP.tif", overwrite=T)

# Realistic
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
r1 <- raster(pas)
r2 <- raster(pas)
r1[pas==0] <- 300
r2[pas>0] <- 0
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(r, "commengMultR.tif", overwrite=T)

# Optimistic
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
r1 <- raster(pas)
r2 <- raster(pas)
r1[pas==0] <- 3000
r2[pas>0] <- 0
st <- stack(r1, r2)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(r, "commengMultO.tif", overwrite=T)

# FLOODPLAIN PLANTING MULT  P==2, R==1.5, O==1.25

setwd(inDir)

# Pessimistic
r <- raster("LandCoverFinal_1ha.tif")
r[r==1 | r==2 | r==4 | r==5] <- 1
r[r==3] <- 2
writeRaster(r, "floodplainMultP.tif", overwrite=T)

# Realistic
r <- raster("LandCoverFinal_1ha.tif")
r[r==1 | r==2 | r==4 | r==5] <- 1
r[r==3] <- 1.5
writeRaster(r, "floodplainMultR.tif", overwrite=T)

# Optimistic
r <- raster("LandCoverFinal_1ha.tif")
r[r==1 | r==2 | r==4 | r==5] <- 1
r[r==3] <- 1.25
writeRaster(r, "floodplainMultO.tif", overwrite=T)
