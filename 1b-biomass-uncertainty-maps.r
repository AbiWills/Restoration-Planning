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

# Uncertainty maps
outDir <- paste(basename,"outputs/uncertainty",sep="/")
if (file.exists(outDir)){
  cat("\n check directory outputs/uncertainty")
} else {
  dir.create(file.path(outDir))
}

################################################################################
############################ UNCERTAINTY MAPPING ###############################

############################### CURRENT BIOMASS ################################

### 1. Current biomass uncertainty map, created using the following attached data:
#   List of plot coordinates used to calibrate model: current_agb_plot_coords.csv (attached)
#   Upscaled using Google Earth Engine Landsat tile located in the Force_Project Team that Marion recently invited you to join, here: Force_Project/Force_Results/Force_Map/Temp_Tiles_google1/ (full raster split into 100 tiles for processing)
#   Spectral reflectance predictors (bands) used in the above raster: swir1+nir+red_dis+nir_dis+swir1_dis
#   Relative contribution of each predictor to model: rf_model_importancevalues_satelliteupscale.csv (attached)

sa <- readOGR(paste(basename, "inputs/Study_Area.shp", sep="/")) # Study area boundary

# Read in predictor raster stack tiles and merge
# Tiles created by Marion using the 01 code - Force Analyses Prepare rasters
# The google 1 tiles are tiles from the the Goog_RefTex_Sub raster

f <- list.files(paste(basename,"inputs/Temp_Tiles_google1",sep="/"),full.names=T)
r <- lapply(f, stack)
pred <- do.call(merge, c(r, tolerance = 1))
names(pred) <- c("red","nir","swir1","swir2","red_av","nir_av","swir1_av",
              "swir2_av","red_dis","nir_dis","swir1_dis","swir2_dis")
pred
#class      : RasterBrick
#dimensions : 11440, 12720, 145516800, 12  (nrow, ncol, ncell, nlayers)
#resolution : 0.00025, 0.00025  (x, y)
#extent     : 34.65, 37.83, -9.96, -7.1  (xmin, xmax, ymin, ymax)
#crs        : +proj=longlat +datum=WGS84 +no_defs
#source     : C:/Users/wills/AppData/Local/Temp/RtmpIdMTWZ/raster/r_tmp_2021-03-03_083754_7724_58342.grd
#names      :         red,         nir,       swir1,       swir2,      red_av,      nir_av,    swir1_av,    swir2_av,     red_dis,     nir_dis,   swir1_dis,   swir2_dis
#min values :    0.000000,    0.000000,    0.000000,    0.000000,    0.015625,    0.015625,    0.015625,    0.015625,    0.000000,    0.000000,    0.000000,    0.000000
#max values : 255.0000000, 248.0000000, 249.0000000, 255.0000000,   0.9461806,   0.7022569,   0.9218750,   0.8706597,  54.7777778,  29.3333333,  34.8888889,  32.4444444

# Subset rasters in predictor stack keeping only relevant predictor variables used in model: swir1, nir, red_dis, nir_dis, swir1_dis
r1 <- subset(pred, 2)
r2 <- subset(pred, 3)
r3 <- subset(pred, 9)
r4 <- subset(pred, 10)
r5 <- subset(pred, 11)

# Stack relevant predictor variables: swir1, nir, red_dis, nir_dis, swir1_dis
pred <- stack(r1,r2,r3,r4,r5)

# Project predictor raster stack to same CRS as study area (or vice versa) # Working CRS = WGS84 UTM 36S)
newproj <- projection(sa)
pred <- projectRaster(pred, crs=newproj)
#option vice versa:
#newproj <- projection(pred)
#sa2 <- spTransform(sa,CRS(newproj))

# Mask predictor raster stack using study area boundary
pred_sa <- mask(pred, sa)
# check
# pred <- pred_sa
# rm(pred_sa)
# Subset rasters in predictor stack keeping only relevant predictor variables used in model: swir1, nir, red_dis, nir_dis, swir1_dis
#r1 <- subset(pred, 2)
#r2 <- subset(pred, 3)
#r3 <- subset(pred, 9)
#r4 <- subset(pred, 10)
#r5 <- subset(pred, 11)

# Read in locations of plots used for biomass upscaling
plots <- readOGR("current_agb_plot_coords.shp")

# Extract only pixels in predictor raster stack that coincide with plot locations (i.e. sample used for upscaling = calibration subset)
cal_sub <- mask(pred, plots)

# Get values of calibration subset
vec_sub <- as.data.frame(getValues(cal_sub))
names(vec_sub) <- c("s1","s2","s3","s4","s5") # <- c("nir","swir1","red_dis","nir_dis","swir1_dis")
head(vec_sub)

##### EQUATION 1: RELATIVE DISTANCE OF PREDICTORS FROM CALIBRATION ENVELOPE ####

##### PREDICTOR 1 ##########

# Get values of predictor raster 1
vec_pred <- as.data.frame(getValues(r1))
names(vec_pred)[1] <- "p1" # <- c("nir","swir1","red_dis","nir_dis","swir1_dis")
head(vec_pred)

# Create empty vector of equal length
outvec1 <- rep(NA, length(vec_pred$p1))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s1                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p1)){

  a <- vec_pred$p1[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p1[i])){outvec1[i] <- NA} else {

  outvec1[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist1 <- raster(r1)
prop_dist1 <- setValues(prop_dist1,outvec1)
prop_dist1
plot(prop_dist1)
filen <- "current_agb_dist_nir"
writeRaster(prop_dist1, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

##### REPEAT ABOVE CODE FOR REMAINING 5 PREDICTOR VARIABLES #####

##### PREDICTOR 2 #####
# Get values of predictor raster 2
vec_pred <- as.data.frame(getValues(r2))
names(vec_pred)[1] <- "p2" # <- c("nir","swir1","red_dis","nir_dis","swir1_dis")
head(vec_pred)

# Create empty vector of equal length
outvec2 <- rep(NA, length(vec_pred$p2))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s2                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p2)){

  a <- vec_pred$p2[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p2[i])){outvec2[i] <- NA} else {

  outvec2[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist2 <- raster(r2)
prop_dist2 <- setValues(prop_dist2,outvec2)
prop_dist2
plot(prop_dist2)
filen <- "current_agb_dist_swir1"
writeRaster(prop_dist2, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

##### PREDICTOR 3 #####
# Get values of predictor raster 3
vec_pred <- as.data.frame(getValues(r3))
names(vec_pred)[1] <- "p3" # <- c("nir","swir1","red_dis","nir_dis","swir1_dis")
head(vec_pred)

# Create empty vector of equal length
outvec3 <- rep(NA, length(vec_pred$p3))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s3                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p3)){

  a <- vec_pred$p3[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p3[i])){outvec3[i] <- NA} else {

  outvec3[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist3 <- raster(r3)
prop_dist3 <- setValues(prop_dist3,outvec3)
prop_dist3
plot(prop_dist3)
filen <- "current_agb_dist_reddis"
writeRaster(prop_dist3, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

##### PREDICTOR 4 #####
# Get values of predictor raster 4
vec_pred <- as.data.frame(getValues(r4))
names(vec_pred)[1] <- "p4" # <- c("nir","swir1","red_dis","nir_dis","swir1_dis")
head(vec_pred)

# Create empty vector of equal length
outvec4 <- rep(NA, length(vec_pred$p4))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s4                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p4)){

  a <- vec_pred$p4[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p4[i])){outvec4[i] <- NA} else {

  outvec4[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist4 <- raster(r4)
prop_dist4 <- setValues(prop_dist4,outvec4)
prop_dist4
plot(prop_dist4)
filen <- "current_agb_dist_nirdis"
writeRaster(prop_dist4, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

##### PREDICTOR 5 #####
# Get values of predictor raster 5
vec_pred <- as.data.frame(getValues(r5))
names(vec_pred)[1] <- "p5" # <- c("nir","swir1","red_dis","nir_dis","swir1_dis")
head(vec_pred)

# Create empty vector of equal length
outvec5 <- rep(NA, length(vec_pred$p5))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s5                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p5)){

  a <- vec_pred$p5[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p5[i])){outvec5[i] <- NA} else {

  outvec5[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist5 <- raster(r5)
prop_dist5 <- setValues(prop_dist5,outvec5)
prop_dist5
plot(prop_dist5)
filen <- "current_agb_dist_swir1dis"
writeRaster(prop_dist5, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

# CREATE RASTER STACK OF PREDICTOR DISTANCES FROM CALIBRATION ENVELOPE
st <- stack(prop_dist1, prop_dist2, prop_dist3, prop_dist4, prop_dist5)
names(st) <- c("nir","swir1","red_dis","nir_dis","swir1_dis")
st
plot(st)
filen <- "current_agb_dist_all_predictors"
writeRaster(st, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

########## EQUATION 2: ENVELOPE UNCERTAINTY MAP ##########

# Relative contributions of predictor variables # <- c("nir","swir1","red_dis","nir_dis","swir1_dis")
c1 <- 27.6193852345908
c2 <- 100
c3 <- 0
c4 <- 14.0695878689848
c5 <- 10.2021366411055

EUM <- sum(c1 * prop_dist1, c2 * prop_dist2, c3 * prop_dist3, c4 * prop_dist4, c5 * prop_dist5) / sum(c1, c2, c3, c4, c5)
EUM
plot(EUM)
filen <- "current_agb_eum"
writeRaster(EUM, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

################################################################################

############################### FORMER BIOMASS #################################

### 2. Former biomass uncertainty map, created using the following attached data:
#   List of plot coordinates used to calibrate model: former_agb_plot_coords.csv (attached)
#   Upscaled using climatic variables in the raster �environ_raster_climate_agb.tif� located here: Force_Project/Force_Results/Force_Map
#   Climatic predictors (bands) used in the above raster: bio2+bio3+bio9+bio4+bio18+mwd
#   Relative contribution of each predictor to model: rf_model_importancevalues_climateupscale.csv (attached)

sa <- readOGR(paste(basename, "inputs/Study_Area.shp", sep="/")) # Study area boundary
pred <- stack(paste(basename, "inputs/environ_raster_climate_agb.tif", sep="/")) # Predictor variables
pred <- mask(pred, sa) # Mask predictor variable raster stack by study area boundary
names(pred) <- c("bio2","bio3","bio4","bio18","bio9","mwd","cos","ele","slope")

# Subset rasters in predictor stack
r1 <- subset(pred, 1)
r2 <- subset(pred, 2)
r3 <- subset(pred, 3)
r4 <- subset(pred, 4)
r5 <- subset(pred, 5)
r6 <- subset(pred, 6)

pred <- stack(r1,r2,r3,r4,r5,r6)

# Read in locations of plots used for biomass upscaling
plots <- readOGR("former_agb_plot_coords.shp")

# Extract only pixels in predictor raster stack that coincide with plot locations (i.e. sample used for upscaling = calibration subset)
cal_sub <- mask(pred, plots)

# Get values of calibration subset
vec_sub <- as.data.frame(getValues(cal_sub))
names(vec_sub) <- c("s1","s2","s3","s4","s5","s6") # <- c("bio2","bio3","bio4","bio18","bio9","mwd")
head(vec_sub)

##### EQUATION 1: RELATIVE DISTANCE OF PREDICTORS FROM CALIBRATION ENVELOPE ####

##### PREDICTOR 1 ##########

# Get values of predictor raster 1
vec_pred <- as.data.frame(getValues(r1))
names(vec_pred)[1] <- "p1" # <- c("bio2","bio3","bio4","bio18","bio9","mwd")
head(vec_pred)

# Create empty vector of equal length
outvec1 <- rep(NA, length(vec_pred$p1))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s1                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p1)){

  a <- vec_pred$p1[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p1[i])){outvec1[i] <- NA} else {

  outvec1[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist1 <- raster(r1)
prop_dist1 <- setValues(prop_dist1,outvec1)
prop_dist1
plot(prop_dist1)
filen <- "former_agb_dist_bio2"
writeRaster(prop_dist1, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

##### REPEAT ABOVE CODE FOR REMAINING 5 PREDICTOR VARIABLES #####

##### PREDICTOR 2 #####
# Get values of predictor raster 2
vec_pred <- as.data.frame(getValues(r2))
names(vec_pred)[1] <- "p2" # <- c("bio2","bio3","bio4","bio18","bio9","mwd")
head(vec_pred)

# Create empty vector of equal length
outvec2 <- rep(NA, length(vec_pred$p2))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s2                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p2)){

  a <- vec_pred$p2[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p2[i])){outvec2[i] <- NA} else {

  outvec2[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist2 <- raster(r2)
prop_dist2 <- setValues(prop_dist2,outvec2)
prop_dist2
plot(prop_dist2)
filen <- "former_agb_dist_bio3"
writeRaster(prop_dist2, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

##### PREDICTOR 3 #####
# Get values of predictor raster 3
vec_pred <- as.data.frame(getValues(r3))
names(vec_pred)[1] <- "p3" # <- c("bio2","bio3","bio4","bio18","bio9","mwd")
head(vec_pred)

# Create empty vector of equal length
outvec3 <- rep(NA, length(vec_pred$p3))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s3                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p3)){

  a <- vec_pred$p3[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p3[i])){outvec3[i] <- NA} else {

  outvec3[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist3 <- raster(r3)
prop_dist3 <- setValues(prop_dist3,outvec3)
prop_dist3
plot(prop_dist3)
filen <- "former_agb_dist_bio4"
writeRaster(prop_dist3, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

##### PREDICTOR 4 #####
# Get values of predictor raster 4
vec_pred <- as.data.frame(getValues(r4))
names(vec_pred)[1] <- "p4" # <- c("bio2","bio3","bio4","bio18","bio9","mwd")
head(vec_pred)

# Create empty vector of equal length
outvec4 <- rep(NA, length(vec_pred$p4))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s4                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p4)){

  a <- vec_pred$p4[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p4[i])){outvec4[i] <- NA} else {

  outvec4[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist4 <- raster(r4)
prop_dist4 <- setValues(prop_dist4,outvec4)
prop_dist4
plot(prop_dist4)
filen <- "former_agb_dist_bio18"
writeRaster(prop_dist4, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

##### PREDICTOR 5 #####
# Get values of predictor raster 5
vec_pred <- as.data.frame(getValues(r5))
names(vec_pred)[1] <- "p5" # <- c("bio2","bio3","bio4","bio18","bio9","mwd")
head(vec_pred)

# Create empty vector of equal length
outvec5 <- rep(NA, length(vec_pred$p5))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s5                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p5)){

  a <- vec_pred$p5[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p5[i])){outvec5[i] <- NA} else {

  outvec5[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist5 <- raster(r5)
prop_dist5 <- setValues(prop_dist5,outvec5)
prop_dist5
plot(prop_dist5)
filen <- "former_agb_dist_bio9"
writeRaster(prop_dist5, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

##### PREDICTOR 6 #####
# Get values of predictor raster 6
vec_pred <- as.data.frame(getValues(r6))
names(vec_pred)[1] <- "p6" # <- c("bio2","bio3","bio4","bio18","bio9","mwd")
head(vec_pred)

# Create empty vector of equal length
outvec6 <- rep(NA, length(vec_pred$p6))

# Compute proportional distance of each pixel from calibration envelope for predictor
b <- vec_sub$s6                   # b = predictor values (calibration subset)
mnb <- min(b, na.rm=T)            # mnb <- min value in calibration subset
mxb <- max(b, na.rm=T)            # mxb <- max value in calibration subset
den <- mxb - mnb                  # denominator = max value in calibration subset - min value in calibration subset (i.e. data range)

for (i in 1:length(vec_pred$p6)){

  a <- vec_pred$p6[i]                 # a = predictor values (full)

  if(is.na(vec_pred$p6[i])){outvec6[i] <- NA} else {

  outvec6[i] <- max(c(mnb - a, a - mxb, 0)) / den

  }

}

# Apply values to raster surface
prop_dist6 <- raster(r6)
prop_dist6 <- setValues(prop_dist6,outvec6)
prop_dist6
plot(prop_dist6)
filen <- "former_agb_dist_mwd"
writeRaster(prop_dist6, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

# CREATE RASTER STACK OF PREDICTOR DISTANCES FROM CALIBRATION ENVELOPE
st <- stack(prop_dist1, prop_dist2, prop_dist3, prop_dist4, prop_dist5, prop_dist6)
names(st) <- c("bio2","bio3","bio4","bio18","bio9","mwd")
st
#class      : RasterStack
#dimensions : 3420, 3942, 13481640, 6  (nrow, ncol, ncell, nlayers)
#resolution : 100, 100  (x, y)
#extent     : 660053.1, 1054253, 8886748, 9228748  (xmin, xmax, ymin, ymax)
#crs        : +proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs
#names      :       bio2,       bio3,       bio4,      bio18,       bio9,        mwd
#min values :          0,          0,          0,          0,          0,          0
#max values : 0.55751982, 0.62694191, 1.83604825, 0.98149624, 0.08654952, 1.14651383
plot(st)
filen <- "former_agb_dist_all_predictors"
writeRaster(st, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)


########## EQUATION 2: ENVELOPE UNCERTAINTY MAP ##########

#relative contributions of predictor variables
c1 <- 5.637077553
c2 <- 21.07064133
c3 <- 100
c4 <- 0
c5 <- 46.7337677
c6 <- 82.18595532

EUM <- sum(c1 * prop_dist1, c2 * prop_dist2, c3 * prop_dist3, c4 * prop_dist4, c5 * prop_dist5, c6 * prop_dist6) / sum(c1, c2, c3, c4, c5, c6)
EUM
#class      : RasterLayer
#dimensions : 3420, 3942, 13481640  (nrow, ncol, ncell)
#resolution : 100, 100  (x, y)
#extent     : 660053.1, 1054253, 8886748, 9228748  (xmin, xmax, ymin, ymax)
#crs        : +proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs
#source     : memory
#names      : layer
#values     : 0, 0.7182516  (min, max)
plot(EUM)
filen <- "former_agb_eum"
writeRaster(EUM, paste(outDir,filen,sep="/"),format="GTiff",overwrite=T)

