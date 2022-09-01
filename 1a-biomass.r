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

########################### PREPARE INPUT RASTERS ##############################                                      <- PREPARE INPUT RASTERS

# Ensure all inputs have same CRS, extent, res, etc.

### CRS correct projectRaster code
#maxbio <- projectRaster(maxbio, currbio)
#writeRaster(maxbio, "agbmax_climate_notaggregated.tif", overwrite = T)

### Easiest way is to resample since there are varying CRSs and extents
temp <- raster("Extent_1.tif") # Template raster used to resample other input rasters
temp
#class      : RasterLayer
#dimensions : 3101, 3445, 10682945  (nrow, ncol, ncell)
#resolution : 100, 100  (x, y)
#extent     : 685971.2, 1030471, 8901502, 9211602  (xmin, xmax, ymin, ymax)
#crs        : +proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs
#source     : E:/R/workspaces/UdzKilo_Restoration_Planning/inputs/Extent_1.grd
#names      : Extent_1
#values     : 1, 1  (min, max)
sa <- readOGR("Study_Area.shp") # Mask raster for setting pixel values outside Study Region to NA

### BIOMASS RASTERS
currbio <- raster("agbcurr_1ha_rf_full.tif")
maxbio <- raster("agbmax_climate_notaggregated.tif")
currbio <- resample(currbio, temp, method="bilinear")
maxbio <- resample(maxbio, temp, method="bilinear")
writeRaster(currbio, "agbcurr.tif", overwrite = T)
writeRaster(maxbio, "agbmax.tif", overwrite = T)

currbio <- raster("agbcurr_lower_1ha_rf_full.tif")
maxbio <- raster("agbmax_lower_climate_notaggregated.tif")
currbio <- resample(currbio, temp, method="bilinear")
maxbio <- resample(maxbio, temp, method="bilinear")
writeRaster(currbio, "agbcurr_lower.tif", overwrite = T)
writeRaster(maxbio, "agbmax_lower.tif", overwrite = T)

currbio <- raster("agbcurr_upper_1ha_rf_full.tif")
maxbio <- raster("agbmax_upper_climate_notaggregated.tif")
currbio <- resample(currbio, temp, method="bilinear")
maxbio <- resample(maxbio, temp, method="bilinear")
writeRaster(currbio, "agbcurr_upper.tif", overwrite = T)
writeRaster(maxbio, "agbmax_upper.tif", overwrite = T)

### LANDCOVER
r <- raster("LandCoverFinal_1ha.tif")
r <- resample(r, temp, method='ngb') # using nearest neighbour 'ngb' method (necessary as categorical dataset; 'bilinear' for continuous)
writeRaster(r, "LandCoverFinal_1ha.tif", overwrite=T)
# Extract and save rasters for individial landcover classes:
lc <- ("LandCoverFinal_1ha.tif")
lcfor <- raster(lc)
lcfor[lc==1] <- 1
lcsav <- raster(lc)
lcsav[lc==2] <- 2
lcfp <- raster(lc)
lcfp[lc==3] <- 3
lcag <- raster(lc)
lcag[lc==4] <- 4
lcoth <- raster(lc)
lcoth[lc==5] <- 5
writeRaster(lcfor, filename = "LandCover_Forest.tif", overwrite=T)
writeRaster(lcsav, filename = "LandCover_Savanna.tif", overwrite=T)
writeRaster(lcfp, filename = "LandCover_Floodplain.tif", overwrite=T)
writeRaster(lcag, filename = "LandCover_AgMosaic.tif", overwrite=T)
writeRaster(lcoth, filename = "LandCover_Other.tif", overwrite=T)

# ELEVATION
r <- raster("dem_srtm_43_4414.tif")
r <- resample(r, temp, method='bilinear')
writeRaster(r, "dem_srtm_1ha.tif", overwrite=T)

################################################################################
############################# LANDSCAPE BIOMASS ################################                                   <- LANDSCAPE BIOMASS
################################################################################

################################################################################
############### CALCULATING AGB DEFICIT FROM MAX AND CURRENT AGB ###############                                   <- CALCULATING AGB DEFICIT FROM MAX AND CURRENT AGB

# This section of code computes the difference and percentage difference between
# the current and maximum potential biomass of each landscape pixel. Biomass deficit
# and percentage deficit are also calculated, along with the percentage of maximum
# potential biomass remaining prior to intervention.

# SET WORKING DIRECTORY:
setwd(inDir)

### ENSURE ALL INPUT RASTERS HAVE MATCHING extent, res, crs, origin, etc. ######

### MEAN BIOMASS DEFICIT MAPS (REALISTIC SCENARIO) #############################

### READ IN INPUT RASTERS: SA, CURRBIO & MAXBIO
sa <- readOGR("Study_Area.shp")
currbio <- raster("agbcurr.tif")
maxbio <- raster("agbmax.tif")

### COMPUTE OUTPUT RASTERS: AGBDIFF, PCTDIFF, AGBDEF, PCTDEF, PCTREM
agbdiff <- maxbio - currbio
agbdef <- calc(agbdiff, fun = function(x){x[x < 0] = 0; return(x)})
pctdiff <- overlay(agbdiff, maxbio, fun=function(r1, r2){return(r1/r2)})
pctdef <- overlay(agbdef, maxbio, fun=function(r1, r2){return(r1/r2)})
pctrem <- calc(pctdef, fun=function(x){1-x})

writeRaster(agbdiff, "agbdiff.tif", overwrite = T)
writeRaster(agbdef, "agbdef.tif", overwrite = T)
writeRaster(pctdiff, "agbdiff_pct.tif", overwrite = T)
writeRaster(pctdef, "agbdef_pct.tif", overwrite = T)
writeRaster(pctrem, "agbrem_pct.tif", overwrite = T)

# BIOMASS STACK:
agbs <- stack(currbio, maxbio, agbdiff, pctdiff, agbdef, pctdef, pctrem)
names(agbs) <- c("currbio", "maxbio", "agbdiff", "pctdiff", "agbdef", "pctdef", "pctrem")
agbs
writeRaster(agbs, "biomass_stack.grd", overwrite=T)

### UPPER BIOMASS DEFICIT MAPS (PESSIMISTIC SCENARIO) ##########################
### Largest deficit = lower current - upper max

### READ IN INPUT RASTERS: SA, CURRBIO & MAXBIO
sa <- readOGR("Study_Area.shp")
currbio <- raster("agbcurr_lower.tif")
maxbio <- raster("agbmax_upper.tif")

### COMPUTE OUTPUT RASTERS: AGBDIFF, PCTDIFF, AGBDEF, PCTDEF, PCTREM

agbdiff <- maxbio - currbio
agbdef <- calc(agbdiff, fun = function(x){x[x < 0] = 0; return(x)})
pctdiff <- overlay(agbdiff, maxbio, fun=function(r1, r2){return(r1/r2)})
pctdef <- overlay(agbdef, maxbio, fun=function(r1, r2){return(r1/r2)})
pctrem <- calc(pctdef, fun=function(x){1-x})

writeRaster(agbdiff, "agbdiff_pess.tif", overwrite = T)
writeRaster(agbdef, "agbdef_pess.tif", overwrite = T)
writeRaster(pctdiff, "agbdiff_pct_pess.tif", overwrite = T)
writeRaster(pctdef, "agbdef_pct_pess.tif", overwrite = T)
writeRaster(pctrem, "agbrem_pct_pess.tif", overwrite = T)

# BIOMASS STACK:
agbs <- stack(currbio, maxbio, agbdiff, pctdiff, agbdef, pctdef, pctrem)
names(agbs) <- c("currbio", "maxbio", "agbdiff", "pctdiff", "agbdef", "pctdef", "pctrem")
agbs
writeRaster(agbs, "biomass_stack_pess.grd", overwrite=T)

### LOWER BIOMASS DEFICIT MAPS (OPTIMISTIC SCENARIO) ###########################
### Lowest deficit = upper current - lower max

### READ IN INPUT RASTERS: SA, CURRBIO & MAXBIO
sa <- readOGR("Study_Area.shp")
currbio <- raster("agbcurr_upper.tif")
maxbio <- raster("agbmax_lower.tif")

### COMPUTE OUTPUT RASTERS: AGBDIFF, PCTDIFF, AGBDEF, PCTDEF, PCTREM

agbdiff <- maxbio - currbio
agbdef <- calc(agbdiff, fun = function(x){x[x < 0] = 0; return(x)})
pctdiff <- overlay(agbdiff, maxbio, fun=function(r1, r2){return(r1/r2)})
pctdef <- overlay(agbdef, maxbio, fun=function(r1, r2){return(r1/r2)})
pctrem <- calc(pctdef, fun=function(x){1-x})

writeRaster(agbdiff, "agbdiff_opt.tif", overwrite = T)
writeRaster(agbdef, "agbdef_opt.tif", overwrite = T)
writeRaster(pctdiff, "agbdiff_pct_opt.tif", overwrite = T)
writeRaster(pctdef, "agbdef_pct_opt.tif", overwrite = T)
writeRaster(pctrem, "agbrem_pct_opt.tif", overwrite = T)

# BIOMASS STACK:
agbs <- stack(currbio, maxbio, agbdiff, pctdiff, agbdef, pctdef, pctrem)
names(agbs) <- c("currbio", "maxbio", "agbdiff", "pctdiff", "agbdef", "pctdef", "pctrem")
agbs
writeRaster(agbs, "biomass_stack_opt.grd", overwrite=T)

################################################################################
############## ESTIMATED AGB GAIN AND YEARS TO FULL AGB RECOVERY ###############                                   <- ESTIMATED AGB GAIN AND YEARS TO FULL AGB RECOVERY

# This section of code computes the number of years to full AGB recovery,
# AGB gain, and % deficit recovered (after 5 years and to full recovery).

# NOTE:
# Herein, this version of code works from mean current and max AGB rasters across all scenarios
# rather than using upper and lower 95% CI rasters for pessimistic and realistic scenarios.
# This is useful for when optimistic AGB deficit maps generated by
# subtracting the lower 95% CI maximum AGB map from the upper 95% CI current AGB map
# results in too few pixels with AGB deficit/regeneration or restoration potential (in our case just 8)
# for meaningful further analysis of restoration methods, costs and cost-effectiveness
# across the landscape.

# SET WORKING DIRECTORY:
setwd(inDir)

# Read in inputs from working directory
sa <- readOGR("Study_Area.shp")
ext <- raster("Extent_1.tif")

agbdata <- read.csv("agbgain-burnal-afr-regen-meanagb.csv")
head(agbdata)
#  Year    MeanAGB   MaxAGB     MinAGB MeanAGB_pct MaxAGB_pct  MinAGB_pct
#1    0  0.0000000 30.57121 -30.171583 0.000000000 0.05688371 -0.15664417
#2    1  0.6537201 29.90979 -28.219918 0.002293252 0.05565301 -0.14651156
#3    2  2.8347511 30.83995 -24.804367 0.009944315 0.05738376 -0.12877878
#4    3  6.4916941 33.31762 -19.983571 0.022772881 0.06199395 -0.10375027
#5    4 11.4522329 37.17806 -13.937308 0.040174465 0.06917705 -0.07235941
#6    5 17.5185015 42.23183  -6.871772 0.061454952 0.07858057 -0.03567672
#  Change_Mean Change_Max Change_Min
#1   0.6537201  0.0000000   1.951665
#2   2.1810310  0.9301605   3.415550
#3   3.6569430  2.4776740   4.820797
#4   4.9605388  3.8604343   6.046263
#5   6.0662686  5.0537662   7.065536
#6   6.9758790  6.0604087   7.879382

############################## BIOMASS GAIN ####################################

########## REALISTIC ###########################################################

cagb <- raster("agbcurr.tif")
magb <- raster("agbmax.tif")
cpct <- cagb / magb * 100
cpct[cpct >= 100] <- NA

cbn <- 0.456 # Set Carbon : AGB ratio (R = 0.456; P = 0.453; O = 0.459)
# Martin et al 2018 47.6% global average.
# For tropical angiosperms = 45.6% ± 0.2 Lower 95% CI = 45.3%, Higher = 45.9%

y <- 20
r1 <- 7.282732287        # Rate of gain 0-20y
r2 <- 2.56123302656164   # Rate of gain 21-max
# Rates of gain based on Burnal's data for natural regeneration in moist African forests:
# 0-20y: P = 7.02219569596378; R = 7.282732287; O = 7.57821570391423
# 20+y: P = 1.53633848987282; R = 2.56123302656164; O = 4.10957277372204

# What % maximum AGB is reached after 20 years of natural regeneration in African forests?
# Burnal data: Mean % at 20 years = 51; Max = 32; Min = 63. I.e. Min = Optimistic (higher % recovered quicker); Max = Pessimistic; Mean = Realistic.
n <- agbdata$MeanAGB_pct[agbdata$Year[y+2]] # Returns THIS[when this[is this]]
                                            # +2 because count num rows rather than cell value
# Multiple this by our max AGB values
est20 <- magb * n

st <- stack(cagb, magb, cpct, est20)
names(st) <- c("cagb", "magb", "cpct", "est20")
st <- mask(st, sa)

vecagb <- as.data.frame(st)

# Create vector with # years remaining to equivalent 20 years recovery based on Burnal's data

Yto20 <- rep(NA, length(vecagb$cagb))

for (i in 1:length(vecagb$cagb)){

  cb <- vecagb$cagb[i]
  mb <- vecagb$magb[i]
  cp <- vecagb$cpct[i]
  b20 <- vecagb$est20[i]

  if(is.na(cb)){Yto20[i] <- NA} else {

   ifelse(cb < b20, Yto20[i] <- (b20 - cb) / r1, Yto20[i] <- 0)

   }

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, Yto20)
outr
plot(outr)

# Add output as column in data frame
Yto20 <- as.data.frame(Yto20)
vecagb <- do.call(cbind, list(vecagb, Yto20))

# Create vector with estimated AGB after 20 years of intervention
agb20 <- rep(NA, length(vecagb$cagb))
for (i in 1:length(vecagb$cagb)){

ifelse(vecagb$Yto20[i] > 0, agb20[i] <- vecagb$cagb[i] + (vecagb$Yto20[i] * r1), agb20[i] <- vecagb$cagb[i])

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, agb20)
outr
plot(outr)

# Add output as column in data frame
agb20 <- as.data.frame(agb20)
vecagb <- do.call(cbind, list(vecagb, agb20))

# Create vector with # years remaining to equivalent MAX years recovery (y21 -> yMAX) based on Burnal's data

Y21toMAX <- rep(NA, length(vecagb$cagb))

for (i in 1:length(vecagb$cagb)){

  cb <- vecagb$cagb[i]
  mb <- vecagb$magb[i]
  cp <- vecagb$cpct[i]
  b20 <- vecagb$est20[i]
  y20 <- vecagb$Yto20[i]
  cb20 <- vecagb$agb20[i]

  if(is.na(cb)){Y21toMAX[i] <- NA} else {

   ifelse(cb20 < mb, Y21toMAX[i] <- (mb - cb20) / r2, Y21toMAX[i] <- 0)

   }

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, Y21toMAX)
outr
plot(outr)

# Add output as column in data frame
Y21toMAX <- as.data.frame(Y21toMAX)
vecagb <- do.call(cbind, list(vecagb, Y21toMAX))

# Calculate total # years to max AGB (i.e. full recovery)
YtoMAX <- vecagb$Yto20 + vecagb$Y21toMAX

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, YtoMAX)
outr
plot(outr)

# Add output as column in data frame
YtoMAX <- as.data.frame(YtoMAX)
vecagb <- do.call(cbind, list(vecagb, YtoMAX))

# Create vector with estimated AGB after full recovery # This should == magb and we're all good!
#estMax <- vecagb$agb20 + (vecagb$Y21toMAX * r2)

estMax <- rep(NA, length(vecagb$cagb))
for (i in 1:length(vecagb$cagb)){

ifelse(vecagb$Y21toMAX[i] > 0, estMax[i] <- vecagb$agb20[i] + (vecagb$Y21toMAX[i] * r2), estMax[i] <- vecagb$agb20[i])

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, estMax)
outr
plot(outr)

# Add output as column in data frame
estMax <- as.data.frame(estMax)
vecagb <- do.call(cbind, list(vecagb, estMax))

# Check == MaxAGB
test <- vecagb$magb - estMax
mean(test$estMax, na.rm=T)   # Should ==0 (not actually the case because in some instances cagb > magb)

outr <- raster(cagb)
outr <- setValues(outr, vecagb$YtoMAX)
outr
plot(outr)

writeRaster(outr, paste(outG, "years-to-recovery-R.tif", sep="/"), overwrite=T)

##### ESTIMATE AGB GAIN AFTER 5 YEARS INVESTMENT

head(vecagb)

agbGain5y <- rep(NA, length(vecagb$cagb))
for (i in 1:length(vecagb$cagb)){

ifelse(vecagb$Yto20[i] >= 5, agbGain5y[i] <- 5 * r1, agbGain5y[i] <- vecagb$Yto20[i] * r1 + (5 - vecagb$Yto20[i]) * r2)

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, agbGain5y)
outr
plot(outr)
writeRaster(outr, paste(outG, "agbGain_R5.tif", sep="/"), overwrite=T)

# Add output as column in data frame
agbGain5y <- as.data.frame(agbGain5y)
vecagb <- do.call(cbind, list(vecagb, agbGain5y))

##### ESTIMATE AGB GAIN TO FULL RECOVERY

outr <- magb - cagb
outr[outr<0] <- 0
outr
plot(outr)
outr <- mask(outr, sa)
writeRaster(outr, paste(outG, "agbGain_Rfull.tif", sep="/"), overwrite=T)

# Add output as column in data frame
agbGainFull <- as.data.frame(outr)
vecagb <- do.call(cbind, list(vecagb, agbGainFull))
names(vecagb)[names(vecagb) == "layer"] <- "agbGainFull"
head(vecagb)

########## PESSIMISTIC #########################################################

cagb <- raster("agbcurr.tif")
magb <- raster("agbmax.tif")
cpct <- cagb / magb * 100
cpct[cpct >= 100] <- NA

cbn <- 0.453 # Set Carbon : AGB ratio (R = 0.456; P = 0.453; O = 0.459)
# Martin et al 2018 47.6% global average.
# For tropical angiosperms = 45.6% ± 0.2 Lower 95% CI = 45.3%, Higher = 45.9%

y <- 20
r1 <- 7.02219569596378        # Rate of gain 0-20y
r2 <- 1.53633848987282        # Rate of gain 21-max
# Rates of gain based on Burnal's data for natural regeneration in moist African forests:
# 0-20y: P = 7.02219569596378; R = 7.282732287; O = 7.57821570391423
# 20+y: P = 1.53633848987282; R = 2.56123302656164; O = 4.10957277372204

# What % maximum AGB is reached after 20 years of natural regeneration in African forests?
n <- agbdata$MaxAGB_pct[agbdata$Year[y+2]] # Returns THIS[when this[is this]]
                                           # +2 because count num rows rather than cell value
# Multiple this by our max AGB values
est20 <- magb * n

st <- stack(cagb, magb, cpct, est20)
names(st) <- c("cagb", "magb", "cpct", "est20")
st <- mask(st, sa)

vecagb <- as.data.frame(st)

# Create vector with # years remaining to equivalent 20 years recovery based on Burnal's data

Yto20 <- rep(NA, length(vecagb$cagb))

for (i in 1:length(vecagb$cagb)){

  cb <- vecagb$cagb[i]
  mb <- vecagb$magb[i]
  cp <- vecagb$cpct[i]
  b20 <- vecagb$est20[i]

  if(is.na(cb)){Yto20[i] <- NA} else {

   ifelse(cb < b20, Yto20[i] <- (b20 - cb) / r1, Yto20[i] <- 0)

   }

}

# Check by setting values to raster and plotting
#outr <- raster(cagb)
#outr <- setValues(outr, Yto20)
#outr
#plot(outr)

# Add output as column in data frame
Yto20 <- as.data.frame(Yto20)
vecagb <- do.call(cbind, list(vecagb, Yto20))

# Create vector with estimated AGB after 20 years of intervention
agb20 <- rep(NA, length(vecagb$cagb))
for (i in 1:length(vecagb$cagb)){

ifelse(vecagb$Yto20[i] > 0, agb20[i] <- vecagb$cagb[i] + (vecagb$Yto20[i] * r1), agb20[i] <- vecagb$cagb[i])

}

# Check by setting values to raster and plotting
#outr <- raster(cagb)
#outr <- setValues(outr, agb20)
#outr
#plot(outr)

# Add output as column in data frame
agb20 <- as.data.frame(agb20)
vecagb <- do.call(cbind, list(vecagb, agb20))

# Create vector with # years remaining to equivalent MAX years recovery (y21 -> yMAX) based on Burnal's data

Y21toMAX <- rep(NA, length(vecagb$cagb))

for (i in 1:length(vecagb$cagb)){

  cb <- vecagb$cagb[i]
  mb <- vecagb$magb[i]
  cp <- vecagb$cpct[i]
  b20 <- vecagb$est20[i]
  y20 <- vecagb$Yto20[i]
  cb20 <- vecagb$agb20[i]

  if(is.na(cb)){Y21toMAX[i] <- NA} else {

   ifelse(cb20 < mb, Y21toMAX[i] <- (mb - cb20) / r2, Y21toMAX[i] <- 0)

   }

}

# Check by setting values to raster and plotting
#outr <- raster(cagb)
#outr <- setValues(outr, Y21toMAX)
#outr
#plot(outr)

# Add output as column in data frame
Y21toMAX <- as.data.frame(Y21toMAX)
vecagb <- do.call(cbind, list(vecagb, Y21toMAX))

# Calculate total # years to max AGB (i.e. full recovery)
YtoMAX <- vecagb$Yto20 + vecagb$Y21toMAX

# Check by setting values to raster and plotting
#outr <- raster(cagb)
#outr <- setValues(outr, YtoMAX)
#outr
#plot(outr)

# Add output as column in data frame
YtoMAX <- as.data.frame(YtoMAX)
vecagb <- do.call(cbind, list(vecagb, YtoMAX))

# Create vector with estimated AGB after full recovery # This should == magb and we're all good!
#estMax <- vecagb$agb20 + (vecagb$Y21toMAX * r2)

estMax <- rep(NA, length(vecagb$cagb))
for (i in 1:length(vecagb$cagb)){

ifelse(vecagb$Y21toMAX[i] > 0, estMax[i] <- vecagb$agb20[i] + (vecagb$Y21toMAX[i] * r2), estMax[i] <- vecagb$agb20[i])

}

# Check by setting values to raster and plotting
#outr <- raster(cagb)
#outr <- setValues(outr, estMax)
#outr
#plot(outr)

# Add output as column in data frame
estMax <- as.data.frame(estMax)
vecagb <- do.call(cbind, list(vecagb, estMax))

# Check == MaxAGB
test <- vecagb$magb - estMax
mean(test$estMax, na.rm=T)   # Should ==0 (not actually the case because in some instances cagb > magb)

outr <- raster(cagb)
outr <- setValues(outr, vecagb$YtoMAX)
outr
plot(outr)

writeRaster(outr, paste(outG, "years-to-recovery-P.tif", sep="/"), overwrite=T)

##### ESTIMATE AGB GAIN AFTER 5 YEARS INVESTMENT

head(vecagb)

agbGain5y <- rep(NA, length(vecagb$cagb))
for (i in 1:length(vecagb$cagb)){

ifelse(vecagb$Yto20[i] >= 5, agbGain5y[i] <- 5 * r1, agbGain5y[i] <- vecagb$Yto20[i] * r1 + (5 - vecagb$Yto20[i]) * r2)

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, agbGain5y)
outr
plot(outr)
writeRaster(outr, paste(outG, "agbGain_P5.tif", sep="/"), overwrite=T)

# Add output as column in data frame
agbGain5y <- as.data.frame(agbGain5y)
vecagb <- do.call(cbind, list(vecagb, agbGain5y))

##### ESTIMATE AGB GAIN TO FULL RECOVERY

outr <- magb - cagb
outr[outr<0] <- 0
outr
plot(outr)
outr <- mask(outr, sa)
writeRaster(outr, paste(outG, "agbGain_Pfull.tif", sep="/"), overwrite=T)

# Add output as column in data frame
agbGainFull <- as.data.frame(outr)
vecagb <- do.call(cbind, list(vecagb, agbGainFull))
names(vecagb)[names(vecagb) == "layer"] <- "agbGainFull"
head(vecagb)

########## OPTIMISTIC ##########################################################

cagb <- raster("agbcurr.tif")
magb <- raster("agbmax.tif")
cpct <- cagb / magb * 100
cpct[cpct >= 100] <- NA

cbn <- 0.459 # Set Carbon : AGB ratio (R = 0.456; P = 0.453; O = 0.459)
# Martin et al 2018 47.6% global average.
# For tropical angiosperms = 45.6% ± 0.2 Lower 95% CI = 45.3%, Higher = 45.9%

y <- 20
r1 <- 7.57821570391423   # Rate of gain 0-20y
r2 <- 4.10957277372204   # Rate of gain 21-max
# Rates of gain based on Burnal's data for natural regeneration in moist African forests:
# 0-20y: P = 7.02219569596378; R = 7.282732287; O = 7.57821570391423
# 20+y: P = 1.53633848987282; R = 2.56123302656164; O = 4.10957277372204

# What % maximum AGB is reached after 20 years of natural regeneration in African forests?
n <- agbdata$MinAGB_pct[agbdata$Year[y+2]] # Returns THIS[when this[is this]]
                                           # +2 because count num rows rather than cell value
# Multiple this by our max AGB values
est20 <- magb * n

st <- stack(cagb, magb, cpct, est20)
names(st) <- c("cagb", "magb", "cpct", "est20")
st <- mask(st, sa)

vecagb <- as.data.frame(st)

# Create vector with # years remaining to equivalent 20 years recovery based on Burnal's data

Yto20 <- rep(NA, length(vecagb$cagb))

for (i in 1:length(vecagb$cagb)){

  cb <- vecagb$cagb[i]
  mb <- vecagb$magb[i]
  cp <- vecagb$cpct[i]
  b20 <- vecagb$est20[i]

  if(is.na(cb)){Yto20[i] <- NA} else {

   ifelse(cb < b20, Yto20[i] <- (b20 - cb) / r1, Yto20[i] <- 0)

   }

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, Yto20)
outr
plot(outr)

# Add output as column in data frame
Yto20 <- as.data.frame(Yto20)
vecagb <- do.call(cbind, list(vecagb, Yto20))

# Create vector with estimated AGB after 20 years of intervention
agb20 <- rep(NA, length(vecagb$cagb))
for (i in 1:length(vecagb$cagb)){

ifelse(vecagb$Yto20[i] > 0, agb20[i] <- vecagb$cagb[i] + (vecagb$Yto20[i] * r1), agb20[i] <- vecagb$cagb[i])

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, agb20)
outr
plot(outr)

# Add output as column in data frame
agb20 <- as.data.frame(agb20)
vecagb <- do.call(cbind, list(vecagb, agb20))

# Create vector with # years remaining to equivalent MAX years recovery (y21 -> yMAX) based on Burnal's data

Y21toMAX <- rep(NA, length(vecagb$cagb))

for (i in 1:length(vecagb$cagb)){

  cb <- vecagb$cagb[i]
  mb <- vecagb$magb[i]
  cp <- vecagb$cpct[i]
  b20 <- vecagb$est20[i]
  y20 <- vecagb$Yto20[i]
  cb20 <- vecagb$agb20[i]

  if(is.na(cb)){Y21toMAX[i] <- NA} else {

   ifelse(cb20 < mb, Y21toMAX[i] <- (mb - cb20) / r2, Y21toMAX[i] <- 0)

   }

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, Y21toMAX)
outr
plot(outr)

# Add output as column in data frame
Y21toMAX <- as.data.frame(Y21toMAX)
vecagb <- do.call(cbind, list(vecagb, Y21toMAX))

# Calculate total # years to max AGB (i.e. full recovery)
YtoMAX <- vecagb$Yto20 + vecagb$Y21toMAX

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, YtoMAX)
outr
plot(outr)

# Add output as column in data frame
YtoMAX <- as.data.frame(YtoMAX)
vecagb <- do.call(cbind, list(vecagb, YtoMAX))

# Create vector with estimated AGB after full recovery # This should == magb and we're all good!
#estMax <- vecagb$agb20 + (vecagb$Y21toMAX * r2)

estMax <- rep(NA, length(vecagb$cagb))
for (i in 1:length(vecagb$cagb)){

ifelse(vecagb$Y21toMAX[i] > 0, estMax[i] <- vecagb$agb20[i] + (vecagb$Y21toMAX[i] * r2), estMax[i] <- vecagb$agb20[i])

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, estMax)
outr
plot(outr)

# Add output as column in data frame
estMax <- as.data.frame(estMax)
vecagb <- do.call(cbind, list(vecagb, estMax))

# Check == MaxAGB
test <- vecagb$magb - estMax
mean(test$estMax, na.rm=T)   # Should ==0 (not actually the case because in some instances cagb > magb)

outr <- raster(cagb)
outr <- setValues(outr, vecagb$YtoMAX)
outr
plot(outr)

writeRaster(outr, paste(outG, "years-to-recovery-O.tif", sep="/"), overwrite=T)

##### ESTIMATE AGB GAIN AFTER 5 YEARS INVESTMENT

head(vecagb)

agbGain5y <- rep(NA, length(vecagb$cagb))
for (i in 1:length(vecagb$cagb)){

ifelse(vecagb$Yto20[i] >= 5, agbGain5y[i] <- 5 * r1, agbGain5y[i] <- vecagb$Yto20[i] * r1 + (5 - vecagb$Yto20[i]) * r2)

}

# Check by setting values to raster and plotting
outr <- raster(cagb)
outr <- setValues(outr, agbGain5y)
outr
plot(outr)
writeRaster(outr, paste(outG, "agbGain_O5.tif", sep="/"), overwrite=T)

# Add output as column in data frame
agbGain5y <- as.data.frame(agbGain5y)
vecagb <- do.call(cbind, list(vecagb, agbGain5y))

##### ESTIMATE AGB GAIN TO FULL RECOVERY

outr <- magb - cagb
outr[outr<0] <- 0
outr
plot(outr)
outr <- mask(outr, sa)
outr

writeRaster(outr, paste(outG, "agbGain_Ofull.tif", sep="/"), overwrite=T)

# Add output as column in data frame
agbGainFull <- as.data.frame(outr)
vecagb <- do.call(cbind, list(vecagb, agbGainFull))
names(vecagb)[names(vecagb) == "layer"] <- "agbGainFull"
head(vecagb)

############################# AGB GAIN STACK ###################################

# Mask rasters to differentiate zero and NA values:
P <- raster(paste(outA, "restoreP.grd", sep="/"))
R <- raster(paste(outA, "restoreR.grd", sep="/"))
O <- raster(paste(outA, "restoreO.grd", sep="/"))

### Set input work directory
setwd(outG)

# Carbon gain rasters, mixed cropping on 08/05/20
r1 <- raster("agbGain_P5.tif")
r2 <- raster("agbGain_R5.tif")
r3 <- raster("agbGain_O5.tif")
r4 <- raster("agbGain_Pfull.tif")
r5 <- raster("agbGain_Rfull.tif")
r6 <- raster("agbGain_Ofull.tif")

# Masked to only include area warranting restoration intervention under the various scenarios
r1 <- mask(r1, P)
r4 <- mask(r4, P)
r2 <- mask(r2, R)
r5 <- mask(r5, R)
r3 <- mask(r3, O)
r6 <- mask(r6, O)

st <- stack(r1,r2,r3,r4,r5,r6)
names(st) <- c("P5","R5","O5","PF","RF","OF")
writeRaster(st, "agbgain-byscenario.stack.grd", overwrite=T)

############################ DEFICIT RECOVERED #################################

### Set input work directory
setwd(inDir)

d1 <- raster("agbdef.tif")
d2 <- raster("agbdef.tif")
d3 <- raster("agbdef.tif")
d1[d1==0] <- NA
d2[d2==0] <- NA
d3[d3==0] <- NA

p1 <- r1 / d1
p1[p1>1] <- 1
p2 <- r2 / d2
p2[p2>1] <- 1
p3 <- r3 / d3
p3[p3>1] <- 1
p1
p2
p3

setwd(outG)
writeRaster(p1, "pctdefrec_P5.tif", overwrite=T)
writeRaster(p2, "pctdefrec_R5.tif", overwrite=T)
writeRaster(p3, "pctdefrec_O5.tif", overwrite=T)

############################# YEARS TO FULL RECOVERY ###########################

### Set input work directory
setwd(outG)

# Carbon gain rasters, mixed cropping on 08/05/20
r1 <- raster("years-to-recovery-P.tif")
r2 <- raster("years-to-recovery-R.tif")
r3 <- raster("years-to-recovery-O.tif")
r1
r2
r3
