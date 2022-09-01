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

# BIOMASS GAIN
outG <- paste(outDir,"agb-gain",sep="/")
if (file.exists(outG)){
  cat("\n check directory agb-gain")
} else {
  dir.create(file.path(outG))
}

# COST-EFFECTIVENESS
outCE<-paste(outDir,"cost-effectiveness",sep="/")
if (file.exists(outCE)){
  cat("\n check directory cost-effectiveness")
} else {
  dir.create(file.path(outCE))
}

################################################################################
############################# COST EFFECTIVENESS ###############################                         <- COST EFFECTIVENESS

# We determined expected cost-effectiveness for each landscape pixel  as follows:
#AGB$i = (?AGBi * p) / $i	Equation 1

# Where for time period i, AGB$ was the cost-effectiveness, i.e. expected biomass gain per US$(Mg.US$-1) / carbon sequestration per US$ (kg.US$-1),
# ?AGB was the expected change in biomass/carbon
# p was the expected probability of successful recovery of the full biomass deficit,
# and $ was the expected cost in US$, modified from Firn et al. (2015).

### Set input work directory
setwd(outA)

# Mask rasters to differentiate zero and NA values:
relP <- raster(paste(outA, "restoreP.grd", sep="/"))
relR <- raster(paste(outA, "restoreR.grd", sep="/"))
relO <- raster(paste(outA, "restoreO.grd", sep="/"))

# Multipliers to convert AGB to AGC, i.e. AGB * cbn = AGC:
# R = 0.456; P = 0.453; O = 0.459) Martin et al 2018 47.6% global average. For tropical angiosperms = 45.6% ± 0.2 Lower 95% CI = 45.3%, Higher = 45.9%
cbnP <- 0.453
cbnR <- 0.456
cbnO <- 0.459

p <- 1
cf <- 3.67 # Mutiply stored Carbon figure by conversion factor of 3.67 to determine CO2e
# Multiply by estimated carbon market price ($US/MgCO2e)
# https://www.worldbank.org/en/results/2017/12/01/carbon-pricing
P <- 10                          # $20 per ton CO2e under Realistic scenario; 10 under Pessimistic; 40 under Optimistic
R <- 20
O <- 40

################################################################################
### Set input work directory
setwd(outG)

# Biomass and Carbon rasters:
AGBi5P <- raster("agbGain_P5.tif")
AGBiFP <- raster("agbGain_Pfull.tif")
AGBi5P <- mask(AGBi5P, relP)
AGBiFP <- mask(AGBiFP, relP)

AGBi5R <- raster("agbGain_R5.tif")
AGBiFR <- raster("agbGain_Rfull.tif")
AGBi5R <- mask(AGBi5R, relR)
AGBiFR <- mask(AGBiFR, relR)

AGBi5O <- raster("agbGain_O5.tif")
AGBiFO <- raster("agbGain_Ofull.tif")
AGBi5O <- mask(AGBi5O, relO)
AGBiFO <- mask(AGBiFO, relO)

############################ COST EFFECTIVENESS ################################

### 5 YEARS ####################################################################
### Set input work directory
setwd(outC5)
# Costs rasters (ALL COSTS - INCL STAFF, ADMIN & TRANSPORT, created using code: costs-overall-and-stacks-bymethod)
cost5P <- raster("totals/total-cost-P5USD.tif")
cost5R <- raster("totals/total-cost-R5USD.tif")
cost5O <- raster("totals/total-cost-O5USD.tif")

### Set output work directory
setwd(outCE)

### PESSIMISTIC
AGBi <- (AGBi5P * p) / cost5P
AGBi100 <- AGBi * 100
writeRaster(AGBi, "AGBgainUSD1_P5.tif", overwrite=T)
writeRaster(AGBi100, "AGBgainUSD100_P5.tif", overwrite=T)
AGCi <- (AGCi5P * p) / cost5P
writeRaster(AGCi, "AGCgainUSD1_P5.tif", overwrite=T)

# Expected MgCO2e gain per USD1 per ha
CO2e <- AGCi * cf
CO2e
writeRaster(CO2e, "CO2gainUSD1_P5.tif", overwrite=T)

# Multiplied by expected market price per MgCO2e
CO2eMP <- CO2e * P
CO2eMP
writeRaster(CO2eMP, "CO2gainMarketPrice_P5.tif", overwrite=T)

### REALISTIC
AGBi <- (AGBi5R * p) / cost5R
AGBi100 <- AGBi * 100
writeRaster(AGBi, "AGBgainUSD1_R5.tif", overwrite=T)
writeRaster(AGBi100, "AGBgainUSD100_R5.tif", overwrite=T)
AGCi <- (AGCi5R * p) / cost5R
writeRaster(AGCi, "AGCgainUSD1_R5.tif", overwrite=T)

# Expected MgCO2e gain per USD1 per ha
CO2e <- AGCi * cf
CO2e
writeRaster(CO2e, "CO2gainUSD1_R5.tif", overwrite=T)

# Multiplied by expected market price per MgCO2e
CO2eMR <- CO2e * R
CO2eMR
writeRaster(CO2eMR, "CO2gainMarketPrice_R5.tif", overwrite=T)

### OPTIMISTIC
AGBi <- (AGBi5O * p) / cost5O
AGBi100 <- AGBi * 100
writeRaster(AGBi, "AGBgainUSD1_O5.tif", overwrite=T)
writeRaster(AGBi100, "AGBgainUSD100_O5.tif", overwrite=T)
AGCi <- (AGCi5O * p) / cost5O
writeRaster(AGCi, "AGCgainUSD1_O5.tif", overwrite=T)

# Expected MgCO2e gain per USD1 per ha
CO2e <- AGCi * cf
CO2e
writeRaster(CO2e, "CO2gainUSD1_O5.tif", overwrite=T)

# Multiplied by expected market price per MgCO2e
CO2eMO <- CO2e * O
CO2eMO
writeRaster(CO2eMO, "CO2gainMarketPrice_O5.tif", overwrite=T)

### FULL RECOVERY ##############################################################
### Set input work directory
setwd(outCR)
# Costs rasters (ALL COSTS - INCL STAFF, ADMIN & TRANSPORT)
costFP <- raster("totals/total-cost-PFUSD.tif")
costFR <- raster("totals/total-cost-RFUSD.tif")
costFO <- raster("totals/total-cost-OFUSD.tif")

### Set output work directory
setwd(outCE)

### PESSIMISTIC
AGBi <- (AGBiFP * p) / costFP
AGBi100 <- AGBi * 100
writeRaster(AGBi, "AGBgainUSD1_PF.tif", overwrite=T)
writeRaster(AGBi100, "AGBgainUSD100_PF.tif", overwrite=T)
AGCi <- (AGCi5P * p) / cost5P
writeRaster(AGCi, "AGCgainUSD1_PF.tif", overwrite=T)

# Expected MgCO2e gain per USD1 per ha
CO2e <- AGCi * cf
CO2e
writeRaster(CO2e, "CO2gainUSD1_PF.tif", overwrite=T)

# Multiplied by expected market price per MgCO2e
CO2eMP <- CO2e * P
CO2eMP
writeRaster(CO2eMP, "CO2gainMarketPrice_PF.tif", overwrite=T)

### REALISTIC
AGBi <- (AGBiFR * p) / costFR
AGBi100 <- AGBi * 100
writeRaster(AGBi, "AGBgainUSD1_RF.tif", overwrite=T)
writeRaster(AGBi100, "AGBgainUSD100_RF.tif", overwrite=T)
AGCi <- (AGCiFR * p) / costFR
writeRaster(AGCi, "AGCgainUSD1_RF.tif", overwrite=T)

# Expected MgCO2e gain per USD1 per ha
CO2e <- AGCi * cf
CO2e
writeRaster(CO2e, "CO2gainUSD1_RF.tif", overwrite=T)

# Multiplied by expected market price per MgCO2e
CO2eMR <- CO2e * R
CO2eMR
writeRaster(CO2eMR, "CO2gainMarketPrice_RF.tif", overwrite=T)

### OPTIMISTIC
AGBi <- (AGBiFO * p) / costFO
AGBi100 <- AGBi * 100
writeRaster(AGBi, "AGBgainUSD1_OF.tif", overwrite=T)
writeRaster(AGBi100, "AGBgainUSD100_OF.tif", overwrite=T)
AGCi <- (AGCiFO * p) / costFO
writeRaster(AGCi, "AGCgainUSD1_OF.tif", overwrite=T)

# Expected MgCO2e gain per USD1 per ha
CO2e <- AGCi * cf
CO2e
writeRaster(CO2e, "CO2gainUSD1_OF.tif", overwrite=T)

# Multiplied by expected market price per MgCO2e
CO2eMO <- CO2e * O
CO2eMO
writeRaster(CO2eMO, "CO2gainMarketPrice_OF.tif", overwrite=T)



######################## COST EFFECTIVENESS STACK ##############################

# Mask rasters to differentiate zero and NA values:
P <- raster(paste(outA, "restoreP.grd", sep="/"))
R <- raster(paste(outA, "restoreR.grd", sep="/"))
O <- raster(paste(outA, "restoreO.grd", sep="/"))

############################ AGB GAIN / US$100 #################################
### Set input work directory
setwd(outCE)

r1 <- raster("AGBgainUSD100_P5.tif")
r2 <- raster("AGBgainUSD100_R5.tif")
r3 <- raster("AGBgainUSD100_O5.tif")
r4 <- raster("AGBgainUSD100_PF.tif")
r5 <- raster("AGBgainUSD100_RF.tif")
r6 <- raster("AGBgainUSD100_OF.tif")

st <- stack(r1,r2,r3,r4,r5,r6)
names(st) <- c("P5","R5","O5","PF","RF","OF")
st

writeRaster(st, "agbgainUSD100-byscenario.stack.grd", overwrite=T)


### Set input work directory
setwd(outA)

# Mask rasters to differentiate zero and NA values:
relP <- raster(paste(outA, "restoreP.grd", sep="/"))
relR <- raster(paste(outA, "restoreR.grd", sep="/"))
relO <- raster(paste(outA, "restoreO.grd", sep="/"))

################################################################################
### Set output work directory
setwd(outCE)

##### 5 YEARS
##### PESS
r1 <- raster("AGBgainUSD1_P5.tif")
# Calculate median of full dataset (Q2)
med <- cellStats(r1, median)
med
# Use median to separate upper half of data
up <- r1
up[up<med] <- NA
# Calculate median of upper half of data (Q3)
med2 <- cellStats(up, median)
# Use median to separate upper quartile
q3 <- up
q3[q3<med2] <- NA
q3
# Convert to binary and compute freq pixels
mask <- q3
mask[mask>0] <- 1
mask <- ratify(mask, count=T)

writeRaster(q3, "cost-eff_top25val_P5.tif", overwrite=T)
writeRaster(mask, "cost-eff_top25bin_P5.tif", overwrite=T)

##### REAL
r1 <- raster("AGBgainUSD1_R5.tif")
# Calculate median of full dataset (Q2)
med <- cellStats(r1, median)
med
# Use median to separate upper half of data
up <- r1
up[up<med] <- NA
# Calculate median of upper half of data (Q3)
med2 <- cellStats(up, median)
# Use median to separate upper quartile
q3 <- up
q3[q3<med2] <- NA
q3
# Convert to binary and compute freq pixels
mask <- q3
mask[mask>0] <- 1
mask <- ratify(mask, count=T)

writeRaster(q3, "cost-eff_top25val_R5.tif", overwrite=T)
writeRaster(mask, "cost-eff_top25bin_R5.tif", overwrite=T)

##### OPT
r1 <- raster("AGBgainUSD1_O5.tif")
# Calculate median of full dataset (Q2)
med <- cellStats(r1, median)
med
# Use median to separate upper half of data
up <- r1
up[up<med] <- NA
# Calculate median of upper half of data (Q3)
med2 <- cellStats(up, median)
# Use median to separate upper quartile
q3 <- up
q3[q3<med2] <- NA
q3
# Convert to binary and compute freq pixels
mask <- q3
mask[mask>0] <- 1
mask <- ratify(mask, count=T)

writeRaster(q3, "cost-eff_top25val_O5.tif", overwrite=T)
writeRaster(mask, "cost-eff_top25bin_O5.tif", overwrite=T)

##### F YEARS
##### PESS
r1 <- raster("AGBgainUSD1_PF.tif")
# Calculate median of full dataset (Q2)
med <- cellStats(r1, median)
med
# Use median to separate upper half of data
up <- r1
up[up<med] <- NA
# Calculate median of upper half of data (Q3)
med2 <- cellStats(up, median)
# Use median to separate upper quartile
q3 <- up
q3[q3<med2] <- NA
q3
# Convert to binary and compute freq pixels
mask <- q3
mask[mask>0] <- 1
mask <- ratify(mask, count=T)

writeRaster(q3, "cost-eff_top25val_PF.tif", overwrite=T)
writeRaster(mask, "cost-eff_top25bin_PF.tif", overwrite=T)

##### REAL
r1 <- raster("AGBgainUSD1_RF.tif")
# Calculate median of full dataset (Q2)
med <- cellStats(r1, median)
med
# Use median to separate upper half of data
up <- r1
up[up<med] <- NA
# Calculate median of upper half of data (Q3)
med2 <- cellStats(up, median)
# Use median to separate upper quartile
q3 <- up
q3[q3<med2] <- NA
q3
# Convert to binary and compute freq pixels
mask <- q3
mask[mask>0] <- 1
mask <- ratify(mask, count=T)

writeRaster(q3, "cost-eff_top25val_RF.tif", overwrite=T)
writeRaster(mask, "cost-eff_top25bin_RF.tif", overwrite=T)

##### OPT
r1 <- raster("AGBgainUSD1_OF.tif")
# Calculate median of full dataset (Q2)
med <- cellStats(r1, median)
med
# Use median to separate upper half of data
up <- r1
up[up<med] <- NA
# Calculate median of upper half of data (Q3)
med2 <- cellStats(up, median)
# Use median to separate upper quartile
q3 <- up
q3[q3<med2] <- NA
q3
# Convert to binary and compute freq pixels
mask <- q3
mask[mask>0] <- 1
mask <- ratify(mask, count=T)

writeRaster(q3, "cost-eff_top25val_OF.tif", overwrite=T)
writeRaster(mask, "cost-eff_top25bin_OF.tif", overwrite=T)

######################## COST EFFECTIVENESS STACK ##############################

r1 <- raster("cost-eff_top25bin_P5.tif")
r2 <- raster("cost-eff_top25bin_R5.tif")
r3 <- raster("cost-eff_top25bin_O5.tif")
r4 <- raster("cost-eff_top25bin_PF.tif")
r5 <- raster("cost-eff_top25bin_RF.tif")
r6 <- raster("cost-eff_top25bin_OF.tif")

r7 <- r1 + r2 + r3
r7[r7==3] <- 1

writeRaster(r7, "cost-eff_top25bin_5Y.tif", overwrite=T)

r8 <- r3 + r4 + r5
r8[r8==3] <- 1

writeRaster(r8, "cost-eff_top25bin_Full.tif", overwrite=T)

r9 <- r1 + r2 + r3 + r4 + r5 + r6
r9[r9==6] <- 1

writeRaster(r9, "cost-eff_top25bin.tif", overwrite=T)

st <- stack(r1,r2,r3,r4,r5,r6,r7,r8,r9)
names(st) <- c("P5","R5","O5","PF","RF","OF","5Y","Full","All")
writeRaster(st, "effectiveness-top25-byscenario.stack.grd", overwrite=T)

