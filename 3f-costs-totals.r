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
####################### TOTAL COSTS PER RESTORATION METHOD #####################                                <- TOTAL COSTS PER RESTORATION METHOD

# This code combines Lab, Eqp, Transport, Admin costs
# for each restoration method into 1 cost raster for that method

### Set input work directory
setwd(inDir)

# Read in mask rasters:
sa <- readOGR("Study_Area.shp")                      # Study area
relP <- raster(paste(outA, "restoreP.grd", sep="/")) # Pixels with restoration potential
relR <- raster(paste(outA, "restoreR.grd", sep="/"))
relO <- raster(paste(outA, "restoreO.grd", sep="/"))

################################# 5 YEARS ######################################

# Exchange rate:
rate <- 0.00043

# Admin rate
vp <- 0.12
vr <- 0.11
vo <- 0.10

### Set output work directory
setwd(outC5)

### PESSIMISTIC ################################################################

# Land - note land cost is the only one initially estimated in USD, hence converting back to TZS
r <- raster("landCostP.tif")
a <- r * vp
r <- r + a
r <- mask(r, relP)
writeRaster(a, "totals/bymethod/land_adminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminP5USD.tif", overwrite=T)
a <- a / rate
r <- r / rate
writeRaster(a, "totals/bymethod/land_adminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminP5TZS.tif", overwrite=T)

# Passive monitoring
# (assume cost same as ANR, monitoring costs for ANR included under respective methods)
# (other passive restoration costs = land purchase, community engagement and staff, which are covered separately)
# (total passive costs raster created in costs-totals-per-approach)
rel <- raster(paste(outA, "passiveP.grd", sep="/"))
r1 <- raster("monANRCostP.tif")                   # ANR monitoring to be applied across multiple
r2 <- raster("transport/monANR_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
st <- stack(r1, r2)
st <- mask(st, rel)
r <- sum(st)
writeRaster(r1, "totals/bymethod/passive_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminP5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/passive_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminP5USD.tif", overwrite=T)

# Vines
r1 <- raster("vnCostP.tif")
r2 <- raster("transport/vines_tsptTotP.tif")
r3 <- raster("monANRCostP.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
r3 <- r3 + r3 * vp
r4 <- r4 + r4 * vp
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/vines_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminP5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/vines_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminP5USD.tif", overwrite=T)

# Herbs/shrubs
r1 <- raster("sbCostP.tif")
r2 <- raster("transport/shrub_tsptTotP.tif")
r3 <- raster("monANRCostP.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
r3 <- r3 + r3 * vp
r4 <- r4 + r4 * vp
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminP5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminP5USD.tif", overwrite=T)

# Lantana
r1 <- raster("ltCostP.tif")
r2 <- raster("transport/lant_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/lant_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminP5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/lant_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminP5USD.tif", overwrite=T)

# Grass
r1 <- raster("gsCostP.tif")
r2 <- raster("transport/grass_tsptTotP.tif")
r3 <- raster("monANRCostP.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
r3 <- r3 + r3 * vp
r4 <- r4 + r4 * vp
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/grass_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminP5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/grass_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminP5USD.tif", overwrite=T)

# Firebreaks
r1 <- raster("fbCostP.tif")
r2 <- raster("transport/fireb_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminP5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminP5USD.tif", overwrite=T)

# Soil
r1 <- raster("slCostP.tif") # includes ploughing and fertiliser application
r2 <- raster("transport/soil_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/soil_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminP5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/soil_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminP5USD.tif", overwrite=T)

# FSA planting (including enrichment planting under pessimistic scenario)
r1 <- raster("nurseryFSACostP.tif") # includes procurement, establishment and management
r2 <- raster("fsCostP.tif")
r3 <- raster("fsamCostP.tif") # includes weeding, fertiliser, watering, (thinning and enrichment planting)
r4 <- raster("monFSACostP.tif")
r5 <- raster("fmCostP.tif")
r6 <- raster("enrCostP.tif")
r7 <- raster("enrmCostP.tif")
r8 <- raster("monEnrCostP.tif")
st1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st1 <- st1 + st1 * vp
st1 <- mask(st1, r1)
r9 <- sum(st1)

r1 <- raster("transport/fsa_onrdTotP.tif")
r2 <- raster("transport/fsa_offrdTotP.tif")
r3 <- raster("transport/monFSA_tsptTotP.tif")
r4 <- raster("transport/mgmtFSA_tsptTotP.tif")
r5 <- raster("transport/enr_onrdTotP.tif")
r6 <- raster("transport/enr_offrdTotP.tif")
r7 <- raster("transport/monEnr_tsptTotP.tif")
r8 <- raster("transport/mgmtEnr_tsptTotP.tif")

st2 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st2 <- st2 + st2 * vp
st2 <- mask(st2, r1)
r10 <- sum(st2)

st <- stack(r9, r10)
r <- sum(st)

writeRaster(r9, "totals/bymethod/fsa_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminP5TZS.tif", overwrite=T)
r9 <- r9 * rate
r10 <- r10 * rate
r <- r * rate
writeRaster(r9, "totals/bymethod/fsa_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminP5USD.tif", overwrite=T)

# Nurse trees
r1 <- raster("nurseryNseCostP.tif")
r2 <- raster("nsCostP.tif")
r3 <- raster("nursemCostP.tif")
r4 <- raster("monNseCostP.tif")
r5 <- raster("fmCostP.tif")

r6 <- raster("transport/nse_onrdTotP.tif")
r7 <- raster("transport/nse_offrdTotP.tif")
r8 <- raster("transport/monNse_tsptTotP.tif")
r9 <- raster("transport/mgmtNse_tsptTotP.tif")

st <- stack(r1, r2, r3, r4, r5, r6, r7, r8, r9)
st <- st + st * vp
st <- mask(st, r1)
r <- sum(st)
st1 <- stack(r1, r2, r3, r4, r5)
st1 <- st1 + st1 * vp
st1 <- mask(st1, r1)
r1 <- sum(st1)
st2 <- stack(r6, r7, r8, r9)
st2 <- st2 + st2 * vp
st2 <- mask(st2, r1)
r2 <- sum(st2)

writeRaster(r1, "totals/bymethod/nurse_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminP5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/nurse_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminP5USD.tif", overwrite=T)

# Community engagement
r <- raster("ceCostP.tif")
r <- r + r * vp
writeRaster(r, "totals/bymethod/commeng_labEqpAdminP5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminP5TZS.tif", overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/commeng_labEqpAdminP5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminP5USD.tif", overwrite=T)

# Staff (no admin costs added for staff)
r <- raster("staffCostP.tif")
r <- mask(r, relP)
writeRaster(r, "totals/bymethod/staff_labEqpAdminP5TZS.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminP5TZS.tif",overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/staff_labEqpAdminP5USD.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminP5USD.tif",overwrite=T)

### REALISTIC ##################################################################

# Land - note land cost is the only one initially estimated in USD, hence converting back to TZS
r <- raster("landCostR.tif")
a <- r * vr
r <- r + a
r <- mask(r, relR)
writeRaster(a, "totals/bymethod/land_adminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminR5USD.tif", overwrite=T)
a <- a / rate
r <- r / rate
writeRaster(a, "totals/bymethod/land_adminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminR5TZS.tif", overwrite=T)

# Passive monitoring
# (assume cost same as ANR, monitoring costs for ANR included under respective methods)
# (other passive restoration costs = land purchase, community engagement and staff, which are covered separately)
# (total passive costs raster created in costs-totals-per-approach)
rel <- raster(paste(outA, "passiveR.grd", sep="/"))
r1 <- raster("monANRCostR.tif")                   # ANR monitoring to be applied across multiple
r2 <- raster("transport/monANR_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
st <- stack(r1, r2)
st <- mask(st, rel)
r <- sum(st)
writeRaster(r1, "totals/bymethod/passive_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminR5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/passive_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminR5USD.tif", overwrite=T)

# Vines
r1 <- raster("vnCostR.tif")
r2 <- raster("transport/vines_tsptTotR.tif")
r3 <- raster("monANRCostR.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
r3 <- r3 + r3 * vr
r4 <- r4 + r4 * vr
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/vines_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminR5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/vines_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminR5USD.tif", overwrite=T)

# Herbs/shrubs
r1 <- raster("sbCostR.tif")
r2 <- raster("transport/shrub_tsptTotR.tif")
r3 <- raster("monANRCostR.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
r3 <- r3 + r3 * vr
r4 <- r4 + r4 * vr
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminR5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminR5USD.tif", overwrite=T)

# Lantana
r1 <- raster("ltCostR.tif")
r2 <- raster("transport/lant_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/lant_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminR5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/lant_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminR5USD.tif", overwrite=T)

# Grass
r1 <- raster("gsCostR.tif")
r2 <- raster("transport/grass_tsptTotR.tif")
r3 <- raster("monANRCostR.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
r3 <- r3 + r3 * vr
r4 <- r4 + r4 * vr
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/grass_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminR5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/grass_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminR5USD.tif", overwrite=T)

# Firebreaks
r1 <- raster("fbCostR.tif")
r2 <- raster("transport/fireb_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminR5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminR5USD.tif", overwrite=T)

# Soil
r1 <- raster("slCostR.tif") # includes ploughing and fertiliser application
r2 <- raster("transport/soil_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/soil_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminR5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/soil_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminR5USD.tif", overwrite=T)

# FSA planting (including enrichment planting under pessimistic scenario)
r1 <- raster("nurseryFSACostR.tif") # includes procurement, establishment and management
r2 <- raster("fsCostR.tif")
r3 <- raster("fsamCostR.tif") # includes weeding, fertiliser, watering, (thinning and enrichment planting)
r4 <- raster("monFSACostR.tif")
r5 <- raster("fmCostR.tif")
r6 <- raster("enrCostR.tif")
r7 <- raster("enrmCostR.tif")
r8 <- raster("monEnrCostR.tif")
st1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st1 <- st1 + st1 * vr
st1 <- mask(st1, r1)
r9 <- sum(st1)

r1 <- raster("transport/fsa_onrdTotR.tif")
r2 <- raster("transport/fsa_offrdTotR.tif")
r3 <- raster("transport/monFSA_tsptTotR.tif")
r4 <- raster("transport/mgmtFSA_tsptTotR.tif")
r5 <- raster("transport/enr_onrdTotR.tif")
r6 <- raster("transport/enr_offrdTotR.tif")
r7 <- raster("transport/monEnr_tsptTotR.tif")
r8 <- raster("transport/mgmtEnr_tsptTotR.tif")

st2 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st2 <- st2 + st2 * vr
st2 <- mask(st2, r1)
r10 <- sum(st2)

st <- stack(r9, r10)
r <- sum(st)

writeRaster(r9, "totals/bymethod/fsa_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminR5TZS.tif", overwrite=T)
r9 <- r9 * rate
r10 <- r10 * rate
r <- r * rate
writeRaster(r9, "totals/bymethod/fsa_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminR5USD.tif", overwrite=T)

# Nurse trees
r1 <- raster("nurseryNseCostR.tif")
r2 <- raster("nsCostR.tif")
r3 <- raster("nursemCostR.tif")
r4 <- raster("monNseCostR.tif")
r5 <- raster("fmCostR.tif")

r6 <- raster("transport/nse_onrdTotR.tif")
r7 <- raster("transport/nse_offrdTotR.tif")
r8 <- raster("transport/monNse_tsptTotR.tif")
r9 <- raster("transport/mgmtNse_tsptTotR.tif")

st <- stack(r1, r2, r3, r4, r5, r6, r7, r8, r9)
st <- st + st * vr
st <- mask(st, r1)
r <- sum(st)
st1 <- stack(r1, r2, r3, r4, r5)
st1 <- st1 + st1 * vr
st1 <- mask(st1, r1)
r1 <- sum(st1)
st2 <- stack(r6, r7, r8, r9)
st2 <- st2 + st2 * vr
st2 <- mask(st2, r1)
r2 <- sum(st2)

writeRaster(r1, "totals/bymethod/nurse_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminR5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/nurse_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminR5USD.tif", overwrite=T)

# Community engagement
r <- raster("ceCostR.tif")
r <- r + r * vr
writeRaster(r, "totals/bymethod/commeng_labEqpAdminR5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminR5TZS.tif", overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/commeng_labEqpAdminR5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminR5USD.tif", overwrite=T)

# Staff (no admin costs added for staff)
r <- raster("staffCostR.tif")
r <- mask(r, relR)
writeRaster(r, "totals/bymethod/staff_labEqpAdminR5TZS.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminR5TZS.tif",overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/staff_labEqpAdminR5USD.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminR5USD.tif",overwrite=T)

### OPTIMISTIC #################################################################

# Land - note land cost is the only one initially estimated in USD, hence converting back to TZS
r <- raster("landCostO.tif")
a <- r * vo
r <- r + a
r <- mask(r, relO)
writeRaster(a, "totals/bymethod/land_adminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminO5USD.tif", overwrite=T)
a <- a / rate
r <- r / rate
writeRaster(a, "totals/bymethod/land_adminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminO5TZS.tif", overwrite=T)

# Passive monitoring
# (assume cost same as ANR, monitoring costs for ANR included under respective methods)
# (other passive restoration costs = land purchase, community engagement and staff, which are covered separately)
# (total passive costs raster created in costs-totals-per-approach)
rel <- raster(paste(outA, "passiveO.grd", sep="/"))
r1 <- raster("monANRCostO.tif")                   # ANR monitoring to be applied across multiple
r2 <- raster("transport/monANR_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
st <- stack(r1, r2)
st <- mask(st, rel)
r <- sum(st)
writeRaster(r1, "totals/bymethod/passive_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminO5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/passive_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminO5USD.tif", overwrite=T)

# Vines
r1 <- raster("vnCostO.tif")
r2 <- raster("transport/vines_tsptTotO.tif")
r3 <- raster("monANRCostO.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
r3 <- r3 + r3 * vo
r4 <- r4 + r4 * vo
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/vines_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminO5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/vines_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminO5USD.tif", overwrite=T)

# Herbs/shrubs
r1 <- raster("sbCostO.tif")
r2 <- raster("transport/shrub_tsptTotO.tif")
r3 <- raster("monANRCostO.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
r3 <- r3 + r3 * vo
r4 <- r4 + r4 * vo
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminO5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminO5USD.tif", overwrite=T)

# Lantana
r1 <- raster("ltCostO.tif")
r2 <- raster("transport/lant_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/lant_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminO5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/lant_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminO5USD.tif", overwrite=T)

# Grass
r1 <- raster("gsCostO.tif")
r2 <- raster("transport/grass_tsptTotO.tif")
r3 <- raster("monANRCostO.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
r3 <- r3 + r3 * vo
r4 <- r4 + r4 * vo
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/grass_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminO5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/grass_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminO5USD.tif", overwrite=T)

# Firebreaks
r1 <- raster("fbCostO.tif")
r2 <- raster("transport/fireb_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminO5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminO5USD.tif", overwrite=T)

# Soil
r1 <- raster("slCostO.tif") # includes ploughing and fertiliser application
r2 <- raster("transport/soil_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/soil_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminO5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/soil_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminO5USD.tif", overwrite=T)

# FSA planting (including enrichment planting under pessimistic scenario)
r1 <- raster("nurseryFSACostO.tif") # includes procurement, establishment and management
r2 <- raster("fsCostO.tif")
r3 <- raster("fsamCostO.tif") # includes weeding, fertiliser, watering, (thinning and enrichment planting)
r4 <- raster("monFSACostO.tif")
r5 <- raster("fmCostO.tif")
r6 <- raster("enrCostO.tif")
r7 <- raster("enrmCostO.tif")
r8 <- raster("monEnrCostO.tif")
st1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st1 <- st1 + st1 * vo
st1 <- mask(st1, r1)
r9 <- sum(st1)

r1 <- raster("transport/fsa_onrdTotO.tif")
r2 <- raster("transport/fsa_offrdTotO.tif")
r3 <- raster("transport/monFSA_tsptTotO.tif")
r4 <- raster("transport/mgmtFSA_tsptTotO.tif")
r5 <- raster("transport/enr_onrdTotO.tif")
r6 <- raster("transport/enr_offrdTotO.tif")
r7 <- raster("transport/monEnr_tsptTotO.tif")
r8 <- raster("transport/mgmtEnr_tsptTotO.tif")

st2 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st2 <- st2 + st2 * vo
st2 <- mask(st2, r1)
r10 <- sum(st2)

st <- stack(r9, r10)
r <- sum(st)

writeRaster(r9, "totals/bymethod/fsa_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminO5TZS.tif", overwrite=T)
r9 <- r9 * rate
r10 <- r10 * rate
r <- r * rate
writeRaster(r9, "totals/bymethod/fsa_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminO5USD.tif", overwrite=T)

# Nurse trees
r1 <- raster("nurseryNseCostO.tif")
r2 <- raster("nsCostO.tif")
r3 <- raster("nursemCostO.tif")
r4 <- raster("monNseCostO.tif")
r5 <- raster("fmCostO.tif")

r6 <- raster("transport/nse_onrdTotO.tif")
r7 <- raster("transport/nse_offrdTotO.tif")
r8 <- raster("transport/monNse_tsptTotO.tif")
r9 <- raster("transport/mgmtNse_tsptTotO.tif")

st <- stack(r1, r2, r3, r4, r5, r6, r7, r8, r9)
st <- st + st * vo
st <- mask(st, r1)
r <- sum(st)
st1 <- stack(r1, r2, r3, r4, r5)
st1 <- st1 + st1 * vo
st1 <- mask(st1, r1)
r1 <- sum(st1)
st2 <- stack(r6, r7, r8, r9)
st2 <- st2 + st2 * vo
st2 <- mask(st2, r1)
r2 <- sum(st2)

writeRaster(r1, "totals/bymethod/nurse_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminO5TZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/nurse_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminO5USD.tif", overwrite=T)

# Community engagement
r <- raster("ceCostO.tif")
r <- r + r * vo
writeRaster(r, "totals/bymethod/commeng_labEqpAdminO5TZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminO5TZS.tif", overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/commeng_labEqpAdminO5USD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminO5USD.tif", overwrite=T)

# Staff (no admin costs added for staff)
r <- raster("staffCostO.tif")
r <- mask(r, relO)
writeRaster(r, "totals/bymethod/staff_labEqpAdminO5TZS.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminO5TZS.tif",overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/staff_labEqpAdminO5USD.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminO5USD.tif",overwrite=T)

################################################################################
############################## FULL RECOVERY ###################################

# Exchange rate:
rate <- 0.00043

# Admin rate
vp <- 0.12
vr <- 0.11
vo <- 0.10

### Set output work directory
setwd(outCR)

### PESSIMISTIC ################################################################

# Land - note land cost is the only one initially estimated in USD, hence converting back to TZS
r <- raster("landCostP.tif")
a <- r * vp
r <- r + a
r <- mask(r, relP)
writeRaster(a, "totals/bymethod/land_adminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminPFUSD.tif", overwrite=T)
a <- a / rate
r <- r / rate
writeRaster(a, "totals/bymethod/land_adminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminPFTZS.tif", overwrite=T)

# Passive monitoring
# (assume cost same as ANR, monitoring costs for ANR included under respective methods)
# (other passive restoration costs = land purchase, community engagement and staff, which are covered separately)
# (total passive costs raster created in costs-totals-per-approach)
rel <- raster(paste(outA, "passiveP.grd", sep="/"))
r1 <- raster("monANRCostP.tif")                   # ANR monitoring to be applied across multiple
r2 <- raster("transport/monANR_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
st <- stack(r1, r2)
st <- mask(st, rel)
r <- sum(st)
writeRaster(r1, "totals/bymethod/passive_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminPFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/passive_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminPFUSD.tif", overwrite=T)

# Vines
r1 <- raster("vnCostP.tif")
r2 <- raster("transport/vines_tsptTotP.tif")
r3 <- raster("monANRCostP.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
r3 <- r3 + r3 * vp
r4 <- r4 + r4 * vp
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/vines_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminPFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/vines_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminPFUSD.tif", overwrite=T)

# Herbs/shrubs
r1 <- raster("sbCostP.tif")
r2 <- raster("transport/shrub_tsptTotP.tif")
r3 <- raster("monANRCostP.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
r3 <- r3 + r3 * vp
r4 <- r4 + r4 * vp
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminPFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminPFUSD.tif", overwrite=T)

# Lantana
r1 <- raster("ltCostP.tif")
r2 <- raster("transport/lant_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/lant_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminPFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/lant_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminPFUSD.tif", overwrite=T)

# Grass
r1 <- raster("gsCostP.tif")
r2 <- raster("transport/grass_tsptTotP.tif")
r3 <- raster("monANRCostP.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
r3 <- r3 + r3 * vp
r4 <- r4 + r4 * vp
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/grass_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminPFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/grass_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminPFUSD.tif", overwrite=T)

# Firebreaks
r1 <- raster("fbCostP.tif")
r2 <- raster("transport/fireb_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminPFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminPFUSD.tif", overwrite=T)

# Soil
r1 <- raster("slCostP.tif") # includes ploughing and fertiliser application
r2 <- raster("transport/soil_tsptTotP.tif")
r1 <- r1 + r1 * vp
r2 <- r2 + r2 * vp
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/soil_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminPFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/soil_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminPFUSD.tif", overwrite=T)

# FSA planting (including enrichment planting under pessimistic scenario)
r1 <- raster("nurseryFSACostP.tif") # includes procurement, establishment and management
r2 <- raster("fsCostP.tif")
r3 <- raster("fsamCostP.tif") # includes weeding, fertiliser, watering, (thinning and enrichment planting)
r4 <- raster("monFSACostP.tif")
r5 <- raster("fmCostP.tif")
r6 <- raster("enrCostP.tif")
r7 <- raster("enrmCostP.tif")
r8 <- raster("monEnrCostP.tif")
st1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st1 <- st1 + st1 * vp
st1 <- mask(st1, r1)
r9 <- sum(st1)

r1 <- raster("transport/fsa_onrdTotP.tif")
r2 <- raster("transport/fsa_offrdTotP.tif")
r3 <- raster("transport/monFSA_tsptTotP.tif")
r4 <- raster("transport/mgmtFSA_tsptTotP.tif")
r5 <- raster("transport/enr_onrdTotP.tif")
r6 <- raster("transport/enr_offrdTotP.tif")
r7 <- raster("transport/monEnr_tsptTotP.tif")
r8 <- raster("transport/mgmtEnr_tsptTotP.tif")

st2 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st2 <- st2 + st2 * vp
st2 <- mask(st2, r1)
r10 <- sum(st2)

st <- stack(r9, r10)
r <- sum(st)

writeRaster(r9, "totals/bymethod/fsa_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminPFTZS.tif", overwrite=T)
r9 <- r9 * rate
r10 <- r10 * rate
r <- r * rate
writeRaster(r9, "totals/bymethod/fsa_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminPFUSD.tif", overwrite=T)

# Nurse trees
r1 <- raster("nurseryNseCostP.tif")
r2 <- raster("nsCostP.tif")
r3 <- raster("nursemCostP.tif")
r4 <- raster("monNseCostP.tif")
r5 <- raster("fmCostP.tif")

r6 <- raster("transport/nse_onrdTotP.tif")
r7 <- raster("transport/nse_offrdTotP.tif")
r8 <- raster("transport/monNse_tsptTotP.tif")
r9 <- raster("transport/mgmtNse_tsptTotP.tif")

st <- stack(r1, r2, r3, r4, r5, r6, r7, r8, r9)
st <- st + st * vp
st <- mask(st, r1)
r <- sum(st)
st1 <- stack(r1, r2, r3, r4, r5)
st1 <- st1 + st1 * vp
st1 <- mask(st1, r1)
r1 <- sum(st1)
st2 <- stack(r6, r7, r8, r9)
st2 <- st2 + st2 * vp
st2 <- mask(st2, r1)
r2 <- sum(st2)

writeRaster(r1, "totals/bymethod/nurse_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminPFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/nurse_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminPFUSD.tif", overwrite=T)

# Community engagement
r <- raster("ceCostP.tif")
r <- r + r * vp
writeRaster(r, "totals/bymethod/commeng_labEqpAdminPFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminPFTZS.tif", overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/commeng_labEqpAdminPFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminPFUSD.tif", overwrite=T)

# Staff (no admin costs added for staff)
r <- raster("staffCostP.tif")
r <- mask(r, relP)
writeRaster(r, "totals/bymethod/staff_labEqpAdminPFTZS.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminPFTZS.tif",overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/staff_labEqpAdminPFUSD.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminPFUSD.tif",overwrite=T)

### REALISTIC ##################################################################

# Land - note land cost is the only one initially estimated in USD, hence converting back to TZS
r <- raster("landCostR.tif")
a <- r * vr
r <- r + a
r <- mask(r, relR)
writeRaster(a, "totals/bymethod/land_adminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminRFUSD.tif", overwrite=T)
a <- a / rate
r <- r / rate
writeRaster(a, "totals/bymethod/land_adminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminRFTZS.tif", overwrite=T)

# Passive monitoring
# (assume cost same as ANR, monitoring costs for ANR included under respective methods)
# (other passive restoration costs = land purchase, community engagement and staff, which are covered separately)
# (total passive costs raster created in costs-totals-per-approach)
rel <- raster(paste(outA, "passiveR.grd", sep="/"))
r1 <- raster("monANRCostR.tif")                   # ANR monitoring to be applied across multiple
r2 <- raster("transport/monANR_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
st <- stack(r1, r2)
st <- mask(st, rel)
r <- sum(st)
writeRaster(r1, "totals/bymethod/passive_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminRFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/passive_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminRFUSD.tif", overwrite=T)

# Vines
r1 <- raster("vnCostR.tif")
r2 <- raster("transport/vines_tsptTotR.tif")
r3 <- raster("monANRCostR.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
r3 <- r3 + r3 * vr
r4 <- r4 + r4 * vr
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/vines_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminRFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/vines_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminRFUSD.tif", overwrite=T)

# Herbs/shrubs
r1 <- raster("sbCostR.tif")
r2 <- raster("transport/shrub_tsptTotR.tif")
r3 <- raster("monANRCostR.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
r3 <- r3 + r3 * vr
r4 <- r4 + r4 * vr
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminRFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminRFUSD.tif", overwrite=T)

# Lantana
r1 <- raster("ltCostR.tif")
r2 <- raster("transport/lant_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/lant_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminRFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/lant_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminRFUSD.tif", overwrite=T)

# Grass
r1 <- raster("gsCostR.tif")
r2 <- raster("transport/grass_tsptTotR.tif")
r3 <- raster("monANRCostR.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
r3 <- r3 + r3 * vr
r4 <- r4 + r4 * vr
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/grass_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminRFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/grass_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminRFUSD.tif", overwrite=T)

# Firebreaks
r1 <- raster("fbCostR.tif")
r2 <- raster("transport/fireb_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminRFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminRFUSD.tif", overwrite=T)

# Soil
r1 <- raster("slCostR.tif") # includes ploughing and fertiliser application
r2 <- raster("transport/soil_tsptTotR.tif")
r1 <- r1 + r1 * vr
r2 <- r2 + r2 * vr
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/soil_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminRFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/soil_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminRFUSD.tif", overwrite=T)

# FSA planting (including enrichment planting under pessimistic scenario)
r1 <- raster("nurseryFSACostR.tif") # includes procurement, establishment and management
r2 <- raster("fsCostR.tif")
r3 <- raster("fsamCostR.tif") # includes weeding, fertiliser, watering, (thinning and enrichment planting)
r4 <- raster("monFSACostR.tif")
r5 <- raster("fmCostR.tif")
r6 <- raster("enrCostR.tif")
r7 <- raster("enrmCostR.tif")
r8 <- raster("monEnrCostR.tif")
st1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st1 <- st1 + st1 * vr
st1 <- mask(st1, r1)
r9 <- sum(st1)

r1 <- raster("transport/fsa_onrdTotR.tif")
r2 <- raster("transport/fsa_offrdTotR.tif")
r3 <- raster("transport/monFSA_tsptTotR.tif")
r4 <- raster("transport/mgmtFSA_tsptTotR.tif")
r5 <- raster("transport/enr_onrdTotR.tif")
r6 <- raster("transport/enr_offrdTotR.tif")
r7 <- raster("transport/monEnr_tsptTotR.tif")
r8 <- raster("transport/mgmtEnr_tsptTotR.tif")

st2 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st2 <- st2 + st2 * vr
st2 <- mask(st2, r1)
r10 <- sum(st2)

st <- stack(r9, r10)
r <- sum(st)

writeRaster(r9, "totals/bymethod/fsa_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminRFTZS.tif", overwrite=T)
r9 <- r9 * rate
r10 <- r10 * rate
r <- r * rate
writeRaster(r9, "totals/bymethod/fsa_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminRFUSD.tif", overwrite=T)

# Nurse trees
r1 <- raster("nurseryNseCostR.tif")
r2 <- raster("nsCostR.tif")
r3 <- raster("nursemCostR.tif")
r4 <- raster("monNseCostR.tif")
r5 <- raster("fmCostR.tif")

r6 <- raster("transport/nse_onrdTotR.tif")
r7 <- raster("transport/nse_offrdTotR.tif")
r8 <- raster("transport/monNse_tsptTotR.tif")
r9 <- raster("transport/mgmtNse_tsptTotR.tif")

st <- stack(r1, r2, r3, r4, r5, r6, r7, r8, r9)
st <- st + st * vr
st <- mask(st, r1)
r <- sum(st)
st1 <- stack(r1, r2, r3, r4, r5)
st1 <- st1 + st1 * vr
st1 <- mask(st1, r1)
r1 <- sum(st1)
st2 <- stack(r6, r7, r8, r9)
st2 <- st2 + st2 * vr
st2 <- mask(st2, r1)
r2 <- sum(st2)

writeRaster(r1, "totals/bymethod/nurse_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminRFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/nurse_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminRFUSD.tif", overwrite=T)

# Community engagement
r <- raster("ceCostR.tif")
r <- r + r * vr
writeRaster(r, "totals/bymethod/commeng_labEqpAdminRFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminRFTZS.tif", overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/commeng_labEqpAdminRFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminRFUSD.tif", overwrite=T)

# Staff (no admin costs added for staff)
r <- raster("staffCostR.tif")
r <- mask(r, relR)
writeRaster(r, "totals/bymethod/staff_labEqpAdminRFTZS.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminRFTZS.tif",overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/staff_labEqpAdminRFUSD.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminRFUSD.tif",overwrite=T)

### OPTIMISTIC #################################################################

# Land - note land cost is the only one initially estimated in USD, hence converting back to TZS
r <- raster("landCostO.tif")
a <- r * vo
r <- r + a
r <- mask(r, relO)
writeRaster(a, "totals/bymethod/land_adminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminOFUSD.tif", overwrite=T)
a <- a / rate
r <- r / rate
writeRaster(a, "totals/bymethod/land_adminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/land_labEqpTsptAdminOFTZS.tif", overwrite=T)

# Passive monitoring
# (assume cost same as ANR, monitoring costs for ANR included under respective methods)
# (other passive restoration costs = land purchase, community engagement and staff, which are covered separately)
# (total passive costs raster created in costs-totals-per-approach)
rel <- raster(paste(outA, "passiveO.grd", sep="/"))
r1 <- raster("monANRCostO.tif")                   # ANR monitoring to be applied across multiple
r2 <- raster("transport/monANR_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
st <- stack(r1, r2)
st <- mask(st, rel)
r <- sum(st)
writeRaster(r1, "totals/bymethod/passive_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminOFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/passive_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/passive_tsptAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/passive_labEqpTsptAdminOFUSD.tif", overwrite=T)

# Vines
r1 <- raster("vnCostO.tif")
r2 <- raster("transport/vines_tsptTotO.tif")
r3 <- raster("monANRCostO.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
r3 <- r3 + r3 * vo
r4 <- r4 + r4 * vo
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/vines_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminOFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/vines_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/vines_tsptAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/vines_labEqpTsptAdminOFUSD.tif", overwrite=T)

# Herbs/shrubs
r1 <- raster("sbCostO.tif")
r2 <- raster("transport/shrub_tsptTotO.tif")
r3 <- raster("monANRCostO.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
r3 <- r3 + r3 * vo
r4 <- r4 + r4 * vo
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminOFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/shrub_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/shrub_tsptAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/shrub_labEqpTsptAdminOFUSD.tif", overwrite=T)

# Lantana
r1 <- raster("ltCostO.tif")
r2 <- raster("transport/lant_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/lant_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminOFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/lant_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/lant_tsptAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/lant_labEqpTsptAdminOFUSD.tif", overwrite=T)

# Grass
r1 <- raster("gsCostO.tif")
r2 <- raster("transport/grass_tsptTotO.tif")
r3 <- raster("monANRCostO.tif")                   # ANR monitoring to be applied across multiple
r4 <- raster("transport/monANR_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
r3 <- r3 + r3 * vo
r4 <- r4 + r4 * vo
r1 <- r1 + r3
r2 <- r2 + r4
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/grass_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminOFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/grass_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/grass_tsptAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/grass_labEqpTsptAdminOFUSD.tif", overwrite=T)

# Firebreaks
r1 <- raster("fbCostO.tif")
r2 <- raster("transport/fireb_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminOFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/fireb_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/fireb_tsptAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fireb_labEqpTsptAdminOFUSD.tif", overwrite=T)

# Soil
r1 <- raster("slCostO.tif") # includes ploughing and fertiliser application
r2 <- raster("transport/soil_tsptTotO.tif")
r1 <- r1 + r1 * vo
r2 <- r2 + r2 * vo
st <- stack(r1, r2)
st <- mask(st, r1)
r <- sum(st)
writeRaster(r1, "totals/bymethod/soil_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminOFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/soil_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/soil_tsptAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/soil_labEqpTsptAdminOFUSD.tif", overwrite=T)

# FSA planting (including enrichment planting under pessimistic scenario)
r1 <- raster("nurseryFSACostO.tif") # includes procurement, establishment and management
r2 <- raster("fsCostO.tif")
r3 <- raster("fsamCostO.tif") # includes weeding, fertiliser, watering, (thinning and enrichment planting)
r4 <- raster("monFSACostO.tif")
r5 <- raster("fmCostO.tif")
r6 <- raster("enrCostO.tif")
r7 <- raster("enrmCostO.tif")
r8 <- raster("monEnrCostO.tif")
st1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st1 <- st1 + st1 * vo
st1 <- mask(st1, r1)
r9 <- sum(st1)

r1 <- raster("transport/fsa_onrdTotO.tif")
r2 <- raster("transport/fsa_offrdTotO.tif")
r3 <- raster("transport/monFSA_tsptTotO.tif")
r4 <- raster("transport/mgmtFSA_tsptTotO.tif")
r5 <- raster("transport/enr_onrdTotO.tif")
r6 <- raster("transport/enr_offrdTotO.tif")
r7 <- raster("transport/monEnr_tsptTotO.tif")
r8 <- raster("transport/mgmtEnr_tsptTotO.tif")

st2 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
st2 <- st2 + st2 * vo
st2 <- mask(st2, r1)
r10 <- sum(st2)

st <- stack(r9, r10)
r <- sum(st)

writeRaster(r9, "totals/bymethod/fsa_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminOFTZS.tif", overwrite=T)
r9 <- r9 * rate
r10 <- r10 * rate
r <- r * rate
writeRaster(r9, "totals/bymethod/fsa_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r10, "totals/bymethod/fsa_tsptAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/fsa_labEqpTsptAdminOFUSD.tif", overwrite=T)

# Nurse trees
r1 <- raster("nurseryNseCostO.tif")
r2 <- raster("nsCostO.tif")
r3 <- raster("nursemCostO.tif")
r4 <- raster("monNseCostO.tif")
r5 <- raster("fmCostO.tif")

r6 <- raster("transport/nse_onrdTotO.tif")
r7 <- raster("transport/nse_offrdTotO.tif")
r8 <- raster("transport/monNse_tsptTotO.tif")
r9 <- raster("transport/mgmtNse_tsptTotO.tif")

st <- stack(r1, r2, r3, r4, r5, r6, r7, r8, r9)
st <- st + st * vo
st <- mask(st, r1)
r <- sum(st)
st1 <- stack(r1, r2, r3, r4, r5)
st1 <- st1 + st1 * vo
st1 <- mask(st1, r1)
r1 <- sum(st1)
st2 <- stack(r6, r7, r8, r9)
st2 <- st2 + st2 * vo
st2 <- mask(st2, r1)
r2 <- sum(st2)

writeRaster(r1, "totals/bymethod/nurse_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminOFTZS.tif", overwrite=T)
r1 <- r1 * rate
r2 <- r2 * rate
r <- r * rate
writeRaster(r1, "totals/bymethod/nurse_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r2, "totals/bymethod/nurse_tsptAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/nurse_labEqpTsptAdminOFUSD.tif", overwrite=T)

# Community engagement
r <- raster("ceCostO.tif")
r <- r + r * vo
writeRaster(r, "totals/bymethod/commeng_labEqpAdminOFTZS.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminOFTZS.tif", overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/commeng_labEqpAdminOFUSD.tif", overwrite=T)
writeRaster(r, "totals/bymethod/commeng_labEqpTsptAdminOFUSD.tif", overwrite=T)

# Staff (no admin costs added for staff)
r <- raster("staffCostO.tif")
r <- mask(r, relO)
writeRaster(r, "totals/bymethod/staff_labEqpAdminOFTZS.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminOFTZS.tif",overwrite=T)
r <- r * rate
writeRaster(r, "totals/bymethod/staff_labEqpAdminOFUSD.tif",overwrite=T)
writeRaster(r, "totals/bymethod/staff_labEqpTsptAdminOFUSD.tif",overwrite=T)

################################################################################
############################ TOTAL COSTS OVERALL ###############################                                 <- TOTAL OVERALL LANDSCAPE RESTORATION COSTS

# This code uses output (total costs per method) rasters to create
# (a) raster stack with total costs for each method as separate layer; and,
# (b) overall costs rasters for the region

### Set input work directory
setwd(inDir)

########## 5 YEARS #############################################################

### Set output work directory
setwd(outC5)
mainDir <- outC5
subDir <- "totals/bymethod/"

filen <- c("Engagement", "Firebreaks", "FSA", "Grass", "Land", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Staff", "Vines")

########## PESS ##########
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpTsptAdminP5USD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "total-cost-bymethod-P5USD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/total-cost-P5USD.tif", overwrite=T)

########## REAL ##########
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpTsptAdminR5USD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "total-cost-bymethod-R5USD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/total-cost-R5USD.tif", overwrite=T)

########## OPT ##########
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpTsptAdminO5USD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "total-cost-bymethod-O5USD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/total-cost-O5USD.tif", overwrite=T)

########## FULL RECOVERY #######################################################

### Set output work directory
setwd(outCR)
mainDir <- outCR
subDir <- "totals/bymethod/"

filen <- c("Engagement", "Firebreaks", "FSA", "Grass", "Land", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Staff", "Vines")

########## PESS ##########
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpTsptAdminPFUSD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "total-cost-bymethod-PFUSD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/total-cost-PFUSD.tif", overwrite=T)

########## REAL ##########
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpTsptAdminRFUSD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "total-cost-bymethod-RFUSD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/total-cost-RFUSD.tif", overwrite=T)

########## OPT ##########
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpTsptAdminOFUSD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "total-cost-bymethod-OFUSD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/total-cost-OFUSD.tif", overwrite=T)

################################################################################
################################################################################

##################################
########## NO TRANSPORT ##########
##################################

########## 5 YEARS ##########

### Set output work directory
setwd(outC5)
mainDir <- outC5
subDir <- "totals/bymethod/"

filen <- c("Engagement", "Firebreaks", "FSA", "Grass", "Land", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Staff", "Vines")

########## PESS ##########
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpAdminP5USD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-labeqp-bymethod-P5USD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-labeqp-P5USD.tif", overwrite=T)

########## REAL ##########
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpAdminR5USD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-labeqp-bymethod-R5USD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-labeqp-R5USD.tif", overwrite=T)

########## OPT ##########
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpAdminO5USD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-labeqp-bymethod-O5USD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-labeqp-O5USD.tif", overwrite=T)

########## FULL RECOVERY ##########

### Set output work directory
setwd(outCR)
mainDir <- outCR
subDir <- "totals/bymethod/"

filen <- c("Engagement", "Firebreaks", "FSA", "Grass", "Land", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Staff", "Vines")

########## PESS ##########
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpAdminPFUSD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-labeqp-bymethod-PFUSD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-labeqp-PFUSD.tif", overwrite=T)

########## REAL ##########
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpAdminRFUSD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-labeqp-bymethod-RFUSD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-labeqp-RFUSD.tif", overwrite=T)

########## OPT ##########
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="labEqpAdminOFUSD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-labeqp-bymethod-OFUSD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-labeqp-OFUSD.tif", overwrite=T)

##################################
######### ONLY TRANSPORT #########
##################################

########## 5 YEARS ##########

### Set output work directory
setwd(outC5)
mainDir <- outC5
subDir <- "totals/bymethod/"

filen <- c("Firebreaks", "FSA", "Grass", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Vines")

########## PESS ##########
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="tsptAdminP5USD.tif", full.names = T)
rlist
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-tspt-bymethod-P5USD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-tspt-P5USD.tif", overwrite=T)

########## REAL ##########
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="tsptAdminR5USD.tif", full.names = T)
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-tspt-bymethod-R5USD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-tspt-R5USD.tif", overwrite=T)

########## OPT ##########
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="tsptAdminO5USD.tif", full.names = T)
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-tspt-bymethod-O5USD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-tspt-O5USD.tif", overwrite=T)

########## FULL RECOVERY ##########

### Set output work directory
setwd(outCR)
mainDir <- outCR
subDir <- "totals/bymethod/"

filen <- c("Firebreaks", "FSA", "Grass", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Vines")

########## PESS ##########
rel <- raster(paste(outA, "restoreP.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="tsptAdminPFUSD.tif", full.names = T)
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-tspt-bymethod-PFUSD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-tspt-PFUSD.tif", overwrite=T)

########## REAL ##########
rel <- raster(paste(outA, "restoreR.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="tsptAdminRFUSD.tif", full.names = T)
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-tspt-bymethod-RFUSD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-tspt-RFUSD.tif", overwrite=T)

########## OPT ##########
rel <- raster(paste(outA, "restoreO.grd", sep="/"))
rlist <- list.files(file.path(mainDir, subDir), pattern="tsptAdminOFUSD.tif", full.names = T)
st <- stack(rlist)
names(st) <- filen
st <- mask(st, rel)
r <- sum(st, na.rm=T)
r <- mask(r, rel)
writeRaster(st, paste(mainDir, subDir, "subt-cost-tspt-bymethod-OFUSD.stack.grd", sep="/"), overwrite=T)
writeRaster(r, "totals/subt-cost-tspt-OFUSD.tif", overwrite=T)

################################################################################
###################### COST BY APPROACH ~ AGGREGATED METHODS ###################                                             <- RESTORATION COSTS PER APPROACH

# This code uses output stacks in previous [costs-totals-overall-and-stacks-bymethod] to create:
# (a) overall cost rasters per approach (ANR in forests / ANR in savag / Active restoration)
# (b) raster stacks with total costs for each approach as separate layer

### Set input work directory
setwd(inDir)

################################# 5 YEARS ######################################
### Set output work directory
setwd(outC5)
mainDir <- outC5
subDir <- "totals/byapproach/"

##### PESSIMISTIC #################################
st <- stack("totals/bymethod/total-cost-bymethod-P5USD.stack.grd") # c("Engagement", "Firebreaks", "FSA", "Grass", "Land", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Staff", "Vines")
# Passive
rel <- raster(paste(outA, "passiveP.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 8)            # Monitoring
s1 <- stack(r1, r2, r3, r4)
r <- sum(s1, na.rm=T)
r[r==0] <- NA                
r <- mask(r, rel)            
writeRaster(r, paste(mainDir, subDir, "passive_labEqpTsptAdminP5USD.tif", sep="/"), overwrite=T)

# ANR
rel <- raster(paste(outA, "anrP.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 12)           # vines
r5 <- subset(st, 9)            # shrubs
r6 <- subset(st, 6)            # lantana
r7 <- subset(st, 4)            # grass
r8 <- subset(st, 2)            # firebreaks
s1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
r <- sum(s1, na.rm=T)
r[r==0] <- NA  
r <- mask(r, rel)              
writeRaster(r, paste(mainDir, subDir, "anr_labEqpTsptAdminP5USD.tif", sep="/"), overwrite=T)

# ACTIVE
rel <- raster(paste(outA, "activeP.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 3)            # FSA
r5 <- subset(st, 7)            # Nurse
r6 <- subset(st, 10)           # Soil
s1 <- stack(r1, r2, r3, r4, r5, r6)
r <- sum(s1, na.rm=T)
r[r==0] <- NA            
r <- mask(r, rel) 
writeRaster(r, paste(mainDir, subDir, "active_labEqpTsptAdminP5USD.tif", sep="/"), overwrite=T)

# STACK
r1 <- raster(paste(mainDir, subDir, "passive_labEqpTsptAdminP5USD.tif", sep="/")) 
r2 <- raster(paste(mainDir, subDir, "anr_labEqpTsptAdminP5USD.tif", sep="/")) 
r3 <- raster(paste(mainDir, subDir, "active_labEqpTsptAdminP5USD.tif", sep="/")) 
s1 <- stack(r1, r2, r3)
names(s1) <- c("Passive", "ANR", "Active")
writeRaster(s1, paste(mainDir, subDir, "total-cost-byapproach-P5USD.stack.grd", sep="/"), overwrite=T)

##### REALISTIC ######################################
st <- stack("totals/bymethod/total-cost-bymethod-R5USD.stack.grd") # c("Engagement", "Firebreaks", "FSA", "Grass", "Land", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Staff", "Vines")
# Passive
rel <- raster(paste(outA, "passiveR.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 8)            # Monitoring
s1 <- stack(r1, r2, r3, r4)
r <- sum(s1, na.rm=T)
r[r==0] <- NA                
r <- mask(r, rel)            
writeRaster(r, paste(mainDir, subDir, "passive_labEqpTsptAdminR5USD.tif", sep="/"), overwrite=T)

# ANR
rel <- raster(paste(outA, "anrR.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 12)           # vines
r5 <- subset(st, 9)            # shrubs
r6 <- subset(st, 6)            # lantana
r7 <- subset(st, 4)            # grass
r8 <- subset(st, 2)            # firebreaks
s1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
r <- sum(s1, na.rm=T)
r[r==0] <- NA  
r <- mask(r, rel)              
writeRaster(r, paste(mainDir, subDir, "anr_labEqpTsptAdminR5USD.tif", sep="/"), overwrite=T)

# ACTIVE
rel <- raster(paste(outA, "activeR.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 3)            # FSA
r5 <- subset(st, 7)            # Nurse
r6 <- subset(st, 10)           # Soil
s1 <- stack(r1, r2, r3, r4, r5, r6)
r <- sum(s1, na.rm=T)
r[r==0] <- NA            
r <- mask(r, rel) 
writeRaster(r, paste(mainDir, subDir, "active_labEqpTsptAdminR5USD.tif", sep="/"), overwrite=T)

# STACK
r1 <- raster(paste(mainDir, subDir, "passive_labEqpTsptAdminR5USD.tif", sep="/")) 
r2 <- raster(paste(mainDir, subDir, "anr_labEqpTsptAdminR5USD.tif", sep="/")) 
r3 <- raster(paste(mainDir, subDir, "active_labEqpTsptAdminR5USD.tif", sep="/")) 
s1 <- stack(r1, r2, r3)
names(s1) <- c("Passive", "ANR", "Active")
writeRaster(s1, paste(mainDir, subDir, "total-cost-byapproach-R5USD.stack.grd", sep="/"), overwrite=T)

##### OPTIMISTIC ####################################
st <- stack("totals/bymethod/total-cost-bymethod-O5USD.stack.grd") # c("Engagement", "Firebreaks", "FSA", "Grass", "Land", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Staff", "Vines")
# Passive
rel <- raster(paste(outA, "passiveO.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 8)            # Monitoring
s1 <- stack(r1, r2, r3, r4)
r <- sum(s1, na.rm=T)
r[r==0] <- NA                
r <- mask(r, rel)            
writeRaster(r, paste(mainDir, subDir, "passive_labEqpTsptAdminO5USD.tif", sep="/"), overwrite=T)

# ANR
rel <- raster(paste(outA, "anrO.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 12)           # vines
r5 <- subset(st, 9)            # shrubs
r6 <- subset(st, 6)            # lantana
r7 <- subset(st, 4)            # grass
r8 <- subset(st, 2)            # firebreaks
s1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
r <- sum(s1, na.rm=T)
r[r==0] <- NA  
r <- mask(r, rel)              
writeRaster(r, paste(mainDir, subDir, "anr_labEqpTsptAdminO5USD.tif", sep="/"), overwrite=T)

# ACTIVE
rel <- raster(paste(outA, "activeO.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 3)            # FSA
r5 <- subset(st, 7)            # Nurse
r6 <- subset(st, 10)           # Soil
s1 <- stack(r1, r2, r3, r4, r5, r6)
r <- sum(s1, na.rm=T)
r[r==0] <- NA            
r <- mask(r, rel) 
writeRaster(r, paste(mainDir, subDir, "active_labEqpTsptAdminO5USD.tif", sep="/"), overwrite=T)

# STACK
r1 <- raster(paste(mainDir, subDir, "passive_labEqpTsptAdminO5USD.tif", sep="/")) 
r2 <- raster(paste(mainDir, subDir, "anr_labEqpTsptAdminO5USD.tif", sep="/")) 
r3 <- raster(paste(mainDir, subDir, "active_labEqpTsptAdminO5USD.tif", sep="/")) 
s1 <- stack(r1, r2, r3)
names(s1) <- c("Passive", "ANR", "Active")
writeRaster(s1, paste(mainDir, subDir, "total-cost-byapproach-O5USD.stack.grd", sep="/"), overwrite=T)

################################################################################
################################################################################

############################## FULL RECOVERY ###################################
### Set output work directory
setwd(outCR)
mainDir <- outCR
subDir <- "totals/byapproach/"

##### PESSIMISTIC #################################
st <- stack("totals/bymethod/total-cost-bymethod-PFUSD.stack.grd") # c("Engagement", "Firebreaks", "FSA", "Grass", "Land", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Staff", "Vines")
# Passive
rel <- raster(paste(outA, "passiveP.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 8)            # Monitoring
s1 <- stack(r1, r2, r3, r4)
r <- sum(s1, na.rm=T)
r[r==0] <- NA                
r <- mask(r, rel)            
writeRaster(r, paste(mainDir, subDir, "passive_labEqpTsptAdminPFUSD.tif", sep="/"), overwrite=T)

# ANR
rel <- raster(paste(outA, "anrP.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 12)           # vines
r5 <- subset(st, 9)            # shrubs
r6 <- subset(st, 6)            # lantana
r7 <- subset(st, 4)            # grass
r8 <- subset(st, 2)            # firebreaks
s1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
r <- sum(s1, na.rm=T)
r[r==0] <- NA  
r <- mask(r, rel)              
writeRaster(r, paste(mainDir, subDir, "anr_labEqpTsptAdminPFUSD.tif", sep="/"), overwrite=T)

# ACTIVE
rel <- raster(paste(outA, "activeP.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 3)            # FSA
r5 <- subset(st, 7)            # Nurse
r6 <- subset(st, 10)           # Soil
s1 <- stack(r1, r2, r3, r4, r5, r6)
r <- sum(s1, na.rm=T)
r[r==0] <- NA            
r <- mask(r, rel) 
writeRaster(r, paste(mainDir, subDir, "active_labEqpTsptAdminPFUSD.tif", sep="/"), overwrite=T)

# STACK
r1 <- raster(paste(mainDir, subDir, "passive_labEqpTsptAdminPFUSD.tif", sep="/")) 
r2 <- raster(paste(mainDir, subDir, "anr_labEqpTsptAdminPFUSD.tif", sep="/")) 
r3 <- raster(paste(mainDir, subDir, "active_labEqpTsptAdminPFUSD.tif", sep="/")) 
s1 <- stack(r1, r2, r3)
names(s1) <- c("Passive", "ANR", "Active")
writeRaster(s1, paste(mainDir, subDir, "total-cost-byapproach-PFUSD.stack.grd", sep="/"), overwrite=T)

##### REALISTIC ######################################
st <- stack("totals/bymethod/total-cost-bymethod-RFUSD.stack.grd") # c("Engagement", "Firebreaks", "FSA", "Grass", "Land", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Staff", "Vines")
# Passive
rel <- raster(paste(outA, "passiveR.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 8)            # Monitoring
s1 <- stack(r1, r2, r3, r4)
r <- sum(s1, na.rm=T)
r[r==0] <- NA                
r <- mask(r, rel)            
writeRaster(r, paste(mainDir, subDir, "passive_labEqpTsptAdminRFUSD.tif", sep="/"), overwrite=T)

# ANR
rel <- raster(paste(outA, "anrR.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 12)           # vines
r5 <- subset(st, 9)            # shrubs
r6 <- subset(st, 6)            # lantana
r7 <- subset(st, 4)            # grass
r8 <- subset(st, 2)            # firebreaks
s1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
r <- sum(s1, na.rm=T)
r[r==0] <- NA  
r <- mask(r, rel)              
writeRaster(r, paste(mainDir, subDir, "anr_labEqpTsptAdminRFUSD.tif", sep="/"), overwrite=T)

# ACTIVE
rel <- raster(paste(outA, "activeR.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 3)            # FSA
r5 <- subset(st, 7)            # Nurse
r6 <- subset(st, 10)           # Soil
s1 <- stack(r1, r2, r3, r4, r5, r6)
r <- sum(s1, na.rm=T)
r[r==0] <- NA            
r <- mask(r, rel) 
writeRaster(r, paste(mainDir, subDir, "active_labEqpTsptAdminRFUSD.tif", sep="/"), overwrite=T)

# STACK
r1 <- raster(paste(mainDir, subDir, "passive_labEqpTsptAdminRFUSD.tif", sep="/")) 
r2 <- raster(paste(mainDir, subDir, "anr_labEqpTsptAdminRFUSD.tif", sep="/")) 
r3 <- raster(paste(mainDir, subDir, "active_labEqpTsptAdminRFUSD.tif", sep="/")) 
s1 <- stack(r1, r2, r3)
names(s1) <- c("Passive", "ANR", "Active")
writeRaster(s1, paste(mainDir, subDir, "total-cost-byapproach-RFUSD.stack.grd", sep="/"), overwrite=T)

##### OPTIMISTIC ####################################
st <- stack("totals/bymethod/total-cost-bymethod-OFUSD.stack.grd") # c("Engagement", "Firebreaks", "FSA", "Grass", "Land", "Lantana", "Nurse", "Passive", "Shrubs", "Soil", "Staff", "Vines")
# Passive
rel <- raster(paste(outA, "passiveO.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 8)            # Monitoring
s1 <- stack(r1, r2, r3, r4)
r <- sum(s1, na.rm=T)
r[r==0] <- NA                
r <- mask(r, rel)            
writeRaster(r, paste(mainDir, subDir, "passive_labEqpTsptAdminOFUSD.tif", sep="/"), overwrite=T)

# ANR
rel <- raster(paste(outA, "anrO.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 12)           # vines
r5 <- subset(st, 9)            # shrubs
r6 <- subset(st, 6)            # lantana
r7 <- subset(st, 4)            # grass
r8 <- subset(st, 2)            # firebreaks
s1 <- stack(r1, r2, r3, r4, r5, r6, r7, r8)
r <- sum(s1, na.rm=T)
r[r==0] <- NA  
r <- mask(r, rel)              
writeRaster(r, paste(mainDir, subDir, "anr_labEqpTsptAdminOFUSD.tif", sep="/"), overwrite=T)

# ACTIVE
rel <- raster(paste(outA, "activeO.grd", sep="/"))
r1 <- subset(st, 1)            # Engagement
r2 <- subset(st, 5)            # Land
r3 <- subset(st, 11)           # Staff
r4 <- subset(st, 3)            # FSA
r5 <- subset(st, 7)            # Nurse
r6 <- subset(st, 10)           # Soil
s1 <- stack(r1, r2, r3, r4, r5, r6)
r <- sum(s1, na.rm=T)
r[r==0] <- NA            
r <- mask(r, rel) 
writeRaster(r, paste(mainDir, subDir, "active_labEqpTsptAdminOFUSD.tif", sep="/"), overwrite=T)

# STACK
r1 <- raster(paste(mainDir, subDir, "passive_labEqpTsptAdminOFUSD.tif", sep="/")) 
r2 <- raster(paste(mainDir, subDir, "anr_labEqpTsptAdminOFUSD.tif", sep="/")) 
r3 <- raster(paste(mainDir, subDir, "active_labEqpTsptAdminOFUSD.tif", sep="/")) 
s1 <- stack(r1, r2, r3)
names(s1) <- c("Passive", "ANR", "Active")
writeRaster(s1, paste(mainDir, subDir, "total-cost-byapproach-OFUSD.stack.grd", sep="/"), overwrite=T)

################################################################################
################### TOTAL LANDSCAPE RESTORATION COSTS STACK ####################                                   <- TOTAL LANDSCAPE RESTORATION COSTS STACK

### Set input work directory
setwd(inDir)

# Read in mask rasters:
sa <- readOGR("Study_Area.shp")                   # Study area
P <- raster(paste(outA, "restoreP.grd", sep="/")) # Pixels with restoration potential
R <- raster(paste(outA, "restoreR.grd", sep="/")) 
O <- raster(paste(outA, "restoreO.grd", sep="/")) 

############################ RESTORATION COSTS #################################
# Costs rasters (ALL COSTS - INCL STAFF, ADMIN & TRANSPORT, created using code: costs-overall-and-stacks-bymethod)
setwd(outC5)
r1 <- raster("totals/total-cost-P5USD.tif")
r2 <- raster("totals/total-cost-R5USD.tif")
r3 <- raster("totals/total-cost-O5USD.tif")
setwd(outCR)
r4 <- raster("totals/total-cost-PFUSD.tif")
r5 <- raster("totals/total-cost-RFUSD.tif")
r6 <- raster("totals/total-cost-OFUSD.tif")
# Masked to only include area warranting restoration intervention under the various scenarios
r1 <- mask(r1, P)
r4 <- mask(r4, P)
r2 <- mask(r2, R)
r5 <- mask(r5, R)
r3 <- mask(r3, O)
r6 <- mask(r6, O)
st <- stack(r1,r2,r3,r4,r5,r6)
names(st) <- c("P5","R5","O5","PF","RF","OF")

writeRaster(st, paste(outC, "total-cost-byscenario.stack.grd", sep="/"), overwrite=T)


