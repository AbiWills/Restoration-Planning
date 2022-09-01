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
############################ RESTORATION METHODS ###############################                                      <- RESTORATION METHODS
################################################################################

################################################################################
################### DISTANCE FROM PROXIMAL INTACT HABITAT ######################                                      <- DISTANCE FROM PROXIMAL INTACT HABITAT

# This section of code computes the Euclidean distance of each landscape pixel from
# the nearest 'intact' or 'near intact' pixel of the same land cover class.
# Here, '(near) intact' pixels were extracted from a categorical landcover raster
# for later computing Euclidean distance surfaces in Arc/QGIS software.

### COMPUTE DISTANCE RASTERS ###################################################

### Set input work directory
setwd(inDir)

sa <- readOGR("Study_Area.shp")
lc <- raster("LandCoverFinal_1ha.tif")

################################################################################
#################### OPTION 1: ZERO DEFICIT = INTACT ###########################
################################################################################

# EUCLIDEAN DISTANCE INTACT LC CLASSES (REALISTIC)
r2 <- raster("agbdef_pct.tif")
r3 <- r2
r3[r3==0] <- NA
r4 <- mask(r1, r3, inverse=T)
writeRaster(r4, "LandCover_intact.tif", overwrite=T) ### Used to calculate distance from intact forests/savanna in Arc/QGIS
fr <- r4
fr[fr>1] <- NA
sav <- r4
sav[sav==1 | sav==4 | sav==5] <- NA
frsav <- r4
frsav[frsav==4 | frsav==5] <- NA
writeRaster(fr, "Forest_intact.tif", overwrite=T)
writeRaster(sav, "SavFlood_intact.tif", overwrite=T)
writeRaster(frsav, "ForSavFlood_intact.tif", overwrite=T)

# EUCLIDEAN DISTANCE INTACT LC CLASSES (PESSIMISTIC)
r2 <- raster("agbdef_pct_pess.tif")
r3 <- r2
r3[r3==0] <- NA
r4 <- mask(r1, r3, inverse=T)
writeRaster(r4, "LandCover_intact_pess.tif", overwrite=T)
fr <- r4
fr[fr>1] <- NA
sav <- r4
sav[sav==1 | sav==4 | sav==5] <- NA
frsav <- r4
frsav[frsav==4 | frsav==5] <- NA
writeRaster(fr, "Forest_intact_pess.tif", overwrite=T)
writeRaster(sav, "SavFlood_intact_pess.tif", overwrite=T)
writeRaster(frsav, "ForSavFlood_intact_pess.tif", overwrite=T)

# EUCLIDEAN DISTANCE INTACT LC CLASSES (OPTIMISTIC)
r2 <- raster("agbdef_pct_opt.tif")
r3 <- r2
r3[r3==0] <- NA
r4 <- mask(r1, r3, inverse=T)
writeRaster(r4, "LandCover_intact_opt.tif", overwrite=T)
fr <- r4
fr[fr>1] <- NA
sav <- r4
sav[sav==1 | sav==4 | sav==5] <- NA
frsav <- r4
frsav[frsav==4 | frsav==5] <- NA
writeRaster(fr, "Forest_intact_opt.tif", overwrite=T)
writeRaster(sav, "SavFlood_intact_opt.tif", overwrite=T)
writeRaster(frsav, "ForSavFlood_intact_opt.tif", overwrite=T)

################################################################################
############## OPTION 2: PASSIVE REGEN DEFICIT THRESHOLD = INTACT ##############
################################################################################

r1 <- lc

# EUCLIDEAN DISTANCE INTACT LC CLASSES (REALISTIC)
agbdef <- raster("agbdef_pct.tif")
r1 <- raster(lc)
r1[(lc==1 & agbdef<0.5) | ((lc==2 | lc==3 | lc==4) & agbdef<0.4)] <- 1  # Forest able to regenerate without silvicultural intervention
r2 <- mask(lc, r1)
writeRaster(r2, "LandCover_intact.tif", overwrite=T) ### Used to calculate distance from intact forests/savanna in Arc/QGIS
fr <- r2
fr[fr>1] <- NA
sav <- r2
sav[sav==1 | sav==4 | sav==5] <- NA
frsav <- r2
frsav[frsav==4 | frsav==5] <- NA
writeRaster(fr, "Forest_intact.tif", overwrite=T)
writeRaster(sav, "SavFlood_intact.tif", overwrite=T)
writeRaster(frsav, "ForSavFlood_intact.tif", overwrite=T)

# EUCLIDEAN DISTANCE INTACT LC CLASSES (PESSIMISTIC)
agbdef <- raster("agbdef_pct_pess.tif")
r1 <- raster(lc)
r1[(lc==1 & agbdef<0.4) | ((lc==2 | lc==3 | lc==4) & agbdef<0.3)] <- 1
r2 <- mask(lc, r1)
writeRaster(r2, "LandCover_intact_pess.tif", overwrite=T)
fr <- r2
fr[fr>1] <- NA
sav <- r2
sav[sav==1 | sav==4 | sav==5] <- NA
frsav <- r2
frsav[frsav==4 | frsav==5] <- NA
writeRaster(fr, "Forest_intact_pess.tif", overwrite=T)
writeRaster(sav, "SavFlood_intact_pess.tif", overwrite=T)
writeRaster(frsav, "ForSavFlood_intact_pess.tif", overwrite=T)

# EUCLIDEAN DISTANCE INTACT LC CLASSES (OPTIMISTIC)
agbdef <- raster("agbdef_pct_opt.tif")
r1 <- raster(lc)
r1[(lc==1 & agbdef<0.8) | ((lc==2 | lc==3 | lc==4) & agbdef<0.5)] <- 1
r2 <- mask(lc, r1)
writeRaster(r2, "LandCover_intact_opt.tif", overwrite=T)
fr <- r2
fr[fr>1] <- NA
sav <- r2
sav[sav==1 | sav==4 | sav==5] <- NA
frsav <- r2
frsav[frsav==4 | frsav==5] <- NA
writeRaster(fr, "Forest_intact_opt.tif", overwrite=T)
writeRaster(sav, "SavFlood_intact_opt.tif", overwrite=T)
writeRaster(frsav, "ForSavFlood_intact_opt.tif", overwrite=T)

################################################################################
########### OPTION 3: REDUCED ECOSYSTEM FUNCTION THRESHOLD = INTACT ############
################################################################################
# We assumed that areas with >70% (pessimistic; Hanski 2011) to 90% (optimistic; Brancalion et al. 2019)
# AGB deficit were likely to have significantly reduced ecosystem function.

r1 <- lc

# EUCLIDEAN DISTANCE INTACT LC CLASSES (REALISTIC)
agbdef <- raster("agbdef_pct.tif")
r1 <- raster(lc)
r1[agbdef<0.8] <- 1
r2 <- mask(lc, r1)
writeRaster(r2, "LandCover_intact.tif", overwrite=T) ### Used to calculate distance from intact forests/savanna in Arc/QGIS
fr <- r2
fr[fr>1] <- NA
sav <- r2
sav[sav==1 | sav==4 | sav==5] <- NA
frsav <- r2
frsav[frsav==4 | frsav==5] <- NA
writeRaster(fr, "Forest_intact.tif", overwrite=T)
writeRaster(sav, "SavFlood_intact.tif", overwrite=T)
writeRaster(frsav, "ForSavFlood_intact.tif", overwrite=T)

# EUCLIDEAN DISTANCE INTACT LC CLASSES (PESSIMISTIC)
agbdef <- raster("agbdef_pct_pess.tif")
r1 <- raster(lc)
r1[agbdef<0.7] <- 1
r2 <- mask(lc, r1)
writeRaster(r2, "LandCover_intact_pess.tif", overwrite=T)
fr <- r2
fr[fr>1] <- NA
sav <- r2
sav[sav==1 | sav==4 | sav==5] <- NA
frsav <- r2
frsav[frsav==4 | frsav==5] <- NA
writeRaster(fr, "Forest_intact_pess.tif", overwrite=T)
writeRaster(sav, "SavFlood_intact_pess.tif", overwrite=T)
writeRaster(frsav, "ForSavFlood_intact_pess.tif", overwrite=T)

# EUCLIDEAN DISTANCE INTACT LC CLASSES (OPTIMISTIC)
agbdef <- raster("agbdef_pct_opt.tif")
r1 <- raster(lc)
r1[agbdef<0.9] <- 1
r2 <- mask(lc, r1)
writeRaster(r2, "LandCover_intact_opt.tif", overwrite=T)
fr <- r2
fr[fr>1] <- NA
sav <- r2
sav[sav==1 | sav==4 | sav==5] <- NA
frsav <- r2
frsav[frsav==4 | frsav==5] <- NA
writeRaster(fr, "Forest_intact_opt.tif", overwrite=T)
writeRaster(sav, "SavFlood_intact_opt.tif", overwrite=T)
writeRaster(frsav, "ForSavFlood_intact_opt.tif", overwrite=T)

################################################################################
##################### RESTORATION METHODS INPUTS STACK #########################                            <- METHODS INPUTS STACK

# This section of code creates a raster stack comprising all input rasters used to
# assign appropriate restoration methods.

### Set input work directory
setwd(inDir)

# Key determinants of Rest Method: #Current Land Cover class (all methods)
                                   #Percent AGB Deficit (all methods)
                                   #Elevation (ANR in forests)
                                   #Euclidean distance from intact (=zero deficit) Forest (tree planting in degraded Forest)
                                   #Euclidean distance from intact Savanna/Floodplain (tree planting in Savanna/Floodplain)
                                   #Euclidean distance from intact ForestSavanna/Floodplain (tree planting in Agriculture mosaic)
                                   #Euclidean distance from Agriculture mosaic (lantana removal)
                                   #Euclidean distance from Road (lantana removal)
                                   #Path distance from Agriculture mosaic (community engagement)

# r1 = Current Land Cover ("lc")
# r2 = Percent AGB Deficit ("agbdef")
# r3 = Elevation ("elev")
# r4 = Euclidean distance to intact Forest ("distfor")
# r5 = Euclidean distance to intact Savanna/Floodplain ("distsav")
# r6 = Euclidean distance to intact Forest/Savanna/Floodplain ("distforsav")
# r7 = Euclidean distance to Agriculture mosaic ("distag")
# r8 = Euclidean distance to Road ("distrd")
# r9 = Path distance to Agriculture ("pdistag")

### ENSURE ALL INPUT RASTERS HAVE MATCHING extent, res, crs, origin, etc.

sa <- readOGR("Study_Area.shp")
ext <- raster("Extent_1.tif")

### CREATE STACK: ##############################################################

### REALISTIC

# Read in rasters:
r1 <- raster("LandCoverFinal_1ha.tif")
r2 <- raster("agbdef_pct.tif")
r3 <- raster("dem_srtm_1ha.tif")
r4 <- raster("dist_intact_forest.tif")
r5 <- raster("dist_intact_savflood.tif")
r6 <- raster("dist_intact_forsavflood.tif")
r7 <- raster("dist_agmosaic_1ha.tif")
r8 <- raster("dist_road_1ha.tif")
r9 <- raster("pathdist_agmosaic.tif")

# Create and check stack:
st <- stack(r1, r2, r3, r4, r5, r6, r7, r8, r9)

# Assign meaningful names to layers:
names(st) <- c("lc","agbdef","elev","distfor","distsav","distforsav","distag","distrd","pdistag")

# Mask by study area boundary
st <- mask(st, sa)

# Save raster stack:
writeRaster(st,"methods_stack.grd",overwrite=T)

### PESSIMISTIC

# Read in rasters:
r1 <- raster("LandCoverFinal_1ha.tif")
r2 <- raster("agbdef_pct_pess.tif")
r3 <- raster("dem_srtm_1ha.tif")
r4 <- raster("dist_intact_forest_pess.tif")
r5 <- raster("dist_intact_savflood_pess.tif")
r6 <- raster("dist_intact_forsavflood_pess.tif")
r7 <- raster("dist_agmosaic_1ha.tif")
r8 <- raster("dist_road_1ha.tif")
r9 <- raster("pathdist_agmosaic.tif")

# Create and check stack:
st <- stack(r1, r2, r3, r4, r5, r6, r7, r8, r9)

# Assign meaningful names to layers:
names(st) <- c("lc","agbdef","elev","distfor","distsav","distforsav","distag","distrd","pdistag")

# Mask by study area boundary
st <- mask(st, sa)

# Save raster stack:
writeRaster(st,"methods_stack_pess.grd",overwrite=T)

### OPTIMISTIC

# Read in rasters:
r1 <- raster("LandCoverFinal_1ha.tif")
r2 <- raster("agbdef_pct_opt.tif")
r3 <- raster("dem_srtm_1ha.tif")
r4 <- raster("dist_intact_forest_opt.tif")
r5 <- raster("dist_intact_savflood_opt.tif")
r6 <- raster("dist_intact_forsavflood_opt.tif")
r7 <- raster("dist_agmosaic_1ha.tif")
r8 <- raster("dist_road_1ha.tif")
r9 <- raster("pathdist_agmosaic.tif")

# Create and check stack:
st <- stack(r1, r2, r3, r4, r5, r6, r7, r8, r9)

# Assign meaningful names to layers:
names(st) <- c("lc","agbdef","elev","distfor","distsav","distforsav","distag","distrd","pdistag")

# Mask by study area boundary
st <- mask(st, sa)

# Save raster stack:
writeRaster(st,"methods_stack_opt.grd",overwrite=T)

################################################################################
######################### ASSIGN RESTORATION METHOD ############################                             <- ASSIGN RESTORATION METHODS

# Code for assigning appropriate restoration methods based on biomass deficit,
# landcover class, elevation, distance from proximal 'intact' habitat, etc.,
# including pessimistic, realistic and optimistic scenarios.

# NOTE:
# This version of code works from mean current and max AGB rasters across all scenarios
# rather than using upper and lower 95% CI rasters for pessimistic and realistic scenarios.
# This is useful for when optimistic AGB deficit maps generated by
# subtracting the lower 95% CI maximum AGB map from the upper 95% CI current AGB map
# results in too few pixels with AGB deficit/regeneration or restoration potential (in our case just 8)
# for meaningful further analysis of restoration methods, costs and cost-effectiveness
# across the landscape.

based on mean AGB deficit map only (used for more insightful subsequent cost & cost-effectiveness analyses under the different scenarios)

setwd(inDir)

# Read in inputs from working directory
sa <- readOGR("Study_Area.shp")
ext <- raster("Extent_1.tif")
cl <- readOGR("Commercial_land.shp")                     # Commercial land
pas <- raster("WDPA_less-open-gamecontrol-wma_clip.tif") # Protected Areas (incl. all areas where
                                                         # fire expected to be managed and where community engagement costs are already covered by management
                                                         # i.e. Nature Reserves, National Parks, Game Reserves, Forest Reserves and Forest Plantations)
fbmask <- raster("SavAgFlood1ha_noEnclosed.tif")         # All LC classes except forest and other, excluding areas enclosed by forest

################################## REALISTIC ###################################

# Read in raster stack used to determine appropriate methods
st <- stack("methods_stack.grd")
lc <- subset(st,1)                    # Create object from layer in stack: Current Land Cover ("lc")
agbdef <- subset(st,2)                # Percent AGB Deficit ("agbdef")                                                #
elev <- subset(st,3)                  # Elevation ("elev")
distfor <- subset(st,4)               # Euclidean distance to intact Forest ("distfor")                               #
distsav <- subset(st,5)               # Euclidean distance to intact Savanna/Floodplain ("distsav")                   #
distforsav <- subset(st,6)            # Euclidean distance to intact Forest/Savanna/Floodplain ("distforsav")         #
distag <- subset(st,7)                # Euclidean distance to Agriculture mosaic ("distag")
distrd <- subset(st,8)                # Euclidean distance to Road ("distrd")
pdistag <- subset(st,9)               # Path distance to Agriculture ("pdistag")

# Define methods based on thresholds and assumptions in methods table

########### NONE - I.E. NO DEFICIT SO NO RESTORATION INTERVENTION ##############
r2 <- raster(lc)
r2[agbdef==0] <- 1
r2 <- mask(r2, sa)
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outA,"noneR.grd",sep="/"),overwrite=T)

########### PASSIVE - I.E. NO SILVICULTURAL INTERVENTION #######################
r2 <- raster(lc)
r2[(lc==1 & (agbdef<0.5 & agbdef>0)) | ((lc==2 | lc==3 | lc==4) & (agbdef<0.4 & agbdef>0))] <- 1
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outA,"passiveR.grd",sep="/"),overwrite=T)
writeRaster(r2, paste(outM,"passiveR.grd",sep="/"),overwrite=T)

########### METHODS RELEVANT TO FOREST LC ######################################
# AGB deficit limits for managing vines, herbs/shrubs and invasive lantana:
plim <- 0.40 # 1-mean tree AGB in open / min tree AGB in closed forests (0.51)         [max open / mean closed = 0.44]
rlim <- 0.50 # 1-mean tree AGB in open / mean tree AGB in closed forests (0.77)        [mean open / min closed = 0.51]
olim <- 0.80 # 1-mean tree AGB in open / max tree AGB in closed forests (0.88)         [mean open / mean closed = 0.77]

########### VINES ##############################
r2 <- raster(lc)
r2[lc==1 & agbdef>=rlim & elev <1000] <- 1
r2 <- ratify(r2, count=TRUE)
writeRaster(r2, paste(outM,"vinesR.grd",sep="/"),overwrite=T)

########## HERBS/SHRUBS ##############################
r2 <- raster (lc)
r2[lc==1 & agbdef>=rlim & elev >=1000] <- 1
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outM,"herbsR.grd",sep="/"),overwrite=T)

########### LANTANA ##############################
r2 <- raster (lc)
r2[lc==1 & agbdef>=rlim & elev <1400 & distag <=100 & distrd <=100] <- 1
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outM,"lantR.grd",sep="/"),overwrite=T)

##### METHODS RELEVANT TO SAVANNA & AGMOSAIC LC ################################
# AGB deficit limits for firebreaks and grass cutting:
plim <- 0.3 # Literature
rlim <- 0.4 # Literature
olim <- 0.5 # Literature

########## FIREBREAKS ##############################
r2 <- raster (lc)
r2[(lc==2 | lc==3 | lc==4) & agbdef>=rlim] <- 1
r2 <- mask(r2, fbmask) # Crop out areas enclosed by forest
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outM,"fireR.grd",sep="/"),overwrite=T)

########## GRASS ##############################
r2 <- raster (lc)
r2[(lc==2 | lc==3 | lc==4) & agbdef>=rlim] <- 1
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outM,"grassR.grd",sep="/"),overwrite=T)

########## TREE PLANTING METHODS ###############################################
# AGB deficit limits for framework species planting:
plim <- 0.50 # Comparison of known areas where fsa planting is necessary based on ground observations with AGB deficit raster
rlim <- 0.65 # Comparison of known areas where fsa planting is necessary based on ground observations with AGB deficit raster
olim <- 0.80 # Literature

########## FRAMEWORK SPECIES ##############################
r2 <- raster (lc)
r2[((lc==2 | lc==3) & (agbdef>=rlim & agbdef<0.95) & distsav>=200) | (lc==4 & (agbdef>=rlim & agbdef<0.95) & distforsav>=200)] <- 1
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outM,"fsaR.grd",sep="/"),overwrite=T)

########## ENRICHMENT PLANTING ##############################
r2 <- raster(lc)
r2[agbdef<0] <- 1
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outM,"enrichR.grd",sep="/"),overwrite=T)

########## NURSE TREES ##############################
r2 <- raster (lc)
r2[(lc==2 | lc==3 | lc==4)  & agbdef>=0.95] <- 1
r2 <- ratify(r2, count=T)
r2
writeRaster(r2, paste(outM,"nurseR.grd",sep="/"),overwrite=T)

########## SOIL ##############################
r2 <- raster (lc)
r2[(lc==2 | lc==3 | lc==4) & agbdef>=0.95] <- 1
r2 <- ratify(r2, count=T)
r2
writeRaster(r2, paste(outM,"soilR.grd",sep="/"),overwrite=T)

########## COMMUNITY ENGAGEMENT ##############################
# Assumed necessary for pixels with deficit that are:
# (a) within agmosaic (100%) or within 3km of agmosaic (50%); and, (b) outside Protected Areas and large commercial farms
r2 <- raster(lc)
r2[agbdef>0 & pdistag<=3000] <- 1
r2 <- mask(r2, cl, inverse=T)
r2 <- mask(r2, pas, inverse=T)
r2[r2==1 & lc!=4 & pdistag<=3000] <- 0.5  # Adjust for cost variation outside Agriculture Mosaic areas
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outM,"commengR.grd",sep="/"),overwrite=T)

################################# PESSIMISTIC ##################################

# Read in raster stack used to determine appropriate methods
st <- stack("methods_stack.grd")
lc <- subset(st,1)                    # Create object from layer in stack: Current Land Cover ("lc")
agbdef <- subset(st,2)                # Percent AGB Deficit ("agbdef")                                                #
elev <- subset(st,3)                  # Elevation ("elev")
distfor <- subset(st,4)               # Euclidean distance to intact Forest ("distfor")                               #
distsav <- subset(st,5)               # Euclidean distance to intact Savanna/Floodplain ("distsav")                   #
distforsav <- subset(st,6)            # Euclidean distance to intact Forest/Savanna/Floodplain ("distforsav")         #
distag <- subset(st,7)                # Euclidean distance to Agriculture mosaic ("distag")
distrd <- subset(st,8)                # Euclidean distance to Road ("distrd")
pdistag <- subset(st,9)               # Path distance to Agriculture ("pdistag")

########### NONE - I.E. NO DEFICIT SO NO RESTORATION INTERVENTION ##############
r1 <- raster(lc)
r1[agbdef==0] <- 1
r1 <- mask(r1, sa)
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outA,"noneP.grd",sep="/"),overwrite=T)

########### PASSIVE - I.E. NO SILVICULTURAL INTERVENTION #######################
r1 <- raster(lc)
r1[(lc==1 & (agbdef<0.4 & agbdef>0)) | ((lc==2 | lc==3 | lc==4) & (agbdef<0.3 & agbdef>0))] <- 1
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outA,"passiveP.grd",sep="/"),overwrite=T)
writeRaster(r1, paste(outM,"passiveP.grd",sep="/"),overwrite=T)

##### METHODS RELEVANT TO FOREST LC ############################################
# AGB deficit limits for managing vines, herbs/shrubs and invasive lantana:
plim <- 0.40 # 1-mean tree AGB in open / min tree AGB in closed forests (0.51)         [max open / mean closed = 0.44]
rlim <- 0.50 # 1-mean tree AGB in open / mean tree AGB in closed forests (0.77)        [mean open / min closed = 0.51]
olim <- 0.80 # 1-mean tree AGB in open / max tree AGB in closed forests (0.88)         [mean open / mean closed = 0.77]

########### VINES ##############################
r1 <- raster(lc)
r1[lc==1 & agbdef>=plim & elev <1000] <- 1
r1 <- ratify(r1, count=TRUE)
writeRaster(r1, paste(outM,"vinesP.grd",sep="/"),overwrite=T)

########## HERBS/SHRUBS ##############################
r1 <- raster (lc)
r1[lc==1 & agbdef>=plim & elev >=1000] <- 1
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outM,"herbsP.grd",sep="/"),overwrite=T)

########### LANTANA ##############################
r1 <- raster (lc)
r1[lc==1 & agbdef>=plim & elev <1400 & distag <=200 & distrd <=200] <- 1
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outM,"lantP.grd",sep="/"),overwrite=T)

########## METHODS RELEVANT TO SAVANNA & AGMOSAIC LC ###########################
# AGB deficit limits for firebreaks and grass cutting:
plim <- 0.3 # Literature
rlim <- 0.4 # Literature
olim <- 0.5 # Literature

########## FIREBREAKS ##############################
r1 <- raster (lc)
r1[(lc==2 | lc==3 | lc==4) & agbdef>=plim] <- 1
r1 <- mask(r1, fbmask) # Crop out areas enclosed by forest
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outM,"fireP.grd",sep="/"),overwrite=T)

########## GRASS ##############################
r1 <- raster(lc)
r1[(lc==2 | lc==3 | lc==4) & agbdef>=plim] <- 1
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outM,"grassP.grd",sep="/"),overwrite=T)

########## TREE PLANTING METHODS ###############################################
# AGB deficit limits for framework species planting:
plim <- 0.50 # Comparison of known areas where fsa planting is necessary based on ground observations with AGB deficit raster
rlim <- 0.65 # Comparison of known areas where fsa planting is necessary based on ground observations with AGB deficit raster
olim <- 0.80 # Literature

########## FRAMEWORK SPECIES ##############################
r1 <- raster (lc)
r1[(lc==1 & agbdef>=0.9 & distfor>=100) | ((lc==2 | lc==3) & (agbdef>=plim & agbdef<0.9) & distsav>=100) | (lc==4 & (agbdef>=plim & agbdef<0.9) & distforsav>=100)] <- 1
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outM,"fsaP.grd",sep="/"),overwrite=T)

########## ENRICHMENT PLANTING ##############################
r1 <- raster(lc)
r1[(lc==1 & agbdef>=0.9) | ((lc==2 | lc==3 | lc==4) & (agbdef>=plim & agbdef<0.9))] <- 1
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outM,"enrichP.grd",sep="/"),overwrite=T)

########## NURSE TREES ##############################
r1 <- raster (lc)
r1[(lc==2 | lc==3 | lc==4) & agbdef>=0.9] <- 1
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outM,"nurseP.grd",sep="/"),overwrite=T)

########## SOIL ##############################
r1 <- raster (lc)
r1[(lc==2 | lc==3 | lc==4) & agbdef>=0.9] <- 1
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outM,"soilP.grd",sep="/"),overwrite=T)

########## COMMUNITY ENGAGEMENT ##############################
# Assumed necessary for pixels with deficit that are:
# (a) within agmosaic (100%) or within 3km of agmosaic (50%); and, (b) outside Protected Areas and large commercial farms
r1 <- raster(lc)
r1[agbdef>0 & pdistag<=3000] <- 1
r1 <- mask(r1, cl, inverse=T)
r1 <- mask(r1, pas, inverse=T)
r1[r1==1 & lc!=4 & pdistag<=3000] <- 0.5            # Adjust for cost variation outside Agriculture Mosaic areas
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outM,"commengP.grd",sep="/"),overwrite=T)

################################# OPTIMISTIC ###################################

# Read in raster stack used to determine appropriate methods
st <- stack("methods_stack.grd")
lc <- subset(st,1)                    # Create object from layer in stack: Current Land Cover ("lc")
agbdef <- subset(st,2)                # Percent AGB Deficit ("agbdef")                                                #
elev <- subset(st,3)                  # Elevation ("elev")
distfor <- subset(st,4)               # Euclidean distance to intact Forest ("distfor")                               #
distsav <- subset(st,5)               # Euclidean distance to intact Savanna/Floodplain ("distsav")                   #
distforsav <- subset(st,6)            # Euclidean distance to intact Forest/Savanna/Floodplain ("distforsav")         #
distag <- subset(st,7)                # Euclidean distance to Agriculture mosaic ("distag")
distrd <- subset(st,8)                # Euclidean distance to Road ("distrd")
pdistag <- subset(st,9)               # Path distance to Agriculture ("pdistag")

########### NONE - I.E. NO DEFICIT SO NO RESTORATION INTERVENTION ##############
r3 <- raster(lc)
r3[agbdef==0] <- 1
r3 <- mask(r3, sa)
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outA,"noneO.grd",sep="/"),overwrite=T)

########### PASSIVE - I.E. NO SILVICULTURAL INTERVENTION #######################
r3 <- raster(lc)
r3[(lc==1 & (agbdef<0.8 & agbdef>0)) | ((lc==2 | lc==3 | lc==4) & (agbdef<0.5 & agbdef>0))] <- 1
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outA,"passiveO.grd",sep="/"),overwrite=T)
writeRaster(r3, paste(outM,"passiveO.grd",sep="/"),overwrite=T)

##### METHODS RELEVANT TO FOREST LC ############################################
# AGB deficit limits for managing vines, herbs/shrubs and invasive lantana:
plim <- 0.40 # 1-mean tree AGB in open / min tree AGB in closed forests (0.51)         [max open / mean closed = 0.44]
rlim <- 0.50 # 1-mean tree AGB in open / mean tree AGB in closed forests (0.77)        [mean open / min closed = 0.51]
olim <- 0.80 # 1-mean tree AGB in open / max tree AGB in closed forests (0.88)         [mean open / mean closed = 0.77]

########### VINES ##############################
r3 <- raster(lc)
r3[lc==1 & agbdef>=olim & elev <1000] <- 1
r3 <- ratify(r3, count=TRUE)
writeRaster(r3, paste(outM,"vinesO.grd",sep="/"),overwrite=T)

########## HERBS/SHRUBS ##############################
r3 <- raster (lc)
r3[lc==1 & agbdef>=olim & elev >=1000] <- 1
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outM,"herbsO.grd",sep="/"),overwrite=T)

########### LANTANA ##############################
r3 <- raster (lc)
r3[agbdef<0] <- 1
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outM,"lantO.grd",sep="/"),overwrite=T)

##### METHODS RELEVANT TO SAVANNA & AGMOSAIC LC ################################
# AGB deficit limits for firebreaks and grass cutting:
plim <- 0.3 # Literature
rlim <- 0.4 # Literature
olim <- 0.5 # Literature

########## FIREBREAKS ##############################
r3 <- raster (lc)
r3[(lc==2 | lc==3 | lc==4) & agbdef>=olim] <- 1
r3 <- mask(r3, fbmask) # Crop out areas enclosed by forest
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outM,"fireO.grd",sep="/"),overwrite=T)

########## GRASS ##############################
r3 <- raster (lc)
r3[(lc==2 | lc==3 | lc==4) & agbdef>=olim] <- 1
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outM,"grassO.grd",sep="/"),overwrite=T)

########## TREE PLANTING METHODS ###############################################
# AGB deficit limits for framework species planting:
plim <- 0.50 # Comparison of known areas where fsa planting is necessary based on ground observations with AGB deficit raster
rlim <- 0.65 # Comparison of known areas where fsa planting is necessary based on ground observations with AGB deficit raster
olim <- 0.80 # Literature

########## FRAMEWORK SPECIES ##############################
r3 <- raster (lc)
r3[((lc==2 | lc==3) & (agbdef>=olim & agbdef<0.99) & distsav>=300) | (lc==4 & (agbdef>=olim & agbdef<0.99) & distforsav>=300)] <- 1
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outM,"fsaO.grd",sep="/"),overwrite=T)

########## ENRICHMENT PLANTING ##############################
r3 <- raster(lc)
r3[agbdef<0] <- 1
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outM,"enrichO.grd",sep="/"),overwrite=T)

########## NURSE TREES ##############################
r3 <- raster (lc)
r3[(lc==2 | lc==3 | lc==4) & agbdef>=0.99] <- 1
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outM,"nurseO.grd",sep="/"),overwrite=T)

########## SOIL ##############################
r3 <- raster (lc)
r3[(lc==2 | lc==3 | lc==4) & agbdef>=0.99] <- 1
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outM,"soilO.grd",sep="/"),overwrite=T)

########## COMMUNITY ENGAGEMENT ##############################
# Assumed necessary for pixels with deficit that are:
# (a) within agmosaic (100%) or within 3km of agmosaic (50%); and, (b) outside Protected Areas and large commercial farms
r3 <- raster(lc)
r3[agbdef>0 & pdistag<=3000] <- 1
r3 <- mask(r3, cl, inverse=T)
r3 <- mask(r3, pas, inverse=T)
r3[r3==1 & lc!=4 & pdistag<=3000] <- 0.5             # Adjust for cost variation outside Agriculture Mosaic areas
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outM,"commengO.grd",sep="/"),overwrite=T)

######### COMBINED METHODS: PASSIVE VS ANR VS ACTIVE PLANTING ##################

########## ACTIVE RESTORATION ~ TREE PLANTING ##################################
# Combine tree planting methods rasters into single tree planting raster...

# PESS
r1 <- raster(paste(outM,"fsaP.grd",sep="/"))
r2 <- raster(paste(outM,"nurseP.grd",sep="/"))
r3 <- raster(paste(outM,"enrichP.grd",sep="/"))
st <- stack(r1, r2, r3)
r1 <- sum(st, na.rm=TRUE)
r1[r1>=1] <- 1
r1[r1==0] <- NA
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outA,"activeP.grd",sep="/"),overwrite=T)

# REAL
r1 <- raster(paste(outM,"fsaR.grd",sep="/"))
r2 <- raster(paste(outM,"nurseR.grd",sep="/"))
r3 <- raster(paste(outM,"enrichR.grd",sep="/"))
st <- stack(r1, r2, r3)
r2 <- sum(st, na.rm=TRUE)
r2[r2>=1] <- 1
r2[r2==0] <- NA
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outA,"activeR.grd",sep="/"),overwrite=T)

# OPT
r1 <- raster(paste(outM,"fsaO.grd",sep="/"))
r2 <- raster(paste(outM,"nurseO.grd",sep="/"))
r3 <- raster(paste(outM,"enrichO.grd",sep="/"))
st <- stack(r1, r2, r3)
r3 <- sum(st, na.rm=TRUE)
r3[r3>=1] <- 1
r3[r3==0] <- NA
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outA,"activeO.grd",sep="/"),overwrite=T)

########## ASSISTED NATURAL REGENERATION (ANR) #################################

# PESS
r <- raster(paste(outA,"activeP.grd",sep="/"))
r1 <- raster(paste(outM,"grassP.grd",sep="/"))
r2 <- raster(paste(outM,"vinesP.grd",sep="/"))
r3 <- raster(paste(outM,"herbsP.grd",sep="/"))
r4 <- raster(paste(outM,"lantP.grd",sep="/"))
r5 <- raster(paste(outM,"fireP.grd",sep="/"))
st <- stack(r1, r2, r3, r4, r5)
r1 <- sum(st, na.rm=TRUE)
r1[r1>=1] <- 1
r1[r1==0] <- NA
r1 <- mask(r1, r, inverse=T)
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outA,"anrP.grd",sep="/"),overwrite=T)

# REAL
r <- raster(paste(outA,"activeR.grd",sep="/"))
r1 <- raster(paste(outM,"grassR.grd",sep="/"))
r2 <- raster(paste(outM,"vinesR.grd",sep="/"))
r3 <- raster(paste(outM,"herbsR.grd",sep="/"))
r4 <- raster(paste(outM,"lantR.grd",sep="/"))
r5 <- raster(paste(outM,"fireR.grd",sep="/"))
st <- stack(r1, r2, r3, r4, r5)
r2 <- sum(st, na.rm=TRUE)
r2[r2>=1] <- 1
r2[r2==0] <- NA
r2 <- mask(r2, r, inverse=T)
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outA,"anrR.grd",sep="/"),overwrite=T)

# OPT
r <- raster(paste(outA,"activeO.grd",sep="/"))
r1 <- raster(paste(outM,"grassO.grd",sep="/"))
r2 <- raster(paste(outM,"vinesO.grd",sep="/"))
r3 <- raster(paste(outM,"herbsO.grd",sep="/"))
r4 <- raster(paste(outM,"lantO.grd",sep="/"))
r5 <- raster(paste(outM,"fireO.grd",sep="/"))
st <- stack(r1, r2, r3, r4, r5)
r3 <- sum(st, na.rm=TRUE)
r3[r3>=1] <- 1
r3[r3==0] <- NA
r3 <- mask(r3, r, inverse=T)
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outA,"anrO.grd",sep="/"),overwrite=T)

########## ANY RESTORATION INTERVENTION ##############################

# PESS
r1 <- raster(paste(outA,"activeP.grd",sep="/"))
r2 <- raster(paste(outA,"anrP.grd",sep="/"))
r3 <- raster(paste(outA,"passiveP.grd",sep="/"))
st <- stack(r1, r2, r3)
r1 <- sum(st, na.rm=TRUE)
r1[r1>=1] <- 1
r1[r1==0] <- NA
r1 <- ratify(r1, count=T)
writeRaster(r1, paste(outA,"restoreP.grd",sep="/"),overwrite=T)

# REAL
r1 <- raster(paste(outA,"activeR.grd",sep="/"))
r2 <- raster(paste(outA,"anrR.grd",sep="/"))
r3 <- raster(paste(outA,"passiveR.grd",sep="/"))
st <- stack(r1, r2, r3)
r2 <- sum(st, na.rm=TRUE)
r2[r2>=1] <- 1
r2[r2==0] <- NA
r2 <- ratify(r2, count=T)
writeRaster(r2, paste(outA,"restoreR.grd",sep="/"),overwrite=T)

# OPT
r1 <- raster(paste(outA,"activeO.grd",sep="/"))
r2 <- raster(paste(outA,"anrO.grd",sep="/"))
r3 <- raster(paste(outA,"passiveO.grd",sep="/"))
st <- stack(r1, r2, r3)
r3 <- sum(st, na.rm=TRUE)
r3[r3>=1] <- 1
r3[r3==0] <- NA
r3 <- ratify(r3, count=T)
writeRaster(r3, paste(outA,"restoreO.grd",sep="/"),overwrite=T)

################## METHOD TYPE ############################

### Pessimistic
r0 <- raster(paste(outA,"noneP.grd",sep="/"))
r1 <- raster(paste(outA,"passiveP.grd",sep="/"))
r2 <- raster(paste(outA,"anrP.grd",sep="/"))
r3 <- raster(paste(outA,"activeP.grd",sep="/"))
r0[r0==1] <- 0
r1[r1==1] <- 1
r2[r2==1] <- 2
r3[r3==1] <- 3
st <- stack(r0, r1, r2, r3)
r <- sum(st, na.rm=T)
r <- mask(r, sa)
r <- ratify(r, count=T)
rat <- levels(r)[[1]]
rat[["TYPE"]] <- c("None", "Passive", "ANR", "Active")
levels(r) <- rat
levels(r)
writeRaster(r, paste(outA,"methodP.grd",sep="/"),overwrite=T)
r[r==0] <- NA
writeRaster(r, paste(inDir,"relP.grd",sep="/"),overwrite=T)

### Realistic
r0 <- raster(paste(outA,"noneR.grd",sep="/"))
r1 <- raster(paste(outA,"passiveR.grd",sep="/"))
r2 <- raster(paste(outA,"anrR.grd",sep="/"))
r3 <- raster(paste(outA,"activeR.grd",sep="/"))
r0[r0==1] <- 0
r1[r1==1] <- 1
r2[r2==1] <- 2
r3[r3==1] <- 3
st <- stack(r0, r1, r2, r3)
r <- sum(st, na.rm=T)
r <- mask(r, sa)
r <- ratify(r, count=T)
rat <- levels(r)[[1]]
rat[["TYPE"]] <- c("None", "Passive", "ANR", "Active")
levels(r) <- rat
levels(r)
writeRaster(r, paste(outA,"methodR.grd",sep="/"),overwrite=T)
r[r==0] <- NA
writeRaster(r, paste(inDir,"relR.grd",sep="/"),overwrite=T)

### Optimistic
r0 <- raster(paste(outA,"noneO.grd",sep="/"))
r1 <- raster(paste(outA,"passiveO.grd",sep="/"))
r2 <- raster(paste(outA,"anrO.grd",sep="/"))
r3 <- raster(paste(outA,"activeO.grd",sep="/"))
r0[r0==1] <- 0
r1[r1==1] <- 1
r2[r2==1] <- 2
r3[r3==1] <- 3
st <- stack(r0, r1, r2, r3)
r <- sum(st, na.rm=T)
r <- mask(r, sa)
r <- ratify(r, count=T)
rat <- levels(r)[[1]]
rat[["TYPE"]] <- c("None", "Passive") #, "ANR", "Active")
levels(r) <- rat
levels(r)
writeRaster(r, paste(outA,"methodO.grd",sep="/"),overwrite=T)
r[r==0] <- NA
writeRaster(r, paste(inDir,"relO.grd",sep="/"),overwrite=T)
