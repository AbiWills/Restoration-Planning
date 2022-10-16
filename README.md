# Restoration-Planning

This readme file was generated on 2022-09-23 by Abigail Wills

GENERAL INFORMATION

Title of Dataset: A practice-led assessment of landscape restoration potential in a biodiversity hotspot

Author/Principal Investigator Information
Name: Abigail Wills
ORCID: https://orcid.org/0000-0003-3370-156X
Institution: University of York
Email: wills.abigail@gmail.com

Author/Associate or Co-investigator Information
Name: Andrew Marshall
Institution: University of the Sunshine Coast
Email: amarsha1@usc.edu.au

Author/Alternate Contact Information
Name: Marion Pfeifer
Institution: Newcastle University
Email: Marion.Pfeifer@newcastle.ac.uk

Date of data collection: 2019-2021

Geographic location of data collection: Udzungwa-Kilombero Region, Tanzania (study region); Extent (WGS 84 UTM 36): 685971.2, 1030471, 8901502, 9211602  (xmin, xmax, ymin, ymax)

SHARING/ACCESS INFORMATION

Licenses/restrictions placed on the data: C0

Links to publications that cite or use the data: https://doi.org/10.1098/rstb.2021.0070

Links to other publicly accessible locations of the data: 
Full dataset (https://doi.org/10.5061/dryad.nk98sf7wr)
R code (https://bit.ly/3KO5Hgz)
Maps (https://bit.ly/3QkIrrI)

Links/relationships to ancillary data sets: See "Additional related data not included in current data package" below

Recommended citation for this dataset: As per https://doi.org/10.1098/rstb.2021.0070

DATA & FILE OVERVIEW

This dataset is organised into Inputs (tabular data in Excel/csv format, segments of R code, and spatial datasets in shp/tif format), Analysis (R code), 
and key Outputs (spatial datasets in tif format, organised under subheadings pertaining to R code used to generate each output or set of outputs). 
All Input data have been formatted for reading into and processing using R analysis code supplied. Note that input files have been organised into sub-folders for sharing, 
these should be deposited into a single 'inputs' file folder for processing usign the R code provided. The input and output folder structure used is as follows:

R/workspaces/Project_Folder/
  inputs
  outputs/
    uncertainty
    methods
    agb-gain
    costs
    cost-effectiveness

File List (Inputs):

Tables

current_agb_plot_coords.csv - Coordinates of vegetation plots used to generate AGB estimates subsequently upscaled to predict current AGB across the landscape
rf_model_importancevalues_satelliteupscale.csv - Relative contribution of each spectral reflectance predictor used to model current landscape AGB from plot data
former_agb_plot_coords.csv - Coordinates of vegetation plots used to generate AGB estimates subsequently upscaled to predict maximum potential AGB across the landscape
rf_model_importancevalues_climateupscale.csv - Relative contribution of each climatic predictor used to model maximum landscape AGB from plot data

restorationCosts.xlxs - Restoration costs calculation inputs (detailed calculation and justification of input variables used to calculate landscape restoration costs)
restorationCosts.csv - Restoration costs calculation inputs (simplified csv version of restorationCosts.xlxs, formatted for analysis in R using code provided with this dataset)
firebreakCalc.csv - Framework used to determine firebreak size and type
agbgain-bernal-afr-regen-meanagb.csv - See "Additional related data not included in current data package" below

R Code

restCosts_calc.r - Code to estimate 5-year restoration labour and equipment costs per hectare
restCosts_calc-full-recovery.r - Code to estimate restoration labour and equipment costs per hectare to full-recovery of maximum above-ground biomass
tsportCosts_calc.r - Code to estimate 5-year restoration transport costs per hectare
tsportCosts_calc-full-recovery.r - Code to estimate restoration transport costs per hectare to full-recovery of maximum above-ground biomass

Spatial datasets (all pertain to study region area and/or extent)

General

Study_Area.shp - Shapefile comprising study region as polygon feature layer
Extent_1.tif - Template raster with chosen extent, resolution, CRS, etc. for processing and creation of output maps 
(ideally all input maps created to match this, though in case not they can be converted using R script provided with this dataset)

Geophysical

environ_raster_climate_agb.tif - Raster containing climatic variables used to predict maximum potential landscape AGB from plot-based estimates as bands
dem_srtm_43_4414.tif - Study region digital elevation model (DEM) map

Landcover / use

Commercial_land.shp - Shapefile comprising commercial land as polygon feature layer, excluded from restoration planning
WDPA_less-open-gamecontrol-wma_clip.tif - Categorical raster distinguishing protected (values 1-5) from unprotected areas (NA)
LandCoverFinal.tif - Study region categorical landcover classification raster, where: 1=Forest, 2=Savanna spectrum, 3=Floodplain/swamp, 4=Agro-ecological mosaic, 5=Other
SavAgFlood1ha_noEnclosed.tif - Raster comprising all landcover classes except forest and other, excluding areas enclosed by forest (used to determine firebreak size and type)
landprices.tif - Estimated land price of each landscape pixel, based on known settlement / commercial land prices, with non-commercial land decreasing in value by $1 per metre from settlement to minimum $281 (Udekwa village)

Biomass

current_agb_plot_coords.shp - Shapefile comprising point feature locations of plots used in current AGB upscaling
former_agb_plot_coords.shp - Shapefile comprising point feature locations of plots used in maximum AGB upscaling
agbcurr_1ha_rf_full.tif - Mean modelled current above-ground biomass (AGB) estimates per landscape pixel
agbcurr_lower_1ha_rf_full.tif - Lower 95% CI modelled current AGB estimates per landscape pixel
agbcurr_upper_1ha_rf_full.tif - Upper 95% CI modelled current AGB estimates per landscape pixel
agbmax_climate_notaggregated.tif - Mean modelled maximum AGB estimates per landscape pixel
agbmax_lower_climate_notaggregated.tif - Lower 95% CI modelled maximum AGB estimates per landscape pixel
agbmax_upper_climate_notaggregated.tif - Upper 95% CI modelled maximum AGB estimates per landscape pixel

Secondary input maps

Transformed

The following 8 rasters were generated using code 0-prep-inputs.r to match extent, resolution, CRS, etc. of Extent_1.tif, with subsequent analyses performed on the transformed datasets:
   LandCoverFinal_1ha.tif - LandCoverFinal.tif resampled (nearest neighbour method) to match Extent_1.tif, using code 0-prep-inputs.r
   dem_srtm_1ha - dem_srtm_43_4414.tif resampled (bilinear method) to match Extent_1.tif, using code 0-prep-inputs.r
   agbcurr.tif - agbcurr_1ha_rf_full.tif resampled (bilinear method) to match Extent_1.tif, using code 0-prep-inputs.r
   agbcurr_lower.tif - agbcurr_lower_1ha_rf_full.tif resampled (bilinear method) to match Extent_1.tif, using code 0-prep-inputs.r
   agbcurr_upper.tif - agbcurr_upper_1ha_rf_full.tif resampled (bilinear method) to match Extent_1.tif, using code 0-prep-inputs.r
   agbmax.tif - agbmax_climate_notaggregated.tif resampled (bilinear method) to match Extent_1.tif, using code 0-prep-inputs.r
   agbmax_lower.tif - agbmax_lower_climate_notaggregated.tif resampled (bilinear method) to match Extent_1.tif, using code 0-prep-inputs.r
   agbmax_upper.tif - agbmax_upper_climate_notaggregated.tif resampled (bilinear method) to match Extent_1.tif, using code 0-prep-inputs.r

Landcover

The following 3 rasters were generated using code 2-methods.r 
   LandCover_intact.tif - Realistic intact landcover map, where pixels with AGB deficit <80% are considered functionally 'intact' (all other pixels = NA)
   LandCover_intact_pess.tif - Pessimistic intact landcover map, where pixels with AGB deficit <70% are considered functionally 'intact' (all other pixels = NA)
   LandCover_intact_opt.tif - Optimistic intact landcover map, where pixels with AGB deficit <90% are considered functionally 'intact' (all other pixels = NA)

Distance matrices

The following 4 distance matrices were created from  LandCoverFinal_1ha.tif using ArcGIS Pro version 2.7.1 (ESRI Inc. 2020)
   dist_agmosaic_1ha.tif - Euclidean distance of each landscape pixel from agro-ecological mosaic landcover class
   dist_road_1ha.tif- Euclidean distance of each landscape pixel from nearest road
   pathdist_agmosaic.tif - Path distance of each landscape pixel from agro-ecological mosaic
   pathdist_road.tif - Path distance of each landscape pixel from nearest road

The following 3 rasters were created from LandCover_intact.tif, along with pessimistic (xxx_pess.tif) and optimistic (xxx_opt.tif) equivalents, using ArcGIS Pro version 2.7.1 (ESRI Inc. 2020)
   dist_intact_forest.tif - Euclidean distance of each landscape pixel from nearest 'intact' forest landcover class
   dist_intact_savflood.tif - Euclidean distance of each landscape pixel from neared 'intact' savanna / floodplain landcover class
   dist_intact_forsavflood.tif - Euclidean distance of each landscape pixel from nearest 'intact' forest / savanna / floodplain landcover class

Raster stacks

The following raster stack was generated using code 2-methods.r for assigning most likely appropriate methods for restoring AGB to each landscape pixel
methods_stack.grd* - stack of raster datasets containing variables used to assign restoration methods per landscape pixel, as follows:
   LandCoverFinal_1ha.tif
   agbdef_pct.tif*
   dem_srtm_1ha.tif
   dist_intact_forest.tif*
   dist_intact_savflood.tif*
   dist_intact_forsavflood.tif*
   dist_agmosaic_1ha.tif
   dist_road_1ha.tif
   pathdist_agmosaic.tif
*Pessimistic (xxx_pess.tif) and optimistic (xxx_opt.tif) versions also generated.

The following raster stack was generated using code 4a-costs-inputs.r for use in calculation of restoration costs
costs_stack.grd - stacked of raster datasets containing variables used to compute restoration costs per landscape pixel, as follows:
   LandCoverFinal_1ha.tif
   dem_srtm_1ha.tif
   campnights_7km.grd - categorical raster indicating number of nights camping required to access restoration site, based on distance from nearest road / agro-ecological mosaic
   road_tsport_need.grd - categorical raster indicating whether road transport is required to access restoration site, based on distance from nearest road / agro-ecological mosaic
   pathdist_agmosaic.tif
   pathdist_road.tif
   road_dist_1ha.tif

File List (Analysis):

R Code

0-prep-inputs.r - Code to ensure all input rasters have same extent, resolution, CRS, etc. for processing and to perform conversions where necessary
1a-biomass.r - Code to estimate AGB deficit from current and maximum former biomass maps
1b-biomass-uncertainty-maps.r - Code to generate envelope uncertainty maps (EUMs), following Platts et al. 2008
2-methods.r - Code to assign most likely appropriate restoration methods to each landscape pixel (based on parameters and thresholds in Table 1 in https://doi.org/10.1098/rstb.2021.0070)
3-biomass-gain.r - Code to estimate AGB gain after 5 years, and number of years to full-recovery of AGB deficit
4a-costs-inputs.r - Code to generate determinents of restoration labour, equipment and transport costs, as detailed in Table S1 and S2 in https://doi.org/10.1098/rstb.2021.0070
4b-costs-labour-and-equipment-5y.r - Code to calculate the total labour and equipment costs incurred for each restoration method over a (adjustable) 5 year timeframe
4c-costs-transport-5y.r - Code to calculate the total transport costs incurred for each restoration method over a (adjustable) 5 year timeframe
4d-costs-labour-and-equipment-full-recovery.r - Code to calculate the total labour and equipment costs incurred for each restoration method to the point of full AGB recovery
4e-costs-transport-full-recovery.r - Code to calculate the total transport costs incurred for each restoration method to the point of full AGB recovery
4f-costs-totals.r - Code to sum the total labour, equipment, transport and admin costs for each restoration method and overall across the landscape
5-cost-effectiveness.r - Code to calculate cost-effectiveness of each landscape pixel based on AGB gains per unit investment, and to identify the top 25% cost-effective pixels for prioritisation

File List (Outputs): Spatial datasets

Biomass deficit (Generated in 1a-biomass.r with corresponding pessimistic [xxx_pess.tif] and optimistic [xxx_opt.tif] maps also produced)

agbdiff.tif - Realistic difference in current and maximum potential AGB (mean maximum - mean current)
agbdef.tif - Realistic AGB deficit (i.e. where AGB difference >0)
agbdiff_pct.tif - Realistic AGB difference as percentage of maximum potential AGB
agbdef_pct.tif - Realistic AGB deficit as percentage of maximum potential AGB
agbrem_pct.tif - Realistic percentage of maximum potential AGB remaining (current as percentage of maximum AGB)
biomass_stack.grd - Stack of above 5 raster datasets

Uncertainty (Generated in 1b-biomass-uncertainty-maps.r)

current_agb_eum - Envelope uncertainty map (EUM) for current landscape AGB predicted from plot data upscaled using spectral reflectance predictors
former_agb_eum - EUM for maximum potential landscape AGB predicted from plot data upscaled using climatic predictors

Restoration methods (Generated in 2-methods.r)

Restoration methods maps generated, all with pessimistic (xxxP.grd) and optimistic (xxxO.grd) counterparts, include:
noneR.grd - No restoration intervention
passiveR.grd - Areas expected to recover naturally without silvicultural intervention
vinesR.grd - Areas requiring vine cutting
herbsR.grd - Areas reqiring management of herbs and shrubs
lantR.grd - Areas requiring lantana control
fireR.grd - Areas requiring firebreaks
grassR.grd - Areas requiring firebreaks
fsaR.grd - Areas requiring framework species planting
enrichR.grd - Areas requiring enrichment planting
nurseR.grd - Areas requiring nurse tree planting
soilR.grd - Areas requiring soil improvement
commengR.grd - Areas requiring community engagement

Restoration approach

Methods rasters used to indicate restoration approach, e.g. assisted natural regeneration (ANR), planting, etc., incl. pessimistic (xxxP.grd) and optimistic (xxxO.grd) counterparts:
anrR.grd - Areas restored through ANR, including vine cutting, herb and shrub cutting, lantana control, firebreaks, and/or grass cutting
activeR.grd - Areas restored through planting, including framework species, enrichment and/or nurse tree planting with or without soil improvement
restoreR.grd - Areas requiring restration intervention through ANR or planting
methodR.tif - Categorical raster indicating realistic appropriate restoration method (none, passive, ANR, or planting) for each landscape pixel

Biomass gains (Generated in 3-biomass-gain.r)

agbGain_P5.tif - Pessimistic AGB gain after 5 years
agbGain_R5.tif - Realistic AGB gain after 5 years
agbGain_O5.tif - Optimistic AGB gain after 5 years
agbGain_Pfull.tif - Pessimistic AGB gain to achieve full-recovery of AGB deficit
agbGain_Rfull.tif - Realistic AGB gain to achieve full-recovery of AGB deficit
agbGain_Ofull.tif - Optimistic AGB gain to achieve full-recovery of AGB deficit
years-to-recovery-P.tif - Pessimistic number of years to full-recovery of AGB deficit
years-to-recovery-R.tif - Realistic number of years to full-recovery of AGB deficit
years-to-recovery-O.tif - Optimistic number of years to full-recovery of AGB deficit
pctdefrec_P5.tif - Pessimistic percentage of AGB deficit recovered after 5 years
pctdefrec_R5.tif - Realistic percentage of AGB deficit recovered after 5 years
pctdefrec_O5.tif - Optimistic percentage of AGB deficit recovered after 5 years

Restoration costs (Generated in 4f-costs-totals.r)

total-cost-R5USD.tif - Total realistic restoration cost in USD per hectare landscape pixel over 5 years, including all labour, equipment, transport, community engagement and administrative costs 
total-cost-P5USD.tif - Total pessimistic restoration cost in USD per hectare landscape pixel over 5 years, including all above costs 
total-cost-O5USD.tif - Total optimistic restoration cost in USD per hectare landscape pixel over 5 years, including all above costs 
total-cost-RFUSD.tif - Total realistic restoration cost in USD per hectare landscape pixel to full AGB recovery, including all above costs 
total-cost-PFUSD.tif - Total pessimistic restoration cost in USD per hectare landscape pixel to full AGB recovery, including all above costs 
total-cost-OFUSD.tif - Total optimistic restoration cost in USD per hectare landscape pixel to full AGB recovery, including all above costs

Note: Intermediary costs rasters for individual methods (vine cutting, framework species planting, etc.) in TZS and USD also computed.
However, these were not included in the current data package as the files are numerous. They can be made available on request.

Cost-effectiveness (Generated in 5-cost-effectiveness.r)

AGBgainUSD100_R5.tif - Realistic cost-effectiveness (Mg AGB gained per $100 investment) per hectare landscape pixel after 5 years
AGBgainUSD100_P5.tif - Pessimistic cost-effectiveness (Mg AGB gained per $100 investment) per hectare landscape pixel after 5 years
AGBgainUSD100_O5.tif - Optimistic cost-effectiveness (Mg AGB gained per $100 investment) per hectare landscape pixel after 5 years
AGBgainUSD100_RF.tif - Realistic cost-effectiveness (Mg AGB gained per $100 investment) per hectare landscape pixel to full AGB recovery
AGBgainUSD100_PF.tif - Pessimistic cost-effectiveness (Mg AGB gained per $100 investment) per hectare landscape pixel to full AGB recovery
AGBgainUSD100_OF.tif - Optimistic cost-effectiveness (Mg AGB gained per $100 investment) per hectare landscape pixel to full AGB recovery

Additional related data not included in current data package:

Above-ground biomass (AGB) measurements from plot data - 
AGB (Mg per hectare) was estimated from 17,983 woody plant stems (trees, lianas, palms and stranglers) measured between 2007 and 2017 in 195 plots
Data and references cited in Table S7 of publication (https://doi.org/10.1098/rstb.2021.0070) associated with this dataset

Temp_Tiles_google1/xxx.tif (100 files) - 
Google Earth Engine Landsat raster (Goog_RefTex_Sub.tif) split into tile 100 tiles for processing using code: 
01 code - Force Analyses Prepare rasters.r (related data not included in current data package, details below)
Used to upscale plot based AGB estimates to predict current landscape AGB using code: 
Force_Restoration_Paper_Abi v2.r (related data not included in current data package, details below)
Also used to generate envelope uncertainty maps using code: 1b-biomass-uncertainty-maps.r (included in current data package)

01 code - Force Analyses Prepare rasters.r
Code used to split Google Earth Engine Landsat raster (Goog_RefTex_Sub.tif) into tile 100 tiles for processing
Contact Co-author Marion Pfeifer for further details and/or to request data access

Force_Restoration_Paper_Abi v2.r - 
R script used to model current and former landscape AGB from plot measurements
Output current and maximum potential landscape AGB maps included in the current data package
Methods for computing these maps described in publication (https://doi.org/10.1098/rstb.2021.0070) associated with this dataset
Contact Co-author Marion Pfeifer for further details and/or to request data access

agbgain-bernal-afr-regen-meanagb.csv - 
Comprises cumulative modelled annual mean, maximum and minimum estimates of above-ground carbon (AGC, from zero to maximum)
and derivatives: mean, maximum and minimum AGC as % of maximum AGC; and mean, maximum and minimum annual change in AGC
Generated based on vegetation plot AGB measurement over time in naturally-regenerating African forests (Bernal et al. 2018)
Used to estimate AGB gain after 5 years of restoration intervention and number of years to full-recovery of AGB deficit (in 1a-biomass.r)
Contact Bernal et al. 2018 (for full citation, see https://doi.org/10.1098/rstb.2021.0070) or 
See 1a-biomass.r for table format for recreation using comparable dataset

METHODOLOGICAL INFORMATION

Description of methods used for collection/generation of data: Details provided within individual datasets and/or in https://doi.org/10.1098/rstb.2021.0070

Methods for processing the data: Details provided within individual datasets and/or in https://doi.org/10.1098/rstb.2021.0070

Instrument- or software-specific information needed to interpret the data:

All statistical and spatial analyses were conducted using R version 4.0.1 (R Core Team 2020), 
besides the distance matrices and maps, which were produced in ArcGIS Pro version 2.7.1 (ESRI Inc. 2020)
The caret package was used for modelling (Kuhn 2008) and the raster package for spatial up-scaling (Hijmans and van Etten 2012)
Statistical analyses were performed using the R base package

People involved with sample collection, processing, analysis and/or submission: Full list of authors and contributors as per https://doi.org/10.1098/rstb.2021.0070

TABULAR DATASETS

Filename: current_agb_plot_coords.csv
Description: Coordinates of vegetation plots used to generate AGB estimates subsequently upscaled to predict current AGB across the landscape
Variables:
  project_plotno - unique plot identifier
  latitute_geo - plot latitude (WGS 1984 UTM 36S)
  longitude_geo - plot longitude (WGS 1984 UTM 36S)

Filename: rf_model_importancevalues_satelliteupscale.csv
Description: Relative contribution of each spectral reflectance predictor used to model current landscape AGB from plot data
Variables:
  Predictor - name of predictor variable
  Overall - relative contribution of predictor to model

Filename: former_agb_plot_coords.csv
Description: Coordinates of vegetation plots used to generate AGB estimates subsequently upscaled to predict maximum potential AGB across the landscape
Variables:
  latitute_geo - plot latitude (WGS 1984 UTM 36S)
  longitude_geo - plot longitude (WGS 1984 UTM 36S)

Filename: rf_model_importancevalues_climateupscale.csv
Description: Relative contribution of each climatic predictor used to model maximum landscape AGB from plot data
Variables:
  Predictor - name of predictor variable
  Overall - relative contribution of predictor to model

Filename: restorationCosts.xlxs
Description: Restoration costs calculation inputs (detailed calculation and justification of input variables used to calculate landscape restoration costs)
Variables: Refer to Readme tab in workbook

Filename: restorationCosts.csv
Description: Restoration costs calculation inputs (simplified csv version of restorationCosts.xlxs, formatted for analysis in R using code provided with this dataset)
Purpose: This table is designed to facilitate estimation of the financial implementation costs of landscape above-ground biomass (AGB) restoration 
in the Udzungwa-Kilombero Landscape in Tanzania. All land procurement, labour, equipment and transport, community engagement, project management and 
administrative costs associated with different potentially applicable restoration methods are accounted for. The dataset comprises 30 data columns for each restoration method, 
comprising pessimistic (P), realistic (R) and optimistic (O) estimates for 10 variables, as listed below. This format is designed to facilitate subsequent extraction 
and computation of total restoration implementation costs per hectare landscape pixel after 5 years and to full AGB recovery in R version 4.0.1 (R Core Team 2020, 
using script provided with this dataset and also made available online: https://bit.ly/3KO5Hgz).
Variables:
cost = labour costs per ha per year; 
styr = start year for activity; 
yrs = number of years activity is repeated; 
reps = number of times activity is repeated per year; 
int = interval between years (i.e. activity is repeated at X year intervals [int] for X years [yrs]); 
eqp = equipment cost per hectare per year; 
life = equipment lifetime before need for replacement due to depreciation; 
team = number of people / team size required to conduct activity (relevant for transport costs); 
haday = number of hectares to which method can be applied per team per day (relevant for transport costs);
stems = number of stems per tree planting method and able to be transported in one trip for each transport method (relevant for transport costs).
Unique values:
No data or N/A values are indicated using ".". 
Interval (int) values of 9999 indicate method is not repeated, chosen since 9999 years falls outside maximum number of years to full AGB recovery.

Filename: firebreakCalc.csv
Description: Framework used to determine firebreak type and size
Variables:
code - numeric code to indicate land use type category, where: 
       1 = all pixels within the largest protected areas
       2 = pixels within all other protected areas and/or unprotected savanna areas
       3 = all pixels classed as agriculture mosaic/other
grass - binary variable indicating whethere grass cutting is already being employed as a restoration method (1) or not (0)
m2P - pessimistic threshold size of management area (m2) used to determine firebreak type and size (as percentage of area being managed/restored)
m2R - realistic threshold size of management area (m2) used to determine firebreak type and size (as percentage of area being managed/restored)
m2O - optimistic threshold size of management area (m2) used to determine firebreak type and size (as percentage of area being managed/restored)
typeP - binary variable indicating pessimistic firebreak type needed, none (0), external only (1), or internal and external (2), based on presence of grass cutting and size of management area
typeR - binary variable indicating realistic firebreak type needed, none (0), external only (1), or internal and external (2), based on presence of grass cutting and size of management area
typeO - binary variable indicating optimistic firebreak type needed, none (0), external only (1), or internal and external (2), based on presence of grass cutting and size of management area
percP - pessimistic proportion of restoration area managed using firebreaks
percR - realistic proportion of restoration area managed using firebreaks
percO - optimistic proportion of restoration area managed using firebreaks
