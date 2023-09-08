# ------------------------------------------------------------------- X
# ------------------------------------------------------------------- X
# ---------------------------- Data Prep ---------------------------- X
# ------------------------------------------------------------------- X
# ---- Prepare Geospatial Data for Demos, Examples, and Exercies ---- X
# ------------------------------------------------------------------- X
# ------------------------------------------------------------------- X

library(tidyverse)
library(sf)
# library(raster)
library(terra)
library(lubridate)

raw_dir <- "Data/Raw/"

# 1. Filter Utah Roads shapefile to just Freeways/Interstates ----
# Utah Road Shapefile: https://gis.utah.gov/data/transportation/roads-system/
roads <- st_read(paste0(raw_dir, "Utah_Roads"))
unique(roads$CARTOCODE)
# 1 Interstates
# 2 US Highways, Separated
# 3 US Highways, Unseparated
# 4 Major State Highways, Separated
# 5 Major State Highways, Unseparated
# 6 Other State Highways (Institutional)
# 7 Ramps, Collectors
# 8 Major Local Roads, Paved
# 9 Major Local Roads, Not Paved
# 10 Other Federal Aid Eligible Local Roads
# 11 Other Local, Neighborhood, Rural Roads
# 12 Other
# 13 Non-road feature
# 14 Driveway
# 15 Proposed
# 16 4WD and/or high clearance may be required
# 17 Service Access Roads
# 18 General Access Roads

# filter to just the interstates
interstate <- filter(roads, CARTOCODE == 1)
plot(interstate$geometry)

# Remove unnecessary columns
colnames(interstate)
head(interstate)
interstate <- interstate %>%
  dplyr::select(c(OBJECTID, FULLNAME, NAME, POSTTYPE, SPEED_LMT, DOT_HWYNAM,
                  DOT_SRFTYP, DOT_AADT, UNIQUE_ID))
head(interstate)

# Save
interstate_dir <- "Data/Examples/utah_interstate"
if(!dir.exists(interstate_dir)){
  dir.create(interstate_dir, recursive = T)
}
st_write(interstate, dsn = interstate_dir, layer = "utah_interstate",
         driver = "ESRI Shapefile")


# 2. Clip Utah landcover raster to a small extent for a demo ----
# State-wide landcover: https://www.usgs.gov/programs/gap-analysis-project/science/land-cover-data-download
ut_land <- raster(paste0(raw_dir, "Utah_Landcover/Utah_Landcover.tif"))

# Create a new extent
xmin <- 498000
xmax <- 510000
ymin <- 4522000
ymax <- 4534000

# Turn these extents into an sf object so we can project it to the landcover's CRS
rect <- st_sfc(
  st_point(c(xmin, ymin)),
  st_point(c(xmin, ymax)),
  st_point(c(xmax, ymax)),
  st_point(c(xmax, ymin)),
  crs = 32612
) %>%
  st_transform(st_crs(ut_land))

ext <- extent(st_bbox(rect))

# Crop the landcover to this extent
land_demo <- crop(ut_land, ext)
plot(land_demo)

# Save
land_dir <- "Data/demo"
if(!dir.exists(land_dir)){
  dir.create(land_dir, recursive = T)
}
writeRaster(land_demo, paste0(land_dir, "landcover_demo"), "GTiff")

# Prep the summary table
land_summary <- read.csv(paste0(raw_dir, "Utah_Landcover/GAP_LANDFIRE_2011_Attributes.csv")) 
head(land_summary)

# Select just the columns "Value" and "NVC_CLASS"
land_summary_demo <- land_summary %>%
  dplyr::select(c(Value, NVC_CLASS)) %>%
  # rename "NVC_CLASS" to "Landcover
  rename(Landcover = NVC_CLASS)
head(land_summary_demo)

# Save
write.csv(land_summary_demo, paste0(land_dir, "/landcover_summary.csv"), row.names = F)


# 3. Export clip of landsat imagery from Google Earth Engine ----
# Note: you will need to set up a GEE account

# var geometry = ee.Geometry.Polygon(ee.List([
#   [-113.22352972720947,40.279605186950036],
#   [-111.14711371158447,40.279605186950036],
#   [-111.14711371158447,41.7877747223748],
#   [-113.22352972720947,41.7877747223748],
#   [-113.22352972720947,40.279605186950036]
# ]));
# var landsat = ee.ImageCollection("LANDSAT/LC08/C02/T1_TOA")
# .filterBounds(geometry)
# .filterDate('2000-01-01', '2022-12-31')
# .median()
# .clip(geometry)
# .select(['B4', 'B3', 'B2']);
# Export.image.toDrive({
#   image: landsat,
#   description: 'landsat_demo',
#   folder: 'landsat_demo', 
#   fileNamePrefix: 'landsat_demo', 
#   scale: 100, 
#   crs: 'EPSG:3857',
#   fileFormat: 'GeoTIFF'
# })


# 4. Make elevation and snow rasters ----
elev <- rast(paste0(raw_dir, "dem_wgs.tif"))
elev

snow <- rast(paste0(raw_dir, "SNODAS_20190223.tif"))
snow

elev_snow_ext_wgs <- ext(c(-112, -110, 40, 42))
elev_crop <- crop(elev, elev_snow_ext_wgs)
snow_crop <- crop(snow, elev_snow_ext_wgs)

elev_crop
snow_crop

plot(elev_crop)
plot(snow_crop)

# Project these rasters to 26912
utm_poly <- st_sfc(
    st_point(c(elev_snow_ext_wgs[1], elev_snow_ext_wgs[3])),
    st_point(c(elev_snow_ext_wgs[1], elev_snow_ext_wgs[4])),
    st_point(c(elev_snow_ext_wgs[2], elev_snow_ext_wgs[3])),
    st_point(c(elev_snow_ext_wgs[2], elev_snow_ext_wgs[4])),
  crs = 4326) %>%
  st_transform(26912) 
new_ext <- st_bbox(utm_poly)

template <- rast(extent = ext(new_ext), 
                 crs = crs(vect(utm_poly)), 
                 resolution = 1000)
template

elev_crop_proj <- project(elev_crop, template)
elev_crop_proj
plot(elev_crop_proj)

snow_crop_proj <- project(snow_crop, template)
snow_crop_proj
plot(snow_crop_proj)

writeRaster(elev_crop_proj, 
            filename = "Data/Examples/elev_crop.tif",overwrite = T)
writeRaster(snow_crop_proj, 
            filename = "Data/Examples/snow_crop.tif",overwrite = T)

elev_snow_stk <- c(elev_crop_proj, snow_crop_proj)
elev_snow_stk
writeRaster(elev_snow_stk, 
            filename = "Data/demo/elev_snow_nw_stack.tif",overwrite = T)


# 5. Make Sites dataframe ----
# make an extent of utah
utah <- maps::map("state", plot = F, fill = TRUE) %>%
  # turn into sf obj
  sf::st_as_sf() %>%
  # pull out utah 
  dplyr::filter(ID == "utah")
(utah_ext <- st_bbox(utah))

# 5466
# 5214
# 2507
# 2557
# 8246
(seed <- round(runif(1, max = 10000)))
# set.seed(seed)
set.seed(2557)
(sites <- data.frame(Site = 1:15,
                     Latitude = runif(15, min = utah_ext["ymin"], max = utah_ext["ymax"]),
                     Longitude = runif(15, min = utah_ext["xmin"], max = utah_ext["xmax"])))
plot(elev_snow_stk$elevation)
plot(st_geometry(sites_sf_proj), pch = 16, add = T)

# Check to see if there are any sites within the elevation/snow rasters extent
sites %>%
  filter(Latitude >= elev_snow_ext_wgs[3] & Latitude <= elev_snow_ext_wgs[4] & 
           Longitude >= elev_snow_ext_wgs[1] & Longitude <= elev_snow_ext_wgs[2])

ggplot(sites) +
  geom_sf(data = utah) +
  geom_point(aes(x = Longitude, y = Latitude))

# Save
write.csv(sites, "Data/Examples/Sites.csv", row.names = F)


# 6. Make Plots dataframe ----

# I want to make 10 random plots within the extent of the raster data for the NW
#   corner of Utah
(plots_ext_utm <- extent(elev_snow_stk))

# convert to latlong

# make a dataframe of random coordinates within this extent
set.seed(1)
(plots <- data.frame(Plots = LETTERS[1:10],
                     Latitude = runif(10, min = elev_snow_ext_wgs[3], 
                                      max = elev_snow_ext_wgs[4]),
                     Longitude = runif(10, min = elev_snow_ext_wgs[1], 
                                       max = elev_snow_ext_wgs[2])))
ggplot(plots) +
  geom_point(aes(x = Longitude, y = Latitude))
write.csv(plots, "Data/Exercises/Plots_location.csv", row.names = F)

# Make a dataframe with some simulated data for each plot
startdate <- ymd("2021-04-30") 

plot_data <- lapply(1:10, function(i){
  start <- startdate + period(i, units = "days")
  end <- start + period(4*7, units = "days")
  dates <- seq(start, end, by = 7)
  plot_data <- data.frame(Plots = LETTERS[i],
                          Species = c(rep("R. maritimus", 5), 
                                      rep("B. cernua", 5), 
                                      rep("S. acutus", 5)),
                          Date = rep(dates, 3),
                          AboveGroundBiomass = runif(15, 0, 100),
                          MeanHeight = runif(15, 0, 5),
                          PercentCover = runif(15, 0, 100))
  return(plot_data)
}) %>%
  bind_rows() %>%
  arrange(Date)
plot_data
write.csv(plot_data, "data/Exercises/Plots_data.csv", row.names = F)


# 7. Prep land ownership boundaries ----
# Land ownership Shapefile: https://gis.utah.gov/data/cadastre/land-ownership/
boundaries <- st_read(paste0(raw_dir, "UT_LandOwnership")) %>%
  st_transform(crs = 4326)
plot(st_geometry(boundaries))
head(boundaries)

# Remove unnecessary columns
boundaries <- boundaries %>%
  dplyr::select(-c(Edit_Date, Label_Fede, Label_Stat, GIS_Acres, COUNTY, STATE_LGD,
                   UT_LGD))

# Save
owner_dir <- "Data/Exercises/UT_land_ownership/"
if(!dir.exists(owner_dir)){dir.create(owner_dir, recursive = T)}
st_write(boundaries, owner_dir, "UT_land_ownership", driver = "ESRI Shapefile")


# 8. Prep landcover dataframe ----
state_summary <- read.csv(paste0(raw_dir, "Utah_Landcover/gaplf_state_summary.csv"))
head(state_summary)

# remove unncessary columns and rename columns to more intuitve names
state_summary <- state_summary %>%
  dplyr::select(-c(intStCode, intAcres, intSqMiles, numPercent)) %>%
  rename(StName = strStName) %>%
  rename(ClassCode = intClassCode) %>%
  rename(ClassName = strClassName) %>%
  rename(FormCode = strFormCode) %>%
  rename(FormName = strFormName) %>%
  rename(MacroCode = strMacroCode) %>%
  rename(MacroName = strMacroName) %>%
  rename(EcoSysCode = intEcoSysCode) %>%
  rename(EcoSysName = strEcoSysName)

# Use the landcover summary table from earlier
head(land_summary)
land_summary <- land_summary %>%
  # remove unnecessary columns
  dplyr::select(-c(Count, RED, GREEN, BLUE, NVCMES, LEVEL3)) %>%
  # rename to more intuitive names
  filter(!(Value == 0)) %>%
  rename(ClassCode = CL) %>%
  rename(ClassName = NVC_CLASS) %>%
  rename(SubClassCode = SC) %>%
  rename(SubClassName = NVC_SUBCL) %>%
  rename(FormCode = FRM) %>%
  rename(FormName = NVC_FORM) %>%
  rename(DivCode = DIV) %>%
  rename(DivName = NVC_DIV) %>%
  rename(MacroCode = MACRO_CD) %>%
  rename(MacroName = NVC_MACRO) %>%
  rename(GroupCode = GR) %>%
  rename(GroupName = NVC_GROUP) %>%
  rename(EcoSysName = ECOLSYS_LU)

# Join together
land_df <- left_join(state_summary, land_summary, 
                     by = c("ClassCode", "ClassName", "FormCode", "FormName", 
                            "MacroCode", "MacroName", "EcoSysName")) %>%
  relocate(Value, .before = ClassCode) %>%
  relocate(c(EcoSysCode, EcoSysName), .after = GroupName) %>%
  relocate(c(SubClassCode, SubClassName), .after = ClassName)
head(land_df)

# Save in the "Examples" folder
write.csv(land_summary, "Data/Examples/landcover_info.csv", row.names = F)


# ----------------------X
# ---- Visualization ----
# ----------------------X

# 9. Prep DEM for Yellowstone ----

ynp <- st_read("Data/Visualization/Yellowstone", quiet = T) %>%
  # be sure it's projected to WGS 84 (EPSG:4326)
  st_transform(4326)

ynp_animals <- read.csv("Data/yellowstone_animals.csv")
ynp_xlim <- c(min(ynp_animals$Longitude), max(ynp_animals$Longitude))
ynp_ylim <- c(min(ynp_animals$Latitude), max(ynp_animals$Latitude))

dem <- rast("Data/DEM.tif")
plot(dem)
plot(ynp$geometry, add = T)

(crop_ext <- ext(ynp_xlim[1] - 0.25,
                 ynp_xlim[2] + 0.25,
                 ynp_ylim[1] - 0.25,
                 ynp_ylim[2] + 0.25))
ynp_dem <- crop(dem, crop_ext)
ynp_dem

ynp_dem <- aggregate(ynp_dem, 10)

plot(ynp_dem)
plot(ynp$geometry, add = T)

writeRaster(ynp_dem, "Data/Visualization/Yellowstone_DEM.tif", overwrite = T)


# 10. Prep Yellowstone Animal Data and Ranges ----

library(amt)

ynp_animals <- read.csv("Data/Visualization/yellowstone_animals.csv")
unique(ynp_animals$Species)
ynp_animals <- ynp_animals %>%
  mutate(Species = str_to_title(Species))
unique(ynp_animals$Species)
write.csv(ynp_animals, "Data/yellowstone_animals.csv", row.names = F)

ynp_animals <- split(ynp_animals, ynp_animals$Species)


range <- lapply(ynp_animals, function(df){
  trk <- make_track(df, .x = "Longitude", .y = "Latitude", crs = 4326,
                    all_cols = T)
  
  kde <- hr_kde(trk)
  
  kde_ud_sf <- as.polygons(kde$ud, round = T, digits = 1) %>%
    st_as_sf() %>%
    filter(lyr.1 >= 0.1) %>%
    rename(Use = lyr.1) %>%
    # normalize from 0 to 1
    mutate(Species = unique(trk$Species),
           Use = (Use - min(Use)) / (max(Use) - min(Use)))
  
  return(kde_ud_sf)
}) %>%
  bind_rows()
head(range)
summary(range)

ggplot(range) +
  facet_wrap(~Species) +
  geom_sf(aes(fill = Use), col = NA) +
  scale_fill_viridis_c()

st_write(range, "Data/Visualization/Species_Range", layer = "Species_Range",
         driver = "ESRI Shapefile")


# 11. Prep 4 Corners Landcover raster ----

basin <- rast("Data/Visualization/Landcover/Great_Basin.tif")
desert <- rast("Data/Visualization/Landcover/Southeast_Desert.tif")
rockies <- rast("Data/Visualization/Landcover/Southern_Rockies.tif")

basin
basin <- aggregate(basin, fact = 10, fun = "median")
basin
gc()

desert
desert <- aggregate(desert, fact = 10, fun = "median")
desert
gc()

rockies
rockies <- aggregate(rockies, fact = 10, fun = "median")
rockies
gc()

writeRaster(basin, "Data/Visualization/Landcover/Great_Basin.tif", overwrite = T)
writeRaster(desert, "Data/Visualization/Landcover/Southeast_Desert.tif", overwrite = T)
writeRaster(rockies, "Data/Visualization/Landcover/Southern_Rockies.tif", overwrite = T)

# Prep landcover metadata
land_info <- read.csv("Data/Visualization/Landcover/Landcover_Attributes.csv") 
land_info <- land_info %>%
  select(-c(Count, RED, GREEN, BLUE, CL, FRM, DIV, MACRO_CD, GR, LEVEL3, NVCMES))
head(land_info)
write.csv(land_info, "Data/Visualization/Landcover/Landcover_Attributes.csv", row.names = F)




