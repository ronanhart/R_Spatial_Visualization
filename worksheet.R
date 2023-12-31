# ==============================================X
# ----- Ecology Center: R Spatial Workshop -----X
# ----------------- 2022-10-26 -----------------X
# ==============================================X
# ----------------- Worksheet ------------------X
# ==============================================X

# This "worksheet" is designed to help you follow along and try out the code from
# the R Spatial Workshop. There are a few practice problems to help you practice the 
# concepts you learned from the workshop.

# ----------------------X
# -------- INTRO --------
# ----------------------X

# ----------------------X
# ---- Load packages ----
# ----------------------X
# which packages do you need to load?


# ----------------X
# ---- Vectors ----
# ----------------X
# ...Loading a vector ----
# How do you load in the "Sites" csv dataframe? 



# How do you convert this to a spatial object? 



# How do you check the feature's information and attributes?



# How do you load in a shapefile? 



# ...Projecting a vector ----
# How do you project these two features to NAD83 UTM12N? 



# ...Saving a vector ----
# How do you save these vectors as shapefiles?



# ...Your Turn ----
# The files for these are in the "Data/Exercises" folder

# ... ...1) Convert the "Plots_data.csv" data frame into a spatial object ----



# ... ...2) Load in the "UT_land_management" shapefile -----



# ... ...3) Project these features ----
# First project to NAD83 UTM12N. 
# How would you project them to WGS84 UTM12N?



# ... ...4) Save these features ----
# First save them as shapefiles. Try creating their own folder. 
# Can you save them as another format?



# ... ...5) Examine the information and attributes of these features ----



# ----------------------------------------------------------------------------#

# ----------------X
# ---- Rasters ----
# ----------------X
# ...Loading a raster ----
# How do you load in the "elevation" raster? 



# How do you examine the information about this raster? 



# How do you load in the raster stack "snow_20190223"?



# ...Creating a raster ----
# How do you create a blank raster?



# How do you fill this raster with values?



# ...Projecting a vector ----
# How do you project a raster?



# How do you make sure two rasters are projected the same so that they can stack?



# ...Saving a raster ----
# How do you save these rasters?



# ...Your Turn ----
# The files for these are in the "Data/Exercises" folder

# ... ...1) Load the "Elevation" raster ----



# ... ...2) Load the "Snow" raster stack -----



# ... ...3) Project these rasters so that they can be stacked together ----



## ... ...3a) How do you change the name of the raster layers in this stack? ----



# ... ...4) Save this raster stack ----



# ... ...5) Examine the information of this raster stack ----



# ----------------------------------------------------------------------------#

# ---------------------------------X
# -------- SPATIAL ANALYSIS --------
# ---------------------------------X

# How do you select specific attributes from a spatial object? ----



# How do you select features based on location? ----



# How do you join attributes from two features together? ----



# How do you extract raster values? ----



# How do you find the distance between each site and a major road? ----



# How do you derive slope, aspect, and roughness from a DEM? ----



# How do you re-classify a raster? ----



# How can you compute raster cell algebra? ----



# How do crop a vector? ----



# How do you crop a raster? ----



# How do you make a buffer around a feature? ----



# IF YOU ARE FAMILIAR WITH LOOPS
# Are you able to put any of these processes within a loop? ----



# ...Your Turn ----
# The files for these are in the "Data/Exercises" folder

# ... ...1a) Select boundaries that are managed by either BLM or USFS. ----
# ... ...1b) Select roads that have a speed limit 70mph or higher ----



# ... ...2) Select management boundaries that a site is located in -----



# ... ...3) Crop the boundary shapefile to the extent of the elevation/snow raster stack ----



# ... ...4) Extract the elevation and snow data for each plot ----



## ... ...4a) Join the extracted values to the plot's data frame ----



# ... ...5) Find the area of National Park land ----
# Hint: first find which column in the management boundaries feature has the 
#       attribute indicating it's a National Park



# ... ...6) Find the minimum snow depth value ----



# ... ...7) Derive the flow direction, TPI, and TRI of the elevation raster ----
# Hint: type ?terrain in the console if you need help



# ... ...7a) Try deriving slope and aspect in degrees (instead of radians)



# ... ...7b) Compute Northness and Eastness from the aspect raster ----
#        who's units are in degrees
# Hint: How do you convert radians to degrees and vice versa? You will have to 
#       do this step first



