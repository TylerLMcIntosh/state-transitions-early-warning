#!/usr/bin/env Rscript
taskID <- commandArgs(trailingOnly = TRUE)

# Check format
taskID <- as.numeric(taskID)
print(taskID)

# Input data: downloaded Daymet 1-km climate tif files for North America between 2008 and 2017 from the NASA's Earthdata website for
# all available climate variables (tmin, tmax, swe, prcp, vp)

library(dplyr)
library(stringr)
library(raster)
library(sf)

# We stored the Daymet files in directory called Daymet under this directory
data_dir <- "/scratch/alpine/nihe1301/"
out_dir <- "/projects/nihe1301/southern_rockies"
temp_dir <- "/scratch/alpine/nihe1301/daymet_cropped"

# Location of region of interest shape files
shp_dir <- out_dir

fname_df <- read.csv(paste0(out_dir, "/fname_df.csv"))
fname_df <- fname_df[taskID,]

prj <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Read in Southern Rockies L3 ecoregion shapefiles
shps <- st_read(paste0(out_dir, "/ecoregions/us_eco_l3.shp"))
shps <- dplyr::filter(shps, US_L3NAME == "Southern Rockies")

# Apply a 4 km buffer
shps <- st_buffer(shps, dist = 4000)

# Transform to CRS of Daymet data
shps <- st_transform(shps,  crs = prj)
shps <- as(shps, Class = "Spatial")

# Pull out the extents of the shapefiles
ext <- extent(shps)

r <- list()
# For each month in the raster file during the growing season (June-September)
count <- 1

for (j in 6:9){
  r_temp <- raster(fname_df$fname_full, band = j)
  r_temp <- crop(r_temp, ext)
  r[[count]] <- r_temp
  count <- count+1
}
# Stack the cropped raster data back together
rstack <- do.call(stack, r)

# Save stacked raster to temp storage directory; using an r-raster
# workaround to save raster stack from stack exchange using r-terra
rtemp <- terra::rast(rstack)
fname_new <- paste0(temp_dir, "/", fname_df$fname)
terra::writeRaster(rtemp, fname_new, overwrite = TRUE)

# Compute the growing season summaries here
if (fname_df$cvar == "prcp"){
  # Compute total growing season precipitation
  rsummary <- calc(rstack, sum, na.rm = FALSE)
} else{
  # and median growing season values for the other variables
  rsummary <- calc(rstack, median, na.rm = TRUE)
}

# Save to the project out directory
fname_new <- paste0(out_dir, "/daymet/", fname_df$fname)
writeRaster(rsummary, fname_new, overwrite = TRUE)

