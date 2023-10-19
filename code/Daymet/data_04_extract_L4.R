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
library(exactextractr)

# We stored the Daymet files in directory called Daymet under this directory
data_dir <- "/scratch/alpine/nihe1301/"
out_dir <- "/projects/nihe1301/southern_rockies/"

# Location of region of interest shape files
shp_dir <- out_dir

fname_df <- read.csv(paste0(out_dir, "fname_df_revised.csv"))
fname_df <- fname_df[taskID,]

prj <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Read in Southern Rockies L3 ecoregion shapefiles
shps <- st_read(paste0(out_dir, "ecoregions/us_eco_l4_no_st.shp"))
shps <- dplyr::filter(shps, US_L3NAME == "Southern Rockies")

# Transform to CRS of Daymet data
shps <- st_transform(shps, crs = prj)
shps <- as(shps, Class = "Spatial")

# Read in the cropped Daymet data
r <- raster(paste0(out_dir, "daymet/", fname_df$fname))

cvar <- exact_extract(r, shps, 'median')

cvar_df <- data.frame(regionID = 1:length(cvar), year = fname_df$year, cvar = cvar)
colnames(cvar_df) <- c("regionID", "year", fname_df$cvar)

# Save to the out directory
fname_new <- paste0(out_dir, "l4_data/", fname_df$cvar, "_", fname_df$year, ".csv")
write.csv(cvar_df, fname_new)
