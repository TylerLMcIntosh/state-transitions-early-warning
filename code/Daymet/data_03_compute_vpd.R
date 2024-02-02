#!/usr/bin/env Rscript

library(dplyr)
library(stringr)
library(raster)

data_dir <- "/projects/nihe1301/southern_rockies"
daymet_dir <- "/scratch/alpine/nihe1301/daymet_cropped"

setwd(daymet_dir)

fname_df <- read.csv(paste0(data_dir, "/fname_df.csv"))

get_vpd <- function(vp, Tval){
  # Compute the saturation vapor pressure using the temperature (deg. C)
  vps <- 6.1078*exp((17.269*Tval)/(237.3+Tval))
  vpd <- vps - vp
  return(vpd)
}

vp_df <- dplyr::filter(fname_df, cvar == "vp")

N <- nrow(vp_df)

# For each vapor pressure entry...

fnames_revised <- list()
count <- 1

for (i in 1:N){
  # Collect the file names of the corresponding VPD, Tmin, and Tmax files
  fname_vp <- vp_df[i,]$fname
  fname_tmin <- dplyr::filter(fname_df, cvar == "tmin", year == vp_df$year[i])$fname
  fname_tmax <- dplyr::filter(fname_df, cvar == "tmax", year == vp_df$year[i])$fname
  
  # Initialize a list for storing the monthly calculations of Tmin and Tmax
  vpdmin <- list()
  vpdmax <- list()
  # There are only entries for June, July, August, and September
  for (j in 1:4){
    # Read in month j
    tmin <- raster(fname_tmin, band = j)
    tmax <- raster(fname_tmax, band = j)
    vp <- raster(fname_vp, band = j)
    
    # Calculate the VPD
    vpdmin[[j]] <- get_vpd(vp, tmin)
    vpdmax[[j]] <- get_vpd(vp, tmax)
  }
  # Stack the months together
  vpdmin <- do.call(stack, vpdmin)
  vpdmax <- do.call(stack, vpdmax)
  
  # Create file names consistent with the other file structures
  fname_vpdmin <- paste0("daymet_v4_vpdmin_monavg_na_", vp_df$year[i], ".tif")
  fname_vpdmax <- paste0("daymet_v4_vpdmax_monavg_na_", vp_df$year[i], ".tif")
  
  # Add these two lines to the list
  fnames_revised[[i]] <- data.frame(fname_full = c(NA, NA),
                                         fname = c(fname_vpdmin, fname_vpdmax),
                                          cvar = c("vpdmin", "vpdmax"),
                                          year = c(vp_df[i,]$year, vp_df[i,]$year))

  # Save the summarized vpdmin and vpdmax
  med_vpdmin <- calc(vpdmin, median)
  med_vpdmax <- calc(vpdmax, median)

  writeRaster(med_vpdmin, paste0(data_dir, "/daymet/", fname_vpdmin), overwrite = TRUE)
  writeRaster(med_vpdmax, paste0(data_dir, "/daymet/", fname_vpdmax), overwrite = TRUE)

  rvpdmin <- terra::rast(vpdmin)
  rvpdmax <- terra::rast(vpdmax)
  
  # Write the VPD min and max raster data
  terra::writeRaster(rvpdmin, fname_vpdmin, overwrite = TRUE)
  terra::writeRaster(rvpdmax, fname_vpdmax, overwrite = TRUE)
  
}

# Bind the resulting file names together to add vpd min and vpd max 
fnames_revised <- do.call(rbind.data.frame, fnames_revised)

# Remove vapor pressure from the original file data frame
fname_df <- dplyr::filter(fname_df, cvar != "vp") %>% dplyr::select(-X)

# And replace vp with vpd min and vpd max
fnames_revised <- rbind.data.frame(fname_df, fnames_revised)

# Save revised data frame
write.csv(fnames_revised, paste0(data_dir, "/fname_df_revised.csv"))
