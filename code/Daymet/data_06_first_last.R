#!/usr/bin/env Rscript

library(dplyr)
library(stringr)
library(raster)
library(sf)

data_dir <- "/projects/nihe1301/southern_rockies"
setwd(data_dir) 

fname_df <- read.csv(paste0(data_dir, "/fname_df_revised.csv"))

cvars <- unique(fname_df$cvar)

N_vars <- length(cvars)

# To obtain the first 5 and last 5, uncomment these lines and comment out the
# lines below
#yrs_start <- c(1985, 2017)
#yrs_end <- c(1989, 2021)

# To obtain five year increments
yrs_start <- seq(1985, 2015, by = 5)
yrs_end <- seq(1989, 2019, by = 5)

# For each variable
for (i in 1:N_vars){

  # Start with the first climate variable that we detected in the data set
  cvar <- cvars[i]
  
  for (j in 1:length(yrs_start)){
    # Initialize a list to store the raster data in
    rcvar <- list()
    
    # Create a sequence of the years from a given start point to a give end date,
    # increasing by ones
    yrs <- seq(yrs_start[j], yrs_end[j], by = 1)

    for (k in 1:length(yrs)){
      # Find the file name for each year of data
      idx <- which(fname_df$year == yrs[k] & fname_df$cvar == cvar)
      fname <- fname_df[idx,]$fname
      # Read it in and add it to the list
      rcvar[[k]] <- raster(paste0("daymet/", fname))
    }
    # Stack the years of raster data together
    rstack <- do.call(stack, rcvar)
    
    # Summarize by taking the median
    rcalc <- calc(rstack, median)

    # Create a file name that includes the climate variable, the year start date, and year end data
    # for the summary
    save_name <- paste0("compare/", cvar, "_", yrs_start[j], "_", yrs_end[j], ".tif")
    writeRaster(rcalc, save_name, overwrite = TRUE)
  
  }
}
