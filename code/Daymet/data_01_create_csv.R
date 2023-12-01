#!/usr/bin/env Rscript

# Input: downloaded Daymet 1-km climate tif files for North America from the ORNL DAAC  website for
# all available climate variables (tmin, tmax, swe, prcp, vp)

# Output: CSV file of full file names, short file names, climate variable, and data year

library(dplyr)
library(stringr)
library(raster)
library(sf)

# We stored the Daymet files in directory called Daymet under this directory
data_dir <- "/scratch/alpine/nihe1301"
out_dir <- "/projects/nihe1301/southern_rockies"

# Search the Earthdata orders recursively for all of the data products 
fname_full <- list.files(path = paste0(data_dir, "/orders"), pattern = glob2rx("*_na_*.tif"), recursive = TRUE, full.names = TRUE)

# Function to obtain the short file name from each full filename
get_fname <- function(long_fname){
  fname <- str_extract(long_fname, pattern = "daymet_v4_.+tif$")[1]
  return(fname)
}

# Collect the short file names from each long file name
fname <- lapply(fname_full, get_fname)

# Function to obtain the climate variable
get_cvar <- function(fname){
  cvar_plus <- str_extract(fname, pattern = "v4_[a-z]+_mon")[1]
  cvar <- gsub("v4_", "", cvar_plus)
  cvar <- gsub("_mon", "", cvar)
  return(cvar)
}

# Function to obtain the year
get_year <- function(fname){
  year <- str_extract(fname, pattern = "[0-9]{4}")[1] %>% as.numeric()
  return(year)
}

# Collect the year
year <- lapply(fname, get_year)
year <- do.call(c, year)

# Collect the climate variable
cvar <- lapply(fname, get_cvar)
cvar <- do.call(c, cvar)

fname <- do.call(c, fname)

# Create a data frame
fname_df <- data.frame(fname_full = fname_full, fname = fname, cvar = cvar, year = year)

# Save CSV file of file names to read in to next script
write.csv(fname_df, paste0(out_dir, "/fname_df.csv"))
