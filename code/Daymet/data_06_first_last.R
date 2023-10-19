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

yrs_first <- seq(1985, 1989)
yrs_last <- seq(2017, 2021)

# For each variable
for (i in 1:N_vars){
  cvar <- cvars[i]
  rcvar <- list()

  for (j in 1:length(yrs_first)){
    idx <- which(fname_df$year == yrs_first[j] & fname_df$cvar == cvar)
    fname <- fname_df[idx,]$fname
    rcvar[[j]] <- raster(paste0("daymet/", fname))
  }

  rstack <- do.call(stack, rcvar)
  rcalc <- calc(rstack, median)
  save_name <- paste0("compare/", cvar, "_", yrs_first[1], "_", yrs_first[length(yrs_first)], ".tif")
  writeRaster(rcalc, save_name, overwrite = TRUE)
  
  rcvar <- list()
  for (j in 1:length(yrs_first)){
    idx	<- which(fname_df$year == yrs_last[j] & fname_df$cvar == cvar)
    fname <- fname_df[idx,]$fname  
    rcvar[[j]] <- raster(paste0("daymet/", fname))
  }

  rstack <- do.call(stack, rcvar)
  rcalc	<- calc(rstack,	median)
  save_name <- paste0("compare/", cvar,	"_", as.character(yrs_last[1]), "_", as.character(yrs_last[length(yrs_last)]), ".tif")
  writeRaster(rcalc, save_name, overwrite = TRUE)
}
