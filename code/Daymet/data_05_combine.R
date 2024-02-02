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


# Read in Southern Rockies L4 ecoregion shapefiles
shps <- st_read("ecoregions/us_eco_l4_no_st.shp")
shps <- dplyr::filter(shps, US_L3NAME == "Southern Rockies")


collect_info <- function(fname){
  temp_df <- read.csv(fname)
  year <- temp_df$year[1]
  temp_df <- dplyr::select(temp_df, -regionID, -year)
  cvar <- colnames(temp_df)
  colnames(temp_df) <- paste0(cvar, as.character(year))
  return(temp_df)
}


for (i in 1:N_vars){
  cvar <- cvars[i]
  fnames <- list.files(path = paste0(data_dir, "/l4_data"), pattern = glob2rx(paste0("*", cvar, "*.csv")), full.names = TRUE)
  col_list <- lapply(fnames, collect_info)
  col_df <- do.call(cbind.data.frame, col_list)
  shp_append <- cbind.data.frame(shps, col_df)
  fname_new <- paste0("shps/", cvar, ".shp")
  st_write(shp_append, fname_new)
}
