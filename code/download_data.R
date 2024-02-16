### ### ### ### ### ### ### ### ### ### ### ###

#Download data for the state transitions early warning signs project
#CU Boulder CIRES Earth Lab
#4/7/23


#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----

## Clean workspace ----
rm(list=ls()) #Ensure empty workspace if running from the beginning
here::here() #Check here location


## Set directories structure ----

# Set landfire directories & create if doesn't already exist
landfireDir <- here::here("data", "raw", "landfire")
if (!dir.exists(landfireDir)){
  dir.create(landfireDir)
}


### ### ### ### ### ### ### ### ### ### ### ###

# Landfire data download ----

#URLS to download
zipUrls <- c("https://www.landfire.gov/bulk/downloadfile.php?FNAME=US_220_mosaic-LF2020_BPS_220_CONUS.zip&TYPE=landfire",
          "https://landfire.gov/bulk/downloadfile.php?FNAME=US_220_mosaic-LF2020_EVT_220_CONUS.zip&TYPE=landfire")

#Filenames for each URL
zipFileNms <- c(here::here(landfireDir, "biophysical-settings.zip"),
                here::here(landfireDir, "existing-vegetation-type.zip"))

  
#Function to download
download <- function(url, destfile) {
  download.file(url = url,
                destfile = destfile,
                mode = "wb")
}

#Function to unzip
unzip_me <- function(destfile, exdir) {
  unzip(zipfile = destfile, exdir = exdir)
}
  
#Get all zips and csvs already downloaded
zips_dl <- list.files(path = here::here("data", "raw", "landfire"), pattern = ".zip$", full.names = TRUE)

#Download and unzip zips if don't already have them
options(timeout = max(3600, getOption("timeout")))

if(!all(zipFileNms %in% zips_dl)) {
  #Download zips from URLs
  mapply(FUN = download,
         url = zipUrls,
         destfile = zipFileNms)
  
  #Unzip files
  mapply(FUN = unzip_me,
         destfile = zipFileNms,
         exdir = gsub(pattern = ".zip$", replacement = "", x = zipFileNms))
  
} else {
  print("You already have all the zips")
}

