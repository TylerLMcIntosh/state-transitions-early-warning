### ### ### ### ### ### ### ### ### ### ### ###

#Explore forest type relationships with transitions
#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#4/7/23



#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(tidyverse) #Tidyverse!
library(here)
library(sf)
library(terra)
library(mapview)

## Clean workspace ----
rm(list=ls()) #Ensure empty workspace if running from the beginning
here::here() #Check here location


## Set directories structure ----


# set data directories
landfireDir <- here::here("data", "raw", "landfire")
conversionDir <- here::here("data", "shared", "conversion-rasters")

## Load data ----

evtFile <- list.files(here::here(landfireDir, "existing-vegetation-type"),
                       pattern = ".tif$",
                       recursive = TRUE,
                       full.names = TRUE)
evtCsv <- list.files(here::here(landfireDir, "existing-vegetation-type"),
                     pattern = ".csv$",
                     recursive = TRUE,
                     full.names = TRUE)
bpsFile <- list.files(here::here(landfireDir, "biophysical-settings"),
                      pattern = ".tif$",
                      recursive = TRUE,
                      full.names = TRUE)
bpsCsv <- list.files(here::here(landfireDir, "biophysical-settings"),
                     pattern = ".csv$",
                     recursive = TRUE,
                     full.names = TRUE)

#years since 1986 that the transformation occurred. Derived by Nayani
conversionNonFire <- terra::rast(here::here(conversionDir, "non_fire_new1.tif"))


#Disturbance stack data
disturbanceStack <- terra::rast(here('data', 'shared', "simple_disturbance_stack_southern_rockies_EPSG32613.tif"))
datatype(disturbanceStack)


### ### ### ### ### ### ### ### ### ### ### ###

# SCRIPTED ANALYSIS ----

terra::plot(conversionNonFire)
terra::freq(conversionNonFire)

#Convert raster to points
conversionNonFirePts <- conversionNonFire %>%
  terra::as.points() %>%
  sf::st_as_sf() %>%
  mutate(conversionYr = non_fire_new1 + 1986)
  
# t <- ext(conversionNonFire)  %>% as.polygons(crs = crs(conversionNonFire)) %>% sf::st_as_sf() 
# mapview(t)


### ### ### ### ### ### ### ### ### ### ### ###

# Vegetation type data ----

# evt <- terra::rast(evtFile)
# evt$EVT_NAME
# evtD <- read.csv(evtCsv)
bps <- terra::rast(bpsFile)
bpsD <- read.csv(bpsCsv)
levels(bps)
activeCat(bps) <- "BPS_CODE"

#Create new vegetation groups with certain veg types of interest highlighted
#output of case_when is based on the FIRST statement that is true; i.e. order of statements is priority order
newBpsD <- bpsD %>%
  mutate(NEWGROUPVEG = case_when(grepl("aspen", BPS_NAME, ignore.case = TRUE) & grepl("conifer", BPS_NAME, ignore.case = TRUE) ~ "AspenConifer",
                          grepl("aspen", BPS_NAME, ignore.case = TRUE) ~ "Aspen",
                          grepl("ponderosa", BPS_NAME, ignore.case = TRUE) ~ "Ponderosa",
                          grepl("douglas-fir", BPS_NAME, ignore.case = TRUE) ~ "Douglas-fir",
                          grepl("mixed conifer", BPS_NAME, ignore.case = TRUE) ~ "MixedConifer",
                          grepl("pinyon|juniper", BPS_NAME, ignore.case = TRUE) ~ "PinyonOrJuniper",
                          grepl("lodgepole", BPS_NAME, ignore.case = TRUE) ~ "Lodgepole"
                          )) %>%
  mutate(NEWGROUPVEG = case_when(is.na(NEWGROUPVEG) ~ GROUPVEG,
                                 TRUE ~ NEWGROUPVEG))



#Create new codes & join to data
newCodes <- unique(newBpsD$NEWGROUPVEG) %>%
  cbind(seq(1:length(.))) %>%
  `colnames<-`(c("NEWGROUPVEG", "NEWCODE"))  %>% #way to set column names when piping, yay!
  as.data.frame()
newBpsD <- newBpsD %>% left_join(newCodes, by=join_by(NEWGROUPVEG))

# #Output updated BPS data
# write.csv(newBpsD, here::here("data", "derived", "newBPScodes.csv"))


# Join to conversion points
conversionBPSdats <- bps %>%
  terra::extract(conversionNonFirePts) %>%
  mutate(BPS_CODE = as.integer(as.character(BPS_CODE))) %>%
  left_join(newBpsD, multiple = "any") %>%
  select(NEWGROUPVEG, NEWCODE, BPS_NAME, GROUPVEG)

conversionNonFirePts <- conversionNonFirePts %>% cbind(conversionBPSdats)
conversionNonFirePts <- conversionNonFirePts %>% filter(NEWGROUPVEG != "Sparse" & 
                                                          NEWGROUPVEG != "Open Water" &
                                                          NEWGROUPVEG != "Grassland" &
                                                          NEWGROUPVEG != "Barren-Rock/Sand/Clay")



ggplot(conversionNonFirePts) +
  geom_bar(aes(x = NEWGROUPVEG)) +
  labs(title = "Non-Fire Converted Pixels by Landfire BPS (test tile)", x = "Vegetation type")


ggplot(data = conversionNonFirePts) +
  geom_bar(aes(x = conversionYr)) +
  facet_wrap(~NEWGROUPVEG, scales = "free") +
  labs(title = "Non-Fire Converted Pixels by Landfire BPS and year of conversion (test tile)",
       x = "Year of conversion",
       y = "Count of pixels converted (note free-ranging scales)")




### ### ### ### ### ### ### ### ### ### ### ###

# Disturbance stack data ----

#Get subset of sample tile that converted in 1999 or later
disturbanceDatsConversion <- conversionNonFirePts %>% 
  filter(conversionYr >= 1999 & conversionYr <= 2020)

#Extract simplified disturbance stack values
distD <- disturbanceStack %>%
  terra::extract(disturbanceDatsConversion %>% sf::st_transform(crs(disturbanceStack))) %>%
  select(-"ID")

#Extract specific disturbance binaries
fire <- distD %>%
  mutate(across(1:ncol(distD), ~case_when(.==0 ~ 0,
                                          .==1 ~ 1,
                                          .==2 ~ 0,
                                          .==3 ~ 0,
                                          .==4 ~ 1,
                                          .==5 ~ 0))) %>%
  `names<-`(paste('fire_', seq(1999,2020), sep=""))

hotDrought <- distD %>%
  mutate(across(1:ncol(distD), ~case_when(.==0 ~ 0,
                                          .==1 ~ 0,
                                          .==2 ~ 0,
                                          .==3 ~ 1,
                                          .==4 ~ 1,
                                          .==5 ~ 0))) %>%
  `names<-`(paste('hotdrought_', seq(1999,2020), sep=""))

insect <- distD %>%
  mutate(across(1:ncol(distD), ~case_when(.==0 ~ 0,
                                          .==1 ~ 0,
                                          .==2 ~ 1,
                                          .==3 ~ 0,
                                          .==4 ~ 0,
                                          .==5 ~ 1))) %>%
  `names<-`(paste('insectdisease_', seq(1999,2020), sep=""))

#Bind together
disturbanceDatsConversion <- disturbanceDatsConversion %>% cbind(fire, hotDrought, insect)

#Remove those pixels outside of SR-masked DStack
disturbanceDatsConversion <- disturbanceDatsConversion %>% filter(!is.na(fire_1999))



fireYearOf <- c()
hotDroughtYearOf <- c()
insectDiseaseYearOf <- c()

for (i in 1:nrow(disturbanceDatsConversion)) {
  yr <- disturbanceDatsConversion[i,]$conversionYr
  fnm <- paste0("fire_", yr, sep="")
  #print(fnm)
  fire <- disturbanceDatsConversion %>% as.data.frame() %>% .[,fnm]
  fire <- fire[i]
  #print(fire)
  fireYearOf <- fireYearOf %>% append(fire)
  
  hnm <- paste0("hotdrought_", yr, sep="")
  #print(hnm)
  hot <- disturbanceDatsConversion %>% as.data.frame() %>% .[,hnm]
  hot <- hot[i]
  #print(fire)
  hotDroughtYearOf <- hotDroughtYearOf %>% append(hot)
  
  inm <- paste0("insectdisease_", yr, sep="")
  #print(inm)
  insect <- disturbanceDatsConversion %>% as.data.frame() %>% .[,inm]
  insect <- insect[i]
  #print(fire)
  insectDiseaseYearOf <- insectDiseaseYearOf %>% append(insect)
}

t <- cbind(disturbanceDatsConversion, fireYearOf, hotDroughtYearOf, insectDiseaseYearOf)

ggplot(data = t) +
  geom_bar(aes(x = insectDiseaseYearOf)) +
  facet_wrap(~NEWGROUPVEG, scales = "free") +
  labs(title = "Porportion of vegetation types that transitioned AND \n saw insect/disease disturbance recorded in the year of transition",
       x = "Binary insect/disease recorded in year of transition (0 = no, 1 = yes)")
ggplot(data = t) +
  geom_bar(aes(x = hotDroughtYearOf)) +
  facet_wrap(~NEWGROUPVEG, scales = "free") +
  labs(title = "Porportion of vegetation types that transitioned AND \n saw hotter drought disturbance recorded in the year of transition",
       x = "Binary hotter drought recorded in year of transition (0 = no, 1 = yes)")

tt <- t %>% select(conversionYr, NEWGROUPVEG, fireYearOf, hotDroughtYearOf, insectDiseaseYearOf)
ttt <- tt %>% group_by(NEWGROUPVEG, conversionYr) %>% summarise(sumFire = sum(fireYearOf),
                                                                sumInsect = sum(insectDiseaseYearOf),
                                                                sumDrought = sum(hotDroughtYearOf),
                                                                n = n(),
                                                                percFire = (sum(fireYearOf) / n()) * 100,
                                                                percInsect = (sum(insectDiseaseYearOf) / n()) * 100,
                                                                percDrought = (sum(hotDroughtYearOf) / n()) * 100
                                                                )
tttt <- tt %>% group_by(conversionYr) %>% summarise(sumFire = sum(fireYearOf),
                                                    sumInsect = sum(insectDiseaseYearOf),
                                                    sumDrought = sum(hotDroughtYearOf),
                                                    n = n(),
                                                    percFire = (sum(fireYearOf) / n()) * 100,
                                                    percInsect = (sum(insectDiseaseYearOf) / n()) * 100,
                                                    percDrought = (sum(hotDroughtYearOf) / n()) * 100)
ttttt <- tt %>% summarise(sumFire = sum(fireYearOf),
                                                    sumInsect = sum(insectDiseaseYearOf),
                                                    sumDrought = sum(hotDroughtYearOf),
                                                    n = n(),
                                                    percFire = (sum(fireYearOf) / n()) * 100,
                                                    percInsect = (sum(insectDiseaseYearOf) / n()) * 100,
                                                    percDrought = (sum(hotDroughtYearOf) / n()) * 100)

ggplot(ttt) +
  geom_line(aes(x = conversionYr, y = sumInsect)) +
  facet_wrap(~NEWGROUPVEG, scales = "free")
ggplot(ttt) +
  geom_line(aes(x = conversionYr, y = sumDrought)) +
  facet_wrap(~NEWGROUPVEG, scales = "free")
ggplot(ttt) +
  geom_line(aes(x = conversionYr, y = percInsect)) +
  facet_wrap(~NEWGROUPVEG, scales = "free") +
  labs(title = "Percentage of vegetation types that transitioned AND \n saw insect/disease disturbance recorded in the year of transition",
       x = "Year of transition",
       y = "Percentage of converted pixels with \n insect/disease disturbance in year of transition")
ggplot(ttt) +
  geom_line(aes(x = conversionYr, y = percDrought)) +
  facet_wrap(~NEWGROUPVEG, scales = "free") +
  labs(title = "Percentage of vegetation types that transitioned AND \n saw hotter drought disturbance recorded in the year of transition",
       x = "Year of transition",
       y = "Percentage of converted pixels with \n hotter drought disturbance in year of transition")

# None of this works & not sure why... ask someone about the easy way to do this
# t <- disturbanceDatsConversion %>%
#   mutate(fireYearOf = !!as.name(paste0("fire_", disturbanceDatsConversion$conversionYr, sep = "")))
# tt <- t %>% filter(fireYearOf == 1)
# 
# 
# rightCols <- paste0("forest.disturbance.s.rockies.", as.character(disturbanceDatsConversion$conversionYr), sep = "")
# t <- disturbanceDatsConversion %>% cbind(rightCols)
# t <- t %>% mutate(dCodeConvertYr = !!as.name(rightCols))
# 
# t <- disturbanceDatsConversion %>%
#   mutate(dCodeConvertYr = disturbanceDatsConversion[[conversionYr - 1999 + 3]])
