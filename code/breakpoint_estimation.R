#This script performs breakpoint analysis on time series of LCMAP data aggregated at the EPA ecoregion level iv. Chooses model that minimizes AIC
#It requires time series of land cover per ecoregion
#It returns a data frame with years when statistically significant shifts in the time series took place 
library(tidyverse)
library(raster)
library(foreach)
library(sf)
library(doParallel)
library(EnvCpt)
file <- #enter name of file with LCMAP data
df <- read.csv(file)
df <- filter(df, LC %in% 4)#only keep rows with tree data

ggplot(filter(df)) +
  geom_line(aes(yr, area_km2)) +
  facet_wrap(.~eco, scales = 'free')#Plot time series

#Helper to estimate breakpoints
env <- function(data, area, yr, ecoregion, minseglen, mdl){
  area <- data %>% 
    dplyr::select(all_of(area))
  yr <- data %>% 
    dplyr::select(all_of(yr))
  ecoregion <- data %>% 
    dplyr::select(all_of(ecoregion))
  if(mdl %in% 'best'){
    fit_envcpt <-  envcpt(area[,1], minseglen = minseglen) #fit all models
    model <- which.min(AIC(fit_envcpt))#choose the model that minimizes AIC
    bkp <- fit_envcpt[model+1]
    df <- data.frame(ecoregion = ecoregion[1,1],
                     bkp_yr = if(names(model) %in% c('mean', 'trend')) {
                       NA
                       }else{
                         yr[,1][bkp[[1]]@cpts]
    },
                     model = names(model))
  }else{
    fit_envcpt <-  envcpt(area[,1], minseglen = minseglen, model = mdl)
    df <- data.frame(ecoregion = ecoregion[1,1],
                     bkp_yr = yr[,1][eval(parse(text = paste0('fit_envcpt$', mdl)))@cpts],
                     model = mdl)
    }
  return(df)
}#data = your data frame; area = name of column with area data; yr = name of column with years; ecoregion = name of column with ecoregion names; minseglen = minimum segment length
#mdl = any model from the envcpt function. choose 'best' if you want to obtain breakpoints from the model that minimizes the AIC

eco_split <- split(df, df$eco)#split data frame by ecoregion

registerDoParallel(detectCores()-1)#register cores to run analysis in parallel
m <- foreach(i = 1:length(eco_split),.combine=rbind)%dopar%{
  env(eco_split[[i]], area = 'area_km2', yr = 'yr', ecoregion = 'eco', minseglen = 7, mdl = 'best')
}#estimate breakpoints with 'best' model

ggplot(m) +
  geom_point(aes(bkp_yr, factor(ecoregion))) +
  xlab('Year of change') +
  ylab('Ecoregion') +
  ggtitle('Best model') +
  theme_bw()

mm <- foreach(i = 1:length(eco_split),.combine=rbind)%dopar%{
  env(eco_split[[i]], area = 'area_km2', yr = 'yr', ecoregion = 'eco', minseglen = 7, mdl = 'meancpt')
}#estimate breakpoints with 'meancpt' model


ggplot(mm) +
  geom_point(aes(bkp_yr, factor(ecoregion))) +
  xlab('Year of change') +
  ylab('Ecoregion') +
  ggtitle('Meancpt') +
  theme_bw()

df2 <- inner_join(df, m, by = c('yr' = 'bkp_yr', 'eco' = 'ecoregion'))
  
ggplot() +
  geom_line(aes(yr, area_km2), data = filter(df, area_km2>1)) +
  geom_point(aes(yr, area_km2), col = 'red', data = filter(df2, area_km2>1)) +
  facet_wrap(.~eco, scales = 'free') +
  theme_bw() +
  xlab('Year') +
  ylab(expression('Area'~'(km'^2*')'))#plot time series & breakpoints of ecoregions with more than 1km2 of tree cover 
