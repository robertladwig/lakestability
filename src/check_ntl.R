# Package ID: knb-lter-ntl.29.35 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/35/03e232a1b362900e0f059859abe8eb97" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "sampledate",     
                 "depth",     
                 "rep",     
                 "sta",     
                 "event",     
                 "wtemp",     
                 "o2",     
                 "o2sat",     
                 "deck",     
                 "light",     
                 "frlight",     
                 "flagdepth",     
                 "flagwtemp",     
                 "flago2",     
                 "flago2sat",     
                 "flagdeck",     
                 "flaglight",     
                 "flagfrlight"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$event)!="factor") dt1$event<- as.factor(dt1$event)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]               
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$o2)=="factor") dt1$o2 <-as.numeric(levels(dt1$o2))[as.integer(dt1$o2) ]               
if (class(dt1$o2)=="character") dt1$o2 <-as.numeric(dt1$o2)
if (class(dt1$o2sat)=="factor") dt1$o2sat <-as.numeric(levels(dt1$o2sat))[as.integer(dt1$o2sat) ]               
if (class(dt1$o2sat)=="character") dt1$o2sat <-as.numeric(dt1$o2sat)
if (class(dt1$deck)=="factor") dt1$deck <-as.numeric(levels(dt1$deck))[as.integer(dt1$deck) ]               
if (class(dt1$deck)=="character") dt1$deck <-as.numeric(dt1$deck)
if (class(dt1$light)=="factor") dt1$light <-as.numeric(levels(dt1$light))[as.integer(dt1$light) ]               
if (class(dt1$light)=="character") dt1$light <-as.numeric(dt1$light)
if (class(dt1$frlight)=="factor") dt1$frlight <-as.numeric(levels(dt1$frlight))[as.integer(dt1$frlight) ]               
if (class(dt1$frlight)=="character") dt1$frlight <-as.numeric(dt1$frlight)
if (class(dt1$flagdepth)!="factor") dt1$flagdepth<- as.factor(dt1$flagdepth)
if (class(dt1$flagwtemp)!="factor") dt1$flagwtemp<- as.factor(dt1$flagwtemp)
if (class(dt1$flago2)!="factor") dt1$flago2<- as.factor(dt1$flago2)
if (class(dt1$flago2sat)!="factor") dt1$flago2sat<- as.factor(dt1$flago2sat)
if (class(dt1$flagdeck)!="factor") dt1$flagdeck<- as.factor(dt1$flagdeck)
if (class(dt1$flaglight)!="factor") dt1$flaglight<- as.factor(dt1$flaglight)
if (class(dt1$flagfrlight)!="factor") dt1$flagfrlight<- as.factor(dt1$flagfrlight)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(year4)
summary(sampledate)
summary(depth)
summary(rep)
summary(sta)
summary(event)
summary(wtemp)
summary(o2)
summary(o2sat)
summary(deck)
summary(light)
summary(frlight)
summary(flagdepth)
summary(flagwtemp)
summary(flago2)
summary(flago2sat)
summary(flagdeck)
summary(flaglight)
summary(flagfrlight) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$rep)) 
summary(as.factor(dt1$sta)) 
summary(as.factor(dt1$event)) 
summary(as.factor(dt1$flagdepth)) 
summary(as.factor(dt1$flagwtemp)) 
summary(as.factor(dt1$flago2)) 
summary(as.factor(dt1$flago2sat)) 
summary(as.factor(dt1$flagdeck)) 
summary(as.factor(dt1$flaglight)) 
summary(as.factor(dt1$flagfrlight))
detach(dt1)               




schmidt.stability_idso = function(wtr, depths, bthA, bthD, sal = 0){
  orig_depths = depths
  if(length(wtr) != length(depths)){
    stop('water temperature array must be the same length as the depth array')
  }
  #having some weird issues with wtr and sal lengths, trying to fix with this
  if(length(sal) == 1){
    sal = rep(sal, length(wtr))
  }
  #Constants
  g = 9.81
  dz = 0.1
  if(min(bthD) < 0){
    useI = bthD >= 0
    if(any(bthD == 0)){
      depT = bthD[useI]
    }else{
      depT = c(0, bthD[useI])
    }
    bthA = stats::approx(bthD, bthA, depT)$y
    bthD = depT
  }
  numD = length(wtr)
  if(max(bthD) > depths[numD]){
    wtr[numD+1] = wtr[numD]
    sal[numD+1] = sal[numD]
    depths[numD+1] = max(bthD)
  }else if(max(bthD) < depths[numD]) {
    bthD = c(bthD, depths[numD])
    bthA = c(bthA, 0)
  }
  if(min(bthD) < depths[1]) {
    wtr = c(wtr[1], wtr)
    sal = c(sal[1], sal)
    depths = c(min(bthD), depths)
  }
  Zo = min(depths)
  Io = which.min(depths)
  Ao = bthA[Io]
  if(Ao == 0){
    stop('Surface area cannot be zero, check *.bth file')
  }
  #Calculate water density
  rhoL = water.density(wtr, sal)
  #The approx (interp1 in matlab) just does linear interpolation
  layerD = seq(min(depths), max(depths), by=dz)
  layerP = stats::approx(depths, rhoL, layerD)$y
  layerA = stats::approx(bthD, bthA, layerD)$y
  mean_density = ((layerP %*% layerA) * dz) / sum(layerA * dz)
  # mean_density_depth <- (((layerP *layerA) %*% layerD) * dz) / sum((layerP %*% layerA) * dz)
  # mean_density <- mean(layerP)
  if (length(unique(layerP)) ==1){
    mean_density_depth = NA
  } else{
    mean_density_depth <- approx(layerP, layerD, mean_density)$y
  }

  Zcv <- layerD %*% layerA / sum(layerA)
  Zg <- (layerP %*% (layerD * layerA)) / sum(layerA %*% layerP)
  St <- (layerP - as.vector(mean_density)) %*% ((layerD - as.vector(mean_density_depth)) * layerA) * dz * g / Ao
  
  
  
  return(data.frame('St' = St, 'rho' = mean_density, "z_v" = Zcv, "z_g" = Zg))
}

library(tidyverse)
library(glmtools)
library(lubridate)
library(rLakeAnalyzer)
lakes_names = data.frame('lakeid' = c("AL", "BM", "CR", "FI", "ME", "MO", "SP", "TR"),
                         "lake" = c("Allequash", "BigMuskellunge", "Crystal", "Fish", "Mendota", "Monona", "Sparkling", "Trout"),
                         'mean_depth' = c(2.9, 7.5, 10.4, 6.6, 12.8, 8.2, 10.9, 14.6))

results = data.frame('lake' = NULL,
                     'St' = NULL,
                     'mean_rho' = NULL,
                     'Zg' = NULL,
                     'Zv' = NULL,
                     'mean_depth' = NULL,
                     'surface_temp' = NULL,
                     'bottom_temp' = NULL, 
                     'therm_dep' = NULL)
for (id in lakes_names$lakeid){
  
  df = dt1 %>%
    filter(lakeid == id)
  
  depth_raw = get_nml_value(glm_nml = read_nml(paste0("ntl/", lakes_names$lake[match(id, lakes_names$lakeid)],'/config.nml')), arg_name = "H")
  area_raw = get_nml_value(glm_nml = read_nml(paste0("ntl/", lakes_names$lake[match(id, lakes_names$lakeid)],'/config.nml')), arg_name = "A")
  
  dz = 0.1
  
  depth = seq(min(depth_raw), max(depth_raw), dz)
  area = approx(x = depth_raw, y = area_raw, xout = depth)$y
  
  depth = max(depth) - depth
  area = rev(area)

  for (time in unique(df$sampledate)){
    
    dat = df %>%
      filter(sampledate == time)
    
    if (all(is.na(dat$wtemp))){
      next 
    }
    res = schmidt.stability_idso(wtr = dat$wtemp, depths = dat$depth, bthA = area, bthD = depth)
    
    bf = center.buoyancy(wtr = dat$wtemp, depths = dat$depth)
    
    results = rbind(results, data.frame('lake' = id,
                                        'Datetime' = mean(dat$sampledate),
                                        'St' = res$St,
                                        'mean_rho' = res$rho,
                                        'Zg' = res$z_g,
                                        'Zv' = res$z_v,
                                        'mean_depth' = lakes_names$mean_depth[match(id, lakes_names$lakeid)],
                                        'surface_temp' = dat$wtemp[which(dat$depth == min(dat$depth))],
                                        'bottom_temp' = dat$wtemp[which(dat$depth == max(dat$depth))],
                                        'therm_dep' = bf))
  }
  
}

ggplot(results) +
  geom_point(aes((Zg- Zv) * mean_rho * mean_depth * 9.81, St)) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(Datetime, St, col = "SchmidtStability")) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(Datetime, mean_rho, col = "mean_rho")) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(Datetime, Zg- Zv, col = "Zg-Zv")) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(mean_rho, St)) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(Zg- Zv, St)) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(mean_rho, surface_temp, col = yday(Datetime))) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(Zg- Zv, surface_temp, col = yday(Datetime))) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(Zg- Zv, surface_temp - bottom_temp, col = yday(Datetime))) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(Zg- Zv, therm_dep, col = yday(Datetime))) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(mean_rho, surface_temp - bottom_temp, col = yday(Datetime))) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(mean_rho, Zg- Zv, col = yday(Datetime))) +
  facet_wrap(~ lake)

ggplot(results) +
  geom_point(aes(mean_rho, Zg- Zv, size = mean_depth, col = St), alpha = 0.2) 

ggplot(results) +
  geom_point(aes(mean_rho, Zg- Zv, col = lake, size = St), alpha = 0.6) 
             
results$g = 9.81

ggplot(results) +
  geom_point(aes(Datetime, St, col = "SchmidtStability")) +
  geom_point(aes(Datetime, mean_rho, col = "mean_rho")) +
  geom_point(aes(Datetime, Zg- Zv, col = "Zg-Zv")) +
  geom_point(aes(Datetime, g, col = "g")) +
  geom_point(aes(Datetime, mean_depth, col = "mean_depth")) +
  facet_wrap(~ lake)
