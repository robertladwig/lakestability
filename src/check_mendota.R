# GET LAKE DATA FROM NTL-LTER
# AUTHOR: Robert Ladwig
rm(list = ls())
Sys.setenv(TZ="UTC")

# setwd('~/Documents/lakestability/src/')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
# library(ecmwfr)
# library(ncdf4)
library(lubridate)
library(rLakeAnalyzer)
library(MetBrewer)
library(broom)


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
  # mean_density_depth <- approx(layerP, layerD, mean_density)$y
  
  Zcv <- layerD %*% layerA / sum(layerA)
  Zg <- (layerP %*% (layerD * layerA)) / sum(layerA %*% layerP)
  
  St <- (layerP - as.vector(mean_density)) %*% ((layerD - as.vector(Zg)) * layerA) * dz * g / Ao
  # St_perLayer <- (layerP - as.vector(mean_density)) * ((layerD - as.vector(Zg)) * layerA) #* dz 
  
  # St_perLayer = approx(layerD, St_perLayer, orig_depths)$y
  
  return(data.frame('St' = St, 'Zv' = Zcv, 'Zg' = Zg))
  
}



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

detach(dt1)               

df_watTemp <- dt1 %>%
  filter(lakeid == 'ME') %>%
  select(sampledate, depth, wtemp)

# Package ID: knb-lter-ntl.31.32 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Secchi Disk Depth; Other Auxiliary Base Crew Sample Data 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/32/d01c782e0601d2217b94dd614444bd33" 
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
                 "daynum",     
                 "sampledate",     
                 "sta",     
                 "secview",     
                 "secnview",     
                 "timeon",     
                 "timeoff",     
                 "airtemp",     
                 "windir",     
                 "windspd",     
                 "waveht",     
                 "cloud",     
                 "ice"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$secview)=="factor") dt1$secview <-as.numeric(levels(dt1$secview))[as.integer(dt1$secview) ]               
if (class(dt1$secview)=="character") dt1$secview <-as.numeric(dt1$secview)
if (class(dt1$secnview)=="factor") dt1$secnview <-as.numeric(levels(dt1$secnview))[as.integer(dt1$secnview) ]               
if (class(dt1$secnview)=="character") dt1$secnview <-as.numeric(dt1$secnview)
if (class(dt1$timeon)=="factor") dt1$timeon <-as.numeric(levels(dt1$timeon))[as.integer(dt1$timeon) ]               
if (class(dt1$timeon)=="character") dt1$timeon <-as.numeric(dt1$timeon)
if (class(dt1$timeoff)=="factor") dt1$timeoff <-as.numeric(levels(dt1$timeoff))[as.integer(dt1$timeoff) ]               
if (class(dt1$timeoff)=="character") dt1$timeoff <-as.numeric(dt1$timeoff)
if (class(dt1$airtemp)=="factor") dt1$airtemp <-as.numeric(levels(dt1$airtemp))[as.integer(dt1$airtemp) ]               
if (class(dt1$airtemp)=="character") dt1$airtemp <-as.numeric(dt1$airtemp)
if (class(dt1$windir)!="factor") dt1$windir<- as.factor(dt1$windir)
if (class(dt1$windspd)=="factor") dt1$windspd <-as.numeric(levels(dt1$windspd))[as.integer(dt1$windspd) ]               
if (class(dt1$windspd)=="character") dt1$windspd <-as.numeric(dt1$windspd)
if (class(dt1$waveht)=="factor") dt1$waveht <-as.numeric(levels(dt1$waveht))[as.integer(dt1$waveht) ]               
if (class(dt1$waveht)=="character") dt1$waveht <-as.numeric(dt1$waveht)
if (class(dt1$cloud)=="factor") dt1$cloud <-as.numeric(levels(dt1$cloud))[as.integer(dt1$cloud) ]               
if (class(dt1$cloud)=="character") dt1$cloud <-as.numeric(dt1$cloud)
if (class(dt1$ice)!="factor") dt1$ice<- as.factor(dt1$ice)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

detach(dt1)               

df_secchi <- dt1 %>%
  filter(lakeid == 'ME') %>%
  select(sampledate, secview, secnview, airtemp, windspd)

meteo <- read.csv('../data/NLDAS2_Mendota_1979_2016_cell_5_GLMReady_cut_timezonecorr_snow.csv') 
df_meteo <- meteo  %>%
  mutate(date = as.Date(Date)) %>%
  group_by(date) %>%
  summarise_all(mean)
  



df_hypso <- read.csv("../data/bathymetry.csv")

depths <- df_hypso$Depth_meter#seq(min(df_hypso$Depth_meter), max(df_hypso$Depth_meter), 0.1)
areas <- df_hypso$Area_meterSquared#approx(df_hypso$Depth_meter, df_hypso$Area_meterSquared, depths)$y

meanDepth = sum(areas)/max(areas)

df_schmidt <- data.frame(sampledate = NULL,
                        st = NULL,
                        zv = NULL,
                        zg = NULL,
                        meta_upper = NULL,
                        meta_lower = NULL,
                        thermDep = NULL,
                        cenBuoy = NULL,
                        secchi = NULL,
                        airTemp = NULL,
                        windSpd = NULL,
                        jb = NULL,
                        ux = NULL,
                        lmo = NULL,
                        zcrit = NULL,
                        lmo2 = NULL,
                        zcrit2 = NULL,
                        rad = NULL,
                        zg_temp = NULL)

for (time in unique(df_watTemp$sampledate)){
  
  data <- df_watTemp %>%
    filter(sampledate == time)
  
  anxi <- df_secchi %>%
    filter(sampledate == time)
  
  meteo <- df_meteo %>%
    filter(date == time) %>%
    mutate(jb = (207 * 10^(-6) * 9.81)/(4180 * 1000) * ShortWave / 1e3 * 1000,
           ux = sqrt(1.3 *10^(-3) * 1.43 * 10^(-3) * (WindSpeed)^2),
           lmo = ux^3 /(jb * 0.41),
           lmo2 = ux^3 /(ShortWave))
  
  if (year(mean(data$sampledate) ) > 2016){
    next
  }
  
  if (nrow(anxi) == 0){
    idy = which.min(abs(unique(df_secchi$sampledate) - unique(data$sampledate)))
    
    anxi <- df_secchi %>%
      filter(sampledate == unique(df_secchi$sampledate)[idy])
  }
  
  if (all(is.na(data$wtemp))){
    next
  }
  
  
  results <- schmidt.stability_idso(wtr = data$wtemp, depths = data$depth, bthA = areas, bthD = depths)
  
  meta <- meta.depths(wtr = data$wtemp, depths = data$depth)
  td <- thermo.depth(wtr = data$wtemp, depths = data$depth)
  buoy <- center.buoyancy(wtr = data$wtemp, depths = data$depth)
  
  length = 9230 
  z_crit = 0.493 * anxi$secnview  + sqrt(0.493**2 * anxi$secnview **2 + 0.0006 * length * meteo$lmo)
  z_crit2 = 0.493 * anxi$secnview  + sqrt(0.493**2 * anxi$secnview **2 + 0.0006 * length * meteo$lmo2)
  
  zg_temp = approx(data$depth, data$wtemp, (results$Zg))$y
  
  df_schmidt <- rbind(df_schmidt, data.frame(sampledate = mean(data$sampledate),
                          st = (results$St),
                          zv = (results$Zv),
                          zg = (results$Zg),
                          meta_upper = meta[1],
                          meta_lower = meta[2],
                          thermDep = td,
                          cenBuoy = buoy,
                          secchi = anxi$secnview,
                          airTemp = anxi$airtemp,
                          windSpd = anxi$windspd,
                          jb = meteo$jb,
                          ux = meteo$ux,
                          lmo = meteo$lmo,
                          zcrit = z_crit,
                          lmo2 = meteo$lmo2,
                          zcrit2 = z_crit2,
                          rad = meteo$ShortWave,
                          zg_temp = zg_temp))
  
}


ggplot(df_schmidt) +
  geom_line(aes(zg/zv, st))

ggplot(df_schmidt) +
  geom_point(aes(sampledate, zg, col = "Mass center")) +
  geom_line(aes(sampledate, zv, col = "Volume center")) +
  geom_point(aes(sampledate, meta_upper, col = "Meta upper")) +
  geom_point(aes(sampledate, meta_lower, col = "Meta lower")) +
  geom_point(aes(sampledate, thermDep, col = "Thermocline")) +
  geom_point(aes(sampledate, cenBuoy, col = "Buoyancy center")) +
  geom_point(aes(sampledate, lmo, col = "Monin-Obkuhov length")) +
  geom_point(aes(sampledate, zcrit, col = "Critical length")) +
  scale_y_continuous(trans = "reverse") +
  ylim(0, 25)+
  theme_minimal()

ggplot(df_schmidt) +
  geom_point(aes(sampledate, zg/zv, col = st) )

ggplot(df_schmidt) +
  geom_point(aes(sampledate, zcrit/meanDepth, col = scale(st)) )

ggplot(df_schmidt) +
  geom_point(aes(secchi/meanDepth, zg/zv, col = st) )

ggplot(df_schmidt) +
  geom_point(aes(jb, zg-zv, col = st) )

ggplot(df_schmidt) +
  geom_point(aes(zcrit2/meanDepth, zg, col = st) )

summary(lm(log10(zg) ~ jb + ux + lmo + zcrit + lmo2 + zcrit2 + secchi + rad, data = df_schmidt))
summary(lm(log10(zg) ~ jb, data = df_schmidt))
            

library(gganimate)
a = 1
for (i in unique(df_watTemp$sampledate)){
  g <- ggplot() +
    geom_path(data = df_watTemp %>% filter(sampledate == i), aes(wtemp, depth, group = sampledate)) +
    geom_point(data = df_schmidt %>% filter(sampledate == i), aes(zg_temp, zv+(zg - zv) * 500 , group = sampledate, col = st)) +
    geom_hline( yintercept = mean(df_schmidt$zv)) +
    scale_y_continuous(trans = "reverse", limits = c(25,0)) +
    ggtitle(paste0(unique(df_watTemp$sampledate)[a])) + 
    xlim(0, 30) + #ylim(0,25) +
    # ylim(8.1,  8.7) +
    theme_bw(); g
  ggsave(paste0('../figs/gravity/',a,'.png'), g)
  a= a+1
  
}
g <- ggplot() +
  geom_path(data = df_watTemp, aes(wtemp, depth, group = sampledate)) +
  geom_point(data = df_schmidt, aes(zg_temp, zv+(zg - zv) * 500 , group = sampledate, col = st)) +
  geom_hline(yintercept = mean(df_schmidt$zv)) +
  scale_y_continuous(trans = "reverse") +
  # ylim(8.1,  8.7) +
  theme_minimal(); g

# g + transition_time(sampledate)
