# GET LAKE DATA FROM PILLA ET AL. (2022)
# AUTHOR: Robert Ladwig
rm(list = ls())
Sys.setenv(TZ="UTC")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ecmwfr)
library(ncdf4)
library(lubridate)
library(rLakeAnalyzer)

# ORIGINAL METADATA
# Package ID: edi.705.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Global data set of long-term summertime vertical temperature profiles in 153 lakes.
# Data set creator:  Rachel Pilla - Miami University 
# Data set creator:  Elizabeth Mette - Miami University 
# Data set creator:  Craig Williamson - Miami University 
# Data set creator:  Boris Adamovich - Belarusian State University 
# Data set creator:  Rita Adrian - IGB-Berlin 
# Data set creator:  Orlane Anneville - INRAE, University of Savoie Mont-Blanc 
# Data set creator:  Esteban Balseiro - University of Comahue: INIBIOMA, CONICET 
# Data set creator:  Syuhei Ban - University of Shiga Prefecture 
# Data set creator:  Sudeep Chandra - University of Nevada, Reno 
# Data set creator:  William Colom-Montero - Uppsala University 
# Data set creator:  Shawn Devlin - University of Montana 
# Data set creator:  Margaret Dix - Universidad del Valle de Guatemala Centro de Estudios Atitlan 
# Data set creator:  Martin Dokulil - University of Insbruck 
# Data set creator:  Natalie Feldsine - Mohonk Preserve 
# Data set creator:  Heidrun Feuchtmayr - UK Centre for Ecology & Hydrology 
# Data set creator:  Natalie Fogarty - Seqwater 
# Data set creator:  Evelyn Gaiser - Florida International University 
# Data set creator:  Scott Girdner - U.S. National Park Service 
# Data set creator:  MarÃ­a GonzÃ¡lez - Miami University 
# Data set creator:  K Hambright - University of Oklahoma 
# Data set creator:  David Hamilton - Griffith University 
# Data set creator:  Karl Havens - University of Florida 
# Data set creator:  Dag Hessen - University of Oslo 
# Data set creator:  Harald Hetzenauer - Institut fÃ¼r Seenforschung 
# Data set creator:  Scott Higgins - IISD Experimnetal Lake Area Inc. 
# Data set creator:  Timo Huttula - FAO; BELSPO 
# Data set creator:  Hannu Huuskonen - University of Eastern Finland 
# Data set creator:  Peter Isles - Swiss Federeal Institute of Aquatic Science and Technology 
# Data set creator:  Klaus Joehnk - CSIRO 
# Data set creator:  Wendel Keller - Laurentian University 
# Data set creator:  Jen Klug - Fairfield University 
# Data set creator:  Lesley Knoll - University of Minnesota 
# Data set creator:  Johanna Korhonen - Finnish Environment Institute SYKE 
# Data set creator:  Nikolai Korovchinsky - A.N. Severtsov Institute of Ecology and Evolution of The Russian Academy of Sciences 
# Data set creator:  Oliver KÃ¶ster - Zurich Water Supply 
# Data set creator:  Benjamin Kraemer - Leibniz-Institute of Freshwater Ecology and Inland Fisheries 
# Data set creator:  Peter Leavitt - University of Regina 
# Data set creator:  Barbara Leoni - Milano-Bicocca University 
# Data set creator:  Fabio Lepori - University of Applied Sciences and Arts of Southern Switzerland 
# Data set creator:  Ekaterina Lepskaya - Kamchatka Research Institute of Fisheries & Oceanography 
# Data set creator:  Noah Lottig - University of Wisconsin 
# Data set creator:  Martin Luger - Federal Agency for Water Management 
# Data set creator:  Stephen Maberly - UK Centre for Ecology & Hydrology 
# Data set creator:  Sally MacIntyre - University of California Santa Barbara 
# Data set creator:  Chris McBride - University of Waikato 
# Data set creator:  Peter McIntyre - University of Wisconsin 
# Data set creator:  Stephanie Melles - Ryerson University 
# Data set creator:  Beatriz Modenutti - University of Comahue: INIBIOMA, CONICET 
# Data set creator:  DÃ¶rthe MÃ¼ller-Navarra - University of Hamburg 
# Data set creator:  Laura Pacholski - Dominion Diamond Mines 
# Data set creator:  Andrew Paterson - Ontario Ministry of the Environment, Conservation and Parks 
# Data set creator:  Don Pierson - Uppsala University 
# Data set creator:  Helen Pislegina - Irkutsk State University 
# Data set creator:  Pierre-Denis Plisnier - University of LiÃ¨ge 
# Data set creator:  David Richardson - SUNY New Paltz 
# Data set creator:  Alon Rimmer - Israel Oceanographic and Limnological Research 
# Data set creator:  Michela Rogora - CNR Water Research institute 
# Data set creator:  Denis Rogozin - Krasnoyarsk Scientific Center SB RAS 
# Data set creator:  Jim Rusak - Ontario Ministry of the Environment, Conservation and Parks 
# Data set creator:  Olga Rusanovskaya - Irkutsk State University 
# Data set creator:  Steve Sadro - University of California Davis 
# Data set creator:  Nico Salmaso - Fondazione Edmund Mach 
# Data set creator:  Jasmine Saros - University of Maine 
# Data set creator:  Jouko Sarvala - University of Turku 
# Data set creator:  Ãmilie Saulnier-Talbot - UniversitÃ© Laval Departments of biology and geography 
# Data set creator:  Daniel Schindler - University of Washington 
# Data set creator:  Svetlana Shimaraeva - Irkutsk State University 
# Data set creator:  Eugene Silow - Irkutsk State University 
# Data set creator:  Lewis Sitoki - The Technical University of Kenya 
# Data set creator:  Ruben Sommaruga - University of Innsbruck 
# Data set creator:  Dietmar Straile - University of Konstanz 
# Data set creator:  Kristin Strock - Dickinson College 
# Data set creator:  Hilary Swain - Archbold Biological Station 
# Data set creator:  Jason Tallant - University of Michigan 
# Data set creator:  Wim Thiery - 1 Vrije Universiteit Brussel; 2 ETH Zurich 
# Data set creator:  Maxim Timofeyev - Irkutsk State University 
# Data set creator:  Alexander Tolomeev - Krasnoyarsk Scientific Center SB RAS 
# Data set creator:  Koji Tominaga - University of Oslo 
# Data set creator:  Michael Vanni - Miami University 
# Data set creator:  Piet Verburg - National Institute of Water & Atmospheric Research 
# Data set creator:  Rolf Vinebrooke - University of Alberta 
# Data set creator:  Josef WanzenbÃ¶ck - University of Insbruck 
# Data set creator:  Kathleen Weathers - SUNY New Paltz; Cary Institute 
# Data set creator:  Gesa Weyhenmeyer - Uppsala University 
# Data set creator:  Egor Zadereev - Krasnoyarsk Scientific Center SB RAS 
# Data set creator:  Tatyana Zhukova - Belarusian State University 
# Contact:  Rachel Pilla -  Miami University  - pillarm@miamioh.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/705/5/1c3181f05aab7420182a14851f916ef3" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "SiteID",     
                 "LakeID",     
                 "LakeName",     
                 "Date",     
                 "Depth_m",     
                 "Temperature_degCelsius"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$SiteID)=="factor") dt1$SiteID <-as.numeric(levels(dt1$SiteID))[as.integer(dt1$SiteID) ]               
if (class(dt1$SiteID)=="character") dt1$SiteID <-as.numeric(dt1$SiteID)
if (class(dt1$LakeID)=="factor") dt1$LakeID <-as.numeric(levels(dt1$LakeID))[as.integer(dt1$LakeID) ]               
if (class(dt1$LakeID)=="character") dt1$LakeID <-as.numeric(dt1$LakeID)
if (class(dt1$LakeName)!="factor") dt1$LakeName<- as.factor(dt1$LakeName)                                   
# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 
if (class(dt1$Depth_m)=="factor") dt1$Depth_m <-as.numeric(levels(dt1$Depth_m))[as.integer(dt1$Depth_m) ]               
if (class(dt1$Depth_m)=="character") dt1$Depth_m <-as.numeric(dt1$Depth_m)
if (class(dt1$Temperature_degCelsius)=="factor") dt1$Temperature_degCelsius <-as.numeric(levels(dt1$Temperature_degCelsius))[as.integer(dt1$Temperature_degCelsius) ]               
if (class(dt1$Temperature_degCelsius)=="character") dt1$Temperature_degCelsius <-as.numeric(dt1$Temperature_degCelsius)

# Convert Missing Values to NA for non-dates

dt1$Temperature_degCelsius <- ifelse((trimws(as.character(dt1$Temperature_degCelsius))==trimws("NA")),NA,dt1$Temperature_degCelsius)               
suppressWarnings(dt1$Temperature_degCelsius <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temperature_degCelsius))==as.character(as.numeric("NA"))),NA,dt1$Temperature_degCelsius))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(SiteID)
summary(LakeID)
summary(LakeName)
summary(Date)
summary(Depth_m)
summary(Temperature_degCelsius) 
# Get more details on character variables

summary(as.factor(dt1$LakeName))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/705/5/1e5e05e46c3094d2d368a798eae22cbf" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "SiteID",     
                 "LakeID",     
                 "LakeName",     
                 "Date",     
                 "Depth_m",     
                 "Temperature_degCelsius"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$SiteID)=="factor") dt2$SiteID <-as.numeric(levels(dt2$SiteID))[as.integer(dt2$SiteID) ]               
if (class(dt2$SiteID)=="character") dt2$SiteID <-as.numeric(dt2$SiteID)
if (class(dt2$LakeID)=="factor") dt2$LakeID <-as.numeric(levels(dt2$LakeID))[as.integer(dt2$LakeID) ]               
if (class(dt2$LakeID)=="character") dt2$LakeID <-as.numeric(dt2$LakeID)
if (class(dt2$LakeName)!="factor") dt2$LakeName<- as.factor(dt2$LakeName)                                   
# attempting to convert dt2$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2Date<-as.Date(dt2$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2Date) == length(tmp2Date[!is.na(tmp2Date)])){dt2$Date <- tmp2Date } else {print("Date conversion failed for dt2$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2Date) 
if (class(dt2$Depth_m)=="factor") dt2$Depth_m <-as.numeric(levels(dt2$Depth_m))[as.integer(dt2$Depth_m) ]               
if (class(dt2$Depth_m)=="character") dt2$Depth_m <-as.numeric(dt2$Depth_m)
if (class(dt2$Temperature_degCelsius)=="factor") dt2$Temperature_degCelsius <-as.numeric(levels(dt2$Temperature_degCelsius))[as.integer(dt2$Temperature_degCelsius) ]               
if (class(dt2$Temperature_degCelsius)=="character") dt2$Temperature_degCelsius <-as.numeric(dt2$Temperature_degCelsius)

# Convert Missing Values to NA for non-dates

dt2$Temperature_degCelsius <- ifelse((trimws(as.character(dt2$Temperature_degCelsius))==trimws("NA")),NA,dt2$Temperature_degCelsius)               
suppressWarnings(dt2$Temperature_degCelsius <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Temperature_degCelsius))==as.character(as.numeric("NA"))),NA,dt2$Temperature_degCelsius))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(SiteID)
summary(LakeID)
summary(LakeName)
summary(Date)
summary(Depth_m)
summary(Temperature_degCelsius) 
# Get more details on character variables

summary(as.factor(dt2$LakeName))
detach(dt2)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/705/5/974aa15895f4d691edd7a9dcb0ab4457" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "SiteID",     
                 "LakeID",     
                 "LakeName",     
                 "AlternateLakeName",     
                 "LakeOrReservoir",     
                 "CountryOfLake",     
                 "Region",     
                 "Latitude",     
                 "Longitude",     
                 "Elevation_m",     
                 "SurfaceArea_km2",     
                 "Volume_km3",     
                 "MaxDepth_m",     
                 "MeanDepth_m",     
                 "Secchi_m",     
                 "Chlorophyll_ug_L",     
                 "TotalPhosphorus_ug_L",     
                 "DissolvedOrganicCarbon_mg_L",     
                 "Contributor",     
                 "ContributorContact",     
                 "ContributorInstitution"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$SiteID)=="factor") dt3$SiteID <-as.numeric(levels(dt3$SiteID))[as.integer(dt3$SiteID) ]               
if (class(dt3$SiteID)=="character") dt3$SiteID <-as.numeric(dt3$SiteID)
if (class(dt3$LakeID)=="factor") dt3$LakeID <-as.numeric(levels(dt3$LakeID))[as.integer(dt3$LakeID) ]               
if (class(dt3$LakeID)=="character") dt3$LakeID <-as.numeric(dt3$LakeID)
if (class(dt3$LakeName)!="factor") dt3$LakeName<- as.factor(dt3$LakeName)
if (class(dt3$AlternateLakeName)!="factor") dt3$AlternateLakeName<- as.factor(dt3$AlternateLakeName)
if (class(dt3$LakeOrReservoir)!="factor") dt3$LakeOrReservoir<- as.factor(dt3$LakeOrReservoir)
if (class(dt3$CountryOfLake)!="factor") dt3$CountryOfLake<- as.factor(dt3$CountryOfLake)
if (class(dt3$Region)!="factor") dt3$Region<- as.factor(dt3$Region)
if (class(dt3$Latitude)=="factor") dt3$Latitude <-as.numeric(levels(dt3$Latitude))[as.integer(dt3$Latitude) ]               
if (class(dt3$Latitude)=="character") dt3$Latitude <-as.numeric(dt3$Latitude)
if (class(dt3$Longitude)=="factor") dt3$Longitude <-as.numeric(levels(dt3$Longitude))[as.integer(dt3$Longitude) ]               
if (class(dt3$Longitude)=="character") dt3$Longitude <-as.numeric(dt3$Longitude)
if (class(dt3$Elevation_m)=="factor") dt3$Elevation_m <-as.numeric(levels(dt3$Elevation_m))[as.integer(dt3$Elevation_m) ]               
if (class(dt3$Elevation_m)=="character") dt3$Elevation_m <-as.numeric(dt3$Elevation_m)
if (class(dt3$SurfaceArea_km2)=="factor") dt3$SurfaceArea_km2 <-as.numeric(levels(dt3$SurfaceArea_km2))[as.integer(dt3$SurfaceArea_km2) ]               
if (class(dt3$SurfaceArea_km2)=="character") dt3$SurfaceArea_km2 <-as.numeric(dt3$SurfaceArea_km2)
if (class(dt3$Volume_km3)=="factor") dt3$Volume_km3 <-as.numeric(levels(dt3$Volume_km3))[as.integer(dt3$Volume_km3) ]               
if (class(dt3$Volume_km3)=="character") dt3$Volume_km3 <-as.numeric(dt3$Volume_km3)
if (class(dt3$MaxDepth_m)=="factor") dt3$MaxDepth_m <-as.numeric(levels(dt3$MaxDepth_m))[as.integer(dt3$MaxDepth_m) ]               
if (class(dt3$MaxDepth_m)=="character") dt3$MaxDepth_m <-as.numeric(dt3$MaxDepth_m)
if (class(dt3$MeanDepth_m)=="factor") dt3$MeanDepth_m <-as.numeric(levels(dt3$MeanDepth_m))[as.integer(dt3$MeanDepth_m) ]               
if (class(dt3$MeanDepth_m)=="character") dt3$MeanDepth_m <-as.numeric(dt3$MeanDepth_m)
if (class(dt3$Secchi_m)=="factor") dt3$Secchi_m <-as.numeric(levels(dt3$Secchi_m))[as.integer(dt3$Secchi_m) ]               
if (class(dt3$Secchi_m)=="character") dt3$Secchi_m <-as.numeric(dt3$Secchi_m)
if (class(dt3$Chlorophyll_ug_L)=="factor") dt3$Chlorophyll_ug_L <-as.numeric(levels(dt3$Chlorophyll_ug_L))[as.integer(dt3$Chlorophyll_ug_L) ]               
if (class(dt3$Chlorophyll_ug_L)=="character") dt3$Chlorophyll_ug_L <-as.numeric(dt3$Chlorophyll_ug_L)
if (class(dt3$TotalPhosphorus_ug_L)=="factor") dt3$TotalPhosphorus_ug_L <-as.numeric(levels(dt3$TotalPhosphorus_ug_L))[as.integer(dt3$TotalPhosphorus_ug_L) ]               
if (class(dt3$TotalPhosphorus_ug_L)=="character") dt3$TotalPhosphorus_ug_L <-as.numeric(dt3$TotalPhosphorus_ug_L)
if (class(dt3$DissolvedOrganicCarbon_mg_L)=="factor") dt3$DissolvedOrganicCarbon_mg_L <-as.numeric(levels(dt3$DissolvedOrganicCarbon_mg_L))[as.integer(dt3$DissolvedOrganicCarbon_mg_L) ]               
if (class(dt3$DissolvedOrganicCarbon_mg_L)=="character") dt3$DissolvedOrganicCarbon_mg_L <-as.numeric(dt3$DissolvedOrganicCarbon_mg_L)
if (class(dt3$Contributor)!="factor") dt3$Contributor<- as.factor(dt3$Contributor)
if (class(dt3$ContributorContact)!="factor") dt3$ContributorContact<- as.factor(dt3$ContributorContact)
if (class(dt3$ContributorInstitution)!="factor") dt3$ContributorInstitution<- as.factor(dt3$ContributorInstitution)

# Convert Missing Values to NA for non-dates

dt3$Elevation_m <- ifelse((trimws(as.character(dt3$Elevation_m))==trimws("NA")),NA,dt3$Elevation_m)               
suppressWarnings(dt3$Elevation_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Elevation_m))==as.character(as.numeric("NA"))),NA,dt3$Elevation_m))
dt3$SurfaceArea_km2 <- ifelse((trimws(as.character(dt3$SurfaceArea_km2))==trimws("NA")),NA,dt3$SurfaceArea_km2)               
suppressWarnings(dt3$SurfaceArea_km2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SurfaceArea_km2))==as.character(as.numeric("NA"))),NA,dt3$SurfaceArea_km2))
dt3$Volume_km3 <- ifelse((trimws(as.character(dt3$Volume_km3))==trimws("NA")),NA,dt3$Volume_km3)               
suppressWarnings(dt3$Volume_km3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Volume_km3))==as.character(as.numeric("NA"))),NA,dt3$Volume_km3))
dt3$MaxDepth_m <- ifelse((trimws(as.character(dt3$MaxDepth_m))==trimws("NA")),NA,dt3$MaxDepth_m)               
suppressWarnings(dt3$MaxDepth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$MaxDepth_m))==as.character(as.numeric("NA"))),NA,dt3$MaxDepth_m))
dt3$MeanDepth_m <- ifelse((trimws(as.character(dt3$MeanDepth_m))==trimws("NA")),NA,dt3$MeanDepth_m)               
suppressWarnings(dt3$MeanDepth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$MeanDepth_m))==as.character(as.numeric("NA"))),NA,dt3$MeanDepth_m))
dt3$Secchi_m <- ifelse((trimws(as.character(dt3$Secchi_m))==trimws("NA")),NA,dt3$Secchi_m)               
suppressWarnings(dt3$Secchi_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Secchi_m))==as.character(as.numeric("NA"))),NA,dt3$Secchi_m))
dt3$Chlorophyll_ug_L <- ifelse((trimws(as.character(dt3$Chlorophyll_ug_L))==trimws("NA")),NA,dt3$Chlorophyll_ug_L)               
suppressWarnings(dt3$Chlorophyll_ug_L <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Chlorophyll_ug_L))==as.character(as.numeric("NA"))),NA,dt3$Chlorophyll_ug_L))
dt3$TotalPhosphorus_ug_L <- ifelse((trimws(as.character(dt3$TotalPhosphorus_ug_L))==trimws("NA")),NA,dt3$TotalPhosphorus_ug_L)               
suppressWarnings(dt3$TotalPhosphorus_ug_L <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$TotalPhosphorus_ug_L))==as.character(as.numeric("NA"))),NA,dt3$TotalPhosphorus_ug_L))
dt3$DissolvedOrganicCarbon_mg_L <- ifelse((trimws(as.character(dt3$DissolvedOrganicCarbon_mg_L))==trimws("NA")),NA,dt3$DissolvedOrganicCarbon_mg_L)               
suppressWarnings(dt3$DissolvedOrganicCarbon_mg_L <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$DissolvedOrganicCarbon_mg_L))==as.character(as.numeric("NA"))),NA,dt3$DissolvedOrganicCarbon_mg_L))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(SiteID)
summary(LakeID)
summary(LakeName)
summary(AlternateLakeName)
summary(LakeOrReservoir)
summary(CountryOfLake)
summary(Region)
summary(Latitude)
summary(Longitude)
summary(Elevation_m)
summary(SurfaceArea_km2)
summary(Volume_km3)
summary(MaxDepth_m)
summary(MeanDepth_m)
summary(Secchi_m)
summary(Chlorophyll_ug_L)
summary(TotalPhosphorus_ug_L)
summary(DissolvedOrganicCarbon_mg_L)
summary(Contributor)
summary(ContributorContact)
summary(ContributorInstitution) 
# Get more details on character variables

summary(as.factor(dt3$LakeName)) 
summary(as.factor(dt3$AlternateLakeName)) 
summary(as.factor(dt3$LakeOrReservoir)) 
summary(as.factor(dt3$CountryOfLake)) 
summary(as.factor(dt3$Region)) 
summary(as.factor(dt3$Contributor)) 
summary(as.factor(dt3$ContributorContact)) 
summary(as.factor(dt3$ContributorInstitution))
detach(dt3)               


site_information <- dt3 %>%
  select(SiteID, LakeID, LakeName, Latitude, Longitude, Elevation_m, SurfaceArea_km2, Volume_km3, MaxDepth_m, 
         MeanDepth_m, Secchi_m)

interpolated_data <- read.csv("../data/TempProfiles_Interpolated.csv")

df <- merge(interpolated_data, site_information, by = c("LakeID", "SiteID", "LakeName"))

# ERA5:
source("get_meteorology.R")

overall_df <- data.frame(lake = NULL,
                         date = NULL,
                         st = NULL,
                         zv = NULL,
                         lmo = NULL,
                         mean_wind = NULL,
                         mean_swr = NULL)

for (lakes in unique(df$LakeName)){
  print(paste0(match(lakes, unique(df$LakeName)),'/',length(unique(df$LakeName))))
  
  input <- df %>% filter(LakeName == lakes)
  lat = mean(input$Latitude)
  lon = mean(input$Longitude)
  
  hypsography = approx.bathy(Zmax = mean(input$MaxDepth_m), lkeArea = mean(input$SurfaceArea_km2 * 1e6), Zmean = mean(input$MeanDepth_m), method = "voldev", zinterval = 0.5)
  
  for (dates in unique(input$Date)){
    print(paste0(match(dates, unique(input$Date)),'/',length(unique(input$Date))))
    
    input_date <- input %>%
      filter(Date == dates)
    
    st = schmidt.stability(wtr = input_date$Temperature_degCelsius, depths =  input_date$Depth_m, bthA = hypsography$Area.at.z, bthD = hypsography$depths)
    
    zv <- hypsography$depths %*% hypsography$Area.at.z / sum(hypsography$Area.at.z, na.rm = TRUE)
    
    output <- get_meteorology(lakename = lakes, 
                              path = '../era5_output/',
                              password = read_csv("../sensitive/era5.txt"),
                              datetime <- dates,
                              lat = lat,
                              lon = lon, 
                              variables = c("10u", "10v","mean_surface_downward_short_wave_radiation_flux"))
    
    overall_df <- rbind(overall_df, data.frame(lake = lakes,
                             date = dates,
                             st = st,
                             zv = zv,
                             lmo = output[[2]][1],
                             mean_wind = output[[2]][2],
                             mean_swr = output[[2]][3]))
  }
}

write_csv(overall_df, file = '../analysis_output/hypothesis_test')
ggplot(overall_df, aes(lmo/ zv, st)) + geom_point()




