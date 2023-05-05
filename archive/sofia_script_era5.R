rm(list = ls())
Sys.setenv(TZ="UTC")
library(ncdf4)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ecmwfr)
library(here)

# Input file with lake name and coordinates:
sites <- read_csv("xx.csv")

outputDir <- here::here("Input", "Data", "ERA5")

#  Read coordinates 
lake_coord <- sites %>% select(lake, lat, lon)

#  adding 0.1 degrees to the center of lakes for a uniform box per coordinate:
lake_coord <- lake_coord %>%
  mutate(lon1 = round(lon - 0.1, 1),
         lon2 = round(lon + 0.1, 1),
         lat1 = round(lat - 0.1, 1),
         lat2 = round(lat + 0.1, 1))

#  extract lake names
lake_names <- select(sites, lake)

#  Define start  and end years
yearStart <- 1960
yearEnd <- 2022

#  Define vector based on start year and end year
year <- as.character(seq(yearStart,yearEnd))
byear <- split(year, ceiling(seq_along(year) / 8))

# define variables and timestep
variables <- c("2m_temperature", "total_precipitation")
timestep <- "hourly"

# Set user and key 
wf_set_key(user = "12345",
           key = "xxxx",
           service = "cds")

# Download all years for all lake_names ----------------------------------

for (j in 1:length(lake_names)) {
  
  message(paste("Run code for ", lake_names[j, ],"... started: ", Sys.time(),"\n", sep = ""))
  
  #  (N,W,S,E)
  area_lake <- as.numeric(lake_coord[j, c("lat2","lon1","lat1","lon2")])
  
  
  lnam <- tolower(gsub(" ", "_", lake_names[j, ]))
  out_dir <- paste0(outputDir, '/', lnam)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  for(v in variables) {
    
    #  Request of files from the Copernicus website -------------------------------
    for (i in 1:length(byear)) {
      
      
      output <- sapply(byear[i], \(y) {
        paste0("era5", "", v, "", timestep, "", y, "", lake_names[j, ],".nc")
      })
      output_chk <- (file.exists(file.path(out_dir, output)))
      
      if(all(output_chk)) {
        next
      }
      
      tgt_year <- byear[[i]][!output_chk]
      
      message(paste("Run code for batch ", paste0(tgt_year, collapse = ", "),"...started: ", Sys.time(),"\n", sep = ""))
      
      request_list <- lapply(tgt_year, \(y) {
        list(
          
          dataset_short_name = "reanalysis-era5-single-levels", # "reanalysis-era5-land",
          product_type   = "reanalysis",
          format = "netcdf",
          variable = v,
          year = y,
          month = c("01","02","03","04","05","06",
                    "07","08","09","10","11","12"),
          day = c("01","02","03","04","05","06","07","08","09","10",
                  "11","12","13","14","15","16","17","18","19","20",
                  "21","22","23","24","25","26","27","28","29","30","31"),
          time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00",
                   "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00",
                   "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
          #  (N,W,S,E)
          area = area_lake,
          target = paste0("era5", "", v, "", timestep, "", y, "", lake_names[j, ],".nc")
          
          
        )
      })
      
      
      fail <- TRUE
      
      while (fail) {
        
        tryCatch({
          wf_request_batch(request_list = request_list, 
                           workers = length(request_list),  
                           user = "12345", 
                           path = out_dir)
        }, error = function(e) return(NULL))
        
        output_chk2 <- sapply(tgt_year, \(y) {
          f <- paste0("era5", "", v, "", timestep, "", y, "", lake_names[j, ],".nc")
          (file.exists(file.path(out_dir, f)))
        })
        
        fail <- ifelse(any(!output_chk2), TRUE, FALSE)
        
      }
      
      message(paste("Run code for ", byear[i],"...finished. ", Sys.time(),"\n", sep = ""))
      
    }
    
    message("Downloaded ", v)
  }
  
  message(paste("Run code for ", lake_names[j, ],"...finished", Sys.time(), "\n", sep = ""))
  
}