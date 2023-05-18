# GET LAKE DATA FROM PILLA ET AL. (2022)
# AUTHOR: Robert Ladwig
rm(list = ls())
Sys.setenv(TZ="UTC")

setwd('~/Documents/lakestability/src/')
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ecmwfr)
library(ncdf4)
library(lubridate)
library(rLakeAnalyzer)

files <- list.files('../data/holgerson_2022/', pattern = "*.csv")

test <- read_csv('../data/holgerson_2022/TempData_HorseshoePond.csv')
temp_col <- grep('Temp_degC', colnames(test))
depth_col <- grep('DepthFromTop_m', colnames(test))
test_data <- test %>%
  select(depth_col, temp_col)#, PondMaxDepth_m,LightAboveSurface_units)

depth_col_new = grep('DepthFromTop_m', colnames(test_data))
temp_col_new = grep('Temp_degC', colnames(test_data))

sub(".*_", "", colnames(test_data)) 

colnames(test_data)[depth_col_new] <- paste0('depth',seq(1,length(depth_col_new)))
colnames(test_data)[temp_col_new] <- paste0('temp',seq(1,length(temp_col_new)))

test_data = test_data %>%
  pivot_longer(
    everything(),
    cols_vary = "slowest",
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)"
  )
test_data$set = rep(test$DateTime, times = 3)

test_data = test_data %>%
  rename(Datetime = set, Depth = d, Wtemp = t) %>%
  arrange(Datetime, Depth)

daily_data <- test_data %>%
  mutate(Date = as.Date(Datetime)) %>%
  group_by(Date, Depth) %>%
  summarise(Wtemp = mean(Wtemp))

# write_csv(df, file = '../data/hypothesis_data.csv')


# ERA5:
source("get_meteorology.R")



get_lake_results <- function(lakes){
  overall_df <- data.frame(lake = NULL,
                           date = NULL,
                           st = NULL,
                           zv = NULL,
                           lmo = NULL,
                           mean_wind = NULL,
                           mean_swr = NULL)
  print(paste0(match(lakes, unique(df$LakeName)),'/',length(unique(df$LakeName))))
  
  input <- df %>% filter(LakeName == lakes)
  lat = mean(input$Latitude)
  lon = mean(input$Longitude)
  
  if (!is.na(mean(input$MeanDepth_m))){
    mean_depth = mean(input$MeanDepth_m)
  } else {
    mean_depth = mean(input$Volume_km3 * 1e9) / mean(input$SurfaceArea_km2 * 1e6)
  }
  if (!is.na(mean(input$MaxDepth_m))){
    max_depth = mean(input$MaxDepth_m)
  } else {
    max_depth = max(input$Depth_m)
  }
  
  
  hypsography = approx.bathy(Zmax = max_depth, lkeArea = mean(input$SurfaceArea_km2 * 1e6), Zmean = mean_depth, method = "voldev", zinterval = 0.5)
  
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
  write_csv(overall_df, file = paste0('../analysis_output/',lakes,'.csv'))
}

# get_lake_results(unique(df$LakeName)[2])

library(parallel)
library(MASS)
# library(future.apply)
numCores <- detectCores()


all.dne <- list.files('../analysis_output/')
all.dne <- str_remove(all.dne, '.csv')

idx <- match(all.dne, unique(df$LakeName))
idy <- seq(1:length(unique(df$LakeName)))

if (length(idx) == 0){
  all.nml_missing <- unique(df$LakeName)[idy]
} else {
  all.nml_missing <- unique(df$LakeName)[idy[-c(idx)]]
}

for (lakes in unique(all.nml_missing)){
  get_lake_results(lakes)
}

# system.time(
#   results <- mclapply(unique(df$LakeName), get_lake_results, mc.cores = numCores)
# )

# plan(multicore)
# future_lapply(unique(df$LakeName), get_lake_results)

# overall_df <- data.frame(lake = NULL,
#                          date = NULL,
#                          st = NULL,
#                          zv = NULL,
#                          lmo = NULL,
#                          mean_wind = NULL,
#                          mean_swr = NULL)
# for (lakes in unique(df$LakeName)){
#   print(paste0(match(lakes, unique(df$LakeName)),'/',length(unique(df$LakeName))))
#   
#   input <- df %>% filter(LakeName == lakes)
#   lat = mean(input$Latitude)
#   lon = mean(input$Longitude)
#   
#   if (!is.na(mean(input$MeanDepth_m))){
#     mean_depth = mean(input$MeanDepth_m)
#   } else {
#     mean_depth = mean(input$Volume_km3 * 1e9) / mean(input$SurfaceArea_km2 * 1e6)
#   }
#   
#   hypsography = approx.bathy(Zmax = mean(input$MaxDepth_m), lkeArea = mean(input$SurfaceArea_km2 * 1e6), Zmean = mean_depth, method = "voldev", zinterval = 0.5)
#   
#   for (dates in unique(input$Date)){
#     print(paste0(match(dates, unique(input$Date)),'/',length(unique(input$Date))))
#     
#     input_date <- input %>%
#       filter(Date == dates)
#     
#     st = schmidt.stability(wtr = input_date$Temperature_degCelsius, depths =  input_date$Depth_m, bthA = hypsography$Area.at.z, bthD = hypsography$depths)
#     
#     zv <- hypsography$depths %*% hypsography$Area.at.z / sum(hypsography$Area.at.z, na.rm = TRUE)
#     
#     output <- get_meteorology(lakename = lakes, 
#                               path = '../era5_output/',
#                               password = read_csv("../sensitive/era5.txt"),
#                               datetime <- dates,
#                               lat = lat,
#                               lon = lon, 
#                               variables = c("10u", "10v","mean_surface_downward_short_wave_radiation_flux"))
#     
#     overall_df <- rbind(overall_df, data.frame(lake = lakes,
#                              date = dates,
#                              st = st,
#                              zv = zv,
#                              lmo = output[[2]][1],
#                              mean_wind = output[[2]][2],
#                              mean_swr = output[[2]][3]))
#     
#     ggplot(overall_df, aes(lmo/ zv, st)) + geom_point()
#   }
# }

# write_csv(overall_df, file = '../analysis_output/hypothesis_test.csv')
# ggplot(overall_df, aes(lmo/ zv, st)) + geom_point() + xlim(0, 3)




