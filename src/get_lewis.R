dt1 = read.csv("data/lewis_2023/Compiled data/temp_o2_interpolated.csv")
hydro = read.csv("data/lewis_2023/Compiled data/hydrolakes_full.csv")

dt = merge(dt1, hydro %>% rename(LakeID = ID), by = 'LakeID')

schmidt.stability_idso = function(wtr, depths, bthA, bthD, sal = 0){
  orig_depths = depths
  
  if (any(is.na(wtr))){
    idx = which(is.na(wtr))
    wtr = wtr[- idx]
    depths = depths[- idx]
  }
  
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
library(factoextra)
library(cluster)


results = data.frame('lake' = NULL,
                     'St' = NULL,
                     'mean_rho' = NULL,
                     'Zg' = NULL,
                     'Zv' = NULL,
                     'mean_depth' = NULL,
                     'lake_area' = NULL,
                     'surface_temp' = NULL,
                     'bottom_temp' = NULL, 
                     'therm_dep' = NULL)
for (id in unique(dt$LakeID)){
  print(paste0(match(id, unique(dt$LakeID)),' from ', length(unique(dt$LakeID))))
  
  df = dt %>%
    filter(LakeID == id)
  
  if (is.na(max(df$Lake_area))){
    next
  }
  
  hyps = approx.bathy(Zmax = max(df$Depth_m), lkeArea = max(df$Lake_area * 10^6), Zmean = max(df$Depth_avg), method = 'voldev')
  
  depth_raw = hyps$depths
  area_raw = hyps$Area.at.z
  
  dz = 0.1
  
  depth = seq(min(depth_raw), max(depth_raw), dz)
  area = approx(x = depth_raw, y = area_raw * 1e6, xout = depth)$y
  
  # depth = max(depth) - depth
  # area = rev(area)
  # 
  for (time in unique(df$Date)){
    
    dat = df %>%
      filter(Date == time) %>%
      arrange(Depth_m)
    
    if (all(is.na(dat$Temp_C))){
      next 
    }
    res = schmidt.stability_idso(wtr = dat$Temp_C, depths = dat$Depth_m, bthA = area, bthD = depth)
    
    bf = center.buoyancy(wtr = dat$Temp_C, depths = dat$Depth_m)
    
    results = rbind(results, data.frame('lake' = id,
                                        'Datetime' =  dat$Date[1],
                                        'St' = res$St,
                                        'mean_rho' = res$rho,
                                        'Zg' = res$z_g,
                                        'Zv' = res$z_v,
                                        'mean_depth' = mean(dat$Depth_avg),
                                        'lake_area' = max(dat$Lake_area * 10^6),
                                        'surface_temp' = dat$Temp_C[which(dat$Depth_m == min(dat$Depth_m))],
                                        'bottom_temp' = dat$Temp_C[which(dat$Depth_m == max(dat$Depth_m))],
                                        'therm_dep' = bf))
  }
  
}

write.csv(file = "data_processed/lewis.csv", x = results, quote = F, row.names = F)


