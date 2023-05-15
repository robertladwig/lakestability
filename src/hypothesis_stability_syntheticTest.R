setwd(dirname(rstudioapi::getSourceEditorContext()$path))


library(dplyr)
library(ggplot2)
library(rLakeAnalyzer)
library(patchwork)

schmidt.stability_schmidt = function(wtr, depths, bthA, bthD, sal = 0){
  
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
  
  # Here is just some madeup data. This should 
  # seem valid to the Schmidt Stability algorithm. Valid enough at least
  #wtr = c(24,24,24,20,17,12,11,10,10)
  #depths = 1:9
  #sal = wtr*0
  #bthD = 1:9
  #bthA = seq(8,0,by=-1)
  
  # if bathymetry has negative values, drop and interpolate to 0
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
  
  Zcv <- layerD %*% layerA / sum(layerA)
  St <- (layerP) %*% ((layerD - as.vector(Zcv)) * layerA) * dz * g / Ao
  
  St_perLayer <- (layerP) * ((layerD - as.vector(Zcv)) * layerA) 
  
  St_perLayer = approx(layerD, St_perLayer, orig_depths)$y

  
  return(data.frame('St' = St, 'z_v' = Zcv,
                    'St_perLayer' =St_perLayer))
  
}

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
  mean_density_depth <- approx(layerP, layerD, mean_density)$y
  
  Zcv <- layerD %*% layerA / sum(layerA)
  Zg <- (layerP %*% (layerD * layerA)) / sum(layerA %*% layerP)
  
  St <- (layerP - as.vector(mean_density)) %*% ((layerD - as.vector(mean_density_depth)) * layerA) * dz * g / Ao
  St_perLayer <- (layerP - as.vector(mean_density)) * ((layerD - as.vector(mean_density_depth)) * layerA) #* dz 
  
  St_perLayer = approx(layerD, St_perLayer, orig_depths)$y
  
  return(data.frame('St' = St, 'z_g' = mean_density_depth,
         'St_perLayer' =St_perLayer, 'Zg' = Zg))
  
}

analysis <- data.frame('zmixzv' = NULL,
                       'metadepth' =NULL,
                       'st_28' = NULL,
                       'st_73' = NULL)

max_depth <- 35
mean_depth = 8

for (mixing_depth in seq(2,30)){
    
  # for (meta_thickness in seq(1)){ #2,20

    meta_thickness = 4#floor(seq(2,30)*0.2 +2) #0.5 * mixing_depth
    
  
    temp_profile <- c(seq(25.5,25.0, length.out = floor(mixing_depth)), seq(25, 10, length.out = meta_thickness))
    temp_profile <- c(temp_profile, seq(10, 9.8, length.out =(max_depth+1) - length(temp_profile)))
    
    dens_profile <- water.density(temp_profile)
    
    area = seq(4e6, 1e-6, length.out = max_depth)
    
    bath_profile <- approx.bathy(Zmax = max_depth, lkeArea = max(area), Zmean = mean_depth, method = 'voldev', zinterval = 1)
    
    area_profile <- bath_profile$Area.at.z # seq(4e6, 1e-6, length.out = max_depth)
    depth_profile <- bath_profile$depths # seq(1, max_depth)
    
    z_v <- (area_profile %*% depth_profile) / sum(area_profile)
    
    
    st <- schmidt.stability_schmidt(wtr = temp_profile, depths = depth_profile, bthA = area_profile, bthD = depth_profile)
    st_idso <- schmidt.stability_idso(wtr = temp_profile, 
                                      depths = depth_profile, 
                                      bthA = area_profile, 
                                      bthD = depth_profile)
    
    
    df <- data.frame('depth' = depth_profile,
                     'density' = dens_profile,
                     'energy_idso' = st_idso$St_perLayer,
                     'energy_schmidt' = st$St_perLayer)
    
    
    # sml < z_v: St increases
    # sml > z_v: St decreases
    g1 <- ggplot() +
      geom_point(data = df, aes(density, depth)) +
      geom_hline(yintercept = z_v) +
      geom_hline(yintercept = mean(st_idso$z_g),  linetype = 'dashed') +
      # geom_hline(yintercept = mean(st$z_v),  linetype = 'dashed') +
      geom_hline(yintercept = mixing_depth,  linetype = 'dotted') +
      ggtitle(paste0('St(28): ', floor(mean(st$St)), ', St(73): ', floor(mean(st_idso$St)), ', zmix: ', mixing_depth)) + 
      scale_y_continuous(trans = "reverse") + 
      theme_minimal()
    
    g2 <- ggplot() +
      geom_point(data = df, aes(energy_idso, depth)) +
      geom_hline(yintercept = z_v) +
      geom_hline(yintercept = mean(st_idso$z_g),  linetype = 'dashed') +
      # geom_hline(yintercept = mean(st$z_v),  linetype = 'dashed') +
      geom_hline(yintercept = mixing_depth,  linetype = 'dotted') +
      # ggtitle('idso') +
      scale_y_continuous(trans = "reverse") + 
      theme_minimal()
    
    g3 <- ggplot() +
      geom_point(data = df, aes((energy_schmidt), depth)) +
      geom_hline(yintercept = z_v) +
      geom_hline(yintercept = mean(st_idso$z_g),  linetype = 'dashed') +
      geom_hline(yintercept = mixing_depth,  linetype = 'dotted') +
      geom_vline(xintercept = 0,  linetype = 'solid') +
      # scale_x_continuous(trans='log10') +
      # ggtitle('schmidt') +
      scale_y_continuous(trans = "reverse") + 
      theme_minimal()
    

    
    g <-  g1 + g2 + g3; g
    ggsave(paste0('../figs/',mixing_depth,'_', meta_thickness,'.png'), g)
    
    analysis <- rbind(analysis, data.frame('zmixzv' = mixing_depth/mean(st$z_v), 'metadepth' =meta_thickness,
                                           'st_28' = mean(st$St), 'st_73' = mean(st_idso$St)))  
  # }
  
}


g1 <- ggplot(analysis) +
  geom_point(aes(zmixzv, st_28, size = as.factor(metadepth))) +
  # geom_point(aes(zmixzv, st_73, col ='1973', size = as.factor(metadepth))) +
  theme_minimal()

g2 <- ggplot(analysis) +
  # geom_point(aes(zmixzv, st_28, col ='1928', size = as.factor(metadepth))) +
  geom_line(aes(zmixzv, st_73, size = as.factor(metadepth), col = as.factor(metadepth))) +
  theme_minimal()

ggplot(analysis) +
  # geom_point(aes(zmixzv, st_28, col ='1928', size = as.factor(metadepth))) +
  geom_line(aes(zmixzv, c(0, diff(st_73)), size = as.factor(metadepth), col = as.factor(metadepth))) +
  theme_minimal()

g2
ggsave(paste0('../figs/all_runs.png'), g2)

# g2 + theme(legend.position="none") + xlab('')+ ylab('')
# ggsave(paste0('../figs/test.png'))
