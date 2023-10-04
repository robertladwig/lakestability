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

ggplot(results %>% filter(lake == sample(results$lake, size = 10))) +
  geom_point(aes(Zg - Zv, St)) +
  facet_wrap(~ lake, ncol = 5, scales = 'free')

ggplot(results %>% filter(lake == sample(results$lake, size = 10))) +
  geom_point(aes(mean_rho, St)) +
  facet_wrap(~ lake, ncol = 5, scales = 'free')


cluster_data = data.frame('lake' = NULL,
                          'mean_depth' = NULL,
                          'lake_area' = NULL,
                          'slope' = NULL,
                          'max_St' = NULL,
                          'mean_St' = NULL,
                          'min_St' = NULL,
                          'tempDiff' = NULL)

for (i in unique(results$lake)){
  
  dat = results %>%
    filter(lake == i) %>%
    mutate(ZgZv = Zg - Zv)
  
  sum = summary(lm(St ~ ZgZv, data = dat))
  
  cluster_data = rbind(cluster_data, data.frame('lake' = i,
                            'mean_depth' = mean(dat$mean_depth),
                            'lake_area' = max(dat$lake_area),
                            'slope' = sum$coefficients[2],
                       'max_St' = max(dat$St, na.rm = T),
                       'mean_St' = mean(dat$St, na.rm = T),
                       'min_St' = min(dat$St, na.rm = T),
                       'tempDiff' = abs(water.density(mean(dat$surface_temp, na.rm=T)) - water.density(mean(dat$bottom_temp,na.rm=T)))))
}
plot(cluster_data$max_St, cluster_data$slope)
plot(cluster_data$tempDiff, cluster_data$slope)

cluster_data = cluster_data %>%
  mutate(osgood = mean_depth / sqrt(lake_area/10^6))

summary(lm(slope ~ mean_depth + lake_area + osgood, data = cluster_data))
summary(lm(mean_St ~ slope, data = cluster_data))
summary(lm(mean_St ~ mean_depth, data = cluster_data))
summary(lm(max_St ~ mean_depth, data = cluster_data))
summary(lm(mean_St ~ slope , data = cluster_data))

rsq = 0
for (i in seq(50,1000, 50)){
  sum = summary(lm(slope ~ mean_depth, data = cluster_data %>% filter(max_St > i)))
  if ( sum$adj.r.squared >= rsq){
    rsq =  sum$adj.r.squared
    intv = i
  }
 
}

rsq = 0
for (i in seq(1e-2,3, 1e-2)){
  sum = summary(lm(slope ~ mean_depth, data = cluster_data %>% filter(tempDiff > i)))
  if ( sum$adj.r.squared >= rsq){
    rsq =  sum$adj.r.squared
    intv = i
  }
  
}


ggplot(cluster_data %>% mutate(flag =ifelse(osgood <= 6, 1, 0)) %>% filter(max_St > 100), aes(mean_depth, slope)) + geom_point(aes(mean_depth, slope, col = log10(mean_St))) +
  geom_smooth(method='lm', formula= y~x)


ggplot(cluster_data %>% mutate(flag =ifelse(osgood <= 6, 1, 0)) %>% filter(tempDiff > 0.56), aes(mean_depth, slope)) + geom_point(aes(mean_depth, slope, col = log10(mean_St))) +
  geom_smooth(method='lm', formula= y~x)

summary(lm(slope ~ mean_depth , data = cluster_data %>% filter(max_St > 100) ))
summary(lm(mean_St ~ slope , data = cluster_data))


ggplot(cluster_data) + geom_point(aes(lake_area, slope, col = mean_depth))
ggplot(cluster_data %>% mutate(flag =ifelse(osgood <= 6, 1, 0)), aes(mean_depth, slope)) + geom_point(aes(mean_depth, slope, col = as.factor(flag))) +
  geom_smooth(method='lm', formula= y~x)
ggplot(cluster_data) + geom_point(aes(mean_depth, min_St, col = lake_area))

dat_nor = na.omit(data.frame('lake' = cluster_data$lake,
                     'mean_depth' = as.matrix(as.data.frame(scale(cluster_data[, 2:7])))[,1],
                     'lake_area' = as.matrix(as.data.frame(scale(cluster_data[, 2:7])))[,2],
                     'slope' = as.matrix(as.data.frame(scale(cluster_data[, 2:7])))[,3],
                     'max_St' = as.matrix(as.data.frame(scale(cluster_data[, 2:7])))[,4],
                     'mean_St' = as.matrix(as.data.frame(scale(cluster_data[, 2:7])))[,5],
                     'min_St' = as.matrix(as.data.frame(scale(cluster_data[, 2:7])))[,6]) )

nor = na.omit(as.matrix(as.data.frame(scale(cluster_data[, 2:7]))))

data.pca <- princomp(nor)
summary(data.pca)

fviz_pca_var(data.pca, col.var = "black")

avg_sil <- function(k) {
  km.res <- kmeans(nor, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(nor))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
g.sil <- fviz_nbclust(nor, kmeans, method = "silhouette")
print(g.sil)

dist = dist(nor,  method= 'euclidean')
mydata.hclust = hclust(dist, method = 'ward.D')
plot(mydata.hclust)
groups <- cutree(mydata.hclust, k=2) #k=5) # cut tree into 7 clusters

rect.hclust(mydata.hclust, k=2,border='red')

dat_nor$groups = groups

results_clust = merge(results, dat_nor, by = 'lake')

ggplot(results_clust) +
  geom_point(aes(mean_rho, Zg-Zv, col = log10(St))) +
  facet_wrap(~ as.factor(groups), scales = 'free')

ggplot(results_clust) +
  geom_density(aes(mean_depth.x)) +
  facet_wrap(~ as.factor(groups))

ggplot(results_clust) +
  geom_density(aes(St)) +
  facet_wrap(~ as.factor(groups))
