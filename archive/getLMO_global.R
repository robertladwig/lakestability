rm(list = ls())

library(tidyverse)
library(lubridate)
# library(RPostgreSQL)
# library(odbc)
library(zoo)
library(sf)
library(ncdf4)

# setwd('/Users/robertladwig/Documents/DSI/Limno_DataScience')
setwd('/Users/robertladwig/Documents/DSI/LMO/')

### ALL LAKES
# load shapefiles and calculate center of volume for all these lakes assuming a cone shape
# https://www.hydrosheds.org/pages/hydrolakes
lakes = st_read(file.path('HydroLAKES_points_v10_shp/HydroLAKES_points_v10_shp/HydroLAKES_points_v10.shp'))
ggplot(lakes) + geom_sf()

lakes$zv = rep(NA, nrow(lakes))
lakes$long = rep(NA, nrow(lakes))
lakes$lat = rep(NA, nrow(lakes))
for (i in 1:nrow(lakes)){
  print(paste0(i,'/',nrow(lakes)))
  dz = 0.1
  depths = 3 * lakes$Vol_total[i] / lakes$Lake_area[i]
  bthD = c(0, depths)
  bthA = c(lakes$Lake_area[i], 0)
  layerD = seq(0, depths, by=dz)
  layerA = stats::approx(bthD, bthA, layerD)$y
  Zcv <- layerD %*% layerA / sum(layerA)
  lakes$zv[i] = Zcv
  lakes$long[i] = lakes$Pour_long[i]
  lakes$lat[i] = lakes$Pour_lat[i]
}

df = data.frame('id'=lakes$Hylak_id, 
                'name' = lakes$Lake_name,
                'zv' = lakes$zv, 
                'long' = lakes$long, 
                'latl' = lakes$lat)
write.csv(df,  'lakes_zv.csv')#,row.names=FALSE,quote = FALSE)


lakes = st_read(file.path('HydroLAKES_points_v10_shp/HydroLAKES_points_v10_shp/HydroLAKES_points_v10.shp'))
lakes_df <- read_csv('lakes_zv_met.csv')

short_df <- lakes_df[, c("zv", 'swr90', 'wnd90', 'lmo90', 'swr20', 'wnd20', 'lmo20')]
lakes$zv = short_df$zv
lakes$swr90 = short_df$swr90
lakes$wnd90 = short_df$wnd90
lakes$lmo90 = short_df$lmo90
lakes$swr20 = short_df$swr20
lakes$wnd20 = short_df$wnd20
lakes$lmo20 = short_df$lmo20
lakes$lmozv90 = lakes$lmo90 / lakes$zv
lakes$lmozv20 = lakes$lmo20 / lakes$zv

lakes$diff <- (lakes$lmo20/lakes$zv) - (lakes$lmo90/lakes$zv)
lakes$distnull <- abs(lakes$diff) - 0
lakes$change <- ifelse(lakes$diff > 0, -1, 1)
lakes$change[which(lakes$diff < 0.1 & lakes$diff > -0.1)] <- 0
lakes$change = as.factor(lakes$change)

# Essentially it would be zmix = (gamma)^-1 + sqrt(gamma^-2 + C*L*LMO), where gamma 
# is extinction coeff, L is lake length, C = 0.0006 (dimensionless), 
# all units otherwise [m].

library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")

lakes$SSI20 <- 32.1 + (8143.7 - 32.1) * exp(-9.1 * lakes$lmozv20)
lakes$SSI90 <- 32.1 + (8143.7 - 32.1) * exp(-9.1 * lakes$lmozv90)

ggplot(lakes) + 
  geom_sf(data = world) +
  geom_sf(aes(col = (change), alpha = distnull)) +
  scale_fill_manual(values=c( "#E69F00","#999999", "#56B4E9"))+
  theme_bw()


library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")
lakes$scale_diff = scale(lakes$diff, center = 0, scale = FALSE)
lakes$scale_zv = (lakes$zv - min(lakes$zv)) / (max(lakes$zv) - min(lakes$zv))

ggplot(subset(lakes, SSI90 < 1000)) + 
  geom_sf(data = world) +
  geom_sf(aes(col = (SSI90), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="SSI 1990-2000") +
  theme_bw()

ggplot(lakes) + 
  geom_sf(data = world) +
  geom_sf(aes(col = (SSI20), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="SSI 2090-2100") +
  theme_bw()

ggplot(lakes) + 
  geom_sf(data = world) +
  geom_sf(aes(col = scale(zv), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="center of volume depth") +
  theme_bw()

ggplot(subset(lakes, scale_diff < 0 & scale_diff > -1.96)) + 
  geom_sf(data = world) +
  geom_sf(aes(col = abs(scale_diff), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Increase of future stability") +
  theme_bw()

ggplot(subset(lakes, scale_diff > 0 & scale_diff < 1.96)) + 
  geom_sf(data = world) +
  geom_sf(aes(col = scale_diff, alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Decrease of future stability") +
  theme_bw()

ggplot(lakes) + 
  geom_sf(data = world) +
  geom_sf(aes(col = log(wnd20/wnd90), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Log Change in wind shear stress") +
  theme_bw()

ggplot(lakes) + 
  geom_sf(data = world) +
  geom_sf(aes(col = log(swr20/swr90), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Log Change in global radiation") +
  theme_bw()

ggplot(lakes) + 
  geom_sf(data = world) +
  geom_sf(aes(col = log(scale_zv), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Scaled Log Center of volume depth") +
  theme_bw()

ggplot(lakes) +
  geom_point(aes(Pour_lat, log(lmozv20/lmozv90), col = log(wnd20/wnd90))) +
  geom_hline(yintercept = 0) +
  scale_color_viridis_c() +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red',
                        midpoint = 0)

ggplot(lakes) +
  geom_point(aes(Pour_lat, log(lmozv20/lmozv90), col = log(swr20/swr90))) +
  geom_hline(yintercept = 0) +
  scale_color_viridis_c() +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red',
                            midpoint = 0)

ggplot(lakes) +
  geom_point(aes(Pour_lat, log(lmo20/lmo90), col = log(swr20/swr90))) +
  geom_hline(yintercept = 0) +
  scale_color_viridis_c() +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red',
                        midpoint = 0)

g1=ggplot(lakes) +
  geom_point(aes(Pour_lat, SSI20, col = log(swr20/swr90))) +
  geom_hline(yintercept = 0) +
  scale_color_viridis_c() +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red',
                        midpoint = 0)
g2=ggplot(lakes) +
  geom_point(aes(Pour_lat, SSI90, col = log(swr20/swr90))) +
  geom_hline(yintercept = 0) +
  scale_color_viridis_c() +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red',
                        midpoint = 0)
g1/g2

g1=ggplot(lakes) +
  geom_point(aes(Pour_lat, SSI20)) +
  geom_hline(yintercept = 0) +
  scale_color_viridis_c() +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red',
                        midpoint = 0)
g2=ggplot(lakes) +
  geom_point(aes(Pour_lat, SSI90)) +
  geom_hline(yintercept = 0) +
  scale_color_viridis_c() +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red',
                        midpoint = 0)
g1/g2

ggplot(subset(lakes,Pour_long > -20 & Pour_long < 30 & Pour_lat > 45 & Pour_lat < 73)) +
  geom_point(aes(SSI90, zv))


ggplot(subset(lakes,Pour_long > -20 & Pour_long < 30 & Pour_lat > 45 & Pour_lat < 73)) +
  geom_point(aes(SSI90, lmo90))

ggplot(lakes) +
  geom_point(aes(lmo20/lmo90, lmozv20-lmozv90))

ggplot(lakes) +
  geom_point(aes(lmo20-lmo90, lmozv20-lmozv90))


(sum(lakes$diff < 0, na.rm = TRUE) * 100)/nrow(lakes)
(sum(lakes$diff > 0, na.rm = TRUE) * 100)/nrow(lakes)

# EUROPE

ggplot(lakes) + 
  geom_sf(data = world) +
  geom_sf(aes(col = SSI90)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="SSI 1990-2000") +
  theme_bw() +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE)

ggplot(subset(lakes, zv < 50)) + 
  geom_sf(data = world) +
  geom_sf(aes(col = zv)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Center of volume depth") +
  theme_bw() +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE)

ggplot(subset(lakes, scale_diff < 0 & scale_diff > -1.96)) + 
  geom_sf(data = world) +
  geom_sf(aes(col = abs(scale_diff), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Increase of future stability") +
  theme_bw() +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE)

ggplot(subset(lakes, scale_diff > 0 & scale_diff < 1.96)) + 
  geom_sf(data = world) +
  geom_sf(aes(col = scale_diff, alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Decrease of future stability") +
  theme_bw() +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE)

ggplot(lakes) + 
  geom_sf(data = world) +
  geom_sf(aes(col = log(wnd20/wnd90), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Log Change in wind shear stress") +
  theme_bw() +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE)

ggplot(lakes) + 
  geom_sf(data = world) +
  geom_sf(aes(col = log(swr20/swr90), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Log Change in global radiation") +
  theme_bw() +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE)

ggplot(lakes) + 
  geom_sf(data = world) +
  geom_sf(aes(col = log(scale_zv), alpha = distnull)) +
  scale_color_gradientn(colours = pal) + 
  labs(title="Scaled Log Center of volume depth") +
  theme_bw() +
  coord_sf(xlim = c(-20, 45), ylim = c(30, 73), expand = FALSE)


# get meteorology data
lakes_df <- read_csv('lakes_zv.csv')
# head(lakes_df)

lakes_df$swr90 = rep(NA, nrow(lakes_df))
lakes_df$wnd90 = rep(NA, nrow(lakes_df))
lakes_df$swr20 = rep(NA, nrow(lakes_df))
lakes_df$wnd20 = rep(NA, nrow(lakes_df))
lakes_df$lmo90 = rep(NA, nrow(lakes_df))
lakes_df$lmo20 = rep(NA, nrow(lakes_df))

out_fl_rsds90 <- 'mpi-esm1-2-hr_r1i1p1f1_w5e5_historical_rsds_global_daily_1991_2000.nc'
id_rsds90 <- nc_open(paste0('global/MLP/',out_fl_rsds90), readunlim=TRUE)
time_rsds90 <- ncvar_get(id_rsds90, 'time')
long_rsds90 <- ncvar_get(id_rsds90, 'lon')
lat_rsds90 <- ncvar_get(id_rsds90, 'lat')
rsds_rsds90 <- ncvar_get(id_rsds90, 'rsds')
nc_close(id_rsds90)

out_fl_wnd90 <- 'mpi-esm1-2-hr_r1i1p1f1_w5e5_historical_sfcwind_global_daily_1991_2000.nc'
id_wnd90 <- nc_open(paste0('global/MLP/',out_fl_wnd90), readunlim=TRUE)
time_wnd90 <- ncvar_get(id_wnd90, 'time')
long_wnd90 <- ncvar_get(id_wnd90, 'lon')
lat_wnd90 <- ncvar_get(id_wnd90, 'lat')
wnd_wnd90 <- ncvar_get(id_wnd90, 'sfcwind')
nc_close(id_wnd90)


out_fl_rsds20 <- 'mpi-esm1-2-hr_r1i1p1f1_w5e5_ssp585_rsds_global_daily_2091_2100.nc'
id_rsds20 <- nc_open(paste0('global/MLP/',out_fl_rsds20), readunlim=TRUE)
time_rsds20 <- ncvar_get(id_rsds20, 'time')
long_rsds20 <- ncvar_get(id_rsds20, 'lon')
lat_rsds20 <- ncvar_get(id_rsds20, 'lat')
rsds_rsds20 <- ncvar_get(id_rsds20, 'rsds')
nc_close(id_rsds20)

out_fl_wnd20 <- 'mpi-esm1-2-hr_r1i1p1f1_w5e5_ssp585_sfcwind_global_daily_2091_2100.nc'
id_wnd20 <- nc_open(paste0('global/MLP/',out_fl_wnd20), readunlim=TRUE)
time_wnd20 <- ncvar_get(id_wnd20, 'time')
long_wnd20 <- ncvar_get(id_wnd20, 'lon')
lat_wnd20 <- ncvar_get(id_wnd20, 'lat')
wnd_wnd20 <- ncvar_get(id_wnd20, 'sfcwind')
nc_close(id_wnd20)

get_mean <- function(long, lat, start, time, data){
  long_rsds90 <- (long - long_n)^2
  lake_lon_rsds90 <- which.min(long_rsds90)
  
  lat_quad_rsds90 <- (lat - lat_n)^2
  lake_lat_rsds90 <- which.min(lat_quad_rsds90)
  
  
  start_date_rsds90 <- start
  start_date_asDate_rsds90 <- as.POSIXct(sub(".*since ", "", start_date_rsds90))
  time_dates_rsds90 <- start_date_asDate_rsds90 + time * 24 *3600
  
  df_rsds_rsds90 <- data.frame('rsds' = data[lake_lon_rsds90, lake_lat_rsds90, ],
                               'time' = time_dates_rsds90)
  
  average <- df_rsds_rsds90 %>%
    mutate(doy = yday(time)) %>%
    mutate(year = year(time)) %>%
    dplyr::filter(doy >= 120 & doy <= 275) %>%
    group_by(year) %>%
    summarise(mean = mean(rsds, na.rm = TRUE))
  
  mean.average_rsds90 <- mean(average$mean)
  
  return(mean.average_rsds90)
}


# save.image(file='myEnvironment.RData')
# load('myEnvironment.RData')
lakes_df <- read_csv('lakes_zv_met.csv')

for (i in 1100001:nrow(lakes_df)){#nrow(lakes_df)){
  print(paste0(i,'/',nrow(lakes_df)))
  
  long_n <- lakes_df$long[i]
  lat_n <- lakes_df$latl[i]
  

  mean.average_rsds90 <- get_mean(long = long_rsds90, 
                                  lat = lat_rsds90,
                                  start = id_rsds90$dim$time$units,
                                  time = time_rsds90,
                                  data = rsds_rsds90)
  mean.average_wnd90 <- get_mean(long = long_wnd90, 
                                 lat = lat_wnd90,
                                 start = id_wnd90$dim$time$units,
                                 time = time_wnd90,
                                 data = wnd_wnd90)
  mean.average_rsds20 <- get_mean(long = long_rsds20, 
                                  lat = lat_rsds20,
                                  start = id_rsds20$dim$time$units,
                                  time = time_rsds20,
                                  data = rsds_rsds20)
  mean.average_wnd20 <- get_mean(long = long_wnd20, 
                                 lat = lat_wnd20,
                                 start = id_wnd20$dim$time$units,
                                 time = time_wnd20,
                                 data = wnd_wnd20)
  
  # 
  # 
  # long_n <- lakes_df$long[i]
  # lat_n <- lakes_df$latl[i]
  # 
  # long_rsds90 <- (long_rsds90 - long_n)^2
  # lake_lon_rsds90 <- which.min(long_quad_rsds90)
  # 
  # lat_quad_rsds90 <- (lat_rsds90 - lat_n)^2
  # lake_lat_rsds90 <- which.min(lat_quad_rsds90)
  # 
  # 
  # start_date_rsds90 <- id_rsds90$dim$time$units
  # start_date_asDate_rsds90 <- as.POSIXct(sub(".*since ", "", start_date_rsds90))
  # time_dates_rsds90 <- start_date_asDate_rsds90 + time_rsds90 * 24 *3600
  # 
  # df_rsds_rsds90 <- data.frame('rsds' = rsds_rsds90[lake_lon_rsds90, lake_lat_rsds90, ],
  #                              'time' = time_dates_rsds90)
  # 
  # average <- df_rsds_rsds90 %>%
  #   mutate(doy = yday(time)) %>%
  #   mutate(year = year(time)) %>%
  #   dplyr::filter(doy >= 120 & doy <= 275) %>%
  #   group_by(year) %>%
  #   summarise(mean = mean(rsds, na.rm = TRUE))
  # mean.average_rsds90 <- mean(average$mean)
  # 
  # 
  # long_quad_wnd90 <- (long_wnd90 - long_n)^2
  # lake_lon_wnd90 <- which.min(long_quad_wnd90)
  # 
  # lat_quad_wnd90 <- (lat_wnd90 - lat_n)^2
  # lake_lat_wnd90 <- which.min(lat_quad_wnd90)
  # 
  # 
  # start_date_wnd90 <- id_wnd90$dim$time$units
  # start_date_asDate_wnd90 <- as.POSIXct(sub(".*since ", "", start_date_wnd90))
  # time_dates_wnd90 <- start_date_asDate_wnd90 + time_wnd90 * 24 *3600
  # 
  # df_wnd_wnd90 <- data.frame('wnd' = wnd_wnd90[lake_lon_wnd90, lake_lat_wnd90, ],
  #                            'time' = time_dates_wnd90)
  # 
  # average <- df_wnd_wnd90 %>%
  #   mutate(doy = yday(time)) %>%
  #   mutate(year = year(time)) %>%
  #   dplyr::filter(doy >= 120 & doy <= 275) %>%
  #   group_by(year) %>%
  #   summarise(mean = mean(wnd, na.rm = TRUE))
  # mean.average_wnd90 <- mean(average$mean)
  # 
  # 
  # long_quad_rsds20 <- (long_rsds20 - long_n)^2
  # lake_lon_rsds20 <- which.min(long_quad_rsds20)
  # 
  # lat_quad_rsds20 <- (lat_rsds20 - lat_n)^2
  # lake_lat_rsds20 <- which.min(lat_quad_rsds20)
  # 
  # 
  # start_date_rsds20 <- id_rsds20$dim$time$units
  # start_date_asDate_rsds20 <- as.POSIXct(sub(".*since ", "", start_date_rsds20))
  # time_dates_rsds20 <- start_date_asDate_rsds20 + time_rsds20 * 24 *3600
  # 
  # df_rsds_rsds20 <- data.frame('rsds' = rsds_rsds20[lake_lon_rsds20, lake_lat_rsds20, ],
  #                              'time' = time_dates_rsds20)
  # 
  # average <- df_rsds_rsds20 %>%
  #   mutate(doy = yday(time)) %>%
  #   mutate(year = year(time)) %>%
  #   dplyr::filter(doy >= 120 & doy <= 275) %>%
  #   group_by(year) %>%
  #   summarise(mean = mean(rsds, na.rm = TRUE))
  # mean.average_rsds20 <- mean(average$mean)
  # 
  # 
  # long_quad_wnd20 <- (long_wnd20 - long_n)^2
  # lake_lon_wnd20 <- which.min(long_quad_wnd20)
  # 
  # lat_quad_wnd20 <- (lat_wnd20 - lat_n)^2
  # lake_lat_wnd20 <- which.min(lat_quad_wnd20)
  # 
  # 
  # start_date_wnd20 <- id_wnd20$dim$time$units
  # start_date_asDate_wnd20 <- as.POSIXct(sub(".*since ", "", start_date_wnd20))
  # time_dates_wnd20 <- start_date_asDate_wnd20 + time_wnd20 * 24 *3600
  # 
  # df_wnd_wnd20 <- data.frame('wnd' = wnd_wnd20[lake_lon_wnd20, lake_lat_wnd20, ],
  #                            'time' = time_dates_wnd20)
  # 
  # average <- df_wnd_wnd20 %>%
  #   mutate(doy = yday(time)) %>%
  #   mutate(year = year(time)) %>%
  #   dplyr::filter(doy >= 120 & doy <= 275) %>%
  #   group_by(year) %>%
  #   summarise(mean = mean(wnd, na.rm = TRUE))
  # mean.average_wnd20 <- mean(average$mean)
  # 
  # 
  # print(mean.average_rsds90)
  # print(mean.average_wnd90)
  # print(mean.average_rsds20)
  # print(mean.average_wnd20)
  
  lakes_df$swr90[i] = mean.average_rsds90
  lakes_df$wnd90[i] = mean.average_wnd90
  lakes_df$swr20[i] = mean.average_rsds20
  lakes_df$wnd20[i] = mean.average_wnd20
  
  ux90 <- 1.3 *10^(-3) * 1.43 * 10^(-3) * (mean.average_wnd90)^2
  ux20 <- 1.3 *10^(-3) * 1.43 * 10^(-3) * (mean.average_wnd20)^2
  
  jb90 <-  (207 * 10^(-6) * 9.81)/(4180 *1000) * mean.average_rsds90 / 1e3 * 1000
  jb20 <-  (207 * 10^(-6) * 9.81)/(4180 *1000) * mean.average_rsds20 / 1e3 * 1000
  lakes_df$lmo90[i] = sqrt(ux90)^3 /(jb90 *0.41)
  lakes_df$lmo20[i] = sqrt(ux20)^3 /(jb20 *0.41)
}
write.csv(lakes_df,  'lakes_zv_met.csv')#,row.names=FALSE,quote = FALSE)
