rm(list = ls())

library(tidyverse)
library(lubridate)
library(zoo)
library(sf)
library(ncdf4)
library(rnaturalearth)
library(rnaturalearthdata)
library(wesanderson)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


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

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(lakes) +
  geom_sf(data = world) +
  geom_sf(aes(col = log10(Vol_total))) +
  # scale_fill_manual(values=c( "#E69F00","#999999", "#56B4E9"))+
  scale_color_gradientn(colours = pal) +
  theme_minimal()

pal <- wes_palette("Zissou1", 100, type = "continuous")
lakes$scale_diff = scale(lakes$diff, center = 0, scale = FALSE)
lakes$scale_zv = (lakes$zv - min(lakes$zv)) / (max(lakes$zv) - min(lakes$zv))

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

(sum(lakes$diff < 0, na.rm = TRUE) * 100)/nrow(lakes)
(sum(lakes$diff > 0, na.rm = TRUE) * 100)/nrow(lakes)
