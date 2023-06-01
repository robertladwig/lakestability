# GET LAKE DATA FROM PILLA ET AL. (2022)
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

# info <- read_csv(file = '../data/hypothesis_data.csv')
# 
# 
# all.dne <- list.files('../analysis_output/')
# all.dne <- str_remove(all.dne, '.csv')
# 
# all_results <- c()
# for (filename in all.dne){
#   if (filename %in% c('109502', "54902", "206548")){
#     next
#   }
#   df <- read_csv(paste0('../analysis_output/', filename, '.csv'))
#   all_results <- rbind(all_results, df)
# }
# 
# all_results <- all_results %>%
#   rename(LakeID = lake)
# 
# info$LakeID <- as.character(match(info$LakeName, info$LakeName))
# info_merge <- info %>% select(LakeID, Latitude, Longitude, Elevation_m, SurfaceArea_km2, Volume_km3, MaxDepth_m, MeanDepth_m, Secchi_m) %>%
#   distinct()
# 
# 
# df <- merge(all_results, info_merge, by = 'LakeID') %>%
#   mutate(lmozv = lmo/zv) #%>%
#  # filter(st < 5000 & lmozv < 10)

df <- read_csv(file = '../data/processed_data.csv')

df <- df %>%
  filter(lmo <= MaxDepth_m & zg <= MaxDepth_m) %>%
  mutate(
    length = sqrt((3 * Volume_km3 * 1e9) / (pi * MaxDepth_m)),
    z_crit = 0.493 * Secchi_m  + sqrt(0.493**2 * Secchi_m**2 + 0.0006 * length * lmo),
    zg_zv = zg / zv,
    zcrit_meanz = z_crit / MeanDepth_m,
    SecchitoVolume = Secchi_m/zv)




z1 <- ggplot(df ) +
  geom_point(aes(MaxDepth_m, zg/zv, col = log10(st))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  xlab("Max. depth (m)") + ylab("gravity depth:volume depth (m)")+ labs(colour = 'Stability (J/m2)') +
  scale_color_gradientn(colors = met.brewer("Benedictus", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

z2 <- ggplot(df ) +
  geom_point(aes(zg/zv,st, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  xlab("gravity depth:volume depth (m)") + ylab("Stability (J/m2)")+ labs(colour = "Max. depth (m)") +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()


z3 <- ggplot(df ) +
  geom_point(aes(zg/zv,z_crit/MeanDepth_m, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  xlab("gravity depth:volume depth (m)") + ylab("critical depth:mean depth (m)")+ labs(colour = "Max. depth (m)") +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  geom_hline(yintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

z4 <- ggplot(df ) +
  geom_point(aes(z_crit/MeanDepth_m,st,col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  xlab("critical depth:mean depth (m)") + ylab("Stability (J/m2)")+ labs(colour = "Max. depth (m)") +
  # xlim(0, 1) + ylim(0, 10000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()


z5 <- ggplot(df ) +
  geom_point(aes(zg, Secchi_m/zv, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  xlab("gravity depth (m)") + ylab("secchi depth:volume depth (m)")+ labs(colour = "Max. depth (m)") +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

z6 <- ggplot(df ) +
  geom_point(aes(zg, z_crit/MeanDepth_m, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  xlab("gravity depth (m)") + ylab("critical depth:mean depth (m)")+ labs(colour = "Max. depth (m)") +
  xlim(0, 100) + #ylim(0, 10000) +
  geom_hline(yintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()



z_all <- (z1 + z2) / (z3 + z4) / (z5 + z6)  + plot_annotation(tag_levels = 'A')

ggplot(df ) +
  geom_point(aes(zg/zv, meta_lower - meta_upper, col = scale(surf_temp))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  xlab("gravity depth (m)") + ylab("critical depth:mean depth (m)")+ labs(colour = "Max. depth (m)") +
  # xlim(0, 100) + #ylim(0, 10000) +
  geom_hline(yintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

