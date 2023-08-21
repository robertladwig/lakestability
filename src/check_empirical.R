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
  mutate(MeanDepth_m = ifelse(is.na(MeanDepth_m), Volume_km3/SurfaceArea_km2 * 1000, MeanDepth_m)) %>%
  mutate(
    length = sqrt((3 * Volume_km3 * 1e9) / (pi * MaxDepth_m)),
    z_crit = 0.493 * Secchi_m  + sqrt(0.493**2 * Secchi_m**2 + 0.0006 * length * lmo),
    zg_zv = zg / zv,
    zcrit_meanz = z_crit / MeanDepth_m,
    SecchitoVolume = Secchi_m/zv)

df$depthCat <- cut(df$MaxDepth_m, breaks = c(0,10,50,150,1000,Inf), labels = c("Shallow","Medium","Deep", "Deeper", "Super deep"))

df_info <- data.frame('LakeID' = NULL, 
                      'Slope' = NULL,
                      'r2' = NULL,
                      'sd' = NULL,
                      'max' = NULL,
                      'min' = NULL,
                      cv = NULL,
                      Latitude = NULL,
                      Longitude = NULL,
                      Elevation_m = NULL,
                      SurfaceArea_km2 = NULL,
                      Volume_km3 = NULL,
                      MaxDepth_m = NULL,
                      MeanDepth_m = NULL,
                      Secchi_m = NULL)
for (i in unique(df$LakeID)){
  data = df %>%
    filter(LakeID == i) %>%
    mutate(diff = zg -zv)
  
  model <- summary(lm(st ~ diff, data = data))
  coef(lm(st ~ diff, data = data))
  # plot(data$diff, data$st)
  
  df_info <- rbind(df_info, data.frame('LakeID' = mean(data$LakeID), 
                        'Slope' = model$coefficients[2],
                        'r2' = model$r.squared,
                        'sd' = sd(data$zg - data$zv),
                        'max' = max(data$zg - data$zv),
                        'min' = min(data$zg - data$zv),
                        cv = mean(data$zv),
                        Latitude = mean(data$Latitude),
                        Longitude = mean(data$Longitude),
                        Elevation_m = mean(data$Elevation_m),
                        SurfaceArea_km2 = mean(data$SurfaceArea_km2),
                        Volume_km3 = mean(data$Volume_km3),
                        MaxDepth_m = mean(data$MaxDepth_m),
                        MeanDepth_m = mean(data$MeanDepth_m),
                        Secchi_m = mean(data$Secchi_m)))
}
df_info$depthCat <- cut(df_info$MaxDepth_m, breaks = c(0,10,50,150,1000,Inf), labels = c("Shallow","Medium","Deep", "Deeper", "Super deep"))

df_info$Slope[1] * mean(df_info$max)
df_info$Slope[1] * mean(df_info$min)
df_info$Slope[1] * (mean(df_info$max) - mean(df_info$min))

proj = df_info %>%
  group_by(LakeID) %>%
  summarise(st = Slope * (max -min))

obs = df %>%
  group_by(LakeID) %>%
  summarise(max = max(st),
            min = min(st))

ggplot(df %>%
  group_by(LakeID) %>%
  summarise(st = mean(st),
            MeanDepth_m = mean(MeanDepth_m),
            MaxDepth_m = mean(MaxDepth_m))) +
  geom_point(aes(MeanDepth_m, st))

plot(proj$st, obs$max)
plot(proj$st, obs$min)

ggplot(df_info, aes(max-min)) +
  geom_histogram()

ggplot(df_info) +
  geom_point(aes(MaxDepth_m, Slope)) 
ggplot(df_info) +
  geom_point(aes(MeanDepth_m, Slope, col = r2))
ggplot(df_info) +
  geom_point(aes(MeanDepth_m, max-min, col = r2))


ggplot(df_info) +
  geom_point(aes(cv, Slope, col = r2))
ggplot(df_info) +
  geom_point(aes(sd, Slope, col = r2))
ggplot(df_info) +
  geom_point(aes(MeanDepth_m, sd, col = r2))
ggplot(df_info) +
  geom_point(aes(max, Slope, col = r2))
ggplot(df_info) +
  geom_point(aes(MeanDepth_m, max, col = r2))
ggplot(df_info) +
  geom_point(aes(Longitude, Slope))
ggplot(df_info) +
  geom_point(aes(r2, Slope))
ggplot(df_info) +
  geom_point(aes(MeanDepth_m, Slope, col = log10(MaxDepth_m))) +
  facet_wrap(~ depthCat, scales = 'free')

df_info %>%
  filter(r2 < 0.9)

ggplot(df %>% filter(LakeID %in% c('130702', '216303', '247476', '253347', '255435', '271662', '403286'))) +
  geom_point(aes(zg-zv, st, col = (MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  xlab("gravity depth - volume depth (m)") + ylab("Stability (J/m2)")+ labs(colour = 'Max. depth (m)') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  facet_wrap(~ LakeID, scales = 'free') +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(MaxDepth_m, st, col = log10(st))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  xlab("Max. depth (m)") + ylab("gravity depth:volume depth (m)")+ labs(colour = 'Stability (J/m2)') +
  scale_color_gradientn(colors = met.brewer("Benedictus", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(zg-zv, st, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  xlab("gravity depth - volume depth (m)") + ylab("Stability (J/m2)")+ labs(colour = 'Max. depth (m)') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  facet_wrap(~ depthCat, scales = 'free') +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

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

ggplot(df %>% filter(LakeID < 100000) ) +
  geom_point(aes(zg/zv,st, col = (MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(1, 1.10) + ylim(0, 1000) +
  xlab("gravity depth:volume depth (m)") + ylab("Stability (J/m2)")+ labs(colour = "Max. depth (m)") +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  facet_wrap(~ LakeID, scales = 'free') +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df %>% filter(LakeID < 100000) ) +
  geom_point(aes( z_crit/MeanDepth_m,st, col = (MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(1, 1.10) + ylim(0, 1000) +
  xlab("gravity depth:volume depth (m)") + ylab("Stability (J/m2)")+ labs(colour = "Max. depth (m)") +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  facet_wrap(~ LakeID, scales = 'free') +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df  %>% filter(LakeID < 500000) ) +
  geom_point(aes((zg/zv), z_crit/MeanDepth_m )) + #Secchi_m/zv  z_crit/MeanDepth_m
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  xlab("gravity depth (m)") + ylab("critical depth:mean depth (m)")+ labs(colour = "Max. depth (m)") +
  # xlim(0, 100) + #ylim(0, 10000) +
  # geom_hline(yintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  facet_wrap(~ LakeID, scales = 'free') +
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

