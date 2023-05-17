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

info <- read_csv(file = '../data/hypothesis_data.csv')


all.dne <- list.files('../analysis_output/')
all.dne <- str_remove(all.dne, '.csv')

all_results <- c()
for (filename in all.dne){
  if (filename %in% c('109502', "54902")){
    next
  }
  df <- read_csv(paste0('../analysis_output/', filename, '.csv'))
  all_results <- rbind(all_results, df)
}

all_results <- all_results %>%
  rename(LakeID = lake)

info$LakeID <- as.character(match(info$LakeName, info$LakeName))
info_merge <- info %>% select(LakeID, Latitude, Longitude, Elevation_m, SurfaceArea_km2, Volume_km3, MaxDepth_m, MeanDepth_m, Secchi_m) %>%
  distinct()


df <- merge(all_results, info_merge, by = 'LakeID') %>%
  mutate(lmozv = lmo/zv) %>%
  filter(st < 5000 & lmozv < 10) 

# https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
fit <- nls(st ~ SSasymp(lmozv, yf, y0, log_alpha), data = df)
yf = round(259.262,1)#round(6.0743,1)#round(32.059,1)#round(19.667,1)
y0 = round( 899.239,1)#round(332.1759,1)#round(8143.702,1)#round(4004.553,1)
alpha = round(exp(  1.305 ),1)#round(exp(0.9106),1)#round(exp(2.209),1)#round(exp(2.138),1)
summary(fit)
predicted2 <- yf * (y0 - yf) * exp(- ((alpha)) * (seq(min(df$lmozv), max(df$lmozv), 0.1)))
plot(predicted2)
pred.data = augment(fit)
plot(augment(fit)$lmozv,augment(fit)$.fitted)
predicted2 = augment(fit)$.fitted

exponential.model <- lm(log(st)~ lmozv, data = df)
summary(exponential.model)
coef(exponential.model)
timevalues <- seq(min(df$lmozv), max(df$lmozv), 0.1)
Counts.exponential2 <- exp(predict(exponential.model,list(lmozv=timevalues)))
predicted_st = data.frame('st' = Counts.exponential2, 'lmozv' = timevalues)

nmae = (1/nrow(pred.data) * sum(abs(pred.data$.fitted - pred.data$st))) /
  (max(pred.data$st) - min(pred.data$st))
rmse = sqrt((1/nrow(pred.data) * sum((pred.data$.fitted - pred.data$st)^2)))
nse = 1 - (sum((pred.data$.fitted - pred.data$st)^2))/(sum((pred.data$st - mean(pred.data$st,na.rm = TRUE))^2))

  
ggplot(df) +
  geom_point(aes(lmozv, st, col = scale(MaxDepth_m))) +
  geom_line(data = predicted_st, aes(lmozv, st)) +
  xlim(0,10) + ylim(0, 1000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()
