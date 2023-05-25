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
  filter(lmo <= MaxDepth_m & zg <= MaxDepth_m)


# https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
fit <- nls(st ~ SSasymp((lmo/MaxDepth_m)/zv, yf, y0, log_alpha), data = df, algorithm = "port")
yf = round(281.993,1)#round(6.0743,1)#round(32.059,1)#round(19.667,1)
y0 = round( 922.190 ,1)#round(332.1759,1)#round(8143.702,1)#round(4004.553,1)
alpha = round(exp(   1.538  ),1)#round(exp(0.9106),1)#round(exp(2.209),1)#round(exp(2.138),1)
summary(fit)
predicted2 <- yf * (y0 - yf) * exp(- ((alpha)) * (seq(min(df$lmozv), max(df$lmozv), 0.1)))
plot(predicted2)
pred.data = augment(fit)
plot((augment(fit)$lmo/augment(fit)$MaxDepth_m)/ augment(fit)$zv,augment(fit)$.fitted)
predicted2 = augment(fit)$.fitted

exponential.model <- lm(log(st) ~ lmozv, data = df)
summary(exponential.model)

exponential.model <- lm(log(st) ~ zv, data = df)
summary(exponential.model)

exponential.model <- lm(log(st) ~ MaxDepth_m, data = df)
summary(exponential.model)

exponential.model <- lm(log(st) ~ lmo, data = df)
summary(exponential.model)


coef(exponential.model)
timevalues <- seq(min(df$lmozv), max(df$lmozv), 0.1)
Counts.exponential2 <- exp(predict(exponential.model,list(lmo=timevalues)))
predicted_st = data.frame('st' = Counts.exponential2, 'lmozv' = timevalues)

nmae = (1/nrow(pred.data) * sum(abs(pred.data$.fitted - pred.data$st))) /
  (max(pred.data$st) - min(pred.data$st))
rmse = sqrt((1/nrow(pred.data) * sum((pred.data$.fitted - pred.data$st)^2)))
nse = 1 - (sum((pred.data$.fitted - pred.data$st)^2))/(sum((pred.data$st - mean(pred.data$st,na.rm = TRUE))^2))

length(unique(df$LakeID))

ggplot(df %>% group_by(LakeID) %>% filter(st == max(st))) +
  geom_point(aes(lmozv, st, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,10) + ylim(0, 10000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df %>% mutate(dist = (abs(lmozv - 1)))  %>% group_by(LakeID) %>% filter(dist == min(dist))) +
  geom_point(aes(lmozv, st, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,10) + ylim(0, 10000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()
  
ggplot(df ) +
  geom_point(aes(lmozv, st, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  xlim(0,10) + ylim(0, 1000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(zp/zv, st, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0.99, 1.12) + ylim(0, 100000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(zg/zv, st, col = (surf_temp))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  xlim(0.99, 1.12) + ylim(0, 100000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

summary(lm(zg ~ lmo/zv, data = df))

summary(lm(zg ~ lmo + surf_temp, data = df))

ggplot(df ) +
  geom_point(aes((lmo/MaxDepth_m)/zv, st, col = log10(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  xlim(0, 1) + ylim(0, 10000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(lmo, st, col = log10(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()


#### START

df = df %>%
  mutate(SecchitoVolume = Secchi_m/zv)
ggplot(df ) +
  geom_point(aes(MaxDepth_m, zg/zv, col = log10(st), size = scale(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(st, zg/zv, col = log10(MaxDepth_m), size = scale(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(zg, Secchi_m/zv, col = log10(MaxDepth_m), size = scale(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

model <- lm((zg) ~ (Secchi_m/zv), data = df)
model <- lm(log10(zg) ~ SecchitoVolume, data = df)
model <- lm(log10(zg) ~ log10(SecchitoVolume), data = df)
model <- lm(formula = (zg) ~ pred, data = df %>% mutate(pred = Secchi_m/zv))
# model <- lm((zg) ~ Secchi_m/zv, data = df)
summary(model)
coef(model)

secchi <- seq(min(df$Secchi_m, na.rm = T), max(df$Secchi_m, na.rm = T), 0.1)
zv <- seq(min(df$zv, na.rm = T), max(df$zv, na.rm = T), length.out = length(secchi))
ratio <- seq(min(df$Secchi_m/df$zv, na.rm = T), max(df$Secchi_m/df$zv, na.rm = T), length.out = length(secchi))
Counts.exponential2 <- (predict(model,list("Secchi_m"=secchi, "zv" = zv)))
predicted_st = data.frame('zg' = Counts.exponential2, 'SecchimTozv' = ratio)

ggplot(df ) +
  geom_point(aes(Secchi_m/zv,  zg, col = log10(st), size = scale(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()


ggplot(df ) +
  geom_point(aes(Secchi_m,  zg, col = log10(st), size = scale(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()


ggplot(df ) +
  geom_point(aes(Secchi_m/zv, st, col = log10(st), size = scale(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()


ggplot(df ) +
  geom_point(aes((Secchi_m/zv)/zv, st, col = log10(st), size = scale(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()



ggplot(df ) +
  geom_point(aes(MaxDepth_m, zg/zv, col = log10(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()


ggplot(df ) +
  geom_point(aes( (207 * 10^(-6) * 9.81)/(4180 * water.density(surf_temp)) * swr / 1e3 * 1000, zg/zv, col = log10(lmo))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0, 1) + ylim(0, 10000) +
  # geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes((lmo/MaxDepth_m)/zv, .fitted), col = 'red') +
  theme_minimal()

library(Boruta)
df_red <- df %>%
  select(st, zv, lmo, wind, swr, Latitude, Longitude, Elevation_m, SurfaceArea_km2, Volume_km3, MaxDepth_m, MeanDepth_m, Secchi_m,
         Secchi_m, zg, zp, meta_upper, meta_lower, thermo_dep, surf_temp)
sc.info <- scale(df_red)
# df.data.spiny = df.red %>%
# mutate(Spiny = (ifelse(Spiny > 0, 1, 0)))
df.data <- na.omit(as.data.frame(scale(df_red)))
# df.data$Spiny = df.data.spiny$Spiny
boruta_output <- Boruta(zg ~ .,
                        data = df.data, doTrace=2,
                        maxRuns = 1e5)  # perform Boruta search
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
plot(boruta_output, cex.axis=1.5, las=3, xlab="", main="")  # plot variable importance

final.boruta <- TentativeRoughFix(boruta_output)
print(final.boruta)
plot(final.boruta)
boruta.df <- attStats(final.boruta)
boruta_signif =getSelectedAttributes(final.boruta, withTentative = F)
print(boruta.df)
print(boruta_signif)

summary(lm(zg ~ surf_temp + Secchi_m  + swr + lmo, data = df))
summary(lm(zg ~ surf_temp + Secchi_m/zv  + swr + lmo, data = df))
summary(lm(zg ~ Secchi_m/zv , data = df))



summary(lm(log10(st) ~ zg/zv, data = df))
summary(lm(log10(st) ~ zv, data = df))
summary(lm(log10(zg/zv) ~ lmo, data = df))

summary(lm((zg) ~ lmo/MaxDepth_m, data = df))

summary(lm(log10(st) ~ (lmo/MaxDepth_m)/zv, data = df))

ggplot(df ) +
  geom_point(aes(log10(zg/zv), log10(st), col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,100) +
  # ylim(0, 10000) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  labs(x = 'Mixing:Volume depth', y = 'Stability') +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(log10(zg), log10(lmo/zv), col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,100) +
  # ylim(0, 10000) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  labs(x = 'Mixing:Volume depth', y = 'Stability') +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()


summary(lm(log10(zg) ~ lmo/thermo_dep, data = df))

ggplot(df ) +
  geom_point(aes(zg, lmo/zv, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,10) + ylim(0, 10000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(lmo * zv/MaxDepth_m, st, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,10) + ylim(0, 10000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(lmo / (zv*MaxDepth_m), st, col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,10) + ylim(0, 10000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(1/zv, st, col = log10(MaxDepth_m))) +
  xlim(0,0.5) + ylim(0, 1000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(1/MaxDepth_m, st, col = log10(MaxDepth_m))) +
  xlim(0,0.5) + ylim(0, 1000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(lmo, st, col = log10(MaxDepth_m))) +
  xlim(0,10) + ylim(0, 1000) +
  geom_vline(xintercept = 1.0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(log10(lmozv), log10(st), col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,100) +
  # ylim(0, 10000) +
  geom_vline(xintercept = 10^0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  labs(x = 'Mixing:Volume depth', y = 'Stability') +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(log10(1/MaxDepth_m), log10(st), col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,100) +
  # ylim(0, 10000) +
  geom_vline(xintercept = 10^0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  labs(x = 'Mixing:Volume depth', y = 'Stability') +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(log10(1/zv), log10(st), col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,100) +
  # ylim(0, 10000) +
  geom_vline(xintercept = 10^0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  labs(x = 'Mixing:Volume depth', y = 'Stability') +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(log10(lmo), log10(st), col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,100) +
  # ylim(0, 10000) +
  geom_vline(xintercept = 10^0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  labs(x = 'Mixing:Volume depth', y = 'Stability') +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()

ggplot(df ) +
  geom_point(aes(log10(lmo / (zv*MaxDepth_m)), log10(st), col = log10(MaxDepth_m))) +
  # geom_line(data = predicted_st, aes(lmozv, st)) +
  # xlim(0,100) +
  # ylim(0, 10000) +
  geom_vline(xintercept = 10^0, linetype = 'dashed') +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  labs(x = 'Mixing:Volume depth', y = 'Stability') +
  # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
  theme_minimal()


plot_df <- c()
for (lakename in unique(df$LakeID)){
  plot_df <- rbind(plot_df, df %>% dplyr::filter(LakeID == lakename))
  ggplot(plot_df ) +
    geom_point(aes(lmozv, st, col = log10(MaxDepth_m))) +
    # geom_line(data = predicted_st, aes(lmozv, st)) +
    xlim(0,10) + ylim(0, 1000) +
    geom_vline(xintercept = 1.0, linetype = 'dashed') +
    scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
    # geom_line(data = augment(fit), aes(lmozv, .fitted), col = 'red') +
    # facet_wrap(~ LakeID) +
    theme_minimal()
  ggsave(paste0('../figs/analysis_pilla/', lakename, '.png'))
  
}
