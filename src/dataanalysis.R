library(tidyverse)
library(glmtools)
library(lubridate)
library(rLakeAnalyzer)
library(factoextra)
library(cluster)

df_ntl = read.csv('data_processed/ntl.csv') #%>%
 # mutate('factor' = as.numeric(as.factor(lake)))
df_lewis = read.csv('data_processed/lewis.csv')# %>%
 # mutate('factor' = as.numeric(as.factor(lake)))
df_pilla = read.csv('data_processed/pilla.csv')#%>%
 # mutate('factor' = as.numeric(as.factor(lake)))

df = rbind(df_ntl, df_lewis, df_pilla)

nrow(df_ntl) + nrow(df_lewis) + nrow(df_pilla) == nrow(df)

##############
df = df %>%
  mutate(ZgZv = Zg - Zv)

df_scale = df %>%
  group_by(lake) %>%
  mutate(scale_ZgZv = (ZgZv - min(ZgZv, na.rm = T)) / (max(ZgZv, na.rm = T) - min(ZgZv, na.rm = T)),
         scale_mean_rho =  (mean_rho - min(mean_rho, na.rm = T)) / (max(mean_rho, na.rm = T) - min(mean_rho, na.rm = T)),
         scale_St =  (St - min(St, na.rm = T)) / (max(St, na.rm = T) - min(St, na.rm = T)),
         scale_therm_dep =  (therm_dep - min(therm_dep, na.rm = T)) / (max(therm_dep, na.rm = T) - min(therm_dep, na.rm = T)))

interp_scaled = data.frame("Lake" = NULL,'time' = NULL, "St" = NULL,
                           "mean_rho" = NULL,
                           'ZgZv' = NULL,
                           'therm_dep' = NULL)

for (id in unique(df_scale$lake)){
  data = df_scale %>% filter(lake == id) %>%
    mutate(doy = yday(Datetime)) %>%
    # arrange(scale_St)
    arrange(doy)
  
  if (nrow(data) <= 1){
    next
  }
  
  St_scale = seq(0, 1, 0.01)
  time_scale = seq(0, 365, 1)
  mean_rho = approx(x = data$doy, y = data$scale_mean_rho, xout = time_scale, rule = 2)$y
  ZgZv = approx(x = data$doy, y = data$scale_ZgZv, xout = time_scale, rule = 2)$y
  St = approx(x = data$doy, y = data$scale_St, xout = time_scale, rule = 2)$y
  therm_dep = approx(x = data$doy, y = data$scale_therm_dep, xout = time_scale, rule = 2)$y
  
  interp_scaled = rbind(interp_scaled, data.frame("Lake" = id, 'time' = time_scale, "St" = St,
                             "mean_rho" = mean_rho,
                             'ZgZv' = ZgZv,
                             'therm_dep' = therm_dep))
}

## plot 1: mean response over year
ggplot(df_scale) +
  geom_point(aes(scale_mean_rho, 
                 scale_ZgZv,
                 group = lake,
                 color = scale_St), alpha = 0.1) +
  geom_path(data = interp_scaled %>% group_by(time) %>%summarise(mean_mean_rho = mean(mean_rho),
                                                                mean_ZgZv = mean(ZgZv),
                                                                mean_St = mean(St)),
            aes(mean_mean_rho, mean_ZgZv, col = mean_St), linewidth = 3)

ggplot(df_scale) +
  geom_point(aes(scale_therm_dep, 
                 scale_ZgZv,
                 group = lake,
                 color = scale_St), alpha = 0.1) +
  geom_path(data = interp_scaled %>% group_by(time) %>%summarise(mean_mean_rho = mean(mean_rho),
                                                                 mean_ZgZv = mean(ZgZv),
                                                                 mean_St = mean(St),
                                                                 mean_therm_dep = mean(therm_dep)),
            aes(mean_therm_dep, mean_ZgZv, col = mean_St), linewidth = 3)
## plot 1: mean response over year
ggplot(df %>% mutate(scale_mean_depth = (mean_depth - min(mean_depth, na.rm = T)) / (max(mean_depth, na.rm = T) - min(mean_depth, na.rm = T))))+
  geom_point(aes(Zg - Zv, St, col = scale_mean_depth)) 

ggplot(df %>% mutate(scale_mean_depth = (mean_depth - min(mean_depth, na.rm = T)) / (max(mean_depth, na.rm = T) - min(mean_depth, na.rm = T))))+
  geom_point(aes(mean_rho, St, col = scale_mean_depth)) 

ggplot(df %>% mutate(scale_mean_depth = (mean_depth - min(mean_depth, na.rm = T)) / (max(mean_depth, na.rm = T) - min(mean_depth, na.rm = T))))+
  geom_point(aes(Zv, Zg, col = scale_mean_depth)) 

ggplot(df %>% mutate(scale_mean_depth = (mean_depth - min(mean_depth, na.rm = T)) / (max(mean_depth, na.rm = T) - min(mean_depth, na.rm = T))))+
  geom_point(aes(Zg - Zv, therm_dep, col = mean_depth)) 

ggplot(df %>% filter(lake == sample(df$lake, size = 10))) +
  geom_point(aes(Zg - Zv, St)) +
  facet_wrap(~ lake, ncol = 5, scales = 'free')

ggplot(df %>% filter(lake == sample(df$lake, size = 10))) +
  geom_point(aes(mean_rho, St)) +
  facet_wrap(~ lake, ncol = 5, scales = 'free')

ggplot(df %>% filter(lake == sample(df$lake, size = 10))) +
  geom_point(aes(Zg - Zv, mean_rho)) +
  facet_wrap(~ lake, ncol = 5, scales = 'free')


cluster_data = data.frame('lake' = NULL,
                          'mean_depth' = NULL,
                          'lake_area' = NULL,
                          'slope' = NULL,
                          'intercept' = NULL,
                          'max_St' = NULL,
                          'mean_St' = NULL,
                          'min_St' = NULL,
                          'tempDiff' = NULL)

overall_p <- function(my_model) {
  f <- (my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

for (i in unique(df$lake)){
  
  dat = df %>%
    filter(lake == i) %>%
    mutate(ZgZv = Zg - Zv)
  
  if (nrow(dat) <= 2){
    next
  }
  
  sum = summary(lm(St ~ ZgZv, data = dat))
  
  if (overall_p(sum) <= 0.05){
    intercept = sum$coefficients[1]
    slope = sum$coefficients[2]
  } else {
    slope = NA
    intercept = NA
  }
  
  cluster_data = rbind(cluster_data, data.frame('lake' = i,
                                                'mean_depth' = mean(dat$mean_depth),
                                                'lake_area' = max(dat$lake_area),
                                                'slope' = slope,
                                                'intercept' = intercept,
                                                'max_St' = max(dat$St, na.rm = T),
                                                'mean_St' = mean(dat$St, na.rm = T),
                                                'min_St' = min(dat$St, na.rm = T),
                                                'tempDiff' = abs(water.density(mean(dat$surface_temp, na.rm=T)) - water.density(mean(dat$bottom_temp,na.rm=T)))))
}

cluster_data = cluster_data %>%
  mutate(osgood = mean_depth / sqrt(lake_area/10^6))

rsq = 0
for (i in seq(10,1000, 10)){
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

ggplot(cluster_data %>% mutate(flag =ifelse(osgood <= 6, 1, 0)) ,aes(max_St, slope)) +
  geom_point(aes(col = (flag))) +
  geom_smooth(method='lm', formula= y~x)

ggplot(cluster_data %>% mutate(flag =ifelse(osgood <= 6, 1, 0)) %>% filter(max_St < 1e4) ,aes(max_St, slope)) +
  geom_point(aes(col = (flag))) +
  geom_smooth(method='lm', formula= y~x)

ggplot(cluster_data %>% mutate(flag =ifelse(osgood <= 6, 1, 0)) %>% filter(max_St < 1e4 & mean_depth < 100 & osgood <10) ,aes(osgood, slope)) +
  geom_point(aes(col = (mean_depth))) +
  geom_smooth(method='lm', formula= y~x)

ggplot(cluster_data %>% mutate(flag =ifelse(osgood <= 6, 1, 0)) %>% filter(max_St < 1e4 & mean_depth < 50 & osgood <10 & lake_area < 2e9) ,aes(mean_depth, slope)) +
  geom_point(aes(col = (mean_depth))) +
  geom_smooth(method='lm', formula= y~x)

ggplot(cluster_data %>% mutate(flag =ifelse(osgood <= 6, 1, 0)) ,aes(slope, intercept)) +
  geom_point(aes(col = (max_St))) +
  geom_smooth(method='lm', formula= y~x)

ggplot(cluster_data %>% mutate(flag =ifelse(osgood <= 6, 1, 0)) ,aes(max_St, slope)) +
  geom_point(aes(size = scale(osgood), color = as.factor(flag))) + xlim(0,6e4) + ylim(0, 2.5e6) +
  geom_smooth(method='lm', formula= y~x)

summary(lm(max_St ~ slope, data = cluster_data %>% filter(osgood > 6)))
summary(lm(max_St ~ slope, data = cluster_data ))

ggplot(cluster_data %>% mutate(flag =ifelse(osgood <= 6, 1, 0)) ,aes(mean_depth, slope)) +
  geom_point(aes(col = (flag))) +
  geom_smooth(method='lm', formula= y~x)

ggplot(cluster_data %>% filter(max_St >= 650),aes(mean_depth, slope)) +
  geom_point(aes(col = log10(mean_St))) +
  geom_smooth(method='lm', formula= y~x)

summary(lm(slope ~ mean_depth, data = cluster_data))
summary(lm(slope ~ mean_depth, data = cluster_data %>% filter(max_St >= 650)))

summary(lm(mean_St ~ slope, data = cluster_data %>% filter(max_St >= 650)))
summary(lm(mean_St ~ slope, data = cluster_data %>% filter(max_St < 1e4)))
summary(lm(mean_St ~ slope + intercept, data = cluster_data %>% filter(max_St >= 650)))

fit <- lm(slope ~ mean_depth, data = cluster_data)# fit the model
cluster_data$predicted <- predict(fit)   # Save the predicted values
cluster_data$residuals <- residuals(fit) # Save the residual values

ggplot(cluster_data, aes(mean_depth, slope)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = mean_depth, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

ggplot(cluster_data) +
  geom_point(aes(residuals, log10(lake_area)))
ggplot(cluster_data) +
  geom_point(aes(residuals, scale(mean_depth)))
ggplot(cluster_data) +
  geom_point(aes(residuals, (osgood)))
ggplot(cluster_data) +
  geom_point(aes(residuals, log10(max_St)))
ggplot(cluster_data) +
  geom_point(aes(residuals, log10(min_St)))
ggplot(cluster_data) +
  geom_point(aes(residuals, log10(mean_St)))

# https://rpubs.com/iabrady/residual-analysis
plot(fit, which=2, col=c("red"))  # Q-Q Plot
plot(fit, which=3, col=c("blue"))  # Scale-Location Plot
plot(fit, which=5, col=c("blue"))  # Residuals vs Leverage

cluster_redux = cluster_data %>% filter(max_St >= 650)
cluster_redux = cluster_data %>% filter(osgood >= 6)
fit <- lm(slope ~ mean_depth, data = cluster_redux)# fit the model
cluster_redux$predicted <- predict(fit)   # Save the predicted values
cluster_redux$residuals <- residuals(fit) # Save the residual values

fit_St <- lm(max_St ~ predicted, data = cluster_redux)# fit the model
cluster_data$predicted <- predict(fit)   # Save the predicted values
cluster_data$residuals <- residuals(fit) # Save the residual values
