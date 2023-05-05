rm(list = ls())

library(rLakeAnalyzer)
library(tidyverse)
library(lubridate)
library(glmtools)
library(viridis)
library(patchwork)
library(RPostgreSQL)
library(odbc)
library(berryFunctions)
library(broom)
library(zoo)
library(signal)
library(Hmisc)



# setwd('/Users/robertladwig/Documents/DSI/Limno_DataScience')
setwd('/Users/robertladwig/Documents/DSI/LMO/')


all_ids <- read.csv('nhdids.csv')
lks = all_ids$nhdid # sample(all_ids$nhdid, 100)
df_hyps = data.frame('id' = NULL, 'area' = NULL, 'depth' = NULL)
for (ii in lks){
  eg_nml <- read_nml(paste0('../pball_nml/pball_nml_files/pball_',ii,'.nml'))
  H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
  A <- eg_nml$morphometry$A
  df_hyps <- rbind(df_hyps, data.frame('id' = ii,
                                       'area' = A,
                                       'depth' = H))
}

model = lm(area ~ depth +factor(id) - 1, df_hyps)
summary(model)
ggplot(df_hyps, aes(log(area),depth, col = id)) +
  geom_line() +
  scale_y_continuous(trans = "reverse") +
  theme_minimal() +
  theme(legend.position = "none") 

water.density <- function(wtr, sal = wtr*0){

  if(length(wtr) != length(sal)){
    stop('water temperature array must be the same length as the salinity array')
  }

  # Determine which method we want to use, initially set both methods to false
  MM = FALSE; # Martin & McCutcheon
  UN = FALSE; # UNESCO


  # specify temperature and salinity range for the calculations
  Trng <- c(0,40) # temperature range
  Srng <- c(0.5,43) # salinity range


  # check to see if all values lie within the ranges specified
  if ( all(sal < Srng[1], na.rm=TRUE) ){
    MM <- TRUE # use Martin & McCutcheon
  }else if (!(sum(wtr<Trng[1], na.rm=TRUE) || sum(wtr>Trng[2], na.rm=TRUE)) &&
            !(sum(sal<Srng[1], na.rm=TRUE)) || sum(sal>Srng[2], na.rm=TRUE)){
    UN <- TRUE # use UNESCO
  }


  # if MM is true we use method of Martin and McCutcheon (1999)
  if (MM){
    rho <- (1000*(1-(wtr+288.9414)*(wtr-3.9863)^2/(508929.2*(wtr+68.12963))))
  }

  # if UN is true we use method of Martin and McCutcheon (1999)
  if (UN){
    # -- equation 1:
    rho_0 <- 999.842594 + 6.793952*10^(-2)*wtr - 9.095290*10^(-3)*wtr^2 + 1.001685*10^(-4)*wtr^3 - 1.120083*10^(-6)*wtr^4 + 6.536335e-9*wtr^5

    # -- equation 2:
    A <- 8.24493*10^(-1) - 4.0899e-3*wtr + 7.6438*10^(-5)*wtr^2 - 8.2467*10^(-7)*wtr^3 + 5.3875*10^(-9)*wtr^4

    # -- equation 3:
    B <- -5.72466*10^(-3) + 1.0227*10^(-4)*wtr - 1.6546*10^(-6)*wtr^2

    # -- equation 4:
    C <- 4.8314*10^(-4)

    # -- equation 5:
    rho <- rho_0 + A*sal + B*sal^(3/2) + C*sal^2
  }

  # if there is a combination of fresh and saline water we need to use a combination of MM and UN
  if (MM == FALSE && UN == FALSE){
    rho <- wtr*0
    for (j in 1:length(rho)){
      rho[j] <- water.density(wtr[j],sal[j])
    }
    dim(rho) <- dim(wtr) # ensure same dimension as input array
  }
  return(rho)
}

ssi.distribution = function(wtr, depths, bthA, bthD, sal = 0){

  if(length(wtr) != length(depths)){
    stop('water temperature array must be the same length as the depth array')
  }

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

  Zcv <- layerD %*% layerA / sum(layerA)
  St <- layerP * ((layerD - as.vector(Zcv)) * layerA) * dz

  return(data.frame('layerD' =  layerD, 'Zcv' = rep(Zcv, length(St)), 'St' = St))
}

### OBSERVED DATA
odbc::odbcListDrivers()
# drv = dbDriver("PostgreSQL Driver")
# drv = odbc::odbc()
drv = dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "UW_data_science",
                 host = '144.92.62.199', port = 5432,
                 user = "postgres", password = 'SparklingRusty')

mydata <- dbGetQuery(con,"select * from data.observation_temps", stringsAsFactors = FALSE)
str(mydata)

lookup_nhd_metfile <- dbGetQuery(con,'select * from data.nhd_met_relation', stringsAsFactors = FALSE)
met_data <- dbGetQuery(con,"select * from data.met_input_data where meteofile in (select distinct(meteo_filename) from data.nhd_met_relation where nhdid = 'nhdhr_120018459')",
                       stringsAsFactors = FALSE)
str(met_data)


all_ids <- read.csv('nhdids.csv')
lks = all_ids$nhdid # sample(all_ids$nhdid, 100)
# lks <- list.dirs(path = 'inst/extdata/', full.names = TRUE, recursive = F)
a=1
for (ii in lks){
  print(paste0('Running ',ii,' / ',round((a*100)/length(lks),1),'%'))
  
  mydata_test <- subset(mydata, site_id == ii)
  # mydata_test <- dbGetQuery(con,paste("select * from data.predicted_temps_calibrated where nhd_lake_id = '",ii,"'",sep = ''), stringsAsFactors = FALSE)
  met_data_test <- dbGetQuery(con,paste("select * from data.met_input_data where meteofile in (select distinct(meteo_filename) from
                                        data.nhd_met_relation where nhdid = '",ii,"')",sep = ""), stringsAsFactors = FALSE)
  
  mydata_test <- mydata_test[order(mydata_test$date, mydata_test$depth),]
  
  # mydata_test = dbGetQuery(con,"select * from data.predicted_temps_calibrated where nhd_lake_id = 'nhdhr_120018459'", stringsAsFactors = FALSE)
  
  len.data <- c()
  for (ki in sort(unique(mydata_test$date))){
    yi <- which(mydata_test$date == ki)
    len.data <- rbind(len.data, length(yi))
  }
  
  if (all(len.data ==1 )){
    a =a +1
  } else {
    deps = seq(min(mydata_test$depth), max(mydata_test$depth), 0.5)
    tst = sort(unique(mydata_test$date))
    tst = tst[len.data > 1]
    
    p.data <- c()
    for (ki in tst){
      yi <- which(mydata_test$date == ki)
      wdat = (mydata_test$temp[yi])
      wdep = (mydata_test$depth[yi])
      if (max((mydata_test$depth[yi])) < max(deps)){
        wdat <- c(wdat, max(wdat))
        wdep <- c(wdep, max(deps))
      }
      if (min((mydata_test$depth[yi])) > min(deps)){
        wdat <- c(min(wdat), wdat)
        wdep <- c(min(deps), wdep)
      }
      intdat <- approx(wdep, wdat, deps)$y
      p.data <- rbind(p.data, rev(intdat))
    }
    
    print('Finished reversing the wtemp data')
    
    data <- as.data.frame(cbind(rep(1, length(unique(tst))), p.data))
    colnames(data) <- c('date',paste0('temp_',deps))
    data$date = as.Date(sort(tst))
    
    meteo <- met_data_test[order(met_data_test$time),]
    # data <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'pball', include.dirs = T)))
    # meteo <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'NLDAS', include.dirs = T)))
    
    eg_nml <- read_nml(paste0('../pball_nml/pball_nml_files/pball_',ii,'.nml'))
    H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
    A <- eg_nml$morphometry$A
    Lmax <- max(eg_nml$morphometry$bsn_len, eg_nml$morphometry$bsn_wid)
    # Lmax <- 2*sqrt(max(A)/pi)
    
    ssi <- rep(NA, length(data$date))
    data.wtr <- as.matrix(data[,-c(1)])
    strdep <- gsub("[a-zA-Z ]", "", colnames(data))
    strdep <- gsub("_", "", strdep)
    strdep <-  as.numeric(strdep[-1])
    for (ji in 1:length(data$date)){
      # error <- try(schmidt.stability(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A),bthD = rev(H)))
      if (is.error(ssi[ji] <- schmidt.stability(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A),bthD = rev(H)))){
        ssi[ji] = NA
      } else {
        ssi[ji] <- schmidt.stability(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A),bthD = rev(H))
      }
    }
    
    wind.ts <- data.frame('datetime' = meteo$time, 'u10' = meteo$WindSpeed)
    wind.ts$ux <- 1.3 *10^(-3) * 1.43 * 10^(-3) * (wind.ts$u10)^2
    wind.ts$sw <- meteo$ShortWave # 1 W = kg m2 / s3 # kg = 1000 kg/m3 - m5 / s3 m2
    wind.ts$year <- year(wind.ts$datetime)
    wind.ts$doy = yday(wind.ts$datetime)
    wind.ts$ux_corr = sqrt(wind.ts$ux) * (1-exp(-0.002 * Lmax))
    
    annual.wind.ts <- wind.ts %>%
      dplyr::filter(doy >= 120 & doy <= 275) %>%
      group_by(year) %>%
      summarise(mean.ux = mean(sqrt(ux), na.rm = TRUE),
                mean.js = mean(sw, na.rm = TRUE))
    # summarise(mean.ux = mean((ux_corr), na.rm = TRUE),
    #           mean.js = mean(sw, na.rm = TRUE))
    
    annual.wind.ts$Jb <-  (207 * 10^(-6) * 9.81)/(4180 *1000) * annual.wind.ts$mean.js / 1e3 * 1000
    annual.wind.ts$Lmo <- annual.wind.ts$mean.ux^3 /(annual.wind.ts$Jb *0.41)# m3/s3 / w/m2
    
    
    E.schmidt<- data.frame('time' = data$date, 'St' = ssi)
    dep <- strdep
    App <- stats::approx(rev(H), rev(A), dep)$y
    zv <- dep %*% App / sum(App, na.rm = TRUE)
    if (is.na(zv)){
      zv <- dep[!is.na(App)] %*% App[!is.na(App)] / sum(App, na.rm = TRUE)
    }
    
    
    E.schmidt$year = year(E.schmidt$time)
    E.schmidt$doy = yday(E.schmidt$time)
    annual.E.schmidt <- E.schmidt %>%
      dplyr::filter(doy >= 120 & doy <= 275) %>%
      group_by(year) %>%
      summarise(mean.st = mean(St, na.rm = TRUE))
    
    id.yr <- match(annual.E.schmidt$year, annual.wind.ts$year)
    idy.yr <- match(annual.wind.ts$year,annual.E.schmidt$year)
    annual.wind.ts = annual.wind.ts[na.omit(id.yr),]
    annual.E.schmidt = annual.E.schmidt[na.omit(idy.yr),]
    
    if (a==1){
      lmo.df <- data.frame('year' = unique(annual.E.schmidt$year),
                           'lmoz' = annual.wind.ts$Lmo/zv,
                           'lmo' = annual.wind.ts$Lmo,
                           'st' = annual.E.schmidt$mean.st,
                           'id' = sub("\\).*", "", sub(".*\\(", "", ii)),
                           'z' = rep(max(H),length(unique(annual.E.schmidt$year))))
      
    } else {
      lmo.df <- rbind(lmo.df, data.frame('year' = unique(annual.E.schmidt$year),
                                         'lmoz' = annual.wind.ts$Lmo/zv,
                                         'lmo' = annual.wind.ts$Lmo,
                                         'st' = annual.E.schmidt$mean.st,
                                         'id' = sub("\\).*", "", sub(".*\\(", "", ii)),
                                         'z' = rep(max(H),length(unique(annual.E.schmidt$year)))))
    }

  a=a+1
  }
}

write_delim(lmo.df, path = 'pball_observed.csv',delim = "\t")#,row.names=FALSE,quote = FALSE)

lmo.df <- read_delim('pball_observed.csv', delim = '\t')
lmo.df_corr <- read_delim('pball_observed_fetchcorrected.csv', delim = '\t')

df_corr = data.frame('id' = lmo.df$id, 'z' = lmo.df$z, 'area' = lmo.df$A, 'lmo' = lmo.df$lmo,
                     'lmo_corr' = lmo.df_corr$lmo)

h1=ggplot(lmo.df) +
  geom_point(aes(z, ((lmoz)^(-1)*lmo), col = (z-((lmoz)^(-1)*lmo))))+
  xlab('max depth (m)') + ylab('center of volume depth (m)') +
  scale_color_continuous(type = "viridis") +
  labs(color='Center of volume depth (m)') +
  theme_minimal()

h2=ggplot(lmo.df) +
  geom_density(aes(z))+
  xlab('max depth (m)') +
  theme_minimal()

h3=ggplot(df_corr, aes(lmo, lmo_corr, col = (z))) +
  geom_point()+
  xlab('Lmo (m)') + ylab('fetch-corrected Lmo (m)') +
  scale_color_continuous(type = "viridis") +
  labs(color='Max. depth (m)') +
  theme_minimal()

g <- h1/h2/h3; g
ggsave(file=paste0('fetch_correction.png'), g, dpi = 300,width = 6.65,height = 9.5, units = 'in')# ,width = 584,height = 316, units = 'mm'
###

lmo.df$A = NULL
lmo.df$maxz = NULL
all_ids <- read.csv('nhdids.csv')
lks = all_ids$nhdid # sample(all_ids$nhdid, 100)
df = data.frame('id' = NULL, 'A' = NULL, 'maxz' = NULL)
for (ii in lks){
  eg_nml <- read_nml(paste0('../pball_nml/pball_nml_files/pball_',ii,'.nml'))
  H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
  A <- eg_nml$morphometry$A
  df = rbind(df, data.frame('id' = ii,
                            'A' = max(A),
                            'maxz' = max(H)))
  idy = match(ii, lmo.df$id)
  lmo.df$A[idy] = max(A)
  lmo.df$maxz[idy] = max(H)
}


unique(lmo.df$id[which(lmo.df$st < 0)])

ggplot(lmo.df, aes(lmoz, (st), col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  labs(color='Max. depth (m)') +
  theme_minimal()

cut.lmo.df <- lmo.df[which(lmo.df$st > 0),]
cut.cut.lmo.df <-  cut.lmo.df[which(cut.lmo.df$z > ((cut.lmo.df$lmoz)^(-1)*cut.lmo.df$lmo)),]
# cut.lmo.df <- lmo.df
# cut.lmo.df$st[which(cut.lmo.df$st < 0)] = 0

# https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
fit <- nls(st ~ SSasymp(lmoz, yf, y0, log_alpha), data = cut.cut.lmo.df)
yf = round(32.059,1)#round(6.0743,1)#round(32.059,1)#round(19.667,1)
y0 = round(8143.702,1)#round(332.1759,1)#round(8143.702,1)#round(4004.553,1)
alpha = round(exp(2.209),1)#round(exp(0.9106),1)#round(exp(2.209),1)#round(exp(2.138),1)
summary(fit)
predicted2 <- yf * (y0 - yf) * exp(- ((alpha)) * (seq(min(cut.lmo.df$lmoz), max(cut.lmo.df$lmoz), 0.1)))
plot(predicted2)
pred.data = augment(fit)
plot(augment(fit)$lmoz,augment(fit)$.fitted)
predicted2 = augment(fit)$.fitted

nmae = (1/nrow(pred.data) * sum(abs(pred.data$.fitted - pred.data$st))) /
  (max(pred.data$st) - min(pred.data$st))
rmse = sqrt((1/nrow(pred.data) * sum((pred.data$.fitted - pred.data$st)^2)))
nse = 1 - (sum((pred.data$.fitted - pred.data$st)^2))/(sum((pred.data$st - mean(pred.data$st,na.rm = TRUE))^2))

model <- lm(log(st) ~ lmoz, data = cut.lmo.df)
fit2 <- nls(log(st) ~ SSasymp(lmoz, yf, y0, log_alpha), data = cut.lmo.df)
g0 <- ggplot(cut.lmo.df, aes(lmoz, log(st), col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  # geom_line(data = augment(model), aes(lmoz, .fitted), col ='red')+
  # geom_line(data = augment(fit), aes(lmoz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  # annotate(geom="label", x=18, y=2800, label=paste0('SSI ~ ',yf,' + (',y0,' - ',yf,') exp(-',alpha,' * Lmoz)')) +
  # ggtitle(paste0('nmae=',round(nmae,3),', rmse=',round(rmse,1),'Jm-2, nse=',round(nse,2),' (n=',nrow(cut.lmo.df),')')) +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  labs(color='Max. depth (m)') +
  theme_minimal(); g0

model <- lm((st) ~ log(lmoz), data = cut.cut.lmo.df)
timevalues <- seq(min((cut.cut.lmo.df$lmoz)), max((cut.cut.lmo.df$lmoz)), 0.1)
exp(7.3-5.82* timevalues)
Counts.exponential2 <- (predict(model,list(lmoz=log(timevalues))))
predicted <- exp(predict(model,list(lmoz=cut.cut.lmo.df$lmoz),interval = "confidence"))
plot(log(cut.cut.lmo.df$lmoz), cut.cut.lmo.df$st,pch=16,
     xlab = 'Lmo/zv', ylab = 'SSI')
lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")


library(MASS)
library(fit.models)
fm1 <- fit.models(c("rlm", "lm"), log(st) ~ lmoz, data = cut.cut.lmo.df)

ggplot(cut.cut.lmo.df, aes(lmoz, st, col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  geom_line(data = augment(fit), aes(lmoz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  annotate(geom="label", x=18, y=2800, label=paste0('SSI ~ ',yf,' + (',y0,' - ',yf,') exp(-',alpha,' * Lmoz)')) +
  ggtitle(paste0('nmae=',round(nmae,3),', rmse=',round(rmse,1),'Jm-2, nse=',round(nse,2),' (n=',nrow(cut.lmo.df),')')) +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  labs(color='Max. depth (m)') +
  xlim(0,2)+
  theme_minimal()

ggplot(cut.cut.lmo.df) +
  geom_density(aes(st))

ggplot(cut.cut.lmo.df, aes(scale(lmoz), st^(-1), col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  # geom_line(data = augment(fit), aes(lmoz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  # annotate(geom="label", x=18, y=2800, label=paste0('SSI ~ ',yf,' + (',y0,' - ',yf,') exp(-',alpha,' * Lmoz)')) +
  ggtitle(paste0('nmae=',round(nmae,3),', rmse=',round(rmse,1),'Jm-2, nse=',round(nse,2),' (n=',nrow(cut.lmo.df),')')) +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  labs(color='Max. depth (m)') +
  # xlim(0,2)+
  theme_minimal()

g1 <- ggplot(cut.cut.lmo.df, aes(lmoz, st, col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  geom_line(data = augment(fit), aes(lmoz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  annotate(geom="label", x=18, y=2800, label=paste0('SSI ~ ',yf,' + (',y0,' - ',yf,') exp(-',alpha,' * Lmoz)')) +
  ggtitle(paste0('nmae=',round(nmae,3),', rmse=',round(rmse,1),'Jm-2, nse=',round(nse,2),' (n=',nrow(cut.lmo.df),')')) +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  labs(color='Max. depth (m)') +
  theme_minimal(); g1
ggsave(file=paste0('fig/Fig_ExpDecModel_pball.eps'), g1, dpi = 300,width = 6,height = 6, units = 'in')
ggsave(file=paste0('fig/Fig_ExpDecModel_pball.png'), g1, dpi = 300,width = 6,height = 6, units = 'in')

g2 <- ggplot(cut.lmo.df, aes(lmo, st, col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  # geom_line(data = augment(fit), aes(lmoz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  # annotate(geom="label", x=18, y=2800, label=paste0('SSI ~ ',yf,' + (',y0,' - ',yf,') exp(-',alpha,' * Lmoz)')) +
  # ggtitle(paste0('nmae=',round(nmae,3),', rmse=',round(rmse,1),'Jm-2, nse=',round(nse,2),' (n=',nrow(cut.lmo.df),')')) +
  xlab('Average annual Monin-Obukhov length') +
  ylab('Average annual Schmidt stability index') +
  labs(color='Max. depth (m)') +
  theme_minimal(); g2

g <- ggplot(cut.lmo.df, aes(lmoz, st, col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  geom_line(data = augment(fit), aes(lmoz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  annotate(geom="label", x=18, y=2800, label=paste0('SSI ~ ',yf,' + (',y0,' - ',yf,') exp(-',alpha,' * Lmoz)')) +
  ggtitle(paste0('nmae=',round(nmae,3),', rmse=',round(rmse,1),'Jm-2, nse=',round(nse,2),' (n=',nrow(cut.lmo.df),')')) +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  xlim(0,2.5) +
  labs(color='Max. depth (m)') +
  theme_minimal(); g
ggsave(file=paste0('fig/Fig_ExpDecModel_pball_cut.eps'), g, dpi = 300,width = 6,height = 6, units = 'in')
ggsave(file=paste0('fig/Fig_ExpDecModel_pball_cut.png'), g, dpi = 300,width = 6,height = 6, units = 'in')

g.dens <- ggplot(cut.lmo.df, aes(lmoz, col = z)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 70)+
  geom_density(alpha=.2, fill="lightblue") +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Density') +
  xlim(0, 10) +
  theme_minimal(); g.dens

g <- g2 / g1 / g.dens; g
ggsave(file=paste0('pball_observed.png'), g, dpi = 300,width = 6.65,height = 9.5, units = 'in')# ,width = 584,height = 316, units = 'mm'
###

odbc::odbcListDrivers()
# drv = dbDriver("PostgreSQL Driver")
# drv = odbc::odbc()
drv = dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "UW_data_science",
                 host = '144.92.62.199', port = 5432,
                 user = "postgres", password = 'SparklingRusty')

mydata <- dbGetQuery(con,"select * from data.predicted_temps_calibrated where nhd_lake_id = 'nhdhr_120018459'", stringsAsFactors = FALSE)
str(mydata)

lookup_nhd_metfile <- dbGetQuery(con,'select * from data.nhd_met_relation', stringsAsFactors = FALSE)
met_data <- dbGetQuery(con,"select * from data.met_input_data where meteofile in (select distinct(meteo_filename) from data.nhd_met_relation where nhdid = 'nhdhr_120018459')",
                       stringsAsFactors = FALSE)
str(met_data)


all_ids <- read.csv('nhdids.csv')
lks = all_ids$nhdid # sample(all_ids$nhdid, 100)
# lks <- list.dirs(path = 'inst/extdata/', full.names = TRUE, recursive = F)
a=1
for (ii in lks){
  print(paste0('Running ',ii,' / ',round((a*100)/length(lks),1),'%'))

  mydata_test <- dbGetQuery(con,paste("select * from data.predicted_temps_calibrated where nhd_lake_id = '",ii,"'",sep = ''), stringsAsFactors = FALSE)
  met_data_test <- dbGetQuery(con,paste("select * from data.met_input_data where meteofile in (select distinct(meteo_filename) from
                                        data.nhd_met_relation where nhdid = '",ii,"')",sep = ""), stringsAsFactors = FALSE)

  p.data <- c()
  for (ki in sort(unique(mydata_test$date))){
    yi <- which(mydata_test$date == ki)
    p.data <- rbind(p.data, rev(mydata_test$Value[yi]))
  }

  print('Finished reversing the wtemp data')

  data <- as.data.frame(cbind(rep(1, length(unique(mydata_test$date))), p.data))
  colnames(data) <- c('date',paste0('temp_',sort(unique(mydata_test$Depth))))
  data$date = as.Date(sort(unique(mydata_test$date)))

  meteo <- met_data_test[order(met_data_test$time),]
  # data <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'pball', include.dirs = T)))
  # meteo <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'NLDAS', include.dirs = T)))

  eg_nml <- read_nml(paste0('../pball_nml/pball_nml_files/pball_',ii,'.nml'))
  H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
  A <- eg_nml$morphometry$A

  ssi <- rep(NA, length(data$date))
  data.wtr <- as.matrix(data[,-c(1)])
  strdep <- gsub("[a-zA-Z ]", "", colnames(data))
  strdep <- gsub("_", "", strdep)
  strdep <-  as.numeric(strdep[-1])
  for (ji in 1:length(data$date)){
    # error <- try(schmidt.stability(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A),bthD = rev(H)))
    if (is.error(ssi[ji] <- schmidt.stability(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A),bthD = rev(H)))){
      ssi[ji] = NA
    } else {
      ssi[ji] <- schmidt.stability(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A),bthD = rev(H))
    }
  }

  wind.ts <- data.frame('datetime' = meteo$time, 'u10' = meteo$WindSpeed)
  wind.ts$ux <- 1.3 *10^(-3) * 1.43 * 10^(-3) * (wind.ts$u10)^2
  wind.ts$sw <- meteo$ShortWave # 1 W = kg m2 / s3 # kg = 1000 kg/m3 - m5 / s3 m2
  wind.ts$year <- year(wind.ts$datetime)
  wind.ts$doy = yday(wind.ts$datetime)

  annual.wind.ts <- wind.ts %>%
    dplyr::filter(doy >= 120 & doy <= 275) %>%
    group_by(year) %>%
    summarise(mean.ux = mean(sqrt(ux), na.rm = TRUE),
              mean.js = mean(sw, na.rm = TRUE))

  annual.wind.ts$Jb <-  (207 * 10^(-6) * 9.81)/(4180 *1000) * annual.wind.ts$mean.js / 1e3 * 1000
  annual.wind.ts$Lmo <- annual.wind.ts$mean.ux^3 /(annual.wind.ts$Jb *0.41)# m3/s3 / w/m2
  
  E.schmidt<- data.frame('time' = data$date, 'St' = ssi)
  dep <- strdep
  App <- stats::approx(rev(H), rev(A), dep)$y
  zv <- dep %*% App / sum(App, na.rm = TRUE)
  if (is.na(zv)){
    zv <- dep[!is.na(App)] %*% App[!is.na(App)] / sum(App, na.rm = TRUE)
  }

  E.schmidt$year = year(E.schmidt$time)
  E.schmidt$doy = yday(E.schmidt$time)
  annual.E.schmidt <- E.schmidt %>%
    dplyr::filter(doy >= 120 & doy <= 275) %>%
    group_by(year) %>%
    summarise(mean.st = mean(St, na.rm = TRUE))
  

  
  if (a==1){
    lmo.df <- data.frame('year' = unique(E.schmidt$year),
                         'lmoz' = annual.wind.ts$Lmo/zv,
                         'lmo' = annual.wind.ts$Lmo,
                         'st' = annual.E.schmidt$mean.st,
                         'id' = sub("\\).*", "", sub(".*\\(", "", ii)),
                         'z' = rep(max(H),length(unique(E.schmidt$year))))

  } else {
    lmo.df <- rbind(lmo.df, data.frame('year' = unique(E.schmidt$year),
                                       'lmoz' = annual.wind.ts$Lmo/zv,
                                       'lmo' = annual.wind.ts$Lmo,
                                       'st' = annual.E.schmidt$mean.st,
                                       'id' = sub("\\).*", "", sub(".*\\(", "", ii)) ,
                                       'z' = rep(max(H),length(unique(E.schmidt$year)))))
  }
  a=a+1
}


##########




# write_delim(lmo.df, path = 'pball_processed.csv',delim = "\t")#,row.names=FALSE,quote = FALSE)

## load example data
setwd('/Users/robertladwig/Documents/DSI/Limno_DataScience')
lks <- list.dirs(path = 'inst/extdata/', full.names = TRUE, recursive = F)
a=1
for (ii in lks){
  print(paste0('Running ',ii))

  data <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'pball', include.dirs = T)))
  meteo <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'NLDAS', include.dirs = T)))
  eg_nml <- read_nml(paste0(ii,'/', list.files(ii, pattern = 'nml', include.dirs = T)))
  H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
  A <- eg_nml$morphometry$A

  if (length( list.files(ii, pattern = 'wq_data', include.dirs = T)) > 0){
    wq_data<- paste0(ii,'/', list.files(ii, pattern = 'wq_data', include.dirs = T))
    obs <- NULL

    for(jj in wq_data){
      raw_obs <- read.csv(jj)
      wq <- raw_obs %>%
        dplyr::filter(CharacteristicName== "Depth, Secchi disk depth") %>%
        dplyr::select(c('ActivityStartDate', 'ResultMeasureValue'))
    }
  }

  ssi <- rep(NA, length(data$date))
  t.dep <- rep(NA, length(data$date))
  data.wtr <- as.matrix(data[,-c(1)])
  strdep <- gsub("[a-zA-Z ]", "", colnames(data))
  strdep <- gsub("_", "", strdep)
  strdep <-  as.numeric(strdep[-1])
  for (ji in 1:length(data$date)){
    ssi[ji] <- schmidt.stability(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A),bthD = rev(H))
    t.dep[ji] <- thermo.depth(wtr = data.wtr[ji,], depths = strdep)
  }

  wind.ts <- data.frame('datetime' = meteo$time, 'u10' = meteo$WindSpeed)
  wind.ts$ux <- 1.3 *10^(-3) * 1.43 * 10^(-3) * (wind.ts$u10)^2
  wind.ts$sw <- meteo$ShortWave # 1 W = kg m2 / s3 # kg = 1000 kg/m3 - m5 / s3 m2
  wind.ts$year <- year(wind.ts$datetime)
  wind.ts$doy = yday(wind.ts$datetime)

  annual.wind.ts <- wind.ts %>%
    dplyr::filter(doy >= 120 & doy <= 275) %>%
    group_by(year) %>%
    summarise(mean.ux = mean(sqrt(ux), na.rm = TRUE),
              mean.js = mean(sw, na.rm = TRUE))

  annual.wind.ts$Jb <-  (207 * 10^(-6) * 9.81)/(4180 *1000) * annual.wind.ts$mean.js / 1e3 * 1000
  annual.wind.ts$Lmo <- annual.wind.ts$mean.ux^3 /(annual.wind.ts$Jb *0.41)# m3/s3 / w/m2


  E.schmidt<- data.frame('time' = data$date, 'St' = ssi)
  t.dep.df <- data.frame('time' = data$date, 'thermdep' = t.dep)
  wq.df <- data.frame('time' = wq$ActivityStartDate, 'secchi' = wq$ResultMeasureValue)
  dep <- strdep
  App <- stats::approx(rev(H), rev(A), dep)$y
  zv <- dep %*% App / sum(App)

  E.schmidt$year = year(E.schmidt$time)
  E.schmidt$doy = yday(E.schmidt$time)
  annual.E.schmidt <- E.schmidt %>%
    dplyr::filter(doy >= 120 & doy <= 275) %>%
    group_by(year) %>%
    summarise(mean.st = mean(St, na.rm = TRUE))

  t.dep.df$year = year(t.dep.df$time)
  t.dep.df$doy = yday(t.dep.df$time)
  annual.t.dep.df <- t.dep.df %>%
    dplyr::filter(doy >= 120 & doy <= 275) %>%
    group_by(year) %>%
    summarise(mean.thermdep = mean(thermdep, na.rm = TRUE))

  wq.df$year = year(wq$ActivityStartDate)
  wq.df$doy = yday(wq$ActivityStartDate)
  annual.wq.df <- wq.df %>%
    dplyr::filter(doy >= 120 & doy <= 275) %>%
    group_by(year) %>%
    summarise(mean.secchi = mean(secchi, na.rm = TRUE))

  secchiapp <-  Hmisc::approxExtrap(annual.wq.df$year, annual.wq.df$mean.secchi, annual.t.dep.df$year,
                        method = 'linear')$y

  annual.t.dep.df$mean.thermdep <- na.approx(annual.t.dep.df$mean.thermdep)
  annual.t.dep.df$minimum <- rep(NA, nrow(annual.t.dep.df))
  for (tdix in 1:nrow(annual.t.dep.df)){
    annual.t.dep.df$minimum[tdix] <- which.min(abs(dep-annual.t.dep.df$mean.thermdep[tdix]))
  }
  annual.t.dep.df$volepi <- rep(NA, nrow(annual.t.dep.df))
  for (tdix in 1:nrow(annual.t.dep.df)){
    annual.t.dep.df$volepi[tdix] <- App[1:annual.t.dep.df$minimum[tdix]] %*% dep[1:annual.t.dep.df$minimum[tdix]]
  }
  annual.t.dep.df$volhypo <- (App %*% dep)  - annual.t.dep.df$volepi

  gsc <- secchiapp/2 + sqrt( (secchiapp/2)^2 + 0.0006 * sqrt(max(A)) * annual.wind.ts$Lmo )

  if (a==1){
    lmo.df <- data.frame('year' = unique(E.schmidt$year),
                       'lmoz' = annual.wind.ts$Lmo/zv,
                       'lmo' = annual.wind.ts$Lmo,
                       'genscc' = gsc,
                       'gscz' = gsc/zv,
                       'st' = annual.E.schmidt$mean.st,
                       'thermdep' = annual.t.dep.df$mean.thermdep,
                       'volratio' = annual.t.dep.df$volepi /  annual.t.dep.df$volhypo ,
                       'secchidep' = secchiapp,
                       'area' = max(A),
                       'id' = sub("\\).*", "", sub(".*\\(", "", ii)),
                       'z' = rep(max(H),length(unique(E.schmidt$year))))

    # all.dat <- data.frame('date' = wind.ts$datetime[match(data$date[1], meteo$time) : (length(wind.ts$datetime)-1)],
    #                       'jb' = (207 * 10^(-6) * 9.81)/(4180 *1000) * wind.ts$sw[match(data$date[1], meteo$time) : (length(wind.ts$datetime)-1)] / 1e3 * 1000,
    #                       'st' = E.schmidt$St,
    #                       'id' = sub("\\).*", "", sub(".*\\(", "", ii)),
    #                       'z' = rep(max(H)))
    # all.dat$lmo <- (sqrt(wind.ts$ux[match(data$date[1], meteo$time) : (length(wind.ts$datetime)-1)])^3 /all.dat$jb) / zv
    #
    # lmo.all <- all.dat
  } else {
    lmo.df <- rbind(lmo.df, data.frame('year' = unique(E.schmidt$year),
                                       'lmoz' = annual.wind.ts$Lmo/zv,
                                       'lmo' = annual.wind.ts$Lmo,
                                       'genscc' = gsc,
                                       'gscz' = gsc/zv,
                                       'st' = annual.E.schmidt$mean.st,
                                       'thermdep' = annual.t.dep.df$mean.thermdep,
                                       'volratio' = annual.t.dep.df$volepi /  annual.t.dep.df$volhypo ,
                                       'secchidep' = secchiapp,
                                       'area' = max(A),
                                       'id' = sub("\\).*", "", sub(".*\\(", "", ii)),
                                       'z' = rep(max(H),length(unique(E.schmidt$year)))))

    # all.dat <- data.frame('date' = wind.ts$datetime[match(data$date[1], meteo$time) : (length(wind.ts$datetime)-1)],
    #                       'jb' = (207 * 10^(-6) * 9.81)/(4180 *1000) * wind.ts$sw[match(data$date[1], meteo$time) : (length(wind.ts$datetime)-1)] / 1e3 * 1000,
    #                       'st' = E.schmidt$St,
    #                       'id' = sub("\\).*", "", sub(".*\\(", "", ii)),
    #                       'z' = rep(max(H)))
    # all.dat$lmo <- (sqrt(wind.ts$ux[match(data$date[1], meteo$time) : (length(wind.ts$datetime)-1)])^3 /all.dat$jb) / zv
    #
    # lmo.all <- rbind(lmo.all,all.dat )
  }
  a=a+1

}
write_delim(lmo.df, path = 'pball_processed_LTER.csv',delim = "\t")#,row.names=FALSE,quote = FALSE)



# ISIMIP DATA
setwd('/Users/robertladwig/Documents/DSI/LMO/lakes/')
lks <- list.dirs(path = '.', full.names = TRUE, recursive = F)
a= 1
for (ii in lks){
  print(paste0('Running ',ii))

  if (length(list.files(paste0(ii,'/'))) < 2) {
    next
  }

  ssi_glm <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'glm', include.dirs = T)))
  ssi_gotm <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'gotm', include.dirs = T)))
  meteo <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'lmo', include.dirs = T)))

  ssi_glm$time <- as.Date(ssi_glm$time)
  ssi_glm$year = year(ssi_glm$time)
  ssi_glm$doy = yday(ssi_glm$time)
  annual.E.schmidt_glm <- ssi_glm %>%
    dplyr::filter(doy >= 120 & doy <= 275) %>%
    group_by(year) %>%
    summarise(mean.st = mean(SSI, na.rm = TRUE))
  ssi_gotm$time <- as.Date(ssi_gotm$time)
  ssi_gotm$year = year(ssi_gotm$time)
  ssi_gotm$doy = yday(ssi_gotm$time)
  annual.E.schmidt_gotm <- ssi_gotm %>%
    dplyr::filter(doy >= 120 & doy <= 275) %>%
    group_by(year) %>%
    summarise(mean.st = mean(SSI, na.rm = TRUE))

  if (a==1){
    lmo.df <- data.frame('year' = unique(ssi_glm$year),
                         'lmoz' = meteo$lmoz,
                         'lmo' = meteo$lmo,
                         'st_glm' = annual.E.schmidt_glm$mean.st,
                         'st_gotm' = annual.E.schmidt_gotm$mean.st,
                         'id' = sub("\\).*", "", sub(".*\\(", "", ii)),
                         'z' = meteo$z)
  } else {
    lmo.df <- rbind(lmo.df, data.frame('year' = unique(ssi_glm$year),
                                       'lmoz' = meteo$lmoz,
                                       'lmo' = meteo$lmo,
                                       'st_glm' = annual.E.schmidt_glm$mean.st,
                                       'st_gotm' = annual.E.schmidt_gotm$mean.st,
                                       'id' = sub("\\).*", "", sub(".*\\(", "", ii)),
                                       'z' = meteo$z))
  }
  a=a+1
}
write_delim(lmo.df, path = '../pball_processed_ISIMIP.csv',delim = "\t")#,row.names=FALSE,quote = FALSE)





## LOCAL DEMONSTRATION LAKE MENDOTA

setwd('/Users/robertladwig/Documents/DSI/Limno_DataScience')

ii = lks[2]
data <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'pball', include.dirs = T)))
meteo <- read.csv(paste0(ii,'/', list.files(ii, pattern = 'NLDAS', include.dirs = T)))
eg_nml <- read_nml(paste0(ii,'/', list.files(ii, pattern = 'nml', include.dirs = T)))
H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
A <- eg_nml$morphometry$A

data.wtr <- as.matrix(data[,-c(1)])
strdep <- gsub("[a-zA-Z ]", "", colnames(data))
strdep <- gsub("_", "", strdep)
strdep <-  as.numeric(strdep[-1])


leg.title <- '2005-05-01'
ji = which(data$date ==  leg.title)
ssi <- ssi.distribution(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A), bthD = rev(H))
schmidt.stability(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A), bthD = rev(H))

criticalValues <- c(unique(ssi$Zcv), max(ssi$layerD))
shadeNormalTwoTailedLeft <- rbind(c(criticalValues[1],0),
                                  subset(ssi, layerD > criticalValues[1]),
                                  c(criticalValues[2],0))

criticalValues <- c(min(ssi$layerD), unique(ssi$Zcv))
shadeNormalTwoTailedRight <- rbind(c(criticalValues[1],0),
                                  subset(ssi, layerD > criticalValues[1]),
                                  c(criticalValues[2],0))

g1 <- ggplot(ssi, aes(St, layerD)) +
  geom_point() +
  geom_hline(yintercept = zv, linetype = 'dashed', color = 'black', size = 1) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  scale_y_reverse() +
  geom_polygon(data = shadeNormalTwoTailedLeft, aes(x=St, y=layerD, fill="red")) +
  geom_polygon(data = shadeNormalTwoTailedRight, aes(x=St, y=layerD, fill="red")) +
  guides(fill="none") +
# geom_area(mapping=aes(x=ifelse(x>0 & x<10, x, 0)), fill="#9898fb", alpha=1.) +
  ylab("Depth [m]") +
  xlab('Schmidt Stability [kg m4 m-3]') +
  annotate(geom="label", x=-3000, y=2.5, label=paste0(formatC(round(sum(subset(ssi$St, ssi$St > 0)),2), format = "e", digits = 2))) +
  annotate(geom="label", x=1400, y=17.5, label=paste0(formatC(round(sum(subset(ssi$St, ssi$St > 0)),2), format = "e", digits = 2))) +
  ggtitle(paste0(leg.title,' St = ',round(9.81/max(A) * sum(ssi$St),1),' J/m2')) +
  theme_minimal(); g1


leg.title <- '2005-07-01'
ji = which(data$date ==  leg.title)
ssi <- ssi.distribution(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A), bthD = rev(H))
schmidt.stability(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A), bthD = rev(H))

criticalValues <- c(unique(ssi$Zcv), max(ssi$layerD))
shadeNormalTwoTailedLeft <- rbind(c(criticalValues[1],0),
                                  subset(ssi, layerD > criticalValues[1]),
                                  c(criticalValues[2],0))

criticalValues <- c(min(ssi$layerD), unique(ssi$Zcv))
shadeNormalTwoTailedRight <- rbind(c(criticalValues[1],0),
                                   subset(ssi, layerD > criticalValues[1]),
                                   c(criticalValues[2],0))

g2 <- ggplot(ssi, aes(St, layerD)) +
  geom_point() +
  geom_hline(yintercept = zv, linetype = 'dashed', color = 'black', size = 1) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  scale_y_reverse() +
  geom_polygon(data = shadeNormalTwoTailedLeft, aes(x=St, y=layerD, fill="red")) +
  geom_polygon(data = shadeNormalTwoTailedRight, aes(x=St, y=layerD, fill="red")) +
  guides(fill="none") +
  # geom_area(mapping=aes(x=ifelse(x>0 & x<10, x, 0)), fill="#9898fb", alpha=1.) +
  # ylab("Depth [m]") +
  xlab('Schmidt Stability [kg m4 m-3]') +
  annotate(geom="label", x=-3000, y=2.5, label=paste0(formatC(round(sum(subset(ssi$St, ssi$St > 0)),2), format = "e", digits = 2))) +
  annotate(geom="label", x=1400, y=17.5, label=paste0(formatC(round(sum(subset(ssi$St, ssi$St > 0)),2), format = "e", digits = 2))) +
  ggtitle(paste0(leg.title,' St = ',round(9.81/max(A) * sum(ssi$St),1),' J/m2')) +
  theme_minimal(); g2


leg.title <- '2005-09-01'
ji = which(data$date ==  leg.title)
ssi <- ssi.distribution(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A), bthD = rev(H))
schmidt.stability(wtr = data.wtr[ji,], depths = strdep, bthA =  rev(A), bthD = rev(H))

criticalValues <- c(unique(ssi$Zcv), max(ssi$layerD))
shadeNormalTwoTailedLeft <- rbind(c(criticalValues[1],0),
                                  subset(ssi, layerD > criticalValues[1]),
                                  c(criticalValues[2],0))

criticalValues <- c(min(ssi$layerD), unique(ssi$Zcv))
shadeNormalTwoTailedRight <- rbind(c(criticalValues[1],0),
                                   subset(ssi, layerD > criticalValues[1]),
                                   c(criticalValues[2],0))

g3 <- ggplot(ssi, aes(St, layerD)) +
  geom_point() +
  geom_hline(yintercept = zv, linetype = 'dashed', color = 'black', size = 1) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  scale_y_reverse() +
  geom_polygon(data = shadeNormalTwoTailedLeft, aes(x=St, y=layerD, fill="red")) +
  geom_polygon(data = shadeNormalTwoTailedRight, aes(x=St, y=layerD, fill="red")) +
  guides(fill="none") +
  # geom_area(mapping=aes(x=ifelse(x>0 & x<10, x, 0)), fill="#9898fb", alpha=1.) +
  # ylab("Depth [m]") +
  xlab('Schmidt Stability [kg m4 m-3]') +
  annotate(geom="label", x=-3000, y=2.5, label=paste0(formatC(round(sum(subset(ssi$St, ssi$St > 0)),2), format = "e", digits = 2))) +
  annotate(geom="label", x=1400, y=17.5, label=paste0(formatC(round(sum(subset(ssi$St, ssi$St > 0)),2), format = "e", digits = 2))) +
  ggtitle(paste0(leg.title,' St = ',round(9.81/max(A) * sum(ssi$St),1),' J/m2')) +
  theme_minimal(); g3


setwd('/Users/robertladwig/Documents/DSI/LMO/')

g <- g1 + g2 + g3 + plot_annotation(tag_levels = 'A');g
ggsave(file=paste0('fig/Fig_SSI.eps'), g, dpi = 300,width = 9,height = 9, units = 'in')



## GLOBAL ANALYSIS

setwd('/Users/robertladwig/Documents/DSI/LMO/')
lmo.df.ISIMIP <- read_delim('pball_processed_ISIMIP.csv', delim = '\t')
lmo.df.LTER <- read_delim('pball_processed_LTER.csv', delim = '\t')
lmo.df <- read_delim('pball_processed.csv', delim = '\t')



# https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
fit <- nls(st ~ SSasymp(lmoz, yf, y0, log_alpha), data = lmo.df)
yf = round(28.060,1)
y0 = round(6370.58,1)
alpha = round(exp(1.618),1)
summary(fit)
predicted2 <- 18.674 * (5877.491 - 18.674) * exp(- exp((1.518)) * (seq(min(lmo.df$lmoz), max(lmo.df$lmoz), 0.1)))
plot(predicted2)
pred.data = augment(fit)
plot(augment(fit)$lmoz,augment(fit)$.fitted)
predicted2 = augment(fit)$.fitted

nmae = (1/nrow(pred.data) * sum(abs(pred.data$.fitted - pred.data$st))) /
  (max(pred.data$st) - min(pred.data$st))
rmse = sqrt((1/nrow(pred.data) * sum((pred.data$.fitted - pred.data$st)^2)))
nse = 1 - (sum((pred.data$.fitted - pred.data$st)^2))/(sum((pred.data$st - mean(pred.data$st,na.rm = TRUE))^2))

model <- lm(log(st) ~ lmoz, lmo.df.LTER)
timevalues <- seq(min(lmo.df.LTER$lmoz), max(lmo.df.LTER$lmoz), 0.1)
exp(7.3-5.82* timevalues)
Counts.exponential2 <- exp(predict(model,list(lmoz=timevalues)))
predicted <- exp(predict(model,list(lmoz=lmo.df.LTER$lmoz),interval = "confidence"))
plot(lmo.df.LTER$lmoz, lmo.df.LTER$st,pch=16,
     xlab = 'Lmo/zv', ylab = 'SSI')
lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")

model <- lm(log(st) ~ volratio, lmo.df.LTER)
timevalues2 <- seq(min(lmo.df.LTER$volratio), max(lmo.df.LTER$volratio), 0.1)
exp(5.3603-0.2865* timevalues2)
Counts.exponential22 <- exp(predict(model,list(volratio=timevalues2)))
predicted12 <- exp(predict(model,list(volratio=lmo.df.LTER$volratio),interval = "confidence"))

model <- lm(log(st) ~ gscz, lmo.df.LTER)
timevalues3 <- seq(min(lmo.df.LTER$gscz), max(lmo.df.LTER$gscz), 0.1)
exp(5.3603-0.2865* timevalues2)
Counts.exponential23 <- exp(predict(model,list(gscz=timevalues3)))
predicted13 <- exp(predict(model,list(gscz=lmo.df.LTER$gscz),interval = "confidence"))

par(mfrow=c(1,3))
plot(lmo.df.LTER$lmoz, lmo.df.LTER$st,pch=16,
     xlab = 'Lmo/zv', ylab = 'SSI')
lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")
plot(lmo.df.LTER$volratio, lmo.df.LTER$st,pch=16,
     xlab = 'vol.epi/vol.hypo', ylab = 'SSI')
lines(timevalues2, Counts.exponential22,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")
plot(lmo.df.LTER$gscz, lmo.df.LTER$st,pch=16,
     xlab = 'GSC/zv', ylab = 'SSI')
lines(timevalues3, Counts.exponential23,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")
dev.off()

pred.data2 <- c()
pred.data2$.fitted <- predicted[,1]
pred.data2$st <- lmo.df.LTER$st
pred.data2 <- as.data.frame(pred.data2)
pred.data2$lmoz <- lmo.df.LTER$lmoz

pred.data3 <- c()
pred.data3$.fitted <- predicted13[,1]
pred.data3$st <- lmo.df.LTER$st
pred.data3 <- as.data.frame(pred.data3)
pred.data3$gscz <- lmo.df.LTER$gscz

nmae2 = (1/nrow(pred.data2) * sum(abs(pred.data2$.fitted - pred.data2$st), na.rm = TRUE)) /
  (max(pred.data2$st,na.rm = TRUE) - min(pred.data2$st,na.rm = TRUE))
rmse2 = sqrt((1/nrow(pred.data2) * sum((pred.data2$.fitted - pred.data2$st)^2, na.rm = TRUE)))
nse2 = 1 - (sum((pred.data2$.fitted - pred.data2$st)^2, na.rm = TRUE))/(sum((pred.data2$st - mean(pred.data2$st,na.rm = TRUE))^2, na.rm = TRUE))

nmae3 = (1/nrow(pred.data3) * sum(abs(pred.data3$.fitted - pred.data3$st), na.rm = TRUE)) /
  (max(pred.data3$st,na.rm = TRUE) - min(pred.data3$st,na.rm = TRUE))
rmse3 = sqrt((1/nrow(pred.data3) * sum((pred.data3$.fitted - pred.data3$st)^2, na.rm = TRUE)))
nse3 = 1 - (sum((pred.data3$.fitted - pred.data3$st)^2, na.rm = TRUE))/(sum((pred.data3$st - mean(pred.data3$st,na.rm = TRUE))^2, na.rm = TRUE))


# lmo.df$predicted <- predicted2 #predicted[,1] # exp(7.3-5.82* lmo.df$lmoz)
# lmo.df$lwr <- predicted[,2]
# lmo.df$upr <- predicted[,3]


g.lter <- ggplot(lmo.df.LTER, aes(lmoz, st, col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  geom_line(data = pred.data2, aes(lmoz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  annotate(geom="label", x=2, y=500, label=paste0('SSI ~ exp(-2.38 * Lmoz + 7.30)')) +
  ggtitle(paste0('nmae=',round(nmae2,3),', rmse=',round(rmse2,1),'Jm-2, nse=',round(nse2,2),' (n=',nrow(lmo.df.LTER),')')) +
  xlim(0,3) + ylim(0, 700)+
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  labs(color='Max. depth (m)') +
  theme_minimal(); g.lter
ggsave(file=paste0('fig/Fig_ExpDecModel_NTLLTER.eps'), g.lter, dpi = 300,width = 6,height = 6, units = 'in')
ggsave(file=paste0('fig/Fig_ExpDecModel_NTLLTER.png'), g.lter, dpi = 300,width = 6,height = 6, units = 'in')

g.lter.gsc <- ggplot(lmo.df.LTER, aes(gscz, st, col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  geom_line(data = pred.data3, aes(gscz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  annotate(geom="label", x=2, y=500, label=paste0('SSI ~ exp(-1.71 * Lmoz + 6.75)')) +
  ggtitle(paste0('nmae=',round(nmae3,3),', rmse=',round(rmse3,1),'Jm-2, nse=',round(nse3,2),' (n=',nrow(lmo.df.LTER),')')) +
  xlim(0,3) + ylim(0, 700)+
  xlab('Average annual Generalised Scaling Criterion by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  labs(color='Max. depth (m)') +
  theme_minimal(); g.lter.gsc

g = g.lter + g.lter.gsc;g
ggsave(file=paste0('fig/Fig_ExpDecModel_generalscal.png'), g, dpi = 300,width = 12,height = 9, units = 'in')

g <- ggplot(lmo.df, aes(lmoz, st, col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  geom_line(data = augment(fit), aes(lmoz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  annotate(geom="label", x=18, y=2800, label=paste0('SSI ~ ',yf,' + (',y0,' - ',yf,') exp(-',alpha,' * Lmoz)')) +
  ggtitle(paste0('nmae=',round(nmae,3),', rmse=',round(rmse,1),'Jm-2, nse=',round(nse,2),' (n=',nrow(lmo.df),')')) +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  labs(color='Max. depth (m)') +
  theme_minimal(); g
ggsave(file=paste0('fig/Fig_ExpDecModel.eps'), g, dpi = 300,width = 6,height = 6, units = 'in')
ggsave(file=paste0('fig/Fig_ExpDecModel.png'), g, dpi = 300,width = 6,height = 6, units = 'in')

g.dens.lter <- ggplot(lmo.df.LTER, aes(lmoz, col = z)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 70)+
  geom_density(alpha=.2, fill="lightblue") +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Density') +
  xlim(0, 10) +
  theme_minimal(); g.dens.lter

g.dens <- ggplot(lmo.df, aes(lmoz, col = z)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 70)+
  geom_density(alpha=.2, fill="lightblue") +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Density') +
  xlim(0, 10) +
  theme_minimal(); g.dens
ggsave(file=paste0('fig/Fig_ExpDecModel_density.eps'), g.dens, dpi = 300,width = 6,height = 6, units = 'in')
ggsave(file=paste0('fig/Fig_ExpDecModel_density.png'), g.dens, dpi = 300,width = 6,height = 6, units = 'in')

g.verificiation <- (g.dens.lter + g.dens) / (g.lter+ g)  + plot_annotation(tag_levels = c('A')); g.verificiation
ggsave(file=paste0('fig/Fig_ExpDecModel_verification.eps'), g.verificiation, dpi = 300,width = 12,height = 9, units = 'in')
ggsave(file=paste0('fig/Fig_ExpDecModel_verification.png'), g.verificiation, dpi = 300,width = 12,height = 9, units = 'in')


ranges <- lmo.df.ISIMIP %>%
  group_by(id) %>%
  summarise(range = median(st_glm))

deep.lakes <- ranges$id[ranges$range > 4000]

which(lmo.df.ISIMIP$id == deep.lakes)
id.deep <- c()
for (xi in 1:length(deep.lakes)){
  id.deep <- append(id.deep, which(lmo.df.ISIMIP$id == deep.lakes[xi]))
}
lmo.df.ISIMIP.deep <- lmo.df.ISIMIP[-c(id.deep),]

lmo.df.ISIMIP.deep$type = rep(NA, nrow(lmo.df.ISIMIP.deep))
lmo.df.ISIMIP.deep$pattern = rep(NA, nrow(lmo.df.ISIMIP.deep))
lmo.df.ISIMIP.deep$pattern2 = rep(NA, nrow(lmo.df.ISIMIP.deep))
for (xi in 1:nrow(lmo.df.ISIMIP.deep)){
  if (lmo.df.ISIMIP.deep$z[xi] < 10){
    lmo.df.ISIMIP.deep$type[xi] = 'shallow'
    dt = which(lmo.df.ISIMIP.deep$id ==lmo.df.ISIMIP.deep$id[xi])
    if (mean(lmo.df.ISIMIP.deep$lmoz[dt]) > 1){
      lmo.df.ISIMIP.deep$pattern[xi] = 'decreasing SSI'
      if (mean(lmo.df.ISIMIP.deep$lmo[dt]) > 1){
        lmo.df.ISIMIP.deep$pattern2[xi] = 'decreasing SSI'
      } else {
        lmo.df.ISIMIP.deep$pattern2[xi] = 'increasing SSI'
      }
    } else {
      lmo.df.ISIMIP.deep$pattern[xi] = 'increasing SSI'
      if (mean(lmo.df.ISIMIP.deep$lmo[dt]) > 1){
        lmo.df.ISIMIP.deep$pattern2[xi] = 'decreasing SSI'
      } else {
        lmo.df.ISIMIP.deep$pattern2[xi] = 'increasing SSI'
      }
    }
  } else if (lmo.df.ISIMIP.deep$z[xi] > 10 & lmo.df.ISIMIP.deep$z[xi] < 25){
    lmo.df.ISIMIP.deep$type[xi] = 'moderate'
    dt = which(lmo.df.ISIMIP.deep$id ==lmo.df.ISIMIP.deep$id[xi])
    if (mean(lmo.df.ISIMIP.deep$lmoz[dt]) > 1){
      lmo.df.ISIMIP.deep$pattern[xi] = 'decreasing SSI'
      if (mean(lmo.df.ISIMIP.deep$lmo[dt]) > 1){
        lmo.df.ISIMIP.deep$pattern2[xi] = 'decreasing SSI'
      } else {
        lmo.df.ISIMIP.deep$pattern2[xi] = 'increasing SSI'
      }
    } else {
      lmo.df.ISIMIP.deep$pattern[xi] = 'increasing SSI'
      if (mean(lmo.df.ISIMIP.deep$lmo[dt]) > 1){
        lmo.df.ISIMIP.deep$pattern2[xi] = 'decreasing SSI'
      } else {
        lmo.df.ISIMIP.deep$pattern2[xi] = 'increasing SSI'
      }
    }
  } else if (lmo.df.ISIMIP.deep$z[xi] > 25){
    lmo.df.ISIMIP.deep$type[xi] = 'deep'
    dt = which(lmo.df.ISIMIP.deep$id ==lmo.df.ISIMIP.deep$id[xi])
    if (mean(lmo.df.ISIMIP.deep$lmoz[dt]) > 1){
      lmo.df.ISIMIP.deep$pattern[xi] = 'decreasing SSI'
      if (mean(lmo.df.ISIMIP.deep$lmo[dt]) > 1){
        lmo.df.ISIMIP.deep$pattern2[xi] = 'decreasing SSI'
      } else {
        lmo.df.ISIMIP.deep$pattern2[xi] = 'increasing SSI'
      }
    } else {
      lmo.df.ISIMIP.deep$pattern[xi] = 'increasing SSI'
      if (mean(lmo.df.ISIMIP.deep$lmo[dt]) > 1){
        lmo.df.ISIMIP.deep$pattern2[xi] = 'decreasing SSI'
      } else {
        lmo.df.ISIMIP.deep$pattern2[xi] = 'increasing SSI'
      }
    }
  }
}

g.lmoz_time <- ggplot(lmo.df.ISIMIP.deep, aes(year, lmoz, col = id)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_grid(type ~ pattern, scales = 'free_y') +
  xlab('') + ylab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  theme_minimal() +
  theme(legend.position = "none");g.lmoz_time

g.ssi_time <- ggplot(lmo.df.ISIMIP.deep, aes(year, st_glm, col = id)) +
  geom_line() +
  geom_smooth(method = "lm") +
  xlab('') + ylab('Average annual Schmidt stability index') +
  facet_grid(type ~ pattern, scales = 'free_y') +
  theme_minimal() +
  theme(legend.position = "none");g.ssi_time

g.ssi_time <- ggplot(lmo.df.ISIMIP.deep, aes(year, st_gotm, col = id)) +
  geom_line() +
  geom_smooth(method = "lm") +
  xlab('') + ylab('Average annual Schmidt stability index') +
  facet_grid(type ~ pattern, scales = 'free_y') +
  theme_minimal() +
  theme(legend.position = "none");g.ssi_time

g.lmo_time <- ggplot(lmo.df.ISIMIP.deep, aes(year, lmo, col = id)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_grid(type ~ pattern2) +
  xlab('') + ylab('Average annual Monin-Obukhov length') +
  theme_minimal() +
  theme(legend.position = "none");g.lmo_time

g.ssi_time2 <- ggplot(lmo.df.ISIMIP.deep, aes(year, st, col = id)) +
  geom_line() +
  geom_smooth(method = "lm") +
  xlab('') + ylab('Average annual Schmidt stability index') +
  facet_grid(type ~ pattern2, scales = 'free_y') +
  theme_minimal() +
  theme(legend.position = "none");g.ssi_time2

g.test <- g.lmoz_time  + g.ssi_time+ plot_annotation(tag_levels = c('A')); g.test
ggsave(file=paste0('fig/Fig_ExpDecModel_test_lmoz.eps'), g.test, dpi = 300,width = 12,height = 9, units = 'in')
ggsave(file=paste0('fig/Fig_ExpDecModel_test_lmoz.png'), g.test, dpi = 300,width = 12,height = 9, units = 'in')

g.test2 <- g.lmo_time  + g.ssi_time2+ plot_annotation(tag_levels = c('A')); g.test
ggsave(file=paste0('fig/Fig_ExpDecModel_test_lmo.eps'), g.test2, dpi = 300,width = 12,height = 9, units = 'in')
ggsave(file=paste0('fig/Fig_ExpDecModel_test_lmo.png'), g.test2, dpi = 300,width = 12,height = 9, units = 'in')


test = subset(lmo.df.ISIMIP.deep$st_glm, lmo.df.ISIMIP.deep$id == './kinneret')
plot(lowess(test, f=2/3),ylim=c(0,4000))
points(subset(lmo.df.ISIMIP.deep$st_glm, lmo.df.ISIMIP.deep$id == './kinneret'))
# https://online.stat.psu.edu/stat510/lesson/5/5.2
lmo.df.ISIMIP.deep$loess_st = rep(NA, nrow(lmo.df.ISIMIP.deep))
lmo.df.ISIMIP.deep$diff_loess_st = rep(NA, nrow(lmo.df.ISIMIP.deep))
for (ki in (unique(lmo.df.ISIMIP.deep$id))){
  idx = which(ki == lmo.df.ISIMIP.deep$id)
  test = subset(lmo.df.ISIMIP.deep$st_glm, lmo.df.ISIMIP.deep$id == ki)
  lmo.df.ISIMIP.deep$loess_st[idx] = lowess(test, f = 2/3)$y
  lmo.df.ISIMIP.deep$diff_loess_st[idx] = c(0, diff(lmo.df.ISIMIP.deep$loess_st[idx]))
}


g.predic <- ggplot(lmo.df.ISIMIP.deep, aes(lmoz, diff_loess_st, col = loess_st, shape = type)) +
  geom_point() +
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'black', size = 1) +
  scale_color_viridis(option = 'viridis', direction = -1) +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') + ylab('Change in Schmidt stability index (J/m2/y)') +
  theme_minimal() +
  labs(color='Average annual Schmidt stability index (J/m2)') +
  theme(legend.position="bottom");g.predic
g.predic2 <- ggplot(lmo.df.ISIMIP.deep, aes(lmo, diff_loess_st, col = loess_st, shape = type)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  xlab('Average annual Monin-Obukhov length (m)') + ylab('Change in Schmidt stability index (J/m2/y)') +
  theme_minimal() +
  theme(legend.position = "none");g.predic2
g.prediciton <- g.predic2 + g.predic + plot_annotation(tag_levels = c('A')); g.prediciton
ggsave(file=paste0('fig/Fig_Prediction.eps'), g.prediciton, dpi = 300,width = 14,height = 9, units = 'in')



p = 1
for (ki in (unique(lmo.df.ISIMIP.deep$id))){
  idx = which(ki == lmo.df.ISIMIP.deep$id)
  t=lm(lmoz ~ year, lmo.df.ISIMIP.deep[idx,])
  mean_lmoz <- t$coefficients[2]#mean((lmo.df.ISIMIP.deep$lmoz[idx]))
  t=lm(lmo ~ year, lmo.df.ISIMIP.deep[idx,])
  mean_lmo <- t$coefficients[2]#mean(diff(lmo.df.ISIMIP.deep$lmo[idx]))
  t=lm(st_glm ~ year, lmo.df.ISIMIP.deep[idx,])
  mean_st_glm <- t$coefficients[2]#mean(diff(lmo.df.ISIMIP.deep$st[idx]))
  t=lm(st_gotm ~ year, lmo.df.ISIMIP.deep[idx,])
  mean_st_gotm <- t$coefficients[2]#mean(diff(lmo.df.ISIMIP.deep$st[idx]))
  if (p == 1){
    isimip.test <- data.frame('id' = ki,
                              'mean_lmoz' = mean_lmoz,
                              'mean_lmo' = mean_lmo,
                              'mean_st_glm' = mean_st_glm,
                              'mean_st_gotm' = mean_st_gotm,
                              'type' = lmo.df.ISIMIP.deep$type[idx[1]],
                              'pattern' = lmo.df.ISIMIP.deep$pattern[idx[1]])
  } else {
    isimip.test <- rbind(isimip.test, data.frame('id' = ki,
                                                 'mean_lmoz' = mean_lmoz,
                                                 'mean_lmo' = mean_lmo,
                                                 'mean_st_glm' = mean_st_glm,
                                                 'mean_st_gotm' = mean_st_gotm,
                                                 'type' = lmo.df.ISIMIP.deep$type[idx[1]],
                                                 'pattern' = lmo.df.ISIMIP.deep$pattern[idx[1]]))
  }
  p=p+1
}
ggplot(isimip.test, aes(mean_lmoz, mean_st_gotm, col = pattern)) +
  geom_point() +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Change in Schmidt stability index (J/m2/y)') +
  guides(col=guide_legend(title="Change in Monin-Obukhov length (m/y)"))
  scale_color_viridis(option = 'viridis', direction = -1)

library(plotly)
plot_ly(x=isimip.test$lmoz, y=isimip.test$mean_lmo, z=isimip.test$mean_st, type="scatter3d", mode="markers", color=isimip.test$pattern)

g <- ggplot(lmo.df.ISIMIP.deep, aes(lmoz, st, col = z)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  geom_line(data = augment(fit), aes(lmoz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  annotate(geom="label", x=1.7, y=2000, label=paste0('SSI ~ ',yf,' + (',y0,' - ',yf,') exp(-',alpha,' * Lmoz)')) +
  ggtitle(paste0('nmae=',round(nmae,3),', rmse=',round(rmse,1),'Jm-2, nse=',round(nse,2),' (n=',nrow(lmo.df),')')) +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  # xlim(0,3) + ylim(0,1000)+
  labs(color='Max. depth (m)') +
  theme_minimal(); g
ggsave(file=paste0('fig/Fig_ExpDecModel_ISIMIP.eps'), g, dpi = 300,width = 6,height = 6, units = 'in')
ggsave(file=paste0('fig/Fig_ExpDecModel_ISIMIP.png'), g, dpi = 300,width = 6,height = 6, units = 'in')



ggplot(lmo.df, aes(lmoz, st, col = lmo)) +
  geom_point() +
  scale_color_viridis(option = 'viridis', direction = -1) +
  geom_line(data = augment(fit), aes(lmoz, .fitted), col = 'red') +
  # geom_line(aes(lmoz, predicted), col = 'red') +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  xlab('Average annual Monin-Obukhov length scaled by center of volume depth') +
  ylab('Average annual Schmidt stability index') +
  theme_minimal()

ggplot(lmo.df, aes(year, st, col = 'SSI')) +
  geom_line() +
  ylab("SSI [J/m2]") +
  geom_line(aes(year, lmoz *500, col = "LMO")) +
  # geom_line(aes(year, predicted, col = "pred")) +
  # geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  # geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./500, name = "Scaled Monin-Obukhov length [-]")) +
  facet_wrap(~id)+
  theme_minimal()

ggplot(lmo.df, aes(lmoz, st-predicted, col = z)) +
  geom_point() +
  theme_minimal()

#
# # all values
#
# model <- lm(log(st) ~ lmo, all.dat)
# summary(model)
# timevalues <- seq(0, 1.5, 0.1)
# exp(7.3-5.82* timevalues)
# Counts.exponential2 <- exp(predict(model,list(lmoz=timevalues)))
# predicted <- exp(predict(model,list(lmoz=lmo.df$lmoz),interval = "confidence"))
# plot(lmo.df$lmoz, lmo.df$st,pch=16,
#      xlab = 'Lmo/zv', ylab = 'SSI')
# lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")
#
# lmo.df$predicted <- predicted[,1] # exp(7.3-5.82* lmo.df$lmoz)
# lmo.df$lwr <- predicted[,2]
# lmo.df$upr <- predicted[,3]


# ISIMIP test
eg_nml <- read_nml(paste0('../pball_nml/pball_nml_files/pball_',ii,'.nml'))
H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H))
A <- eg_nml$morphometry$A

meteo_file <- read.csv('/Users/robertladwig/Documents/ISIMIP/isimip_gotm_b/GLM_setup/Geneva/meteo.csv')

# meteofile with the columns time, WindSpeed (m/s) and ShortWave (W/m2)
# needs lubridate
# H and A should go from bottom to top
get_lmo <- function(meteo_file, bthA, bthD){
  ux <- 1.3 *10^(-3) * 1.43 * 10^(-3) * (meteo_file$WindSpeed)^2
  sw <- meteo_file$ShortWave
  year <- year(meteo_file$time)
  doy = yday(meteo_file$time)

  df <- data.frame('datetime' = meteo_file$time, 'ux' = ux, 'sw' = sw, 'year' = year, 'doy' = doy)

  annual.wind.ts <- df %>%
    dplyr::filter(doy >= 120 & doy <= 275) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(mean.ux = mean(sqrt(ux), na.rm = TRUE),
              mean.js = mean(sw, na.rm = TRUE))

  annual.wind.ts$Jb <-  (207 * 10^(-6) * 9.81)/(4180 *1000) * annual.wind.ts$mean.js / 1e3 * 1000
  annual.wind.ts$Lmo <- annual.wind.ts$mean.ux^3 /(annual.wind.ts$Jb *0.41)

  dep <- seq(min(H), max(H), 0.5)
  App <- stats::approx(rev(H), rev(A), dep)$y
  zv <- dep %*% App / sum(App)

  lmo.df <- data.frame('year' = unique(df$year),
                       'lmoz' = annual.wind.ts$Lmo / zv,
                       'lmo' = annual.wind.ts$Lmo,
                       'z' = rep(max(H),length(unique(df$year))))

  retrun(lmo.df)
}

get_lmo(meteo_file, bthA, bthD)




### ALL LAKES
# load shapefiles
# https://www.hydrosheds.org/pages/hydrolakes
library(sf)
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

ggplot(lakes) + geom_sf(aes(col = scale(zv)))


