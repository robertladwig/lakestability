### test Trout Lake

# hourly temp profile

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/116/28/8a04fbda8e4f5ca1c7f0044f1d93d599" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "sampledate",     
                 "hour",     
                 "depth",     
                 "wtemp",     
                 "flag_wtemp"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$year4)=="factor") dt2$year4 <-as.numeric(levels(dt2$year4))[as.integer(dt2$year4) ]               
if (class(dt2$year4)=="character") dt2$year4 <-as.numeric(dt2$year4)                                   
# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2sampledate) == length(tmp2sampledate[!is.na(tmp2sampledate)])){dt2$sampledate <- tmp2sampledate } else {print("Date conversion failed for dt2$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2sampledate) 
if (class(dt2$hour)=="factor") dt2$hour <-as.numeric(levels(dt2$hour))[as.integer(dt2$hour) ]               
if (class(dt2$hour)=="character") dt2$hour <-as.numeric(dt2$hour)
if (class(dt2$depth)=="factor") dt2$depth <-as.numeric(levels(dt2$depth))[as.integer(dt2$depth) ]               
if (class(dt2$depth)=="character") dt2$depth <-as.numeric(dt2$depth)
if (class(dt2$wtemp)=="factor") dt2$wtemp <-as.numeric(levels(dt2$wtemp))[as.integer(dt2$wtemp) ]               
if (class(dt2$wtemp)=="character") dt2$wtemp <-as.numeric(dt2$wtemp)
if (class(dt2$flag_wtemp)!="factor") dt2$flag_wtemp<- as.factor(dt2$flag_wtemp)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(hour)
summary(depth)
summary(wtemp)
summary(flag_wtemp) 
# Get more details on character variables

summary(as.factor(dt2$flag_wtemp))
detach(dt2)        

df_temp = dt2


## hourly wind data

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/117/41/7f36b24e62c5798b16517e9dd85fd628" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "sampledate",     
                 "hour",     
                 "avg_air_temp",     
                 "flag_avg_air_temp",     
                 "avg_rel_hum",     
                 "flag_avg_rel_hum",     
                 "avg_wind_speed",     
                 "flag_avg_wind_speed",     
                 "avg_wind_dir",     
                 "flag_avg_wind_dir",     
                 "avg_do_raw",     
                 "flag_avg_do_raw",     
                 "avg_do_sat",     
                 "flag_avg_do_sat",     
                 "avg_do_wtemp",     
                 "flag_avg_do_wtemp",     
                 "avg_barom_pres_mbar",     
                 "flag_avg_barom_pres_mbar",     
                 "avg_par",     
                 "flag_avg_par",     
                 "avg_co2_atmos",     
                 "flag_avg_co2_atmos",     
                 "avg_co2_dissolved",     
                 "flag_avg_co2_dissolved",     
                 "avg_vapor_pres",     
                 "flag_avg_vapor_pres",     
                 "avg_sat_vapor_pres",     
                 "flag_avg_sat_vapor_pres",     
                 "avg_spec_cond",     
                 "flag_avg_spec_cond"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$year4)=="factor") dt2$year4 <-as.numeric(levels(dt2$year4))[as.integer(dt2$year4) ]               
if (class(dt2$year4)=="character") dt2$year4 <-as.numeric(dt2$year4)                                   
# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2sampledate) == length(tmp2sampledate[!is.na(tmp2sampledate)])){dt2$sampledate <- tmp2sampledate } else {print("Date conversion failed for dt2$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2sampledate) 
if (class(dt2$hour)=="factor") dt2$hour <-as.numeric(levels(dt2$hour))[as.integer(dt2$hour) ]               
if (class(dt2$hour)=="character") dt2$hour <-as.numeric(dt2$hour)
if (class(dt2$avg_air_temp)=="factor") dt2$avg_air_temp <-as.numeric(levels(dt2$avg_air_temp))[as.integer(dt2$avg_air_temp) ]               
if (class(dt2$avg_air_temp)=="character") dt2$avg_air_temp <-as.numeric(dt2$avg_air_temp)
if (class(dt2$flag_avg_air_temp)!="factor") dt2$flag_avg_air_temp<- as.factor(dt2$flag_avg_air_temp)
if (class(dt2$avg_rel_hum)=="factor") dt2$avg_rel_hum <-as.numeric(levels(dt2$avg_rel_hum))[as.integer(dt2$avg_rel_hum) ]               
if (class(dt2$avg_rel_hum)=="character") dt2$avg_rel_hum <-as.numeric(dt2$avg_rel_hum)
if (class(dt2$flag_avg_rel_hum)!="factor") dt2$flag_avg_rel_hum<- as.factor(dt2$flag_avg_rel_hum)
if (class(dt2$avg_wind_speed)=="factor") dt2$avg_wind_speed <-as.numeric(levels(dt2$avg_wind_speed))[as.integer(dt2$avg_wind_speed) ]               
if (class(dt2$avg_wind_speed)=="character") dt2$avg_wind_speed <-as.numeric(dt2$avg_wind_speed)
if (class(dt2$flag_avg_wind_speed)!="factor") dt2$flag_avg_wind_speed<- as.factor(dt2$flag_avg_wind_speed)
if (class(dt2$avg_wind_dir)=="factor") dt2$avg_wind_dir <-as.numeric(levels(dt2$avg_wind_dir))[as.integer(dt2$avg_wind_dir) ]               
if (class(dt2$avg_wind_dir)=="character") dt2$avg_wind_dir <-as.numeric(dt2$avg_wind_dir)
if (class(dt2$flag_avg_wind_dir)!="factor") dt2$flag_avg_wind_dir<- as.factor(dt2$flag_avg_wind_dir)
if (class(dt2$avg_do_raw)=="factor") dt2$avg_do_raw <-as.numeric(levels(dt2$avg_do_raw))[as.integer(dt2$avg_do_raw) ]               
if (class(dt2$avg_do_raw)=="character") dt2$avg_do_raw <-as.numeric(dt2$avg_do_raw)
if (class(dt2$flag_avg_do_raw)!="factor") dt2$flag_avg_do_raw<- as.factor(dt2$flag_avg_do_raw)
if (class(dt2$avg_do_sat)=="factor") dt2$avg_do_sat <-as.numeric(levels(dt2$avg_do_sat))[as.integer(dt2$avg_do_sat) ]               
if (class(dt2$avg_do_sat)=="character") dt2$avg_do_sat <-as.numeric(dt2$avg_do_sat)
if (class(dt2$flag_avg_do_sat)!="factor") dt2$flag_avg_do_sat<- as.factor(dt2$flag_avg_do_sat)
if (class(dt2$avg_do_wtemp)=="factor") dt2$avg_do_wtemp <-as.numeric(levels(dt2$avg_do_wtemp))[as.integer(dt2$avg_do_wtemp) ]               
if (class(dt2$avg_do_wtemp)=="character") dt2$avg_do_wtemp <-as.numeric(dt2$avg_do_wtemp)
if (class(dt2$flag_avg_do_wtemp)!="factor") dt2$flag_avg_do_wtemp<- as.factor(dt2$flag_avg_do_wtemp)
if (class(dt2$avg_barom_pres_mbar)=="factor") dt2$avg_barom_pres_mbar <-as.numeric(levels(dt2$avg_barom_pres_mbar))[as.integer(dt2$avg_barom_pres_mbar) ]               
if (class(dt2$avg_barom_pres_mbar)=="character") dt2$avg_barom_pres_mbar <-as.numeric(dt2$avg_barom_pres_mbar)
if (class(dt2$flag_avg_barom_pres_mbar)!="factor") dt2$flag_avg_barom_pres_mbar<- as.factor(dt2$flag_avg_barom_pres_mbar)
if (class(dt2$avg_par)=="factor") dt2$avg_par <-as.numeric(levels(dt2$avg_par))[as.integer(dt2$avg_par) ]               
if (class(dt2$avg_par)=="character") dt2$avg_par <-as.numeric(dt2$avg_par)
if (class(dt2$flag_avg_par)!="factor") dt2$flag_avg_par<- as.factor(dt2$flag_avg_par)
if (class(dt2$avg_co2_atmos)=="factor") dt2$avg_co2_atmos <-as.numeric(levels(dt2$avg_co2_atmos))[as.integer(dt2$avg_co2_atmos) ]               
if (class(dt2$avg_co2_atmos)=="character") dt2$avg_co2_atmos <-as.numeric(dt2$avg_co2_atmos)
if (class(dt2$flag_avg_co2_atmos)!="factor") dt2$flag_avg_co2_atmos<- as.factor(dt2$flag_avg_co2_atmos)
if (class(dt2$avg_co2_dissolved)=="factor") dt2$avg_co2_dissolved <-as.numeric(levels(dt2$avg_co2_dissolved))[as.integer(dt2$avg_co2_dissolved) ]               
if (class(dt2$avg_co2_dissolved)=="character") dt2$avg_co2_dissolved <-as.numeric(dt2$avg_co2_dissolved)
if (class(dt2$flag_avg_co2_dissolved)!="factor") dt2$flag_avg_co2_dissolved<- as.factor(dt2$flag_avg_co2_dissolved)
if (class(dt2$avg_vapor_pres)=="factor") dt2$avg_vapor_pres <-as.numeric(levels(dt2$avg_vapor_pres))[as.integer(dt2$avg_vapor_pres) ]               
if (class(dt2$avg_vapor_pres)=="character") dt2$avg_vapor_pres <-as.numeric(dt2$avg_vapor_pres)
if (class(dt2$flag_avg_vapor_pres)!="factor") dt2$flag_avg_vapor_pres<- as.factor(dt2$flag_avg_vapor_pres)
if (class(dt2$avg_sat_vapor_pres)=="factor") dt2$avg_sat_vapor_pres <-as.numeric(levels(dt2$avg_sat_vapor_pres))[as.integer(dt2$avg_sat_vapor_pres) ]               
if (class(dt2$avg_sat_vapor_pres)=="character") dt2$avg_sat_vapor_pres <-as.numeric(dt2$avg_sat_vapor_pres)
if (class(dt2$flag_avg_sat_vapor_pres)!="factor") dt2$flag_avg_sat_vapor_pres<- as.factor(dt2$flag_avg_sat_vapor_pres)
if (class(dt2$avg_spec_cond)=="factor") dt2$avg_spec_cond <-as.numeric(levels(dt2$avg_spec_cond))[as.integer(dt2$avg_spec_cond) ]               
if (class(dt2$avg_spec_cond)=="character") dt2$avg_spec_cond <-as.numeric(dt2$avg_spec_cond)
if (class(dt2$flag_avg_spec_cond)!="factor") dt2$flag_avg_spec_cond<- as.factor(dt2$flag_avg_spec_cond)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(hour)
summary(avg_air_temp)
summary(flag_avg_air_temp)
summary(avg_rel_hum)
summary(flag_avg_rel_hum)
summary(avg_wind_speed)
summary(flag_avg_wind_speed)
summary(avg_wind_dir)
summary(flag_avg_wind_dir)
summary(avg_do_raw)
summary(flag_avg_do_raw)
summary(avg_do_sat)
summary(flag_avg_do_sat)
summary(avg_do_wtemp)
summary(flag_avg_do_wtemp)
summary(avg_barom_pres_mbar)
summary(flag_avg_barom_pres_mbar)
summary(avg_par)
summary(flag_avg_par)
summary(avg_co2_atmos)
summary(flag_avg_co2_atmos)
summary(avg_co2_dissolved)
summary(flag_avg_co2_dissolved)
summary(avg_vapor_pres)
summary(flag_avg_vapor_pres)
summary(avg_sat_vapor_pres)
summary(flag_avg_sat_vapor_pres)
summary(avg_spec_cond)
summary(flag_avg_spec_cond) 
# Get more details on character variables

summary(as.factor(dt2$flag_avg_air_temp)) 
summary(as.factor(dt2$flag_avg_rel_hum)) 
summary(as.factor(dt2$flag_avg_wind_speed)) 
summary(as.factor(dt2$flag_avg_wind_dir)) 
summary(as.factor(dt2$flag_avg_do_raw)) 
summary(as.factor(dt2$flag_avg_do_sat)) 
summary(as.factor(dt2$flag_avg_do_wtemp)) 
summary(as.factor(dt2$flag_avg_barom_pres_mbar)) 
summary(as.factor(dt2$flag_avg_par)) 
summary(as.factor(dt2$flag_avg_co2_atmos)) 
summary(as.factor(dt2$flag_avg_co2_dissolved)) 
summary(as.factor(dt2$flag_avg_vapor_pres)) 
summary(as.factor(dt2$flag_avg_sat_vapor_pres)) 
summary(as.factor(dt2$flag_avg_spec_cond))
detach(dt2)      

df_wind = dt2

## hourly radiation data


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/17/37/243910e4bc06665975139a06082e64b9" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "sampledate",     
                 "daynum",     
                 "hour",     
                 "avg_air_temp",     
                 "flag_avg_air_temp",     
                 "avg_dewpoint_temp",     
                 "flag_avg_dewpoint_temp",     
                 "avg_rel_hum",     
                 "flag_avg_rel_hum",     
                 "avg_barom_pres_mbar",     
                 "flag_avg_barom_pres_mbar",     
                 "avg_wind_speed",     
                 "flag_avg_wind_speed",     
                 "max_wind_speed_1min",     
                 "flag_max_wind_speed_1min",     
                 "std_dev_wind_direction",     
                 "flag_std_dev_wind_direction",     
                 "avg_par",     
                 "flag_avg_par",     
                 "avg_shortwave_rad_licor",     
                 "flag_avg_sw_rad_licor",     
                 "avg_sw_sol_rad_eppley",     
                 "flag_avg_sw_rad_eppley",     
                 "avg_longwave_rad_eppley",     
                 "flag_avg_longwave_rad_eppley",     
                 "tot_precip",     
                 "flag_tot_precip"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$year4)=="factor") dt2$year4 <-as.numeric(levels(dt2$year4))[as.integer(dt2$year4) ]               
if (class(dt2$year4)=="character") dt2$year4 <-as.numeric(dt2$year4)                                   
# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2sampledate) == length(tmp2sampledate[!is.na(tmp2sampledate)])){dt2$sampledate <- tmp2sampledate } else {print("Date conversion failed for dt2$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2sampledate) 
if (class(dt2$daynum)=="factor") dt2$daynum <-as.numeric(levels(dt2$daynum))[as.integer(dt2$daynum) ]               
if (class(dt2$daynum)=="character") dt2$daynum <-as.numeric(dt2$daynum)
if (class(dt2$hour)=="factor") dt2$hour <-as.numeric(levels(dt2$hour))[as.integer(dt2$hour) ]               
if (class(dt2$hour)=="character") dt2$hour <-as.numeric(dt2$hour)
if (class(dt2$avg_air_temp)=="factor") dt2$avg_air_temp <-as.numeric(levels(dt2$avg_air_temp))[as.integer(dt2$avg_air_temp) ]               
if (class(dt2$avg_air_temp)=="character") dt2$avg_air_temp <-as.numeric(dt2$avg_air_temp)
if (class(dt2$flag_avg_air_temp)!="factor") dt2$flag_avg_air_temp<- as.factor(dt2$flag_avg_air_temp)
if (class(dt2$avg_dewpoint_temp)=="factor") dt2$avg_dewpoint_temp <-as.numeric(levels(dt2$avg_dewpoint_temp))[as.integer(dt2$avg_dewpoint_temp) ]               
if (class(dt2$avg_dewpoint_temp)=="character") dt2$avg_dewpoint_temp <-as.numeric(dt2$avg_dewpoint_temp)
if (class(dt2$flag_avg_dewpoint_temp)!="factor") dt2$flag_avg_dewpoint_temp<- as.factor(dt2$flag_avg_dewpoint_temp)
if (class(dt2$avg_rel_hum)=="factor") dt2$avg_rel_hum <-as.numeric(levels(dt2$avg_rel_hum))[as.integer(dt2$avg_rel_hum) ]               
if (class(dt2$avg_rel_hum)=="character") dt2$avg_rel_hum <-as.numeric(dt2$avg_rel_hum)
if (class(dt2$flag_avg_rel_hum)!="factor") dt2$flag_avg_rel_hum<- as.factor(dt2$flag_avg_rel_hum)
if (class(dt2$avg_barom_pres_mbar)=="factor") dt2$avg_barom_pres_mbar <-as.numeric(levels(dt2$avg_barom_pres_mbar))[as.integer(dt2$avg_barom_pres_mbar) ]               
if (class(dt2$avg_barom_pres_mbar)=="character") dt2$avg_barom_pres_mbar <-as.numeric(dt2$avg_barom_pres_mbar)
if (class(dt2$flag_avg_barom_pres_mbar)!="factor") dt2$flag_avg_barom_pres_mbar<- as.factor(dt2$flag_avg_barom_pres_mbar)
if (class(dt2$avg_wind_speed)=="factor") dt2$avg_wind_speed <-as.numeric(levels(dt2$avg_wind_speed))[as.integer(dt2$avg_wind_speed) ]               
if (class(dt2$avg_wind_speed)=="character") dt2$avg_wind_speed <-as.numeric(dt2$avg_wind_speed)
if (class(dt2$flag_avg_wind_speed)!="factor") dt2$flag_avg_wind_speed<- as.factor(dt2$flag_avg_wind_speed)
if (class(dt2$max_wind_speed_1min)=="factor") dt2$max_wind_speed_1min <-as.numeric(levels(dt2$max_wind_speed_1min))[as.integer(dt2$max_wind_speed_1min) ]               
if (class(dt2$max_wind_speed_1min)=="character") dt2$max_wind_speed_1min <-as.numeric(dt2$max_wind_speed_1min)
if (class(dt2$flag_max_wind_speed_1min)!="factor") dt2$flag_max_wind_speed_1min<- as.factor(dt2$flag_max_wind_speed_1min)
if (class(dt2$std_dev_wind_direction)=="factor") dt2$std_dev_wind_direction <-as.numeric(levels(dt2$std_dev_wind_direction))[as.integer(dt2$std_dev_wind_direction) ]               
if (class(dt2$std_dev_wind_direction)=="character") dt2$std_dev_wind_direction <-as.numeric(dt2$std_dev_wind_direction)
if (class(dt2$flag_std_dev_wind_direction)!="factor") dt2$flag_std_dev_wind_direction<- as.factor(dt2$flag_std_dev_wind_direction)
if (class(dt2$avg_par)=="factor") dt2$avg_par <-as.numeric(levels(dt2$avg_par))[as.integer(dt2$avg_par) ]               
if (class(dt2$avg_par)=="character") dt2$avg_par <-as.numeric(dt2$avg_par)
if (class(dt2$flag_avg_par)!="factor") dt2$flag_avg_par<- as.factor(dt2$flag_avg_par)
if (class(dt2$avg_shortwave_rad_licor)=="factor") dt2$avg_shortwave_rad_licor <-as.numeric(levels(dt2$avg_shortwave_rad_licor))[as.integer(dt2$avg_shortwave_rad_licor) ]               
if (class(dt2$avg_shortwave_rad_licor)=="character") dt2$avg_shortwave_rad_licor <-as.numeric(dt2$avg_shortwave_rad_licor)
if (class(dt2$flag_avg_sw_rad_licor)!="factor") dt2$flag_avg_sw_rad_licor<- as.factor(dt2$flag_avg_sw_rad_licor)
if (class(dt2$avg_sw_sol_rad_eppley)=="factor") dt2$avg_sw_sol_rad_eppley <-as.numeric(levels(dt2$avg_sw_sol_rad_eppley))[as.integer(dt2$avg_sw_sol_rad_eppley) ]               
if (class(dt2$avg_sw_sol_rad_eppley)=="character") dt2$avg_sw_sol_rad_eppley <-as.numeric(dt2$avg_sw_sol_rad_eppley)
if (class(dt2$flag_avg_sw_rad_eppley)!="factor") dt2$flag_avg_sw_rad_eppley<- as.factor(dt2$flag_avg_sw_rad_eppley)
if (class(dt2$avg_longwave_rad_eppley)=="factor") dt2$avg_longwave_rad_eppley <-as.numeric(levels(dt2$avg_longwave_rad_eppley))[as.integer(dt2$avg_longwave_rad_eppley) ]               
if (class(dt2$avg_longwave_rad_eppley)=="character") dt2$avg_longwave_rad_eppley <-as.numeric(dt2$avg_longwave_rad_eppley)
if (class(dt2$flag_avg_longwave_rad_eppley)!="factor") dt2$flag_avg_longwave_rad_eppley<- as.factor(dt2$flag_avg_longwave_rad_eppley)
if (class(dt2$tot_precip)=="factor") dt2$tot_precip <-as.numeric(levels(dt2$tot_precip))[as.integer(dt2$tot_precip) ]               
if (class(dt2$tot_precip)=="character") dt2$tot_precip <-as.numeric(dt2$tot_precip)
if (class(dt2$flag_tot_precip)!="factor") dt2$flag_tot_precip<- as.factor(dt2$flag_tot_precip)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(daynum)
summary(hour)
summary(avg_air_temp)
summary(flag_avg_air_temp)
summary(avg_dewpoint_temp)
summary(flag_avg_dewpoint_temp)
summary(avg_rel_hum)
summary(flag_avg_rel_hum)
summary(avg_barom_pres_mbar)
summary(flag_avg_barom_pres_mbar)
summary(avg_wind_speed)
summary(flag_avg_wind_speed)
summary(max_wind_speed_1min)
summary(flag_max_wind_speed_1min)
summary(std_dev_wind_direction)
summary(flag_std_dev_wind_direction)
summary(avg_par)
summary(flag_avg_par)
summary(avg_shortwave_rad_licor)
summary(flag_avg_sw_rad_licor)
summary(avg_sw_sol_rad_eppley)
summary(flag_avg_sw_rad_eppley)
summary(avg_longwave_rad_eppley)
summary(flag_avg_longwave_rad_eppley)
summary(tot_precip)
summary(flag_tot_precip) 
# Get more details on character variables

summary(as.factor(dt2$flag_avg_air_temp)) 
summary(as.factor(dt2$flag_avg_dewpoint_temp)) 
summary(as.factor(dt2$flag_avg_rel_hum)) 
summary(as.factor(dt2$flag_avg_barom_pres_mbar)) 
summary(as.factor(dt2$flag_avg_wind_speed)) 
summary(as.factor(dt2$flag_max_wind_speed_1min)) 
summary(as.factor(dt2$flag_std_dev_wind_direction)) 
summary(as.factor(dt2$flag_avg_par)) 
summary(as.factor(dt2$flag_avg_sw_rad_licor)) 
summary(as.factor(dt2$flag_avg_sw_rad_eppley)) 
summary(as.factor(dt2$flag_avg_longwave_rad_eppley)) 
summary(as.factor(dt2$flag_tot_precip))
detach(dt2)               

df_radiation = dt2

#### morphometry

# Package ID: knb-lter-ntl.301.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER Morphometry and Hypsometry data for core study lakes.
# Data set creator:  Dale Robertson - U.S. Geological Survey 
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Metadata Provider:    - North Temperate Lakes LTER 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/301/3/1b7896efd0d6e11ae4448c2afd96ae98" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "depth",     
                 "hp_factor"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$hp_factor)=="factor") dt1$hp_factor <-as.numeric(levels(dt1$hp_factor))[as.integer(dt1$hp_factor) ]               
if (class(dt1$hp_factor)=="character") dt1$hp_factor <-as.numeric(dt1$hp_factor)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(depth)
summary(hp_factor) 
# Get more details on character variables

summary(as.factor(dt1$lakeid))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/301/3/a6905b8cdf899fe706972eee9cdfeffb" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "lakeid",     
                 "basin",     
                 "depth",     
                 "area",     
                 "volume",     
                 "percentvol"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$lakeid)!="factor") dt2$lakeid<- as.factor(dt2$lakeid)
if (class(dt2$basin)!="factor") dt2$basin<- as.factor(dt2$basin)
if (class(dt2$depth)=="factor") dt2$depth <-as.numeric(levels(dt2$depth))[as.integer(dt2$depth) ]               
if (class(dt2$depth)=="character") dt2$depth <-as.numeric(dt2$depth)
if (class(dt2$area)=="factor") dt2$area <-as.numeric(levels(dt2$area))[as.integer(dt2$area) ]               
if (class(dt2$area)=="character") dt2$area <-as.numeric(dt2$area)
if (class(dt2$volume)=="factor") dt2$volume <-as.numeric(levels(dt2$volume))[as.integer(dt2$volume) ]               
if (class(dt2$volume)=="character") dt2$volume <-as.numeric(dt2$volume)
if (class(dt2$percentvol)=="factor") dt2$percentvol <-as.numeric(levels(dt2$percentvol))[as.integer(dt2$percentvol) ]               
if (class(dt2$percentvol)=="character") dt2$percentvol <-as.numeric(dt2$percentvol)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(basin)
summary(depth)
summary(area)
summary(volume)
summary(percentvol) 
# Get more details on character variables

summary(as.factor(dt2$lakeid)) 
summary(as.factor(dt2$basin))
detach(dt2)               


df_hypsography = dt2


##############

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

  return(data.frame('St' = St, 'z_g' = mean_density_depth))
}





setwd('~/Documents/lakestability/src/')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(lubridate)
library(rLakeAnalyzer)
library(zoo)

head(df_temp)
head(df_wind)
head(df_radiation)
head(df_hypsography)

id = "TR"

# morphometry for Trout Lake
input_hypsography <- df_hypsography %>% filter(lakeid == id) %>%
  select(depth, area) %>%
  arrange(depth)

input_temp <- df_temp %>%
  mutate(datetime = as.POSIXct(paste0(sampledate," ",substr(sprintf("%04d",hour),1,2),":00:00")),
         year = year(datetime)) %>%
  filter(year >= 2005 & year <= 2016 ) %>%
  select(datetime, depth, wtemp) %>%
  mutate(day = as.Date(datetime)) %>%
  group_by(day, depth) %>%
  summarise(wtemp = mean(wtemp, na.rm = T))
  

input_wind <- df_wind %>%
  mutate(datetime = as.POSIXct(paste0(sampledate," ",substr(sprintf("%02d",hour),1,2),":00:00")),
         avg_wind_speed = ifelse(!is.na(avg_wind_speed), avg_wind_speed, 0)) %>%
  select(datetime, avg_wind_speed)
# 2005 - 2016

input_radiation<- df_radiation %>%
  mutate(datetime = as.POSIXct(paste0(sampledate," ",substr(sprintf("%04d",hour),1,2),":00:00")),
         avg_shortwave_rad_licor = ifelse(avg_shortwave_rad_licor >= 0, avg_shortwave_rad_licor, 0)) %>%
  select(datetime, avg_shortwave_rad_licor)

input_meteorology <- merge(input_radiation, input_wind, by = 'datetime') 

input_meteorology <- input_meteorology %>%
  mutate(day = as.Date(datetime)) %>%
  group_by(day) %>%
  summarise(avg_shortwave_rad_licor = mean(avg_shortwave_rad_licor, na.rm = T),
            avg_wind_speed = mean(avg_wind_speed, na.rm =T))

overall_df <- data.frame(date = NULL,
                         st = NULL,
                         zv = NULL,
                         zg = NULL, 
                         lmo = NULL,
                         wind = NULL,
                         swr = NULL)
tt = 1
for (dates in as.POSIXct(unique(input_temp$day))){
  
  orig_dates = as.POSIXct(unique(input_temp$day))[tt]
  tt = tt +1
  # dates = unique(input_temp$datetime)[4903]
  print(paste0(match(orig_dates, unique(input_temp$day)),'/',length( unique(input_temp$day))))
  

  
  input <- input_temp %>% filter(day == orig_dates)
  
  if (all(is.na(input$wtemp))){
    next
  }
  
  input$wtemp <- na.approx(input$wtemp, rule = 2)
  
  dz = 0.1
  depth = seq(min(input_hypsography$depth), max(input_hypsography$depth), dz)
  area = approx(input_hypsography$depth, input_hypsography$area, depth)$y
  
  if (max(input$depth) < max(depth)){
    input <- rbind(input, data.frame('day' = mean(input$day), 'depth' = max(depth), 'wtemp' = input$wtemp[length(input$wtemp)], 'datetime' = NA))
  }
  
  if (min(input$depth) > min(depth)){
    input <- rbind(data.frame('day' = mean(input$day), 'depth' = min(depth), 'wtemp' = input$wtemp[1], 'datetime' = NA), input)
  }
  
  interp_temp = approx(input$depth, input$wtemp, depth)$y
  
  if (sd(interp_temp) == 0){
    st = data.frame('St' = NA, 'z_g' = NA)
  } else{
    st = schmidt.stability_idso(wtr = interp_temp, depths = depth, bthA = area, bthD = depth)
  }
  
  meta_depths = meta.depths(wtr = interp_temp, depths = depth)
    
    
    zv <- depth%*% area/ sum(area, na.rm = TRUE)
    
    if (nrow(input_meteorology %>%
           filter(day == orig_dates) ) == 0){
      next
    } else {
      meteo <- input_meteorology %>%
        filter(day == orig_dates) %>%
        mutate(jb = (207 * 10^(-6) * 9.81)/(4180 * 1000) * avg_shortwave_rad_licor / 1e3 * 1000,
               ux = sqrt(1.3 *10^(-3) * 1.43 * 10^(-3) * (avg_wind_speed)^2),
               lmo = ux^3 /(jb * 0.41)) 
    }

    

    overall_df <- rbind(overall_df, data.frame(date = orig_dates,
                                               st = st$St,
                                               zv = zv,
                                               zg = st$z_g, 
                                               upper = meta_depths[1],
                                               lower = meta_depths[2],
                                               lmo = meteo$lmo,
                                               wind = meteo$avg_wind_speed,
                                               swr = meteo$avg_shortwave_rad_licor))
    # print(dates)
    # print(overall_df)
}

write.csv(overall_df, file = '../data/trout_analysis.csv')

library(patchwork)
library(ggplot2)

summer_df <- overall_df %>%
  mutate(month = month(date)) %>%
         # hour = hour(date)) %>%
  filter(month >= 7 & month <= 8)# & hour == 12)

g1 <- ggplot(summer_df) +
  geom_point(aes(lmo/zv, st)) +
  labs(x = 'Mixing:Volume depth', y = 'Energy') +
  geom_vline(xintercept = 1, linetype="dashed") +
  xlim(0, 10) +
  theme_minimal()

g2 <- ggplot(summer_df) +
  geom_point(aes(lmo/zg, st)) +
  labs(x = 'Mixing:Avg density depth', y = 'Energy') +
  geom_vline(xintercept = 1, linetype="dashed") +
  xlim(0, 10) +
  theme_minimal()

g3 <- ggplot(summer_df) +
  geom_point(aes(upper/zg, st)) +
  labs(x = 'Upper:Avg density depth', y = 'Energy') +
  geom_vline(xintercept = 1, linetype="dashed") +
  xlim(0, 10) +
  theme_minimal()

g4 <- ggplot(summer_df,aes(zg, upper)) +
  geom_point(aes(zg, upper, col = st)) +
  labs(x = 'Avg density depth', y = 'Metalimnion depth', col = 'Stability') +
  theme_minimal()

g5 <- ggplot(summer_df,aes(zg, lmo)) +
  geom_point(aes(zg, lmo, col = st)) +
  labs(x = 'Avg density depth', y = 'Mixing depth', col = 'Stability') +
  ylim(0,30)+
  theme_minimal()

g6 <- ggplot(summer_df,aes(upper, lmo)) +
  geom_point(aes(upper, lmo, col = st)) +
  labs(x = 'Metalimnion depth', y = 'Mixing depth', col = 'Stability') +
  ylim(0,30)+
  theme_minimal()

g1 / g2 / g3 / g4 / g5 / g6

ggplot(summer_df) +
  geom_point(aes(date, lmo/zg, col = st)) + ylim(0,10)


