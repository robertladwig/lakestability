
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
