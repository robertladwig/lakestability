# GET METEOROLOGY DATA FROM ERA5, SCRIPT BY SOFIA LA FUENTE AND TADHG N. MOORE
# AUTHOR: Robert Ladwig




get_meteorology <- function(lakename, datetime, lat, lon, variables, path, password){
  Sys.setenv(TZ="UTC")

  lake_coord <- data.frame(lat2 = round(lat + 0.1, 1),
                           lon1 = round(lon - 0.1, 1),
                           lat1 = round(lat - 0.1, 1),
                           lon2 = round(lon + 0.1, 1))

  
  cds.key <- password$VALUE[2]
  wf_set_key(user = password$VALUE[1], key = cds.key, service = "cds")
  
  for (v in variables){
    request <- list(
      dataset_short_name = "reanalysis-era5-single-levels",
      product_type   = "reanalysis",
      format = "netcdf",
      variable = v,
      year = year(datetime),
      month = month(datetime),
      day = day(datetime),
      time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
      # area is specified as N, W, S, E
      area = as.numeric(lake_coord),
      target = "download_e5_single.nc"
    )
    
    file <- wf_request(user = password$VALUE[1],
                       request = request,
                       transfer = TRUE,
                       path = path,
                       verbose = TRUE)
    
    nc_data <- nc_open('../era5_output/download_e5_single.nc')
    if (v == '10u'){
      u10 <- ncvar_get(nc_data, "u10")
    } else if (v == '10v'){
      v10 <- ncvar_get(nc_data, "v10")
    } else if (v == 'mean_surface_downward_short_wave_radiation_flux'){
      swr <- ncvar_get(nc_data, "msdwswrf")
    }

  }
  
  data <- data.frame(u10 = u10,
                     v10 = v10,
                     swr = swr) %>%
    mutate(jb = (207 * 10^(-6) * 9.81)/(4180 * 1000) * swr / 1e3 * 1000,
           ux = sqrt(1.3 *10^(-3) * 1.43 * 10^(-3) * (sqrt(u10^2 + v10^2))^2),
           lmo = ux^3 /(jb * 0.41)) 
  mean_lmo <- data %>%
    summarise(lmo = mean(ux)^3 / (mean(jb) * 0.41))

  return(list(data, 
              mean_lmo))

}
