nbdc_targets <- list(
  
  # helper functions ----------------------------------------------
  
  #' Pull NBDC historical meteorological data for a site
  #' 
  #' Pulls data for a given site from 2010-2024 from NBDC. Also formats dates.
  #'
  #' @param site NBDC site code
  #'
  #' @returns Data frame of data for site
  tar_target(read_nbdc_stdmet, function(site) {
    nbdc_names <- c("YY", "MM", "DD", "hh", "mm", "wdir_degt", "wspd_ms", "gst_ms", "wvht_m", "dpd_s", "apd_s", "mwd_degt", "pres_hpa", "atemp_c", "wtemp_c", "dewp_c", "vis_mi", "tide_ft")
    historical = data.frame()
    for (year in 2010:2024) {
      nbdc_url <- str_c("https://www.ndbc.noaa.gov/view_text_file.php?filename=", site, "h", year, ".txt.gz&dir=data/historical/stdmet/")
      tryCatch({
        nbdc <- read_table(nbdc_url, skip = 2, col_names = nbdc_names, show_col_types = FALSE) %>% 
          mutate(date = str_c(YY, "-", MM, "-", DD, " ", hh, ":", mm),
                 date = ymd_hm(date),
                 across(-c(date), ~if_else(.x == 99 | .x == 999, NA, .x)),
                 site = site) %>% 
          select(-c(YY, MM, DD, hh, mm)) %>% 
          relocate(date, site)
        historical <- bind_rows(historical, nbdc)
      },error=function(e){cat("No stdmet data for", site, "in", year, "\n")})
    }
    return(historical)
  }),
  
  #' Pull NBDC historical solar radiation data for a site
  #' 
  #' Pulls data for a given site from 2012-2024 from NBDC. Also formats dates. (No data available in 2010-11)
  #'
  #' @param site NBDC site code
  #'
  #' @returns Data frame of data for site
  tar_target(read_nbdc_srad, function(site) {
    nbdc_names <- c("YY", "MM", "DD", "hh", "mm", "srad_wm2", "swrad_wm2", "lwrad_wm2")
    historical = data.frame()
    for (year in 2012:2024) {
      nbdc_url <- str_c("https://www.ndbc.noaa.gov/view_text_file.php?filename=", site, "r", year, ".txt.gz&dir=data/historical/srad/")
      tryCatch({
        nbdc <- read_table(nbdc_url, skip = 2, col_names = nbdc_names, show_col_types = FALSE) %>% 
          mutate(date = str_c(YY, "-", MM, "-", DD, " ", hh, ":", mm),
                 date = ymd_hm(date),
                 across(-c(date), ~if_else(.x == 9999, NA, .x)),
                 site = site) %>% 
          select(-c(YY, MM, DD, hh, mm)) %>% 
          relocate(date, site)
        historical <- bind_rows(historical, nbdc)
      },error=function(e){cat("No srad data for", site, "in", year, "\n")})
    }
    return(historical)
  }),
  
  # get data ---------------------------------------------------------
  
  #' Pull NBDC meteorological historical data for sites
  #' 
  #' Pulls data for Western Lake Superior buoys from 2010-2024 from NBDC. Then, bind data together
  #'
  #' @param site NBDC site codes
  #'
  #' @returns Data frame of data for all sites
  tar_target(nbdc_dulm5_stdmet, read_nbdc_stdmet("dulm5")),
  tar_target(nbdc_45027_stdmet, read_nbdc_stdmet("45027")),
  tar_target(nbdc_45028_stdmet, read_nbdc_stdmet("45028")),
  tar_target(nbdc_45219_stdmet, read_nbdc_stdmet("45219")),
  tar_target(nbdc_pngw3_stdmet, read_nbdc_stdmet("pngw3")),
  tar_target(nbdc_slvm5_stdmet, read_nbdc_stdmet("slvm5")),
  tar_target(nbdc_disw3_stdmet, read_nbdc_stdmet("disw3")),
  tar_target(nbdc_sxhw3_stdmet, read_nbdc_stdmet("sxhw3")),
  tar_target(nbdc_stdmet, bind_rows(nbdc_dulm5_stdmet, nbdc_45027_stdmet, nbdc_45028_stdmet, nbdc_45219_stdmet,
                             nbdc_pngw3_stdmet, nbdc_slvm5_stdmet, nbdc_disw3_stdmet, nbdc_sxhw3_stdmet)),
  
  #' Pull NBDC solar radiation historical data for sites
  #' 
  #' Pulls data for Western Lake Superior buoys from 2010-2024 from NBDC. Then, bind data together.
  #' Only a couple buoys have solar radiation data
  #'
  #' @param site NBDC site codes
  #'
  #' @returns Data frame of data for all sites
  tar_target(nbdc_45027_srad, read_nbdc_srad("45027")),
  tar_target(nbdc_45028_srad, read_nbdc_srad("45028")),
  tar_target(nbdc_srad, bind_rows(nbdc_45027_srad, nbdc_45028_srad)),
  
  #' Join solar radiation and meteorological data
  #' 
  #' Combine the two big files by site and date
  #'
  #' @returns Data frame of all NBDC data for all sites
  tar_target(nbdc, full_join(nbdc_stdmet, nbdc_srad, by = join_by(date, site)) %>% 
               arrange(date, site)),
  
  # make daily version of data -------------------------------------------
  
  #' Make daily version of NBDC data
  #' 
  #' Gets daily average of data from NBDC
  #'
  #' @param nbdc NBDC data
  #'
  #' @returns Data frame of daily NBDC data
  tar_target(nbdc_daily, nbdc %>% 
               mutate(date = date(date)) %>% 
               group_by(date, site) %>% 
               summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup() %>% 
               mutate(across(-c(date, site), replace_nan)))
  
  
  
)