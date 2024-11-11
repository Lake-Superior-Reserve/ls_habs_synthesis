nbdc_targets <- list(
  
  tar_target(read_nbdc, function(site) {
    nbdc_names <- c("YY", "MM", "DD", "hh", "mm", "wdir_degt", "wspd_ms", "gst_ms", "wvht_m", "dpd_s", "apd_s", "mwd_degt", "pres_hpa", "atemp_c", "wtemp_c", "dewp_c", "vis_mi", "tide_ft")
    historical = data.frame()
    for (year in 2010:2023) {
      nbdc_url <- str_c("https://www.ndbc.noaa.gov/view_text_file.php?filename=", site, "h", year, ".txt.gz&dir=data/historical/stdmet/")
      tryCatch({
        nbdc <- read_table(nbdc_url, skip = 2, col_names = nbdc_names) %>% 
          mutate(date = str_c(YY, "-", MM, "-", DD, " ", hh, ":", mm),
                 date = ymd_hm(date),
                 across(-c(date), ~if_else(.x == 99 | .x == 999, NA, .x)),
                 site = site) %>% 
          select(-c(YY, MM, DD, hh, mm)) %>% 
          relocate(date, site)
        historical <- bind_rows(historical, nbdc)
      },error=function(e){cat("ERROR: No data for", site, "in", year, "\n")})
    }
    return(historical)
  }),
  

  # pull nbdc data
  tar_target(nbdc_dulm5, read_nbdc("dulm5")),
  tar_target(nbdc_45027, read_nbdc("45027")),
  tar_target(nbdc_45028, read_nbdc("45028")),
  tar_target(nbdc_45219, read_nbdc("45219")),
  tar_target(nbdc_pngw3, read_nbdc("pngw3")),
  tar_target(nbdc_slvm5, read_nbdc("slvm5")),
  tar_target(nbdc_disw3, read_nbdc("disw3")),
  tar_target(nbdc_sxhw3, read_nbdc("sxhw3")),
  
  tar_target(nbdc, bind_rows(nbdc_dulm5, nbdc_45027, nbdc_45028, nbdc_45219, nbdc_pngw3, nbdc_slvm5, nbdc_disw3, nbdc_sxhw3)),
  
  #clean nbdc
  tar_target(nbdc_daily, nbdc %>% 
               mutate(date = date(date)) %>% 
               group_by(date, site) %>% 
               summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup() %>% 
               mutate(across(-c(date, site), replace_nan)))
  
  
  
)