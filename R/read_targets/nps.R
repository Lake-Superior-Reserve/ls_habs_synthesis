nps_targets <- list(
  
  tar_target(nps16_sonde_file, "raw_data/nps/Apostle Islands Sonde Data.csv", format = "file"),
  tar_target(nps_sonde_file, "raw_data/nps/sonde.csv", format = "file"),
  tar_target(nps_chem_file, "raw_data/nps/Apostle Islands Water Chemistry.csv", format = "file"),
  tar_target(nps_wetlab_file, "raw_data/nps/wetlab.csv", format = "file"),
  
  tar_target(nps16_sonde, read_csv(nps16_sonde_file, skip = 4)),
  tar_target(nps_sonde, read_csv(nps_sonde_file)),
  tar_target(nps_chem, read_csv(nps_chem_file, skip = 5)),
  tar_target(nps_wetlab, read_csv(nps_wetlab_file)),
  
  #nps sensor cleaning
  tar_target(nps16_sonde_deploy_clean, nps16_sonde %>% # continuous data at consistent depth
               mutate(site = Station,
                      date = parse_date_time(`Date/Time`, c("mdy HM", "mdy HMS"), tz = "America/Chicago"),
                      depth = if_else(is.na(`Depth m`), Depth, `Depth m`),
                      Latitude = if_else(str_detect(site, "West"), Latitude[1], Latitude), #fixing coordinates, on 7/7/16 SIW starts having has coordinates for SIE
                      Longitude = if_else(str_detect(site, "West"), Longitude[1], Longitude)) %>%
               filter(!is.na(date) & depth > 8) %>% #conveniently, types of data (cont vs discrete) have different date formats, so we can parse one type and remove the ones that fail to parse; also drop measurements during deployment, indicated by depth
               select(date, site, latitude = Latitude, longitude = Longitude, depth,
                      temp = `Temp °C`, do = `ODO mg/L`, do_sat = `ODO % sat`, ph = pH, cond = `SpCond µS/cm`, turb = `Turbidity NTU`)),
  tar_target(nps16_sonde_profile_clean, nps16_sonde %>% # discrete data at multiple depths
               mutate(site = paste("SIW", Transect_Station, sep = " "),
                      date = parse_date_time(`Date/Time`, c("%m/%d/%Y"), exact = TRUE, tz = "America/Chicago"),
                      depth = if_else(is.na(`Depth m`), Depth, `Depth m`)) %>%
               filter(!is.na(date)) %>%
               select(date, site, latitude = Latitude, longitude = Longitude, depth,
                      temp = `Temp °C`, do = `ODO mg/L`, do_sat = `ODO % sat`, ph = pH, cond = `SpCond µS/cm`, turb = `Turbidity NTU`)),
  tar_target(nps_chem_clean, nps_chem %>%
               mutate(site = if_else(!is.na(Transect_Station), str_c("SIW ", Transect_Station), Station),
                      Latitude = as.numeric(str_trim(Latitude)),
                      Longitude = as.numeric(str_trim(Longitude)),
                      `SRP (ug/L)` = if_else(`SRP (ug/L)` == "BDL" | `SRP (ug/L)` == "0", 0.0008, as.numeric(`SRP (ug/L)`)), # using 0.5 * UMD MDL
                      `NO3 -` = if_else(`NO3 -` == "BDL", 0.001, as.numeric(`NO3 -`)), # using 0.5 * UMD MDL
                      `SO43-` = as.numeric(`SO43-`),
                      po4 = `SRP (ug/L)` * 0.001,
                      tdp = `TDP (ug/L)` * 0.001, # convert to mg/L
                      pp = `PP (ug/L)` * 0.001,
                      tp = tdp + pp,
                      date = mdy(Date, tz = "America/Chicago")) %>%
               filter(!is.na(Latitude)) %>%
               select(date, site, latitude = Latitude, longitude = Longitude, depth = `Sample Depth (m)`,
                      chl = `Chl-a (ug/L)`, tp, tdp, pp, po4, no3 = `NO3 -`, cl = `Cl-`, so4 = `SO43-`)),
  tar_target(nps_wetlab_clean, nps_wetlab %>% 
               mutate(date = mdy_hm(DateTime, tz = "America/Chicago"),
                      date = ceiling_date(date, unit = "minute")) %>%
               filter(!is.na(date)) %>% 
               select(date, site = Station, depth = StationDepth, chl = CHL, cdom = CDOM) %>%
               mutate(across(c(chl, cdom), ~if_else(.x < 0, NA, .x))) %>% 
               group_by(date, site) %>%
               summarise(across(c(depth, chl, cdom), ~mean(.x, na.rm = TRUE)))),
  tar_target(nps_sonde_clean, nps_sonde %>% 
               mutate(SpCond = if_else(is.na(SpCond), Cond, SpCond)) %>% # values in wrong column
               select(date = DateTime, site = Station, latitude = Latitude, longitude = Longitude, depth = SensorDepth,
                      chl = Chlorophyll, temp = Temp, do = ODOmg, do_sat = ODOsat, cond = SpCond, ph = pH, turb = Turbidity, tds = TDS) %>% 
               mutate(date = mdy_hm(date, tz = "America/Chicago"),
                      cond = if_else(cond < 70, NA, cond),
                      across(c(chl, temp, turb), ~if_else(.x < 0, NA, .x))) %>% 
               filter(date > ymd("2016-01-01")) %>% 
               bind_rows(nps16_sonde_deploy_clean) %>% 
               arrange(date)),
  
  tar_target(nps_wetlab_hourly, nps_wetlab_clean %>% 
               mutate(date = floor_date(date, unit = "hour")) %>%
               group_by(date, site) %>%
               summarise(across(c(depth, chl, cdom), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup()),
  tar_target(nps_sonde_hourly, nps_sonde_clean %>%
               mutate(date = floor_date(date, unit = "hour")) %>%
               group_by(date, site, latitude, longitude) %>%
               summarise(across(c(depth, chl, temp, do, do_sat, ph, cond, turb, tds), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup()),
  tar_target(nps_sonde_daily, nps_sonde_hourly %>%
               mutate(date = date(date)) %>%
               group_by(date, site, latitude, longitude) %>%
               summarise(across(c(depth, chl, temp, do, do_sat, ph, cond, turb, tds), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup()),
  tar_target(nps_wetlab_daily, nps_wetlab_hourly %>%
               mutate(date = date(date)) %>%
               group_by(date, site) %>%
               summarise(across(c(depth, chl, cdom), ~mean(.x, na.rm = TRUE))) %>% 
               ungroup()),
  tar_target(nps_sonde_wetlab, nps_wetlab_hourly %>%
               inner_join(nps_sonde_hourly, by = join_by(date, site)) %>% 
               filter(depth.y != 5) %>% 
               select(date, site, latitude, longitude, depth = depth.y,
                      chl = chl.x, cdom, turb, temp, do, do_sat, ph, cond, tds) %>% 
               arrange(date)),
  
  tar_target(nps_sonde_surf, nps16_sonde_profile_clean %>%
               filter(depth <= 2) %>% # matching with chem depth of 2 m
               group_by(date, site, latitude, longitude) %>%
               summarise(across(c(temp, do, do_sat, ph, cond, turb), ~mean(.x, na.rm = TRUE)))),
  tar_target(nps_chem_surf, nps_chem_clean %>%
               filter(is.na(depth) | depth < 8)),
  
  tar_target(nps, nps_chem_surf %>%
               left_join(nps_sonde_surf, by = join_by(date, site)) %>%
               rename(latitude = latitude.x, # using coordinates from chem file, since they're consistent with other sonde data
                      longitude = longitude.x) %>%
               select(-c(latitude.y, longitude.y)) %>%
               mutate(source = "NPS", type = "Lake") %>%
               arrange(date))
  
  
  
  
)