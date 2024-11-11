umd_targets <- list(
  
  tar_target(umd_1721_file, "raw_data/umd/SouthShoreArchive.csv", format = "file"),
  tar_target(umd_2123_file, "raw_data/umd/SouthShoreArchive2021to2023.csv", format = "file"),
  
  tar_target(umd20_troll_file, "raw_data/umd/Troll 2020.xlsx", format = "file"),
  
  tar_target(get_umd23_file, function() {
    dir <- "raw_data/umd/2023"
    troll <- data.frame()
    for (file in list.files(dir)) {
      site <- str_split_i(file, "_", 3)
      site <- str_split_i(site, ".x", 1)
      new_troll <- read_xlsx(str_c(dir,file,sep = "/"))
      new_troll$site <- site
      troll <- bind_rows(troll, new_troll)
    }
    return(troll)
  }),
  
  tar_target(umd_1721, read_csv(umd_1721_file)),
  tar_target(umd_2123, read_csv(umd_2123_file)),
  
  tar_target(umd20_troll, read_xlsx(umd20_troll_file)),
  tar_target(umd23_troll, get_umd23_file()),
  
  # umd clean and combine
  tar_target(umd_1721_clean, umd_1721 %>% 
               mutate(Date = mdy(Date, tz = "America/Chicago"),
                      NO3 = if_else(NO3Flag == "bdl", 0.001, NO3), # set to 0.5 * detection limit if below detection limit
                      PON = if_else(PONFlag == "bdl", 0.0009, PON), # NO3 LOD = 0.002, PON LOD = 0.0018, SRP LOD = 0.0015 per Sandy B 9/17/24
                      SRP = if_else(SRPFlag == "bdl", 0.0008, SRP)) %>% 
               group_by(Date, Site) %>% 
               summarise(latitude = mean(Latitude), longitude = mean(Longitude), depth = mean(Depth), source = first(Project), type = first(Type),
                         doc = mean(DOC, na.rm = T), poc = mean(POC, na.rm = T), poc_filt = mean(POC2, na.rm = T), no3 = mean(NO3, na.rm = T),
                         nh3 = mean(NH3, na.rm = T), tdn = mean(TDN, na.rm = T), pon = mean(PON, na.rm = T), pon_filt = mean(PON2, na.rm = T),
                         po4 = mean(SRP, na.rm = T), tdp = mean(TDP, na.rm = T), pp = mean(PP, na.rm = T), tp = mean(TP, na.rm = T), si = mean(SRSi, na.rm = T), 
                         chl = mean(Chl, na.rm = T), chl_filt = mean(Chl2, na.rm = T), phyco = mean(Phyco, na.rm = T), tss = mean(TSS, na.rm = T)) %>% 
               mutate(si = si * .001 * 28.086, doc = doc * .001 * 12.011, poc = poc * .001 * 12.011, poc_filt = poc_filt * .001 * 12.011, #converting units from to umol/L to mg/L by converting to mmol/L then multiplying by molar mass
                      no3 = no3 * .001 * 14.007, nh3 = nh3 * .001 * 14.007, tdn = tdn * .001 * 14.007, pon = pon * .001 * 14.007, pon_filt = pon_filt * .001 * 14.007,
                      po4 = po4 * .001 * 30.974, tdp = tdp * .001 * 30.974, pp = pp * .001 * 30.974, tp = tp * .001 * 30.974,
                      toc = poc + doc) %>% 
               rename(date = Date, site = Site)),
  tar_target(umd_2123_clean, umd_2123 %>% 
               mutate(Date = ymd(Date, tz = "America/Chicago"),
                      NO3 = if_else(NO3Flag == "bdl", 0.001, NO3), # set to 0.5 * detection limit if below detection limit
                      PON = if_else(PONFlag == "bdl", 0.0009, PON) # NO3 LOD = 0.002, PON LOD = 0.0018, SRP LOD = 0.0015 per Sandy B 9/17/24
               ) %>% 
               group_by(Date, Site) %>% 
               summarise(latitude = mean(Latitude), longitude = mean(Longitude), depth = 0, source = "NPS/BRICO", type = first(Type),
                         doc = mean(DOC, na.rm = T), poc = mean(POC, na.rm = T), no3 = mean(NO3, na.rm = T), nh3 = mean(NH3, na.rm = T), 
                         tdn = mean(TDN, na.rm = T), pon = mean(PON, na.rm = T), po4 = mean(SRP, na.rm = T), tdp = mean(TDP, na.rm = T), 
                         pp = mean(PP, na.rm = T), tp = mean(TP, na.rm = T), si = mean(SRSi, na.rm = T), chl = mean(Chl, na.rm = T),  
                         tss = mean(TSS, na.rm = T)) %>% 
               mutate(si = si * .001 * 28.086, doc = doc * .001 * 12.011, poc = poc * .001 * 12.011,  #converting units from to umol/L to mg/L by converting to mmol/L then multiplying by molar mass
                      no3 = no3 * .001 * 14.007, nh3 = nh3 * .001 * 14.007, tdn = tdn * .001 * 14.007, pon = pon * .001 * 14.007, 
                      po4 = po4 * .001 * 30.974, tdp = tdp * .001 * 30.974, pp = pp * .001 * 30.974, tp = tp * .001 * 30.974,
                      toc = poc + doc) %>% 
               rename(date = Date, site = Site)),
  
  tar_target(umd20_troll_profile, umd20_troll %>% 
               filter(`Profile or Transect` == "P") %>% 
               select(date = `Date Time`, site = Station,
                      latitude = `Latitude (°)`, longitude = `Longitude (°)`, depth = `Depth (ft) (525639)`,
                      chl_field = `Chlorophyll-a Concentration (µg/L) (652536)`, temp = `Water Temperature (°C) (519767)`,
                      cond = `Specific Conductivity (µS/cm) (675375)`, turb = `Turbidity (NTU) (695981)`, 
                      tds = `Total Dissolved Solids (ppt) (675375)`, tss = `Total Suspended Solids (mg/L) (695981)`) %>% 
               mutate(tds = tds * 1000,
                      depth = depth * 0.3048,
                      date = force_tz(date, tzone = "America/Chicago")) %>% 
               filter(depth > 0 & cond > 80)), #remove measurements above water/suspected above water
  tar_target(umd20_troll_transect, umd20_troll %>% 
               filter(`Profile or Transect` == "T") %>% 
               select(date = `Date Time`, site = Station,
                      latitude = `Latitude (°)`, longitude = `Longitude (°)`, depth = `Depth (ft) (525639)`,
                      chl_field = `Chlorophyll-a Concentration (µg/L) (652536)`, temp = `Water Temperature (°C) (519767)`,
                      cond = `Specific Conductivity (µS/cm) (675375)`, turb = `Turbidity (NTU) (695981)`, 
                      tds = `Total Dissolved Solids (ppt) (675375)`, tss = `Total Suspended Solids (mg/L) (695981)`) %>% 
               mutate(tds = tds * 1000,
                      depth = depth * 0.3048,
                      date = force_tz(date, tzone = "America/Chicago")) %>% 
               filter(depth > 0 & cond > 80)), #remove measurements above water/suspected above water
  tar_target(umd20_troll_profile_surf, umd20_troll_profile %>% 
               filter(depth < 2) %>% # only want surface measurements
               mutate(date = date(date)) %>% 
               group_by(date, site) %>%
               summarise(across(c(latitude, longitude, depth, chl_field, temp, cond, turb, tds, tss), ~mean(.x, na.rm = TRUE)))%>% 
               ungroup()),
  tar_target(umd20_troll_transect_surf, umd20_troll_transect %>% 
               filter(depth < 2) %>% # only want surface measurements
               mutate(date = date(date)) %>% 
               group_by(date, site) %>%
               summarise(across(c(latitude, longitude, depth, chl_field, temp, cond, turb, tds, tss), ~mean(.x, na.rm = TRUE)))%>% 
               ungroup()),
  tar_target(umd20_troll_surf, umd20_troll_profile_surf %>% 
               bind_rows(umd20_troll_transect_surf)),
  
  tar_target(umd23_troll_clean, umd23_troll %>% 
               mutate(date = force_tz(`Date Time`, tzone = "America/Chicago"),
                      tds = `Total Dissolved Solids (ppt) (803397)` * 1000,
                      depth = `Depth (ft) (785112)` * 0.3048) %>% 
               select(date, site,
                      latitude = `Latitude (°)`, longitude = `Longitude (°)`, depth,
                      chl_field = `Chlorophyll-a Fluorescence (RFU) (804408)`, temp = `Temperature (°C) (804550)`, # based on the troll 2020 file, chl RFU is equivalent to ug/L for this sensor
                      cond = `Specific Conductivity (µS/cm) (803397)`, turb = `Turbidity (NTU) (803671)`, tds) %>% 
               mutate(cond = if_else(cond < 50, NA, cond),
                      tds = if_else(tds < 40, NA, tds),
                      turb = if_else(turb > 10000, NA, turb))),
  tar_target(umd23_troll_surf, umd23_troll_clean %>% 
               filter(depth < 2) %>% # only want surface measurements
               mutate(date = date(date)) %>% 
               group_by(date, site) %>%
               summarise(across(c(latitude, longitude, depth, chl_field, temp, cond, turb, tds), ~mean(.x, na.rm = TRUE)))%>% 
               ungroup() %>% 
               mutate(site = sapply(site, function(site){
                 if (site == "BBay") return("Bark Bay")
                 else if (site == "MB5a") return("Mawikwe Bay")
                 else if (site == "MB5c") return("Mawikwe Bay c")
                 else if (site == "SBay") return("Siskiwit Bay")
                 else if (site == "SBay2") return("Siskiwit Bay 2")
                 else if (site == "SBay3") return("Siskiwit Bay 3")
                 else if (site == "SBay1") return("Siskiwit Bay")
                 else if (site == "SCav") return("Sea Caves")
                 else return(site)
               }))),
  
  tar_target(umd_troll_surf, bind_rows(umd20_troll_surf, umd23_troll_surf)),
  
  tar_target(umd, bind_rows(umd_1721_clean, umd_2123_clean) %>%
               mutate(across(-c(source, type, site, depth, latitude, longitude), replace_nan)) %>%
               st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ls_shp)) %>% 
               left_join(select(umd_troll_surf, -c(depth, tss, latitude, longitude))))
  
  
  
  
  
  
)